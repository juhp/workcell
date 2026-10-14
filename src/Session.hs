{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- SPDX-License-Identifier: BSD-3-Clause
module Session (SessionDetails(..), withSession, listSessions, enterSession) where

import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, unless, when)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC
import Data.Bits ((.&.))
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import Data.List (intercalate, sort, transpose)
import Data.Maybe (catMaybes, fromMaybe)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt(..), CULong(..))
import GHC.Generics (Generic)
import SimpleCmd (needProgram)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>), isAbsolute, takeFileName)
import System.IO (hClose, hIsTerminalDevice, openBinaryTempFile, stdin, stdout)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files
import System.Posix.IO (OpenMode(..), closeFd)
import System.Posix.Process (getProcessID)
import System.Process (CreateProcess(..), proc, waitForProcess)
import System.Exit (exitWith)
import System.Posix.Types (Fd(..))
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)

import Network (withChild, openSimpleFd)

data SessionDetails = SessionDetails
  { sessionProject :: Maybe FilePath
  , sessionCommand :: [String]
  , sessionNetwork :: String
  , sessionSharedProcesses :: Bool
  } deriving (Show, Generic)
instance FromJSON SessionDetails
instance ToJSON SessionDetails

data Session = Session
  { sessionID :: String
  , sessionPID :: Int
  , sessionStart :: String
  , sessionBoot :: String
  , sessionPIDNamespace :: String
  , sessionMountNamespace :: String
  , sessionDetails :: SessionDetails
  } deriving (Show, Generic)
instance FromJSON Session
instance ToJSON Session

invalid :: String -> IO a
invalid = ioError . userError

strictRead :: FilePath -> IO String
strictRead path = map (toEnum . fromEnum) . BS.unpack <$> BS.readFile path

bootID :: IO String
bootID = unwords . words <$> strictRead "/proc/sys/kernel/random/boot_id"

pidNamespace :: IO String
pidNamespace = readSymbolicLink "/proc/self/ns/pid"

procPath :: Int -> FilePath -> FilePath
procPath pid suffix = "/proc" </> show pid </> suffix

-- comm may contain spaces and closing parentheses; fields after its LAST ')'
-- start with state (field 3), making starttime (field 22) index 19.
processStart :: Int -> IO String
processStart pid = do
  content <- strictRead $ procPath pid "stat"
  let fields = words $ reverse $ takeWhile (/= ')') $ reverse content
  case fields of
    state:_ | state /= "Z" && state /= "X", length fields > 19 -> pure $ fields !! 19
    _ -> invalid "session process has exited or has invalid /proc metadata"

secureDirectory :: FilePath -> IO ()
secureDirectory path = do
  createDirectoryIfMissing True path
  status <- getSymbolicLinkStatus path
  uid <- getEffectiveUserID
  unless (isDirectory status && fileOwner status == uid) $
    invalid $ "unsafe session directory: " ++ path
  -- Only this user's processes should discover or replace session records.
  setFileMode path 0o700

sessionDirectory :: Bool -> IO FilePath
sessionDirectory create = do
  runtime <- lookupEnv "XDG_RUNTIME_DIR"
  base <- case runtime of
    Just path | isAbsolute path -> pure path
    _ -> do
      state <- lookupEnv "XDG_STATE_HOME"
      case state of
        Just path | isAbsolute path -> pure path
        _ -> (</> ".local/state") <$> getHomeDirectory
  ns <- pidNamespace
  let parent = base </> "workcell/sessions"
      path = parent </> filter isDigit ns
  if create
    then secureDirectory parent >> secureDirectory path
    else do
      exists <- doesPathExist parent
      when exists $ checkDirectory parent
      childExists <- doesPathExist path
      when childExists $ checkDirectory path
  pure path
  where
    checkDirectory path = do
      status <- getSymbolicLinkStatus path
      uid <- getEffectiveUserID
      unless (isDirectory status && fileOwner status == uid && fileMode status .&. 0o077 == 0) $
        invalid $ "unsafe session directory: " ++ path

withSession :: SessionDetails -> Int -> IO a -> IO a
withSession details pid action = bracket register cleanup $ const action
  where
    register = do
      directory <- sessionDirectory True
      commandPID <- timeout (10 * 1000000) (findCommandPID pid) >>= maybe
        (invalid "timed out finding the paused sandbox command") pure
      start <- processStart commandPID
      boot <- bootID
      ns <- pidNamespace
      mount <- readSymbolicLink $ procPath commandPID "ns/mnt"
      bracket (openBinaryTempFile directory "pending-")
        (\(path, handle) -> hClose handle >> ignoreMissing (removeFile path)) $ \(path, handle) -> do
          let ident = "s" ++ drop (length "pending-") (takeFileName path)
              record = Session ident commandPID start boot ns mount details
              final = directory </> ident
          BL.hPut handle $ encode record
          hClose handle
          renameFile path final
          pure final
    cleanup = ignoreMissing . removeFile

-- With private PIDs, bubblewrap reports its tiny init process. Register the
-- paused exec shell underneath it: its PID survives exec, and /proc/environ
-- reflects the sandbox environment rather than bubblewrap's original one.
findCommandPID :: Int -> IO Int
findCommandPID pid = do
  result <- search pid
  case result of
    Just found -> pure found
    Nothing -> do
      _ <- processStart pid
      threadDelay 10000
      findCommandPID pid
  where
    search candidate = do
      argv <- BS.split 0 <$> BS.readFile (procPath candidate "cmdline")
      if any (`elem` argv) [BSC.pack "workcell-session", BSC.pack "workcell-network"] &&
         take 1 argv == [BSC.pack "/bin/sh"]
        then pure $ Just candidate
        else do
          children <- words <$> strictRead (procPath candidate $ "task" </> show candidate </> "children")
          results <- mapM (search . read) children
          pure $ case catMaybes results of
            found:_ -> Just found
            [] -> Nothing

ignoreMissing :: IO () -> IO ()
ignoreMissing action = catchIOError action $ \err ->
  unless (isDoesNotExistError err) $ ioError err

validSession :: Session -> IO Bool
validSession record = catchIOError check $ const $ pure False
  where
    check = do
      start <- processStart $ sessionPID record
      boot <- bootID
      ns <- pidNamespace
      mount <- readSymbolicLink $ procPath (sessionPID record) "ns/mnt"
      pure $ start == sessionStart record && boot == sessionBoot record &&
        ns == sessionPIDNamespace record && mount == sessionMountNamespace record

sessions :: IO [Session]
sessions = do
  directory <- sessionDirectory False
  exists <- doesDirectoryExist directory
  if not exists then pure [] else do
    paths <- sort <$> listDirectory directory
    records <- mapM (readRecord directory) $ filter validID paths
    filterM validSession $ catMaybes records
  where
    readRecord directory ident = catchIOError (do
      let path = directory </> ident
      status <- getSymbolicLinkStatus path
      uid <- getEffectiveUserID
      if not (isRegularFile status && fileOwner status == uid && fileMode status .&. 0o077 == 0)
        then pure Nothing
        else do
          bytes <- BS.readFile path
          pure $ case eitherDecodeStrict' bytes of
            Right record | sessionID record == ident -> Just record
            _ -> Nothing) $ const $ pure Nothing

validID :: String -> Bool
validID ('s':suffix) = not (null suffix) && all (\c -> isDigit c || isAsciiLower c || isAsciiUpper c || c == '-') suffix
validID _ = False

quote :: String -> String
quote value = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) value ++ "'"

listSessions :: IO ()
listSessions = do
  records <- sessions
  unless (null records) $ do
    let rows = ["ID", "PROJECT", "NETWORK", "COMMAND"] : map row records
        widths = map (maximum . map length) (transpose $ map (take 3) rows) ++ [0]
        pad width value = value ++ replicate (width - length value) ' '
    mapM_ (putStrLn . intercalate "  " . zipWith pad widths) rows
  where
    row record =
      let details = sessionDetails record
      in [sessionID record, fromMaybe "-" $ sessionProject details,
         sessionNetwork details, unwords $ map quote $ sessionCommand details]

foreign import ccall unsafe "ioctl"
  namespaceIoctl :: CInt -> CULong -> CULong -> IO CInt
foreign import capi unsafe "linux/nsfs.h value NS_GET_USERNS"
  getOwnerNamespace :: CULong

ownerNamespace :: Fd -> IO Fd
ownerNamespace (Fd fd) = Fd <$> throwErrnoIfMinus1 "mount namespace owner"
  (namespaceIoctl fd getOwnerNamespace 0)

enterSession :: String -> Bool -> [String] -> IO ()
enterSession ident dry command = do
  unless (validID ident) $ invalid "invalid session ID (use --list)"
  matches <- filter ((== ident) . sessionID) <$> sessions
  record <- case matches of
    [found] -> pure found
    _ -> invalid $ "unknown or expired session " ++ ident ++ " in this PID namespace (use --list)"
  interactive <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stdout
  entryCommand <-
    let cmd = if null command then ["/bin/bash"] else command in
      if interactive then terminalCommand cmd else pure cmd
  let pid = sessionPID record
      details = sessionDetails record
      namespaces = ["mnt"] ++ ["pid" | not $ sessionSharedProcesses details] ++
        ["net" | sessionNetwork details /= "host"]
      flag "mnt" = "--mount"
      flag name = "--" ++ name
      tailArgs = ["--target", show pid, "--env", "--preserve-credentials"]
  if dry then putStrLn $ unwords $ map quote $
    ["nsenter", "--user=" ++ procPath pid "ns/user", "--user-parent"] ++
    [flag name ++ "=" ++ procPath pid ("ns" </> name) | name <- namespaces] ++
    tailArgs ++ ["--root", "--wd", "--"] ++ entryCommand
  else do
    needProgram "nsenter"
    withDescriptors [procPath pid ("ns" </> name) | name <- namespaces] $ \fds ->
      case fds of
        mount:_ -> bracket (ownerNamespace mount) closeFd $ \owner ->
          withDescriptors [procPath pid "root", procPath pid "cwd"] $ \directories -> do
            valid <- validSession record
            unless valid $ invalid "session exited while preparing entry"
            -- Explicit descriptors avoid nsenter's mixed pidfd/FD join ordering.
            supervisor <- getProcessID
            let reference fd = "/proc/" ++ show supervisor ++ "/fd/" ++ show fd
                args = ["--user=" ++ reference owner] ++
                  zipWith (\name fd -> flag name ++ "=" ++ reference fd) namespaces fds ++
                  tailArgs ++ zipWith (\name fd -> name ++ "=" ++ reference fd)
                    ["--root", "--wd"] directories ++ ["--"] ++ entryCommand
            -- nsenter opens the supervisor's descriptors before joining. Its
            -- command inherits none of our namespace/root directory handles.
            result <- withChild ((proc "nsenter" args)
              {delegate_ctlc = True, close_fds = True}) waitForProcess
            exitWith result
        _ -> invalid "missing mount namespace descriptor"
  where

-- The caller's terminal belongs to another devpts instance. Allocate the new
-- terminal after nsenter has joined the session, and relay input/output to it.
terminalCommand :: [String] -> IO [String]
terminalCommand command = do
  forward <- findExecutable "systemd-pty-forward"
  case forward of
    Just path -> pure $ [path, "--quiet", "--"] ++ command
    Nothing -> do
      script <- findExecutable "script"
      case script of
        Just path -> pure [path, "--quiet", "--return", "--command",
                           "exec " ++ unwords (map quote command), "/dev/null"]
        Nothing -> invalid "interactive entry requires systemd-pty-forward or script (from util-linux)"

withDescriptors :: [FilePath] -> ([Fd] -> IO a) -> IO a
withDescriptors [] action = action []
withDescriptors (path:rest) action = bracket
  (openSimpleFd path ReadOnly) closeFd $ \fd ->
  withDescriptors rest $ \fds -> action (fd:fds)
