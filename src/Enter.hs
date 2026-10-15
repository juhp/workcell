{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- SPDX-License-Identifier: BSD-3-Clause
module Enter (enterSession) where

import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt(..), CULong(..))
import SimpleCmd (needProgram)
import System.Directory (findExecutable)
import System.Exit (exitWith)
import System.FilePath ((</>))
import System.IO (hIsTerminalDevice, stdin, stdout)
import System.Posix.Files (readSymbolicLink)
import System.Posix.IO (OpenMode(..), closeFd)
import System.Posix.Process (getProcessID)
import System.Posix.Types (Fd(..))
import System.Process (CreateProcess(..), proc, waitForProcess)

import Network (withChild, openSimpleFd)
import Session (Session(..), SessionDetails(..), findSession, validSession)

invalid :: String -> IO a
invalid = ioError . userError

procPath :: Int -> FilePath -> FilePath
procPath pid suffix = "/proc" </> show pid </> suffix

quote :: String -> String
quote value = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) value ++ "'"

foreign import ccall unsafe "ioctl"
  namespaceIoctl :: CInt -> CULong -> CULong -> IO CInt
foreign import capi unsafe "linux/nsfs.h value NS_GET_USERNS"
  getOwnerNamespace :: CULong

ownerNamespace :: Fd -> IO Fd
ownerNamespace (Fd fd) = Fd <$> throwErrnoIfMinus1 "mount namespace owner"
  (namespaceIoctl fd getOwnerNamespace 0)

enterSession :: Bool -> String -> Bool -> [String] -> IO ()
enterSession dry ident root command = do
  record <- findSession ident
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
      -- Preserve the mapped host credentials in either mode: setup-namespace
      -- UID/GID 0 already maps to the invoking account, and setgroups is denied
      -- in unprivileged user namespaces.
      tailArgs = ["--target", show pid, "--env", "--preserve-credentials"]
  withDescriptors [procPath pid ("ns" </> name) | name <- namespaces] $ \fds ->
    case fds of
      mount:_ -> bracket (ownerNamespace mount) closeFd $ \owner -> do
        supervisor <- getProcessID
        let reference fd = "/proc/" ++ show supervisor ++ "/fd/" ++ show fd
        ownerUser <- readSymbolicLink $ reference owner
        targetUser <- readSymbolicLink $ procPath pid "ns/user"
        -- Mount/network entry requires their owning user namespace. Bubblewrap
        -- may then use a child user namespace to restore the invoking UID/GID.
        -- Join that final namespace only after entering the other namespaces.
        finalCommand <-
          if root || ownerUser == targetUser
          then pure entryCommand
          else do
            insidePID <-
              if sessionSharedProcesses details
              then pure (show pid)
              else do
                status <- lines . BS.unpack <$> BS.readFile (procPath pid "status")
                case [inside | line <- status, "NSpid:":ids <- [words line], inside:_ <- [reverse ids]] of
                  [inside] -> pure inside
                  _ -> invalid "cannot determine session PID inside its namespace"
            nsenter <- findExecutable "nsenter" >>= maybe (invalid "nsenter is required for entry") pure
            pure $ [nsenter, "--user=/proc/" ++ insidePID ++ "/ns/user", "--preserve-credentials", "--"] ++ entryCommand
        if dry
          then putStrLn $ unwords $ map quote $
               ["nsenter", "--user=" ++ procPath pid "ns/user"] ++
               ["--user-parent" | ownerUser /= targetUser] ++
               [flag name ++ "=" ++ procPath pid ("ns" </> name) | name <- namespaces] ++
               tailArgs ++ ["--root", "--wd", "--"] ++ finalCommand
          else do
          needProgram "nsenter"
          withDescriptors [procPath pid "root", procPath pid "cwd"] $ \directories -> do
            valid <- validSession record
            unless valid $ invalid "session exited while preparing entry"
            -- Explicit descriptors avoid nsenter's mixed pidfd/FD join ordering.
            let args = ["--user=" ++ reference owner] ++
                  zipWith (\name fd -> flag name ++ "=" ++ reference fd) namespaces fds ++
                  tailArgs ++ zipWith (\name fd -> name ++ "=" ++ reference fd)
                    ["--root", "--wd"] directories ++ ["--"] ++ finalCommand
            -- nsenter opens the supervisor's descriptors before joining. Its
            -- command inherits none of our namespace/root directory handles.
            result <- withChild ((proc "nsenter" args)
              {delegate_ctlc = True, close_fds = True}) waitForProcess
            exitWith result
      _ -> invalid "missing mount namespace descriptor"

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
