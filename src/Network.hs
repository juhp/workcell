{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
module Network
  (NetworkMode(..), networkMode, pastaArguments, verifyNetwork, runPrivateNetwork) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, evaluate)
import Control.Monad (unless, void, when)
import Data.Aeson (FromJSON(..), eitherDecodeStrict', withObject, (.:))
import qualified Data.ByteString as BS
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt(..), CULong(..))
import SimpleCmd (cmdFull)
import System.Directory
  (doesFileExist, doesPathExist, findExecutable, getTemporaryDirectory,
   removeFile, removePathForcibly)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO
  (Handle, SeekMode(..), hClose, hPutStr, hSeek, openTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError, ioeGetErrorString)
import System.Posix.IO
  (OpenMode(..), closeFd, createPipe, defaultFileFlags, fdToHandle, fdWrite,
   handleToFd, openFd)
import System.Posix.Files (createNamedPipe)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd(..))
import System.Process
  (CreateProcess(..), ProcessHandle, StdStream(..), createProcess,
   getPid, getProcessExitCode, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

data NetworkMode = PrivateNetwork | NoNetwork | HostNetwork
  deriving (Eq)

networkMode :: Maybe String -> Bool -> IO NetworkMode
networkMode option shared = do
  when (shared && option /= Nothing) $
    invalid "--share-network and --network cannot be combined"
  case (shared, option) of
    (True, _) -> pure HostNetwork
    (_, Nothing) -> pure PrivateNetwork
    (_, Just "private") -> pure PrivateNetwork
    (_, Just "none") -> pure NoNetwork
    (_, Just "host") -> pure HostNetwork
    _ -> invalid "invalid network mode (expected private, none, or host)"

pastaArguments :: FilePath -> FilePath -> FilePath -> [String]
pastaArguments netns userns pidfile =
  ["--foreground", "--config-net", "--no-map-gw", "--quiet",
   "--dns-forward", "169.254.1.1",
   "-t", "none", "-u", "none", "-T", "none", "-U", "none",
   "--netns", netns, "--userns", userns, "--pid", pidfile]

verifyNetwork :: NetworkMode -> [String] -> IO ()
verifyNetwork HostNetwork _ = pure ()
verifyNetwork mode processArgs = do
  when (mode == PrivateNetwork) $ do
    pasta <- findExecutable "pasta"
    unless (pasta /= Nothing) $
      unavailable "pasta is not installed or not on PATH"
    tun <- doesPathExist "/dev/net/tun"
    unless tun $ unavailable "/dev/net/tun is unavailable in the enclosing environment"
    exists <- doesFileExist "/etc/resolv.conf"
    config <- if exists then readFile "/etc/resolv.conf" else pure ""
    unless (any nameserver $ lines config) $
      unavailable "no nameserver is configured in /etc/resolv.conf"
  let probe = ["--die-with-parent", "--ro-bind", "/", "/", "--dev", "/dev",
               "--unshare-net", "--tmpfs", "/tmp", "--tmpfs", "/run"] ++ processArgs ++ ["--", "/bin/true"]
  if mode == PrivateNetwork
    then do
      result <- catchIOError (runPrivateNetwork [] probe "/tmp/workcell-resolv.conf") $
        unavailable . ioeGetErrorString
      unless (result == ExitSuccess) $ unavailable $ show result
    else do
      (ok, _, err) <- cmdFull "bwrap" probe ""
      unless ok $ unavailable err
  where
    nameserver line = case words line of
      "nameserver":(first:_):_ -> first /= '#'
      _ -> False
    unavailable reason = invalid $
      (if mode == PrivateNetwork then "private networking" else "network isolation")
      ++ " is unavailable. " ++ reason ++ "\n"
      ++ "Use --share-network only if sharing the enclosing network is acceptable."

newtype SandboxInfo = SandboxInfo Int

instance FromJSON SandboxInfo where
  parseJSON = withObject "bubblewrap namespace info" $ \object ->
    SandboxInfo <$> object .: "child-pid"

-- Find the user namespace owning the network, even if bubblewrap enters a
-- different user namespace later to restore the UID after devpts setup.
foreign import ccall unsafe "ioctl"
  namespaceIoctl :: CInt -> CULong -> CULong -> IO CInt

foreign import capi unsafe "linux/nsfs.h value NS_GET_USERNS"
  getOwnerNamespace :: CULong

-- Retain the owning user namespace even if bubblewrap changes its current one.
-- Pasta watches the sandbox's network reference and quits if that process dies.
runPrivateNetwork :: [String] -> [String] -> FilePath -> IO ExitCode
runPrivateNetwork extraPastaArgs args resolverDestination = do
  tmp <- getTemporaryDirectory
  bracket (mkdtemp $ tmp </> "workcell-network-") removePathForcibly $ \dir -> do
    let readyPipe = dir </> "ready"
        sandboxPipe = "/run/workcell-network-ready"
    createNamedPipe readyPipe 0o600
    bracket (openFd readyPipe ReadWrite defaultFileFlags) closeFd $ \gate ->
      withResolver tmp $ \resolverFd ->
      withPipe $ \(_, infoWrite, infoHandle, infoOutput) -> do
        let setup = ["--info-fd", show infoWrite]
            (mounts, cmd) = break (== "--") args
            dns = ["--ro-bind-data", show resolverFd, resolverDestination,
                   "--ro-bind", readyPipe, sandboxPipe]
            -- Wait after bubblewrap has installed its parent-death handling.
            -- Blocking its setup with --block-fd could leave an orphan on failure.
            script = "if IFS= read -r ready < \"$1\" && [ \"$ready\" = ready ]; then shift; exec \"$@\"; fi; exit 1"
            paused = ["--", "/bin/sh", "-c", script, "workcell-network", sandboxPipe] ++ drop 1 cmd
            sandbox = (proc "bwrap" (setup ++ mounts ++ dns ++ paused))
              {delegate_ctlc = True, close_fds = False}
        withChild sandbox $ \child -> do
          hClose infoOutput
          info <- deadline "waiting for bubblewrap namespace setup" $ BS.hGetContents infoHandle
          SandboxInfo pid <- either (invalid . ("invalid bubblewrap namespace info: " ++)) pure $
            eitherDecodeStrict' info
          let childNet = "/proc" </> show pid </> "ns/net"
          bracket (openFd childNet ReadOnly defaultFileFlags) closeFd $ \netFd ->
            bracket (ownerNamespace netFd) closeFd $ \userFd -> do
              supervisor <- getProcessID
              let reference fd = "/proc" </> show supervisor </> "fd" </> show fd
                  pidfile = dir </> "pasta.pid"
                  helper = (proc "pasta" (extraPastaArgs ++
                    pastaArguments childNet (reference userFd) pidfile))
                    {std_in = NoStream, std_out = NoStream, close_fds = True}
              withChild helper $ \network -> do
                deadline "waiting for pasta network setup" $ waitReady pidfile child network
                void $ fdWrite gate "ready\n"
                waitCommand child network
  where
    ownerNamespace (Fd fd) = Fd <$> throwErrnoIfMinus1 "network namespace owner"
      (namespaceIoctl fd getOwnerNamespace 0)

withChild :: CreateProcess -> (ProcessHandle -> IO a) -> IO a
withChild command action = bracket
  (createProcess command >>= \(_, _, _, handle) -> pure handle) stopChild action

stopChild :: ProcessHandle -> IO ()
stopChild handle = do
  status <- getProcessExitCode handle
  case status of
    Just _ -> pure ()
    Nothing -> do
      terminateProcess handle
      result <- timeout (5 * 1000000) (waitForProcess handle)
      case result of
        Just _ -> pure ()
        Nothing -> do
          pid <- getPid handle
          mapM_ (signalProcess sigKILL) pid
          _ <- waitForProcess handle
          pure ()

withPipe :: ((Fd, Fd, Handle, Handle) -> IO a) -> IO a
withPipe = bracket acquire release
  where
    acquire = do
      (input, output) <- createPipe
      reader <- fdToHandle input
      writer <- fdToHandle output
      pure (input, output, reader, writer)
    release (_, _, reader, writer) = hClose reader >> hClose writer

withResolver :: FilePath -> (Fd -> IO a) -> IO a
withResolver tmp action = bracket (openTempFile tmp "workcell-resolv-") cleanup $ \(path, handle) -> do
  exists <- doesFileExist "/etc/resolv.conf"
  config <- if exists then readFile "/etc/resolv.conf" else pure ""
  let preserve line = case words line of
        key:_ -> key `elem` ["search", "domain", "options"]
        _ -> False
  hPutStr handle $ unlines $ "nameserver 169.254.1.1" : filter preserve (lines config)
  hSeek handle AbsoluteSeek 0
  removeFile path
  bracket (handleToFd handle) closeFd action
  where
    cleanup (path, handle) = do
      hClose handle
      ignoreMissing $ removeFile path

waitReady :: FilePath -> ProcessHandle -> ProcessHandle -> IO ()
waitReady pidfile child helper = do
  ensureRunning "bubblewrap exited during network setup" child
  ensureRunning "pasta exited during network setup" helper
  ready <- doesFileExist pidfile
  content <- if ready then readFile pidfile else pure ""
  _ <- evaluate $ length content
  if null content then threadDelay 10000 >> waitReady pidfile child helper else pure ()

waitCommand :: ProcessHandle -> ProcessHandle -> IO ExitCode
waitCommand child helper = do
  status <- getProcessExitCode child
  case status of
    Just code -> pure code
    Nothing -> do
      ensureRunning "pasta exited while the command was running" helper
      threadDelay 10000
      waitCommand child helper

ensureRunning :: String -> ProcessHandle -> IO ()
ensureRunning message handle = do
  status <- getProcessExitCode handle
  case status of
    Nothing -> pure ()
    Just code -> invalid $ message ++ ": " ++ show code

deadline :: String -> IO a -> IO a
deadline message action = timeout (10 * 1000000) action >>= maybe
  (invalid $ "network startup timed out " ++ message) pure

ignoreMissing :: IO () -> IO ()
ignoreMissing action = catchIOError action $ \err ->
  unless (isDoesNotExistError err) $ ioError err

invalid :: String -> IO a
invalid = ioError . userError
