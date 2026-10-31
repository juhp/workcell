{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: LGPL-2.1-or-later
module Sandbox
  (NetworkMode(..),
   networkName,
   Arg(..),
   Sandbox(..),
   PrivateNetwork(..),
   NetworkSetupFailure(..),
   Tracker,
   untracked,
   runSandbox,
   previewSandbox,
   pastaArguments,
   ProbeFailure(..),
   probeNamespaces,
   checkPrivateNetwork)
where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (Exception, Handler(..), bracket, catches, finally, throwIO)
import Control.Monad.Extra (unless, void, when)
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecodeStrict', withObject, withText, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isJust, catMaybes)
import SimpleCmd (cmdFull)
import System.Directory
  (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist,
   findExecutable, getFileSize, getHomeDirectory, getTemporaryDirectory, removeFile,
   removePathForcibly)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), isAbsolute)
import System.IO
  (Handle, IOMode(..), SeekMode(..), hClose, hFileSize, hFlush, hSeek,
   openBinaryTempFile, withBinaryFile)
import System.IO.Error (catchIOError, ioeGetErrorString)
import System.Posix.IO (OpenMode(..), closeFd, createPipe, fdToHandle, fdWrite, handleToFd)
import System.Posix.Files (createNamedPipe)
import System.Posix.Process (getProcessID)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd)
import System.Process (CreateProcess(..), StdStream(..), proc)
import System.Timeout (timeout)

import Child
import Util

data NetworkMode = PrivateNetwork | HostNetwork | NoNetwork
  deriving (Eq, Show)

networkName :: NetworkMode -> String
networkName PrivateNetwork = "private"
networkName HostNetwork = "host"
networkName NoNetwork = "none"

instance ToJSON NetworkMode where
  toJSON = toJSON . networkName

instance FromJSON NetworkMode where
  parseJSON = withText "network mode" $
    \case
      "private" -> pure PrivateNetwork
      "host" -> pure HostNetwork
      "none" -> pure NoNetwork
      _ -> fail "unknown network mode"

-- A bubblewrap option word, or a file whose contents bubblewrap reads from
-- a descriptor opened at launch (shown by its placeholder in previews).
data Arg = Arg String | FileFd String FilePath | DataFd String BS.ByteString

data Sandbox = Sandbox
  { sandboxArgs :: [Arg]
  , sandboxCommand :: [String]
  }

data PrivateNetwork = PrivateNetworkConfig
  { pastaExtraArgs :: [String]
  , resolverDestination :: FilePath
  }

-- Private networking failed before the command was released.
newtype NetworkSetupFailure = NetworkSetupFailure String

instance Show NetworkSetupFailure where
  show (NetworkSetupFailure message) = message

instance Exception NetworkSetupFailure

-- Brackets session registration around the released command.
type Tracker = Int -> IO ExitCode -> IO ExitCode

untracked :: Tracker
untracked _ action = action

pastaArguments :: FilePath -> FilePath -> FilePath -> [String]
pastaArguments netns userns pidfile =
  ["--foreground", "--config-net", "--no-map-gw", "--quiet",
   "--dns-forward", "169.254.1.1",
   "-t", "none", "-u", "none", "-T", "none", "-U", "none",
   "--netns", netns, "--userns", userns, "--pid", pidfile]

gateInside :: FilePath
gateInside = "/run/workcell-ready"

-- Wait after bubblewrap has installed its parent-death handling.
-- Blocking its setup with --block-fd could leave an orphan on failure.
gateCommand :: [String]
gateCommand =
  ["/bin/sh", "-c",
   "if IFS= read -r ready < \"$1\" && [ \"$ready\" = ready ]; then shift; exec \"$@\"; fi; exit 1",
   "workcell-session", gateInside]

-- The bubblewrap command (and pasta command, for private networking) with
-- placeholders for launch-time descriptors and paths.
previewSandbox :: Maybe PrivateNetwork -> Sandbox -> [[String]]
previewSandbox network sandbox =
  ("bwrap" : supervision ++ map render (sandboxArgs sandbox) ++ networkArgs ++
    ["--"] ++ sandboxCommand sandbox) :
  [ "pasta" : pastaArguments "<netns>" "<userns>" "<pid-file>" | isJust network ]
  where
    render (Arg value) = value
    render (FileFd placeholder _) = placeholder
    render (DataFd placeholder _) = placeholder
    supervision = if isJust network then ["--info-fd", "<info-fd>"] else []
    networkArgs = case network of
      Just config -> ["--ro-bind-data", "<dns-fd>", resolverDestination config,
                      "--ro-bind", "<ready-pipe>", gateInside]
      Nothing -> []

-- Bubblewrap starts the command paused behind a FIFO. Private networking
-- attaches pasta to its network namespace before the command is released.
runSandbox :: Maybe PrivateNetwork -> Tracker -> Sandbox -> IO ExitCode
runSandbox Nothing track sandbox = launch Nothing track sandbox
runSandbox (Just config) track sandbox =
  withPastaLog $ \logHandle -> launch (Just (config, logHandle)) track sandbox

launch :: Maybe (PrivateNetwork, Handle) -> Tracker -> Sandbox -> IO ExitCode
launch network track sandbox =
  -- Pasta may run in a confined SELinux domain which cannot write under HOME,
  -- even when the invoking user has chosen a home directory as TMPDIR.
  withRuntimeDirectory $ \dir -> do
    let readyPipe = dir </> "ready"
    createNamedPipe readyPipe 0o600
    bracket (openSimpleFd readyPipe ReadWrite) closeFd $ \gate ->
      withArgDescriptors (sandboxArgs sandbox) $ \args argFds ->
      withResolver (fst <$> network) $ \resolverArgs resolverFds ->
      withPipe $ \(infoFd, reader, writer) -> do
        let argv = ["--info-fd", show infoFd] ++ args ++ resolverArgs ++
              ["--ro-bind", readyPipe, gateInside, "--"] ++ gateCommand ++ sandboxCommand sandbox
        keepOnlyInheritedFds (infoFd : argFds ++ resolverFds)
        withChild ((proc "bwrap" argv) {delegate_ctlc = True, close_fds = False}) $ \child -> do
          hClose writer
          info <- deadline "waiting for bubblewrap namespace setup" $ BS.hGetContents reader
          when (BS.null info) $ do
            code <- waitChild child
            invalid $ "bubblewrap failed during sandbox setup: " ++ show code
          SandboxInfo pid <- either (invalid . ("invalid bubblewrap namespace info: " ++)) pure $
            eitherDecodeStrict' info
          withNetwork network dir pid child $ \wait ->
            track pid $ void (fdWrite gate "ready\n") >> wait

withNetwork :: Maybe (PrivateNetwork, Handle) -> FilePath -> Int -> Child
            -> (IO ExitCode -> IO a) -> IO a
withNetwork Nothing _ _ child action = action (waitChild child)
withNetwork (Just (config, logHandle)) dir pid child action = do
  let childNet = procPath pid "ns/net"
  bracket (setupFailure "cannot open sandbox network namespace" $ openSimpleFd childNet ReadOnly) closeFd $ \netFd ->
    bracket (setupFailure "cannot find network namespace owner" $ ownerNamespace netFd) closeFd $ \userFd -> do
      supervisor <- getProcessID
      confined <- selinuxEnforcing
      let reference fd = "/proc" </> show supervisor </> "fd" </> show fd
          pidfile = dir </> "pasta.pid"
          -- Pasta watches the directory holding its netns path to quit with
          -- the sandbox. Its SELinux policy (passt-selinux) forbids opening
          -- another process's /proc/PID/ns, so skip the watch when confined:
          -- workcell stops pasta itself when the command exits.
          helper = (proc "pasta" (pastaExtraArgs config ++ ["--no-netns-quit" | confined] ++
            pastaArguments childNet (reference userFd) pidfile))
            {std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
      withPastaChild logHandle helper $ \pasta -> do
        timeout (10 * 1000000) (waitReady pidfile child pasta) >>=
          maybe (throwIO $ NetworkSetupFailure "network startup timed out waiting for pasta network setup") pure
        action $ do
          result <- waitEither child pasta
          case result of
            Left code -> pure code
            Right code -> invalid $ "pasta exited while the command was running: " ++ show code
  where
    setupFailure message act = act `catchIOError` \err ->
      throwIO $ NetworkSetupFailure $ message ++ ": " ++ ioeGetErrorString err

-- SELinux is hidden (and pasta unconfined) inside typical toolbox containers.
selinuxEnforcing :: IO Bool
selinuxEnforcing = catchIOError
  ((== "1") . takeWhile (/= '\n') . BSC.unpack <$> BS.readFile "/sys/fs/selinux/enforce")
  (const $ pure False)

waitReady :: FilePath -> Child -> Child -> IO ()
waitReady pidfile child helper = do
  childExited child >>= maybe (pure ()) (\code ->
    invalid $ "bubblewrap exited during network setup: " ++ show code)
  childExited helper >>= maybe (pure ()) (\code ->
    throwIO $ NetworkSetupFailure $ "pasta exited during network setup: " ++ show code)
  ready <- doesFileExist pidfile
  content <- if ready then BS.readFile pidfile else pure ""
  when (BS.null content) $ threadDelay 10000 >> waitReady pidfile child helper

newtype SandboxInfo = SandboxInfo Int

instance FromJSON SandboxInfo where
  parseJSON = withObject "bubblewrap namespace info" $ \object ->
    SandboxInfo <$> object .: "child-pid"

withRuntimeDirectory :: (FilePath -> IO a) -> IO a
withRuntimeDirectory action = do
  runtime <- lookupEnv "XDG_RUNTIME_DIR"
  base <- case runtime of
    Just directory | isAbsolute directory -> do
      exists <- doesDirectoryExist directory
      pure $ if exists then directory else "/tmp"
    _ -> pure "/tmp"
  bracket (mkdtemp $ base </> "workcell-session-") removePathForcibly action

withArgDescriptors :: [Arg] -> ([String] -> [Fd] -> IO a) -> IO a
withArgDescriptors args action = open args [] []
  where
    open [] values fds = action (reverse values) (reverse fds)
    open (Arg value : rest) values fds = open rest (value:values) fds
    open (FileFd _ path : rest) values fds =
      bracket (openSimpleFd path ReadOnly) closeFd $ \fd ->
        open rest (show fd:values) (fd:fds)
    open (DataFd _ bytes : rest) values fds =
      withDataFd "workcell-data-" bytes $ \fd ->
        open rest (show fd:values) (fd:fds)

-- An unlinked temporary file holding the bytes, for bubblewrap to read.
withDataFd :: String -> BS.ByteString -> (Fd -> IO a) -> IO a
withDataFd prefix bytes action = do
  base <- getTemporaryDirectory
  bracket (openBinaryTempFile base prefix) cleanup $ \(path, handle) -> do
    BS.hPut handle bytes
    hSeek handle AbsoluteSeek 0
    removeFile path
    bracket (handleToFd handle) closeFd action
  where
    cleanup (path, handle) = do
      hClose handle
      ignoreMissing $ removeFile path

withPipe :: ((Fd, Handle, Handle) -> IO a) -> IO a
withPipe = bracket acquire release
  where
    acquire = do
      (input, output) <- createPipe
      reader <- fdToHandle input
      writer <- fdToHandle output
      pure (output, reader, writer)
    release (_, reader, writer) = hClose reader >> hClose writer

-- An unlinked resolver file for private networking, pointing to pasta's
-- DNS forwarder and retaining the enclosing search and option settings.
withResolver :: Maybe PrivateNetwork -> ([String] -> [Fd] -> IO a) -> IO a
withResolver Nothing action = action [] []
withResolver (Just config) action = do
  config' <- readResolvConf
  let preserve line = case words line of
        key:_ -> key `elem` ["search", "domain", "options"]
        _ -> False
  withDataFd "workcell-resolv-"
    (BSC.pack $ unlines $ "nameserver 169.254.1.1" : filter preserve config') $ \fd ->
      action ["--ro-bind-data", show fd, resolverDestination config] [fd]

-- Workcell writes the state log itself: a confined pasta may be unable to
-- write an inherited file in HOME, while writing to a pipe is permitted.
withPastaChild :: Handle -> CreateProcess -> (Child -> IO a) -> IO a
withPastaChild logHandle command action =
  withChildStreams command $ \(_, _, output, pasta) -> do
    reader <- maybe (invalid "missing pasta diagnostics pipe") pure output
    finished <- newEmptyMVar
    _ <- forkFinally (copyDiagnostics reader) (putMVar finished)
    action pasta `finally` do
      -- Stopping pasta closes the pipe; wait until its output is logged.
      stopChild pasta
      takeMVar finished >>= either throwIO pure
      hClose reader
  where
    copyDiagnostics reader = do
      bytes <- BS.hGetSome reader 8192
      unless (BS.null bytes) $ do
        BS.hPut logHandle bytes
        hFlush logHandle
        copyDiagnostics reader

withPastaLog :: (Handle -> IO ExitCode) -> IO ExitCode
withPastaLog action = bracket acquire release $ \(path, handle) ->
  action handle `catches`
    [Handler $ \err -> do
       message <- withDiagnostics path handle (ioeGetErrorString err)
       invalid message,
     Handler $ \(NetworkSetupFailure err) -> do
       message <- withDiagnostics path handle err
       throwIO $ NetworkSetupFailure message]
  where
    -- The helper and diagnostics copier have stopped. Release the writer
    -- before opening the log for reading (GHC locks handles within a process).
    withDiagnostics path handle message = do
      hClose handle
      tailText <- catchIOError (diagnosticTail path) $ const $ pure "(unable to read pasta log)\n"
      pure $ message ++
        (if null tailText then "" else "\nPasta log: " ++ path ++
          "\nRecent pasta diagnostics:\n" ++ tailText)
    -- Empty logs are removed; logs with diagnostics are kept.
    release (path, handle) = do
      hClose handle
      ignoreMissing $ do
        size <- getFileSize path
        when (size == 0) $ removeFile path
    acquire = catchIOError openLog $ \err ->
      invalid $ "cannot open pasta log: " ++ ioeGetErrorString err
    openLog = do
      state <- lookupEnv "XDG_STATE_HOME"
      base <- case state of
        Just path | isAbsolute path -> pure path
        _ -> (</> ".local/state") <$> getHomeDirectory
      let directory = base </> "workcell/logs"
      createDirectoryIfMissing True directory
      -- openBinaryTempFile creates the file exclusively with mode 0600.
      openBinaryTempFile directory "pasta.log"

-- Bound both bytes and lines, even for a noisy helper or a huge single line.
diagnosticTail :: FilePath -> IO String
diagnosticTail path = withBinaryFile path ReadMode $ \handle -> do
  size <- hFileSize handle
  hSeek handle AbsoluteSeek (max 0 $ size - 8192)
  bytes <- BS.hGet handle 8192
  let recent = lines $ BSC.unpack bytes
  pure $ unlines $ drop (max 0 $ length recent - 20) recent

deadline :: String -> IO a -> IO a
deadline message action = timeout (10 * 1000000) action >>= maybe
  (invalid $ "sandbox startup timed out " ++ message) pure

data ProbeFailure = ProcessIsolationUnavailable String | NetworkIsolationUnavailable String

-- One probe for the namespaces a launch needs; a narrower probe only on
-- failure, to report which kind of isolation is unavailable.
probeNamespaces :: Bool -> Bool -> IO (Maybe ProbeFailure)
probeNamespaces privateProc privateNet
  | not privateProc && not privateNet = pure Nothing
  | otherwise = do
      (ok, err) <- probe privateProc privateNet
      if ok then pure Nothing
        else if privateProc && privateNet then do
          (procOk, procErr) <- probe True False
          pure $ Just $ if procOk then NetworkIsolationUnavailable err
                        else ProcessIsolationUnavailable procErr
        else pure $ Just $ (if privateProc then ProcessIsolationUnavailable
                            else NetworkIsolationUnavailable) err
  where
    probe proc' net = do
      (ok, _, err) <- cmdFull "bwrap" (["--die-with-parent", "--ro-bind", "/", "/", "--dev", "/dev"] ++
        (if proc' then ["--unshare-pid", "--proc", "/proc"] else []) ++
        ["--unshare-net" | net] ++ ["--", "/bin/true"]) ""
      pure (ok, err)

-- Requirements for private networking which can be checked without
-- launching anything. Returns the reason if it is unavailable.
checkPrivateNetwork :: IO (Maybe String)
checkPrivateNetwork = do
  pasta <- findExecutable "pasta"
  tun <- doesPathExist "/dev/net/tun"
  config <- readResolvConf
  pure $ case catMaybes
    [ if isJust pasta then Nothing else Just "pasta is not installed or not on PATH"
    , if tun then Nothing else Just "/dev/net/tun is unavailable in the enclosing environment"
    , if any nameserver config then Nothing else Just "no nameserver is configured in /etc/resolv.conf"
    ] of
    reason:_ -> Just reason
    [] -> Nothing
  where
    nameserver line = case words line of
      "nameserver":(first:_):_ -> first /= '#'
      _ -> False
