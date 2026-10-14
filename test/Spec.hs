{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: LGPL-2.1-or-later
module Main (main) where

import Control.Exception (bracket, throwIO)
import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Value(..), eitherDecodeStrict', encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isNothing)
import qualified Network as WorkcellNetwork
import qualified Session as WorkcellSession
import qualified Network.Socket as Net
import System.Directory hiding (executable)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..), die, exitWith)
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, hFlush, hGetLine, hPutStrLn)
import System.Posix.Files (fileMode, getFileStatus, readSymbolicLink, setFileMode)
import Data.Bits ((.&.))
import qualified System.Posix.IO as Posix
import System.Posix.Temp (mkdtemp)
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process hiding (shell)
import System.Timeout (timeout)
import Test.Hspec

data Context = Context
  { binary :: FilePath
  , privateProc :: Bool
  , privateNet :: Bool
  , pastaNet :: Bool
  , root :: FilePath
  , project :: FilePath
  , home :: FilePath
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--test-tracked-loopback-network":bwrapArgs ->
      WorkcellNetwork.runTrackedPrivateNetwork
        (WorkcellSession.withSession $ WorkcellSession.SessionDetails Nothing ["sleep", "30"] "private" False)
        ["--splice-only"] bwrapArgs "/tmp/workcell-test-resolv" >>= exitWith
    "--test-loopback-network":bwrapArgs ->
      WorkcellNetwork.runPrivateNetwork ["--splice-only"] bwrapArgs "/tmp/workcell-test-resolv" >>= exitWith
    _ -> testMain

testMain :: IO ()
testMain = do
  bwrap <- findExecutable "bwrap" >>= maybe
    (die "workcell integration tests require bubblewrap (bwrap) on PATH") pure
  executable <- lookupEnv "WORKCELL" >>= maybe
    (findExecutable "workcell" >>= maybe (fail "workcell executable not found") pure) pure
  resolved <- canonicalizePath executable
  (code, _, _) <- capture $ proc bwrap
    ["--die-with-parent", "--ro-bind", "/", "/", "--dev", "/dev",
     "--unshare-pid", "--proc", "/proc", "--", "/bin/true"]
  (netCode, _, _) <- capture $ proc bwrap
    ["--die-with-parent", "--ro-bind", "/", "/", "--dev", "/dev",
     "--unshare-net", "--", "/bin/true"]
  pasta <- findExecutable "pasta"
  tun <- doesPathExist "/dev/net/tun"
  resolverExists <- doesFileExist "/etc/resolv.conf"
  resolver <- if resolverExists then readFile "/etc/resolv.conf" else pure ""
  let nameservers = [address | "nameserver":address:_ <- map words (lines resolver)]
  -- Probe the helper independently: a workcell regression must fail its tests,
  -- rather than making private-network examples appear unsupported.
  (pastaCode, _, _) <- case pasta of
    Just helper | tun && not (null nameservers) -> capture $ proc helper
      ["--config-net", "--no-map-gw", "--no-pidns",
       "--dns-forward", "169.254.1.1", "-t", "none", "-u", "none",
       "-T", "none", "-U", "none", "--", "/bin/true"]
    _ -> pure (ExitFailure 1, "", "pasta, /dev/net/tun, or resolver configuration is missing")
  hspec $ around (withContext resolved (code == ExitSuccess)
                 (netCode == ExitSuccess) (pastaCode == ExitSuccess)) spec

withContext :: FilePath -> Bool -> Bool -> Bool -> ActionWith Context -> IO ()
withContext executable supported netSupported pastaSupported action = do
  hostHome <- getHomeDirectory >>= canonicalizePath
  bracket (mkdtemp $ hostHome </> "workcell tests ") removePathForcibly $ \dir -> do
    let workdir = dir </> "project with spaces"
    createDirectory workdir
    action $ Context executable supported netSupported pastaSupported dir workdir hostHome

-- Each command has a timeout; process's bracket cleanup stops it on timeout.
capture :: CreateProcess -> IO (ExitCode, String, String)
capture command = timeout (30 * 1000000) (readCreateProcessWithExitCode command "")
  >>= maybe (fail "command timed out after 30 seconds") pure

invoke :: Context -> Maybe Bool -> Maybe [(String, String)] -> [String]
       -> IO (ExitCode, String, String)
invoke ctx shared environment args = capture $
  (proc (binary ctx) (flags ++ args)) {cwd = Just (project ctx), env = environment}
  where
    sharing = fromMaybe (not (privateProc ctx) && "--dryrun" `notElem` args) shared
    explicitNetwork = any (\arg -> arg == "--network" || arg == "--share-network" ||
                          "--network=" `isPrefixOf` arg) (takeWhile (/= "--") args)
    flags = ["--share-processes" | sharing] ++
      ["--share-network" | not explicitNetwork && "--dryrun" `notElem` args]

success :: Context -> [String] -> IO String
success ctx args = successful =<< invoke ctx Nothing Nothing args

successful :: (ExitCode, String, String) -> IO String
successful (code, out, err) = do
  unless (code == ExitSuccess) $ expectationFailure $ show code ++ ": " ++ err
  pure out

failure :: Context -> [String] -> IO String
failure ctx args = do
  (code, _, err) <- invoke ctx Nothing Nothing args
  code `shouldNotBe` ExitSuccess
  pure err

shell :: Context -> String -> [String] -> IO ()
shell ctx script args = do
  _ <- success ctx $ ["--", "/bin/sh", "-c", "set -eu\n" ++ script, "check"] ++ args
  pure ()

spec :: SpecWith Context
spec = describe "workcell CLI" $ do
  it "lists no sessions and rejects conflicting session options without launching" $ \ctx -> do
    sessionInvoke ctx ["--list"] >>= successful >>= (`shouldBe` "")
    forM_ [["--list", "--enter", "s123"], ["--list", "--share-network"],
           ["--list", "--wayland"], ["--list", "--x11"], ["--list", "--dbus"],
           ["--list", "--", "/bin/true"], ["--enter", "s123", "-H", "unwanted"]] $ \args -> do
      (code, _, _) <- sessionInvoke ctx args
      code `shouldNotBe` ExitSuccess
    (code, _, err) <- sessionInvoke ctx ["--enter", "s123"]
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "unknown or expired"
    doesDirectoryExist (project ctx </> "unwanted") >>= (`shouldBe` False)

  it "tracks concurrent sessions and removes records on termination" $ \ctx -> do
    withRunningSession ctx [] $ \first -> do
      withRunningSession ctx [] $ \second -> do
        first `shouldNotBe` second
        output <- successful =<< sessionInvoke ctx ["--list"]
        output `shouldContain` first
        output `shouldContain` second
        output `shouldContain` project ctx
        output `shouldContain` "host"
      output <- successful =<< sessionInvoke ctx ["--list"]
      output `shouldContain` first
      length (lines output) `shouldBe` 2
    sessionInvoke ctx ["--list"] >>= successful >>= (`shouldBe` "")
    sessionRecordPaths ctx >>= (`shouldBe` [])

  it "enters a running workcell without sudo and preserves its environment and filesystem" $ \ctx -> do
    helper <- findExecutable "nsenter"
    when (isNothing helper) $ pendingWith "nsenter unavailable"
    withRunningSession ctx ["-e", "ENTRY_VALUE=hello world"] $ \ident -> do
      preview <- successful =<< sessionInvoke ctx ["--enter", ident, "--dryrun"]
      preview `shouldContain` "nsenter"
      preview `shouldContain` "--mount="
      out <- successful =<< sessionInvoke ctx ["--enter", ident, "--", "/bin/sh", "-c",
        "printf '%s\\n' \"$WORKCELL_ACTIVE\" \"$ENTRY_VALUE\" \"$PWD\" \"$1\"; test -f entry-home-marker; test -f \"$HOME/entry-home-marker\"", "check", "literal argument"]
      lines out `shouldBe` ["1", "hello world", project ctx, "literal argument"]
      (code, _, _) <- sessionInvoke ctx ["--enter", ident, "--", "/bin/sh", "-c", "exit 23"]
      code `shouldBe` ExitFailure 23

  it "enters with a usable terminal and preserves input, arguments, and exit status" $ \ctx -> do
    helpers <- mapM findExecutable ["systemd-pty-forward", "script"]
    when (all isNothing helpers) $ pendingWith "no terminal relay helper available"
    withRunningSession ctx [] $ \ident -> do
      environment <- sessionEnvironment ctx
      let open = do
            (master, slave) <- openPseudoTerminal
            (,) <$> Posix.fdToHandle master <*> Posix.fdToHandle slave
          close (master, slave) = hClose master >> hClose slave
      bracket open close $ \(master, slave) -> do
        let invocation = (proc (binary ctx)
              ["--enter", ident, "--", "/bin/sh", "-c", unlines
               ["set -eu", "test -t 0 && test -t 1 && test -t 2",
                "device=$(tty); test -c \"$device\"",
                "stty -F /dev/tty -a >/dev/null", "stty -echo", "printf 'ready\\n'",
                "IFS= read -r answer", "test \"$answer\" = 'terminal input'",
                "test \"$1\" = \"literal ' argument\"", "exit 23"],
               "check", "literal ' argument"])
              {env = Just environment, std_in = UseHandle slave,
               std_out = UseHandle slave, std_err = UseHandle slave}
        result <- timeout (5 * 1000000) $
          bracket (createProcess invocation)
            (\(_, _, _, handle) -> terminateProcess handle >> waitForProcess handle >> pure ()) $
            \(_, _, _, handle) -> do
              ready <- hGetLine master
              ready `shouldBe` "ready\r"
              hPutStrLn master "terminal input"
              hFlush master
              waitForProcess handle
        result `shouldBe` Just (ExitFailure 23)

  it "ignores stale, mismatched, malformed and publicly readable session records" $ \ctx -> do
    withRunningSession ctx [] $ \ident -> do
      paths <- sessionRecordPaths ctx
      path <- case paths of
        [found] -> pure found
        _ -> fail "expected one session record"
      getFileStatus path >>= (\mode -> mode .&. 0o777 `shouldBe` 0o600) . fileMode
      bytes <- BS.readFile path
      value <- either fail pure $ eitherDecodeStrict' bytes
      forM_ [("sessionStart", "invalid"), ("sessionBoot", "invalid"),
             ("sessionMountNamespace", "mnt:[0]"), ("sessionPIDNamespace", "pid:[0]")] $ \(key, text) -> do
        case value of
          Object object -> BL.writeFile path $ encode $ Object $ KM.insert key (String text) object
          _ -> expectationFailure "invalid session record"
        sessionInvoke ctx ["--list"] >>= successful >>= (`shouldBe` "")
        (code, _, _) <- sessionInvoke ctx ["--enter", ident]
        code `shouldNotBe` ExitSuccess
      BS.writeFile path bytes
      setFileMode path 0o644
      sessionInvoke ctx ["--list"] >>= successful >>= (`shouldBe` "")
      setFileMode path 0o600
      writeFile path "invalid JSON"
      sessionInvoke ctx ["--list"] >>= successful >>= (`shouldBe` "")

  it "scopes discovery to the caller's PID namespace" $ \ctx -> do
    withRunningSession ctx [] $ \_ -> do
      paths <- sessionRecordPaths ctx
      let other = root ctx </> "session-runtime/workcell/sessions/other-namespace"
      createDirectory other
      case paths of
        [path] -> copyFile path (other </> "s999")
        _ -> fail "expected one session record"
      output <- successful =<< sessionInvoke ctx ["--list"]
      length (lines output) `shouldBe` 2
      output `shouldNotContain` "s999"

  it "ignores records left by an uncatchable supervisor stop" $ \ctx -> do
    withRunningSessionHandle ctx [] $ \ident handle -> do
      pid <- getPid handle >>= maybe (fail "supervisor exited unexpectedly") pure
      signalProcess sigKILL pid
      _ <- waitForProcess handle
      sessionInvoke ctx ["--list"] >>= successful >>= (`shouldBe` "")
      paths <- sessionRecordPaths ctx
      length paths `shouldBe` 1
      (code, _, _) <- sessionInvoke ctx ["--enter", ident]
      code `shouldNotBe` ExitSuccess

  it "removes records after normal exit and creates none for failed setup or dry runs" $ \ctx -> do
    environment <- sessionEnvironment ctx
    let flags = ["--share-processes" | not $ privateProc ctx]
        run args = capture $ (proc (binary ctx) (flags ++ args))
          {cwd = Just (project ctx), env = Just environment}
    _ <- successful =<< run ["--network", "host", "--", "/bin/true"]
    sessionRecordPaths ctx >>= (`shouldBe` [])
    _ <- successful =<< run ["--network", "host", "--dryrun"]
    sessionRecordPaths ctx >>= (`shouldBe` [])
    (code, _, _) <- run ["--network", "host", "-v", "/missing-session-test"]
    code `shouldNotBe` ExitSuccess
    sessionRecordPaths ctx >>= (`shouldBe` [])

  it "uses state storage when the runtime directory is unset" $ \ctx -> do
    environment <- sessionEnvironment ctx
    let flags = ["--share-processes" | not $ privateProc ctx]
    _ <- successful =<< capture ((proc (binary ctx) (flags ++ ["--network", "host", "--", "/bin/true"]))
      {cwd = Just (project ctx), env = Just $ filter ((/= "XDG_RUNTIME_DIR") . fst) environment})
    doesDirectoryExist (root ctx </> "session-state/workcell/sessions") >>= (`shouldBe` True)

  it "uses writable fixture storage when enclosing TMPDIR points into a read-only mount" $ \ctx -> do
    environment <- getEnvironment
    let temporary = root ctx </> "enclosing temporary directory"
        flags = ["--share-processes" | not $ privateProc ctx]
        args = ["--ro-bind", "/", "/", "--proc", "/proc",
                "--bind", project ctx, project ctx, "--", binary ctx] ++
               flags ++ ["--network", "host", "--", "/bin/true"]
    createDirectory temporary
    _ <- successful =<< capture ((fixtureProcess args)
      {cwd = Just (project ctx), env = Just $ setVariables [("TMPDIR", temporary)] environment})
    pure ()

  it "enters offline namespaces and respects shared process mode" $ \ctx -> do
    unless (privateNet ctx) $ pendingWith "network namespaces unavailable"
    withRunningSession ctx ["--network", "none", "--share-processes"] $ \ident -> do
      preview <- successful =<< sessionInvoke ctx ["--enter", ident, "--dryrun"]
      preview `shouldContain` "--net="
      preview `shouldNotContain` "--pid="
      out <- successful =<< sessionInvoke ctx ["--enter", ident, "--", "/bin/sh", "-c",
        "readlink /proc/self/ns/net"]
      out `shouldContain` "net:["

  it "registers and enters the private-network supervisor with a real loopback pasta helper" $ \ctx -> do
    requireLoopbackPasta
    unless (privateProc ctx) $ pendingWith "private /proc unavailable"
    executable <- getExecutablePath
    environment <- sessionEnvironment ctx
    let waitListed = do
          output <- successful =<< sessionInvoke ctx ["--list"]
          case drop 1 $ lines output of
            row:_ | ident:_ <- words row -> pure ident
            _ -> threadDelay 10000 >> waitListed
        args = ["--test-tracked-loopback-network", "--die-with-parent", "--ro-bind", "/", "/",
                "--dev", "/dev", "--unshare-net", "--unshare-pid", "--proc", "/proc",
                "--tmpfs", "/tmp", "--tmpfs", "/run", "--", "/bin/sleep", "30"]
    bracket (createProcess $ (proc executable args) {env = Just environment})
      (\(_, _, _, handle) -> terminateProcess handle >> waitForProcess handle >> pure ()) $ \_ -> do
        found <- timeout (5 * 1000000) waitListed
        ident <- maybe (fail "private session was not registered") pure found
        output <- successful =<< sessionInvoke ctx ["--enter", ident, "--", "/bin/sh", "-c",
          "cat /tmp/workcell-test-resolv; readlink /proc/self/ns/net; readlink /proc/self/ns/pid"]
        output `shouldContain` "nameserver 169.254.1.1"
        outerNet <- readSymbolicLink "/proc/self/ns/net"
        outerPID <- readSymbolicLink "/proc/self/ns/pid"
        output `shouldNotContain` outerNet
        output `shouldNotContain` outerPID

  it "provides help, version, shell defaults, and distinct dry-run modes" $ \ctx -> do
    success ctx ["--help"] >>= (`shouldContain` "--volume")
    success ctx ["--version"] >>= (`shouldBe` "0.1.0\n")
    out <- success ctx ["--dryrun"]
    out `shouldContain` "'--' '/bin/bash'"
    out `shouldContain` "'--unshare-pid'"
    out `shouldContain` "'--proc' '/proc'"
    out `shouldContain` "'--remount-ro' '/'"
    out `shouldNotContain` "'--ro-bind' '/' '/'"
    out `shouldContain` "'pasta' '--foreground' '--config-net' '--no-map-gw'"
    out `shouldContain` "'-t' 'none' '-u' 'none' '-T' 'none' '-U' 'none'"
    out `shouldContain` "'--ro-bind-data' '<dns-fd>'"
    offline <- success ctx ["--network", "none", "--dryrun"]
    offline `shouldContain` "'--unshare-net'"
    offline `shouldNotContain` "'pasta'"
    host <- success ctx ["--share-network", "--dryrun"]
    host `shouldNotContain` "'--unshare-net'"
    host `shouldNotContain` "'pasta'"
    fallback <- success ctx ["--share-filesystem", "--dryrun"]
    fallback `shouldContain` "'--ro-bind' '/' '/'"
    shared <- successful =<< invoke ctx (Just True) Nothing ["--dryrun"]
    shared `shouldNotContain` "'--unshare-pid'"
    shared `shouldNotContain` "'--proc'"

  it "shares the enclosing network only in host mode" $ \ctx -> do
    expected <- readSymbolicLink "/proc/self/ns/net"
    forM_ [["--network", "host"], ["--share-network"]] $ \flags -> do
      success ctx (flags ++ ["--", "readlink", "/proc/self/ns/net"])
        >>= (`shouldBe` expected ++ "\n")

  it "isolates offline networking independently of filesystem and process sharing" $ \ctx -> do
    unless (privateNet ctx) $ pendingWith "container blocks network namespaces"
    expected <- readSymbolicLink "/proc/self/ns/net"
    forM_ [[], ["--share-processes", "--share-filesystem"]] $ \flags -> do
      success ctx (flags ++ ["--network", "none", "--", "readlink", "/proc/self/ns/net"])
        >>= (`shouldNotBe` expected ++ "\n")

  it "separates private networking while preserving identity, hostname, DNS, and exit status" $ \ctx -> do
    unless (pastaNet ctx) $ pendingWith "private pasta networking is unavailable here"
    expected <- readSymbolicLink "/proc/self/ns/net"
    success ctx ["--network", "private", "--", "readlink", "/proc/self/ns/net"]
      >>= (`shouldNotBe` expected ++ "\n")
    forM_ [["id", "-u"], ["id", "-g"], ["hostname"]] $ \args -> do
      host <- success ctx $ ["--network", "host", "--"] ++ args
      success ctx (["--network", "private", "--"] ++ args) `shouldReturn` host
    success ctx ["--network", "private", "--", "cat", "/etc/resolv.conf"]
      >>= (`shouldContain` "nameserver 169.254.1.1\n")
    (code, _, _) <- invoke ctx Nothing Nothing
      ["--network", "private", "--", "/bin/sh", "-c", "exit 23"]
    code `shouldBe` ExitFailure 23

  it "blocks enclosing localhost services in offline and private modes" $ \ctx -> do
    unless (privateNet ctx) $ pendingWith "container blocks network namespaces"
    bracket (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol) Net.close $ \listener -> do
      Net.bind listener $ Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1))
      Net.listen listener 4
      address <- Net.getSocketName listener
      port <- case address of
        Net.SockAddrInet value _ -> pure $ show value
        _ -> fail "unexpected listener address"
      let args mode = ["--network", mode, "--", "/bin/bash", "--noprofile", "--norc", "-c",
                       "touch command-ran; exec 3<>/dev/tcp/127.0.0.1/\"$1\"", "check", port]
      _ <- success ctx $ args "host"
      forM_ ("none" : ["private" | pastaNet ctx]) $ \mode -> do
        removeFile (project ctx </> "command-ran")
        (code, _, _) <- invoke ctx Nothing Nothing (args mode)
        code `shouldNotBe` ExitSuccess
        doesFileExist (project ctx </> "command-ran") `shouldReturn` True

  it "fails without side effects when private networking is unavailable" $ \ctx -> do
    when (pastaNet ctx) $ pendingWith "private pasta networking is supported"
    let supplied = root ctx </> "must not be created"
    (code, _, err) <- invoke ctx (Just True) Nothing
      ["--network", "private", "-H", supplied, "--", "/bin/sh", "-c", "touch command-ran"]
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "private networking is unavailable"
    err `shouldContain` "--share-network"
    doesPathExist supplied `shouldReturn` False
    doesPathExist (project ctx </> "command-ran") `shouldReturn` False

  it "requires pasta only for private networking" $ \ctx -> do
    let tools = root ctx </> "tools"
        supplied = root ctx </> "must not be created"
    createDirectory tools
    bwrap <- findExecutable "bwrap" >>= maybe (fail "bwrap disappeared") pure
    createFileLink bwrap (tools </> "bwrap")
    environment <- getEnvironment
    let envs = setVariables [("PATH", tools)] environment
    forM_ ["host", "none"] $ \mode -> do
      when (mode /= "none" || privateNet ctx) $ do
        _ <- successful =<< invoke ctx (Just True) (Just envs)
          ["--network", mode, "--", "/bin/true"]
        pure ()
    (code, _, err) <- invoke ctx (Just True) (Just envs)
      ["--network", "private", "-H", supplied, "--", "/bin/true"]
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "pasta is not installed or not on PATH"
    doesPathExist supplied `shouldReturn` False

  it "terminates private networking when its command is terminated" $ \ctx -> do
    unless (pastaNet ctx) $ pendingWith "private pasta networking is unavailable here"
    let flags = ["--share-processes" | not (privateProc ctx)]
        args = flags ++ ["--network", "private", "--", "/bin/sh", "-c",
                         "touch network-ready; exec /bin/sleep 30"]
    bracket (createProcess ((proc (binary ctx) args) {cwd = Just (project ctx)}))
      (\(_, _, _, handle) -> terminateProcess handle >> waitForProcess handle >> pure ()) $
      \(_, _, _, handle) -> do
        pid <- getPid handle >>= maybe (fail "workcell exited unexpectedly") pure
        waitUntil (doesFileExist (project ctx </> "network-ready"))
        terminateProcess handle
        timeout (5 * 1000000) (waitForProcess handle)
          >>= (`shouldSatisfy` (/= Nothing))
        doesDirectoryExist ("/proc" </> show pid) `shouldReturn` False

  it "uses the real pasta launcher to restore identity, set DNS, and forward argv" $ \ctx -> do
    requireLoopbackPasta
    let value = "spaces ' dollar $ and backtick `"
    uid <- getEffectiveUserID
    gid <- getEffectiveGroupID
    namespace <- readSymbolicLink "/proc/self/ns/net"
    hostname <- takeWhile (/= '\n') <$> success ctx ["--", "hostname"]
    -- Exercise the actual helper/user-namespace/FD plumbing without a TUN
    -- device. Outbound connectivity is covered only by the TAP tests above.
    invocation <- loopbackCommand ctx ["/bin/sh", "-ec",
      unlines ["[ \"$(id -u)\" = \"$2\" ]", "[ \"$(id -g)\" = \"$3\" ]",
               "[ \"$(readlink /proc/self/ns/net)\" != \"$4\" ]", "[ \"$(hostname)\" = \"$5\" ]",
               "grep -qx 'nameserver 169.254.1.1' /tmp/workcell-test-resolv",
               "printf '%s' \"$1\"", "exit 23"],
      "check", value, show uid, show gid, namespace, hostname]
    (code, out, err) <- capture invocation
    unless (code == ExitFailure 23) $ expectationFailure $ show code ++ ": " ++ err
    out `shouldBe` value

  it "cleans up the real pasta launcher when terminated" $ \ctx -> do
    requireLoopbackPasta
    invocation <- loopbackCommand ctx ["/bin/sh", "-c", "touch network-ready; exec /bin/sleep 30"]
    bracket (createProcess invocation)
      (\(_, _, _, handle) -> terminateProcess handle >> waitForProcess handle >> pure ()) $
      \(_, _, _, handle) -> do
        pid <- getPid handle >>= maybe (fail "pasta exited unexpectedly") pure
        waitUntil (doesFileExist (project ctx </> "network-ready"))
        children <- words <$> readFile ("/proc" </> show pid </> "task" </> show pid </> "children")
        children `shouldSatisfy` (not . null)
        terminateProcess handle
        timeout (5 * 1000000) (waitForProcess handle)
          >>= (`shouldSatisfy` (/= Nothing))
        waitUntil $ and <$> mapM (fmap not . doesDirectoryExist . ("/proc" </>)) children

  it "keeps the requested command paused if the network helper fails" $ \ctx -> do
    let tools = root ctx </> "failing helper"
        helper = tools </> "pasta"
    createDirectory tools
    writeFile helper $ unlines $ ["#!/bin/sh", "echo earlier-diagnostic >&2"] ++
      ["echo diagnostic-" ++ show n ++ " >&2" | n <- [1 :: Int .. 25]] ++ ["exit 42"]
    setFileMode helper 0o700
    environment <- getEnvironment
    invocation <- loopbackCommand ctx ["/bin/sh", "-c", "touch command-ran"]
    let envs = setVariables [("PATH", tools ++ ":/usr/bin:/bin"),
                            ("XDG_STATE_HOME", root ctx </> "state")] environment
    (code, _, err) <- capture (invocation {env = Just envs})
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "pasta exited during network setup"
    err `shouldContain` "diagnostic-25"
    err `shouldNotContain` "earlier-diagnostic"
    logs <- pastaLogs ctx
    length logs `shouldBe` 1
    forM_ logs $ \path -> do
      err `shouldContain` path
      readFile path >>= (`shouldContain` "earlier-diagnostic")
      status <- getFileStatus path
      (fileMode status .&. 0o777) `shouldBe` 0o600
    doesPathExist (project ctx </> "command-ran") `shouldReturn` False

  it "logs helper warnings quietly and keeps distinct logs for concurrent runs" $ \ctx -> do
    unless (privateNet ctx) $ pendingWith "container blocks network namespaces"
    let helper = unlines
          ["echo helper-warning >&2", "while [ \"$1\" != --pid ]; do shift; done",
           "echo $$ > \"$2\"", "exec /bin/sleep 30"]
    invocation <- fakeHelperCommand ctx helper ["/bin/sh", "-c", "printf command-output; exit 23"]
    first <- newEmptyMVar
    second <- newEmptyMVar
    _ <- forkFinally (capture invocation) (putMVar first)
    _ <- forkFinally (capture invocation) (putMVar second)
    forM_ [first, second] $ \result -> do
      (code, out, err) <- takeMVar result >>= either throwIO pure
      code `shouldBe` ExitFailure 23
      out `shouldBe` "command-output"
      err `shouldBe` ""
    logs <- pastaLogs ctx
    length logs `shouldBe` 2
    forM_ logs $ \path -> readFile path `shouldReturn` "helper-warning\n"

  it "reports retained diagnostics when the helper exits during a command" $ \ctx -> do
    unless (privateNet ctx) $ pendingWith "container blocks network namespaces"
    let helper = unlines
          ["while [ \"$1\" != --pid ]; do shift; done", "echo $$ > \"$2\"",
           "while [ ! -f command-ran ]; do /bin/sleep 0.01; done",
           "echo runtime-failure >&2", "exit 42"]
    invocation <- fakeHelperCommand ctx helper
      ["/bin/sh", "-c", "touch command-ran; exec /bin/sleep 30"]
    (code, _, err) <- capture invocation
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "pasta exited while the command was running"
    err `shouldContain` "runtime-failure"
    logs <- pastaLogs ctx
    length logs `shouldBe` 1
    forM_ logs $ \path -> err `shouldContain` path

  it "uses enclosing HOME for logs when XDG_STATE_HOME is relative" $ \ctx -> do
    unless (privateNet ctx) $ pendingWith "container blocks network namespaces"
    invocation <- fakeHelperCommand ctx "echo fallback-diagnostic >&2\nexit 42" ["/bin/true"]
    environment <- getEnvironment
    let envs = setVariables [("HOME", root ctx), ("XDG_STATE_HOME", "relative-state")]
          (fromMaybe environment $ env invocation)
    (_, _, err) <- capture (invocation {env = Just envs})
    let directory = root ctx </> ".local/state/workcell/logs"
    err `shouldContain` directory
    entries <- listDirectory directory
    length entries `shouldBe` 1

  it "fails clearly before starting a command when the log cannot be opened" $ \ctx -> do
    let blocked = root ctx </> "blocked state"
    writeFile blocked "not a directory"
    invocation <- loopbackCommand ctx ["/bin/sh", "-c", "touch command-ran"]
    environment <- getEnvironment
    let envs = setVariables [("XDG_STATE_HOME", blocked)] environment
    (code, _, err) <- capture (invocation {env = Just envs})
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "cannot open pasta log"
    doesPathExist (project ctx </> "command-ran") `shouldReturn` False

  it "creates no helper logs for dry runs, host networking, or offline networking" $ \ctx -> do
    environment <- getEnvironment
    let envs = setVariables [("XDG_STATE_HOME", root ctx </> "state")] environment
    _ <- successful =<< invoke ctx Nothing (Just envs) ["--dryrun"]
    _ <- successful =<< invoke ctx Nothing (Just envs) ["--share-network", "--", "/bin/true"]
    when (privateNet ctx) $ do
      _ <- successful =<< invoke ctx Nothing (Just envs) ["--network", "none", "--", "/bin/true"]
      pure ()
    doesPathExist (root ctx </> "state/workcell/logs") `shouldReturn` False

  it "discards successful preflight logs but retains failed preflight diagnostics" $ \ctx -> do
    unless (pastaNet ctx) $ pendingWith "private pasta networking is unavailable here"
    environment <- getEnvironment
    let stateEnv = setVariables [("XDG_STATE_HOME", root ctx </> "state")] environment
    _ <- successful =<< invoke ctx Nothing (Just stateEnv) ["--network", "private", "--", "/bin/true"]
    pastaLogs ctx >>= (\logs -> length logs `shouldBe` 1)
    invocation <- fakeHelperCommand ctx "echo preflight-failure >&2\nexit 42" ["/bin/true"]
    (code, _, err) <- invoke ctx Nothing (env invocation)
      ["--network", "private", "-H", root ctx </> "uncreated home", "--", "/bin/true"]
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "preflight-failure"
    err `shouldContain` "Pasta log:"
    pastaLogs ctx >>= (\logs -> length logs `shouldBe` 2)
    doesPathExist (root ctx </> "uncreated home") `shouldReturn` False

  it "can explicitly share the enclosing PID namespace" $ \ctx -> do
    expected <- readSymbolicLink "/proc/self/ns/pid"
    out <- successful =<< invoke ctx (Just True) Nothing ["--", "readlink", "/proc/self/ns/pid"]
    out `shouldBe` expected ++ "\n"

  it "isolates processes when private proc is available" $ \ctx -> do
    unless (privateProc ctx) $ pendingWith "container blocks private /proc"
    expected <- readSymbolicLink "/proc/self/ns/pid"
    out <- successful =<< invoke ctx (Just False) Nothing ["--", "readlink", "/proc/self/ns/pid"]
    out `shouldNotBe` expected ++ "\n"
    bracket (createProcess $ proc "/bin/sleep" ["30"])
      (\(_, _, _, handle) -> terminateProcess handle >> waitForProcess handle >> pure ()) $
      \(_, _, _, handle) -> do
        pid <- getPid handle >>= maybe (fail "sleep exited unexpectedly") pure
        shell ctx "[ ! -d \"/proc/$1\" ]" [show pid]

  it "fails without side effects when private proc is unavailable" $ \ctx -> do
    when (privateProc ctx) $ pendingWith "private /proc is supported"
    let supplied = root ctx </> "must not be created"
    (code, _, err) <- invoke ctx (Just False) Nothing
      ["-H", supplied, "--", "/bin/sh", "-c", "touch command-ran"]
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "private PID namespace and /proc are unavailable"
    err `shouldContain` "--share-processes"
    doesPathExist supplied `shouldReturn` False
    doesPathExist (project ctx </> "command-ran") `shouldReturn` False

  it "preserves project writes but hides home and discards temporary files" $ \ctx -> do
    let marker = root ctx </> "host-only"
    createDirectory marker
    shell ctx (unlines
      [ "[ \"$PWD\" = \"$1\" ]", "[ \"$HOME\" = \"$2\" ]", "[ ! -e \"$3\" ]"
      , "printf persistent > project-write", "printf temporary > \"$HOME/home-write\""
      , "printf temporary > /tmp/workcell-temp-write"
      , "findmnt -n -o OPTIONS -T /usr | tr ',' '\\n' | grep -qx ro"
      ]) [project ctx, home ctx, marker]
    readFile (project ctx </> "project-write") `shouldReturn` "persistent"
    shell ctx "[ ! -e \"$HOME/home-write\" ] && [ ! -e /tmp/workcell-temp-write ]" []

  it "resolves explicit project symlinks" $ \ctx -> do
    let other = root ctx </> "other project"
        link = project ctx </> "project-link"
    createDirectory other
    createDirectoryLink other link
    success ctx ["-p", link, "--", "/bin/pwd"] >>= (`shouldBe` other ++ "\n")

  it "supports persistent home and a safely quoted side-effect-free dry run" $ \ctx -> do
    let supplied = root ctx </> "new home" </> "nested"
        quoted = "spaces ' dollar $ and backtick `"
    out <- success ctx ["-H", supplied, "--dryrun", "--", "/usr/bin/printf", "%s", quoted]
    doesPathExist supplied `shouldReturn` False
    -- Let the shell parse the printed invocation; no shell expansion of argv is allowed.
    let flags = "--share-network" : ["--share-processes" | not (privateProc ctx)]
    runnable <- success ctx $ flags ++ ["--dryrun", "--", "/usr/bin/printf", "%s", quoted]
    out `shouldContain` "'--'"
    executed <- successful =<< capture ((proc "/bin/sh" ["-c", runnable]) {cwd = Just (project ctx)})
    executed `shouldBe` quoted
    shellHome ctx supplied "printf saved > \"$HOME/home-write\""
    readFile (supplied </> "home-write") `shouldReturn` "saved"
    shellHome ctx supplied "[ \"$(cat \"$HOME/home-write\")\" = saved ]"

  it "enforces read-only file volumes and allows explicit writes" $ \ctx -> do
    let src = root ctx </> "extra file"
        dest = "/tmp/extra file"
    writeFile src "original"
    volumeShell ctx (src ++ ":" ++ dest)
      "[ \"$(cat \"$1\")\" = original ] && ! (printf changed > \"$1\")" [dest]
    readFile src `shouldReturn` "original"
    volumeShell ctx (src ++ ":" ++ dest ++ ":rw") "printf changed > \"$1\"" [dest]
    readFile src `shouldReturn` "changed"

  it "supports same-path directory volumes under home" $ \ctx -> do
    let src = root ctx </> "cache"
    createDirectory src
    writeFile (src </> "marker") "readable"
    volumeShell ctx src "[ \"$(cat \"$1/marker\")\" = readable ] && ! touch \"$1/new\"" [src]
    volumeShell ctx (src ++ ":rw") "printf writable > \"$1/new\"" [src]
    readFile (src </> "new") `shouldReturn` "writable"

  it "clears other environment variables and applies explicit overrides" $ \ctx -> do
    environment <- getEnvironment
    let supplied = setVariables [("WORKCELL_TEST_SECRET", "hidden"), ("WORKCELL_TEST_PASS", "passed"),
                                 ("WORKCELL_ACTIVE", "outside")] environment
    _ <- successful =<< invoke ctx Nothing (Just supplied)
      ["-e", "WORKCELL_TEST_PASS", "-e", "WORKCELL_TEST_VALUE=first", "-e", "WORKCELL_TEST_VALUE=last=part",
       "-e", "WORKCELL_TEST_EMPTY=", "--", "/bin/sh", "-c", unlines
       ["set -eu", "[ \"$WORKCELL_ACTIVE\" = 1 ]",
        "[ -z \"${WORKCELL_TEST_SECRET+x}\" ]", "[ \"$WORKCELL_TEST_PASS\" = passed ]",
        "[ \"$WORKCELL_TEST_VALUE\" = last=part ]", "[ \"${WORKCELL_TEST_EMPTY+x}\" = x ]",
        "[ -z \"$WORKCELL_TEST_EMPTY\" ]"]]
    pure ()

  it "exposes only the Wayland socket and permits explicit environment overrides" $ \ctx -> do
    environment <- getEnvironment
    let runtime = root ctx </> "wayland-runtime"
        socket = runtime </> "wayland-0"
        supplied display = setVariables ([("XDG_RUNTIME_DIR", runtime)] ++
          [("WAYLAND_DISPLAY", value) | Just value <- [display]]) $
          filter ((/= "WAYLAND_DISPLAY") . fst) environment
    createDirectory runtime
    writeFile (runtime </> "private-marker") "hidden"
    bracket (Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol) Net.close $ \listener -> do
      Net.bind listener $ Net.SockAddrUnix socket
      forM_ [Nothing, Just "wayland-0", Just socket] $ \display -> do
        out <- successful =<< invoke ctx Nothing (Just $ supplied display)
          ["--wayland", "--", "/bin/sh", "-c",
           "test -S \"$1\" && test ! -e \"$XDG_RUNTIME_DIR/private-marker\" && printf '%s\\n' \"$XDG_RUNTIME_DIR\" \"$WAYLAND_DISPLAY\" \"$GDK_BACKEND\"",
           "check", socket]
        lines out `shouldBe` [runtime, fromMaybe "wayland-0" display, "wayland"]
      out <- successful =<< invoke ctx Nothing (Just $ supplied Nothing)
        ["--wayland", "-e", "GDK_BACKEND=x11", "--", "/bin/sh", "-c", "echo \"$GDK_BACKEND\""]
      out `shouldBe` "x11\n"

  it "rejects missing or invalid Wayland settings before launch" $ \ctx -> do
    environment <- filter (\(key, _) -> key `notElem` ["XDG_RUNTIME_DIR", "WAYLAND_DISPLAY"]) <$> getEnvironment
    let check settings message = do
          (code, _, err) <- invoke ctx Nothing (Just $ setVariables settings environment)
            ["--wayland", "--dryrun"]
          code `shouldNotBe` ExitSuccess
          err `shouldContain` message
    check [] "--wayland requires XDG_RUNTIME_DIR"
    check [("XDG_RUNTIME_DIR", "relative")] "absolute XDG_RUNTIME_DIR"
    check [("XDG_RUNTIME_DIR", root ctx), ("WAYLAND_DISPLAY", "")] "nonempty WAYLAND_DISPLAY"
    check [("XDG_RUNTIME_DIR", root ctx), ("WAYLAND_DISPLAY", root ctx)] "not a socket"
    check [("XDG_RUNTIME_DIR", root ctx)] "wayland-0"

  it "exposes the selected X11 socket and read-only authority with environment overrides" $ \ctx -> do
    environment <- getEnvironment
    let directory = root ctx </> "x11"
        authority = root ctx </> ".Xauthority"
        -- Exercise an inherited TMPDIR that becomes read-only in the fixture.
        supplied = setVariables [("HOME", root ctx), ("TMPDIR", root ctx)] $
          filter ((/= "XAUTHORITY") . fst) environment
        run envs args = capture $ (proc "bwrap" $
          ["--die-with-parent", "--ro-bind", "/", "/", "--dev", "/dev", "--tmpfs", "/tmp",
           "--bind", "/proc", "/proc",
           "--ro-bind", directory, "/tmp/.X11-unix",
           "--setenv", "TMPDIR", "/tmp",
           "--setenv", "XDG_RUNTIME_DIR", "/tmp/workcell-x11-runtime",
           "--", binary ctx, "--network", "host"] ++
          ["--share-processes" | not $ privateProc ctx] ++ args)
          {cwd = Just $ project ctx, env = Just envs}
    createDirectory directory
    writeFile authority "test-cookie"
    writeFile (directory </> "private-marker") "hidden"
    bracket (Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol) Net.close $ \listener -> do
      Net.bind listener $ Net.SockAddrUnix $ directory </> "X42"
      forM_ [":42", ":42.0", "unix:42", "unix/:42.1"] $ \display -> do
        out <- successful =<< run (setVariables [("DISPLAY", display)] supplied)
          ["--x11", "--", "/bin/sh", "-c", unlines
           ["set -eu", "test -S /tmp/.X11-unix/X42",
            "test ! -e /tmp/.X11-unix/private-marker",
            "test \"$(cat \"$XAUTHORITY\")\" = test-cookie",
            "! printf changed > \"$XAUTHORITY\"",
            "printf '%s\\n' \"$DISPLAY\" \"$XAUTHORITY\" \"$GDK_BACKEND\""]]
        lines out `shouldBe` [display, "/run/workcell-xauthority", "x11"]
      out <- successful =<< run
        (setVariables [("DISPLAY", ":42"), ("XAUTHORITY", authority)] supplied)
        ["--x11", "-e", "GDK_BACKEND=custom", "--", "/bin/sh", "-c", "echo \"$GDK_BACKEND\""]
      out `shouldBe` "custom\n"
      bracket (Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol) Net.close $ \wayland -> do
        Net.bind wayland $ Net.SockAddrUnix $ directory </> "wayland-0"
        let bothEnv = setVariables [("DISPLAY", ":42"),
              ("WAYLAND_DISPLAY", "/tmp/.X11-unix/wayland-0")] supplied
        forM_ [(["--wayland", "--x11"], "wayland,x11"),
               (["--x11", "--wayland"], "wayland,x11"),
               (["--wayland", "--x11", "-e", "GDK_BACKEND=x11,wayland"], "x11,wayland")] $ \(flags, backend) -> do
          combined <- successful =<< run bothEnv (flags ++
            ["--", "/bin/sh", "-c", unlines
             ["set -eu", "test -S /tmp/.X11-unix/X42", "test -S \"$WAYLAND_DISPLAY\"",
              "test ! -e /tmp/.X11-unix/private-marker",
              "test \"$(cat \"$XAUTHORITY\")\" = test-cookie",
              "printf '%s\\n' \"$DISPLAY\" \"$WAYLAND_DISPLAY\" \"$GDK_BACKEND\""]])
          lines combined `shouldBe` [":42", "/tmp/.X11-unix/wayland-0", backend]
      (code, _, err) <- run
        (setVariables [("DISPLAY", ":42"), ("XAUTHORITY", directory)] supplied)
        ["--x11", "--dryrun"]
      code `shouldNotBe` ExitSuccess
      err `shouldContain` "X11 authority is not a regular file"
    readFile authority `shouldReturn` "test-cookie"

  it "rejects missing, remote, or malformed X11 settings before launch" $ \ctx -> do
    environment <- filter ((/= "DISPLAY") . fst) <$> getEnvironment
    let check settings args message = do
          (code, _, err) <- invoke ctx Nothing (Just $ setVariables settings environment)
            (["--x11", "--dryrun"] ++ args)
          code `shouldNotBe` ExitSuccess
          err `shouldContain` message
    check [] [] "--x11 requires DISPLAY"
    forM_ ["", "localhost:10.0", "host:0", ":", ":0.", ":0.bad", ":0.1.2", ":-1"] $ \display ->
      check [("DISPLAY", display)] [] "--x11 requires a local DISPLAY"

  it "exposes only the D-Bus session socket and accumulates desktop settings" $ \ctx -> do
    environment <- getEnvironment
    let directory = root ctx </> "bus-runtime"
        socket = directory </> "bus"
        encoded = concatMap (\c -> if c == ' ' then "%20" else [c]) socket
        address = "unix:path=" ++ encoded
        supplied value = setVariables [("DBUS_SESSION_BUS_ADDRESS", value),
          ("XDG_RUNTIME_DIR", directory), ("WAYLAND_DISPLAY", "bus")] environment
    createDirectory directory
    writeFile (directory </> "private-marker") "hidden"
    bracket (Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol) Net.close $ \listener -> do
      Net.bind listener $ Net.SockAddrUnix socket
      forM_ [address, address ++ ",guid=0123456789abcdef0123456789abcdef",
             "unix:guid=0123456789abcdef0123456789abcdef,path=" ++ encoded] $ \value -> do
        out <- successful =<< invoke ctx Nothing (Just $ supplied value)
          ["--dbus", "--", "/bin/sh", "-c", unlines
           ["set -eu", "test -S /run/workcell-session-bus",
            "test ! -e \"$1/private-marker\"", "echo \"$DBUS_SESSION_BUS_ADDRESS\""],
           "check", directory]
        out `shouldBe` "unix:path=/run/workcell-session-bus\n"
      out <- successful =<< invoke ctx Nothing (Just $ supplied address)
        ["--wayland", "--dbus", "--", "/bin/sh", "-c", unlines
         ["set -eu", "test -S /run/workcell-session-bus",
          "test -S \"$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY\"",
          "printf '%s\\n' \"$DBUS_SESSION_BUS_ADDRESS\" \"$GDK_BACKEND\""]]
      lines out `shouldBe` ["unix:path=/run/workcell-session-bus", "wayland"]
      overridden <- successful =<< invoke ctx Nothing (Just $ supplied address)
        ["--dbus", "-e", "DBUS_SESSION_BUS_ADDRESS=unix:path=/custom", "--", "/bin/sh", "-c",
         "echo \"$DBUS_SESSION_BUS_ADDRESS\""]
      overridden `shouldBe` "unix:path=/custom\n"

  it "rejects missing, unsupported, or malformed D-Bus session addresses" $ \ctx -> do
    environment <- filter ((/= "DBUS_SESSION_BUS_ADDRESS") . fst) <$> getEnvironment
    let check settings message = do
          (code, _, err) <- invoke ctx Nothing (Just $ setVariables settings environment)
            ["--dbus", "--dryrun"]
          code `shouldNotBe` ExitSuccess
          err `shouldContain` message
    check [] "--dbus requires DBUS_SESSION_BUS_ADDRESS"
    forM_ ["", "autolaunch:", "tcp:host=localhost,port=1234", "unix:abstract=bus",
           "unix:path=/tmp/bus;unix:path=/tmp/other", "unix:path=/tmp/a,path=/tmp/b",
           "unix:path=/tmp/bus,abstract=bus", "unix:path=/tmp/bus,invalid"] $ \value ->
      check [("DBUS_SESSION_BUS_ADDRESS", value)] "single unix:path="
    forM_ ["%", "%2", "%GG"] $ \escaped ->
      check [("DBUS_SESSION_BUS_ADDRESS", "unix:path=/tmp/" ++ escaped)] "invalid escape"
    check [("DBUS_SESSION_BUS_ADDRESS", "unix:path=/tmp/%00bus")] "NUL byte"
    check [("DBUS_SESSION_BUS_ADDRESS", "unix:path=relative")] "absolute Unix socket path"
    check [("DBUS_SESSION_BUS_ADDRESS", "unix:path=/tmp/unescaped space")] "unescaped character"
    check [("DBUS_SESSION_BUS_ADDRESS", "unix:path=/tmp")] "not a socket"

  it "forwards arguments without interpreting options or shell syntax" $ \ctx -> do
    let args = ["--home", "-p", "", "a b", "'", "$HOME", "`uname`", "line\nbreak"]
    success ctx (["--", "/usr/bin/printf", "%s\\0"] ++ args)
      >>= (`shouldBe` concatMap (++ "\0") args)

  it "preserves command exit status" $ \ctx -> do
    (code, _, _) <- invoke ctx Nothing Nothing ["--", "/bin/sh", "-c", "exit 23"]
    code `shouldBe` ExitFailure 23

  it "inherits locales, permits overrides, and leaves absent locales unset" $ \ctx -> do
    environment <- filter (not . localeVariable . fst) <$> getEnvironment
    let settings = [("LANG", "C.UTF-8"), ("LANGUAGE", "en:fr"), ("LC_CTYPE", "C.UTF-8"),
                    ("LC_ALL", "C.UTF-8"), ("LC_TIME", "C"), ("LC_TEST_SETTING", "preserved")]
        supplied = settings ++ environment
        run envs args = successful =<< invoke ctx Nothing (Just envs) args
    out <- run supplied ["--", "env"]
    forM_ settings $ \(key, value) -> lines out `shouldContain` [key ++ "=" ++ value]
    run supplied ["--", "locale", "charmap"] `shouldReturn` "UTF-8\n"
    run supplied ["-e", "LC_ALL=C", "--", "locale", "charmap"] >>= (`shouldNotBe` "UTF-8\n")
    absent <- run environment ["--", "env"]
    lines absent `shouldSatisfy` (not . any (localeVariable . takeWhile (/= '=')))

  it "rejects invalid options, paths, mounts, and environment names" $ \ctx -> do
    let missing = root ctx </> "missing"
        cases = [["--unknown"], ["-p", "/"], ["-p", missing], ["-v", missing],
          ["-v", root ctx ++ ":/tmp/extra:O"], ["-v", root ctx ++ ":/"], ["-v", project ctx],
          ["-v", root ctx ++ ":" ++ home ctx], ["-v", root ctx],
          ["-v", root ctx ++ ":/tmp/duplicate", "-v", root ctx ++ ":/tmp/duplicate"],
          ["-v", root ctx ++ ":/usr/workcell-nonexistent-destination"], ["-H", "/"],
          ["-e", "BAD-NAME=value"], ["-p", "/proc"], ["-v", "/etc:/proc"],
          ["-e", "WORKCELL_TEST_UNSET_82394"], ["--network", "invalid"],
          ["--network", "none", "--share-network"]]
    forM_ cases $ \args -> void (failure ctx (args ++ ["--dryrun"]))

  it "validates before creating persistent home" $ \ctx -> do
    let supplied = root ctx </> "must not be created"
    _ <- failure ctx ["-H", supplied, "-v", root ctx </> "missing", "--", "/bin/true"]
    doesPathExist supplied `shouldReturn` False

  it "rejects projects that cover the isolated home" $ \ctx -> do
    let supplied = root ctx </> "must not be created"
    forM_ [[], ["-H", supplied]] $ \homeArgs -> do
      err <- failure ctx $ ["-p", home ctx] ++ homeArgs ++ ["--dryrun"]
      err `shouldContain` "project must not cover HOME"
    doesPathExist supplied `shouldReturn` False

  it "omits implicit projects at home and its ancestors" $ \ctx -> do
    let supplied = root ctx </> "isolated home"
        flags = "--share-network" : ["--share-processes" | not (privateProc ctx)]
    createDirectory supplied
    writeFile (supplied </> "marker") "isolated"
    forM_ [home ctx, takeDirectory (home ctx), "/"] $ \directory -> do
      forM_ [[], ["-H", supplied]] $ \homeArgs -> do
        let script = "[ \"$PWD\" = \"$HOME\" ] && [ ! -e \"$1\" ]" ++
              if null homeArgs then " && [ -z \"$(ls -A \"$HOME\")\" ]"
              else " && [ \"$(cat marker)\" = isolated ]"
            args = flags ++ homeArgs ++
              ["--", "/bin/sh", "-ec", script, "check", root ctx]
        _ <- successful =<< capture ((proc (binary ctx) args) {cwd = Just directory})
        pure ()

  it "runs the installed GHC" $ \ctx -> do
    available <- findExecutable "ghc"
    when (isNothing available) $ pendingWith "GHC is not installed"
    success ctx ["--", "ghc", "--version"] >>= (`shouldContain` "Glorious Glasgow Haskell")

  it "preserves individual system configuration files including binary localtime" $ \ctx -> do
    forM_ ["/etc/resolv.conf", "/etc/hosts", "/etc/host.conf", "/etc/localtime"] $ \path -> do
      exists <- doesFileExist path
      when exists $ do
        let dest = project ctx </> "configuration-copy"
        shell ctx "cat \"$1\" > configuration-copy" [path]
        expected <- BS.readFile path
        BS.readFile dest `shouldReturn` expected

  it "preserves anonymous configuration mounts through nested workcells" $ \ctx -> do
    let sparseEtc = root ctx </> "sparse etc"
        fixture = root ctx </> "binary timezone"
        replacement = root ctx </> "replacement timezone"
        expected = BS.pack [0, 1, 127, 128, 255, 10]
        replaced = BS.pack [255, 128, 127, 1, 0]
        processFlags = ["--share-processes" | not (privateProc ctx)]
        script = "cat /etc/localtime > localtime-copy; " ++
          "[ \"$1\" = '<system-file-0-fd>' ]; " ++
          "! (printf changed > /etc/localtime); [ ! -e /run/host/etc/unrelated ]"
    createDirectory sparseEtc
    createFileLink "/run/host/etc/localtime" (sparseEtc </> "localtime")
    BS.writeFile fixture expected
    BS.writeFile replacement replaced
    forM_ ("host" : ["none" | privateNet ctx]) $ \mode ->
      forM_ [False, True] $ \override ->
      bracket (Posix.openFd fixture Posix.ReadOnly Posix.defaultFileFlags) Posix.closeFd $ \fd -> do
        let outer = ["--ro-bind", "/", "/", "--proc", "/proc",
                     "--bind", project ctx, project ctx, "--ro-bind", sparseEtc, "/etc",
                     "--tmpfs", "/run", "--ro-bind-data", show fd, "/run/host/etc/localtime",
                     "--symlink", "/usr/bin/true", "/run/host/etc/unrelated", "--", binary ctx]
            volume = if override then ["-v", replacement ++ ":/etc/localtime"] else []
            inner = processFlags ++ ["--network", mode, "-v", binary ctx] ++ volume ++ ["--", binary ctx] ++
              processFlags ++ ["--share-network", "--", "/bin/sh", "-ec", script,
                               "check", "<system-file-0-fd>"]
        _ <- successful =<< capture ((fixtureProcess (outer ++ inner))
                                      {cwd = Just (project ctx), close_fds = False})
        BS.readFile (project ctx </> "localtime-copy") `shouldReturn` (if override then replaced else expected)

  it "tracks resolver replacements through systemd and toolbox directory mounts" $ \ctx -> do
    let sparseEtc = root ctx </> "sparse etc"
        resolverDir = project ctx </> "resolver"
        overrideFile = project ctx </> "override"
        flags = ["--share-processes" | not (privateProc ctx)]
    createDirectory sparseEtc
    createDirectory resolverDir
    writeFile overrideFile "override\n"
    forM_ [False, True] $ \toolbox -> do
      let directory = if toolbox then "/run/host/run/systemd/resolve" else "/run/systemd/resolve"
          target = directory </> "stub-resolv.conf"
          link = if toolbox then "/run/host/etc/resolv.conf" else target
          outer = ["--ro-bind", "/", "/", "--proc", "/proc",
                   "--bind", project ctx, project ctx, "--ro-bind", sparseEtc, "/etc",
                   "--tmpfs", "/run", "--ro-bind", resolverDir, directory] ++
                  (if toolbox then ["--symlink", target, link] else []) ++ ["--", binary ctx]
          run args = successful =<< capture ((fixtureProcess (outer ++ flags ++ args))
                                              {cwd = Just (project ctx)})
      createFileLink link (sparseEtc </> "resolv.conf")
      forM_ [False, True] $ \shared ->
        forM_ ("host" : ["none" | privateNet ctx]) $ \mode -> do
          writeFile (resolverDir </> "stub-resolv.conf") "initial\n"
          writeFile (resolverDir </> "replacement") "updated\n"
          out <- run $ ["--network", mode] ++ ["--share-filesystem" | shared] ++
            ["--", "/bin/sh", "-ec", unlines
              ["cat /etc/resolv.conf",
               "mv resolver/replacement resolver/stub-resolv.conf",
               "cat /etc/resolv.conf",
               "! (printf changed > /etc/resolv.conf)",
               "[ ! -e /run/host/etc/unrelated ]"]]
          out `shouldBe` "initial\nupdated\n"
      run ["--share-network", "-v", overrideFile ++ ":/etc/resolv.conf:ro",
           "--", "cat", "/etc/resolv.conf"] `shouldReturn` "override\n"
      dry <- run ["--network", "private", "--dryrun"]
      dry `shouldNotContain` ("'--ro-bind' '" ++ directory ++ "' '" ++ directory ++ "'")
      removeFile (sparseEtc </> "resolv.conf")

  it "starts when optional configuration files are absent or localtime is a dangling symlink" $ \ctx -> do
    let sparseEtc = root ctx </> "sparse etc"
        flags = "--share-network" : ["--share-processes" | not (privateProc ctx)]
        args = ["--ro-bind", "/", "/", "--proc", "/proc", "--ro-bind", sparseEtc, "/etc",
                "--", binary ctx] ++ flags ++
               ["--", "/bin/sh", "-c", "[ ! -e /etc/localtime ] && printf started"]
        check = do
          out <- successful =<< capture ((fixtureProcess args) {cwd = Just (project ctx)})
          out `shouldBe` "started"
          dry <- successful =<<
                 capture
                 ((fixtureProcess
                   ["--ro-bind", "/", "/", "--proc", "/proc",
                    "--ro-bind", sparseEtc, "/etc",
                    "--", binary ctx, "--network", "private", "--dryrun"])
                   {cwd = Just (project ctx)})
          dry `shouldContain` "'--ro-bind-data' '<dns-fd>' '/etc/resolv.conf'"
    createDirectory sparseEtc
    check
    createFileLink "/missing/timezone" (sparseEtc </> "localtime")
    check

  it "preserves localtime through hidden intermediate symlinks" $ \ctx -> do
    let sparseEtc = root ctx </> "sparse etc"
        flags = "--share-network" : ["--share-processes" | not (privateProc ctx)]
        outer = ["--ro-bind", "/", "/", "--proc", "/proc",
                 "--bind", project ctx, project ctx,
                 "--ro-bind", sparseEtc, "/etc", "--tmpfs", "/run",
                 "--dir", "/run/host/etc",
                 "--symlink", "/usr/bin/true", "/run/host/etc/localtime",
                 "--symlink", "/usr/bin/true", "/run/host/etc/unrelated",
                 "--", binary ctx]
    createDirectory sparseEtc
    createFileLink "/etc/timezone-link" (sparseEtc </> "localtime")
    createFileLink "../run/host/etc/localtime" (sparseEtc </> "timezone-link")
    expected <- BS.readFile "/usr/bin/true"
    forM_ [False, True] $ \shared -> do
      let args = outer ++ flags ++ ["--share-filesystem" | shared] ++
            ["--", "/bin/sh", "-ec",
             "cat /etc/localtime > configuration-copy; [ ! -e /run/host/etc/unrelated ]"]
      _ <- successful =<< capture ((fixtureProcess args) {cwd = Just (project ctx)})
      BS.readFile (project ctx </> "configuration-copy") `shouldReturn` expected

  it "hides enclosing data and provides ephemeral writable var/tmp" $ \ctx -> do
    shell ctx (unlines ["[ ! -e /var/log ]", "[ ! -e /sys/class ]", "[ ! -e /run/host/etc/passwd ]",
      "[ -z \"$(ls -A /opt)\" ]", "[ -z \"$(ls -A /srv)\" ]", "! touch /root-write",
      "! touch /opt/root-write", "printf temporary > /var/tmp/workcell-test"]) []
    shell ctx "[ ! -e /var/tmp/workcell-test ]" []

  it "creates new volume destinations only inside the sandbox" $ \ctx -> do
    let src = root ctx </> "source"
        dest = "/opt/workcell tests/new directory"
    createDirectory src
    writeFile (src </> "marker") "visible"
    volumeShell ctx (src ++ ":" ++ dest)
      "[ \"$(cat \"$1/marker\")\" = visible ] && ! touch \"$1/new\"" [dest]
    volumeShell ctx (src ++ ":" ++ dest ++ ":rw") "printf saved > \"$1/new\"" [dest]
    readFile (src </> "new") `shouldReturn` "saved"
    doesPathExist dest `shouldReturn` False
    success ctx ["-v", (src </> "marker") ++ ":/srv/new/path/file", "--", "cat", "/srv/new/path/file"]
      >>= (`shouldBe` "visible")
    _ <- failure ctx ["-v", (src </> "marker") ++ ":/etc", "--dryrun"]
    pure ()

  it "restores enclosing visibility only with share-filesystem" $ \ctx -> do
    let fixture = root ctx </> "outside"
        flags = "--share-network" : ["--share-processes" | not (privateProc ctx)]
        outer = ["--ro-bind", "/", "/", "--proc", "/proc", "--tmpfs", "/opt",
                 "--ro-bind", fixture, "/opt/host-marker", "--", binary ctx]
    writeFile fixture "host data"
    forM_ [False, True] $ \shared -> do
      let args = outer ++ flags ++ ["--share-filesystem" | shared] ++
            ["--", "/bin/sh", "-c", if shared then "[ -f /opt/host-marker ]" else "[ ! -e /opt/host-marker ]"]
      _ <- successful =<< capture ((fixtureProcess args) {cwd = Just (project ctx)})
      pure ()
    _ <- success ctx ["--share-filesystem", "--", "/bin/sh", "-c",
                      "[ ! -e \"$1\" ] && printf saved > shared-write", "check", fixture]
    readFile (project ctx </> "shared-write") `shouldReturn` "saved"

  it "preserves var/home on systems using that alias" $ \ctx -> do
    exists <- doesPathExist "/var/home"
    unless exists $ pendingWith "system does not use /var/home"
    symlink <- pathIsSymbolicLink "/var/home"
    resolved <- canonicalizePath "/var/home"
    unless (symlink && resolved == "/home") $ pendingWith "system does not alias /var/home to /home"
    shell ctx "[ \"$(readlink /var/home)\" = ../home ]" []

  it "runs Git in the writable project" $ \ctx -> do
    _ <- success ctx ["--", "git", "init", "--quiet"]
    _ <- success ctx ["--", "git", "status", "--short"]
    pure ()

shellHome :: Context -> FilePath -> String -> IO ()
shellHome ctx supplied script =
  void $
  success ctx
  ["-H", supplied, "--", "/bin/sh", "-c", "set -eu\n" ++ script]

volumeShell :: Context -> String -> String -> [String] -> IO ()
volumeShell ctx volume script args =
  void $
  success ctx
  (["-v", volume, "--", "/bin/sh", "-c", "set -eu\n" ++ script, "check"] ++ args)

localeVariable :: String -> Bool
localeVariable key = key == "LANG" || key == "LANGUAGE" || "LC_" `isPrefixOf` key

setVariables :: [(String, String)] -> [(String, String)] -> [(String, String)]
setVariables values environment = values ++ filter ((`notElem` map fst values) . fst) environment

requireLoopbackPasta :: IO ()
requireLoopbackPasta = do
  available <- findExecutable "pasta"
  helper <- maybe (pendingWith "pasta is not installed" >> fail "unreachable") pure available
  (code, _, err) <- capture $ proc helper
    ["--splice-only", "--no-map-gw", "--no-pidns", "-t", "none", "-u", "none",
     "-T", "none", "-U", "none", "--", "/bin/true"]
  unless (code == ExitSuccess) $ pendingWith $ "pasta loopback launcher unavailable: " ++ err

loopbackCommand :: Context -> [String] -> IO CreateProcess
loopbackCommand ctx command = do
  executable <- getExecutablePath
  environment <- getEnvironment
  let args = ["--test-loopback-network", "--die-with-parent", "--ro-bind", "/", "/",
              "--dev", "/dev", "--unshare-net", "--tmpfs", "/tmp", "--tmpfs", "/run",
              "--bind", project ctx, project ctx, "--chdir", project ctx] ++
        (if privateProc ctx then ["--unshare-pid", "--proc", "/proc"] else []) ++ ["--"] ++ command
  pure $ (proc executable args) {cwd = Just (project ctx),
    env = Just $ setVariables [("XDG_STATE_HOME", root ctx </> "state")] environment}

pastaLogs :: Context -> IO [FilePath]
pastaLogs ctx = do
  let directory = root ctx </> "state/workcell/logs"
  entries <- listDirectory directory
  pure $ map (directory </>) entries

fakeHelperCommand :: Context -> String -> [String] -> IO CreateProcess
fakeHelperCommand ctx script command = do
  let tools = root ctx </> "helper tools"
      helper = tools </> "pasta"
  createDirectory tools
  writeFile helper $ "#!/bin/sh\n" ++ script
  setFileMode helper 0o700
  invocation <- loopbackCommand ctx command
  environment <- getEnvironment
  pure invocation {env = Just $ setVariables [("PATH", tools ++ ":/usr/bin:/bin")]
                    (fromMaybe environment $ env invocation)}

waitUntil :: IO Bool -> IO ()
waitUntil condition = do
  let wait = do
        done <- condition
        unless done $ threadDelay 10000 >> wait
  timeout (10 * 1000000) wait >>= maybe (expectationFailure "condition timed out") pure


-- Session operations must not inherit invoke's launch-only sharing flags.
sessionEnvironment :: Context -> IO [(String, String)]
sessionEnvironment ctx = do
  environment <- getEnvironment
  pure $ [("XDG_RUNTIME_DIR", root ctx </> "session-runtime"),
          ("XDG_STATE_HOME", root ctx </> "session-state")] ++
    filter (\(key, _) -> key `notElem` ["XDG_RUNTIME_DIR", "XDG_STATE_HOME"]) environment

sessionInvoke :: Context -> [String] -> IO (ExitCode, String, String)
sessionInvoke ctx args = do
  environment <- sessionEnvironment ctx
  capture $ (proc (binary ctx) args) {cwd = Just (project ctx), env = Just environment}

sessionRecordPaths :: Context -> IO [FilePath]
sessionRecordPaths ctx = do
  let parent = root ctx </> "session-runtime/workcell/sessions"
  namespaces <- listDirectory parent
  concat <$> mapM (\ns -> do
    let dir = parent </> ns
    names <- listDirectory dir
    pure [dir </> name | name <- names, "s" `isPrefixOf` name]) namespaces

withRunningSession :: Context -> [String] -> (String -> IO a) -> IO a
withRunningSession ctx extra action =
  withRunningSessionHandle ctx extra $ \ident _ -> action ident

withRunningSessionHandle :: Context -> [String] -> (String -> ProcessHandle -> IO a) -> IO a
withRunningSessionHandle ctx extra action = do
  environment <- sessionEnvironment ctx
  existing <- lines <$> (successful =<< sessionInvoke ctx ["--list"])
  let args = (if "--network" `elem` extra then [] else ["--network", "host"]) ++
        ["--share-processes" | not (privateProc ctx) && "--share-processes" `notElem` extra] ++
        ["-H", root ctx </> "session-home"] ++ extra ++
        ["--", "/bin/sh", "-c", "touch entry-home-marker \"$HOME/entry-home-marker\"; exec sleep 30"]
  bracket (createProcess $ (proc (binary ctx) args)
    {cwd = Just (project ctx), env = Just environment, std_out = NoStream})
    (\(_, _, _, handle) -> terminateProcess handle >> waitForProcess handle >> pure ()) $ \(_, _, _, handle) -> do
      found <- timeout (5 * 1000000) $ waitForSession existing handle
      ident <- maybe (fail "session did not become ready") pure found
      action ident handle
  where
    waitForSession existing handle = do
      status <- getProcessExitCode handle
      case status of
        Just code -> fail $ "workcell exited before registration: " ++ show code
        Nothing -> pure ()
      output <- successful =<< sessionInvoke ctx ["--list"]
      marker <- doesFileExist $ project ctx </> "entry-home-marker"
      -- Select the most recently added record, rather than an existing session.
      case filter (`notElem` existing) (drop 1 $ lines output) of
        row:_ | marker, ident:_ <- words row -> pure ident
        _ -> threadDelay 10000 >> waitForSession existing handle

-- Read-only enclosing fixtures need writable storage for supervision and records.
fixtureProcess :: [String] -> CreateProcess
fixtureProcess args =
  let (mounts, command) = break (== "--") args
  in proc "bwrap" $ mounts ++
    ["--tmpfs", "/tmp", "--setenv", "TMPDIR", "/tmp",
     "--setenv", "XDG_RUNTIME_DIR", "/tmp/workcell-fixture-runtime"] ++ command
