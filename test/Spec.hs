-- SPDX-License-Identifier: LGPL-2.1-or-later
module Main (main) where

import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import qualified Network as WorkcellNetwork
import qualified Network.Socket as Net
import System.Directory hiding (executable)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..), die, exitWith)
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files (readSymbolicLink, setFileMode)
import qualified System.Posix.IO as Posix
import System.Posix.Temp (mkdtemp)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)
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
    sharing = maybe (not (privateProc ctx) && "--dryrun" `notElem` args) id shared
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
    writeFile helper "#!/bin/sh\nexit 42\n"
    setFileMode helper 0o700
    environment <- getEnvironment
    invocation <- loopbackCommand ctx ["/bin/sh", "-c", "touch command-ran"]
    let envs = setVariables [("PATH", tools ++ ":/usr/bin:/bin")] environment
    (code, _, err) <- capture (invocation {env = Just envs})
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "pasta exited during network setup"
    doesPathExist (project ctx </> "command-ran") `shouldReturn` False

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
    let flags = ["--share-network"] ++ ["--share-processes" | not (privateProc ctx)]
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
    lines absent `shouldSatisfy` all (not . localeVariable . takeWhile (/= '='))

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
    forM_ cases $ \args -> failure ctx (args ++ ["--dryrun"]) >> pure ()

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
        flags = ["--share-network"] ++ ["--share-processes" | not (privateProc ctx)]
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
    when (available == Nothing) $ pendingWith "GHC is not installed"
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
        _ <- successful =<< capture ((proc "bwrap" (outer ++ inner))
                                      {cwd = Just (project ctx), close_fds = False})
        BS.readFile (project ctx </> "localtime-copy") `shouldReturn` (if override then replaced else expected)

  it "starts when optional configuration files are absent or localtime is a dangling symlink" $ \ctx -> do
    let sparseEtc = root ctx </> "sparse etc"
        flags = ["--share-network"] ++ ["--share-processes" | not (privateProc ctx)]
        args = ["--ro-bind", "/", "/", "--proc", "/proc", "--ro-bind", sparseEtc, "/etc",
                "--", binary ctx] ++ flags ++
               ["--", "/bin/sh", "-c", "[ ! -e /etc/localtime ] && printf started"]
        check = do
          out <- successful =<< capture ((proc "bwrap" args) {cwd = Just (project ctx)})
          out `shouldBe` "started"
          dry <- successful =<< capture ((proc "bwrap"
            (["--ro-bind", "/", "/", "--proc", "/proc", "--ro-bind", sparseEtc, "/etc",
              "--", binary ctx, "--network", "private", "--dryrun"])) {cwd = Just (project ctx)})
          dry `shouldContain` "'--ro-bind-data' '<dns-fd>' '/etc/resolv.conf'"
    createDirectory sparseEtc
    check
    createFileLink "/missing/timezone" (sparseEtc </> "localtime")
    check

  it "preserves localtime through hidden intermediate symlinks" $ \ctx -> do
    let sparseEtc = root ctx </> "sparse etc"
        flags = ["--share-network"] ++ ["--share-processes" | not (privateProc ctx)]
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
      _ <- successful =<< capture ((proc "bwrap" args) {cwd = Just (project ctx)})
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
        flags = ["--share-network"] ++ ["--share-processes" | not (privateProc ctx)]
        outer = ["--ro-bind", "/", "/", "--proc", "/proc", "--tmpfs", "/opt",
                 "--ro-bind", fixture, "/opt/host-marker", "--", binary ctx]
    writeFile fixture "host data"
    forM_ [False, True] $ \shared -> do
      let args = outer ++ flags ++ ["--share-filesystem" | shared] ++
            ["--", "/bin/sh", "-c", if shared then "[ -f /opt/host-marker ]" else "[ ! -e /opt/host-marker ]"]
      _ <- successful =<< capture ((proc "bwrap" args) {cwd = Just (project ctx)})
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
shellHome ctx supplied script = success ctx
  ["-H", supplied, "--", "/bin/sh", "-c", "set -eu\n" ++ script] >> pure ()

volumeShell :: Context -> String -> String -> [String] -> IO ()
volumeShell ctx volume script args = success ctx
  (["-v", volume, "--", "/bin/sh", "-c", "set -eu\n" ++ script, "check"] ++ args) >> pure ()

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
  let args = ["--test-loopback-network", "--die-with-parent", "--ro-bind", "/", "/",
              "--dev", "/dev", "--unshare-net", "--tmpfs", "/tmp", "--tmpfs", "/run",
              "--bind", project ctx, project ctx, "--chdir", project ctx] ++
        (if privateProc ctx then ["--unshare-pid", "--proc", "/proc"] else []) ++ ["--"] ++ command
  pure $ (proc executable args) {cwd = Just (project ctx)}

waitUntil :: IO Bool -> IO ()
waitUntil condition = do
  let wait = do
        done <- condition
        unless done $ threadDelay 10000 >> wait
  timeout (10 * 1000000) wait >>= maybe (expectationFailure "condition timed out") pure

