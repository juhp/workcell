-- SPDX-License-Identifier: BSD-3-Clause
module Workcell (Options(..), runWorkcell) where

import Control.Exception (bracket, throwTo)
import Control.Concurrent (myThreadId)
import Control.Monad (forM, unless, void, when)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Network (NetworkMode(..), networkMode, pastaArguments, runTrackedPrivateNetwork, runTrackedSandbox, verifyNetwork)
import SimpleCmd (cmdFull, needProgram)
import Session (SessionDetails(..), withSession, listSessions, enterSession)
import System.Directory
  (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist,
   doesPathExist, getHomeDirectory, getSymbolicLinkTarget, pathIsSymbolicLink)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..), exitWith)
import System.Posix.Signals (Handler(CatchOnce), installHandler, sigTERM)
import System.FilePath ((</>), isAbsolute, normalise, splitDirectories, takeDirectory)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (getFileStatus, isRegularFile, isSocket, linkCount)
import System.Posix.IO (OpenMode(..), closeFd, defaultFileFlags, openFd)

data Options = Options
  { projectOption :: Maybe FilePath
  , homeOption :: Maybe FilePath
  , volumeOptions :: [String]
  , environmentOptions :: [String]
  , waylandOption :: Bool
  , x11Option :: Bool
  , dryRun :: Bool
  , shareProcesses :: Bool
  , shareFilesystem :: Bool
  , networkOption :: Maybe String
  , shareNetwork :: Bool
  , listOption :: Bool
  , enterOption :: Maybe String
  , command :: [String]
  }

data Mount = Mount
  { source :: FilePath
  , destination :: FilePath
  , writable :: Bool
  }

data Launch = Launch
  { arguments :: [String]
  , persistentHome :: Maybe FilePath
  , selectedNetwork :: NetworkMode
  , resolverDestination :: FilePath
  , copiedSystemFiles :: [(Int, FilePath)]
  , launchProject :: Maybe FilePath
  }

runWorkcell :: Options -> IO ()
runWorkcell opts
  | listOption opts || isJust (enterOption opts) = do
      when (listOption opts && isJust (enterOption opts)) $
        invalid "--list and --enter cannot be combined"
      when (isJust (projectOption opts) || isJust (homeOption opts) ||
            not (null $ volumeOptions opts) || not (null $ environmentOptions opts) ||
            waylandOption opts || x11Option opts || shareProcesses opts || shareFilesystem opts ||
            isJust (networkOption opts) || shareNetwork opts) $
        invalid "session operations cannot be combined with launch options"
      if listOption opts
        then do
          when (dryRun opts || not (null $ command opts)) $ invalid "--list does not accept --dryrun or a command"
          listSessions
        else maybe (invalid "missing session ID")
          (\ident -> withTermination $ enterSession ident (dryRun opts) (command opts)) (enterOption opts)
  | otherwise = withTermination $ launchWorkcell opts

-- Turn termination into an exception so session/helper brackets can clean up.
withTermination :: IO a -> IO a
withTermination action = do
  thread <- myThreadId
  bracket (installHandler sigTERM (CatchOnce $ throwTo thread $ ExitFailure 143) Nothing)
    (\previous -> void (installHandler sigTERM previous Nothing)) $ const action

launchWorkcell :: Options -> IO ()
launchWorkcell opts = do
  launch <- prepare opts
  if dryRun opts
    then if selectedNetwork launch == PrivateNetwork || not (null $ copiedSystemFiles launch)
      then do
        let networkPlan =
              if selectedNetwork launch == PrivateNetwork
              then ["--info-fd", "<info-fd>",
                    "--ro-bind", "<ready-pipe>", "/run/workcell-network-ready",
                    "--ro-bind-data", "<dns-fd>", resolverDestination launch]
              else []
            preview = "bwrap" : networkPlan ++ arguments launch
        putStrLn $ "# " ++ unwords (map shellQuote preview)
        when (selectedNetwork launch == PrivateNetwork) $
          putStrLn $ "# " ++ unwords (map shellQuote $ "pasta" : pastaArguments "<netns>" "<userns>" "<pid-file>")
        executable <- getExecutablePath
        argv <- getArgs
        let (options, cmd) = break (== "--") argv
        putStrLn $ unwords $ map shellQuote $ executable : filter (/= "--dryrun") options ++ cmd
      else putStrLn $ unwords $ map shellQuote $ "bwrap" : arguments launch
    else do
      needProgram "bwrap"
      unless (shareProcesses opts) verifyProcessIsolation
      verifyNetwork (selectedNetwork launch)
        (if shareProcesses opts then [] else privateProcArgs)
      withSystemFiles (copiedSystemFiles launch) $ \descriptors -> do
        let liveArgs = [fromMaybe arg (lookup index descriptors) |
                        (index, arg) <- zip [0..] (arguments launch)]
        mapM_ (createDirectoryIfMissing True) (persistentHome launch)
        let mode = selectedNetwork launch
            modeName = case mode of
              PrivateNetwork -> "private"
              NoNetwork -> "none"
              HostNetwork -> "host"
            details = SessionDetails (launchProject launch)
              (if null (command opts) then ["/bin/bash"] else command opts)
              modeName (shareProcesses opts)
            track = withSession details
        result <- if mode == PrivateNetwork
          then runTrackedPrivateNetwork track [] liveArgs (resolverDestination launch)
          else runTrackedSandbox track liveArgs
        exitWith result

withSystemFiles :: [(Int, FilePath)] -> ([(Int, String)] -> IO a) -> IO a
withSystemFiles [] action = action []
withSystemFiles ((index, path):rest) action =
  bracket (openFd path ReadOnly defaultFileFlags) closeFd $ \fd ->
    withSystemFiles rest $ \descriptors -> action ((index, show fd) : descriptors)

-- Validate paths and construct argv before creating any persistent home.
prepare :: Options -> IO Launch
prepare opts = do
  mode <- networkMode (networkOption opts) (shareNetwork opts)
  candidate <- existingDirectory "project" (fromMaybe "." $ projectOption opts)
  originalHome <- getHomeDirectory
  unless (isAbsolute originalHome) $ invalid "HOME must be an absolute path"
  home <- existingDirectory "HOME" originalHome
  rejectRoot "HOME" home
  when (isJust (projectOption opts) && within candidate home) $
    invalid "project must not cover HOME; select a project directory with --project"
  let project = if within candidate home then Nothing else Just candidate
  suppliedHome <- traverse resolveHome (homeOption opts)
  displaySettings <- sequence $
    [waylandSetup | waylandOption opts] ++ [x11Setup home | x11Option opts]
  let displayMounts = concatMap fst displaySettings
      backends = ["wayland" | waylandOption opts] ++ ["x11" | x11Option opts]
      displayEnv = concatMap snd displaySettings ++
        [("GDK_BACKEND", intercalate "," backends) | not (null backends)]
  mounts <- (displayMounts ++) <$> mapM resolveMount (volumeOptions opts)
  unless (shareProcesses opts) $ do
    when (maybe False (within "/proc") project || within home "/proc" || within "/proc" home) $
      invalid "home and project must not overlap private /proc"
    mapM_ (\mount -> when (within (destination mount) "/proc" || within "/proc" (destination mount)) $
      invalid "volume destination must not overlap private /proc") mounts
  filesystem <- filesystemArgs (shareFilesystem opts)
  resolverArgs <- if mode == PrivateNetwork then pure []
                  else liveResolverArgs (shareFilesystem opts) mounts
  inheritedSystemFiles <- preservedSystemFiles (shareFilesystem opts)
    (mode /= PrivateNetwork && null resolverArgs)
  let systemFiles =
        filter
        (\system -> not $
          any (\mount -> within (destination mount) (destination system) ||
                         within (destination mount) (source system)) mounts)
        inheritedSystemFiles
  systemMounts <-
    forM (zip [0 :: Int ..] systemFiles) $ \(index, mount) -> do
    status <- getFileStatus (source mount)
    -- Files produced by --ro-bind-data have no directory entry to re-bind.
    -- Open and copy their contents into the next sandbox instead.
    pure $ if linkCount status == 0
      then (["--ro-bind-data", "<system-file-" ++ show index ++ "-fd>", destination mount],
            Just (index, source mount))
      else (mountArgs mount, Nothing)
  resolverTarget <- if mode /= PrivateNetwork then pure "/etc/resolv.conf" else do
    let hidden path = if shareFilesystem opts then any (`within` path) temporaryRoots
                      else not $ any (`within` path) systemDirectories
    target <- catchIOError (hiddenSystemTarget hidden "/etc/resolv.conf") $ \err ->
      if isDoesNotExistError err then pure Nothing else ioError err
    maybe (canonicalizePath "/etc/resolv.conf") pure target
  let destinations = map destination mounts
  unless (length destinations == length (nub destinations)) $
    invalid "duplicate volume destinations"
  mapM_ (validateMount (shareFilesystem opts) project home) mounts
  env <- mapM resolveEnvironment (environmentOptions opts)
  locale <- filter (isLocaleVariable . fst) <$> getEnvironment
  term <- fromMaybe "dumb" <$> lookupEnv "TERM"
  let homeArgs = maybe ["--tmpfs", home]
        (\path -> ["--bind", path, home]) suppliedHome
      defaults = [("HOME", home), ("PATH", defaultPath home), ("TERM", term),
                  ("WORKCELL_ACTIVE", "1")] ++ locale
      cmd = if null (command opts)
        then ["/bin/bash"] else command opts
      beforeSystemFiles = ["--die-with-parent"] ++ filesystem
        ++ concatMap (\path -> ["--tmpfs", path]) temporaryRoots
        ++ ["--dev", "/dev"]
        ++ (if shareProcesses opts then ["--ro-bind", "/proc", "/proc"] else privateProcArgs)
        ++ ["--dir", home]
        ++ homeArgs
        ++ resolverArgs
      args = beforeSystemFiles ++ concatMap fst systemMounts
        ++ ["--unshare-net" | mode /= HostNetwork]
        ++ concatMap mountArgs mounts
        ++ maybe [] (\path -> ["--bind", path, path]) project
        ++ ["--chdir", fromMaybe home project, "--clearenv"]
        ++ concatMap (\(key, value) -> ["--setenv", key, value]) (defaults ++ displayEnv ++ env)
        ++ (if shareFilesystem opts then [] else ["--remount-ro", "/"])
        ++ ["--"] ++ cmd
      copies = [(length beforeSystemFiles + index * 3 + 1, path) |
                 (index, path) <- mapMaybe snd systemMounts]
  pure $ Launch args suppliedHome mode resolverTarget copies project

waylandSetup :: IO ([Mount], [(String, String)])
waylandSetup = do
  runtime <- lookupEnv "XDG_RUNTIME_DIR" >>= maybe
    (invalid "--wayland requires XDG_RUNTIME_DIR") pure
  unless (isAbsolute runtime) $ invalid "--wayland requires an absolute XDG_RUNTIME_DIR"
  display <- fromMaybe "wayland-0" <$> lookupEnv "WAYLAND_DISPLAY"
  when (null display) $ invalid "--wayland requires a nonempty WAYLAND_DISPLAY"
  let socket = normalise $ if isAbsolute display then display else runtime </> display
  status <- getFileStatus socket
  unless (isSocket status) $ invalid $ "Wayland display is not a socket: " ++ socket
  src <- canonicalizePath socket
  pure ([Mount src socket False],
        [("XDG_RUNTIME_DIR", runtime), ("WAYLAND_DISPLAY", display)])

x11Setup :: FilePath -> IO ([Mount], [(String, String)])
x11Setup home = do
  display <- lookupEnv "DISPLAY" >>= maybe (invalid "--x11 requires DISPLAY") pure
  number <- case break (== ':') display of
    (host, ':':suffix) | host `elem` ["", "unix", "unix/"] ->
      case span isDigit suffix of
        (digits, screen) | not (null digits) &&
          (null screen || case screen of
            '.':rest -> not (null rest) && all isDigit rest
            _ -> False) -> pure digits
        _ -> badDisplay
    _ -> badDisplay
  let socket = "/tmp/.X11-unix" </> ("X" ++ number)
      authTarget = "/run/workcell-xauthority"
  status <- getFileStatus socket
  unless (isSocket status) $ invalid $ "X11 display is not a socket: " ++ socket
  authority <- lookupEnv "XAUTHORITY" >>= canonicalizePath . fromMaybe (home </> ".Xauthority")
  authStatus <- getFileStatus authority
  unless (isRegularFile authStatus) $ invalid $ "X11 authority is not a regular file: " ++ authority
  pure ([Mount socket socket False, Mount authority authTarget False],
        [("DISPLAY", display), ("XAUTHORITY", authTarget)])
  where
    badDisplay = invalid "--x11 requires a local DISPLAY such as :0 or :0.0 (TCP displays are unsupported)"

privateProcArgs :: [String]
privateProcArgs = ["--unshare-pid", "--proc", "/proc"]

-- Check before creating a persistent home or running the requested command.
verifyProcessIsolation :: IO ()
verifyProcessIsolation = do
  (ok, _, err) <- cmdFull "bwrap"
    (["--die-with-parent", "--ro-bind", "/", "/", "--dev", "/dev"]
     ++ privateProcArgs ++ ["--", "/bin/true"]) ""
  unless ok $ invalid $
    "private PID namespace and /proc are unavailable in this environment. "
    ++ "Use --share-processes only if sharing enclosing processes is acceptable.\n" ++ err

defaultPath :: FilePath -> String
defaultPath home = intercalate ":"
  [home </> ".local/bin", home </> "bin",
   "/usr/local/bin", "/usr/bin", "/bin", "/usr/local/sbin", "/usr/sbin", "/sbin"]

isLocaleVariable :: String -> Bool
isLocaleVariable key = key == "LANG" || key == "LANGUAGE" || "LC_" `isPrefixOf` key

temporaryRoots :: [FilePath]
temporaryRoots = ["/home", "/root", "/tmp", "/run"]

systemDirectories :: [FilePath]
systemDirectories = ["/usr", "/etc", "/bin", "/sbin", "/lib", "/lib64"]

filesystemArgs :: Bool -> IO [String]
filesystemArgs True = pure ["--ro-bind", "/", "/"]
filesystemArgs False = do
  aliases <- fmap concat $ forM (drop 2 systemDirectories) $ \path -> do
    exists <- doesDirectoryExist path
    if not exists then pure [] else do
      symlink <- pathIsSymbolicLink path
      if symlink
        then do
          target <- getSymbolicLinkTarget path
          pure ["--symlink", target, path]
        else pure ["--ro-bind", path, path]
  varHomeExists <- doesPathExist "/var/home"
  varHomeAlias <- if not varHomeExists then pure [] else do
    symlink <- pathIsSymbolicLink "/var/home"
    resolved <- canonicalizePath "/var/home"
    pure $ if symlink && resolved == "/home"
      then ["--symlink", "../home", "/var/home"] else []
  pure $ ["--ro-bind", "/usr", "/usr", "--ro-bind", "/etc", "/etc"]
    ++ aliases ++ concatMap (\path -> ["--dir", path]) ["/var", "/opt", "/srv", "/sys"]
    ++ ["--perms", "1777", "--tmpfs", "/var/tmp"] ++ varHomeAlias

-- Expose only the resolver directory, following atomic file replacements.
-- Toolbx may route /etc/resolv.conf through /run/host/etc first; recreate
-- that hidden link without exposing the rest of the host's /etc directory.
liveResolverArgs :: Bool -> [Mount] -> IO [String]
liveResolverArgs shared mounts = do
  exists <- doesPathExist "/etc/resolv.conf"
  if not exists then pure [] else do
    resolved <- canonicalizePath "/etc/resolv.conf"
    let directory = takeDirectory resolved
        hidden path = if shared then any (`within` path) temporaryRoots
                      else not $ any (`within` path) systemDirectories
        covered path = any (\mount -> within (destination mount) path) mounts
    target <- hiddenSystemTarget hidden "/etc/resolv.conf"
    status <- getFileStatus resolved
    pure $ case target of
      Just dest | directory `elem` ["/run/systemd/resolve", "/run/host/run/systemd/resolve"]
                  && linkCount status /= 0
                  && not (covered dest) ->
        (if covered resolved then [] else ["--ro-bind", directory, directory]) ++
        [arg | dest /= resolved, arg <- ["--symlink", resolved, dest]]
      _ -> []

-- Toolbx links other configuration into /run/host. Mount only those files.
preservedSystemFiles :: Bool -> Bool -> IO [Mount]
preservedSystemFiles shared preserveResolver = fmap concat $ forM
  (["/etc/resolv.conf" | preserveResolver] ++
   ["/etc/hosts", "/etc/host.conf", "/etc/localtime"]) $ \path -> do
  exists <- doesPathExist path
  if not exists then pure [] else do
    resolved <- canonicalizePath path
    let hidden target = if shared then any (`within` target) temporaryRoots
                        else not $ any (`within` target) systemDirectories
    target <- hiddenSystemTarget hidden path
    pure $ maybe [] (\dest -> [Mount resolved dest False]) target

-- Stop at the first hidden part of a symlink chain. Its final target may be
-- exposed under /usr, while an intermediate link under /run is still hidden.
-- Inspect parent components too, and resolve relative links against their parent.
hiddenSystemTarget :: (FilePath -> Bool) -> FilePath -> IO (Maybe FilePath)
hiddenSystemTarget hidden = restart
  where
    restart path = walk "/" (drop 1 $ splitDirectories path)
    walk _ [] = pure Nothing
    walk parent (".":rest) = walk parent rest
    walk parent ("..":rest) = walk (takeDirectory parent) rest
    walk parent (part:rest) = do
      let path = parent </> part
      if hidden path
        then pure $ Just $ foldl (</>) path rest
        else do
          symlink <- pathIsSymbolicLink path
          if symlink then do
            target <- getSymbolicLinkTarget path
            let absolute = if isAbsolute target then target else parent </> target
            restart $ foldl (</>) absolute rest
          else walk path rest

existingDirectory :: String -> FilePath -> IO FilePath
existingDirectory label path = do
  exists <- doesDirectoryExist path
  unless exists $ invalid $ label ++ " is not an existing directory: " ++ path
  canonicalizePath path

resolveHome :: FilePath -> IO FilePath
resolveHome path = do
  when (null path) $ invalid "home directory must not be empty"
  resolved <- canonicalizePath path
  rejectRoot "home directory" resolved
  exists <- doesPathExist resolved
  isDir <- doesDirectoryExist resolved
  when (exists && not isDir) $ invalid $ "home is not a directory: " ++ path
  pure resolved

resolveMount :: String -> IO Mount
resolveMount spec = do
  (src, dst, rw) <- either invalid pure (parseMount spec)
  exists <- doesPathExist src
  unless exists $ invalid $ "volume source does not exist: " ++ src
  srcPath <- canonicalizePath src
  dstPath <- canonicalizePath (fromMaybe src dst)
  rejectRoot "volume destination" dstPath
  pure $ Mount srcPath dstPath rw

parseMount :: String -> Either String (FilePath, Maybe FilePath, Bool)
parseMount spec = case splitColon spec of
  [src] | valid src -> Right (src, Nothing, False)
  [src, mode] | valid src, Just rw <- access mode -> Right (src, Nothing, rw)
  [src, dst] | valid src && valid dst -> Right (src, Just dst, False)
  [src, dst, mode] | valid src && valid dst, Just rw <- access mode ->
    Right (src, Just dst, rw)
  _ -> Left $ "invalid volume (expected SOURCE[:DEST][:ro|rw]): " ++ spec
  where
    valid = not . null
    access "ro" = Just False
    access "rw" = Just True
    access _ = Nothing

splitColon :: String -> [String]
splitColon input = case break (== ':') input of
  (part, []) -> [part]
  (part, _:rest) -> part : splitColon rest

-- Directory-component comparisons avoid treating /foo and /foobar as overlapping.
within :: FilePath -> FilePath -> Bool
within parent child = splitDirectories (normalise parent)
  `isPrefixOf` splitDirectories (normalise child)

validateMount :: Bool -> Maybe FilePath -> FilePath -> Mount -> IO ()
validateMount shared project home mount = do
  let dest = destination mount
  when (within dest home || maybe False (\path -> within dest path || within path dest) project) $
    invalid $ "volume destination overlaps home or project: " ++ dest
  let temporary = any (`within` dest) (home : temporaryRoots)
  exists <- doesPathExist dest
  when (not exists && not temporary &&
        (shared || any (`within` dest) systemDirectories)) $
    invalid $ "volume destination cannot be created beneath a read-only system mount: " ++ dest
  srcDir <- doesDirectoryExist (source mount)
  dstDir <- doesDirectoryExist dest
  when (exists && srcDir /= dstDir) $
    invalid $ "volume source and destination must both be files or both directories: " ++ dest

mountArgs :: Mount -> [String]
mountArgs mount = [if writable mount then "--bind" else "--ro-bind",
                  source mount, destination mount]

resolveEnvironment :: String -> IO (String, String)
resolveEnvironment spec = do
  let (key, suffix) = break (== '=') spec
      letter c = isAsciiLower c || isAsciiUpper c || c == '_'
      valid = case key of
        [] -> False
        first:rest -> letter first && all (\c -> letter c || isDigit c) rest
  unless valid $ invalid $ "invalid environment variable name: " ++ key
  value <- case suffix of
    '=':rest -> pure rest
    _ -> lookupEnv key >>= maybe (invalid $ "environment variable is unset: " ++ key) pure
  pure (key, value)

rejectRoot :: String -> FilePath -> IO ()
rejectRoot label path = when (path == "/") $ invalid $ label ++ " must not be /"

invalid :: String -> IO a
invalid = ioError . userError

shellQuote :: String -> String
shellQuote value = "'" ++ concatMap escape value ++ "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]
