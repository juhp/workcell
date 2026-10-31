-- SPDX-License-Identifier: LGPL-2.1-or-later
module Run (
  Options(..),
  NetworkMode(..),
  runWorkcell)
where

import Control.Exception (catch, onException)
import Control.Monad.Extra (allM, forM, unless, unlessM, when)
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, ord)
import qualified Data.ByteString as BS
import Data.List (intercalate, isPrefixOf, nub)
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Foreign (peekCString)
import GHC.IO.Encoding (getFileSystemEncoding)
import SimpleCmd (needProgram, warning, (+-+))
import System.Directory
  (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist,
   doesPathExist, getHomeDirectory, getSymbolicLinkTarget, listDirectory, pathIsSymbolicLink,
   removeDirectory)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (exitWith)
import System.FilePath ((</>), isAbsolute, normalise, splitDirectories, takeDirectory, takeFileName)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (getFileStatus, isRegularFile, isSocket, linkCount)

import Sandbox
import Identity (identityFiles)
import Session (SessionDetails(..), withSession)
import Util (invalid, shellQuote)

data Options = Options
  { projectOption :: Maybe FilePath
  , homeOption :: Maybe FilePath
  , volumeOptions :: [String]
  , environmentOptions :: [String]
  , waylandOption :: Bool
  , x11Option :: Bool
  , dbusOption :: Bool
  , dryRun :: Bool
  , shareProcesses :: Bool
  , shareFilesystem :: Bool
  , networkOption :: Maybe NetworkMode
  , command :: [String]
  }

data MountMode = ReadOnlyMount | ReadWriteMount | OverlayMount
  deriving Eq

data Mount = Mount
  { source :: FilePath
  , destination :: FilePath
  , mountMode :: MountMode
  }

data Launch = Launch
  { launchSandbox :: Sandbox
  , persistentHome :: Maybe FilePath
  , selectedNetwork :: NetworkMode
  , resolverTarget :: FilePath
  , launchProject :: Maybe FilePath
  }

privateNetwork :: Launch -> Maybe PrivateNetwork
privateNetwork launch
  | selectedNetwork launch == PrivateNetwork =
      Just $ PrivateNetworkConfig [] (resolverTarget launch)
  | otherwise = Nothing

runWorkcell :: Options -> IO ()
runWorkcell opts = do
  planned <- prepare opts
  if dryRun opts
    then preview opts planned
    else do
    needProgram "bwrap"
    let mode = selectedNetwork planned
        automatic = isNothing (networkOption opts)
    -- Check before creating a persistent home or running the requested command.
    failure <- probeNamespaces (not $ shareProcesses opts) (mode /= HostNetwork)
    available <- case failure of
      Just (ProcessIsolationUnavailable err) -> invalid $
        "private PID namespace and /proc are unavailable in this environment. "
        ++ "Use --share-processes only if sharing enclosing processes is acceptable.\n" ++ err
      Just (NetworkIsolationUnavailable err) -> pure $ Just err
      Nothing | mode == PrivateNetwork -> checkPrivateNetwork
      Nothing -> pure Nothing
    -- The command is still paused when private network setup fails,
    -- so falling back cannot run it twice.
    let unavailable reason
          | automatic && mode == PrivateNetwork = do
              warning $ "private networking is unavailable (" ++ takeWhile (/= '\n') reason ++
                "): falling back to host networking"
              prepare opts {networkOption = Just HostNetwork} >>= launch False
          | otherwise = invalid $
              (if mode == PrivateNetwork then "private networking" else "network isolation")
              ++ " is unavailable. " ++ reason ++ "\n"
              ++ "Try --network-host, if sharing the enclosing network is acceptable."
    result <- case available of
      Just reason -> unavailable reason
      Nothing -> launch (not automatic) planned `catch`
        \(NetworkSetupFailure reason) -> unavailable reason
    exitWith result
  where
    launch removeHomeOnFailure plan =
      withPersistentHome removeHomeOnFailure (persistentHome plan) $
        runSandbox (privateNetwork plan) (withSession details) (launchSandbox plan)
      where
        details = SessionDetails (launchProject plan) (sandboxCommand $ launchSandbox plan)
          (selectedNetwork plan) (shareProcesses opts)

-- Undo creating a persistent home if the run fails while it is still empty,
-- such as when sandbox or network setup fails before the command starts.
-- Bubblewrap may already have created empty mountpoints inside it (for a
-- project beneath HOME); remove only empty directories, never files.
-- A fallback launch reuses the directory instead.
withPersistentHome :: Bool -> Maybe FilePath -> IO a -> IO a
withPersistentHome _ Nothing action = action
withPersistentHome removeOnFailure (Just dir) action = do
  exists <- doesDirectoryExist dir
  if exists then action else do
    warning $ "created" +-+ dir
    createDirectoryIfMissing True dir
    action `onException` when removeOnFailure (ignoreErrors $ do
      pristine <- onlyDirectories dir
      when pristine $ removeTree dir)
  where
    onlyDirectories path = do
      symlink <- pathIsSymbolicLink path
      isDir <- doesDirectoryExist path
      if not isDir || symlink then pure False else
        listDirectory path >>= allM (onlyDirectories . (path </>))
    removeTree path = do
      listDirectory path >>= mapM_ (removeTree . (path </>))
      removeDirectory path
    ignoreErrors act = catchIOError act $ const $ pure ()

preview :: Options -> Launch -> IO ()
preview opts plan = do
  let network = privateNetwork plan
      sandbox = launchSandbox plan
      descriptor (Arg _) = False
      descriptor _ = True
      copies = any descriptor (sandboxArgs sandbox)
      commands = map (unwords . map shellQuote) $ previewSandbox network sandbox
  when (isNothing $ networkOption opts) $
    putStrLn "# trying private networking; fall back to host if network setup fails"
  if isJust network || copies
    then do
      -- Descriptors only exist at launch, so show a plan and a runnable command.
      mapM_ (putStrLn . ("# " ++)) commands
      executable <- getExecutablePath
      argv <- getArgs
      let (options, cmd) = break (== "--") argv
      putStrLn $ unwords $ map shellQuote $ executable : filter (/= "--dryrun") options ++ cmd
    else mapM_ putStrLn commands

-- Validate paths and construct argv before creating any persistent home.
prepare :: Options -> IO Launch
prepare opts = do
  let mode = fromMaybe PrivateNetwork $ networkOption opts
  (projectPath, projectMode) <-
    case splitMountSpec ReadWriteMount (fromMaybe "." $ projectOption opts) of
      ([path], access) | not (null path) -> pure (path, access)
      _ -> invalid "invalid project (expected DIR[:ro|rw|O])"
  candidate <- existingDirectory "project" projectPath
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
  busSettings <- sequence [dbusSetup | dbusOption opts]
  let desktopSettings = displaySettings ++ busSettings
      desktopMounts = concatMap fst desktopSettings
      backends = ["wayland" | waylandOption opts] ++ ["x11" | x11Option opts]
      desktopEnv = concatMap snd desktopSettings ++
        [("GDK_BACKEND", intercalate "," backends) | not (null backends)]
  mounts <- (desktopMounts ++) <$> mapM resolveMount (volumeOptions opts)
  identities <- filter (\(path, _) -> not $
    any (\mount -> within (destination mount) path) mounts) <$> identityFiles
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
      then [Arg "--ro-bind-data", FileFd ("<system-file-" ++ show index ++ "-fd>") (source mount),
            Arg (destination mount)]
      else map Arg (mountArgs mount)
  resolver <- if mode /= PrivateNetwork then pure "/etc/resolv.conf" else do
    target <- catchIOError (hiddenSystemTarget (hiddenPath $ shareFilesystem opts) "/etc/resolv.conf") $ \err ->
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
      afterSystemFiles = ["--unshare-net" | mode /= HostNetwork]
        ++ concatMap mountArgs mounts
        ++ maybe [] (\path -> mountArgs $ Mount path path projectMode) project
        ++ ["--chdir", fromMaybe home project, "--clearenv"]
        ++ concatMap (\(key, value) -> ["--setenv", key, value]) (defaults ++ desktopEnv ++ env)
        ++ (if shareFilesystem opts then [] else ["--remount-ro", "/"])
      identityArgs = concat
        [[Arg "--ro-bind-data", DataFd ("<" ++ name ++ "-fd>") bytes, Arg path]
        | (path, bytes) <- identities, let name = takeFileName path]
      args = map Arg beforeSystemFiles ++ concat systemMounts ++ identityArgs ++ map Arg afterSystemFiles
  pure $ Launch (Sandbox args cmd) suppliedHome mode resolver project

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
  pure ([Mount src socket ReadOnlyMount],
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
  pure ([Mount socket socket ReadOnlyMount, Mount authority authTarget ReadOnlyMount],
        [("DISPLAY", display), ("XAUTHORITY", authTarget)])
  where
    badDisplay = invalid "--x11 requires a local DISPLAY such as :0 or :0.0 (TCP displays are unsupported)"

dbusSetup :: IO ([Mount], [(String, String)])
dbusSetup = do
  address <- lookupEnv "DBUS_SESSION_BUS_ADDRESS" >>= maybe
    (invalid "--dbus requires DBUS_SESSION_BUS_ADDRESS") pure
  value <- case break (== ':') address of
    ("unix", parameters) | ';' `notElem` address ->
      case map (break (== '=')) $ splitComma $ drop 1 parameters of
        fields | all (\(key, val) -> key `elem` ["path", "guid"] && not (null $ drop 1 val)) fields,
                 [path] <- [path | ("path", '=':path) <- fields] -> pure path
        _ -> badAddress
    _ -> badAddress
  bytes <- either invalid pure $ unescape value
  -- D-Bus escapes bytes; decode using the same encoding as filesystem paths.
  encoding <- getFileSystemEncoding
  socket <- BS.useAsCString (BS.pack bytes) $ peekCString encoding
  unless (isAbsolute socket) $ invalid "--dbus requires an absolute Unix socket path"
  status <- getFileStatus socket
  unless (isSocket status) $ invalid $ "D-Bus session address is not a socket: " ++ socket
  src <- canonicalizePath socket
  let target = "/run/workcell-session-bus"
  pure ([Mount src target ReadOnlyMount], [("DBUS_SESSION_BUS_ADDRESS", "unix:path=" ++ target)])
  where
    badAddress = invalid "--dbus requires a single unix:path= session address (abstract and TCP sockets are unsupported)"
    splitComma input = case break (== ',') input of
      (part, []) -> [part]
      (part, _:rest) -> part : splitComma rest
    unescape [] = Right []
    unescape ('%':a:b:rest) | isHexDigit a && isHexDigit b = do
      let byte = digitToInt a * 16 + digitToInt b
      if byte == 0 then Left "D-Bus socket path contains a NUL byte"
      else (fromIntegral byte :) <$> unescape rest
    unescape ('%':_) = Left "invalid escape in D-Bus socket path"
    unescape (c:rest)
      | isAsciiLower c || isAsciiUpper c || c `elem` ['0'..'9'] || c `elem` "-_/.*\\" =
          (fromIntegral (ord c) :) <$> unescape rest
      | otherwise = Left "unescaped character in D-Bus socket path"

privateProcArgs :: [String]
privateProcArgs = ["--unshare-pid", "--proc", "/proc"]

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
        covered path = any (\mount -> within (destination mount) path) mounts
    target <- hiddenSystemTarget (hiddenPath shared) "/etc/resolv.conf"
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
    target <- hiddenSystemTarget (hiddenPath shared) path
    pure $ maybe [] (\dest -> [Mount resolved dest ReadOnlyMount]) target

-- Paths outside the sandbox's view of the enclosing filesystem.
hiddenPath :: Bool -> FilePath -> Bool
hiddenPath shared path
  | shared = any (`within` path) temporaryRoots
  | otherwise = not $ any (`within` path) systemDirectories

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
  (src, dst, mode) <- either invalid pure (parseMount spec)
  exists <- doesPathExist src
  unless exists $ invalid $ "volume source does not exist: " ++ src
  srcPath <- canonicalizePath src
  when (mode == OverlayMount) $
    unlessM (doesDirectoryExist srcPath) $
      invalid $ "overlay source must be a directory: " ++ src
  dstPath <- canonicalizePath (fromMaybe src dst)
  rejectRoot "volume destination" dstPath
  pure $ Mount srcPath dstPath mode

parseMount :: String -> Either String (FilePath, Maybe FilePath, MountMode)
parseMount spec =
  case splitMountSpec ReadOnlyMount spec of
    ([src], access) | valid src -> Right (src, Nothing, access)
    ([src, dst], access) | valid src && valid dst -> Right (src, Just dst, access)
    _ -> Left $ "invalid volume (expected SOURCE[:DEST][:ro|rw|O]): " ++ spec
  where
    valid = not . null

-- Both project and volume specifications accept the same trailing access mode.
splitMountSpec :: MountMode -> String -> ([String], MountMode)
splitMountSpec fallback spec =
  case reverse parts of
    mode:rest | not (null rest), Just value <- access mode -> (reverse rest, value)
    _ -> (parts, fallback)
  where
    parts = splitColon spec
    access "ro" = Just ReadOnlyMount
    access "rw" = Just ReadWriteMount
    access "O" = Just OverlayMount
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
mountArgs mount = case mountMode mount of
  ReadOnlyMount -> ["--ro-bind", source mount, destination mount]
  ReadWriteMount -> ["--bind", source mount, destination mount]
  OverlayMount -> ["--overlay-src", source mount, "--tmp-overlay", destination mount]

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
