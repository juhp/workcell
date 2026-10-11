-- SPDX-License-Identifier: BSD-3-Clause
module Workcell (Options(..), runWorkcell) where

import Control.Monad (forM, unless, when)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import SimpleCmd (cmdFull, needProgram)
import System.Directory
  (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist,
   doesPathExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, normalise, splitDirectories)
import System.Posix.Process (executeFile)

data Options = Options
  { projectOption :: FilePath
  , homeOption :: Maybe FilePath
  , volumeOptions :: [String]
  , environmentOptions :: [String]
  , dryRun :: Bool
  , shareProcesses :: Bool
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
  }

runWorkcell :: Options -> IO ()
runWorkcell opts = do
  launch <- prepare opts
  if dryRun opts
    then putStrLn $ unwords $ map shellQuote $ "bwrap" : arguments launch
    else do
      needProgram "bwrap"
      unless (shareProcesses opts) verifyProcessIsolation
      mapM_ (createDirectoryIfMissing True) (persistentHome launch)
      executeFile "bwrap" True (arguments launch) Nothing

-- Validate paths and construct argv before creating any persistent home.
prepare :: Options -> IO Launch
prepare opts = do
  project <- existingDirectory "project" (projectOption opts)
  rejectRoot "project" project
  originalHome <- getHomeDirectory
  unless (isAbsolute originalHome) $ invalid "HOME must be an absolute path"
  home <- existingDirectory "HOME" originalHome
  rejectRoot "HOME" home
  suppliedHome <- traverse resolveHome (homeOption opts)
  mounts <- mapM resolveMount (volumeOptions opts)
  unless (shareProcesses opts) $ do
    when (within "/proc" project || within home "/proc" || within "/proc" home) $
      invalid "home and project must not overlap private /proc"
    mapM_ (\mount -> when (within (destination mount) "/proc" || within "/proc" (destination mount)) $
      invalid "volume destination must not overlap private /proc") mounts
  systemFiles <- preservedSystemFiles
  let destinations = map destination mounts
  unless (length destinations == length (nub destinations)) $
    invalid "duplicate volume destinations"
  mapM_ (validateMount project home) mounts
  env <- mapM resolveEnvironment (environmentOptions opts)
  term <- fromMaybe "dumb" <$> lookupEnv "TERM"
  let homeArgs = maybe ["--tmpfs", home]
        (\path -> ["--bind", path, home]) suppliedHome
      defaults = [("HOME", home), ("PATH", systemPath), ("TERM", term)]
      cmd = if null (command opts)
        then ["/bin/bash"] else command opts
      args = ["--die-with-parent", "--ro-bind", "/", "/"]
        ++ concatMap (\path -> ["--tmpfs", path]) temporaryRoots
        ++ ["--dev", "/dev"]
        ++ (if shareProcesses opts then ["--ro-bind", "/proc", "/proc"] else privateProcArgs)
        ++ ["--dir", home]
        ++ homeArgs
        ++ concatMap mountArgs systemFiles
        ++ concatMap mountArgs mounts
        ++ ["--bind", project, project, "--chdir", project, "--clearenv"]
        ++ concatMap (\(key, value) -> ["--setenv", key, value]) (defaults ++ env)
        ++ ["--"] ++ cmd
  pure $ Launch args suppliedHome

privateProcArgs :: [String]
privateProcArgs = ["--unshare-pid", "--proc", "/proc"]

-- Check before creating a persistent home or running the requested command.
-- Keep exec for the real launch so terminal handling and exit status are unchanged.
verifyProcessIsolation :: IO ()
verifyProcessIsolation = do
  (ok, _, err) <- cmdFull "bwrap"
    (["--die-with-parent", "--ro-bind", "/", "/", "--dev", "/dev"]
     ++ privateProcArgs ++ ["--", "/bin/true"]) ""
  unless ok $ invalid $
    "private PID namespace and /proc are unavailable in this environment. "
    ++ "Use --share-processes only if sharing enclosing processes is acceptable.\n" ++ err

systemPath :: String
systemPath = "/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin"

temporaryRoots :: [FilePath]
temporaryRoots = ["/home", "/root", "/tmp", "/run"]

-- Toolbx links these files into /run/host. Preserve only the files, not /run/host.
preservedSystemFiles :: IO [Mount]
preservedSystemFiles = fmap concat $ forM ["/etc/resolv.conf", "/etc/hosts"] $ \path -> do
  exists <- doesPathExist path
  if not exists then pure [] else do
    resolved <- canonicalizePath path
    pure [Mount resolved resolved False | any (`within` resolved) temporaryRoots]

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

validateMount :: FilePath -> FilePath -> Mount -> IO ()
validateMount project home mount = do
  let dest = destination mount
  when (within dest home || within dest project || within project dest) $
    invalid $ "volume destination overlaps home or project: " ++ dest
  let temporary = any (`within` dest) (home : temporaryRoots)
  exists <- doesPathExist dest
  unless (temporary || exists) $
    invalid $ "volume destination must already exist outside temporary directories: " ++ dest
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
