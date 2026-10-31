{-# LANGUAGE CPP #-}
-- SPDX-License-Identifier: LGPL-2.1-or-later
module Identity (identityFiles, passwdEntries, groupEntries, appendEntries) where

import Control.Monad (forM, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (intercalate, nub)
import Data.Maybe (catMaybes, maybeToList)
import SimpleCmd (warning)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (getFileStatus, linkCount)
import System.Posix.User
#if MIN_VERSION_unix(2,8,0)
import System.Posix.User.ByteString (UserEntry, GroupEntry)
#endif

import Util (invalid)

-- Resolve NSS records while the host's SSSD socket/cache is still visible.
-- Retain local accounts, and copy no password hashes or remote directory dump.
identityFiles :: IO [(FilePath, BS.ByteString)]
identityFiles = do
  uid <- getEffectiveUserID
  user <- optional $ getUserEntryForID uid
  gid <- getEffectiveGroupID
  supplementary <- getGroups
  groups <- catMaybes <$> mapM (optional . getGroupEntryForID)
    (nub $ gid : supplementary ++ map userGroupID (maybeToList user))
  files <- forM [("/etc/passwd", passwdEntries $ maybeToList user),
                 ("/etc/group", groupEntries groups)] $ \(path, missing) -> optional $ do
    original <- catchIOError (BS.readFile path) $ \err ->
      if isDoesNotExistError err then pure BS.empty else ioError err
    let entries = missing original
    -- An enclosing sandbox's --ro-bind-data copy cannot be mounted over.
    unless (null entries) $ do
      status <- getFileStatus path
      when (linkCount status == 0) $
        invalid $ path ++ " is a sandbox copy which cannot be replaced"
    pure (path, original, entries)
  pure [(path, appendEntries original entries)
       | Just (path, original, entries) <- files, not (null entries)]
  where
    -- Names are a convenience: never let a directory failure block a launch.
    optional action = catchIOError (Just <$> action) $ \err -> do
      unless (isDoesNotExistError err) $
        warning $ "cannot preserve account names: " ++ show err
      pure Nothing

-- Records for users whose UID is absent from the original passwd file.
passwdEntries :: [UserEntry] -> BS.ByteString -> [String]
passwdEntries users = missingEntries
  [(show $ userID user, intercalate ":"
    [userName user, "x", show $ userID user, show $ userGroupID user,
     userGecos user, homeDirectory user, userShell user]) | user <- users]

-- Records for groups whose GID is absent from the original group file.
groupEntries :: [GroupEntry] -> BS.ByteString -> [String]
groupEntries groups = missingEntries
  [(show $ groupID group, intercalate ":"
    [groupName group, "x", show $ groupID group, intercalate "," $ groupMembers group])
   | group <- groups]

-- Both files keep the numeric ID in their third field.
missingEntries :: [(String, String)] -> BS.ByteString -> [String]
missingEntries entries original =
  [entry | (ident, entry) <- entries, BSC.pack ident `notElem` existing]
  where
    existing = [ident | _:_:ident:_ <- map (BSC.split ':') (BSC.lines original)]

-- The unix package unpacks NSS strings one Char per byte, so packing them
-- restores the directory's bytes in whatever encoding it uses.
appendEntries :: BS.ByteString -> [String] -> BS.ByteString
appendEntries original entries
  | BS.null original || BSC.last original == '\n' = original `BS.append` added
  | otherwise = BSC.snoc original '\n' `BS.append` added
  where
    added = BSC.pack $ unlines entries
