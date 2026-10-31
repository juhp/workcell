{-# LANGUAGE CPP #-}
-- SPDX-License-Identifier: LGPL-2.1-or-later
module Identity (identityFiles, passwdEntries, groupEntries) where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (intercalate, nub)
import Data.Maybe (catMaybes, maybeToList)
import GHC.Foreign (withCStringLen)
import GHC.IO.Encoding (getFileSystemEncoding)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.User
#if MIN_VERSION_unix(2,8,0)
import System.Posix.User.ByteString (UserEntry, GroupEntry)
#endif

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
  encoding <- getFileSystemEncoding
  fmap catMaybes $ forM [("/etc/passwd", passwdEntries $ maybeToList user),
        ("/etc/group", groupEntries groups)] $ \(path, append) -> do
    original <- catchIOError (BS.readFile path) $ \err ->
      if isDoesNotExistError err then pure BS.empty else ioError err
    -- Decode no existing bytes: only encode the added NSS records.
    let additions = append $ BSC.unpack original
        suffix = drop (BS.length original) additions
    bytes <- withCStringLen encoding suffix BS.packCStringLen
    pure $ if BS.null bytes then Nothing else Just (path, original `BS.append` bytes)
  where
    optional action = catchIOError (Just <$> action) $ \err ->
      if isDoesNotExistError err then pure Nothing else ioError err

passwdEntries :: [UserEntry] -> String -> String
passwdEntries users = appendEntries
  [(show $ userID user, intercalate ":"
    [userName user, "x", show $ userID user, show $ userGroupID user,
     userGecos user, homeDirectory user, userShell user]) | user <- users]

groupEntries :: [GroupEntry] -> String -> String
groupEntries groups = appendEntries
  [(show $ groupID group, intercalate ":"
    [groupName group, "x", show $ groupID group, intercalate "," $ groupMembers group])
   | group <- groups]

appendEntries :: [(String, String)] -> String -> String
appendEntries entries original = foldl append original entries
  where
    append content (ident, entry)
      | any ((== ident) . identifier) (lines content) = content
      | otherwise = content ++ [ '\n' | not (null content) && last content /= '\n'] ++ entry ++ "\n"
    identifier line = case splitFields line of
      _:_:ident:_ -> ident
      _ -> ""
    splitFields line = case break (== ':') line of
      (part, []) -> [part]
      (part, _:rest) -> part : splitFields rest
