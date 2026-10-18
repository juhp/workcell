{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
-- SPDX-License-Identifier: LGPL-2.1-or-later
module Util (
  invalid,
  ignoreMissing,
  procPath,
  shellQuote,
  openSimpleFd,
  ownerNamespace,
  withDescriptors,
  readResolvConf,
  keepOnlyInheritedFds
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.Maybe (mapMaybe)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt(..), CULong(..))
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.IO
  (FdOption(CloseOnExec), OpenMode(..), closeFd, defaultFileFlags, openFd, setFdOption)
import System.Posix.Types (Fd(..))
import Text.Read (readMaybe)

invalid :: String -> IO a
invalid = ioError . userError

ignoreMissing :: IO () -> IO ()
ignoreMissing action = catchIOError action $ \err ->
  unless (isDoesNotExistError err) $ ioError err

procPath :: Int -> FilePath -> FilePath
procPath pid suffix = "/proc" </> show pid </> suffix

shellQuote :: String -> String
shellQuote value = "'" ++ concatMap escape value ++ "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]

-- taken from shake General.FileLock
openSimpleFd :: FilePath -> OpenMode -> IO Fd
#if MIN_VERSION_unix(2,8,0)
openSimpleFd file mode = openFd file mode defaultFileFlags
#else
openSimpleFd file mode = openFd file mode Nothing defaultFileFlags
#endif

withDescriptors :: [FilePath] -> ([Fd] -> IO a) -> IO a
withDescriptors [] action = action []
withDescriptors (path:rest) action =
  bracket (openSimpleFd path ReadOnly) closeFd $ \fd ->
    withDescriptors rest $ \fds -> action (fd:fds)

-- ioctl is variadic: capi calls it through its C prototype.
foreign import capi unsafe "sys/ioctl.h ioctl"
  namespaceIoctl :: CInt -> CULong -> CULong -> IO CInt

foreign import capi unsafe "linux/nsfs.h value NS_GET_USERNS"
  getOwnerNamespace :: CULong

-- The user namespace owning a namespace, even if bubblewrap later enters
-- a child user namespace to restore the invoking UID after devpts setup.
ownerNamespace :: Fd -> IO Fd
ownerNamespace (Fd fd) = Fd <$> throwErrnoIfMinus1 "namespace owner"
  (namespaceIoctl fd getOwnerNamespace 0)

readResolvConf :: IO [String]
readResolvConf = do
  exists <- doesFileExist "/etc/resolv.conf"
  if exists then lines <$> readFile "/etc/resolv.conf" else pure []

-- Mark every descriptor above stderr close-on-exec except those listed,
-- including ones opened by the GHC runtime, so that a child started with
-- close_fds = False inherits only the descriptors it is meant to consume.
keepOnlyInheritedFds :: [Fd] -> IO ()
keepOnlyInheritedFds keep = do
  open <- mapMaybe readMaybe <$> listDirectory "/proc/self/fd"
  forM_ open $ \n -> do
    let fd = Fd n
    unless (n <= 2) $
      -- The listing's own directory descriptor is already closed.
      ignoreBadFd $ setFdOption fd CloseOnExec (fd `notElem` keep)
  where
    ignoreBadFd action = catchIOError action $ const $ pure ()
