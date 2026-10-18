-- SPDX-License-Identifier: LGPL-2.1-or-later
module Child (
  Child,
  childHandle,
  withChild,
  withChildStreams,
  stopChild,
  waitChild,
  childExited,
  waitEither
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  (MVar, newEmptyMVar, putMVar, readMVar, tryReadMVar)
import Control.Exception
  (AsyncException(UserInterrupt), SomeException, bracket, fromException, throwIO, try)
import Control.Monad (void)
import Control.Monad.Extra (whenJust)
import System.Exit (ExitCode(..))
import System.IO (Handle)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
  (CreateProcess, ProcessHandle, createProcess, getPid, terminateProcess, waitForProcess)
import System.Timeout (timeout)

-- A child is reaped by exactly one thread; everyone else reads its result.
-- This avoids concurrent waitpid calls and repeated polling.
data Child = Child
  { childHandle :: ProcessHandle
  , childResult :: MVar (Either SomeException ExitCode)
  }

type Streams = (Maybe Handle, Maybe Handle, Maybe Handle)

spawn :: CreateProcess -> IO (Streams, Child)
spawn command = do
  (input, output, err, handle) <- createProcess command
  result <- newEmptyMVar
  _ <- forkIO $ try (waitForProcess handle) >>= putMVar result
  pure ((input, output, err), Child handle result)

withChild :: CreateProcess -> (Child -> IO a) -> IO a
withChild command action = withChildStreams command $ \(_, _, _, child) -> action child

-- Stopped (if still running) when the action finishes or fails.
withChildStreams :: CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, Child) -> IO a) -> IO a
withChildStreams command action =
  bracket (spawn command) (stopChild . snd) $ \((input, output, err), child) ->
    action (input, output, err, child)

waitChild :: Child -> IO ExitCode
waitChild child = readMVar (childResult child) >>= either rethrow pure
  where
    -- delegate_ctlc reports a child killed by SIGINT as UserInterrupt.
    rethrow err = case fromException err of
      Just UserInterrupt -> pure $ ExitFailure 130
      _ -> throwIO err

childExited :: Child -> IO (Maybe ExitCode)
childExited child = do
  result <- tryReadMVar (childResult child)
  case result of
    Nothing -> pure Nothing
    Just _ -> Just <$> waitChild child

-- Wait for whichever child exits first.
waitEither :: Child -> Child -> IO (Either ExitCode ExitCode)
waitEither left right = do
  first <- newEmptyMVar
  _ <- forkIO $ try (waitChild left) >>= putMVar first . fmap Left
  _ <- forkIO $ try (waitChild right) >>= putMVar first . fmap Right
  readMVar first >>= either (throwIO :: SomeException -> IO a) pure

stopChild :: Child -> IO ()
stopChild child = do
  status <- tryReadMVar (childResult child)
  case status of
    Just _ -> pure ()
    Nothing -> do
      terminateProcess (childHandle child)
      result <- timeout (5 * 1000000) (readMVar $ childResult child)
      case result of
        Just _ -> pure ()
        Nothing -> do
          pid <- getPid (childHandle child)
          whenJust pid $ signalProcess sigKILL
          void $ readMVar (childResult child)
