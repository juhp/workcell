-- SPDX-License-Identifier: LGPL-2.1-or-later

module Main (main) where

import Control.Concurrent (myThreadId)
import Control.Exception (Handler(..), IOException, bracket, catches, throwTo)
import Control.Monad (void)
import SimpleCmdArgs
import System.Exit (ExitCode(..), die)
import System.Posix.Signals (Handler(CatchOnce), Signal, installHandler, sigHUP, sigTERM)

import Enter (enterSession)
import Paths_workcell (version)
import Run (Options(..), runWorkcell, NetworkMode(..))
import Sandbox (NetworkSetupFailure)
import Session (listSessions)

main :: IO ()
main =
  simpleCmdArgs' (Just version) "workcell"
  "Run tools inside bwrap with an isolated home and writable project" $
  execute <$> subcommands
    [ Subcommand "list" "List active workcells in this PID namespace" $
      pure listSessions
    , Subcommand "enter" "Enter an active workcell without sudo" $
      enterSession
      <$> switchLongWith "dryrun" "Print the entry command without entering"
      <*> optional (strArg "SESSION")
      <*> switchLongWith "root" "Enter as root inside the workcell"
      <*> many (strArg "[--] COMMAND ARG...")
    , Subcommand "run" "Run a workcell bwrap session" $
      runWorkcell <$> options
    ]
  where
    execute action = withTermination action `catches`
      [Handler $ \err -> die $ "workcell: " ++ show (err :: IOException),
       Handler $ \err -> die $ "workcell: " ++ show (err :: NetworkSetupFailure)]

-- Turn termination and hangup into exceptions so session/helper brackets
-- can clean up.
withTermination :: IO a -> IO a
withTermination action =
  withSignal sigTERM 143 $ withSignal sigHUP 129 action
  where
    withSignal :: Signal -> Int -> IO a -> IO a
    withSignal signal code inner = do
      thread <- myThreadId
      bracket (installHandler signal (CatchOnce $ throwTo thread $ ExitFailure code) Nothing)
        (\previous -> void (installHandler signal previous Nothing)) $ const inner

options :: Parser Options
options = Options
  <$> optional (strOptionWith 'p' "project" "DIR[:(ro|rw|O)]" "Project access (default: rw; :O for temporary overlay; directory defaults to cwd, excluding HOME and ancestors)")
  <*> optional (strOptionWith 'H' "home" "DIR" "Persistent writable home (created if missing)")
  <*> many (strOptionWith 'v' "volume" "SOURCE[:DEST][:(ro|rw|O)]" "Extra mount (:O for temporary directory overlay; default: same path, read-only)")
  <*> many (strOptionWith 'e' "env" "KEY[=VALUE]" "Set or pass through an environment variable")
  <*> switchLongWith "wayland" "Expose the Wayland display socket for graphical applications"
  <*> switchLongWith "x11" "Expose the local X11 display socket and authentication file"
  <*> switchLongWith "dbus" "Expose the desktop D-Bus session socket (unfiltered)"
  <*> switchLongWith "dryrun" "Print the bubblewrap command without changing files"
  <*> switchLongWith "share-processes" "Share enclosing processes and /proc instead of isolating them"
  <*> switchLongWith "share-filesystem" "Expose the enclosing filesystem read-only, retaining isolated home"
  <*> optional (flagLongWith' HostNetwork "network-host" "Use host networking" <|>
                flagLongWith' NoNetwork "network-none" "Disable networking except private loopback" <|>
                flagLongWith' PrivateNetwork "network-private" "Use a private pasta network (attempted by default)")
  <*> many (strArg "[--] COMMAND ARG...")
