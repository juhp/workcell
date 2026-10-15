-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Concurrent (myThreadId)
import Control.Exception (IOException, bracket, catch, throwTo)
import Control.Monad (void)
import SimpleCmdArgs
import System.Exit (ExitCode(..), die)
import System.Posix.Signals (Handler(CatchOnce), installHandler, sigTERM)

import Enter (enterSession)
import Paths_workcell (version)
import Run (Options(..), runWorkcell)
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
      <*> many (strArg "COMMAND ARG...")
    , Subcommand "run" "Run a workcell bwrap session" $
      runWorkcell <$> options
    ]
  where
    execute action = withTermination action `catch` report
    report :: IOException -> IO ()
    report err = die $ "workcell: " ++ show err

-- Turn termination into an exception so session/helper brackets can clean up.
withTermination :: IO a -> IO a
withTermination action = do
  thread <- myThreadId
  bracket (installHandler sigTERM (CatchOnce $ throwTo thread $ ExitFailure 143) Nothing)
    (\previous -> void (installHandler sigTERM previous Nothing)) $ const action

options :: Parser Options
options = Options
  <$> optional (strOptionWith 'p' "project" "DIR" "Writable project (default: current directory, excluding HOME and ancestors)")
  <*> optional (strOptionWith 'H' "home" "DIR" "Persistent writable home (created if missing)")
  <*> many (strOptionWith 'v' "volume" "SOURCE[:DEST][:ro|rw]" "Extra mount (default: same path, read-only)")
  <*> many (strOptionWith 'e' "env" "KEY[=VALUE]" "Set or pass through an environment variable")
  <*> switchLongWith "wayland" "Expose the Wayland display socket for graphical applications"
  <*> switchLongWith "x11" "Expose the local X11 display socket and authentication file"
  <*> switchLongWith "dbus" "Expose the desktop D-Bus session socket (unfiltered)"
  <*> switchLongWith "dryrun" "Print the bubblewrap command without changing files"
  <*> switchLongWith "share-processes" "Share enclosing processes and /proc instead of isolating them"
  <*> switchLongWith "share-filesystem" "Expose the enclosing filesystem read-only, retaining isolated home"
  <*> optional (strOptionLongWith "network" "MODE" "Networking: private (default, requires pasta), none, or host")
  <*> switchLongWith "share-network" "Share the enclosing network (alias for --network host)"
  <*> many (strArg "COMMAND ARG...")
