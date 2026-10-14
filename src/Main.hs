-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Exception (IOException, catch)
import Paths_workcell (version)
import SimpleCmdArgs
import System.Exit (die)
import Workcell (Options(..), runWorkcell)

main :: IO ()
main = simpleCmdArgs' (Just version) "workcell"
  "Run system tools with an isolated home and writable project" $
  run <$> options
  where
    run opts = runWorkcell opts `catch` report
    report :: IOException -> IO ()
    report err = die $ "workcell: " ++ show err

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
  <*> switchLongWith "list" "List active workcells in this PID namespace"
  <*> optional (strOptionLongWith "enter" "ID" "Enter an active workcell without sudo (see --list)")
  <*> switchLongWith "root" "Enter as root inside the workcell (requires --enter)"
  <*> many (strArg "COMMAND ARG...")
