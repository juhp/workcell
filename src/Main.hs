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
  <$> strOptionalWith 'p' "project" "DIR" "Writable project (default: current directory)" "."
  <*> optional (strOptionWith 'H' "home" "DIR" "Persistent writable home (created if missing)")
  <*> many (strOptionWith 'v' "volume" "SOURCE[:DEST][:ro|rw]" "Extra mount (default: same path, read-only)")
  <*> many (strOptionWith 'e' "env" "KEY[=VALUE]" "Set or pass through an environment variable")
  <*> switchLongWith "dryrun" "Print the bubblewrap command without changing files"
  <*> many (strArg "COMMAND ARG...")
