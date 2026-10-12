-- SPDX-License-Identifier: BSD-3-Clause
module Main (main) where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (localOption)
import Test.Tasty.Bench

main :: IO ()
main = do
  executable <- fromMaybe "workcell" <$> lookupEnv "WORKCELL"
  let launch = ["--network", "host", "--", "/bin/true"]
      run args = do
        (code, out, err) <- readProcessWithExitCode executable args ""
        case code of
          ExitSuccess -> pure ()
          ExitFailure n -> fail $ "workcell failed (" ++ show n ++ "): " ++ out ++ err
  (code, _, err) <- readProcessWithExitCode executable launch ""
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> putStrLn $ "Skipping run true: sandbox startup unavailable\n" ++ err
  -- CPU time would exclude the time spent in workcell and bubblewrap.
  defaultMain
    [ localOption WallTime $
        bgroup "workcell" $
          [bench "dryrun" $ nfIO $ run ["--network", "host", "--dryrun", "--", "/bin/true"]] ++
          [bench "run true" $ nfIO $ run launch | code == ExitSuccess]
    ]
