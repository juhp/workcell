-- SPDX-License-Identifier: LGPL-2.1-or-later
module Main (main) where

import Data.Maybe (fromMaybe)
import SimpleCmd ((+-+))
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (localOption)
import Test.Tasty.Bench

main :: IO ()
main = do
  exe <- fromMaybe "workcell" <$> lookupEnv "WORKCELL"
  host <- run exe True False ["--network-host"]
  priv <- run exe True False ["--network-private"]
  none <- run exe True False ["--network-none"]
  -- CPU time would exclude the time spent in workcell and bubblewrap.
  defaultMain
    [ localOption WallTime $
      bgroup "workcell" $
      bench "dryrun" (nfIO $ run exe False True ["--network-host"]) :
      [bench "run none" $ nfIO $ run exe False False ["--network-none"]| none] ++
      [bench "run host" $ nfIO $ run exe False False ["--network-host"]| host] ++
      [bench "run private" $ nfIO $ run exe False False ["--network-private"]| priv] ++
      [bench "run auto" $ nfIO $ run exe False False []| host]
    ]
  where
    run exe test dryrun net = do
      (code, out, err) <- readProcessWithExitCode exe ("run":net ++ ["--dryrun" | dryrun] ++ ["true"]) ""
      case code of
        ExitSuccess -> pure ()
        ExitFailure n ->
          if test
          then putStrLn $ "Skipping" +-+ showNet net ++ ": sandbox startup failed\n" ++ err
          else fail $ "workcell failed (" ++ show n ++ "): " ++ out ++ err
      return $ code == ExitSuccess

    showNet net =
      if null net
      then "auto"
      else unwords net
