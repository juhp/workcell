-- SPDX-License-Identifier: BSD-3-Clause
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
  executable <- fromMaybe "workcell" <$> lookupEnv "WORKCELL"
  let run test dryrun net = do
        (code, out, err) <- readProcessWithExitCode executable ("run":net:["--dryrun" | dryrun] ++ ["true"]) ""
        case code of
          ExitSuccess -> pure ()
          ExitFailure n ->
            if test
             then putStrLn $ "Skipping" +-+ net ++ ": sandbox startup failed\n" ++ err
             else fail $ "workcell failed (" ++ show n ++ "): " ++ out ++ err
        return $ code == ExitSuccess
  host <- run True False "--network-host"
  priv <- run True False "--network-private"
  none <- run True False "--network-none"
  -- CPU time would exclude the time spent in workcell and bubblewrap.
  defaultMain
    [ localOption WallTime $
      bgroup "workcell" $
      bench "dryrun" (nfIO $ run False True "--network-host") :
      [bench "run host" $ nfIO $ run False False "--network-host"| host] ++
      [bench "run private" $ nfIO $ run False False "--network-private"| priv] ++
      [bench "run none" $ nfIO $ run False False "--network-none"| none]
    ]
