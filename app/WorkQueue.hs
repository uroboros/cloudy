module WorkQueue where
import Lib
import Types (Job(..), JobQuery(..))

import Control.Distributed.Process
import System.Random (StdGen, randomRs, mkStdGen, split)

randoms :: StdGen -> Int -> [Double]
randoms gen n =  map (1-) rs -- 1-[0,1) = (0,1]
  where rs = take n $ randomRs (0,1) gen

workQ :: StdGen -> Int -> Process ()
workQ randGen nextI
  = do nextI' <- receiveWait [match $ \(JobQuery pid n)
                                        -> do send pid (jobs nextI n)
                                              return (nextI+n)]
       let (randGen', _) = split randGen
       workQ randGen' nextI'
  where
    jobs offset n = zipWith Job (map (+offset) [1..n]) (randoms randGen n)

jobQuery :: ProcessId -> Int -> Process [Job]
jobQuery workQueue n = do pid <- getSelfPid
                          send workQueue (JobQuery pid n)
                          expect
