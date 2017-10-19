module Master where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)

import Types (SlaveStatus(..))
import WorkQueue (workQ, jobQuery)
import Slave (slave)

-- given k,l - ensure a sensible 'minStopPeriod'
calcPeriods :: Int -> Int -> Int -> (Int, Int, Int)
calcPeriods sendPeriod gracePeriod minStopPeriod
  = if (gracePeriod - minStopPeriod) > 0
      then (sendPeriod, gracePeriod - minStopPeriod, minStopPeriod)
      else if (sendPeriod - minStopPeriod) > 0
        then (sendPeriod - minStopPeriod, gracePeriod, minStopPeriod)
        else (planB, planB, planB)
  where planB = (sendPeriod + gracePeriod) `div` 3

-- Manage Slave States --
sendSlaves f sids = mapM_ (\sid -> send sid (f sid)) sids
peerIds sids sid = filter (/= sid) sids

initPeriod delay f sids = do
  sendSlaves f sids
  liftIO $ threadDelay delay

sendJobsPeriod delay sids
  = initPeriod delay sendF sids
  where sendF sid = peerIds sids sid

gracePeriod delay sids
  = initPeriod delay f sids
  where f sid = GRACE

stopPeriod delay sids
  = initPeriod delay f sids
  where f sid = STOP

masterProcess sids sendPeriod gracePeriod_ minStopPeriod = do
  sendJobsPeriod sendPeriod sids
  gracePeriod gracePeriod_ sids
  stopPeriod minStopPeriod sids
