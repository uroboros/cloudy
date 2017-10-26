module Master where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)

import Types (SlaveStatus(..),XMessage(..))
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

-- Send a Msg to all sids
sendSlaves msg sids = mapM_ ((flip send) msg) sids

-- Send a single message to a slave to kickstart the sending process.
-- Because the sender (master) is not on the ring of slave nodes, the slave will interpret this msg as a "kickstart"
kickstartSending sid = do
    masterPid <- getSelfPid
    say $ "Send KICKSTART message to >>> " ++ (show sid)
    send sid (XMessage masterPid initIndex dummyRand)
  where initIndex = 0
        dummyRand = 1.0

masterProcess :: [ProcessId] -> Int -> Int -> Int -> Process ()
masterProcess sids sendPeriod gracePeriod minStopPeriod = do
  -- SEND PERIOD -----------------------------
  -- initialise Slaves by sending each one all the slaveIds on the Ring
  sendSlaves sids sids
  -- send a message to any slave, say the first, to kickstart the sending process
  kickstartSending (head sids)

  liftIO $ threadDelay sendPeriod

  -- GRACE PERIOD ----------------------------
  sendSlaves GRACE sids
  liftIO $ threadDelay gracePeriod

  -- STOP PERIOD -----------------------------
  sendSlaves STOP sids
  liftIO $ threadDelay minStopPeriod
