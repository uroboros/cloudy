{-# LANGUAGE TemplateHaskell #-}

module Main where
import Lib

import Types (initSlaveState)
import WorkQueue (workQ, jobQuery)
import Slave (slave)
import Master (masterProcess, calcPeriods)

import System.Environment (getArgs)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import System.Random (mkStdGen)

-- CONFIG =====================================================
-- TODO find homes for...
_masterHost = "127.0.0.1"
_masterPort = "10502"
_slaveCount = 6::Int -- TODO load slaves from config!

_minStopPeriod = 50000::Int
_slaveTick = 100
-- ============================================================

remotable ['slave, 'jobQuery]
rt :: RemoteTable
rt = Main.__remoteTable initRemoteTable

-- TODO spawnLocal -> remote for all spawn functions...
spawnMaster host port = do
  Right transport <- createTransport host port defaultTCPParameters
  pid <- newLocalNode transport rt
  return pid

spawnWorkQueue seed
  = spawnLocal $ do workQ (mkStdGen seed) initIndex
  where initIndex = 1

spawnSlaves workQ tick = do
  let state = initSlaveState workQ tick
  sids <- replicateM _slaveCount $ spawnLocal (slave state)
  return sids

parseArgs k l s minStop = do
    let periods = calcPeriods (read k) (read l) minStop
    liftIO $ putStrLn $ ">>> (sendPeriod, gracePeriod, minStopPeriod) " ++ (show periods)
    return (periods,seed)
  where seed = (read s)::Int

main :: IO ()
main = do
        args <- getArgs
        case args of
          [k, l, s] -> do
            ((sendPeriod, gracePeriod, minStopPeriod),seed) <- parseArgs k l s _minStopPeriod

            master <- spawnMaster _masterHost _masterPort
            runProcess master $ do
              mId <- getSelfNode

              workQueue <- spawnWorkQueue seed
              sids <- spawnSlaves workQueue _slaveTick

              masterProcess sids sendPeriod gracePeriod minStopPeriod

          _ -> do liftIO $ putStrLn $ "Missing params: k, l, s"
                  return ()
