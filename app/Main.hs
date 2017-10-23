{-# LANGUAGE TemplateHaskell #-}

module Main where
import Lib

import Types (initSlaveState)
import WorkQueue (workQ, jobQuery)
import Slave (slave)
import Master (masterProcess, calcPeriods)

import System.Environment (getArgs)
import Control.Monad (replicateM, forM)
import Control.Applicative ((<$>))

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node hiding (newLocalNode)
import Control.Distributed.Process.Backend.SimpleLocalnet

import System.Random (mkStdGen)
import Data.Word
import Control.Concurrent (forkIO, threadDelay)

-- SLAVE CONFIG -------------
_minStopPeriod = 50000::Int
_slaveTick = 200
-----------------------------

remotable ['slave, 'jobQuery]
rt :: RemoteTable
rt = Main.__remoteTable initRemoteTable

startSlaveNode host port = do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend

spawnSlave :: NodeId -> ProcessId -> Int -> Process ProcessId
spawnSlave sid workQ tick = do
  let state = initSlaveState workQ tick
  -- TODO? pid <- spawn sid ($(mkClosure 'slave) state)
  pid <- spawnLocal (slave state)
  return pid

spawnSlaves :: [NodeId] -> ProcessId -> Int -> Process [ProcessId]
spawnSlaves slaves workQ tick = do
  forM slaves $ \sid -> spawnSlave sid workQ tick

spawnWorkQueue seed
  = spawnLocal $ do workQ (mkStdGen seed) initIndex
  where initIndex = 1

parseArgs k l s minStop = do
    let periods = calcPeriods (read k) (read l) minStop
    liftIO $ putStrLn $ ">>> (sendPeriod, gracePeriod, minStopPeriod) " ++ (show periods)
    return (periods,seed)
  where seed = (read s)::Int

main :: IO ()
main = do
        args <- getArgs
        case args of
          ["master", host, port, k, l, s] -> do
            ((sendPeriod, gracePeriod, minStopPeriod),seed) <- parseArgs k l s _minStopPeriod

            backend <- initializeBackend host port initRemoteTable
            startMaster backend (\nids -> do
                                            say $ "SLAVES: " ++ (show nids)
                                            workQueue <- spawnWorkQueue seed
                                            sids <- spawnSlaves nids workQueue _slaveTick
                                            masterProcess sids sendPeriod gracePeriod minStopPeriod)

          ["slave", host, port] -> do
            startSlaveNode host port

          _ -> do liftIO $ putStrLn $ "Missing params..."
                  return ()
