{-# LANGUAGE TemplateHaskell #-}

module Main where
import Lib

import Types (initSlaveState)
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
_minStopPeriod = 200000::Int
_slaveTick = 200
-----------------------------

remotable [] -- TODO remotable ['slave]
rt :: RemoteTable
rt = Main.__remoteTable initRemoteTable

startSlaveNode host port = do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend

spawnSlave :: NodeId -> Int -> Int -> Process ProcessId
spawnSlave sid tick seed = do
  let state = initSlaveState tick seed
  -- TODO? pid <- spawn sid ($(mkClosure 'slave) state)
  pid <- spawnLocal (slave state)
  return pid

spawnSlaves :: [NodeId] -> Int -> Int -> Process [ProcessId]
spawnSlaves slaves tick seed = do
  forM slaves $ \sid -> spawnSlave sid tick seed

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
                                            sids <- spawnSlaves nids _slaveTick seed
                                            masterProcess sids sendPeriod gracePeriod minStopPeriod
                                            terminateAllSlaves backend)

          ["slave", host, port] -> do
            startSlaveNode host port

          _ -> do liftIO $ putStrLn $ "Missing params..."
                  return ()
