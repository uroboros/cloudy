module Slave where
import Lib
import Types (Job(..), JobQuery(..), SlaveStatus(..), SlaveResult(..), SlaveState(..), initSlaveState)
import WorkQueue (jobQuery)

import Control.Distributed.Process

updateResult ::  SlaveResult -> Job -> SlaveResult
updateResult (SlaveResult accI accS) (Job i r)
  = SlaveResult (accI + 1) (accS + (fromIntegral i) * r)

checkForStatusChangeOrJob :: SlaveState -> Maybe SlaveStatus -> Maybe Job -> Process ()
checkForStatusChangeOrJob state (Just statusChange) _
  = do
      say $ ">>> STATUS CHANGE >>> " ++ (show statusChange)
      slave state {_status = statusChange}

checkForStatusChangeOrJob state Nothing (Just job)
  = slave $ state {_result = result'}
  where result' = updateResult (_result state) job

checkForStatusChangeOrJob state Nothing Nothing
  = do
      case (_status state) of
        SEND -> sendJobsToPeers state >>= slave
        GRACE -> slave state
  where sendJobsToPeers state = do
          let peers = (_peers state)
          -- steal Jobs from WorkQ
          jobs <- jobQuery (_workQueue state) (length peers)
          -- send each peer a Job message
          mapM_ (uncurry send) (zipWith (,) peers jobs)
          -- update STATE/compute result
          return $ state {_result = foldl updateResult (_result state) jobs}

slave :: SlaveState -> Process ()
slave state = do
      case (_status state) of
        START -> do -- wait for setup message
          receiveWait [match $ \peers -> slave state {_peers = peers, _status = SEND}]

        STOP -> do -- show results
          say $ show (_result state)

        _ -> do -- SEND or GRACE
          maybeStatusChange <- expectTimeout (_tick state) :: Process (Maybe SlaveStatus)
          maybeJob <- expectTimeout (_tick state) :: Process (Maybe Job)

          checkForStatusChangeOrJob state maybeStatusChange maybeJob
