module Slave where
import Lib
import Types (XMessage(..), XMessageQuery(..), SlaveStatus(..), SlaveResult(..), SlaveState(..))
import Data.List(elemIndex)
import Control.Distributed.Process
import System.Random (StdGen, randomR)

peerIds sid sids = filter (/= sid) sids

-- Calculate the "Parent node" for a give node, which is just the "node to the left"
-- Since the nodes form a ring, we must calc with wraparound (the last pid is before the first)
getParentPid pid peers
  = case (elemIndex pid peers) of
      Nothing -> error $ "Pid not found: " ++ (show pid) ++ " " ++ (show peers)
      Just i  -> if (i - 1) == -1
                  then (last peers)
                  else peers!!(i-1)

-- Is fromPid the parent of thisPid?
isParent thisPid fromPid ring
  = (fromPid == getParentPid thisPid ring)

-- Does fromPid occur in the Ring of nods?
isFromRing fromPid ring = case (elemIndex fromPid ring) of
                              Nothing -> False
                              (Just _) -> True

-- Update the SlaveResult with the given XMessage (index,rand)
updateResult ::  SlaveResult -> XMessage -> SlaveResult
updateResult (SlaveResult accI accS) (XMessage _ i r)
  = SlaveResult (accI + 1) (accS + (fromIntegral i) * r)

nextRandom :: StdGen -> (StdGen,Double)
nextRandom gen = (gen', 1.0 - r) -- (1 - r) -- 1-[0,1) = (0,1]
  where (r,gen') = randomR (0,1) gen

-- Given an index 'i', construct a new XMessage (with new random number and index = (i+1))
-- Then forward the msg to all peers.
sendMsgToPeers state i = do
    let peers = (_peers state)

    -- calculate the forwardMessage: use thisPid as fromPid and (i+1) as index
    thisPid <- getSelfPid
    let (gen', r) = nextRandom (_gen state)
    let xMessage' = (XMessage thisPid (i+1) r)

    -- send new message to all peers
    mapM_ ((flip send) xMessage') peers

    -- update STATE/compute result
    let result' = updateResult (_result state) xMessage'
    return $ state {_result = result', _gen = gen'}

-- Respond to potential incoming messages: Maybe SlaveStatus -> Maybe XMessage
checkForStatusChangeOrXMsg :: SlaveState -> Maybe SlaveStatus -> Maybe XMessage -> Process ()

-- If we received a statusChange message, then update the slave Status and recur
checkForStatusChangeOrXMsg state (Just statusChange) _
  = do
      say $ ">>> STATUS CHANGE >>> " ++ (show statusChange)
      slave state {_status = statusChange}

-- If we received an XMsg (and no statusChange) -> the Msg can either be:
--  1) a "Kickstart message" from outside the ring => send message to peers
--  2) a message from the Parent node of the receiving node => send message to peers (unless we're in GRACE period)
--  3) a message from a non-parent ring node => update state with original message and recur
checkForStatusChangeOrXMsg state Nothing (Just xMessage)
  = do
      thisPid <- getSelfPid
      let fromPid = (_fromPid xMessage)
      let ring = (_ring state)

      -- TODO yuk! simplify!
      if (isFromRing fromPid ring)
        then do
          -- since message is from Ring node, update the node's result
          let result' = updateResult (_result state) xMessage
          let state' = state {_result = result'}

          if (isParent thisPid fromPid ring)
            then do -- (2) Msg from parent: update state with original message + sent message
              if ((_status state) == GRACE)
                then slave state' -- don't send during GRACE period
                else do
                      state'' <- sendMsgToPeers state' (_i xMessage)
                      slave state''

            else slave state' -- (3)

        else do -- (1) Msg NOT from ring, i.e. it's a kickstart message
            state'' <- sendMsgToPeers state (_i xMessage) -- note: we don't update our state with the original Msg
            slave state''

-- If we received a neither statusChange or XMsg -> do nothing and just recur
checkForStatusChangeOrXMsg state Nothing Nothing
  = slave state

slave :: SlaveState -> Process ()
slave state = do
      case (_status state) of
        START -> do -- wait for "init slave" message -> then move into SEND mode
          thisPid <- getSelfPid
          receiveWait [match $ \ringNodes -> slave state {_peers = peerIds thisPid ringNodes,
                                                          _ring = ringNodes,
                                                          _status = SEND}]

        STOP -> do -- display results
          say $ show (_result state)

        _ -> do -- SEND or GRACE: wait for messages (but don't send any during GRACE period)
          maybeStatusChange <- expectTimeout (_tick state) :: Process (Maybe SlaveStatus)
          maybeJob <- expectTimeout (_tick state) :: Process (Maybe XMessage)

          checkForStatusChangeOrXMsg state maybeStatusChange maybeJob
