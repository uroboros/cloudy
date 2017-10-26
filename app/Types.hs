{-# LANGUAGE DeriveGeneric #-}

module Types where
import Control.Distributed.Process

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import System.Random (StdGen, mkStdGen)

-- Type of message sent between nodes
-- _i -> message index, _r -> Random (0,1]
data XMessage = XMessage {_fromPid::ProcessId, _i::Int, _r::Double} deriving (Show, Generic, Typeable)
instance Binary XMessage

-- Confirmation of receipt: sent from the recipient of an XMessage to the sender
data XMessageReceipt = XMessageReceipt {_xReceived::XMessage, _recipientId::ProcessId} deriving (Show, Generic, Typeable)
instance Binary XMessageReceipt

-- Approves that original xMessage was received by all recipients: sent from the original
--   sender of an XMessage to original recipients to indicate consensus reached w.r.t the particular xMessage
data XMessageApproved = XMessageApproved {_xApproved::XMessage, _origRecipientId::ProcessId} deriving (Show, Generic, Typeable)
instance Binary XMessageApproved

-- The Type of query, sent by slaves to Master, to pull the next xMessage from Master
-- _pid -> Id of Slave sending the Query to Master
data XMessageQuery = XMessageQuery {_pid::ProcessId} deriving (Show, Generic, Typeable)
instance Binary XMessageQuery

-- Slaves move through these states
data SlaveStatus = START | SEND | GRACE | STOP deriving (Show, Generic, Typeable, Eq)
instance Binary SlaveStatus

-- The type of alorithm result for each slave
data SlaveResult = SlaveResult Int Double deriving (Show, Generic, Typeable)
instance Binary SlaveResult

data SlaveState = SlaveState {
    _status :: SlaveStatus, -- processing status
    _peers :: [ProcessId],  -- peer node ids
    _ring :: [ProcessId], -- all node ids in their "ring" order
    _result :: SlaveResult, -- Accumulate the algorithm result
    _gen :: StdGen, -- Random number generator
    _tick :: Int} deriving (Show, Generic, Typeable)
-- TODO instance Binary SlaveState

initSlaveState tick seed = SlaveState {_result = SlaveResult 0 0,
                                          _status = START,
                                          _peers = [],
                                          _ring = [],
                                          _gen = (mkStdGen seed),
                                          _tick = tick}
