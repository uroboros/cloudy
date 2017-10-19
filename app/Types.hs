{-# LANGUAGE DeriveGeneric #-}

module Types where
import Control.Distributed.Process

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

data Job = Job Int Double deriving (Show, Generic, Typeable)
instance Binary Job

data JobQuery = JobQuery ProcessId Int deriving (Show, Generic, Typeable)
instance Binary JobQuery

data SlaveStatus = START | SEND | GRACE | STOP deriving (Show, Generic, Typeable)
instance Binary SlaveStatus

data SlaveResult = SlaveResult Int Double deriving (Show, Generic, Typeable)
instance Binary SlaveResult

data SlaveState = SlaveState {
    _status :: SlaveStatus,
    _peers :: [ProcessId],
    _workQueue :: ProcessId,
    _result :: SlaveResult,
    _tick :: Int} deriving (Show, Generic, Typeable)
instance Binary SlaveState

initSlaveState q tick = SlaveState {_result = SlaveResult 0 0,
                                    _workQueue = q,
                                    _status = START,
                                    _peers = [],
                                    _tick = tick}
