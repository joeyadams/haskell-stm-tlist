module TChan (
    TChan,
    newTChan,
    newTChanIO,

    readTChan,
    writeTChan,
    dupTChan,
    unGetTChan,
    isEmptyTChan,
) where

import Control.Concurrent.STM (STM)
import Data.STM.TList

type TChan a = (TCursor a, TCursor a)

newTChan :: STM (TChan a)
newTChan = newTCursorPair

newTChanIO :: IO (TChan a)
newTChanIO = newTCursorPairIO

readTChan :: TChan a -> STM a
readTChan = readTCursor . fst

writeTChan :: TChan a -> a -> STM ()
writeTChan = writeTCursor . snd

dupTChan :: TChan a -> STM (TChan a)
dupTChan (_, writeEnd) = do
    newReadEnd <- dupTCursor writeEnd
    return (newReadEnd, writeEnd)

unGetTChan :: TChan a -> a -> STM ()
unGetTChan = unGetTCursor . fst

isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan = isEmptyTCursor . fst
