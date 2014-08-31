module Abstract.Testing.Misc (
 atomReplace,
 atomDecr,
 atomIncr,
 atomRetry'IfZero
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

atomReplace fn tv = atomically $ readTVar tv >>= writeTVar tv . fn
atomDecr tv = atomically $ readTVar tv >>= writeTVar tv . (\x -> x - 1)
atomIncr tv = atomically $ readTVar tv >>= writeTVar tv . (\x -> x + 1)
atomRetry'IfZero tv = atomically $ readTVar tv >>= \tv' -> if tv' == 0 then return () else retry
