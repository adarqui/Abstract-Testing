module Abstract.Testing.Misc (
 atomReplace
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

atomReplace fn x = atomically $ readTVar x >>= writeTVar x . fn
