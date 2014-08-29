module Abstract.Testing.Counter (
 runCounterTests
) where

import Abstract.Interfaces.Counter

import Abstract.Impl.Libs.Counter
 (
  mkCounter'MVar, mkCounter'MVar'Inc, mkCounter'MVar'Dec, mkCounter'MVar'Get,
  mkCounter'IORef, mkCounter'IORef'Inc, mkCounter'IORef'Dec, mkCounter'IORef'Get
 )

--import qualified Abstract.Impl.Libs.Counter.MVar.Internal as LCM

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

counters = do
 mapM (\f -> f 0) $ [mkCounter'MVar "mvar counter"]

runCounterTests ctr threads maxN = do
 a1 <- async (forM_ [1..threads] (\_ -> runCounterTest ctr maxN))
 w <- wait a1
 result <- get ctr
 putStrLn $ "done: " ++ show result
 return ()

runCounterTest ctr n = do
 forM_ [1..n] $ \_ -> incr ctr
 return ()
