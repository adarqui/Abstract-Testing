module Abstract.Testing.Counter (
 runCounterTests
) where

import Abstract.Interfaces.Counter

import Abstract.Impl.Libs.Counter
 (
  mkCounter'MVar, mkCounter'MVar'Inc, mkCounter'MVar'Dec, mkCounter'MVar'Get,
  mkCounter'IORef, mkCounter'IORef'Inc, mkCounter'IORef'Dec, mkCounter'IORef'Get
 )

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

-- hideous

runCounterTests threads maxN = do
 mvar <- mkCounter'MVar "mvar counter" 0
 _ <- runCounterTests' mvar threads maxN
 ior <- mkCounter'IORef "ioref counter" 0
 _ <- runCounterTests' ior threads maxN
 red <- mkCounter'IORef "redis counter" 0
 _ <- runCounterTests' red threads maxN
 return ()

runCounterTests' ctr threads maxN = do
 _ <- reset ctr
 a1 <- async (forM_ [1..threads] (\_ -> runCounterTest ctr maxN))
 w <- wait a1
 (Just result) <- get ctr
 case (result == (threads * maxN)) of
  True -> putStrLn "success" >> return ()
  False -> error $ "Fail: " ++ (show (threads * maxN)) ++ " /= " ++ show result

runCounterTest ctr n = do
 forM_ [1..n] $ \_ -> incr ctr
 return ()
