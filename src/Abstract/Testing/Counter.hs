module Abstract.Testing.Counter (
 runCounterTests
) where

import Abstract.Interfaces.Counter

import Abstract.Impl.Libs.Counter
 (
  mkCounter'MVar, mkCounter'MVar'Inc, mkCounter'MVar'Dec, mkCounter'MVar'Get,
  mkCounter'IORef, mkCounter'IORef'Inc, mkCounter'IORef'Dec, mkCounter'IORef'Get
 )

import Abstract.Impl.Redis.Counter
 (
  mkCounter'Redis'Int, mkCounter'Redis'Int'Inc, mkCounter'Redis'Int'Dec, mkCounter'Redis'Int'Get, defaultCounterRedisWrapper'Int
 )

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

-- hideous

runCounterTests threads maxN = do
 mvar <- mkCounter'MVar 0
 _ <- runCounterTests' "mvar" mvar threads maxN
 ior <- mkCounter'IORef 0
 _ <- runCounterTests' "ioref" ior threads maxN
 red <- mkCounter'Redis'Int 0 (defaultCounterRedisWrapper'Int "counter")
 _ <- runCounterTests' "redis" red threads maxN
 return ()

runCounterTests' s ctr threads maxN = do
 putStrLn $ "test: " ++ s
 _ <- reset ctr
 test'incr ctr threads maxN
 _ <- reset ctr
 test'incrBy ctr threads maxN
 test'decr ctr threads maxN
 test'gentleReset ctr
 return ()

test'incr ctr threads maxN = do
 a1 <- async (forM_ [1..threads] (\_ -> forM_ [1..maxN] $ \_ -> incr ctr))
 w <- wait a1
 (Just result) <- get ctr
 case (result == (threads * maxN)) of
  True -> putStrLn "test'incr: success" >> return ()
  False -> error $ "test'incr: fail: " ++ (show (threads * maxN)) ++ " /= " ++ show result

test'incrBy ctr threads maxN = do
 a1 <- async (forM_ [1..threads] (\_ -> incrBy ctr maxN >> return ()))
 w <- wait a1
 (Just result) <- get ctr
 case (result == (threads * maxN)) of
  True -> putStrLn "test'incrBy: success" >> return ()
  False -> error $ "test'incrBy: fail: " ++ (show (threads * maxN)) ++ " /= " ++ show result

test'decr ctr threads maxN = do
 a1 <- async (forM_ [1..threads] (\_ -> forM_ [1..maxN] $ \_ -> decr ctr))
 w <- wait a1
 (Just result) <- get ctr
 case (result == 0) of
  True -> putStrLn "test'decr: success" >> return ()
  False -> error $ "test'decr: fail: " ++ (show (threads * maxN)) ++ " /= " ++ show result

test'gentleReset ctr = do
 v <- get ctr
 _ <- gentleReset ctr
 v' <- get ctr
 putStrLn $ show v ++ " " ++ show v'
 case (v == v') of
  True -> putStrLn "test'gentleReset: success" >> return ()
  False -> error $ "test'gentleReset: fail"
