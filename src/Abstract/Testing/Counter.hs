module Abstract.Testing.Counter (
 runCounterTests
) where

import Abstract.Interfaces.Counter
import Abstract.Impl.Libs.Counter
import Abstract.Impl.Redis.Counter
import Abstract.Impl.Memcache.Counter

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

intCounters = [
  ("mvar", mkCounter'MVar 0),
  ("ioref", mkCounter'IORef 0),
  ("redis", mkCounter'Redis'Int 0 (defaultCounterRedis'Int "counter")),
  ("memcache", mkCounter'Memcache'Int 0 (defaultCounterMemcache'Int "counter"))
 ]

runCounterTests threads maxN = do
 forM_ intCounters
  (\(name, mk) -> mk >>= \ctr -> runCounterTests' name ctr threads maxN)
 return ()

runCounterTests' s ctr threads maxN = do
 putStrLn $ "----- testing: " ++ s
 _ <- reset ctr
 test'incr ctr threads maxN
 _ <- reset ctr
 test'incrBy ctr threads maxN
 test'decr ctr threads maxN
 test'gentleReset ctr
 return ()

test'incr ctr threads maxN = do
 a1 <- async (forM_ [1..threads] (\_ -> forM_ [1..maxN] $ \_ -> incr ctr))
 _ <- wait a1
 (Just result) <- get ctr
 case (result == (threads * maxN)) of
  True -> putStrLn "test'incr: success" >> return ()
  False -> error $ "test'incr: fail: " ++ (show (threads * maxN)) ++ " /= " ++ show result

test'incrBy ctr threads maxN = do
 a1 <- async (forM_ [1..threads] (\_ -> incrBy ctr maxN >> return ()))
 _ <- wait a1
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
