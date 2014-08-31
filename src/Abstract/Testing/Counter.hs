module Abstract.Testing.Counter (
 runCounterTests
) where

import Abstract.Testing.Misc
import Abstract.Interfaces.Counter
import Abstract.Impl.Libs.Counter
import Abstract.Impl.Redis.Counter
import Abstract.Impl.Memcache.Counter

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
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
 test'decr ctr threads maxN
 _ <- reset ctr
 test'incrBy ctr threads maxN
 test'decrBy ctr threads maxN
 test'gentleReset ctr
 return ()

test'incr ctr threads maxN = do
 tc <- atomically $ newTVar threads
 forM_ [1..threads] (\_ -> forkIO $ (forM_ [1..maxN] $ \_ -> incr ctr) `finally` atomDecr tc)
 atomRetry'IfZero tc
 (Just result) <- get ctr
 case (result == (threads * maxN)) of
  True -> putStrLn "test'incr: success" >> return ()
  False -> error $ "test'incr: fail: " ++ (show (threads * maxN)) ++ " /= " ++ show result

test'incrBy ctr threads maxN = do
 tc <- atomically $ newTVar threads
 forM_ [1..threads] (\_ -> forkIO $ (forM_ [1..maxN] $ \_ -> incrBy ctr maxN) `finally` atomDecr tc)
 atomRetry'IfZero tc
 (Just result) <- get ctr
 case (result == (threads * maxN * maxN)) of
  True -> putStrLn "test'incrBy: success" >> return ()
  False -> error $ "test'incrBy: fail: " ++ (show (threads * maxN * maxN)) ++ " /= " ++ show result

test'decr ctr threads maxN = do
 tc <- atomically $ newTVar threads
 forM_ [1..threads] (\_ -> forkIO $ (forM_ [1..maxN] $ \_ -> decr ctr) `finally` atomDecr tc)
 atomRetry'IfZero tc
 (Just result) <- get ctr
 case (result == 0) of
  True -> putStrLn "test'decr: success" >> return ()
  False -> error $ "test'decr: fail: " ++ show result ++ " /= 0"

test'decrBy ctr threads maxN = do
 tc <- atomically $ newTVar threads
 forM_ [1..threads] (\_ -> forkIO $ (forM_ [1..maxN] $ \_ -> decrBy ctr maxN) `finally` atomDecr tc)
 atomRetry'IfZero tc
 (Just result) <- get ctr
 case (result == 0) of
  True -> putStrLn "test'decr: success" >> return ()
  False -> error $ "test'decr: fail: " ++ show result ++ " /= 0"

test'gentleReset ctr = do
 v <- get ctr
 _ <- gentleReset ctr
 v' <- get ctr
 putStrLn $ show v ++ " " ++ show v'
 case (v == v') of
  True -> putStrLn "test'gentleReset: success" >> return ()
  False -> error $ "test'gentleReset: fail"
