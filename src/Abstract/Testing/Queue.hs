{-# LANGUAGE OverloadedStrings #-}
module Abstract.Testing.Queue (
-- runQueueTests
) where

import Abstract.Interfaces.Queue
import Abstract.Impl.Redis.Queue

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

import qualified Data.ByteString.Char8 as B

intQueues = [
  ("redis", mkQueue'Redis (defaultQueueRedis "queue" B.pack B.unpack))
 ]

runQueueTests threads maxN = do
 forM_ intQueues
  (\(name, mk) -> mk >>= \q -> runQueueTests' name q threads maxN)
 return ()

runQueueTests' s q threads maxN = do
 putStrLn $ "----- testing: " ++ s
 _ <- destroy q
 test'enqueue q threads maxN
 return ()


test'enqueue q threads maxN = do
 a1 <- async (forM_ [1..threads] (\_ -> forM_ [1..maxN] $ \n -> enqueue q (show n)))
 _ <- wait a1
 result <- size q
 case (result == (threads * maxN)) of
  True -> putStrLn "test'enqueue: success" >> return ()
  False -> error $ "test'enqueue: fail: " ++ (show (threads * maxN)) ++ " /= " ++ show result
