{-# LANGUAGE OverloadedStrings #-}
module Abstract.Testing.Queue (
 runQueueTests
) where

import Abstract.Testing.Misc
import Abstract.Interfaces.Queue
import Abstract.Impl.Redis.Queue

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import Data.List

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
 test'destroy q
 test'enqueue q threads maxN
 test'dequeue q "1"
 test'destroy q
 test'enqueueBatch q big_data
 test'dequeue q "1"
 test'destroy q
 test'enqueue q threads maxN
 test'drain q big_data
 test'destroy q
 return ()
 where
  big_data = [ show j | i <- [1..threads], j <- [1..maxN] ]


test'enqueue q threads maxN = do
 tc <- atomically $ newTVar threads
 forM_ [1..threads] (\_ -> forkIO $ (forM_ [1..maxN] $ \n -> enqueue q (show n)) `finally` atomDecr tc)
 atomRetry'IfZero tc
 sz <- size q
 case (sz == (threads * maxN)) of
  True -> putStrLn "test'enqueue: success"
  False -> error $ "test'enqueue: fail: " ++ (show (threads * maxN)) ++ " /= " ++ show sz


test'enqueueBatch q l = do
 _ <- enqueueBatch q l
 sz <- size q
 case (sz == (length l)) of
  True -> putStrLn "test'enqueueBatch: success"
  False -> error $ "test'enqueueBatch: fail: " ++ (show (length l)) ++ " /= " ++ show sz


test'dequeue q res = do
 (Just v) <- dequeue q
 case (v == res) of
  True -> putStrLn "test'dequeue: success"
  False -> error $ "test'dequeue: fail: " ++ (show v) ++ " /= " ++ (show res)


test'destroy q = do
 _ <- destroy q
 sz <- size q
 case (sz == 0) of
  True -> putStrLn "test'destroy: success"
  False -> error $ "test'destroy: fail: " ++ (show sz) ++ " /= 0"


test'drain q tot = do
 v <- drain q
 case (ltot == (length v)) of
  True -> case (uniq tot == uniq v) of
   True -> putStrLn "test'drain: success"
   False -> error $ "test'drain: fail: Length check passed. Data validation failed."
  False -> error $ "test'drain: fail: " ++ (show (length v)) ++ "/= " ++ show ltot
 where
  ltot = length tot
  uniq = nub . sort
