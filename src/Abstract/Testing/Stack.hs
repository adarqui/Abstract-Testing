{-# LANGUAGE OverloadedStrings #-}
module Abstract.Testing.Stack (
 runStackTests
) where

import Abstract.Testing.Misc
import Abstract.Interfaces.Stack
import Abstract.Impl.Redis.Stack

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import Data.List

import qualified Data.ByteString.Char8 as B


intStacks = [
  ("redis", mkStack'Redis (defaultStackRedis "queue" B.pack B.unpack))
 ]

runStackTests threads maxN = do
 forM_ intStacks
  (\(name, mk) -> mk >>= \q -> runStackTests' name q threads maxN)
 return ()

runStackTests' s q threads maxN = do
 putStrLn $ "----- testing: " ++ s
 test'destroy q
 test'push q threads maxN
 test'pop q $ show maxN
 test'destroy q
 test'pushBatch q big_data
 test'pop q $ show maxN
 test'destroy q
 test'push q threads maxN
 test'drain q big_data
 test'destroy q
 return ()
 where
  big_data = [ show j | i <- [1..threads], j <- [1..maxN] ]


test'push q threads maxN = do
 tc <- atomically $ newTVar threads
 forM_ [1..threads] (\_ -> forkIO $ (forM_ [1..maxN] $ \n -> push q (show n)) `finally` atomDecr tc)
 atomRetry'IfZero tc
 sz <- size q
 case (sz == (threads * maxN)) of
  True -> putStrLn "test'push: success"
  False -> error $ "test'push: fail: " ++ (show (threads * maxN)) ++ " /= " ++ show sz


test'pushBatch q l = do
 _ <- pushBatch q l
 sz <- size q
 case (sz == (length l)) of
  True -> putStrLn "test'pushBatch: success"
  False -> error $ "test'pushBatch: fail: " ++ (show (length l)) ++ " /= " ++ show sz


test'pop q res = do
 (Just v) <- pop q
 case (v == res) of
  True -> putStrLn "test'pop: success"
  False -> error $ "test'pop: fail: " ++ (show v) ++ " /= " ++ (show res)


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
