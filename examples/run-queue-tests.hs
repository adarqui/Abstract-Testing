import Abstract.Testing.Queue
import System.Environment

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (a1:a2:[]) -> runQueueTests (read a1 :: Int) (read a2 :: Int)
  _ -> error "usage: ./run-queue-tests <threads> <maxN>"
