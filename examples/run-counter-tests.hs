import Abstract.Testing.Counter
import System.Environment

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (a1:a2:[]) -> runCounterTests (read a1 :: Int) (read a2 :: Int)
  _ -> error "usage: ./run-counter-tests <threads> <maxN>"
