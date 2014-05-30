import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random
  
main :: IO ()
main = do v <- newTVarIO 10000
          s <- newTVarIO [("a",7)]
          forkDelay 5 $ atomically $ updateMoneyAndStock "a" 1000 v s
          -- forkDelay 5 $ printMoneyAndStock v s
          return ()

updateMoney :: MVar Integer -> IO ()
updateMoney v = do m <- takeMVar v
                   putStrLn $ "Updating value, which is " ++ show m
                   putMVar v (m + 500) -- suppose a constant price

readMoney :: MVar Integer -> IO ()
readMoney v = do m <- readMVar v
                 putStrLn $ "The current value is " ++ show m

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3,15)
                 threadDelay (r * 1000000)

forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)

updateMoneyAndStock :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
updateMoneyAndStock product price money stock =
  do s <- readTVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- readTVar money
               let newS = map (\(k,v) -> if k == product then (k, v-1) else (k,v)) s
               writeTVar money (m - price) >> writeTVar stock newS
       else return ()

-- printMoneyAndStock :: Show a => TVar Integer -> TVar [(a,Integer)] -> IO ()
-- printMoneyAndStock money stock = do m <- readTVar money
--                                     s <- readTVar stock
--                                     putStrLn $ show m ++ "\n" ++ show s
