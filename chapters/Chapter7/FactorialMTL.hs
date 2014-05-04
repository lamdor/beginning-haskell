module Chapter7.FactorialMTL where

import Control.Monad.State

-- exercise 7-6. Two states at a time
-- outer is our counter, inner the result
factorial' :: StateT Integer (State Integer) ()
factorial' = do n <- get
                if n == 0
                  then return ()
                  else do
                    lift $ modify (*n)
                    put (n - 1)
                    factorial'

factorial :: Integer -> Integer
factorial n = execState (execStateT factorial' n) 1
