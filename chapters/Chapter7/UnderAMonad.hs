module Chapter7.UnderAMonad where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM addPrefix

logInformation :: [String] -> Writer String () 
logInformation = mapM_ (\s -> tell (s ++ "\n"))

logInformation' infos =
  forM_ infos $ \s ->
    tell (s ++ "\n")

-- exercise 7-5. A definition for sequence and mapM

mySequence :: Monad m => [m a] -> m [a]
mySequence [] = return []
mySequence (mx:ms) = do
  x <- mx
  xs <- mySequence ms
  return (x : xs)

myMapM :: Monad m => (a -> m b) -> [a] -> m [b]
myMapM f = sequence . map f

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM (\f x -> tell (Sum 1) >> return (f * x)) 1 [1 .. n]
  
