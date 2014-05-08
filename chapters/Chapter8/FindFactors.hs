module Chapter8.FindFactors where

import Control.DeepSeq
import Control.Monad.Par

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                    in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m         = n
               | n `mod` m == 0 = m
               | otherwise      = findFactor n (m + 1)

findTwoFactors_sequential :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors_sequential x y = (findFactors x, findFactors y)

findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY = findFactors y
      _        = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)
