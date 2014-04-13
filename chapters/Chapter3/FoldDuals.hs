{-# LANGUAGE LambdaCase #-}

module Chapter3.FoldDuals where

foldr2 :: (Maybe (a,b) -> b) -> [a] -> b
foldr2 f [] = f Nothing
foldr2 f (x:xs) = f $ Just (x, foldr2 f xs)

foldAsFold2 :: (a -> b -> b) -> b -> [a] -> b
foldAsFold2 f z =
  foldr2 (\case Nothing -> z
                Just (a, b) -> f a b)

mapAsFold2 :: (a -> b) -> [a] -> [b]
mapAsFold2 f = foldr2 (\case Nothing -> []
                             Just (x,xs) -> f x : xs)
