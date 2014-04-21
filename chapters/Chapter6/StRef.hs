{-# LANGUAGE RankNTypes #-}
import Control.Monad.ST
import Data.STRef
  
-- intExample :: forall s. ST s Integer
-- intExample = newSTRef 0

listLength :: [a] -> Integer
listLength list = runST $ do l <- newSTRef 0
                             traverseList list l
                             readSTRef l
                  where traverseList []     _ = return ()
                        traverseList (_:xs) l = do modifySTRef' l (+1)
                                                   traverseList xs l
