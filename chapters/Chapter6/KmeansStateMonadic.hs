{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter6.Kmeans.State.Monadic where

import qualified Data.Map as M
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List

-- type State s a = s -> (a,s)

-- thenDo :: State s a -> (a -> State s b) -> State s b
-- thenDo f g s = let (resultOfF, stateAfterF) = f s
--                in g resultOfF stateAfterF

class (Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

class Vector v => Vectorizable e v where
  toVector :: e -> v

data KMeansState v = KMeansState { _centroids :: [v]
                                 , _threshold :: Double
                                 , _steps :: Int }
                   deriving Show

makeLenses ''KMeansState

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0.0,0.0) lst
                     n = fromIntegral $ length lst
                 in (u / n, v / n)

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
  in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                    (distance y $ toVector p))
                                                   centrs
                    in M.adjust (p:) chosenCentroid m)
           initialMap points

kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points = do prevCentrs <- use centroids
                    let assignments = clusterAssignments prevCentrs points
                        newCentrs   = newCentroids assignments
                        
                    centroids .= newCentrs
                    steps += 1
                    t <- use threshold
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    if err < t then return newCentrs else kMeans' points

initialState :: (Vector v, Vectorizable e v)
             => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState v
initialState i k pts t = KMeansState (i k pts) t 0

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i k pts t = evalState (kMeans' pts) (initialState i k pts t)

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

samplePoints = [(2,2),(1,1),(3,3)] :: [(Double,Double)]
