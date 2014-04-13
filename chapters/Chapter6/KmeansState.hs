{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter6.Kmeans.State where

import Control.Lens
import Data.List
import qualified Data.Map as M

class (Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0.0,0.0) lst
                     n = fromIntegral $ length lst
                 in (u / n, v / n)

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

data KMeansState e v = KMeansState { _centroids :: [v], _points :: [e]
                                   , _err :: Double, _threshold :: Double
                                   , _steps :: Int}

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0

clusterAssignments :: (Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignments state =
  let initialMap = M.fromList $ zip (state^.centroids) (repeat [])
  in foldr assignPoint initialMap (state^.points)
  where assignPoint p m =
          let chosenCentroids = minimumBy (\x y -> compare (distance x $ toVector p)
                                                           (distance y $ toVector p))
                                          (state^.centroids)
          in M.adjust (p:) chosenCentroids m


kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i n pts t = view centroids $ kMeans' $ initializeState i n pts t

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state =
  let assignments = clusterAssignments state
      state1 = state & centroids.traversed
                     %~ (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
      state2 = state1 & err .~ sum (zipWith distance (state^.centroids)
                                                     (state1^.centroids))
      state3 = state2 & steps +~ 1
  in if state3^.err < state3^.threshold then state3 else kMeans' state3

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v
