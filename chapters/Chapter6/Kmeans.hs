{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter6.Kmeans where

import Data.List
import qualified Data.Map as M

class (Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0.0,0.0) lst
                     n = fromIntegral $ length lst
                 in (u / n, v / n)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double,Double) (Double,Double) where
  toVector = id

clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
  in foldr assignPoint initialMap points
  where assignPoint p m =
          let chosenCentroids = minimumBy (\x y -> compare (distance x $ toVector p)
                                                           (distance y $ toVector p))
                                          centroids
          in M.adjust (p:) chosenCentroids m

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -- intialization function
                                       -> Int -- number of centroids
                                       -> [e] -- the information
                                       -> Double -- threshold
                                       -> [v] -- final centroids
kMeans i k points = kMeans' (i k points) points

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
  in if shouldStop oldNewCentroids threshold
     then newCentroids
     else kMeans' newCentroids points threshold

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

-- exercise 6-1: Counting the number of steps
kMeans_numberOfSteps :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -- intialization function
                                       -> Int -- number of centroids
                                       -> [e] -- the information
                                       -> Double -- threshold
                                       -> (Int, [v]) -- (number of steps, final centroids)
kMeans_numberOfSteps i k points = kMeans_numberOfSteps' (i k points) points 0

kMeans_numberOfSteps' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Int -> Double -> (Int, [v])
kMeans_numberOfSteps' centroids points currentNumberOfSteps threshold =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
      numberOfSteps = currentNumberOfSteps + 1
  in if shouldStop oldNewCentroids threshold
     then (numberOfSteps, newCentroids)
     else kMeans_numberOfSteps' newCentroids points numberOfSteps threshold
