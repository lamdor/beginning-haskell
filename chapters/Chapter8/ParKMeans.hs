{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter8.ParKmeans where

import Data.List
import Control.Monad.Par
import Data.Monoid
import qualified Data.Map as M

class (Show v, Ord v, NFData v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0.0,0.0) lst
                     n = fromIntegral $ length lst
                 in (u / n, v / n)

class (Vector v, NFData e) => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double,Double) (Double,Double) where
  toVector = id
  
closestCentroid :: (Vector v, Vectorizable e v) => [v] -> e -> (v, e)
closestCentroid centroids point = 
   let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector point)
                                                   (distance y $ toVector point))
                                  centroids
   in (chosenCentroid, point)

mergeAllToMap :: Ord k => [(k,a)] -> M.Map k [a]
mergeAllToMap = M.fromListWith (++) . map (\(k,v) -> (k, [v]))
      
clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points = 
  mergeAllToMap $ runPar (parMap (closestCentroid centroids) points)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold =
  let totalDistance = foldr (\(x,y) s -> s + distance x y) 0.0 centroids in
  totalDistance < threshold

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
initializeSimple n v = (fromIntegral n * 10 * negOrPos, fromIntegral n * 5 * negOrPos) : initializeSimple (n - 1) v
  where negOrPos = if n `mod` 2 == 0 then -1 else 1

initialize123 :: Int -> [e] -> [(Double, Double)]
initialize123 3 _ = [(1,1), (2,2), (3,3)]

samplePointsAround :: (Double, Double) -> Double -> [(Double,Double)]
samplePointsAround (x,y) r =
  let half = r/2 in do
    x' <- [(x-half)..(x+half)]
    y' <- [(y-half)..(y+half)]
    return (x', y')

samplePoints :: [(Double, Double)]
samplePoints = (samplePointsAround (100,100) 100) ++
               (samplePointsAround (100,-1000) 100) ++
               (samplePointsAround (0,10000) 1000) ++
               (samplePointsAround (-1000,10) 300)

runKMeansOnSamplePoints = kMeans initializeSimple 4 samplePoints 0.001
