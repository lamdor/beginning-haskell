module Chapter4.BinaryTrees where

import Data.List
import Data.Monoid
import Data.Foldable
  
data TravelGuide = TravelGuide { title :: String
                               , authors :: [String]
                               , price :: Double
                               } deriving (Show, Eq, Ord)

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                   | Leaf2
                   deriving (Show, Eq)

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) =
  case compare t v of
    EQ -> Just v
    LT -> treeFind2 t l
    GT -> treeFind2 t r
treeFind2 t Leaf2 = Nothing

-- exercise 4-7
treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) =
  case compare t v of
    EQ -> n
    LT -> Node2 v (treeInsert2 t l) r
    GT -> Node2 v l (treeInsert2 t r)
treeInsert2 t Leaf2 = Node2 t Leaf2 Leaf2

treeConcat2 :: Ord a => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
treeConcat2 n1@(Node2 v1 l1 r1) n2@(Node2 v2 l2 r2) =
  case compare v1 v2 of
    EQ -> Node2 v1 (treeConcat2 l1 l2) (treeConcat2 r1 r2)
    LT -> Node2 v1 (treeConcat2 l1 n2) r1
    GT -> Node2 v1 l1 (treeConcat2 r1 n2)
treeConcat2 n1@(Node2 _ _ _) Leaf2 = n1
treeConcat2 Leaf2 n2@(Node2 _ _ _) = n2
treeConcat2 Leaf2 Leaf2 = Leaf2

sampleTree = Node2 7
               (Node2 4
                (Node2 2 Leaf2 Leaf2)
                (Node2 5 Leaf2 Leaf2))
               (Node2 11
                (Node2 8 Leaf2 Leaf2)
                (Node2 2 Leaf2 Leaf2))

travelGuides = [ TravelGuide "Map of Iowa" ["Terry Brandstad"] 0.99
               , TravelGuide "Extreme Travel" [] 35.99]
 
newtype TravelGuidePrice = TravelGuidePrice TravelGuide deriving (Eq, Show)

instance Ord TravelGuidePrice where
  (TravelGuidePrice (TravelGuide t1 a1 p1)) <= (TravelGuidePrice (TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1 == p2 && (t2 < t2 || (t1 == t2 && a1 <= a2)))

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                     | Leaf3
                     deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r) =
  case compare v v2 of
    EQ -> Node3 v2 c2 l r
    LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
    GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c n@(Node3 v2 c2 l r) =
  case compare v v2 of
    EQ -> n
    LT -> let newLeft = treeInsert4 v c l
              newCache = cached newLeft <> c2 <> cached r
          in Node3 v2 newCache newLeft r
    GT -> let newRight = treeInsert4 v c r
              newCache = cached l <> c2 <> cached newRight
          in Node3 v2 newCache l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty

newtype Min a = Min a deriving Show

instance (Fractional a, Ord a) => Monoid (Min a) where
  mempty = Min infinity where infinity = 1/0
  (Min x) `mappend` (Min y) = Min $ min x y

-- exercise 4-8 Functor Fun!

newtype MyMaybe a = MyMaybe (Maybe a) deriving Show

instance Functor MyMaybe where
  fmap f (MyMaybe (Just a)) = MyMaybe $ Just (f a)
  fmap f (MyMaybe Nothing) = MyMaybe Nothing
  
instance Functor BinaryTree2 where
  fmap f (Node2 v l r) = Node2 (f v) (fmap f l) (fmap f r)
  fmap f Leaf2 = Leaf2

-- exercise 4-9 Foldable Fun!
instance Foldable MyMaybe where
  foldMap f (MyMaybe (Just v)) = f v
  foldMap f (MyMaybe Nothing) = mempty
  
instance Foldable BinaryTree2 where
  foldMap f (Node2 v l r) = (foldMap f l) <> (f v) <> (foldMap f r)
  foldMap f Leaf2         = mempty
