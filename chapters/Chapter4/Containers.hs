module Chapter4.Containers where
       
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl', partition)
import Data.Tree
import Data.Graph
  
-- exercise 4-2
insertAsAlter :: Ord k => k -> a -> M.Map k a -> M.Map k a
insertAsAlter k a = M.alter (\_ -> Just a) k

deleteAsAlter :: Ord k => k -> M.Map k a -> M.Map k a
deleteAsAlter = M.alter (\_ -> Nothing)

adjustAsAlter :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjustAsAlter f k = M.alter (\mv -> fmap f mv) k

-- exercise 4-3
data ClientKind = GovOrgKind
                | CompanyKind
                | IndividualKind
                deriving (Ord, Show, Eq)

data Client i = GovOrg {clientId :: i, clientName :: String}
              | Company {clientId :: i, clientName :: String
                        , person :: Person, duty :: String}
              | Individual {clientId :: i, person :: Person}
              deriving (Show, Ord, Eq)
                       
data Person = Person String String Gender deriving (Show, Ord, Eq)
data Gender = Male | Female | Unknown deriving (Show, Ord, Eq)

kindOf :: Client i -> ClientKind
kindOf (GovOrg _ _)      = GovOrgKind
kindOf (Company _ _ _ _) = CompanyKind
kindOf (Individual _ _)  = IndividualKind

classifyClients_1 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients_1 clients = foldl' classifyAndAdd M.empty clients
  where classifyAndAdd m c = M.alter (addOrCreateSet c) (kindOf c) m
        addOrCreateSet c Nothing = Just $ S.singleton c
        addOrCreateSet c (Just s) = Just $ S.insert c s

someClients :: [Client Integer]
someClients = [ GovOrg 1 "TMTF"
              , Company 2 "Me Inc" (Person "Luke" "Amdor" Male) "Me"
              , Individual 3 (Person "Luke" "Amdor" Unknown)
              , GovOrg 4 "NASA"
              , GovOrg 5 "IRS"
              , Company 6 "Google" (Person "Sergey" "Brin" Male) "CEO"
              , Individual 7 (Person "Benjamin" "Franklin" Male)]

manyClients = take 500000 $ cycle someClients

classifyClients_2 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients_2 clients = M.fromList [ (GovOrgKind, S.fromList govorgs)
                                       , (CompanyKind, S.fromList companies)
                                       , (IndividualKind, S.fromList indivs)]
  where
    kindOfIs kind c = (kindOf c) == kind
    (govorgs, others) = partition (kindOfIs GovOrgKind) clients
    (companies, indivs) = partition (kindOfIs CompanyKind) others

classifyClients_3 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients_3 clients = M.fromListWith S.union $ map classifyWithSet clients
  where classifyWithSet c = (kindOf c, S.singleton c)

sampleTree = Node 1 [ Node 2 [ Node 3 []
                             , Node 4 []
                             , Node 5 []]
                    , Node 6 []]

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) =
  let subtreesTraversed = concat $ map (preOrder f) subtrees
  in f v : subtreesTraversed

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
  [("wood","wood",["walls"]), ("plastic","plastic",["walls","wheels"])
  ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
  ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]

timeMachinePrecedence :: (Graph, Vertex -> (String,String,[String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

timeMachineTravel :: Graph
timeMachineTravel = buildG (103,2013)
                      [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
                      ,(2013,1408),(1408,1993),(1408,917),(1993,917),(907,103),(103,917)]

