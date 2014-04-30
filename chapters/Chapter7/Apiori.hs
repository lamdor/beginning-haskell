module Chapter7.Apiori where

import Control.Monad
import Data.List (unfoldr)
import Data.Set (Set)
import qualified Data.Set as S

-- Clients
data Client = GovOrg { clientName :: String }
            | Company    { clientName :: String, person :: Person, duty :: String }
            | Individual { person :: Person }
            deriving (Show, Eq, Ord)
                     
data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Show, Eq, Ord)
data Person = Person { firstName :: String, lastName :: String, gender :: Gender }
            deriving (Show, Eq, Ord)
data Gender = Male | Female | UnknownGender deriving (Show, Eq, Ord)

-- Products
data Product = Product { productId :: Integer, productType :: ProductType }
             deriving (Show, Eq, Ord)
data ProductType = TimeMachine | TravelGuide | Tool | Trip deriving (Show, Eq, Ord)

data Purchase = Purchase { client :: Client, products :: [Product] } deriving (Show, Eq, Ord)

data PurchaseInfo = InfoClientKind ClientKind
                  | InfoClientDuty String
                  | InfoClientGender Gender
                  | InfoPurchasedProduct Integer
                  | InfoPurchasedProductType ProductType
                  deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord)

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo = foldr
  (\(Product i t) pinfos -> S.insert (InfoPurchasedProduct i) $
                            S.insert (InfoPurchasedProductType t) pinfos)
  S.empty

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c ps) =
  Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo ps

-- Exercise 7-3. Clients into items
clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo c@(GovOrg _) = clientKindPurchaseInfo c
clientToPurchaseInfo c@(Company _ _ d) = S.insert (InfoClientDuty d) $
                                         clientKindPurchaseInfo c
clientToPurchaseInfo c@(Individual (Person _ _ g)) =
  S.insert (InfoClientGender g) $
  clientKindPurchaseInfo c

clientKindPurchaseInfo :: Client -> Set PurchaseInfo
clientKindPurchaseInfo = S.singleton . InfoClientKind . clientKind
  where clientKind (GovOrg _) = KindGovOrg
        clientKind (Company _ _ _) = KindCompany
        clientKind (Individual _) = KindIndividual

newtype FrequentSet = FrequentSet (Set PurchaseInfo) deriving (Eq, Ord)

data AssocRule = AssocRule (Set PurchaseInfo) (Set PurchaseInfo) deriving (Eq, Ord)
instance Show AssocRule where
  show (AssocRule a b) = show a ++ " => " ++ show b

setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) =
  let total = length trans
      supp = length (filter (\(Transaction tElts) -> sElts `S.isSubsetOf` tElts) trans)
  in fromIntegral supp / fromIntegral total

ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence trans (AssocRule a b) =
  setSupport trans (FrequentSet $ a `S.union` b) / setSupport trans (FrequentSet a)

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions =
  noDups $ do Transaction t <- transactions
              e <- S.toList t
              let fs = FrequentSet $ S.singleton e
              guard $ setSupport transactions fs > minSupport
              return fs

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateNextLk :: Double -> [Transaction] -> (Int, [FrequentSet])
                         -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupport transactions (k, lk) =
  let lk1 = noDups $ do FrequentSet a <- lk
                        FrequentSet b <- lk
                        guard $ S.size (a `S.intersection` b) == k - 1
                        let fs = FrequentSet $ a `S.union` b
                        guard $ setSupport transactions fs > minSupport
                        return fs
  in Just (lk1, (k+1, lk1))

generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets =
  do FrequentSet fs <- sets
     subset@(_:_) <- powerset $ S.toList fs
     let ssubset = S.fromList subset
         rule = AssocRule ssubset (fs `S.difference` ssubset)
     guard $ ruleConfidence transactions rule > minConfidence
     return rule

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

apiori :: Double -> Double -> [Transaction] -> [AssocRule]
apiori minSupport minConfidence transactions =
  generateAssocRules minConfidence transactions
    $ concat $ unfoldr (generateNextLk minSupport transactions)
                       (1, generateL1 minSupport transactions)
