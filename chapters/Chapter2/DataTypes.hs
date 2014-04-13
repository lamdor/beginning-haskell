module Chapter2.DataTypes where

data Client i = GovOrg {clientId :: i, clientName :: String}
              | Company {cleientId :: i, clientName :: String
                        , person :: Person, duty :: String}
              | Individual {clientId :: i, person :: Person}
              deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving (Show, Eq)


data TimeMachine = TimeMachine { manufacturer :: String
                               , model :: Int
                               , name :: String
                               , canTravelToPast :: Bool
                               , canTravelToFuture :: Bool
                               , price :: Double
                               } deriving (Show, Eq)
