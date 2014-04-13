{-# LANGUAGE TemplateHaskell #-}

module Chapter6.Lenses where

import Control.Lens

data Client i = GovOrg { _identifier :: i, _name :: String }
              | Company { _identifier :: i, _name :: String
                        , _person :: Person, _duty :: String }
              | Individual { _identifier :: i, _person :: Person }
              deriving Show
                       
data Person = Person { _firstName :: String , _lastName :: String }
            deriving Show

fullName :: Simple Lens Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                     f:l:_ -> Person f l
                                     _     -> error "Incorrect name")
makeLenses ''Client
makeLenses ''Person

-- exercise 6-2. Time Machine Lenses

data TimeMachine = TimeMachine { _manufacturer :: String
                               , _model :: Int
                               , _modelName :: String
                               , _canTravelToPast :: Bool
                               , _canTravelToFuture :: Bool
                               , _price :: Double
                               } deriving (Show, Eq)

makeLenses ''TimeMachine

increasePricesByPercentage :: Double -> [TimeMachine] -> [TimeMachine]
increasePricesByPercentage p = traverse.price %~ (* (1 + p))

someTimeMachines = [ TimeMachine "Acme Inc" 1 "The First" True False 100.0
                   , TimeMachine "Dr Who" 1 "???" True True 1000.0
                   ]
