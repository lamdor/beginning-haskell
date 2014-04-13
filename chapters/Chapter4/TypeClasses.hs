module Chapter4.TypeClasses where

class Nameable n where
  name :: n -> String

initial :: Nameable n => n -> Char
initial = head . name

data Client i = GovOrg {clientId :: i, clientName :: String}
              | Company {clientId :: i, clientName :: String
                        , person :: Person, duty :: String}
              | Individual {clientId :: i, person :: Person}
              deriving (Show, Eq)
                       
data Person = Person { firstName :: String , lastName :: String }
            deriving (Show, Ord, Eq)

someClients :: [Client Integer]
someClients = [ GovOrg 1 "TMTF"
              , Company 2 "Me Inc" (Person "Luke" "Amdor") "Me"
              , Individual 3 (Person "Luke" "Amdor")]

instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = l } } = f ++ " " ++ l
  name c = clientName c

-- exercise 4-4
class Priceable p where
  price :: p -> Double

totalPrice :: Priceable p => [p] -> Double
totalPrice = sum . map price

-- exercise 4-6
instance Eq i => Ord (Client i) where
  compare c1 c2 =
    let nameo = compare (name c1) (name c2)
    in if nameo /= EQ then nameo
       else
         case (c1, c2) of
           ((Individual _ p1), (Individual _ p2)) -> compare p1 p2
