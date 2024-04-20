module Chapter11Scratch (isCar, isPlane) where

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

newtype Price
  = Price Integer
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

data QuantumBool
  = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

data TwoQs
  = MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

data Person
  = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show)

data Fiction = Fiction deriving (Show)

data NonFiction = NonFiction deriving (Show)

data BookType
  = FictionBook Fiction
  | NonFictionBook NonFiction
  deriving (Show)

data List a = Nil | Cons a (List a) deriving (Show)

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)
