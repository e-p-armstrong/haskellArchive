{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module CatsandDogsandGoatsOhMy where

data Pugtype = PugData deriving Show

data Huskytype a = HuskyData deriving Show

data DogueDeBordeaux doge = DogueDeBordeaux doge deriving Show

pugFunc  = PugData

huskFunc :: Huskytype a
huskFunc = HuskyData

huskfunk2elecbglo :: Huskytype [[[[[[[[[[[[[[[[[[[[[[[[[[[String]]]]]]]]]]]]]]]]]]]]]]]]]]] -> Huskytype [[[[[[[[[[[[[[[[[[[[[[[[[[[String]]]]]]]]]]]]]]]]]]]]]]]]]]]
huskfunk2elecbglo a = HuskyData

dogefunc :: DogueDeBordeaux Int
dogefunc = DogueDeBordeaux 100

dogefunc2 :: Int -> DogueDeBordeaux Int
dogefunc2 x = DogueDeBordeaux x

dogefunc3 :: Int -> DogueDeBordeaux Int
dogefunc3 = DogueDeBordeaux 

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq,Show)

-- Here be planes and automobiles, but not trains

data Price = Price Integer deriving (Eq,Show)

data Size = Size Integer deriving (Eq,Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq,Show)

--isCarp :: Vehicle -> Bool
--isCarp x = x /= Plane PapuAir && x /= Plane CatapultsR'Us && x /= Plane TakeYourChancesUnited

isCar :: Vehicle -> Bool
isCar (Car x y) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Car x y) = False
isPlane _ = True
--isPlame :: Vehicle -> Bool
--isPlame x = x == Plane PapuAir || x == Plane CatapultsR'Us || x == Plane TakeYourChancesUnited

areCars :: [Vehicle] -> [Bool]
areCars x = map isCar x

getManu :: Vehicle -> Manufacturer
getManu (Car x y) = x
getManu _         = error "This is a plane you fool! All of them are made by Boeing! You don't need to ask this question!"

myCar = Car Mini (Price 14000) 
urCar = Car Mazda (Price 20000) 
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir
--Basic nullary const demo
data Example = MakeExample String deriving (Show, Eq)

newtype Goats = Goats Int deriving (Show,TooMany)

type Cows = Int

newtype IntStr = IntStr (Int,String) deriving Show

newtype MultGoats = MultGoats (Int,Int) deriving Show

class TooMany a where
 tooMany :: a -> Bool

instance TooMany IntStr where
 tooMany (IntStr (a,b)) = a > 42

instance TooMany Int where
 tooMany n = n > 42

instance TooMany Integer where
 tooMany n = n > 42

instance TooMany MultGoats where
 tooMany (MultGoats(a,b)) = (a+b) > 84 -- Twice the fields equals twice the space for Goats!

instance TooMany (Int,String) where
 tooMany (a,b) = a > 42

--instance TooMany (a,a) where
-- tooMany :: (Num a,TooMany a) => (a,a) -> Bool
 --tooMany (a,b) = a > 42 && b > 42
 
instance (Num a, TooMany a) => TooMany (a,a) where
 tooMany (a,b) = tooMany a || tooMany b

--class ToooMany a where
 --toooMany :: (Num a, ToooMany a, Ord a) => (a,a) -> Bool
--instance ToooMany (a,a) where
-- toooMany (a,b) = a > 42 && b > 42

--instance ToooMany (Int,Int) where
 --toooMany (a,b) = a > 42 && b > 42
--

--newtype TooManyTup a = TooManyTup ((Num a, TooMany a) => (a,a)) deriving Show

--instance TooMany Goats where
-- tooMany (Goats n) = n > 100 -- You can never have too many goats


type Gardener = String

--data Gardenia = Gardenia deriving Show
--data Daisy    = Daisy deriving Show
--data Rose     = Rose deriving Show
--data Lilac    = Lilac deriving Show

data Garden = Gardenia  Garden Gardener| Daisy  Garden Gardener|   Rose  Garden Gardener|  Lilac  Garden Gardener deriving Show
---
type Annoying = Bool
type Namee = String
data Sum a b = First a | Second b deriving (Eq, Show)
data GoatTraits = GoatTraits Namee Annoying deriving Show
data Animal = Goat GoatTraits

--- 
--- instance TooMany Cows where
--- tooMany n = n > 42 

data Person = Person { name :: String, age :: Integer} deriving (Show, Eq)

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show, Enum)
data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript deriving (Eq, Show, Enum)
data Programmer = 
 Programmer { os :: OperatingSystem
 , lang :: ProgLang}
 deriving (Eq,Show)
  
--Takes one of OS, makes list items with all proglangs w/ successor, and then takes the successor of that OS
allProgrammers :: [Programmer]
allProgrammers = go GnuPlusLinux Haskell
 where go opsys progl
        | opsys == Windows && progl == PureScript = (Programmer opsys progl) : [] -- end and print
        | progl == PureScript = (Programmer opsys progl) : go (succ opsys) Haskell -- go to next opsys
        | otherwise = (Programmer opsys progl) : go opsys (succ progl) -- go to next progl


jm = Person "Julie" 108
ca = Person "Chris" 16

--It's almost harvesting season!
newtype Name    = Name String deriving Show
newtype Acres   = Acres Int deriving Show

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

prettyFarmerType (Farmer x _ y) = print( noBackslash (drop 5 ((show x)++" is a "++ (show y))))
noBackslash xs = [x | x <- xs, x /= '\\',x /= '\"']

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq,Ord,Show)

mapTree :: (a  -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left c right) = Node (mapTree f left) (f c) (mapTree f right) 

testTree' :: BinaryTree Integer
testTree' = 
 Node (Node Leaf 3 Leaf)
      1
      (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = 
 Node (Node Leaf 4 Leaf)
      2
      (Node Leaf 5 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left)++(preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (preorder left)++(preorder right)++[a]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (preorder left)++[a]++(preorder right)


testTree :: BinaryTree Integer
testTree =
 Node (Node Leaf 1 Leaf) 2
 (Node Leaf 3 Leaf)


testPreorder :: IO ()
testPreorder =
 if preorder testTree == [2,1,3]
 then putStrLn "Huzzah"
 else putStrLn "FAILURE *blam*!"


testInorder :: IO ()
testInorder =
 if inorder testTree == [1,2,3]
 then putStrLn "Huzzah"
 else putStrLn "FAILURE *blam*!"

testPostorder :: IO ()
testPostorder =
 if postorder testTree == [1,3,2]
 then putStrLn "Huzzah"
 else putStrLn "FAILURE *blam*!"

testsCheck :: IO ()
testsCheck = do
 testPreorder
 testInorder
 testPostorder
lazyfoldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b 
lazyfoldrTree _ z Leaf = z
lazyfoldrTree f z xs = foldr f z (preorder xs) 



-- foldTree f (f c (foldTree f z left)) right

--
--foldlTree :: (a -> b -> b) -> b -> BinaryTree a -> b 
--foldlTree _ _ Leaf = z
--foldlTree f z (Node left c right) = f z (foldTree f c left) (foldTree f c right) 

--foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b 
--foldrTree f z ts = 
 --case ts of 
   --Leaf -> z
   --(Node left c right) -> f c (foldrTree f z left) (f z (foldrTree f z right)) 
-------------------------------------------------------------Folding Trees
--foldr :: (a -> b -> b) -> b -> [a] -> b 
--foldr f z xs =
-- case xs of
-- [] -> z
-- (x:xs) -> f x (foldr f z xs)



-----------------------------------------------

main :: IO ()
main = do
 putStrLn "Exercises: Dog Types"
 putStrLn "#1: Type constructor"
 putStrLn "#2: * -> *"
 putStrLn "#3: *"
 putStrLn "#4: Num a => Doggies a"
 putStrLn "#5: Doggies Integer"
 putStrLn "#6: Doggies [Char]"
 putStrLn "#7: One cannot tell just by what is provided: it could be either, for they have the same name"
 putStrLn "#8: doge -> DogueDeBordeaux doge"
 putStrLn "#9: DogueDebordeaux [Char]"
 putStrLn "\nAll correct!"
 putStrLn "Exercises: Vehicles"
 putStrLn "#4: It would throw an error"
 putStrLn "Exercises: Cardinality"
 putStrLn "I wish I had more DIRECTION for this part"
 putStrLn "#001: 1"
 putStrLn "#010: 3"
 putStrLn "#011: 65536"
 putStrLn "#100: Binary!"
 putStrLn "Exercises: For Example"
 putStrLn "#1: MakeExample :: Example, and an error for the second case"
 putStrLn "#2: It tells you the datatype and what instances are defined."
 putStrLn "#3: MakeExample :: String -> Example"
 putStrLn "#3: MakeExample :: String -> Example"
 putStrLn "Exercises: Logic Goats"
 putStrLn "#1 instance TooMany IntStr where; tooMany (IntStr (a,b)) = a > 42"
 putStrLn "#2 instance TooMany MultGoats where; tooMany (MultGoats(a,b)) = (a+b) > 84 -- Twice the fields equals twice the space for Goats!"
 putStrLn "#3 instance (Num a, TooMany a) => TooMany (a,a) where; tooMany (a,b) = tooMany a || tooMany b"
 putStrLn "Exercises: Pity the Bool"
 putStrLn "#1: 2"
 putStrLn "#2: 258"
 putStrLn "Exercises: The Quad"
 putStrLn "#1: 4 "
 putStrLn "#2: 16"
 putStrLn "#3: 512"
 putStrLn "#4: 8"
 putStrLn "#5: 16"
 putStrLn "#6: 258"
 putStrLn "#7: 65536 - I believe this is an x-bit number."
 putStrLn "Checked, and guessed correctly with my first try: this is the maxBound of a 16 bit number!"