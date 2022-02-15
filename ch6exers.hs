module BahHumbug where

import Data.List

data Mood = Blah | Woot deriving Show

instance Eq Mood where
 Blah == Blah = True
 Woot == Woot = True
 _ == _ = False

settleDown x = 
 if x == Woot then Blah else x

--data Subject = String deriving Show

--data Object = String deriving Show

--data Verb = String deriving Show


--data Sentence = Sentence Subject Verb Object deriving Show

data Rocks =
 Rocks String deriving (Eq, Show)
data Yeah =
 Yeah Bool deriving (Eq, Show)
data Papu =
 Papu Rocks Yeah deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)
-- Was: phew = Papu "chases" True | Guess: No (Correct)

-- The Answer for #2 was correct as well

equalityForall :: Papu -> Papu -> Bool 
equalityForall p p' = p == p'

-- Guess: Yes (Correct)

--comparePapus :: Papu -> Papu -> Bool 
--comparePapus p p' = p > p'

--Guess: No. No instance of Ord. (Correct)


main :: IO()
main = do
 putStrLn "For the Type Matching Section"
 putStrLn "1: Guess: Fails | Result: Fails | Correct!"
 putStrLn "2: Guess: Works | Result: Fails | !@#%$@$!"
 putStrLn "3: Guess: Works | Result: Works | Correct!"
 putStrLn "4: Guess: Works | Result: Works | Correct!"
 putStrLn "5: Guess: Works | Result: Works | Correct!"
 putStrLn "6: Guess: Works | Result: Works | Correct!"
 putStrLn "7: Guess: Fails | Result: Fails | Correct!"
 putStrLn "8: Guess: Fails | Result: Fails | Correct!"
 putStrLn "9: Guess: Works | Result: Works | Correct!"
 putStrLn "10 Guess: Works | Result: Works | Correct!"
 putStrLn "11 Guess: Works | Result: Fails | Correct!"


-- 2
f :: Float
f = 1.0

-- 3
g :: Fractional a => a
g = 1.0

-- 4
h :: RealFrac a => a
h = 1.0

-- 5
freud:: Ord a=> a->a
freud x = x

-- 6
freud':: Int -> Int 
freud' x = x

-- 7
myX=1::Int
sigmund :: Int -> Int 
sigmund x = myX

-- 8 
myY=1::Int
sigmund' :: Int -> Int 
sigmund' x = myY

-- 9 
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11

mySort :: [Char] -> [Char]
mySort = sort
--signifier :: Ord a => [a] -> a
--signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk = \atob -> \a -> \b -> if (atob a) == b then True else False

arith :: Num b => (a -> b) -> Integer -> a -> b 
arith = \atob -> \int -> \a -> (atob a) * (atob a)

-- s1 = Sentence "dogs" "drool"
-- s2 = Sentence "Julie" "loves" "dogs"


