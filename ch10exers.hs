module Ch10exers where


import Data.Time

main :: IO ()
main = do
 putStrLn "#1: b) and c)"
 putStrLn "#2: in comment"
---------------------------------
-- #2
-- foldl (flip(*)) 1 [1..3]
-- foldl (1*1) [2,3]
-- foldl (2*1) [3]
-- foldl (3*2) []
-- foldl (6) []
-- 6
--- Or if you're going with the "Entire spine and all vals are written out before evaluation" method
-- (((1*1) (flip(*)) 2 )(flip(*)) 3)
-- (((1) (flip(*)) 2 )(flip(*)) 3)
-- ((2*1) (flip(*)) 3)
-- ((2) (flip(*)) 3)
-- (3*2)
-- (6)
-- 6
 putStrLn "#3: c)"
 putStrLn "#4: a)"
 putStrLn "#5 a) It needs a value for z. foldr (++) '' ['woot','WOOT','woot']"
 putStrLn "#5 b) foldr max [] ['fear', 'is', 'the', 'little', 'death']"
 putStrLn "#5 c) foldl (\\x -> \\y -> and (x:y)) True [[False],[True]] ||| foldr (&&) True [False, True]"
 putStrLn "#5 d) foldr (||) False [False, True]"
 putStrLn "#5 e) shame *ding*"
 putStrLn "#5 f) foldl (\\x -> \\y -> (++) x (show y)) '' ([1..5])"
 putStrLn "#5 g) foldr (flip const) 0 'tacos'"
 putStrLn "#5 h) foldl (const) 0 'burritos'"
 putStrLn "#5 i) foldl (flip const) 'z' [1..5]"
--foldl :: (b -> a -> b) -> b -> [a] -> b 
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs
data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq,Show,Ord)



theDatabase :: [DatabaseItem]
theDatabase =
 [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
 , DbNumber 9001
 , DbString "Hello World!"
 , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
 ]

--dbDateFilter :: [DatabaseItem] -> Maybe [DatabaseItem]
--dbDateFilter xs = [x | x <- xs, x == DbDate ]
--dbDateFilter (DbDate x) = Just DbDate x
--dbDateFilter _        = Nothing


delEl :: Int -> [a] -> [a]
delEl _ [] = []
delEl n (bs) = let (xs,ys) = splitAt n bs in (init xs) ++ (ys)

--Write a function that filters for DbDate values and returns a list
--of the UTCTime values inside them:

extractTime :: DatabaseItem -> UTCTime
extractTime (DbDate x) = x

dbDateBool :: DatabaseItem -> Bool
dbDateBool (DbDate x) = True
dbDateBool _          = False

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = go xs []
 where go ys acc
        | ys == []              = reverse acc
        | dbDateBool (head ys)  = go (drop 1 ys) ((extractTime . head $ ys) : acc)
        | otherwise = go (drop 1 ys) acc


extractNum :: DatabaseItem -> Integer
extractNum (DbNumber x) = x

dbNumBool :: DatabaseItem -> Bool
dbNumBool (DbNumber x) = True
dbNumBool _            = False

filterDbNum :: [DatabaseItem] -> [Integer]
filterDbNum xs = go xs []
 where go ys acc
        | ys == []              = reverse acc
        | dbNumBool (head ys)  = go (drop 1 ys) ((extractNum . head $ ys) : acc)
        | otherwise = go (drop 1 ys) acc

highestDate :: [DatabaseItem] -> UTCTime
highestDate xs = go (filterDbDate xs)
 where go ys
        | length ys == 1 = head ys
        | (compare (head . drop 1 $ ys) . head $ ys) == LT = go (delEl 2 ys) 
        | (compare (head . drop 1 $ ys) . head $ ys) == EQ = go (drop  1 ys)
        | (compare (head . drop 1 $ ys) . head $ ys) == GT = go (drop  1 ys)

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (filterDbNum xs)

avgDb :: [DatabaseItem] -> Double
avgDb xs = go (foldr (+) 0.0 (map fromIntegral (filterDbNum xs))) (fromIntegral(length (filterDbNum xs)))
 where go summ lgth = summ/lgth

---------------------------------------------------------
--foldr :: (a -> b -> b) -> b -> [a] -> b ---------------
--foldr f z [] = z---------------------------------------
--foldr f z (x:xs) = f x (foldr f z xs)------------------
--const 1 (const 2 ( const 3 ( const 4 ( const 5 'a'))))-
---------------------------------------------------------
-- const :: a->b->a--------------------------------------

lessthan100 xs = [x | x <- xs, x < 100]
fibs =  lessthan100(1 : scanl (+) 1 fibs)

fibsN = take 20 fibs

oldFactorial :: Integer -> Integer
oldFactorial 0 = 1
oldFactorial x = x * (oldFactorial(x-1))

--factorial :: Integer -> Integer
--factorial 0 = 1
--factorial x = foldr (*) 1 (take (fromIntegral x) (scanl (*) x (factorial x-1)))


-- Not my work

factlist :: [Integer]
factlist = scanl (*) 1 (enumFrom 1)
-- unique sig
factorial' :: Int -> Integer
factorial' x = factlist !! (x)

-- until this point


--factorial :: Integer -> Integer
--factorial 0 = 1
--factorial x = go x
 --where go = 

--scanl :: (a -> b -> a) -> a -> [b] -> [a] 
--scanl f q ls =
--q : (case ls of 
--     [] ->[]
---    x:xs -> scanl f (f q x) xs)

wrds xs ys zs = [[x,y,z]| x <- xs, y <- ys, z <- zs]

wrdsc xs ys zs = concat (wrds xs ys zs)



stops = "pbtdkg"
vows  = "aeiou"
wordss = wrds stops vows stops
wrdsp = wrds "p" vows stops

-- I could not resist it, forgive me.

gotChars   = ["Jamie", "Cersei", "Ned", "Sansa", "Tyrion", "Tywin", "Robert", "The Night King", "Margery", "Geoffery", "Tommen", "Robb", "Bran", "Theon", "Daenerys"]
gotActions = [" screws ", " marries ", " murders "]

--realistic = (map concat (wrds gotChars gotActions gotChars)) !! (randomRs ([0..675]))


-- Was S -> Integer, but I had to change it for frac div
notSecret :: String -> Double
notSecret x = (fromIntegral(sum (map length (words x))))/(fromIntegral(length (words x)))

myOr2bgloo :: [Bool] -> Bool
myOr2bgloo = foldr (||) False

--any2bgloo :: (a->Bool) -> [a] -> Bool
--any2bgloo f l = foldr (flip . const) (undefined) l
--theirAny :: (a->Bool) -> [a] -> Bool
--theirAny f = foldr (||) False . map f
--
--myElem2 :: Eq a => a -> [a] -> Bool
--myElem2 x ys = foldl ((==) x (flip const)) undefined ys
theirElem ::  Eq a => a -> [a] -> Bool
theirElem x = foldr (\a b -> b || a == x) False 

 -- SO CLOSE
--myMap :: (a->b) -> [a] -> [b]
--myMap f l = foldl ((:) . f) [] l

theirFilter :: (a -> Bool) -> [a] -> [a]
theirFilter f = foldr (\a b -> if f a then (a:b) else b) []

theirMaximumBy :: (a->a->Ordering) -> [a] -> a
theirMaximumBy f = head . foldr (\a b -> case b of
                                        [] -> [a]
                                        (b:bs) -> if f a b == GT then [a] else [b]
                                ) []

testMinimumBy :: (a->a->Ordering) -> [a] -> a
testMinimumBy f = head . foldr (\a b -> case b of
                                        [] -> [a]
                                        (b:bs) -> if f a b == LT then [a] else [b]
                                ) []
