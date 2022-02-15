module Ch7Exers where

main :: IO ()
main = do
 putStrLn "The 'Grab Bag'"
 putStrLn " 1. All are equivalent"
 putStrLn " 2. d is correct."
 putStrLn " 3. a) f = \\n -> n + 1"
 putStrLn " 3. b) addFive = \\x -> \\y -> (if x > y then y else x) + 5"
 putStrLn " 3. c) mflip f x y = f y x"
 putStrLn "The 'Variety Pack'"
 putStrLn " 1. a) (a,b) -> a - Correct!"
 putStrLn " 1. b) Num a => a - my initial one was incorrect but I can be forgiven. No it is not the same type as k1 or 3"
 putStrLn " 1. b)onus) [Char]- Correct!"
 putStrLn " 1. c) k1 and k3 will return 3 as a result"
 putStrLn "Case Practice"
 putStrLn "functionC x y = case (x>y) of True  -> x False -> y"
 putStrLn "'Artful Dodgy'"
 putStrLn " 1. It returns 1  - Correct!"
 putStrLn " 2. It returns 11 - Correct!"
 putStrLn " 3. It returns 22 - Correct!"
 putStrLn " 4. It returns 21 - Correct!"
 putStrLn " 5. It returns 12 - Correct!"
 putStrLn " 6. It returns 11 - Correct!"
 putStrLn " 7. It returns 21 - Correct!"
 putStrLn " 8. It returns 21 - Correct!"
 putStrLn " 10.It returns 22 - Correct!"
 putStrLn " 11.It returns 31 - Correct!"
 putStrLn " 12.It returns 23 - Correct!"
 putStrLn "'Guard Duty'"
 putStrLn " 1. The function will return an 'F' in all cases, as that is what its highest-priority Guard is set to do."
 putStrLn " 1. No, it does not work the same way. As these guards are written like y >= x.x, with no upper limit besides higher priority guards, if C is put at the top spot then everything above it will also evaluate to C as C's guard has the highest priority. "
 putStrLn " 3. b)"
 putStrLn " 4. Lists"
 putStrLn " 5. Eq a => [a] -> Bool"
 putStrLn " 6. c)"
 putStrLn " 7. (Num a, Ord a) => a"
 putStrLn " 8. (Ord a, Num a, Num b) => b"
 putStrLn " Main section"
 putStrLn " 1. d) - Correct!"
 putStrLn " 2. b) - Correct!"
 putStrLn " 3. d) - Correct!"
 putStrLn " 4. b) - Correct!"
 putStrLn " 5. a) - Correct!"
 putStrLn "Let's Write Code!"
 putStrLn " 1. a) Done. See 'tensDigit2'"
 putStrLn " 1. b) No, because divMod returns a tuple whereas div and mod seperately do not."
 putStrLn " 1. c) "
 




addOneIfOdd n = case odd n of 
 True -> f n
 False -> n
 where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x


lameadd1 :: Num a => (a,a) -> a
lameadd1 = \(x,y) -> x + y

lameadd2 :: Num a => (a,a) -> a
lameadd2 = \x -> fst x + snd x

fst4 :: (a,b,c,d) -> a
fst4 (x,y,z,p) = x

lst4 :: (a,b,c,d) -> d
lst4 (x,y,z,p) = p



fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x

lst3 :: (a,b,d) -> d
lst3 (x,y,z) = z


caseSynFunc :: (Num a,Ord a) => a -> IO ()
caseSynFunc c = 
    case c + 1 > 0 of
        True -> putStrLn "HUZZAH!"
        False -> putStrLn "doubleplus unhuzzah"


f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f = \(x,y,z) -> \(j,p,q) -> ((x,j), (z,q))

functionC x y = case (x>y) of 
 True  -> x
 False -> y

evenAdd n = case even n of
 True -> n + 2
 False -> n

nums x = case compare x 0 of
 LT -> -1 
 GT -> 1
 _  -> 0

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO () 
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'


employeeRank :: ( Employee -> Employee -> Ordering ) -> Employee -> Employee -> IO ()
employeeRank f e a = case f e a of
 GT -> reportBoss e a 
 EQ -> putStrLn "Right there you have a committee. No one outranks anyone."
 LT -> (flip reportBoss) e a

breamComeTrue :: Employee -> Employee -> Ordering
breamComeTrue Veep Veep = EQ
breamComeTrue Veep _    = GT
breamComeTrue _ Veep    = LT
breamComeTrue e a = compare e a

dodgy x y = x + y * 10 
oneIsOne = dodgy 1 
oneIsTwo = (flip dodgy) 2

--dodgy 1 1 
--dodgy 2 2
--dodgy 1 2 
--dodgy 2 1 
--oneIsOne 1
--oneIsOne 2 
--oneIsTwo 1 
--oneIsTwo 2
--oneIsOne 3 
--oneIsTwo 3


rippedAbs :: Integer -> Integer
rippedAbs x
 | x < 0 = (negate x)
 | True  = x

guardDogYrs :: Integer -> Integer
guardDogYrs x
 | x <= 0    =     0
 | x <= 1    = x * 15
 | x <= 2    = x * 12
 | x <= 4    = x * 8
 | otherwise = x * 6

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

invdivMod x y = divMod y x

tensDigit2 :: Integral a => a -> (a,a)
tensDigit2 x = invdivMod 10 . div x $ 10

hunsDigit :: Integral a => a -> a
hunsDigit x = snd . invdivMod 10 . div x $ 100

foldBool :: a -> a -> Bool -> a 
foldBool q p d 
 | d == False = q
 | d == True  = p 

foldBool2 :: a -> a -> Bool -> a
foldBool2 q p d = case ( d == False) of
 True  -> q
 False -> p

showint :: Int -> [Char]
showint x = show x

g :: (a -> b) -> (a,c) -> (b,c)
g ab (a,c) = ((ab a), c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . showint . read . show


mmain = do
 print (roundTrip 4 :: Int) 
 print (id 4)



-- newtype, type, and data.
-- newtype = one constructor one field
-- type    = synonym
-- data    = anything (except synonym)