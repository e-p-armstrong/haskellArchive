module Ch8Exers where


putsN :: (Num a, Eq a) => a -> IO ()
putsN 0 = putStrLn "Finished"
putsN x = do 
 putStrLn "WhatIsRubyAnyway" 
 putsN (x - 1)


-- applytimes 5 (+1) 5

-- = (+1) (applytimes 5 - 1) (+1) 5)

-- 
-- = (+1) (+1) (applytimes 4 - 1) (+1) 5)

-- = (+1) (+1) (+1) (applytimes 3 - 1) (+1) 5)

-- = (+1) (+1) (+1) (+1) (applytimes 2 - 1) (+1) 5)

-- = (+1) (+1) (+1) (+1) (+1) (applytimes 1 - 1) (+1) 5)

-- = (+1) (+1) (+1) (+1) (+1) (applytimes 0) (+1) 5)

-- = (+1) (+1) (+1) (+1) (+1) (5)

-- = 10

mainn :: IO ()
mainn = do
 putStrLn "Chapter Exercises"
 putStrLn " Subsection: Reviewing Types"
 putStrLn " 1. d)"
 putStrLn " 2. b)"
 putStrLn " 3. d)"
 putStrLn " 4. b)"
 putStrLn " Subsection: Reviewing Currying"
 putStrLn " 1. the value returned is 'woops mrow woohoo!                              - Correct!"
 putStrLn " 2. the value returned is '1 mrow haha                                     - Correct!"
 putStrLn " 1. the value returned is 'woops mrow 2 mrow haha'                         - Correct!"
 putStrLn " 1. the value returned is 'woops mrow blue mrow haha'                      - Correct!"
 putStrLn " 1. the value returned is 'pink mrow haha mrow green mrow woops mrow blue  - Correct!"
 putStrLn " 1. the value returned is 'are mrow pugs mrow awesome                      - Correct!"
 -- Ugh, not again!
 putStrLn " Subsection: Recursion"
 putStrLn " 1. dividedBy 15 2 ="
 putStrLn "  go (15 - 2) 2 (count + 1)"
 putStrLn "  go (13 - 2) 2 (count + 1)"
 putStrLn "  go (11 - 2) 2 (count + 1)"
 putStrLn "  go (9  - 2) 2 (count + 1)"
 putStrLn "  go (7  - 2) 2 (count + 1)"
 putStrLn "  go (5  - 2) 2 (count + 1)"
 putStrLn "  go (3  - 2) 2 (count + 1)"
 putStrLn "  1 < 2 = True"
 putStrLn "  (7, 1)"
 putStrLn " 2. "



type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

thenumberonceunitedmustdivide :: Numerator -> Denominator -> Quotient
thenumberonceunitedmustdivide x y
 | x > y =  1 + thenumberonceunitedmustdivide (x - y) (y)
 | otherwise = 0

-- Below are the strange functions copied from the exercises

--cattyConny :: String -> String -> String
--cattyConny x y = x ++ " mrow " ++ y

--flippy :: String -> String -> String
--flippy = flip cattyConny

--appedCatty :: String -> String
--appedCatty = cattyConny "woops" 

--frappe :: String -> String
--frappe = flippy "haha"

data ProvingKnowledge = 
 Result Integer | DivisionByZeroIsFoolish deriving Show

dividedBy :: Integral a => a -> a -> (a, a) 
dividedBy num denom = go num denom 0
 where go n d count
        | n == 0 || d == 0 = error "Stop Tying to divide by zero you idiot!"
        | n < d && n >= 0 = (count, n)
        | n < 0 && (n + d) > 0 = ((negate count), n)
        | n < 0 && d < 0 = go ((negate n) - (negate d)) (negate d) (count + 1)
        | n > 0 && d < 0 = go ((negate n) + (negate d)) (negate d) (count + 1)
        | n < 0 = go (n + d) d (count + 1) 
        | otherwise = go (n - d) d (count + 1)

dividedBy2ElecBoogaloo :: Integer -> Integer -> (Integer, ProvingKnowledge) 
dividedBy2ElecBoogaloo num denom = go num denom 0
 where go n d count
        | n == 0 || d == 0 = (0,DivisionByZeroIsFoolish)
        | n < d && n >= 0 = (count, (Result n))
        | n < 0 && (n + d) > 0 = ((negate count), (Result n))
        | n < 0 && d < 0 = go ((negate n) - (negate d)) (negate d) (count + 1)
        | n > 0 && d < 0 = go ((negate n) + (negate d)) (negate d) (count + 1)
        | n < 0 = go (n + d) d (count + 1) 
        | otherwise = go (n - d) d (count + 1)


sumIt :: Integer -> Integer
sumIt n = goo n 0
 where goo num count 
        | count >  num = count
        | count <= num = count + goo (num-1) (count+1)



intmultiply :: Integral a => a -> a -> a
intmultiply a b = go a b 0
 where go alpha beta count
        | beta < 0 && alpha < 0 = (flip go) (negate alpha) (negate beta) 0
        | beta < 0 = (flip go) alpha beta 0
        | beta == count = 0
        | beta > count  = alpha + go alpha beta (count + 1)


mc91 :: Integral a => a -> a
mc91 x
 | x > 100  = x - 10
 | x <= 100 = mc91( mc91 (x+11))


--mc91apps :: Integral a => a -> (a,a)
--mc91apps x = go x 0
 --where go x appls
   --     | x > 100  = x - 10
     --   | x <= 100 = go( go (x+11) (appls + 1)

digitToWord :: Int -> String
digitToWord n
        | n == 0 = "Zero-"
        | n == 1 = "One-"
        | n == 2 = "Two-"
        | n == 3 = "Three-"
        | n == 4 = "Four-"
        | n == 5 = "Five-"
        | n == 6 = "Six-"
        | n == 7 = "Seven-"
        | n == 8 = "Eight-"
        | n == 9 = "Nine-"
        | otherwise = "Ah! "


digitToWordNew :: Int -> String
digitToWordNew n
        | n == 0 = "Zero"
        | n == 1 = "One"
        | n == 2 = "Two"
        | n == 3 = "Three"
        | n == 4 = "Four"
        | n == 5 = "Five"
        | n == 6 = "Six"
        | n == 7 = "Seven"
        | n == 8 = "Eight"
        | n == 9 = "Nine"
        | otherwise = "Ah! "


--braveattemptdigits :: Int -> IO ()
--braveattemptdigits n = go n [0] 0 
 --where go num output count
  --      | length (show n) >= 50000 = putStrLn "This number is too large! Please make it smaller so that you don't break your computer (Or this program)."
    --    | count >= 50000 = print output
      --  | otherwise = go ((read . tail . show) num) (((:[]) . read . listify . head . show $ num) ++ output) (count + 1)

--digits :: Int -> [Int]
--digits n = go n [] 0
 --where go n listoutp count
  --      | count >= (length . show $ n) =  0
   --     |  go n (listoutp ++ (mod n ((10^(length . show n))/(10^count))))

listify :: Char -> [Char]
listify x = [x]

lengthOf :: Int -> Int
lengthOf = length . show

digitsFinal :: Int -> [Int] 
digitsFinal x = go x [] 0
 where go :: Int -> [Int] -> Int -> [Int] 
       go n listout count
        | count >= (length . show $ n) = reverse listout
        | otherwise = go n  ((:[]) ((div (mod n (10^((lengthOf n) - count)))) (10^((lengthOf n) - count - 1))) ++ listout) (count + 1)

wordNumber :: Int -> String
wordNumber x = (take ((length digitsToString) - 1) digitsToString)
 where digitsToString = (concat (map digitToWord (digitsFinal x)))


















-- mock application of digitsFinal
-- digitsFinal 123 =
-- 



-- I will not disgrace myself nor the programming language of haskell with such gross work. I will do this correctly tomorrow. 
-- concat as 


-- div (mod n (#digits)) (#digits/10)
-- (length . show n) - count for # of times I need to apply the function
-- 10^count for scale
-- mod the first time (use a special guard  for count == 0 that applies mod not div and then re-applies the function)
-- concatinate every new number into an initially empty list (these work, they just don't if the number is not in a list. Store this list throughout apps through an arg.)

-- mod n ((10^(length . show n))/(10^count))
-- ()


--9,223,372,036,854,775,807
-- 18



