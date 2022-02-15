module Addition where

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1+1 is greater than 1" $ do
            (1+1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            (2+2) ` shouldBe` 4
        it "returns 5 when 15 is divided by 3" $ do
            dividedBy 15 3 `shouldBe` (5,0)
        it "x+1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
        it "except when x == 9223372036854775807" $ do
            ((9223372036854775807 + 1) :: Int) `shouldBe`(negate 9223372036854775808)

multiplicationTest :: IO ()
multiplicationTest = hspec $ do
    describe "intmultiply" $ do
        it "returns 100 when 10 is added ten times" $ do
            intmultiply 10 10 `shouldBe` 100
        it "returns 50 when negative 5 is added negative 10 times" $ do
            intmultiply (negate 5) (negate 10) `shouldBe` 50
        it "returns negative 50 when 5 is added negative 10 times" $ do
            intmultiply 5 (negate 10) `shouldBe` (negate 50)
        it "returns 0 when 1000 is asdded 0 times" $ do
            intmultiply 1000 0 `shouldBe` 0

dividedBy :: Integral a => a -> a -> (a, a) 
dividedBy num denom = go num denom 0
 where go n d count
        | d == 0 = error "Stop Tying to divide by zero you idiot!"
        | n < d && n >= 0 = (count, n)
        | n < 0 && (n + d) > 0 = ((negate count), n)
        | n < 0 && d < 0 = go ((negate n) - (negate d)) (negate d) (count + 1)
        | n > 0 && d < 0 = go ((negate n) + (negate d)) (negate d) (count + 1)
        | n < 0 = go (n + d) d (count + 1) 
        | otherwise = go (n - d) d (count + 1)

intmultiply :: Integral a => a -> a -> a
intmultiply a b = go a b 0
 where go alpha beta count
        | beta < 0 && alpha < 0 = (flip go) (negate alpha) (negate beta) 0
        | beta < 0 = (flip go) alpha beta 0
        | beta == count = 0
        | beta > count  = alpha + go alpha beta (count + 1)


sayHello :: IO ()
sayHello = putStrLn "Salutations!"

trivialInt :: Gen Int
trivialInt = return 1

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT,EQ,GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 < x

runQc :: IO ()
runQc = quickCheck prop_additionGreater