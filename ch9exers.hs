module Blacklisted where

import Data.Char

noExceptionTail :: [a] -> Maybe [a]
noExceptionTail [] = Nothing
noExceptionTail (_ : []) = Nothing 
noExceptionTail (_ : x) = Just x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

saferHead :: [Char] -> Char
saferHead [] = ' '
saferHead (x : _) = x

eftBool :: Bool -> Bool -> [Bool]
eftBool x y = go x y []
 where go xs ys listout
        | xs < ys  = go (succ xs) y (xs : listout)
        | xs == ys = reverse (ys : listout)
        | xs > ys  = listout
        | otherwise = error "How the hell did this happen?!"

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = go x y []
 where go xs ys listout
        | xs < ys  = go (succ xs) y (xs : listout)
        | xs == ys = reverse (y : listout)
        | xs > ys  = listout
        | otherwise = error "How the hell did this happen?!"

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
 where go xs ys listout
        | xs < ys  = go (succ xs) y (xs : listout)
        | xs == ys = reverse (y : listout)
        | xs > ys  = listout
        | otherwise = error "How the hell did this happen?!"

eftChar :: Char -> Char -> String
eftChar x y = go x y []
 where go xs ys listout
        | xs < ys  = go (succ xs) y (xs : listout)
        | xs == ys = reverse (y : listout)
        | xs > ys  = listout
        | otherwise = error "How the hell did this happen?!"

-- Answer to random question in 9.6: takeWhile (=='a') "abracadabra" only returns a single a because the first argument is a, and thus is returned, while the second is not and thus fails the condition, stopping the function.


sepWrds :: [Char] -> [[Char]]
sepWrds x = go x []
 where go xs listout
        | x == [] = []
        -- Even if the above did compile, it would be an infinite loop (the first character of the new string would be a space, forcing the first condition again ad inf.)
        -- Above possibly resolved.
        | length xs <= 0 = reverse listout
        | (length . filter (== ' ') $ xs) == 0 = go (dropWhile (/= ' ') xs) ((takeWhile (/= ' ') xs) : listout)
        | length xs > 0  = go (tail (dropWhile (/= ' ') xs)) ((takeWhile (/= ' ') xs) : listout)
        | otherwise      = error "How the hell did this happen?!"



firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

endOfTheLine :: String -> [String]
endOfTheLine y = go y []
 where go xs listout
        | y == [] = []
        | length xs <= 0 = reverse listout
        | (length . filter (== '\n') $ xs) == 0 = go (dropWhile (/= '\n') xs) ((takeWhile (/= '\n') xs) : listout)
        | length xs > 0  = go (tail (dropWhile (/= '\n') xs)) ((takeWhile (/= '\n') xs) : listout)
        | otherwise      = error "How the hell did this happen?!"

endOfTheLine2 :: String -> [String]
endOfTheLine2 y = go y []
 where go xs listout
        | y == [] = []
        | length xs <= 0 = reverse listout
        | (length [c | c <- xs, c == '\n']) == 0 = go (dropWhile (/= '\n') xs) ((takeWhile (/= '\n') xs) : listout)
        | length xs > 0  = go (tail (dropWhile (/= '\n') xs)) ((takeWhile (/= '\n') xs) : listout)
        | otherwise      = error "How the hell did this happen?!"

shouldEqual =[ "Tyger Tyger, burning bright", "In the forests of the night" , "What immortal hand or eye", "Could frame thy fearful symmetry?" ]

check :: IO ()
check = do
 print $ "Are they equal? " ++ show (endOfTheLine sentences == shouldEqual)


main :: IO ()
main = do
 putStrLn "Exercises: Comprehend thy lists"
 putStrLn "I predict that the first list comp will return all the even values from mySqr -- Correct!"
 putStrLn "I predict that the second list comp will return a list of tuples where the first half of the tuples are either 1,4,9,16,25,36 or 49, and the second half is always either 64,81, or 100. -- Correct!"
 putStrLn "The output of the third function shall be (1,64),(1,81),(1,100),(4,64),(4,81) -- Correct!"
 putStrLn "Bonus at the beginning before the actual qs: it would return all the vowels in a string"
 putStrLn "Exercises: Bottom Madness"
 putStrLn " 1. Boom  | Correct!"
 putStrLn " 2. Nope  | Correct!"
 putStrLn " 3. Boom  | Correct!"
 putStrLn " 4. Nope  | Correct!"
 putStrLn " 5. Boom  | Correct!"
 putStrLn " 6. Nope  | Correct!"
 putStrLn " 7. Boom  | Correct!"
 putStrLn " 8. Nope  | Correct!"
 putStrLn " 9. Nope  | Correct!"
 putStrLn " 10. Bewm | Correct!"
 putStrLn "Intermission: ISITNORMAL"
 putStrLn "1. NF/WHNF (no type is defined, so this would be a whnf, but it seems like it's trying to be a question with nf as an answer so..."
 putStrLn "2. WHNF"
 putStrLn "3. Neither"
 putStrLn "4. Neither"
 putStrLn "5. Neither"
 putStrLn "6. Neither"
 putStrLn "7. WHNF"
 putStrLn "Exercises: More Bottoms"
 putStrLn " 1. The result will be bottom"
 putStrLn " 1. The result will be 1"
 putStrLn " 1. The result will be bottom"
 putStrLn " 1. It takes a string (sentence, alphanumeric character, word) and returns all of its lowercase vowels"
 putStrLn " 5. a) This returns the squares of every # from 1 to 10"
 putStrLn " 5. b) This returns a list of the lowest numbers in three different lists: 1, 10, and 20."
 putStrLn " 5. c) This returns a list of the number 15, three times. [15,15,15]"
 putStrLn " 6. The function: map (\\x -> bool x -3 (x == 3)) [1..10]"
 putStrLn " Exercises: Filtering"
 putStrLn " 1. *Blacklisted Data.Bool> mults3 x = [n | n <- x, rem n 3 == 0] -- *Blacklisted Data.Bool> mults3 [1..30] -- [3,6,9,12,15,18,21,24,27,30] -- this is how we might write a filter function"
 putStrLn " 2. length . mults3 $ [1..30]"
 putStrLn " 3. superiorFilter x = [w | w <- (words x), w //= 'a' && w //= 'and' && w //= 'an' && w //= 'the'] -- with all single quotes replaced by doubles"
 putStrLn "END OF CHAPTER EXERCISES: Data.Char"
 putStrLn " 1. Query'd"
 putStrLn " 2. allLowercaseMustDie l = [c | c <- l, isUpper c]"
 putStrLn " 3. capitalizeGoddamnit (t:ts)= (: ts) . toUpper $ t" 
 putStrLn " 4. vERYRUDE (t:ts) = toUpper t : vERYRUDE ts"
 putStrLn " 5. lonesomeLetter (d:ds) = toUpper $ d"
 putStrLn " 6. lonesomeLetter2elecbgloo (ks) = toUpper . head $ ks -- lonesomeLetter3TheRevenge = toUpper . head " 
 putStrLn "END OF CHAPTER EXERCISES: Cipher"

superiorFilter :: [Char] -> [[Char]]
superiorFilter x = [w | w <- (words x), w /= "a" && w /= "and" && w /= "an" && w /= "the"]

zippitydodah :: [a] -> [b] -> [(a,b)]
zippitydodah _ [] = []
zippitydodah [] _ = []
zippitydodah (x:xs) (y:ys) = (x,y) : zippitydodah xs ys

ziparallel :: (a -> b -> c) -> [a] -> [b] -> [c]
ziparallel  _ _ [] = []
ziparallel  _ [] _ = []
ziparallel f (p:ps) (z:zs) = f p z : ziparallel f ps zs

shameding :: [a] -> [b] -> [(a,b)]
shameding o i = ziparallel (,) o i

--Exercises: Comprehend thy lists
-- I predict that the first list comp will return all the even values from mySqr -- Correct!
-- I predict that the second list comp will return a list of tuples where the first half of the tuples are either 1,4,9,16,25,36 or 49, and the second half is always either 64,81, or 100. -- Correct!
-- The output of the third function shall be (1,64),(1,81),(1,100),(4,64),(4,81) -- Correct!

-- Exercises: Square Cube
-- Bonus at the beginning before the actual qs: it would return all the vowels in a string
-- 1. 




-- = True. VICTORY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

mySqr = [x^2 | x <- [1..5]]
myCbe = [y^3 | y <- [1..5]]

myTup = [(x,y) | x <- mySqr, y <- myCbe, x < 50, y < 50]
comprehendLength = length myTup
--foo1 :: a -> [a]
--foo2 :: [a] -> Int
--foo1 [] = []
--foo2 [] = []
--foo1 x = x : []
--foo2 y = length y

notLength :: [a] -> Int
notLength [] = 0
notLength (_:xs) = 1 + length xs

sum2bgloo :: Num a => [a] -> a
sum2bgloo [] = 0
sum2bgloo (x:xs) = x + sum2bgloo xs

testalpha :: IO ()
testalpha = do 
 putStrLn "interruptme"
 testalpha


robespierre x = "Guillotine all " ++ x
factions = ["Gidorans! ", "Montagnards! ", "Marshers! ", "Jacobins! ", "Royalists! ", "Englishmen! "]

bastille :: Int -> IO ()
bastille numb
 | numb <= 0 = putStrLn (concat (map robespierre factions))
 | otherwise = do
               putStrLn (concat (map robespierre factions))
               bastille (numb-1)
         
allLowercaseMustDie l = [c | c <- l, isUpper c]

capitalizeGoddamnit :: [Char] -> [Char]
capitalizeGoddamnit [] = []
capitalizeGoddamnit (t:ts)= (: ts) . toUpper $ t

vERYRUDE :: [Char] -> [Char]
vERYRUDE [] = "!"
vERYRUDE (t:ts) = toUpper t : vERYRUDE ts

vERYRUDE2 :: [Char] -> [Char]
vERYRUDE2 [] = ""
vERYRUDE2 (t:ts) = toUpper t : vERYRUDE ts

alllower :: [Char] -> [Char]
alllower [] = ""
alllower (r:rs) =  toLower r : alllower rs

nospecial :: [Char] -> [Char]
nospecial "" = ""
nospecial bs = [b | b <- bs, elem b ['a'..'z']]

lonesomeLetter :: [Char] -> Char
lonesomeLetter [] = ' '
lonesomeLetter (d:ds) = toUpper $ d

lonesomeLetter2elecbgloo :: [Char] -> Char
lonesomeLetter2elecbgloo [] = ' '
lonesomeLetter2elecbgloo (ks) = toUpper . head $ ks


lonesomeLetter3TheRevenge :: [Char] -> Char
lonesomeLetter3TheRevenge = toUpper . head

------------------------------------------------------------------------------------------------------------------------------------------------
--------------PNRFNE GVZR!!!!-------------(Caesar time!)------------------------------------------------------------------------------------------------------

---One function w/ guard cases
-- uses mod
-- char# + (mod var 26) for shift
-- uses custom alpha list to make things easier for mod

---------------- Functional but ugly (It looks like bloody LISP code! Those brackets!) -----------------------------

anyXinY :: Eq a => [a] -> [a] -> Bool
anyXinY [] []     = False
anyXinY [] _      = False
anyXinY _ []      = False
anyXinY (x:xs) ys = case (elem x ys) of
       True  -> True
       False -> (anyXinY xs ys)

allXinY :: Eq a => [a] -> [a] -> Bool
allXinY [] []     = False
allXinY [] _      = True
allXinY _ []      = False
allXinY (x:xs) ys = case (elem x ys) of
       True  -> (allXinY xs ys)
       False -> False

-- if any elements not in list X are in list Y
-- Filter all elements  of list X out of list Y, then test if length is > 0. If so, False.

onlyXinY :: Eq a => [a] -> [a] -> Bool
onlyXinY xs ys = if (length [y | y <- ys, not (elem y xs)]) > 0 then False else True


inputCaesar :: IO [Char]
inputCaesar = do
       putStr "Input the word or phrase you wish to cipherize: "
       w <- getLine
       putStr "Input the shift you want to use: "
       s <- getLine
       return (fortunaAudentes w s)


fortunaAudentes :: [Char] -> [Char] -> [Char]
fortunaAudentes msg sft  
                | msg == [] = []
                | sft == [] = fortunaAudentes msg "0"
                | not (onlyXinY "1234567890" sft) = error "Don't put LETTERS or SYMBOLS in your SHIFT, doofus!" 
                | length msg <= 0 = msg
                | length msg == 1 = (intAlpha (mod ((read sft :: Int) + (alphaInt (saferHead(nospecial (alllower msg))))) 26)) : fortunaAudentes ("") sft 
                | length msg > 0 = (intAlpha (mod ((read sft :: Int) + (alphaInt (head (nospecial(alllower msg))))) 26)) : fortunaAudentes (tail (nospecial (alllower msg))) sft 
                | otherwise = error "Something has gone catastrophically wrong. At least THIS ONE isn't infinite looping though..."






















doubleplusUnCaesar :: [Char] -> Int -> [Char]
doubleplusUnCaesar msg sft  
                | msg == [] = []
                | length msg <= 0 = msg
                | length msg == 1 = (intAlpha (mod ((alphaInt (saferHead(nospecial (alllower msg)))) - sft) 26)) : doubleplusUnCaesar ("") sft 
                | length msg > 0 = (intAlpha (mod ((alphaInt (head (nospecial(alllower msg))))- sft) 26)) : doubleplusUnCaesar (tail (nospecial (alllower msg))) sft 
                | otherwise = error "Something has gone catastrophically wrong. At least THIS ONE isn't infinite looping though..."









-- Change things below to uppercase and "toLower" to a toupper when time is available

---------------- Clean but broken -----------------------------

--fortunaAudentes :: [Char] -> Int -> [Char]
--fortunaAudentes m s = go m s
--                where go :: [Char] -> Int -> [Char]
--                      go msg sft 
--                       | msg == [] = []
--                       | length msg <= 0 = msg
--                       | length msg == 1 = (intAlpha (mod (sft + (alphaInt (toLower(head msg)))) 26)) : go ("") sft 
--                       | length msg > 0 = (intAlpha . mod 26 $ (sft + (alphaInt . head . alllower . concat . words $ msg))) : go (tail msg) sft 
--                       | otherwise = error "Something has gone catastrophically wrong. At least it isn't looping, though..."







intAlpha :: Int -> Char
intAlpha 0  = 'a'
intAlpha 1  = 'b'
intAlpha 2  = 'c'
intAlpha 3  = 'd'
intAlpha 4  = 'e'
intAlpha 5  = 'f'
intAlpha 6  = 'g'
intAlpha 7  = 'h'
intAlpha 8  = 'i'
intAlpha 9  = 'j'
intAlpha 10 = 'k'
intAlpha 11 = 'l'
intAlpha 12 = 'm'
intAlpha 13 = 'n'
intAlpha 14 = 'o'
intAlpha 15 = 'p'
intAlpha 16 = 'q'
intAlpha 17 = 'r'
intAlpha 18 = 's'
intAlpha 19 = 't'
intAlpha 20 = 'u'
intAlpha 21 = 'v'
intAlpha 22 = 'w'
intAlpha 23 = 'x'
intAlpha 24 = 'y'
intAlpha 25 = 'z'
intAlpha _  = error "intAlpha too high input"

alphaInt :: Char -> Int
alphaInt 'a' = 0
alphaInt 'b' = 1
alphaInt 'c' = 2
alphaInt 'd' = 3
alphaInt 'e' = 4
alphaInt 'f' = 5
alphaInt 'g' = 6
alphaInt 'h' = 7
alphaInt 'i' = 8
alphaInt 'j' = 9
alphaInt 'k' = 10
alphaInt 'l' = 11
alphaInt 'm' = 12
alphaInt 'n' = 13
alphaInt 'o' = 14
alphaInt 'p' = 15
alphaInt 'q' = 16
alphaInt 'r' = 17
alphaInt 's' = 18
alphaInt 't' = 19
alphaInt 'u' = 20
alphaInt 'v' = 21
alphaInt 'w' = 22
alphaInt 'x' = 23
alphaInt 'y' = 24
alphaInt 'z' = 25
alphaInt _   = error "How can you caesar cipher something with no letters?"

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (v:vs) = v && myAnd vs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (a:as) = a || myOr as

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (o:os) = f o || myAny f os


myElem :: Eq a => a -> [a] -> Bool
myElem val [] = False
myElem val (g:gs) = (val == g) || myElem val gs

--myElem3 :: Eq a => a -> [a] -> Bool
--myElem3 val xs = foldr (==) val xs

--myElem2 :: Eq a => a -> [a] -> Bool
--myElem2 val [] = False
--myElem2 val js = myAny ( val ==) js


--myRev :: [a] -> [a]
--myRev [] = []
--myRev us = (last us) : myRev (take ((length us) - 1) us)

--squish :: [[a]] -> [a]
--squish [[]]   = []
--squish (c:cs) = c ++ (squish cs)
--squish _      = []


--squishMap :: (a -> [b]) -> [a] -> [b]
--squishMap f [] = [] -- Try removing this after the function is written,  because it might still work
--squishMap f (h:hs) = f h ++ (squishMap f hs)

--madMaxBy :: (a -> a -> Ordering) -> [a] -> a
--madMaxBy _ [] = error "One cannot apply this to an empty list"
--madMaxBy f (xys) = go xys (f (head xys) (xys !! 1))
-- where go list comp 
      --  | length xys == 1 = head xys
      --  | comp == GT      = madMaxBy f (delEl 2 xys)
      --  | comp == EQ      = madMaxBy f (delEl 2 xys)
      --  | comp == LT      = madMaxBy f (delEl 1 xys)


--miniByme :: (a -> a -> Ordering) -> [a] -> a
--miniByme _ [] = error "One cannot apply this to an empty list"
--miniByme f (xys) = go xys (f (head xys) (xys !! 1))
-- where go list comp 
 --       | length xys == 1 = head xys
  --      | comp == LT      = miniByme f (delEl 2 xys)
   --     | comp == EQ      = miniByme f (delEl 2 xys)
   --     | comp == GT      = miniByme f (delEl 1 xys)


--paganMin :: Ord a => [a] -> a
--paganMin x = miniByme compare x

--maxPayne :: Ord a => [a] -> a
--maxPayne x = madMaxBy compare x
--madMaxBy :: (a -> a -> Ordering) -> [a] -> a
--madMaxBy _ [] = error "Empty List"
--madMaxBy f (xys) = if ((length xys) <= 1) then a else b
-- where a = 
--madMaxBy f (x:y:xys) =  if ((f x y) > LT) then (madMaxBy f (delEl 2 xys)) else (madMaxBy f (delEl 1 xys))


delEl :: Int -> [a] -> [a]
delEl _ [] = []
delEl n (bs) = let (xs,ys) = splitAt n bs in (init xs) ++ (ys)


---------------- Failed experiments; it is probably faster to start over with my original problem.
-- It was
 --aleaiacta :: [Char] -> Int -> [Char]
 --aleaiacta mm sss = go mm sss
   --              where go :: [Char] -> Int -> [Char]
     --                  go msg sft 
       --                 | msg == [] = []
      --     --             -- | fstTime == True = go (vERYRUDE2(concat (words msg))) sft False
      --                  | length msg <= 0 = msg
        --                | length msg == 1 = (chr (mod ((sft) + (ord (toUpper(head msg)))) 25)) : go ("") sft 
        --                | length msg > 0 = (chr ((mod sft 25) + (ord (head (vERYRUDE2(concat(words(msg)))))))) : go (tail msg) sft --False
        --                | otherwise = error "Something has gone catastrophically wrong. At least it isn't looping, though..."

 --g --reaterThan1In1Branch :: [Char] -> Int -> [Char]
 --greaterThan1In1Branch mm sss = go mm sss
     --            where go :: [Char] -> Int -> [Char]
    --                   go msg sft 
    --                    | msg == [] = []
                      -- | fstTime == True = go (vERYRUDE2(concat (words msg))) sft False
    --                    | length msg <= 0 = msg
     -- --                   | length msg == 1 = (chr ((mod sft 25) + (ord (head (vERYRUDE2(concat(words(msg)))))))) : go ("") sft 
      --                  | length msg > 0 = (chr ((mod sft 25) + (ord (head (vERYRUDE2(concat(words(msg)))))))) : go (tail msg) sft --False
      --                  | otherwise = error "Something has gone catastrophically wrong. At least it isn't looping, though..."

 --experimental2 :: [Char] -> Int -> [Char]
 --experimental2 mm sss = go mm sss
     --            where go :: [Char] -> Int -> [Char]
     -- --                  go msg sft 
          --              | msg == [] = []
       -- - ---                -- | fstTime == True = go (vERYRUDE2(concat (words msg))) sft False
            --            | length msg <= 0 = msg
         ----               | length msg == 1 = (chr (mod ((sft) + (ord (toUpper(head msg)))) 25)) : go ("") sft 
              --          | length msg > 0  = (chr (mod ((mod sft 25) + (ord (head (vERYRUDE2(concat(words(msg)))))))) 25) : go (tail msg) sft --False
               --         | otherwise = error "Something has gone catastrophically wrong. At least it isn't looping, though..."

 --equalTo1InGt1Branch :: [Char] -> Int -> [Char]
 --aleaiacta mm sss = go mm sss
     --            where go :: [Char] -> Int -> [Char]
       --                go msg sft 
        --                | msg == [] = []
                      -- | fstTime == True = go (vERYRUDE2(concat (words msg))) sft False
           --             | length msg <= 0 = msg
           --             | length msg == 1 = (chr (mod ((sft) + (ord (toUpper(head msg)))) 25)) : go ("") sft 
           --             | length msg > 0  = (chr (mod ((sft) + (ord (head (vERYRUDE2(concat(words(msg))))))) 25)) : go (tail msg) sft --False
                      -- | otherwise = error "Something has gone catastrophically wrong. At least it isn't looping, though..."
 --
-- W is causing problems if shift > 4, along with at least one other letter, V.
-- One of the issues is that the addition of (ord (toUpper(head msg))) can push certain letters past 'z'



msgg = "Attack at dawn"
sftt = 13
 --