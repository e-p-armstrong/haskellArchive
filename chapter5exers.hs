module CH5Answers where


answers1AreThus :: IO ()
answers1AreThus = do
 putStrLn "For the first batch of exercises..."
 putStrLn "a) = b) && b) = d) && c) = b) && d) = a) && e) = e)"


answers2AreThus :: IO ()
answers2AreThus = do
 putStrLn "1. The type signature may change to become [Char] -> [Char]"
 -- Right!
 putStrLn "2. The type signature may change to become Fractional a => a -> a"
 -- Right!
 putStrLn "3. The type signature may change to become Int -> [Char]"
 -- Right!
 putStrLn "4. The type signature may change to become Int -> Bool"
 -- Right!
 putStrLn "5. The type signature may change to become Char -> Bool"
 -- Right!
 

answersFinalAreThus :: IO ()
answersFinalAreThus = do
 putStrLn "Part 1: Multiple Choice."
 putStrLn "1. c)"
 putStrLn "2. a)"
 putStrLn "3. b)"
 putStrLn "4. c)"
 putStrLn "Part 2: Determine the type"
 putStrLn "1. a) 54 :: Num a => a"
 putStrLn "b) (0, 'doge') :: Num a => (a, [Char])"
 putStrLn "c) (0, 'doge') :: (Integer, [Char])"
 putStrLn "d) (0, 'doge') :: num a => (a, [Char])"
 putStrLn "e) 5 :: Int"
 putStrLn "f) False :: Bool"
 putStrLn "2. Num a => a"
 putStrLn "3. Num a => a -> a"
 putStrLn "4. Fractional a => a"
 -- Right!
 -- 93% certain that everything above is correct
 putStrLn "5. [Char]"
 -- Right!
 putStrLn "Part 3: Does it Compile?"
 putStrLn "1. Does not compile. Fix successful, as follows: bigNum x = (^10) $ (^) x 5"
 putStrLn "2. Compiles. Hurrah!"
 putStrLn "3. Does not compile -- what the hell were they thinking? Applying a number to an argument?! Fix: a = (+) b = 5 c = a 10 d = c 200"
 -- Right!
 putStrLn "4. Does not compile, and it is really unclear what they want. Here's a fix that adds 33% more variables:     > a = 12 + b    > b = 1000 * c    > c = 42"
 -- Right!
 putStrLn "Part 4: Type Variable or Specific Type Constructor?"
 putStrLn "1 (1). component 1 is a fully poly var, 2 is a type constructor, and 3 is as well"
 putStrLn "2 (3). 1 is a fully poly type var, 2 is a constrained poly type var, and 3 is a (poorly named) type constructor "
 putStrLn "3 (4). 1 is a fully poly type var, 2 is a fully poly type var, and 3 is a concrete type constructor"
 putStrLn "Part 5: Write a type signature"
 putStrLn "1. :: [a] -> a"
 putStrLn "2. :: Ord => a -> a -> Bool"
  -- Shise!!!!!!
 putStrLn "3. :: (a, b) -> b"
  -- Right!
 putStrLn "Part 6: Given a type, write the function"
 putStrLn "0. This is madness, utter madness! THIS IS HASKELLLLLLL! lunacy xy yz peh (a,x) = (a, (yz (xy x))) I did it hurrah"
 putStrLn "1. The most obvious id function ever. it even begins with 'i'. x = x"
 putStrLn "2. \\x -> \\y -> x"
 putStrLn "3. x y = x. Same."
 putStrLn "4. x y = y"
 putStrLn "5. reverse a"
 putStrLn "6. bc ab a = bc (ab a)"
 putStrLn "7. worthless a = a"
 putStrLn "8. cg c = cg c"
 putStrLn "*Curses vehemently*"
 putStrLn "Part 7: Fix it (AS IF I HADN'T BEEN DOING THAT THROUGHOUT THIS WHOLE-----"
 putStrLn "Did they seriously just use an operator in the type signature? Did they seriously not capitalize the module name? Mein gott. Caps the S in mod name and replace the concatination operator with a ->. Holy crap this garbage is broken beyond all belief. Change the 'or' to else, fix the problem where both vars are 'x' in the 'where' area, give sing a type sig, indent the where so taht it matches sing. Add []s around the second Char in sndString's type sign. To make it play the second song, change the string bound to x in the if statement to 'a' or something else less than o. They won't see 'a in the rain' anyway if it doesn't play!"
 putStrLn "And now for Type Kwon Do!"
 putStrLn "(code on, code off!)"
 putStrLn "1. \\x -> g (f x)"
-- 
lunacy :: (x -> y) -> (y -> z) -> c -> (a,x) -> (a,z)
lunacy xy yz peh (a,x) = (a, (yz (xy x)))

totallynotid :: a -> a
totallynotid x = x

itslikefirstbutnotheretical :: a -> b -> a
itslikefirstbutnotheretical = \x -> \y -> x

--right

fst2ElecBoogaloo :: a -> b -> a
fst2ElecBoogaloo = \x -> \y -> x
-- Righto!

comeOnThisIsEasyGoddamnIt :: f -> u -> u
comeOnThisIsEasyGoddamnIt = \f -> \u -> u

creativity :: [a] -> [a]
creativity a = reverse a

-- Right, and id functions for lists only would be boring.

wellDarn :: (b -> c) -> (a -> b) -> a -> c
wellDarn bc ab a = bc (ab a)

-- "Correctamundo"

butWaitTheresMore :: (a -> c) -> a -> a 
butWaitTheresMore worthless a = a

expletives :: (crap -> goddamn) -> crap -> goddamn
expletives cg c = cg c

----- This worked!!!!! (One can check with :t)
---- Typekwondo 1 (all typekwondo answers are correct)

f :: Int -> String 
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = \x -> g (f x)

--------------------------------------- 2
data A 
data B 
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C 
e = \a -> w (q a)
--------------------------------------- 3
data X 
data Y 
data Z
xzc :: X -> Z 
xzc = undefined
yzc :: Y -> Z
yzc = undefined
xform :: (X, Y) -> (Z, Z)
xform = \(x,y) -> ((xzc x), (yzc y))
--------------------------------------- 4

oneFinalEffort :: (x -> y) -> (y -> (w,z)) -> x -> w 
oneFinalEffort = \xtoyasdf -> \ywzasdf -> \asdf -> fst(ywzasdf (xtoyasdf asdf))