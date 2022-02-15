module Hangman where
import Control.Monad (forever) -- [1] 
import Data.Char (toLower) -- [2]
import Data.Maybe (isJust) -- [3]
import Data.List (intersperse) -- [4] 
import System.Exit (exitSuccess) -- [5] 
import System.IO (BufferMode(NoBuffering),hSetBuffering,stdout) -- [6]
import System.Random (randomRIO) -- [7]

data Puzzle = 
  Puzzle String [Maybe Char] [Char]
--          1       2          3
-- 1 = the puzzle word
-- 2 = the list of correctly gussed letters, and the non-gussed ones which are represented by underscores
-- 3 = the list of letters that have been guessed, correct or not
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
     ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle w = (Puzzle w (map charToNothing w) "")

charToNothing :: Char -> Maybe Char
charToNothing _ = Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle a _ _) b = elem b a

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ a) b = elem b a

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a


---

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word 
                 filledInSoFar s) c = 
--
  Puzzle word newFilledInSoFar (c:s)
  where zipper guessed wordChar guessChar = 
          if wordChar == guessed 
          then Just wordChar 
          else guessChar
        newFilledInSoFar = 
          zipWith (zipper c) 
            word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do

  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
    (_,True) -> do
      putStrLn "You ALREADY GUESSED that character, you idiotic meatbag -- pick something else!"
      return puzzle

    (True, _) -> do
      putStrLn "You guessed CORRECTLY, meatbag, congrats. The word is now being filled in:"
      return (fillInCharacter puzzle guess)

    (False,_) -> do
      putStrLn "WRONG! Stupid meatbag."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if ((length guessed) > ((length wordToGuess) + 5)) then
    do putStrLn "Foolish organic, YOU HAVE BEEN VANQUISHED!"
       putStrLn ("The word was " ++ wordToGuess ++ " you doofus!")
       exitSuccess
    else return()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn ("Fine, meatbag, you win this one. The word was " ++ wordToGuess ++ ". Congratulations\"")
       exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Meatbag, there must be a bug in your code. Input ONE character, no more, no less!"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 3 

maxWordLength :: Int
maxWordLength = 15

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
           in l >= minWordLength 
           && l < maxWordLength

lastTwoLy :: String -> Bool
lastTwoLy xs = "ply" == (drop ((length xs) - 3) xs) || "ily" == (drop ((length xs) - 3) xs)

gameAdjs :: IO WordList
gameAdjs = do
  aw <- allWords
  return (filter lastTwoLy (filter gameLength aw))
  where gameLength w =
          let l = length (w :: String)
           in l >= minWordLength 
           && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0,((length wl)-1))
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- to modify so that only incorrect guesses are held against you, make a script that puts all the letters in a Puzzle's guesslist, which appear in the word, in their own list, and then returns the length of that list. That would then be slotted into the failure condition for success.