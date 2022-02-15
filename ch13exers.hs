module Ch13exers where

import Control.Monad (forever)
import Data.Char
import System.Exit (exitSuccess)


alllower :: [Char] -> [Char]
alllower [] = ""
alllower (r:rs) =  toLower r : alllower rs

nospecial :: [Char] -> [Char]
nospecial "" = ""
nospecial bs = [b | b <- bs, elem b ['a'..'z']]


palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case ((nospecial(alllower line1)) == reverse (nospecial(alllower line1))) of
        True -> putStrLn "Palindrome identified"

        False -> do
          putStrLn "Non-palendrome identified"
          exitSuccess

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid =
     NameEmpty
   | AgeTooLow
   | PersonInvalidUnknown String 
   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow | otherwise = 
        Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

givePerson :: IO ()
givePerson = do
    putStr "Please input this person's name: "
    n <- getLine
    putStr "And their age: "
    a <- getLine
    case (personValid (mkPerson n (read a))) of
        True -> do
            putStr "Yay! Successfully got a person: "
            putStrLn (show (unEitherRight(mkPerson n (read a))))
            return()
        False -> do
            putStrLn "Nooooo! Failed to get a person!"
            putStrLn (show (unEitherLeft(mkPerson n(read a))))
            return ()

personValid :: Either PersonInvalid Person -> Bool
personValid (Right (Person _ _)) = True
personValid (Left _) = False

unEitherRight :: Either a b -> b
unEitherRight (Right d) = d

unEitherLeft :: Either a b -> a
unEitherLeft (Left c) = c
