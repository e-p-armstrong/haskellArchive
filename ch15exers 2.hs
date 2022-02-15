module Ch15Exers where

import Data.Monoid
import Hangman

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada Nada = Nada
    (<>) (Only a) Nada = Only a
    (<>) Nada (Only a) = Only a
    (<>) (Only a) (Only b) = Only ((<>) a b)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

madLibBetter :: String -> String -> String -> String -> String -> String -> String
madLibBetter e f g adv noun adj = mconcat[e, " ", f, " ", g, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

randomWord'' :: IO String
randomWord'' = do
    a <- gameWords >>= randomWord
    b <- gameAdjs >>= randomWord
    c <- gameAdjs >>= randomWord
    d <- gameWords >>= randomWord
    e <- gameWords >>= randomWord
    f <- gameWords >>= randomWord
    return (madLibBetter a e f b c d)

