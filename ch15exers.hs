module Ch15Exers where

import Data.Monoid

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

madLibBetter :: String -> String -> String -> String -> String
madLibBetter e adv noun adj = mconcat[e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

