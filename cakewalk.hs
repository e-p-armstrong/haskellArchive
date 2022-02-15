module Dating where

data Trivial =
  Trivial'
instance Eq Trivial where
 Trivial' == Trivial' = False

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

data Date = 
 Date DayOfWeek Int deriving Show

instance Eq DayOfWeek where
 (==) Mon Mon      = True
 (==) Tue Tue      = True
 (==) Wed Wed      = True
 (==) Thu Thu      = True
 (==) Fri Fri      = True
 (==) Sat Sat      = True
 (==) Sun Sun      = True
 (==) _ _          = False


instance Eq Date where
 (Date weekday dayOfMonth) == (Date weekday' dayOfMonth') = weekday == weekday' && dayOfMonth == dayOfMonth'


data Identity a =
 Identity a

instance Eq a => Eq (Identity a) where
 (Identity v) == (Identity b) = v == b


data TisAnInteger = TisAn Integer 

instance Eq TisAnInteger where
 (TisAn number) == (TisAn number') = number == number'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
 (Two alpha beta) == (Two charlie zeta) = alpha == charlie && beta == zeta

data StringOrInt = 
 IsInt Int | IsString String

instance Eq StringOrInt where
 (IsInt first) == (IsInt second) = first == second
 (IsString first) == (IsString second) = first == second
 _ == _ = False

data Pair a =
 Pair a a

instance Eq a => Eq (Pair a) where
 (Pair a' b) == (Pair a'' b') = a' == a'' && b == b'

data Tuple a b =
 Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
 (Tuple alpha bravo) == (Tuple alpha' bravo') = alpha == alpha' && bravo == bravo'
 _ == _ = False

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
 ThisOne a == ThisOne b = a == b 
 ThatOne c == ThatOne d = c == d
 _ == _ = False


data EitherOr h g = 
 Hello h 
 | Goodbye g

instance (Eq h, Eq g) => Eq (EitherOr h g ) where
 Hello a == Hello b = a == b 
 Goodbye c == Goodbye d = c == d
 _ == _ = False


-- data Pair a =
 --Pair' Integer Integer | Pear String String | Pairr a a

--instance Eq a => Eq (Pair a) where
 --(Pair' a' b) == (Pair' a'' b') = a' == a'' && b == b'
-- _ == _ = False


