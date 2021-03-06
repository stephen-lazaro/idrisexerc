import Data.Vect

-- 3.2.4.1
my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

-- 3.2.4.2
my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = (reverse xs) ++ [x]

-- 3.2.4.3
my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = (f x) :: map f xs

-- 3.2.4.4
my_mapV : (a -> b) -> (Vect n a) -> (Vect n b)
my_mapV f [] = []
my_mapV f (x :: xs) = (f x) :: my_mapV f xs
