
-- 3.2.4.1
my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

-- 3.2.4.2
my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = (reverse xs) ++ [x]

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = (f x) :: map f xs


