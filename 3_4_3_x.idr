import Data.Vect

interface Ring a where
  add : a -> a -> a
  mult : a -> a -> a
  unit : a
  zero : a

Ring Int where
  add x y = x + y
  mult x y = x * y
  unit = 1
  zero = 0

Matrix : Ring a => (n: Nat) -> (m : Nat) -> Type -> Type
Matrix n m a = Vect n (Vect m a)

total repeat : (n: Nat) -> a -> Vect n a
repeat Z x = []
repeat (S k) x = x :: repeat k x

total delta : (n: Nat) -> (m: Nat) -> Int -> Vect n Int
delta Z _ _ = []
delta (S k) Z x = x :: repeat k 0
delta (S k) (S j) x = 0 :: delta k j x

total antiDiagHelper : (n: Nat) -> (m: Nat) -> Int -> Matrix m n Int
antiDiagHelper k Z _ = []
antiDiagHelper k (S j) x = row :: antiDiagHelper k j x
  where row = delta k j x

antidiagonal : (n: Nat) -> Int -> Matrix n n Int
antidiagonal a b = antiDiagHelper a a b

