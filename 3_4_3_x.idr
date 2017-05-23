import Data.Vect

infixr 20 /+\

-- Here we'll take Rings to be Rings with Unity
interface Ring a where
  add : a -> a -> a
  mult : a -> a -> a
  zero : a

interface Ring a => Ringu a where
  unit : a

interface Ringu a => Field a where
  inverse : a -> a

interface VectorSpace a where
  (/+\) : a -> a -> a
  inverse : a -> a
  zero : a

Ring Int where
  add x y = x + y
  mult x y = x * y
  zero = 0

Ringu Int where
  unit = 1

Ring Integer where
  add x y = x + y
  mult x y = x * y
  zero = 0

Ringu Integer where
  unit = 1

Matrix : (n: Nat) -> (m : Nat) -> Type -> Type
Matrix n m a = Vect n (Vect m a)

SquareMatrix : (n: Nat) -> Type -> Type
SquareMatrix n = Matrix n n

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

implementation Field a => VectorSpace (Vect n a) where
  (/+\) = zipWith add
  inverse = map inverse
  zero {n} = repeat n zero

--diagHelper n n x = []
--diagHelper n k x = delta n k x :: diagHelper n (S k) x

--total diag: (n: Nat) -> Matrix n n Int
--diag n x = diagHelper n n x

Ring a => Ring (SquareMatrix n a) where
  add = zipWith (zipWith add)
  mult = ?mysteryResult
  zero {n} = repeat n (repeat n zero)

Ringu a => Ringu (SquareMatrix n a) where
  unit {n} = ?mysteryDiagonal

