module Truthy

data AlterBool = Truthy | Falsy

toTrueBool : AlterBool -> Bool
toTrueBool Falsy = False
toTrueBool Truthy = True

interface Truthable a where
    isTruthy: a -> AlterBool
    truthyness: a -> Bool
    truthyness = toTrueBool . isTruthy

Truthable AlterBool where
  isTruthy = id

Truthable Integer where
  isTruthy 0 = Falsy
  isTruthy x = Truthy

Truthable Nat where
  isTruthy x = Truthy

Truthable String where
  isTruthy "" = Falsy
  isTruthy x = Truthy

Truthable Type where
  isTruthy typ = Truthy
