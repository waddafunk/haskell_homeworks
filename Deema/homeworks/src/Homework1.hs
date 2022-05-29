module Homework1 where

-- exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = mod x 10 : toDigitsRev (div x 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (zipWith (*) xs_r ys)
  where
    ys = take (length xs) (cycle [1, 2])
    xs_r = reverse xs

-- exercise 3
sumDigit :: Integer -> Integer
sumDigit x
  | x <= 0 = 0
  | x < 10 = x
  | otherwise = mod x 10 + sumDigit (div x 10)

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sumDigit) 0

-- exercise 4
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

-- exercise 5
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 2 a b c = [(a, b), (a, c), (b, c)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, c)] ++ hanoi (n - 1) b a c
