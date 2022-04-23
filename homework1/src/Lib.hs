module Lib
    ( someFunc
    ) where

x::Integer
x = 369894

--Integer to list
toDigits:: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = numToDigits (n `mod` 10) (n `div` 10) []
    where
      numToDigits a 0 l = (a:l)
      numToDigits a b l = numToDigits (b `mod` 10) (b `div` 10) (a:l)

--Reverse a list (not optimized)
reverseList:: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

--Integer to reversed list
toDigitsRev:: Integer -> [Integer]
toDigitsRev n = reverseList (toDigits n)

--skip first, double the second
doubleEveryOtherNoRev:: [Integer] -> [Integer]
doubleEveryOtherNoRev [] = []
doubleEveryOtherNoRev (x:[]) = [x]
doubleEveryOtherNoRev (x:y:[]) = (x:y*2:[])
doubleEveryOtherNoRev (x:y:xs) = (x:y*2:doubleEveryOtherNoRev xs)

--skip first, double the second starting from the end
doubleEveryOther:: Integer -> [Integer]
doubleEveryOther n = reverseList(doubleEveryOtherNoRev(toDigitsRev n))

--Sum first and second digit of a number
sumSingle:: Integer -> Integer
sumSingle x
  |x<= 0 = 0
  |x < 10 = x
  |otherwise = (x `div`10) + (x `mod` 10)

--Sum all digits of a list
sumDigits:: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumSingle x + sumDigits xs

--Check if it is a valid card
checkValidity:: Integer -> Bool
checkValidity n
  |sumDigits(doubleEveryOther n) `mod` 10 == 0 = True
  |otherwise = False

--print
someFunc :: IO ()
someFunc = putStrLn (show (checkValidity x))
