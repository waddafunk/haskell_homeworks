module Homework3 where

import Data.List

-- excercise 1
enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0 ..]

filterIndex :: (Integer -> Bool) -> [a] -> [a]
filterIndex cond xs = [x | (i, x) <- enumerate xs, cond i]

_skips :: Integer -> [a] -> [[a]]
_skips 0 _ = []
_skips n xs = _skips (n - 1) xs ++ [drop 1 (filterIndex ((== 0) . (`mod` n)) (head xs : xs))]

skips :: [a] -> [[a]]
skips [] = []
skips xs = _skips ((toInteger . length) xs) xs

-- excercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [a] = []
localMaxima [a, b] = []
localMaxima xs = [x | (i, x) <- enumerate xs, i `elem` localMaximaIdx]
  where
    flag1 = zipWith (<) xs (drop 1 xs)
    flag2 = zipWith (>) (drop 1 xs) (drop 2 xs)
    flag3 = zipWith (&&) flag1 flag2
    localMaximaIdx = [toInteger x + 1 | x <- elemIndices True flag3]

-- excercise 3
elemCount :: Eq a => a -> [a] -> Integer
elemCount e = toInteger . length . filter (e ==)

putAsterisk :: Integer -> String
putAsterisk n
  | n > 0 = "* "
  | otherwise = "  "

_histogram :: [Integer] -> String
_histogram xs
  | endRecursion xs = ""
  | otherwise = _histogram (map (subtract 1) xs) ++ "\n" ++ concatMap putAsterisk xs
  where
    endRecursion xs = not (any (> 0) xs)

histogram :: [Integer] -> String
histogram xs = _histogram counter ++ "\n" ++ footer
  where
    footer = "====================\n0 1 2 3 4 5 6 7 8 9\n"
    counter = [elemCount n xs | n <- [0 .. 9]]
