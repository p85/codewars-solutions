module Codewars.Kata.RemoveSmallest where
  import Data.List

  removeSmallest :: [Int] -> [Int]
  removeSmallest xs = delete (foldr min 9999 xs) xs