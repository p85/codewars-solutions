module DigitalRoot where

  digits :: Integer -> [Integer]
  digits = map (`mod` 10) . reverse . takeWhile (> 0) . iterate (`div` 10)
  
  digitalRoot :: Integer -> Integer
  digitalRoot nr = recursive nr
  
  recursive :: Integer -> Integer
  recursive n = do
    let total = sum (digits n)
    if total > 9
      then do
        let a = foldl (+) 0 (digits n)
        recursive a
      else do
        total
  
