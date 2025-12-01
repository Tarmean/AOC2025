module Day1 (main) where

parseLine :: String -> Int
parseLine ('L':n) = -(read n)
parseLine ('R':n) = read n

part1 :: [ Int ] -> [Int]
part1 = scanl (\x y -> (x + y) `mod` 100) 50

part2 :: [Int] -> [Int]
part2 ls = zipWith step (part1 ls) ls
  where
    step acc cur
      | cur > 0 = (acc + cur) `div` 100
      | cur < 0 = ((100-acc)`mod`100 - cur) `div` 100

main :: IO ()
main = do
    inputs <- readFile "inputs/Day01.txt"
    let lin = map parseLine $ lines inputs
    print $ length $ filter (==0) $ part1 $ lin
    print $ sum $ part2 lin

