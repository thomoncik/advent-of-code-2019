module Main (main) where
import Data.List as List

input :: [Int]
input = [271973..785961]

digits :: Int -> [Int]
digits = map (read . (:[])) . show

isDouble :: [Int] -> Bool
isDouble [] = False
isDouble (a:[]) = False
isDouble (a:b:xs) = if (a == b) then True else isDouble (b:xs)

noDec :: [Int] -> Bool
noDec (a:[]) = True
noDec [] = True
noDec (x1:x2:xs) = if (x1 > x2) then False else noDec (x2:xs)

isOk :: Int -> Bool
isOk x = isDouble (digits x) && noDec (digits x)

part1 :: Int
part1 = length (filter isOk input)

isDouble2 :: [Int] -> Bool
isDouble2 l = any (==2) (map length (group l))

isOk2 :: Int -> Bool
isOk2 x = isDouble2 (digits x) && noDec (digits x)

part2 :: Int
part2 = length $ filter isOk2 input

main :: IO ()
main = do
  print $ part1
  print $ part2
