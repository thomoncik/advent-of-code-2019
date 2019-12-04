module Main (main) where
import Data.List as List

input :: [Int]
input = [271973..785961]

digits :: Int -> [Int]
digits = map (read . (:[])) . show

noDec :: [Int] -> Bool
noDec l = sort l == l

isDouble :: [Int] -> Bool
isDouble l = any ((>=2) . length) (group l)

isOk :: Int -> Bool
isOk x = isDouble (digits x) && noDec (digits x)

part1 :: Int
part1 = length (filter isOk input)

isDouble2 :: [Int] -> Bool
isDouble2 = any ((2==) . length) . group

isOk2 :: Int -> Bool
isOk2 x = isDouble2 (digits x) && noDec (digits x)

part2 :: Int
part2 = length $ filter isOk2 input

main :: IO ()
main = do
  print $ part1
  print $ part2
