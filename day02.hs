module Main (main) where
import qualified Data.Map as Map

input :: [Int]
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,13,19,23,2,23,9,27,1,6,27,31,2,10,31,35,1,6,35,39,2,9,39,43,1,5,43,47,2,47,13,51,2,51,10,55,1,55,5,59,1,59,9,63,1,63,9,67,2,6,67,71,1,5,71,75,1,75,6,79,1,6,79,83,1,83,9,87,2,87,10,91,2,91,10,95,1,95,5,99,1,99,13,103,2,103,9,107,1,6,107,111,1,111,5,115,1,115,2,119,1,5,119,0,99,2,0,14,0]

replace [] _ _ = []
replace (_:xs) 0 a = a:xs
replace (x:xs) n a =
  if n < 0
    then (x:xs)
    else x: (replace xs (n-1) a)


f :: Int -> [Int] -> [Int]
f i l
  | x == 99 = l
  | x == 1 = f (i+4) (replace l c (a+b))
  | x == 2 = f (i+4) (replace l c (a*b))
  | otherwise = f (i+1) l
  where 
    x = l !! i
    a = l !! (l !! (i+1))
    b = l !! (l !! (i+2))
    c = l !! (i+3)

part1 :: [Int] -> [Int]
part1 input = f 0 input

setState :: [Int] -> Int -> Int -> [Int]
setState (x:_:_:xs) a b = x:a:b:xs

f2 :: [Int] -> (Int,Int) -> Bool
f2 l (n,v) = head (f 0 (setState l n v)) == 19690720

part2 :: [Int] -> Int
part2 l = head [100*n + v | n <- [0..99], v <- [0..99], f2 l (n,v)]

main :: IO ()
main = print $ part2 input
