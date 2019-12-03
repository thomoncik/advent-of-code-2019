module Main (main) where
import qualified Data.Map as Map

type M = Map.Map Int Int

input :: [Int]
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,13,19,23,2,23,9,27,1,6,27,31,2,10,31,35,1,6,35,39,2,9,39,43,1,5,43,47,2,47,13,51,2,51,10,55,1,55,5,59,1,59,9,63,1,63,9,67,2,6,67,71,1,5,71,75,1,75,6,79,1,6,79,83,1,83,9,87,2,87,10,91,2,91,10,95,1,95,5,99,1,99,13,103,2,103,9,107,1,6,107,111,1,111,5,115,1,115,2,119,1,5,119,0,99,2,0,14,0]

listToMap :: [Int] -> M
listToMap input = (Map.fromList (zip [0..] input))

replace [] _ _ = []
replace (_:xs) 0 a = a:xs
replace (x:xs) n a =
  if n < 0
    then (x:xs)
    else x: (replace xs (n-1) a)


f :: Int -> M -> M
f i l
  | x == 99 = l
  | x == 1 = f (i+4) (Map.update (\y-> Just (a+b)) c l)
  | x == 2 = f (i+4) (Map.update (\y-> Just (a*b)) c l)
  | otherwise = f (i+1) l
  where 
    x = l Map.! i
    a = l Map.! (l Map.! (i+1))
    b = l Map.! (l Map.! (i+2))
    c = l Map.! (i+3)

part1 :: M -> M
part1 input = f 0 input

setState :: M -> Int -> Int -> M
setState m a b = (Map.update (\x -> Just b) 2 (Map.update (\y-> Just (a)) 1 m))

f2 :: M -> (Int,Int) -> Bool
f2 l (n,v) = (f 0 (setState l n v)) Map.! 0 == 19690720

part2 :: M -> Int
part2 l = head [100*n + v | n <- [0..99], v <- [0..99], f2 l (n,v)]

main :: IO ()
main = print $ part2 (listToMap input)
