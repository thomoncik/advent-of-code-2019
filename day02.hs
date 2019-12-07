module Main (main) where
import qualified Data.Map as Map

type Program = [Int]
type Memory = Map.Map Int Int

input :: Program
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,13,19,23,2,23,9,27,1,6,27,31,2,10,31,35,1,6,35,39,2,9,39,43,1,5,43,47,2,47,13,51,2,51,10,55,1,55,5,59,1,59,9,63,1,63,9,67,2,6,67,71,1,5,71,75,1,75,6,79,1,6,79,83,1,83,9,87,2,87,10,91,2,91,10,95,1,95,5,99,1,99,13,103,2,103,9,107,1,6,107,111,1,111,5,115,1,115,2,119,1,5,119,0,99,2,0,14,0]

loadProgramToMemory :: Program -> Memory
loadProgramToMemory input = Map.fromList (zip [0 ..] input)

f :: Int -> Memory -> Memory
f cursor l
  | action == 99 = l
  | action == 1  = f (cursor + 4) (Map.insert c (a + b) l)
  | action == 2  = f (cursor + 4) (Map.insert c (a * b) l)
  | otherwise    = f (cursor + 1) l
  where
    action = l Map.! cursor
    a      = l Map.! (l Map.! (cursor + 1))
    b      = l Map.! (l Map.! (cursor + 2))
    c      = l Map.! (cursor + 3)

part1 :: Program -> Int
part1 input = head $ map snd (Map.toList (f 0 (loadProgramToMemory input)))

setState :: Memory -> Int -> Int -> Memory
setState m a b = (Map.insert 2 b (Map.insert 1 a m))

f2 :: Memory -> (Int, Int) -> Bool
f2 l (n, v) = (f 0 (setState l n v)) Map.! 0 == 19690720

part2 :: [Int] -> Int
part2 l = head [100 * n + v | n <- [0 .. 99], v <- [0 .. 99], f2 (loadProgramToMemory l) (n, v)]

main :: IO ()
main = do 
  print $ part1 input
  print $ part2 input
