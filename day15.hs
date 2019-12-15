module Main (main) where

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Ord as Ord
import Data.Function as Function
import Data.List.Split as Split

data Mode
  = Position
  | Immediate
  | Relative
  deriving (Show, Eq)

data Action
  = JumpTrue Mode Mode
  | JumpFalse Mode Mode
  | LessThan Mode Mode Mode
  | Equals Mode Mode Mode
  | Halt
  | Multiply Mode Mode Mode
  | Add Mode Mode Mode
  | Input Mode
  | Output Mode
  | AdjustRealtive Mode
  deriving (Show, Eq)

data Effect
  = Printed Int
  | Halted
  | InnerAction
  deriving (Show, Eq)

type Memory = Map.Map Int Int
type Program = [Int]

-- (cursor, input, output, memory, relativeOffset)
type State = (Maybe Int, Memory, Int)

input :: Program
input = [3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1002,1034,1,1039,102,1,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1106,0,124,1002,1034,1,1039,1001,1036,0,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,1001,1035,0,1040,1002,1038,1,1043,101,0,1037,1042,1105,1,124,1001,1034,1,1039,1008,1036,0,1041,102,1,1035,1040,1001,1038,0,1043,101,0,1037,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,1,1032,1006,1032,165,1008,1040,9,1032,1006,1032,165,1102,1,2,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,35,1044,1106,0,224,1101,0,0,1044,1105,1,224,1006,1044,247,102,1,1039,1034,1002,1040,1,1035,1002,1041,1,1036,102,1,1043,1038,101,0,1042,1037,4,1044,1105,1,0,1,5,41,19,22,1,39,81,29,20,15,82,33,18,45,30,32,55,28,26,70,13,56,32,28,18,3,59,90,11,95,15,85,8,61,25,59,24,34,1,85,5,25,54,57,18,20,54,80,91,28,65,36,12,44,36,13,92,24,56,13,39,69,29,79,10,41,27,23,25,72,20,3,61,15,51,11,12,12,48,10,45,13,29,49,90,30,17,9,41,21,18,7,30,48,17,83,71,4,10,31,10,96,81,77,9,50,39,21,36,33,72,12,3,23,79,18,4,75,17,58,64,8,7,97,60,72,72,1,94,55,42,2,94,2,21,88,19,82,57,96,19,25,27,41,62,15,40,23,61,86,27,73,61,13,46,52,81,12,34,23,73,23,59,1,30,47,9,99,10,37,17,28,98,5,92,73,8,63,4,86,76,79,7,30,68,28,91,12,12,98,74,4,22,44,10,23,45,37,16,90,76,23,74,75,12,21,38,14,15,76,28,49,71,7,6,6,71,53,33,12,87,15,92,66,21,38,13,53,92,34,49,25,6,67,21,27,89,24,61,25,30,41,30,99,28,19,41,90,51,74,14,33,54,48,10,14,42,2,67,76,10,21,2,67,43,27,69,11,16,78,7,36,9,24,48,63,81,53,29,94,34,25,99,66,47,17,97,33,52,11,62,22,52,30,23,89,95,15,13,50,48,26,10,6,69,78,13,6,94,1,28,67,10,70,16,50,19,24,15,79,50,27,3,19,62,4,31,83,20,17,83,67,5,80,26,36,62,87,3,10,80,22,65,60,10,78,4,20,60,30,11,7,83,10,13,72,81,37,22,14,55,63,51,27,32,77,52,20,50,16,48,2,55,10,53,26,84,6,87,43,37,26,3,85,62,25,78,50,16,10,37,22,54,5,80,24,7,32,49,18,27,12,41,70,82,20,34,91,15,98,77,22,6,79,3,8,54,17,32,4,44,2,97,14,15,65,30,97,14,79,75,11,77,5,61,37,20,91,20,45,74,19,40,2,41,89,12,34,44,18,62,57,17,68,22,96,7,59,63,2,60,70,2,26,75,26,3,53,19,80,16,97,7,34,58,52,66,24,75,25,30,75,42,13,12,89,13,3,84,92,1,75,30,54,43,2,56,15,1,15,84,99,6,98,42,17,29,1,18,26,70,71,29,91,23,21,87,66,18,38,32,18,81,65,2,58,99,12,4,84,24,32,88,30,67,49,29,59,64,18,70,10,24,56,5,27,97,50,4,28,85,65,16,67,83,15,16,61,18,86,8,36,25,36,29,97,45,19,81,41,29,45,30,69,26,57,93,27,72,34,30,99,61,2,48,16,12,76,98,28,14,32,32,90,48,10,30,57,23,39,2,8,39,33,13,88,34,31,74,15,60,8,47,60,31,5,79,1,98,86,33,3,99,33,62,11,96,25,22,38,98,84,3,56,70,49,3,8,56,87,4,29,59,65,26,34,77,7,14,78,26,25,70,49,3,31,45,92,24,95,17,4,9,4,96,64,92,27,67,4,99,6,44,7,16,86,2,75,1,6,68,81,4,1,44,49,7,92,8,40,36,25,81,13,56,99,10,2,30,72,6,43,30,12,43,93,19,20,23,95,10,19,66,63,28,96,40,50,8,15,56,38,13,93,42,71,12,18,87,8,4,21,85,9,2,66,77,10,80,26,61,9,43,20,88,10,39,67,55,31,49,17,58,26,80,20,84,54,49,5,73,11,52,15,63,7,62,24,57,92,61,25,87,56,37,31,38,14,99,0,0,21,21,1,10,1,0,0,0,0,0,0]

loadProgramToMemory :: Program -> Memory
loadProgramToMemory input = Map.fromList (zip [0 ..] input)

digitFromRight :: Int -> Int -> Int
digitFromRight n x = (x `div` (10 ^ n)) `mod` 10

parseToAction :: Int -> Action
parseToAction x = getAction $ getModes
  where
    getModes = (getMode c, getMode b, getMode a)
    getMode codedMode
      | codedMode == 0 = Position
      | codedMode == 1 = Immediate
      | codedMode == 2 = Relative
    getAction (mode1, mode2, mode3)
      | d == 9 && e == 9 = Halt
      | d == 0 && e == 9 = AdjustRealtive mode1
      | d == 0 && e == 8 = Equals mode1 mode2 mode3
      | d == 0 && e == 7 = LessThan mode1 mode2 mode3
      | d == 0 && e == 6 = JumpFalse mode1 mode2
      | d == 0 && e == 5 = JumpTrue mode1 mode2
      | d == 0 && e == 4 = Output mode1
      | d == 0 && e == 3 = Input mode1
      | d == 0 && e == 2 = Multiply mode1 mode2 mode3
      | d == 0 && e == 1 = Add mode1 mode2 mode3
    a = digitFromRight 4 x
    b = digitFromRight 3 x
    c = digitFromRight 2 x
    d = digitFromRight 1 x
    e = digitFromRight 0 x

programStep :: State -> Int -> (Effect, State)
programStep (Just cursor, memory, relativeOffset) input
  = afterAction action cursor memory input
  where
    afterAction Halt cursor memory input = 
      (Halted, (Nothing, memory, relativeOffset))
    afterAction (Output mode1) cursor memory input = 
      (Printed $ arg 1 mode1, (Just (cursor + 2), memory, relativeOffset))
    afterAction (Input mode1) cursor memory input =
      (InnerAction, (Just (cursor + 2), Map.insert (argWrite 1 mode1) input memory, relativeOffset))
    afterAction (Add m1 m2 mode3) cursor memory input =
      (InnerAction, (Just (cursor + 4), (Map.insert (argWrite 3 mode3) ((arg 1 m1) + (arg 2 m2)) memory), relativeOffset))
    afterAction (Multiply mode1 mode2 mode3) cursor memory input =
      (InnerAction, (Just (cursor + 4), (Map.insert (argWrite 3 mode3) ((arg 1 mode1) * (arg 2 mode2)) memory), relativeOffset))
    afterAction (JumpTrue mode1 mode2) cursor memory input
      | arg 1 mode1 /= 0 = (InnerAction, (Just (arg 2 mode2), memory, relativeOffset))
      | otherwise        = (InnerAction, (Just (cursor + 3), memory, relativeOffset))
    afterAction (JumpFalse mode1 mode2) cursor memory input
      | (arg 1 mode1) == 0 = (InnerAction, (Just (arg 2 mode2), memory, relativeOffset))
      | otherwise          = (InnerAction, (Just (cursor + 3), memory, relativeOffset))
    afterAction (LessThan mode1 mode2 mode3) cursor memory input
      | (arg 1 mode1) < (arg 2 mode2) = (InnerAction, (Just (cursor + 4), (Map.insert (argWrite 3 mode3) 1 memory), relativeOffset))
      | otherwise                     = (InnerAction, (Just (cursor + 4), (Map.insert (argWrite 3 mode3) 0 memory), relativeOffset))
    afterAction (Equals mode1 mode2 mode3) cursor memory input
      | (arg 1 mode1) == (arg 2 mode2) = (InnerAction, (Just (cursor + 4), (Map.insert (argWrite 3 mode3) 1 memory), relativeOffset))
      | otherwise                      = (InnerAction, (Just (cursor + 4), (Map.insert (argWrite 3 mode3) 0 memory), relativeOffset))
    afterAction (AdjustRealtive mode1) cursor memory input =
      (InnerAction, (Just (cursor + 2), memory, relativeOffset + (arg 1 mode1)))
    action = parseToAction (memory ! cursor)
    arg number mode
      | mode == Position  = Map.findWithDefault 0 (memory ! (cursor + number)) memory
      | mode == Immediate = memory ! (cursor + number)
      | mode == Relative  = Map.findWithDefault 0 ((memory ! (cursor + number)) + relativeOffset) memory
    argWrite number mode
      | mode == Position  = memory ! (cursor + number)
      | mode == Immediate = memory ! (cursor + number)
      | mode == Relative  = (memory ! (cursor + number)) + relativeOffset

run :: State -> Int -> (Effect, State)
run state input
  | (fst afterStep) == InnerAction = run (snd afterStep) input
  | otherwise = afterStep
  where afterStep = programStep state input

----------------------------------------------------

type Coordinate = (Int, Int)

f :: [(Int, Coordinate, Map Coordinate Int, State)] -> (Int, Coordinate, State)
f ((i, (x,y), world, (cursor, memory, ro)) : nodes)
  | world ! (x,y) == 2 = (i, (x,y), (cursor, memory, ro))
  | otherwise = f (nodes ++ toSearch)
  where
    (n, northState) = go $ run (cursor, memory, ro) 1
    (s, southState) = go $ run (cursor, memory, ro) 2
    (w, westState) = go $ run (cursor, memory, ro) 3
    (e, eastState) = go $ run (cursor, memory, ro) 4

    north = (i + 1, (x, y + 1), Map.insert (x, y + 1) n world, northState)
    south = (i + 1, (x, y - 1), Map.insert (x, y - 1) s world, southState)
    west = (i + 1, (x - 1, y), Map.insert (x - 1, y) w world, westState)
    east = (i + 1, (x + 1, y), Map.insert (x + 1, y) e world, eastState)

    go :: (Effect, State) -> (Int, State)
    go ((Printed x), state) = (x, state)
    go (Halted, _) = error "Should not halt here"

    toSearch = List.map snd $ 
      List.filter
        (\(t, (_,(a,b),m,_)) -> (t /= 0) && not ((a,b) `Map.member` world)) 
        [(w, west), (n, north), (e, east), (s, south)]

part1Helper = f [(0, (0,0), Map.fromList [((0,0), 1)], (Just 0, (loadProgramToMemory input), 0))]
part1 = (\(distance,_,_) -> distance) part1Helper

---------------------

g :: [(Int, Coordinate, Map Coordinate Int, State)] -> (Int, Coordinate, Map Coordinate Int)
g ((i, (x,y), world, (cursor, memory, ro)) : nodes)
  | nodes == [] && toSearch == [] = (i, (x,y), world)
  | otherwise = g (nodes ++ toSearch)
  where
    (n, northState) = go $ run (cursor, memory, ro) 1
    (s, southState) = go $ run (cursor, memory, ro) 2
    (w, westState) = go $ run (cursor, memory, ro) 3
    (e, eastState) = go $ run (cursor, memory, ro) 4

    north = (i + 1, (x, y + 1), Map.insert (x, y + 1) n world, northState)
    south = (i + 1, (x, y - 1), Map.insert (x, y - 1) s world, southState)
    west = (i + 1, (x - 1, y), Map.insert (x - 1, y) w world, westState)
    east = (i + 1, (x + 1, y), Map.insert (x + 1, y) e world, eastState)

    go :: (Effect, State) -> (Int, State)
    go ((Printed x), state) = (x, state)
    go (Halted, _) = error "Should not halt here"

    toSearch = List.map snd $ 
      List.filter
        (\(t, (_,(a,b),m,_)) -> (t /= 0) && not ((a,b) `Map.member` world)) 
        [(w, west), (n, north), (e, east), (s, south)]

(oxygenPosition, state) = (\(_,o,s) -> (o,s)) part1Helper
part2Helper = g [(0, oxygenPosition, Map.fromList [(oxygenPosition, 1)], state)]
part2 = (\(distance,_,_) -> distance) part2Helper


-------------------------
drawMap :: Map.Map Coordinate Int -> [[Char]]
drawMap m = 
  let 
    chars = Map.keys m
    minX = fst $ minimumBy (comparing fst) chars
    minY = snd $ minimumBy (comparing snd) chars
    maxX = fst $ maximumBy (comparing fst) chars
    maxY = snd $ maximumBy (comparing snd) chars
    mm = transpose $ groupBy ((==) `on` snd) [(y,x) | x <- [minX .. maxX], y <- [minY .. maxY]]
  in
    List.map (List.map (\(y,x) ->
      if (x,y) == oxygenPosition then 'O'
      else if (Map.findWithDefault 5 (x,y) m) == 1 then ' ' 
      else if (Map.findWithDefault 5 (x,y) m) == 2 then 'X' 
      else '#')) $ mm

-------------------------

main :: IO ()
main = do
  print $ part1
  print $ part2
  mapM_ print $ drawMap ((\(_,_,c) -> c) part2Helper)
  