module Main (main) where

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Ord as Ord
import Data.Function as Function

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

type Memory = Map.Map Integer Integer
type Program = [Integer]

-- (cursor, input, output, memory, relativeOffset)
type State = (Maybe Integer, [Integer], [Integer], Memory, Integer)

input :: Program
input = [3,8,1005,8,324,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,29,2,1102,17,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,55,2,4,6,10,1,1006,10,10,1,6,14,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,89,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,110,1,104,8,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,137,2,9,17,10,2,1101,14,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,167,1,107,6,10,1,104,6,10,2,1106,6,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,1001,8,0,200,1006,0,52,1006,0,70,1006,0,52,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,232,1006,0,26,1,104,19,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,260,1,2,15,10,2,1102,14,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,290,1,108,11,10,1006,0,36,1006,0,90,1006,0,52,101,1,9,9,1007,9,940,10,1005,10,15,99,109,646,104,0,104,1,21101,0,666412360596,1,21101,341,0,0,1105,1,445,21101,838366659476,0,1,21102,1,352,0,1106,0,445,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,97713695975,1,21102,1,399,0,1106,0,445,21102,179469028392,1,1,21101,410,0,0,1105,1,445,3,10,104,0,104,0,3,10,104,0,104,0,21102,1,988220650260,1,21101,433,0,0,1105,1,445,21101,0,838345843560,1,21101,444,0,0,1106,0,445,99,109,2,22101,0,-1,1,21102,1,40,2,21102,1,476,3,21101,466,0,0,1106,0,509,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,471,472,487,4,0,1001,471,1,471,108,4,471,10,1006,10,503,1101,0,0,471,109,-2,2106,0,0,0,109,4,1202,-1,1,508,1207,-3,0,10,1006,10,526,21101,0,0,-3,22101,0,-3,1,22102,1,-2,2,21102,1,1,3,21101,0,545,0,1106,0,550,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,573,2207,-4,-2,10,1006,10,573,21201,-4,0,-4,1106,0,641,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,592,1,0,1106,0,550,21201,1,0,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,611,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,633,22102,1,-1,1,21102,1,633,0,106,0,508,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0]

loadProgramToMemory :: Program -> Memory
loadProgramToMemory input = Map.fromList (zip [0 ..] input)

digitFromRight :: Integer -> Integer -> Integer
digitFromRight n x = (x `div` (10 ^ n)) `mod` 10

parseToAction :: Integer -> Action
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

programStep :: State -> State
programStep (Just cursor, inputs, outputs, memory, relativeOffset)
  = afterAction action cursor memory outputs inputs
  where
    afterAction Halt cursor memory outputs inputs = 
      (Nothing, inputs, outputs, memory, relativeOffset)
    afterAction (Output mode1) cursor memory outputs inputs = 
      (Just (cursor + 2), inputs, (arg 1 mode1) : outputs, memory, relativeOffset)
    afterAction (Input mode1) cursor memory outputs inputs =
      (Just (cursor + 2), (tail inputs), outputs, (Map.insert (argWrite 1 mode1) (head inputs) memory), relativeOffset)
    afterAction (Add m1 m2 mode3) cursor memory outputs inputs =
      (Just (cursor + 4), inputs, outputs, (Map.insert (argWrite 3 mode3) ((arg 1 m1) + (arg 2 m2)) memory), relativeOffset)
    afterAction (Multiply mode1 mode2 mode3) cursor memory outputs inputs =
      (Just (cursor + 4), inputs, outputs, (Map.insert (argWrite 3 mode3) ((arg 1 mode1) * (arg 2 mode2)) memory), relativeOffset)
    afterAction (JumpTrue mode1 mode2) cursor memory outputs inputs
      | arg 1 mode1 /= 0 = (Just (arg 2 mode2), inputs, outputs, memory, relativeOffset)
      | otherwise        = (Just (cursor + 3), inputs, outputs, memory, relativeOffset)
    afterAction (JumpFalse mode1 mode2) cursor memory outputs inputs
      | (arg 1 mode1) == 0 = (Just (arg 2 mode2), inputs, outputs, memory, relativeOffset)
      | otherwise          = (Just (cursor + 3), inputs, outputs, memory, relativeOffset)
    afterAction (LessThan mode1 mode2 mode3) cursor memory outputs inputs
      | (arg 1 mode1) < (arg 2 mode2) = (Just (cursor + 4), inputs, outputs, (Map.insert (argWrite 3 mode3) 1 memory), relativeOffset)
      | otherwise                     = (Just (cursor + 4), inputs, outputs, (Map.insert (argWrite 3 mode3) 0 memory), relativeOffset)
    afterAction (Equals mode1 mode2 mode3) cursor memory outputs inputs
      | (arg 1 mode1) == (arg 2 mode2) = (Just (cursor + 4), inputs, outputs, (Map.insert (argWrite 3 mode3) 1 memory), relativeOffset)
      | otherwise                      = (Just (cursor + 4), inputs, outputs, (Map.insert (argWrite 3 mode3) 0 memory), relativeOffset)
    afterAction (AdjustRealtive mode1) cursor memory outputs inputs =
      (Just (cursor + 2), inputs, outputs, memory, relativeOffset + (arg 1 mode1))
    action = parseToAction (memory ! cursor)
    arg number mode
      | mode == Position  = Map.findWithDefault 0 (memory ! (cursor + number)) memory
      | mode == Immediate = memory ! (cursor + number)
      | mode == Relative  = Map.findWithDefault 0 ((memory ! (cursor + number)) + relativeOffset) memory
    argWrite number mode
      | mode == Position  = memory ! (cursor + number)
      | mode == Immediate = memory ! (cursor + number)
      | mode == Relative  = (memory ! (cursor + number)) + relativeOffset

runWhileNotHalted :: State -> State
runWhileNotHalted (Just cursor, inputs, outputs, memory, relativeOffset) 
  = runWhileNotHalted $ programStep (Just cursor, inputs, outputs, memory, relativeOffset)
runWhileNotHalted (Nothing, inputs, outputs, memory, relativeOffset) 
  = (Nothing, inputs, outputs, memory, relativeOffset)

programOutput :: Program -> [Integer] -> [Integer]
programOutput program inputs
  = reverse . (\(cursor, inputs, outputs, memory, relativeOffset) -> outputs) . runWhileNotHalted $
    (programStep (Just 0, inputs, [], (loadProgramToMemory program), 0))

----------------------------------------------------

data Direction = N | S | W | E deriving (Show, Eq)
data TurnDirection = Counterclockwise | Clockwise deriving (Show, Eq)
type Coordinate = (Integer, Integer)

turn :: TurnDirection -> Coordinate -> Direction -> (Coordinate, Direction)
turn Counterclockwise (x, y) N = ((x - 1, y), W)
turn Counterclockwise (x, y) W = ((x, y + 1), S)
turn Counterclockwise (x, y) S = ((x + 1, y), E)
turn Counterclockwise (x, y) E = ((x, y - 1), N)
turn Clockwise (x, y) N = ((x + 1, y), E)
turn Clockwise (x, y) E = ((x, y + 1), S)
turn Clockwise (x, y) S = ((x - 1, y), W)
turn Clockwise (x, y) W = ((x, y - 1), N)

runWhileUnlessHaltedOrOutput :: State -> State
runWhileUnlessHaltedOrOutput (Nothing, inputs, outputs, memory, relativeOffset) 
  = (Nothing, inputs, outputs, memory, relativeOffset)
runWhileUnlessHaltedOrOutput (Just cursor, inputs, outputs, memory, relativeOffset) 
  | parseToAction (memory ! cursor) == Output Position = programStep (Just cursor, inputs, outputs, memory, relativeOffset)
  | parseToAction (memory ! cursor) == Output Immediate = programStep (Just cursor, inputs, outputs, memory, relativeOffset)
  | parseToAction (memory ! cursor) == Output Relative = programStep (Just cursor, inputs, outputs, memory, relativeOffset)
  | otherwise =  runWhileUnlessHaltedOrOutput $ programStep (Just cursor, inputs, outputs, memory, relativeOffset)

runRobot :: (Map Coordinate Integer, Coordinate, Direction, State) -> (Map Coordinate Integer, Coordinate, Direction, State)
runRobot (trace, position, direction, (Just cursor, inputs, outputs, memory, relativeOffset))
  = let
      colourOfCurrent = Map.findWithDefault 0 position trace
      (_cursor', _inputs', stateOutput, _memory', _relativeOffset') = runWhileUnlessHaltedOrOutput (Just cursor, inputs ++ [colourOfCurrent], outputs, memory, relativeOffset)
      colour = if List.null stateOutput then colourOfCurrent else head stateOutput
      _stateOutput' = if List.null stateOutput then [] else tail stateOutput
      (cursor', inputs', stateOutput', memory', relativeOffset') = runWhileUnlessHaltedOrOutput (_cursor', _inputs', _stateOutput', _memory', _relativeOffset')
      turnDirection = if List.null stateOutput' then -1 else head stateOutput'
      outputs' = if List.null stateOutput' then [] else tail stateOutput'
      trace' = Map.insert position colour trace
      programState' = (cursor', inputs', outputs', memory', relativeOffset')
      (position', direction') = if turnDirection == -1 then (position, direction) else turn (if turnDirection == 0 then Counterclockwise else Clockwise) position direction
    in
      if (cursor' == Nothing) then (trace', position', direction', programState') else
      runRobot $ (trace', position', direction', programState')
runRobot (trace, position, direction, (Nothing, inputs, outputs, memory, relativeOffset)) 
  = (trace, position, direction, (Nothing, inputs, outputs, memory, relativeOffset))

part1 :: Int
part1 = Map.size ((\(a,b,c,d) -> a)(runRobot (Map.empty, (0,0), N, (Just 0, [], [], loadProgramToMemory input, 0))))


----------------------------------------------------
main :: IO ()
main = do
  print part1
  -- print part2
  let 
    m = ((\(a,b,c,d) -> a)(runRobot (Map.fromList [((0,0),1)], (0,0), N, (Just 0, [], [], loadProgramToMemory input, 0))))
    chars = Map.keys $ Map.filter (==1) m
    minX = fst $ minimumBy (comparing fst) chars
    minY = snd $ minimumBy (comparing snd) chars
    maxX = fst $ maximumBy (comparing fst) chars
    maxY = snd $ maximumBy (comparing snd) chars
    mm = transpose $ groupBy ((==) `on` snd) [(y,x) | x <- [minX -1 .. maxX+1], y <- [minY-1 .. maxY+1]]
    rows = List.map (List.map (\(y,x) -> if ((Map.findWithDefault 0 (x,y) m) == 0) then ' ' else '#')) $ mm
  mapM_ print rows
  