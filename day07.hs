module Main
  ( main
  ) where

import Data.List as List
import Data.Map as Map

data Mode
  = Position
  | Immediate
  deriving (Show, Eq)

data Action
  = JumpTrue Mode Mode
  | JumpFalse Mode Mode
  | LessThan Mode Mode
  | Equals Mode Mode
  | Halt
  | Multiply Mode Mode
  | Add Mode Mode
  | Input
  | Output
  deriving (Show, Eq)

type Memory = Map.Map Int Int

type Program = [Int]

type State = (Maybe Int, [Int], [Int], Memory)

input :: Program
input = [3,8,1001,8,10,8,105,1,0,0,21,38,55,68,93,118,199,280,361,442,99999,3,9,1002,9,2,9,101,5,9,9,102,4,9,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,101,4,9,9,102,3,9,9,4,9,99,3,9,102,2,9,9,101,4,9,9,102,2,9,9,1001,9,4,9,102,4,9,9,4,9,99,3,9,1002,9,2,9,1001,9,2,9,1002,9,5,9,1001,9,2,9,1002,9,4,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99]

loadProgramToMemory :: Program -> Memory
loadProgramToMemory input = Map.fromList (zip [0 ..] input)

digitFromRight :: Int -> Int -> Int
digitFromRight n x = (x `div` (10 ^ n)) `mod` 10

parseToAction :: Int -> Action
parseToAction x = getAction $ getMode
  where
    getMode
      | b == 0 && c == 0 = (Immediate, Immediate)
      | b == 0 && c == 1 = (Immediate, Position)
      | b == 1 && c == 0 = (Position, Immediate)
      | b == 1 && c == 1 = (Position, Position)
    getAction (mode1, mode2)
      | d == 9 && e == 9 = Halt
      | d == 0 && e == 8 = Equals mode1 mode2
      | d == 0 && e == 7 = LessThan mode1 mode2
      | d == 0 && e == 6 = JumpFalse mode1 mode2
      | d == 0 && e == 5 = JumpTrue mode1 mode2
      | d == 0 && e == 4 = Output
      | d == 0 && e == 3 = Input
      | d == 0 && e == 2 = Multiply mode1 mode2
      | d == 0 && e == 1 = Add mode1 mode2
    a = digitFromRight 4 x
    b = digitFromRight 3 x
    c = digitFromRight 2 x
    d = digitFromRight 1 x
    e = digitFromRight 0 x

programStep :: State -> State
programStep (Just cursor, pins, diag, l) = afterAction action cursor l diag pins
  where
    afterAction Halt cursor memory outputs inputs =
      (Nothing, inputs, outputs, memory)
    afterAction Output cursor memory outputs inputs =
      (Just (cursor + 2), inputs, (arg 1 Position) : outputs, memory)
    afterAction Input cursor memory diag pins =
      ( Just (cursor + 2)
      , (tail pins)
      , diag
      , (Map.insert (arg 1 Immediate) (head pins) memory))
    afterAction (Add m1 m2) cursor l diag pins =
      ( Just (cursor + 4)
      , pins
      , diag
      , (Map.insert (arg 3 Immediate) ((arg 1 m1) + (arg 2 m2)) l))
    afterAction (Multiply mode1 mode2) cursor l diag pins =
      ( Just (cursor + 4)
      , pins
      , diag
      , (Map.insert (arg 3 Immediate) ((arg 1 mode1) * (arg 2 mode2)) l))
    afterAction (JumpTrue mode1 mode2) cursor l diag pins
      | arg 1 mode1 /= 0 = (Just (arg 2 mode2), pins, diag, l)
      | otherwise = (Just (cursor + 3), pins, diag, l)
    afterAction (JumpFalse mode1 mode2) cursor l diag pins
      | (arg 1 mode1) == 0 = (Just (arg 2 mode2), pins, diag, l)
      | otherwise = (Just (cursor + 3), pins, diag, l)
    afterAction (LessThan mode1 mode2) cursor l diag pins
      | (arg 1 mode1) < (arg 2 mode2) =
        (Just (cursor + 4), pins, diag, (Map.insert (arg 3 Immediate) 1 l))
      | otherwise =
        (Just (cursor + 4), pins, diag, (Map.insert (arg 3 Immediate) 0 l))
    afterAction (Equals mode1 mode2) cursor l diag pins
      | (arg 1 mode1) == (arg 2 mode2) =
        (Just (cursor + 4), pins, diag, (Map.insert (arg 3 Immediate) 1 l))
      | otherwise =
        (Just (cursor + 4), pins, diag, (Map.insert (arg 3 Immediate) 0 l))
    action = parseToAction (l ! cursor)
    arg number mode
      | mode == Position = l ! (l ! (cursor + number))
      | otherwise = l ! (cursor + number)

runWhileNotHalted :: State -> State
runWhileNotHalted (Nothing, inputs, outputs, memory) =
  (Nothing, inputs, outputs, memory)
runWhileNotHalted (Just cursor, inputs, outputs, memory) =
  runWhileNotHalted $ programStep (Just cursor, inputs, outputs, memory)

programOutput :: Program -> [Int] -> [Int]
programOutput program inputs =
  reverse $
  (\(a, b, c, d) -> c) $
  runWhileNotHalted $
  (programStep (Just 0, inputs, [], (loadProgramToMemory program)))

-----------------------------------------

signalAfterChain :: [Int] -> Program -> Int
signalAfterChain settings program = setCompontents settings
  where
    setCompontents [a, b, c, d, e] =
      let aResult = programOutput program [a, 0]
          bResult = programOutput program (b : aResult)
          cResult = programOutput program (c : bResult)
          dResult = programOutput program (d : cResult)
          eResult = programOutput program (e : dResult)
       in last eResult

part1 = maximum (List.map (\p -> signalAfterChain p input) (permutations [0 .. 4]))

--------------------------

s2 :: [Int] -> [Int] -> Int
s2 sett prog = calculate sett
  where
    calculate [a, b, c, d, e] =
      let oa = programOutput prog (a : 0 : oe)
          ob = programOutput prog (b : oa)
          oc = programOutput prog (c : ob)
          od = programOutput prog (d : oc)
          oe = programOutput prog (e : od)
       in last oe

part2 :: Int
part2 = maximum (List.map (\p -> s2 p input) (permutations [5 .. 9]))

---------------------------

main :: IO ()
main = do
  print part1
  print part2
