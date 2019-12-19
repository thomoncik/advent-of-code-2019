module Main
  ( main
  ) where

import Data.Char as Char
import Data.Function as Function
import Data.List as List
import Data.List.Split as Split
import Data.Map as Map
import Data.Ord as Ord
import Data.Set as Set

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
  | InputGiven
  | InnerAction
  deriving (Show, Eq)

type Memory = Map.Map Int Int

type Program = [Int]

-- (cursor, input, output, memory, relativeOffset)
type State = (Maybe Int, Memory, Int)

input :: Program
input = [109,424,203,1,21101,11,0,0,1106,0,282,21101,18,0,0,1106,0,259,1202,1,1,221,203,1,21102,31,1,0,1105,1,282,21101,0,38,0,1106,0,259,21002,23,1,2,22102,1,1,3,21102,1,1,1,21102,57,1,0,1106,0,303,2102,1,1,222,21002,221,1,3,20102,1,221,2,21102,1,259,1,21102,1,80,0,1105,1,225,21102,105,1,2,21102,91,1,0,1105,1,303,1202,1,1,223,20102,1,222,4,21102,259,1,3,21101,225,0,2,21101,225,0,1,21102,118,1,0,1106,0,225,20101,0,222,3,21101,157,0,2,21102,133,1,0,1106,0,303,21202,1,-1,1,22001,223,1,1,21102,1,148,0,1105,1,259,2101,0,1,223,20101,0,221,4,20101,0,222,3,21102,21,1,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21102,195,1,0,105,1,108,20207,1,223,2,20101,0,23,1,21102,-1,1,3,21101,0,214,0,1106,0,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1201,-4,0,249,21202,-3,1,1,21202,-2,1,2,22102,1,-1,3,21101,0,250,0,1106,0,225,22101,0,1,-4,109,-5,2106,0,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22102,1,-2,-2,109,-3,2106,0,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,21201,-2,0,3,21101,0,343,0,1105,1,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,21201,-4,0,1,21102,384,1,0,1106,0,303,1106,0,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21201,1,0,-4,109,-5,2105,1,0]

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
programStep (Just cursor, memory, relativeOffset) input =
  afterAction action cursor memory input
  where
    afterAction Halt cursor memory input =
      (Halted, (Nothing, memory, relativeOffset))
    afterAction (Output mode1) cursor memory input =
      (Printed $ arg 1 mode1, (Just (cursor + 2), memory, relativeOffset))
    afterAction (Input mode1) cursor memory input =
      ( InputGiven
      , ( Just (cursor + 2)
        , Map.insert (argWrite 1 mode1) input memory
        , relativeOffset))
    afterAction (Add m1 m2 mode3) cursor memory input =
      ( InnerAction
      , ( Just (cursor + 4)
        , (Map.insert (argWrite 3 mode3) ((arg 1 m1) + (arg 2 m2)) memory)
        , relativeOffset))
    afterAction (Multiply mode1 mode2 mode3) cursor memory input =
      ( InnerAction
      , ( Just (cursor + 4)
        , (Map.insert (argWrite 3 mode3) ((arg 1 mode1) * (arg 2 mode2)) memory)
        , relativeOffset))
    afterAction (JumpTrue mode1 mode2) cursor memory input
      | arg 1 mode1 /= 0 =
        (InnerAction, (Just (arg 2 mode2), memory, relativeOffset))
      | otherwise = (InnerAction, (Just (cursor + 3), memory, relativeOffset))
    afterAction (JumpFalse mode1 mode2) cursor memory input
      | (arg 1 mode1) == 0 =
        (InnerAction, (Just (arg 2 mode2), memory, relativeOffset))
      | otherwise = (InnerAction, (Just (cursor + 3), memory, relativeOffset))
    afterAction (LessThan mode1 mode2 mode3) cursor memory input
      | (arg 1 mode1) < (arg 2 mode2) =
        ( InnerAction
        , ( Just (cursor + 4)
          , (Map.insert (argWrite 3 mode3) 1 memory)
          , relativeOffset))
      | otherwise =
        ( InnerAction
        , ( Just (cursor + 4)
          , (Map.insert (argWrite 3 mode3) 0 memory)
          , relativeOffset))
    afterAction (Equals mode1 mode2 mode3) cursor memory input
      | (arg 1 mode1) == (arg 2 mode2) =
        ( InnerAction
        , ( Just (cursor + 4)
          , (Map.insert (argWrite 3 mode3) 1 memory)
          , relativeOffset))
      | otherwise =
        ( InnerAction
        , ( Just (cursor + 4)
          , (Map.insert (argWrite 3 mode3) 0 memory)
          , relativeOffset))
    afterAction (AdjustRealtive mode1) cursor memory input =
      (InnerAction, (Just (cursor + 2), memory, relativeOffset + (arg 1 mode1)))
    action = parseToAction (memory ! cursor)
    arg number mode
      | mode == Position =
        Map.findWithDefault 0 (memory ! (cursor + number)) memory
      | mode == Immediate = memory ! (cursor + number)
      | mode == Relative =
        Map.findWithDefault
          0
          ((memory ! (cursor + number)) + relativeOffset)
          memory
    argWrite number mode
      | mode == Position = memory ! (cursor + number)
      | mode == Immediate = memory ! (cursor + number)
      | mode == Relative = (memory ! (cursor + number)) + relativeOffset

run :: State -> Int -> (Effect, State)
run state input
  | (fst afterStep) == InnerAction = run (snd afterStep) input
  | otherwise = afterStep
  where
    afterStep = programStep state input

----------------------------------------------------
f :: [Int] -> State -> Int
f [] st = foo (run st 0)
  where
    foo (Printed x, s) = x
    foo (InputGiven, s) = error "wtf"
    foo (Halted, _) = 0
f (i:is) st = foo (run st i)
  where
    foo (Printed x, s) = x
    foo (InputGiven, s) = f is s
    foo (Halted, _) = 0

---------------------------------------------
part1 =
  length . List.filter (== 1) $
  [ f [x, y] (Just 0, loadProgramToMemory input, 0)
  | x <- [0 .. 49]
  , y <- [0 .. 49]
  ]

s0 = (Just 0, loadProgramToMemory input, 0)
part2 =
  let foo x y
        | f [x, y] s0 == 0 = foo (x + 1) y
        | f [(x + 99), (y - 99)] s0 == 0 = foo x (y + 1)
        | otherwise = 10000 * x + (y - 99)
   in foo 0 9

main :: IO ()
main = do
  print $ part1
  print $ part2
  -----------------------
