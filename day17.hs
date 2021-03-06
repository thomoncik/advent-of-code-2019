module Main (main) where

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Ord as Ord
import Data.Function as Function
import Data.List.Split as Split
import Data.Char as Char

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
input = [1,330,331,332,109,3914,1101,0,1182,15,1102,1,1457,24,1002,0,1,570,1006,570,36,1002,571,1,0,1001,570,-1,570,1001,24,1,24,1106,0,18,1008,571,0,571,1001,15,1,15,1008,15,1457,570,1006,570,14,21102,1,58,0,1105,1,786,1006,332,62,99,21102,333,1,1,21101,0,73,0,1105,1,579,1101,0,0,572,1101,0,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,1002,574,1,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1106,0,81,21102,1,340,1,1105,1,177,21102,477,1,1,1106,0,177,21102,1,514,1,21101,0,176,0,1105,1,579,99,21102,1,184,0,1105,1,579,4,574,104,10,99,1007,573,22,570,1006,570,165,102,1,572,1182,21101,375,0,1,21101,0,211,0,1106,0,579,21101,1182,11,1,21102,1,222,0,1105,1,979,21102,388,1,1,21101,0,233,0,1106,0,579,21101,1182,22,1,21102,244,1,0,1106,0,979,21101,0,401,1,21101,255,0,0,1105,1,579,21101,1182,33,1,21101,266,0,0,1106,0,979,21101,0,414,1,21102,277,1,0,1105,1,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21102,1182,1,1,21101,0,313,0,1106,0,622,1005,575,327,1102,1,1,575,21101,327,0,0,1105,1,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,0,16,0,109,4,2102,1,-3,586,21001,0,0,-1,22101,1,-3,-3,21102,0,1,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1106,0,597,109,-4,2106,0,0,109,5,2102,1,-4,629,21001,0,0,-2,22101,1,-4,-4,21102,1,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,653,20101,0,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21101,0,702,0,1106,0,786,21201,-1,-1,-1,1106,0,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21101,731,0,0,1106,0,786,1106,0,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21101,0,756,0,1105,1,786,1106,0,774,21202,-1,-11,1,22101,1182,1,1,21101,774,0,0,1105,1,622,21201,-3,1,-3,1106,0,640,109,-5,2105,1,0,109,7,1005,575,802,21002,576,1,-6,21002,577,1,-5,1105,1,814,21101,0,0,-1,21102,1,0,-5,21102,1,0,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,63,-3,22201,-6,-3,-3,22101,1457,-3,-3,2101,0,-3,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21102,1,1,-1,1106,0,924,1205,-2,873,21102,35,1,-4,1106,0,924,1202,-3,1,878,1008,0,1,570,1006,570,916,1001,374,1,374,1201,-3,0,895,1102,2,1,0,1202,-3,1,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,921,21001,0,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,63,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,39,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1102,1,1,575,21102,1,973,0,1106,0,786,99,109,-7,2105,1,0,109,6,21101,0,0,-4,21101,0,0,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1105,1,1041,21101,-4,0,-2,1106,0,1041,21102,-5,1,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,2101,0,-2,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,2101,0,-2,0,1106,0,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1105,1,989,21102,439,1,1,1106,0,1150,21101,0,477,1,1105,1,1150,21101,0,514,1,21101,1149,0,0,1105,1,579,99,21101,1157,0,0,1105,1,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,2102,1,-5,1176,2101,0,-4,0,109,-6,2105,1,0,46,11,52,1,9,1,52,1,9,1,52,1,9,1,12,7,33,1,9,1,12,1,5,1,33,1,9,1,12,1,5,1,9,1,23,1,9,1,12,1,5,1,9,1,23,1,9,1,12,1,5,1,9,1,23,1,9,1,12,1,5,1,9,1,23,1,9,1,12,1,5,13,19,13,12,1,15,1,1,1,19,1,1,1,22,1,15,13,5,7,22,1,17,1,9,1,5,1,3,1,24,1,17,1,9,1,5,1,3,1,24,1,17,1,9,1,5,1,3,1,18,7,17,13,3,1,1,9,46,1,1,1,3,1,1,1,1,1,5,1,46,13,3,1,48,1,3,1,1,1,1,1,1,1,3,1,48,1,3,13,46,1,5,1,1,1,1,1,3,1,1,1,46,9,1,1,3,13,42,1,3,1,5,1,9,1,42,1,3,1,5,1,9,1,42,1,3,1,5,1,9,1,40,7,5,1,9,1,40,1,1,1,9,1,9,1,30,13,9,1,9,1,30,1,9,1,11,1,9,1,30,1,9,1,11,1,9,1,30,1,9,1,11,1,9,1,30,1,9,1,11,11,30,1,9,1,52,1,9,1,52,1,9,1,52,1,9,1,52,1,9,1,52,11,22]

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
      (InputGiven, (Just (cursor + 2), Map.insert (argWrite 1 mode1) input memory, relativeOffset))
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

f :: String -> State -> String
f str st = foo (run st 0) where
  foo (Printed x, s) = f ((chr x):str) s
  foo (Halted, _) = str

-----------------------------------

-- R,6,L,12,R,6,R,6,L,12,R,6,L,12,R,6,L,8,L,12,R,12,L,10,L,10,L12,R,6,L,8,L,12,R,12,L,10,L,10,L,12,R,6,L,8,L,12,R,12,L,10,L,10,L,12,R,6,L,8,L,12,R,6,L,12,R,6

part2 :: Int -> [Int] -> State -> Int
part2 str [] st = bar (programStep st 0) where
  bar (Printed x, s) = part2 x [] s
  bar (InputGiven, s) = error "No input to give"
  bar (Halted, _) = str
  bar (InnerAction, s) = part2 str [] s
part2 str (i:is) st = foo (programStep st i) (i:is) where
  foo (Printed x, s) l = part2 x l s
  foo (InputGiven, s) (l:ls) = part2 str ls s
  foo (Halted, _) l = str
  foo (InnerAction, s) l = part2 str l s

mainRoutine = "A,A,C,B,C,B,C,B,C,A\n"
a = "R,6,L,12,R,6\n"
b = "R,12,L,10,L,10\n"
c = "L,12,R,6,L,8,L,12\n"
debug = "n\n"

providedInput = mainRoutine ++ a ++ b ++ c ++ debug

main :: IO ()
main = do
  let
    s = reverse $ dropWhile (=='\n') $ f "" (Just 0, loadProgramToMemory input, 0)
    grid = Map.fromList $ concat $ List.map (\(a, l) -> List.map (\(b,c) -> ((a,b),c)) l) (zip [0..] (List.map (zip [0..]) (lines s)))
    a = [(x,y) | 
        x<-[0..100], 
        y<-[0..100], 
        (Map.findWithDefault ' ' (x-1,y) grid) == '#' &&
        (Map.findWithDefault ' ' (x+1,y) grid) == '#' &&
        (Map.findWithDefault ' ' (x,y-1) grid) == '#' &&
        (Map.findWithDefault ' ' (x,y+1) grid) == '#' &&
        (Map.findWithDefault ' ' (x,y) grid) == '#']
  print $ sum $ List.map (\(a,b) -> a*b) a
  -----------------------
  print $ part2 0 (List.map ord providedInput) (Just 0, loadProgramToMemory (2:(List.drop 1 input)), 0)