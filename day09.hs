module Main (main) where

import Data.List as List
import Data.Map as Map

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
input = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,3,1,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,22,1012,1101,309,0,1024,1102,1,29,1015,1101,0,30,1014,1101,0,221,1028,1102,24,1,1007,1102,32,1,1006,1102,1,31,1001,1101,0,20,1010,1101,34,0,1003,1102,899,1,1026,1101,304,0,1025,1101,0,1,1021,1101,892,0,1027,1101,0,0,1020,1101,0,484,1023,1101,25,0,1018,1101,0,21,1008,1102,491,1,1022,1102,212,1,1029,1102,1,23,1000,1101,0,26,1009,1102,36,1,1005,1101,27,0,1013,1101,35,0,1019,1101,38,0,1017,1101,0,39,1004,1102,37,1,1002,1102,33,1,1011,1102,28,1,1016,109,1,1208,5,35,63,1005,63,201,1001,64,1,64,1106,0,203,4,187,1002,64,2,64,109,36,2106,0,-9,4,209,1001,64,1,64,1105,1,221,1002,64,2,64,109,-30,2101,0,-4,63,1008,63,34,63,1005,63,247,4,227,1001,64,1,64,1105,1,247,1002,64,2,64,109,1,21108,40,40,8,1005,1016,265,4,253,1106,0,269,1001,64,1,64,1002,64,2,64,109,10,21101,41,0,-7,1008,1011,41,63,1005,63,295,4,275,1001,64,1,64,1105,1,295,1002,64,2,64,109,3,2105,1,3,4,301,1106,0,313,1001,64,1,64,1002,64,2,64,109,-18,2108,38,1,63,1005,63,329,1105,1,335,4,319,1001,64,1,64,1002,64,2,64,109,-11,2108,37,10,63,1005,63,357,4,341,1001,64,1,64,1106,0,357,1002,64,2,64,109,25,21107,42,41,-6,1005,1011,377,1001,64,1,64,1106,0,379,4,363,1002,64,2,64,109,-11,1207,3,25,63,1005,63,395,1105,1,401,4,385,1001,64,1,64,1002,64,2,64,109,-4,1202,0,1,63,1008,63,37,63,1005,63,423,4,407,1105,1,427,1001,64,1,64,1002,64,2,64,109,8,21102,43,1,6,1008,1016,43,63,1005,63,453,4,433,1001,64,1,64,1106,0,453,1002,64,2,64,109,-11,1208,6,36,63,1005,63,471,4,459,1105,1,475,1001,64,1,64,1002,64,2,64,109,21,2105,1,3,1001,64,1,64,1105,1,493,4,481,1002,64,2,64,109,-15,2107,22,3,63,1005,63,513,1001,64,1,64,1106,0,515,4,499,1002,64,2,64,109,-7,2107,35,7,63,1005,63,537,4,521,1001,64,1,64,1105,1,537,1002,64,2,64,109,23,1205,0,551,4,543,1105,1,555,1001,64,1,64,1002,64,2,64,109,-4,21101,44,0,-3,1008,1014,45,63,1005,63,579,1001,64,1,64,1105,1,581,4,561,1002,64,2,64,109,-15,2102,1,3,63,1008,63,33,63,1005,63,601,1106,0,607,4,587,1001,64,1,64,1002,64,2,64,109,23,1205,-5,623,1001,64,1,64,1106,0,625,4,613,1002,64,2,64,109,-7,21102,45,1,-8,1008,1010,43,63,1005,63,645,1105,1,651,4,631,1001,64,1,64,1002,64,2,64,109,-11,2102,1,1,63,1008,63,21,63,1005,63,677,4,657,1001,64,1,64,1106,0,677,1002,64,2,64,109,3,21107,46,47,4,1005,1014,695,4,683,1106,0,699,1001,64,1,64,1002,64,2,64,109,7,21108,47,48,-4,1005,1013,715,1106,0,721,4,705,1001,64,1,64,1002,64,2,64,109,-14,1201,0,0,63,1008,63,32,63,1005,63,741,1106,0,747,4,727,1001,64,1,64,1002,64,2,64,109,4,1201,2,0,63,1008,63,26,63,1005,63,769,4,753,1105,1,773,1001,64,1,64,1002,64,2,64,109,5,1207,-4,22,63,1005,63,795,4,779,1001,64,1,64,1106,0,795,1002,64,2,64,109,2,2101,0,-9,63,1008,63,34,63,1005,63,819,1001,64,1,64,1106,0,821,4,801,1002,64,2,64,109,-11,1202,1,1,63,1008,63,38,63,1005,63,841,1105,1,847,4,827,1001,64,1,64,1002,64,2,64,109,21,1206,-4,865,4,853,1001,64,1,64,1105,1,865,1002,64,2,64,109,3,1206,-6,877,1105,1,883,4,871,1001,64,1,64,1002,64,2,64,109,6,2106,0,-6,1001,64,1,64,1105,1,901,4,889,4,64,99,21101,0,27,1,21101,915,0,0,1106,0,922,21201,1,23692,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,942,1,0,1106,0,922,21202,1,1,-1,21201,-2,-3,1,21101,0,957,0,1106,0,922,22201,1,-1,-2,1106,0,968,22102,1,-2,-2,109,-3,2106,0,0]

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

part1 :: [Integer]
part1 = programOutput input [1]

part2 :: [Integer]
part2 = programOutput input [2]

----------------------------------------------------
main :: IO ()
main = do
  print part1
  print part2
  