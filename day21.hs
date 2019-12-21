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
input = [109,2050,21102,966,1,1,21101,0,13,0,1106,0,1378,21101,0,20,0,1105,1,1337,21101,27,0,0,1105,1,1279,1208,1,65,748,1005,748,73,1208,1,79,748,1005,748,110,1208,1,78,748,1005,748,132,1208,1,87,748,1005,748,169,1208,1,82,748,1005,748,239,21102,1,1041,1,21102,1,73,0,1105,1,1421,21101,0,78,1,21101,0,1041,2,21102,1,88,0,1105,1,1301,21101,0,68,1,21102,1041,1,2,21102,103,1,0,1105,1,1301,1102,1,1,750,1105,1,298,21101,82,0,1,21102,1,1041,2,21102,1,125,0,1105,1,1301,1101,0,2,750,1106,0,298,21101,79,0,1,21102,1,1041,2,21102,1,147,0,1105,1,1301,21102,84,1,1,21101,0,1041,2,21102,162,1,0,1105,1,1301,1102,3,1,750,1106,0,298,21101,0,65,1,21102,1,1041,2,21101,184,0,0,1105,1,1301,21102,1,76,1,21102,1,1041,2,21102,199,1,0,1105,1,1301,21101,75,0,1,21102,1041,1,2,21102,1,214,0,1105,1,1301,21102,221,1,0,1105,1,1337,21102,1,10,1,21101,1041,0,2,21102,1,236,0,1106,0,1301,1106,0,553,21101,0,85,1,21101,0,1041,2,21102,1,254,0,1105,1,1301,21101,78,0,1,21102,1041,1,2,21102,1,269,0,1106,0,1301,21101,0,276,0,1106,0,1337,21102,1,10,1,21101,1041,0,2,21101,0,291,0,1105,1,1301,1102,1,1,755,1106,0,553,21101,32,0,1,21102,1,1041,2,21101,0,313,0,1106,0,1301,21102,320,1,0,1105,1,1337,21101,327,0,0,1106,0,1279,1201,1,0,749,21102,1,65,2,21102,73,1,3,21102,1,346,0,1105,1,1889,1206,1,367,1007,749,69,748,1005,748,360,1101,0,1,756,1001,749,-64,751,1105,1,406,1008,749,74,748,1006,748,381,1101,-1,0,751,1105,1,406,1008,749,84,748,1006,748,395,1101,-2,0,751,1106,0,406,21101,0,1100,1,21101,0,406,0,1106,0,1421,21102,32,1,1,21101,0,1100,2,21101,0,421,0,1105,1,1301,21102,428,1,0,1105,1,1337,21101,0,435,0,1106,0,1279,2101,0,1,749,1008,749,74,748,1006,748,453,1102,-1,1,752,1106,0,478,1008,749,84,748,1006,748,467,1102,-2,1,752,1105,1,478,21101,1168,0,1,21101,478,0,0,1105,1,1421,21101,0,485,0,1105,1,1337,21101,10,0,1,21102,1168,1,2,21101,0,500,0,1105,1,1301,1007,920,15,748,1005,748,518,21101,0,1209,1,21102,518,1,0,1106,0,1421,1002,920,3,529,1001,529,921,529,1002,750,1,0,1001,529,1,537,1002,751,1,0,1001,537,1,545,101,0,752,0,1001,920,1,920,1105,1,13,1005,755,577,1006,756,570,21102,1,1100,1,21102,1,570,0,1105,1,1421,21101,0,987,1,1105,1,581,21101,0,1001,1,21102,588,1,0,1105,1,1378,1101,758,0,594,102,1,0,753,1006,753,654,21001,753,0,1,21101,610,0,0,1106,0,667,21101,0,0,1,21102,1,621,0,1106,0,1463,1205,1,647,21102,1015,1,1,21102,1,635,0,1106,0,1378,21102,1,1,1,21102,646,1,0,1106,0,1463,99,1001,594,1,594,1106,0,592,1006,755,664,1101,0,0,755,1106,0,647,4,754,99,109,2,1102,1,726,757,22101,0,-1,1,21102,1,9,2,21102,697,1,3,21102,1,692,0,1106,0,1913,109,-2,2105,1,0,109,2,101,0,757,706,2101,0,-1,0,1001,757,1,757,109,-2,2106,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,255,63,95,127,159,223,191,0,69,237,34,117,213,198,217,216,221,172,218,185,178,53,177,174,122,163,196,226,103,162,70,111,46,84,233,238,246,206,245,175,166,157,42,248,47,212,118,109,56,121,108,200,92,78,190,155,215,235,59,38,182,76,234,49,71,116,137,68,43,220,107,169,125,179,204,99,54,152,189,123,173,143,232,124,86,126,252,139,241,214,183,187,39,244,113,136,188,203,253,171,58,184,57,242,98,140,154,205,168,119,85,102,239,50,222,100,55,60,229,228,247,79,197,254,35,51,153,167,227,142,251,94,201,249,61,110,62,236,114,158,141,170,219,77,156,138,101,250,199,106,181,243,230,115,202,231,93,120,186,87,207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20,73,110,112,117,116,32,105,110,115,116,114,117,99,116,105,111,110,115,58,10,13,10,87,97,108,107,105,110,103,46,46,46,10,10,13,10,82,117,110,110,105,110,103,46,46,46,10,10,25,10,68,105,100,110,39,116,32,109,97,107,101,32,105,116,32,97,99,114,111,115,115,58,10,10,58,73,110,118,97,108,105,100,32,111,112,101,114,97,116,105,111,110,59,32,101,120,112,101,99,116,101,100,32,115,111,109,101,116,104,105,110,103,32,108,105,107,101,32,65,78,68,44,32,79,82,44,32,111,114,32,78,79,84,67,73,110,118,97,108,105,100,32,102,105,114,115,116,32,97,114,103,117,109,101,110,116,59,32,101,120,112,101,99,116,101,100,32,115,111,109,101,116,104,105,110,103,32,108,105,107,101,32,65,44,32,66,44,32,67,44,32,68,44,32,74,44,32,111,114,32,84,40,73,110,118,97,108,105,100,32,115,101,99,111,110,100,32,97,114,103,117,109,101,110,116,59,32,101,120,112,101,99,116,101,100,32,74,32,111,114,32,84,52,79,117,116,32,111,102,32,109,101,109,111,114,121,59,32,97,116,32,109,111,115,116,32,49,53,32,105,110,115,116,114,117,99,116,105,111,110,115,32,99,97,110,32,98,101,32,115,116,111,114,101,100,0,109,1,1005,1262,1270,3,1262,21002,1262,1,0,109,-1,2106,0,0,109,1,21101,0,1288,0,1105,1,1263,21001,1262,0,0,1101,0,0,1262,109,-1,2106,0,0,109,5,21101,1310,0,0,1106,0,1279,21202,1,1,-2,22208,-2,-4,-1,1205,-1,1332,22101,0,-3,1,21101,1332,0,0,1105,1,1421,109,-5,2105,1,0,109,2,21101,0,1346,0,1105,1,1263,21208,1,32,-1,1205,-1,1363,21208,1,9,-1,1205,-1,1363,1106,0,1373,21101,0,1370,0,1105,1,1279,1105,1,1339,109,-2,2105,1,0,109,5,2102,1,-4,1386,20101,0,0,-2,22101,1,-4,-4,21101,0,0,-3,22208,-3,-2,-1,1205,-1,1416,2201,-4,-3,1408,4,0,21201,-3,1,-3,1106,0,1396,109,-5,2106,0,0,109,2,104,10,21201,-1,0,1,21101,0,1436,0,1106,0,1378,104,10,99,109,-2,2105,1,0,109,3,20002,594,753,-1,22202,-1,-2,-1,201,-1,754,754,109,-3,2105,1,0,109,10,21102,5,1,-5,21101,1,0,-4,21101,0,0,-3,1206,-9,1555,21102,1,3,-6,21101,0,5,-7,22208,-7,-5,-8,1206,-8,1507,22208,-6,-4,-8,1206,-8,1507,104,64,1105,1,1529,1205,-6,1527,1201,-7,716,1515,21002,0,-11,-8,21201,-8,46,-8,204,-8,1105,1,1529,104,46,21201,-7,1,-7,21207,-7,22,-8,1205,-8,1488,104,10,21201,-6,-1,-6,21207,-6,0,-8,1206,-8,1484,104,10,21207,-4,1,-8,1206,-8,1569,21101,0,0,-9,1106,0,1689,21208,-5,21,-8,1206,-8,1583,21101,0,1,-9,1105,1,1689,1201,-5,716,1589,20102,1,0,-2,21208,-4,1,-1,22202,-2,-1,-1,1205,-2,1613,21201,-5,0,1,21101,0,1613,0,1105,1,1444,1206,-1,1634,21201,-5,0,1,21101,1627,0,0,1105,1,1694,1206,1,1634,21101,0,2,-3,22107,1,-4,-8,22201,-1,-8,-8,1206,-8,1649,21201,-5,1,-5,1206,-3,1663,21201,-3,-1,-3,21201,-4,1,-4,1106,0,1667,21201,-4,-1,-4,21208,-4,0,-1,1201,-5,716,1676,22002,0,-1,-1,1206,-1,1686,21101,0,1,-4,1106,0,1477,109,-10,2105,1,0,109,11,21101,0,0,-6,21102,0,1,-8,21102,1,0,-7,20208,-6,920,-9,1205,-9,1880,21202,-6,3,-9,1201,-9,921,1724,21001,0,0,-5,1001,1724,1,1733,20102,1,0,-4,21201,-4,0,1,21101,1,0,2,21102,1,9,3,21101,1754,0,0,1105,1,1889,1206,1,1772,2201,-10,-4,1767,1001,1767,716,1767,20101,0,0,-3,1106,0,1790,21208,-4,-1,-9,1206,-9,1786,21202,-8,1,-3,1106,0,1790,21201,-7,0,-3,1001,1733,1,1795,21001,0,0,-2,21208,-2,-1,-9,1206,-9,1812,21201,-8,0,-1,1106,0,1816,22101,0,-7,-1,21208,-5,1,-9,1205,-9,1837,21208,-5,2,-9,1205,-9,1844,21208,-3,0,-1,1106,0,1855,22202,-3,-1,-1,1105,1,1855,22201,-3,-1,-1,22107,0,-1,-1,1105,1,1855,21208,-2,-1,-9,1206,-9,1869,21202,-1,1,-8,1106,0,1873,21202,-1,1,-7,21201,-6,1,-6,1105,1,1708,21202,-8,1,-10,109,-11,2106,0,0,109,7,22207,-6,-5,-3,22207,-4,-6,-2,22201,-3,-2,-1,21208,-1,0,-6,109,-7,2106,0,0,0,109,5,2101,0,-2,1912,21207,-4,0,-1,1206,-1,1930,21102,1,0,-4,22101,0,-4,1,22102,1,-3,2,21101,1,0,3,21102,1,1949,0,1106,0,1954,109,-5,2106,0,0,109,6,21207,-4,1,-1,1206,-1,1977,22207,-5,-3,-1,1206,-1,1977,22102,1,-5,-5,1106,0,2045,21201,-5,0,1,21201,-4,-1,2,21202,-3,2,3,21101,0,1996,0,1105,1,1954,21202,1,1,-5,21102,1,1,-2,22207,-5,-3,-1,1206,-1,2015,21102,0,1,-2,22202,-3,-2,-3,22107,0,-4,-1,1206,-1,2037,22101,0,-2,1,21102,2037,1,0,106,0,1912,21202,-3,-1,-3,22201,-5,-3,-5,109,-6,2105,1,0]

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

------------

providedInput = "NOT A J\n\
                \NOT B T\n\
                \OR T J\n\
                \NOT C T\n\
                \OR T J\n\
                \AND D J\n\
                \WALK\n"

providedInput2 = "NOT A J\n\
                \NOT B T\n\
                \OR T J\n\
                \NOT C T\n\
                \OR T J\n\
                \AND D J\n" -- that's from part1, now make sure I can walk away or jump from there
             ++ "NOT E T\n\
                \NOT T T\n\
                \OR H T\n\
                \AND T J\n\
                \RUN\n"

asciiIntcode :: String -> [Int] -> State -> String
asciiIntcode str [] st = bar (programStep st 0) where
  bar (Printed x, s) = asciiIntcode (chr x:str) [] s
  bar (InputGiven, s) = error "No input to give"
  bar (Halted, _) = reverse str
  bar (InnerAction, s) = asciiIntcode str [] s
  asciiIntcode str (i:is) st = foo (programStep st i) (i:is) where
  foo (Printed x, s) l = asciiIntcode (chr x:str) l s
  foo (InputGiven, s) (l:ls) = asciiIntcode str ls s
  foo (Halted, _) l = reverse str
  foo (InnerAction, s) l = asciiIntcode str l s

main :: IO ()
main = do
  putStr $ asciiIntcode "" (List.map ord providedInput) (Just 0, loadProgramToMemory input, 0)
  putStr $ asciiIntcode "" (List.map ord providedInput2) (Just 0, loadProgramToMemory input, 0)