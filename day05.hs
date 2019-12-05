module Main (main) where
import qualified Data.Map as Map

data Mode = Position | Immediate deriving (Show, Eq)
data Action = JumpTrue Mode Mode
            | JumpFalse Mode Mode
            | LessThan Mode Mode
            | Equals Mode Mode
            | Halt 
            | Multiply Mode Mode 
            | Add Mode Mode 
            | Input 
            | Output deriving (Show, Eq)
type MapIntInt = Map.Map Int Int


input :: [Int]
input = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,68,5,225,1101,71,12,225,1,117,166,224,1001,224,-100,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1001,66,36,224,101,-87,224,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1101,26,51,225,1102,11,61,224,1001,224,-671,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,59,77,224,101,-136,224,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,1101,11,36,225,1102,31,16,225,102,24,217,224,1001,224,-1656,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,101,60,169,224,1001,224,-147,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,38,69,225,1101,87,42,225,2,17,14,224,101,-355,224,224,4,224,102,8,223,223,1001,224,2,224,1,224,223,223,1002,113,89,224,101,-979,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,69,59,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,344,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,434,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,1007,226,677,224,102,2,223,223,1006,224,494,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,509,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,524,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,539,101,1,223,223,107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,584,1001,223,1,223,1108,226,226,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,629,1001,223,1,223,108,226,226,224,102,2,223,223,1005,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226]
-- input = [1002,4,3,4,33]
-- input = [3,0,4,0,99]
providedInput = 5
-- input = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
listToMap :: [Int] -> MapIntInt
listToMap input = Map.fromList (zip [0..] input)

digits :: Int -> [Int]
digits = map (read . (:[])) . show

parseParam :: Int -> Action
parseParam x = parseParamReverseList (reverse (digits x))

parseParamReverseList :: [Int] -> Action
parseParamReverseList (1:0:1:1:_) = Add Immediate Immediate
parseParamReverseList (1:0:0:1:_) = Add Position Immediate
parseParamReverseList (1:0:1:_) = Add Immediate Position
parseParamReverseList (1:_) = Add Position Position

parseParamReverseList (2:0:1:1:_) = Multiply Immediate Immediate
parseParamReverseList (2:0:0:1:_) = Multiply Position Immediate
parseParamReverseList (2:0:1:_) = Multiply Immediate Position
parseParamReverseList (2:_) = Multiply Position Position

parseParamReverseList (3:_) = Input
parseParamReverseList (4:_) = Output

parseParamReverseList (5:0:1:1:_) = JumpTrue Immediate Immediate
parseParamReverseList (5:0:0:1:_) = JumpTrue Position Immediate
parseParamReverseList (5:0:1:_) = JumpTrue Immediate Position
parseParamReverseList (5:_) = JumpTrue Position Position

parseParamReverseList (6:0:1:1:_) = JumpFalse Immediate Immediate
parseParamReverseList (6:0:0:1:_) = JumpFalse Position Immediate
parseParamReverseList (6:0:1:_) = JumpFalse Immediate Position
parseParamReverseList (6:_) = JumpFalse Position Position

parseParamReverseList (7:0:1:1:_) = LessThan Immediate Immediate
parseParamReverseList (7:0:0:1:_) = LessThan Position Immediate
parseParamReverseList (7:0:1:_) = LessThan Immediate Position
parseParamReverseList (7:_) = LessThan Position Position

parseParamReverseList (8:0:1:1:_) = Equals Immediate Immediate
parseParamReverseList (8:0:0:1:_) = Equals Position Immediate
parseParamReverseList (8:0:1:_) = Equals Immediate Position
parseParamReverseList (8:_) = Equals Position Position

parseParamReverseList (9:9:_) = Halt
parseParamReverseList x = error (show x)

f :: Int -> MapIntInt -> [Int] -> (MapIntInt, [Int])
f i l diag
  | x == Halt   = (l, diag)
  | x == Output = f (i+2) l (ap:diag)
  | x == Input  = f (i+2) (Map.update (\y-> Just providedInput) a l) diag
  | x == (Add Position Position)        = f (i+4) (Map.update (\y-> Just (ap+bp)) c l) diag
  | x == (Add Immediate Position)       = f (i+4) (Map.update (\y-> Just (a+bp)) c l) diag
  | x == (Add Position Immediate)       = f (i+4) (Map.update (\y-> Just (ap+b)) c l) diag
  | x == (Add Immediate Immediate)      = f (i+4) (Map.update (\y-> Just (a+b)) c l) diag
  | x == (Multiply Position Position)   = f (i+4) (Map.update (\y-> Just (ap*bp)) c l) diag
  | x == (Multiply Immediate Position)  = f (i+4) (Map.update (\y-> Just (a*bp)) c l) diag
  | x == (Multiply Position Immediate)  = f (i+4) (Map.update (\y-> Just (ap*b)) c l) diag
  | x == (Multiply Immediate Immediate) = f (i+4) (Map.update (\y-> Just (a*b)) c l) diag
  | x == (JumpTrue Position Position)   = if ap /= 0 then f bp l diag else f (i+3) l diag
  | x == (JumpTrue Immediate Position)  = if a /= 0 then f bp l diag else f (i+3) l diag
  | x == (JumpTrue Position Immediate)  = if ap /= 0 then f b l diag else f (i+3) l diag
  | x == (JumpTrue Immediate Immediate) = if a /= 0 then f b l diag else f (i+3) l diag
  | x == (JumpFalse Position Position)   = if ap == 0 then f bp l diag else f (i+3) l diag
  | x == (JumpFalse Immediate Position)  = if a == 0 then f bp l diag else f (i+3) l diag
  | x == (JumpFalse Position Immediate)  = if ap == 0 then f b l diag else f (i+3) l diag
  | x == (JumpFalse Immediate Immediate) = if a == 0 then f b l diag else f (i+3) l diag
  | x == (LessThan Position Position)   = if ap < bp then f (i+4) (Map.update (\y-> Just 1) c l) diag else f (i+4) (Map.update (\y-> Just 0) c l) diag
  | x == (LessThan Immediate Position)  = if a < bp then f (i+4) (Map.update (\y-> Just 1) c l) diag else f (i+4) (Map.update (\y-> Just 0) c l) diag
  | x == (LessThan Position Immediate)  = if ap < b then f (i+4) (Map.update (\y-> Just 1) c l) diag else f (i+4) (Map.update (\y-> Just 0) c l) diag
  | x == (LessThan Immediate Immediate) = if a < b then f (i+4) (Map.update (\y-> Just 1) c l) diag else f (i+4) (Map.update (\y-> Just 0) c l) diag
  | x == (Equals Position Position)   = if ap == bp then f (i+4) (Map.update (\y-> Just 1) c l) diag else f (i+4) (Map.update (\y-> Just 0) c l) diag
  | x == (Equals Immediate Position)  = if a == bp then f (i+4) (Map.update (\y-> Just 1) c l) diag else f (i+4) (Map.update (\y-> Just 0) c l) diag
  | x == (Equals Position Immediate)  = if ap == b then f (i+4) (Map.update (\y-> Just 1) c l) diag else f (i+4) (Map.update (\y-> Just 0) c l) diag
  | x == (Equals Immediate Immediate) = if a == b then f (i+4) (Map.update (\y-> Just 1) c l) diag else f (i+4) (Map.update (\y-> Just 0) c l) diag
  where 
    x = parseParam $ l Map.! i
    a = l Map.! (i+1)
    ap = l Map.! a
    b = l Map.! (i+2)
    bp = l Map.! b
    c = l Map.! (i+3)

solve :: [Int] -> [Int]
solve i = reverse (snd (f 0 (listToMap i) []))

main :: IO ()
main = print $ solve input
