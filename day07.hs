module Main (main) where
import Data.Map as Map
import Data.List as List

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
input = [3,8,1001,8,10,8,105,1,0,0,21,38,55,68,93,118,199,280,361,442,99999,3,9,1002,9,2,9,101,5,9,9,102,4,9,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,101,4,9,9,102,3,9,9,4,9,99,3,9,102,2,9,9,101,4,9,9,102,2,9,9,1001,9,4,9,102,4,9,9,4,9,99,3,9,1002,9,2,9,1001,9,2,9,1002,9,5,9,1001,9,2,9,1002,9,4,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99]

listToMap :: [Int] -> MapIntInt
listToMap input = Map.fromList (zip [0..] input)

digits :: Int -> [Int]
digits = List.map (read . (:[])) . show

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

f :: Int -> MapIntInt -> [Int] -> [Int] -> (MapIntInt, [Int])
f i l diag pins
  | x == Halt   = (l, diag)
  | x == Output = f (i+2) l (ap:diag) pins
  | x == Input  = f (i+2) (Map.update (\y-> Just (head pins)) a l) diag (tail pins)
  | x == (Add Position Position)        = f (i+4) (Map.update (\y-> Just (ap+bp)) c l) diag pins
  | x == (Add Immediate Position)       = f (i+4) (Map.update (\y-> Just (a+bp)) c l) diag pins
  | x == (Add Position Immediate)       = f (i+4) (Map.update (\y-> Just (ap+b)) c l) diag pins
  | x == (Add Immediate Immediate)      = f (i+4) (Map.update (\y-> Just (a+b)) c l) diag pins
  | x == (Multiply Position Position)   = f (i+4) (Map.update (\y-> Just (ap*bp)) c l) diag pins
  | x == (Multiply Immediate Position)  = f (i+4) (Map.update (\y-> Just (a*bp)) c l) diag pins
  | x == (Multiply Position Immediate)  = f (i+4) (Map.update (\y-> Just (ap*b)) c l) diag pins
  | x == (Multiply Immediate Immediate) = f (i+4) (Map.update (\y-> Just (a*b)) c l) diag pins
  | x == (JumpTrue Position Position)   = if ap /= 0 then f bp l diag pins else f (i+3) l diag pins
  | x == (JumpTrue Immediate Position)  = if a /= 0 then f bp l diag pins else f (i+3) l diag pins
  | x == (JumpTrue Position Immediate)  = if ap /= 0 then f b l diag pins else f (i+3) l diag pins
  | x == (JumpTrue Immediate Immediate) = if a /= 0 then f b l diag pins else f (i+3) l diag pins
  | x == (JumpFalse Position Position)   = if ap == 0 then f bp l diag pins else f (i+3) l diag pins
  | x == (JumpFalse Immediate Position)  = if a == 0 then f bp l diag pins else f (i+3) l diag pins
  | x == (JumpFalse Position Immediate)  = if ap == 0 then f b l diag pins else f (i+3) l diag pins
  | x == (JumpFalse Immediate Immediate) = if a == 0 then f b l diag pins else f (i+3) l diag pins
  | x == (LessThan Position Position)   = if ap < bp then f (i+4) (Map.update (\y-> Just 1) c l) diag pins else f (i+4) (Map.update (\y-> Just 0) c l) diag pins
  | x == (LessThan Immediate Position)  = if a < bp then f (i+4) (Map.update (\y-> Just 1) c l) diag pins else f (i+4) (Map.update (\y-> Just 0) c l) diag pins
  | x == (LessThan Position Immediate)  = if ap < b then f (i+4) (Map.update (\y-> Just 1) c l) diag pins else f (i+4) (Map.update (\y-> Just 0) c l) diag pins
  | x == (LessThan Immediate Immediate) = if a < b then f (i+4) (Map.update (\y-> Just 1) c l) diag pins else f (i+4) (Map.update (\y-> Just 0) c l) diag pins
  | x == (Equals Position Position)   = if ap == bp then f (i+4) (Map.update (\y-> Just 1) c l) diag pins else f (i+4) (Map.update (\y-> Just 0) c l) diag pins
  | x == (Equals Immediate Position)  = if a == bp then f (i+4) (Map.update (\y-> Just 1) c l) diag pins else f (i+4) (Map.update (\y-> Just 0) c l) diag pins
  | x == (Equals Position Immediate)  = if ap == b then f (i+4) (Map.update (\y-> Just 1) c l) diag pins else f (i+4) (Map.update (\y-> Just 0) c l) diag pins
  | x == (Equals Immediate Immediate) = if a == b then f (i+4) (Map.update (\y-> Just 1) c l) diag pins else f (i+4) (Map.update (\y-> Just 0) c l) diag pins
  where 
    x = parseParam $ l Map.! i
    a = l Map.! (i+1)
    ap = l Map.! a
    b = l Map.! (i+2)
    bp = l Map.! b
    c = l Map.! (i+3)


part1 = maximum (List.map (\p -> s p input) (permutations [0..4]))

--------------------------

fAmp :: [Int] -> [Int] -> [Int]
fAmp pins prog = reverse $ snd (f 0 (listToMap prog) [] pins)

part2 :: Int
part2 = maximum (List.map (\p -> s2 p input) (permutations [5..9]))

s :: [Int] -> [Int] -> Int
s sett prog = calculate sett
  where calculate [a, b, c, d, e] = let oa = fAmp [a,0] prog
                                        ob = fAmp (b:oa) prog
                                        oc = fAmp (c:ob) prog
                                        od = fAmp (d:oc) prog
                                        oe = fAmp (e:od) prog
                                    in last oe
s2 :: [Int] -> [Int] -> Int
s2 sett prog = calculate sett
  where calculate [a, b, c, d, e] = let oa = fAmp (a:0:oe) prog
                                        ob = fAmp (b:oa) prog
                                        oc = fAmp (c:ob) prog
                                        od = fAmp (d:oc) prog
                                        oe = fAmp (e:od) prog
                                    in last oe
main :: IO ()
main = print ""
-- main = print $ part2 [-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

