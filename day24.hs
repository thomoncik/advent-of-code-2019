module Main
  ( main
  ) where

import Data.Function as Function
import Data.List as List
import Data.Set as Set
import Data.Map as Map

import Debug.Trace

type Position = (Int, Int)

t1 :: String
t1 =
  "....#\n\
  \#..#.\n\
  \#..##\n\
  \..#..\n\
  \#...."

input :: String
input =
  ".#.##\n\
  \...#.\n\
  \....#\n\
  \.#...\n\
  \..#.."

zipWith2dIndex :: [[a]] -> [(Position, a)]
zipWith2dIndex xss =
  [((i, j), x) | (j, xs) <- zip [0 ..] xss, (i, x) <- zip [0 ..] xs]

parse :: String -> Set Position
parse input =
  Set.fromList .
  List.map fst .
  List.filter (\(position, char) -> ('#' == char)) . zipWith2dIndex . lines $
  input

north = (0, -1)
south = (0, 1)
east = (1, 0)
west = (-1, 0)

tick :: Set Position -> Set Position
tick grid =
  Set.fromList . List.map fst . List.filter (\(pos, bug) -> bug == True) $
  [ ( (a, b)
    , if isBug (a, b) && adjBugCount (a, b) == 1
        then True
        else if notBug (a, b)
               then (adjBugCount (a, b) == 1 || adjBugCount (a, b) == 2)
               else False)
  | a <- [0 .. 4]
  , b <- [0 .. 4]
  ]
  where
    isBug (x, y) = (x, y) `Set.member` grid
    notBug (x, y) = not $ (x, y) `Set.member` grid
    adjBugCount (x, y) = List.length . List.filter (`Set.member` grid) . List.map (\(dx, dy) -> (x + dx, y + dy)) $ [north, south, east, west]

part1 input = go (parse input) Set.empty
    where 
      go grid repeated
        | grid `Set.member` repeated = Set.foldr (\v acc -> acc + (2^v)) 0 $ 
                                        Set.map (\(x, y) -> (y * 5) + x) grid
        | otherwise = go (tick grid) (Set.insert grid repeated)

----------------------------------------------------------------------------------------

countBugs :: Set (Int, Int, Int) -> Int -> Int -> Int -> Int
countBugs bugs x y depth =
  (if x == 0 && (1, 2, depth - 1) `Set.member` bugs then 1 else 0) +
  (if x == 4 && (3, 2, depth - 1) `Set.member` bugs then 1 else 0) +
  (if y == 0 && (2, 1, depth - 1) `Set.member` bugs then 1 else 0) +
  (if y == 4 && (2, 3, depth - 1) `Set.member` bugs then 1 else 0) +

  (if (x, y) == (2, 1) then sum [1 | a <- [0 .. 4], (a, 0, depth + 1) `Set.member` bugs] else 0) +
  (if (x, y) == (2, 3) then sum [1 | a <- [0 .. 4], (a, 4, depth + 1) `Set.member` bugs] else 0) +
  (if (x, y) == (1, 2) then sum [1 | a <- [0 .. 4], (0, a, depth + 1) `Set.member` bugs] else 0) +
  (if (x, y) == (3, 2) then sum [1 | a <- [0 .. 4], (4, a, depth + 1) `Set.member` bugs] else 0) +

  (List.length . List.filter (\(a, b) -> (a, b) /= (2, 2) && (a, b, depth) `Set.member` bugs) . List.map (\(dx, dy) -> (x + dx, y + dy)) $ [north, south, east, west])

tack :: Set (Int, Int, Int) -> Set (Int, Int, Int)
tack bugs =
  let
    depthMin = Set.findMin . Set.map (\(x, y, depth) -> depth) $ bugs
    depthMax = Set.findMax . Set.map (\(x, y, depth) -> depth) $ bugs
    
    bugs' = [(x, y, depth) | depth <- [depthMin - 1 .. depthMax + 1], y <- [0 .. 4], x <- [0 .. 4], 
                  (x, y) /= (2, 2),
                  let neighbours = countBugs bugs x y depth 
                  in (((x, y, depth) `Set.member` bugs && neighbours == 1) || 
                     ((not ((x, y, depth) `Set.member` bugs)) && (neighbours == 1 || neighbours == 2)))]
  in
    Set.fromList bugs'

parse2 :: String -> Set (Int, Int, Int)
parse2 input =
  Set.fromList .
  List.map (\((x, y), char) -> (x, y, 0)) .
  List.filter (\(position, char) -> ('#' == char)) . zipWith2dIndex . lines $
  input
    
drawMap :: Set Position -> [[Char]]
drawMap m =
  let mm =
        transpose $
        groupBy ((==) `on` snd) [(y, x) | x <- [0 .. 4], y <- [0 .. 4]]
   in List.map
        (List.map
           (\(y, x) ->
              if (x, y) `Set.member` m
                then '#'
                else '.')) $
      mm

main :: IO ()
main = do
  print $ part1 input
  print $ Set.size $ iterate tack (parse2 input) !! 200
