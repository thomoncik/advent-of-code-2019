module Main (main) where

import Data.Char as Char
import Data.Function as Function
import Data.List as List
import Data.Map as Map
import Data.Ord as Ord
import Data.Set as Set
import Data.Tuple as Tuple

import Debug.Trace as Trace

t3 =
  "#########\n\
  \#b.A.@.a#\n\
  \#########"

t2 =
  "########################\n\
  \#@..............ac.GI.b#\n\
  \###d#e#f################\n\
  \###A#B#C################\n\
  \###g#h#i################\n\
  \########################"

t1 =
  "#################\n\
  \#i.G..c...e..H.p#\n\
  \########.########\n\
  \#j.A..b...f..D.o#\n\
  \########@########\n\
  \#k.E..a...g..B.n#\n\
  \########.########\n\
  \#l.F..d...h..C.m#\n\
  \#################"
----------------

type Position = (Int, Int)
type Maze = Map Position Char

mazeFromString :: String -> Maze
mazeFromString string = Map.fromList . concat $ List.map (\(y, l) -> List.map (\(x, v) -> ((x, y), v)) l) (zip [0 ..] (List.map (zip [0 ..]) (lines string)))

doorPositions :: Maze -> Map Char Position
doorPositions maze = Map.fromList . List.map swap . Map.toList . Map.filter isAsciiUpper $ maze

keyPositions :: Maze -> Map Char Position
keyPositions maze = Map.fromList . List.map swap . Map.toList . Map.filter isAsciiLower $ maze

positionOf :: Maze -> Char -> Position
positionOf maze x = head . Map.keys . Map.filter (x ==) $ maze

distanceToKeys :: Maze -> Char -> Map Char (Int, Set Char)
distanceToKeys maze x = bfs maze 0 (positionOf maze x) Set.empty Set.empty Map.empty
  where
    bfs maze distance position@(x, y) visited doors result
      | distance == 0 && maze ! position /= '.' = bfs (Map.insert position '.' maze) distance position visited' doors result
      | isAsciiLower (maze ! position) = bfs (Map.insert position '.' maze) distance position visited' doors (Map.insert (maze ! position) (distance, doors) result)
      | isAsciiUpper (maze ! position) = bfs (Map.insert position '.' maze) distance position visited' (Set.insert (maze ! position) doors) result
      | otherwise = List.foldl Map.union result $ List.map (\(x', y') -> bfs maze (distance + 1) (x', y') visited' doors result) toVisit
      where
        cell (a, b) = Map.findWithDefault '#' (a, b) maze
        visited' = Set.insert position visited
        north = (x, y + 1)
        south = (x, y - 1)
        east = (x + 1, y)
        west = (x - 1, y)
        toVisit = List.filter (\dir -> not (dir `Set.member` visited) && cell dir /= '#') [north, south, east, west]

type KeyToKeyMapping = Map Char (Map Char (Int, Set Char))
keyToKeys :: Maze -> KeyToKeyMapping
keyToKeys maze = Map.fromList . List.map (\k -> (k, distanceToKeys maze k)) . ('@':) . Map.elems . Map.filter isAsciiLower $ maze

type Node = (Char, Set Char)
dijkstra :: KeyToKeyMapping -> Set (Int, Node) -> Map Char Int -> Map Char Int
dijkstra mapping queue dist
      | Set.null queue  = trace "?" dist
      | allKeys `Set.isSubsetOf` collectedKeys = dist
      | otherwise =
        let
          adjacent = trace ("u: " ++ show u ++ " ck: " ++ show collectedKeys) $ Map.filter (\(_, neededKeys) -> neededKeys `Set.isSubsetOf` collectedKeys) $ (mapping ! u)
          adjacentDistances = Map.map fst adjacent
          
          toRelax = Map.filterWithKey (\v (c,ks) -> cu + c < (getDist v)) $ adjacent
          toRelaxDistances = Map.map fst toRelax
          
          dist' = Map.union dist $ Map.map (\c -> cu + c) toRelaxDistances

          queueNotToBeChanged = Set.filter (\(_, (k, ks)) -> not $ k `Map.member` toRelax) $ queueWithoutMin
          queue' = Set.union queueWithoutMin $ Set.fromList . List.map(\(v, (c, ks)) -> (c + (getDist u), (v, Set.insert (toUpper v) ks))) . Map.toList $ toRelax
        in
          dijkstra mapping queue' dist'
      where
        ((cu, (u, collectedKeys)), queueWithoutMin) = Set.deleteFindMin queue
        allKeys = Set.fromList . Map.keys $ mapping
        getDist key = Map.findWithDefault 1000000000 key dist

distances :: KeyToKeyMapping -> Map Char Int
distances mapping = dijkstra mapping (Set.fromList [(0, ('@', Set.fromList "@"))]) (Map.fromList [('@', 0)])

main :: IO ()
main = do
  print ""