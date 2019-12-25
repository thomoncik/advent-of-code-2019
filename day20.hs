module Main (main) where

import Data.Char as Char
import Data.Function as Function
import Data.List as List
import Data.Map as Map
import Data.Ord as Ord
import Data.Set as Set
import Data.Tuple as Tuple
import Data.Sequence as Sequence

import Debug.Trace as Trace

t1 =
  "         A            \n\
  \         A            \n\
  \  #######.#########   \n\
  \  #######.........#   \n\
  \  #######.#######.#   \n\
  \  #######.#######.#   \n\
  \  #######.#######.#   \n\
  \  #####  B    ###.#   \n\
  \BC...##  C    ###.#   \n\
  \  ##.##       ###.#   \n\
  \  ##...DE  F  ###.#   \n\
  \  #####    G  ###.#   \n\
  \  #########.#####.#   \n\
  \DE..#######...###.#   \n\
  \  #.#########.###.#   \n\
  \FG..#########.....VV  \n\
  \  ###########.#####   \n\
  \             Z        \n\
  \             Z        "

t2 =
  "                   A               \n\
  \                   A               \n\
  \  #################.#############  \n\
  \  #.#...#...................#.#.#  \n\
  \  #.#.#.###.###.###.#########.#.#  \n\
  \  #.#.#.......#...#.....#.#.#...#  \n\
  \  #.#########.###.#####.#.#.###.#  \n\
  \  #.............#.#.....#.......#  \n\
  \  ###.###########.###.#####.#.#.#  \n\
  \  #.....#        A   C    #.#.#.#  \n\
  \  #######        S   P    #####.#  \n\
  \  #.#...#                 #......VT\n\
  \  #.#.#.#                 #.#####  \n\
  \  #...#.#               YN....#.#  \n\
  \  #.###.#                 #####.#  \n\
  \DI....#.#                 #.....#  \n\
  \  #####.#                 #.###.#  \n\
  \ZZ......#               QG....#..AS\n\
  \  ###.###                 #######  \n\
  \JO..#.#.#                 #.....#  \n\
  \  #.#.#.#                 ###.#.#  \n\
  \  #...#..DI             BU....#..LF\n\
  \  #####.#                 #.#####  \n\
  \YN......#               VT..#....QG\n\
  \  #.###.#                 #.###.#  \n\
  \  #.#...#                 #.....#  \n\
  \  ###.###    J L     J    #.#.###  \n\
  \  #.....#    O F     P    #.#...#  \n\
  \  #.###.#####.#.#####.#####.###.#  \n\
  \  #...#.#.#...#.....#.....#.#...#  \n\
  \  #.#####.###.###.#.#.#########.#  \n\
  \  #...#.#.....#...#.#.#.#.....#.#  \n\
  \  #.###.#####.###.###.#.#.#######  \n\
  \  #.#.........#...#.............#  \n\
  \  #########.###.###.#############  \n\
  \           B   J   C               \n\
  \           U   P   P               "

input =
  "                                           S         C   O       U         Z     R                                             \n\
\                                           M         O   U       K         W     O                                             \n\
\  #########################################.#########.###.#######.#########.#####.###########################################  \n\
\  #.#...#...#.....#.#.#...#.............#...#.#.....#...#...#...........#.#.....#...........#...#.#...#.#.#...#.#...#...#.#.#  \n\
\  #.###.###.#.#.###.#.###.###########.#.###.#.#.#######.###.###########.#.###.#.#######.#.#.###.#.#.###.#.#.###.#.###.###.#.#  \n\
\  #...#.#.#...#.#.........#...#.#...#.#.......#.....#...#.#.#.#...#.........#.#.#.......#.#.......................#.........#  \n\
\  #.###.#.#####.###.#####.###.#.###.#####.#####.#####.###.#.#.###.#.#.#.#.#####.#.###.#####.#.###.#.###.#.#####.#####.#####.#  \n\
\  #.#.#.#...............#.......................#.#...#.#.....#...#.#.#.#.#.....#...#.#...#.#...#.#...#.#.#.#.#.#.......#.#.#  \n\
\  #.#.#.#########.#.#.#####.#.#.#######.#######.#.#.#.#.###.#####.#.#.#####.###.#.#####.#####.#.###.#.###.#.#.###.#####.#.###  \n\
\  #.#.#.#.....#.#.#.#.#.#...#.#.#...#.....#...#...#.#.....#.....#...#...#.#.#.#.#.........#.#.#.#...#.#...#...#.#.#...#.#.#.#  \n\
\  #.#.#.###.###.#######.#####.###.###.#######.#.#######.###.#.###.#.#.###.###.#.###.#.#.###.#.###.#.#.#####.###.#.#.#####.#.#  \n\
\  #.#...#.......#.........#.....#.#.#...#.#.#.....#.......#.#...#.#.#...#.........#.#.#...#.#...#.#.#.#...#...........#...#.#  \n\
\  #.#.#####.#############.#######.#.#####.#.###.###.###########.#.#.#.#.#.#####.###.###.###.#####.#.#####.#.###.###.###.###.#  \n\
\  #.......#...#.......#...........................#.....#.#...#.#.#.#.#.#...#.....#.#...........#.#...#.#...#.#.#.#...#.....#  \n\
\  #####.###.###.###.###.###.#.###.#####.#.#.###########.#.#.#.#.#.#######.#.#####.#.###.#####.#########.###.#.###.#####.#####  \n\
\  #...#.....#.#.#.#.#.#.#.#.#.#.#.#.#...#.#.#...#.........#.#.#.#.#.#.....#.#.#.#.#.#.......#.................#.#.#.....#...#  \n\
\  ###.###.###.###.#.#.###.#####.###.#####.###.#.###.#########.#.#.#.#####.###.#.#######.#######.###.#.###.#.#.#.#.###.#####.#  \n\
\  #.#.#.......#.................#.#.#.........#.#.......#.......#...#...............#...#.....#.#.#.#.#.#.#.#...#.....#.#...#  \n\
\  #.#.###.###.#.#.#.###.###.###.#.#.#####.#.###.###.#.#########.#.#######.#.#.#.#######.#.#######.###.#.#######.#####.#.###.#  \n\
\  #...#.#.#.#.#.#.#...#...#.#.#...........#.#.#.#...#.#.....#.#.#...#.#...#.#.#.#.#.................#.......#.#...#.....#...#  \n\
\  ###.#.#.#.#.#####.#.#######.#.#.###.###.###.#.###.#####.#.###.#.###.#####.###.#.#####.###.#######.#.#######.#########.#.###  \n\
\  #.........#...#.#.#.#.......#.#.#...#...#.#.....#.....#.#...#.#.#.....#...#.#.#.....#...#.......#.#...#.......#.#...#.....#  \n\
\  ###.###.#######.#####.#####.#.#.#######.#.###.#.#.###.#.#.###.#.###.###.###.###.###.#.#.###############.###.###.###.###.###  \n\
\  #.#...#.#.......#.#.#.#...#...#...#.....#.#...#.#.#.#.#.#.#...#.......#.......#.#.#...#...#.#.........#.#.#.....#.........#  \n\
\  #.#####.#####.###.#.#####.#.###########.#.###.###.#.#.#.#.#.#.#.#####.#######.###.#.#####.#.###.#######.#.#############.###  \n\
\  #.#.#.#...#.....#...#.......#...#.#.#.#.#.......#...#.#.#...#.#.#.......#.......#.#...#.#.#.#.#.#...#.....#.#.#.#.#.#.....#  \n\
\  #.#.#.#.###.#.###.###.###.#.###.#.#.#.###.#.#####.#.###.#.#.###.###.#########.###.#.###.###.#.#.###.#.#####.#.#.#.#.###.###  \n\
\  #...#...#.#.#.#.....#.#.#.#.#.......#.....#...#...#...#.#.#.#...#.......#.#.#.#...#.............#.#.#...#...#.#...........#  \n\
\  #.#.#.###.###.###.#####.#########.###.###.###.#.#.#####.#############.###.#.#.#.#.#.###.#.#.#.###.#.#.#####.#.#.#.#.#.###.#  \n\
\  #.#...#...#.#...#...#...#...#.#.....#.#.#.#...#.#.#.#.#.......#.#.#...#.......#.#...#.#.#.#.#.#.#.............#.#.#.#.#...#  \n\
\  #####.#.###.#.###.#####.###.#.#.#.###.#.###.###.###.#.#####.###.#.#.#####.#.###.#####.#.###.###.#.###########.#########.###  \n\
\  #.#.#.#.#.#.....#.#.#.#.#...#...#.#.#.#.......#.......#.#...#.....#...#...#...#.....#.#.#.#.#.#.....#...#.#.#.#.#.#.#.#...#  \n\
\  #.#.#.#.#.#.#####.#.#.#.###.#####.#.#####.#.#.#####.###.###.###.#.#.#######.###.###.#.###.#.#.###.###.###.#.###.#.#.#.#.#.#  \n\
\  #.......#.#.....#...........#.#.#.......#.#.#.#.....#...#.#...#.#...#.....#...#...#.......#...#.................#.......#.#  \n\
\  #######.#.#.#####.###.#####.#.#.###.###.#####.#.#####.###.###.#.#########.#.#########.###.#######.###.#.#.###############.#  \n\
\  #.....#.....#...#.#.#...#.....#...#.#.........#.....#.........#.........#.......#.......#.#.#.......#.#.#...#...#...#.....#  \n\
\  ###.###.#######.###.#######.#.#.#######.#########.#######.###########.###.#########.#######.#####.###########.#.#.###.#.#.#  \n\
\  #.#.#.......#.#.#.#.#...#.#.#.#...#    H         G       U           O   O         Z    #.#...#.......#.....#.#...#.#.#.#.#  \n\
\  #.#.###.#.###.#.#.#.###.#.###.###.#    Y         M       K           H   U         W    #.###.#.#########.#######.#.#####.#  \n\
\  #.......#.#.#.....#.#.......#.....#                                                     #.#.......#.#.#.#.#...............#  \n\
\  #.#.#######.#####.#.#####.#.#.###.#                                                     #.###.#.###.#.#.#.#####.#########.#  \n\
\TV..#...#...#.#.#.......#...#.....#..AO                                                   #.#...#...#.#.#...#.#.#...#.....#.#  \n\
\  ###.#####.#.#.#####.#####.#######.#                                                     #.###.#.###.#.#.###.#.###.#######.#  \n\
\ZZ....#.#...#.#...#.......#.....#...#                                                     #...#.#.........#.#.#.#.#...#.....#  \n\
\  #.###.#.#.#.#.###.###.#.###.#####.#                                                     #.#.#.#####.###.#.#.#.###.#.#.#####  \n\
\  #.......#...........#.#.......#.#.#                                                   FA..#.....#.#.#.#...........#.#......DI\n\
\  #####################.#.#.#.###.###                                                     ###.#####.###.#############.#######  \n\
\  #.....#.............#.#.#.#.#.#...#                                                     #...#...............#...#.#.#......AO\n\
\  #.#.###.#.###.#.###.#####.###.#.#.#                                                     #####.#####.#.###.#.#.#.#.###.#####  \n\
\LU..#.....#.#...#.#.#...#.#.#.....#.#                                                   DR......#.....#.#.#.#...#.#...#.....#  \n\
\  #####.#.#####.###.#.###.#####.###.#                                                     #.#.#####.#####.#######.#.#.#####.#  \n\
\  #.#.#.#.....#.#...........#.....#..ZA                                                   #.#.#.......#...........#.#.......#  \n\
\  #.#.###.#####.#####.#####.#.#.#.#.#                                                     #######.#######.#.#.#####.###.#.#.#  \n\
\  #...#.#...#.#.....#.#.......#.#.#.#                                                     #.#...#...#.....#.#.......#...#.#.#  \n\
\  ###.#.#####.#.#####.#########.#####                                                     #.#.#####.#########.###.#.#####.###  \n\
\  #.........#...#...#.#.....#.#.....#                                                   CQ..#.....#.#...#.......#.#.#.#...#..FA\n\
\  #.#######.#####.#######.#.#.#######                                                     #.###.#####.#############.#.#####.#  \n\
\WA......#...#.#.#...#.#...#...#.....#                                                     #.#...#...#.....#.#.#.#.#...#...#.#  \n\
\  #.#####.###.#.###.#.###.###.#.###.#                                                     #.###.#.#####.#.#.#.#.#.#####.###.#  \n\
\  #.#.#...#.#.#.#.#.....#...#.#...#..OB                                                   #.#.......#...#.#.#.......#.#.#.#.#  \n\
\  ###.###.#.#.#.#.#.###.###.#.###.###                                                     #.#.#.#########.#.#.#####.#.#.#.#.#  \n\
\  #.....#.............#.....#.......#                                                     #...#.................#...........#  \n\
\  #.#########.#.#.###.#.###.#.#.#.###                                                     #########################.#####.#.#  \n\
\  #.....#.....#.#.#.#.#.#...#.#.#.#.#                                                     #...#...................#.#...#.#.#  \n\
\  #.###.#####.#.#.#.#############.#.#                                                     #.#.#.#######.#.#####.#####.#.###.#  \n\
\  #...#...#...#.#.#.....#.#.#...#.#.#                                                     #.#.#.#.....#.#...#...#.....#...#.#  \n\
\  ###.#.###############.#.#.#.#.###.#                                                     #.#.#.###.###.#####.###.#####.#####  \n\
\ZA..#.#...#.#.#...#...#.......#...#.#                                                   KN..#.#.......#.#.......#...#.#.#....FB\n\
\  #.#.#.###.#.#.#.###.#.###.###.###.#                                                     #.#.#####.#.#########.###.#.#.#.#.#  \n\
\  #...#.........#.......#.#.#........SM                                                   #.#.......#.#.....#.......#.#...#.#  \n\
\  ###########.###.#######.###########                                                     #############.#.###########.#######  \n\
\  #.#.....#...#...#...#...#.......#.#                                                     #.........#.#.#.#.#................KL\n\
\  #.#.#.###########.#.#.#.#.#.#####.#                                                     #.#####.#.#.###.#.###.#.#####.###.#  \n\
\WW....#.............#...#...#........LU                                                 WW..#...#.#.............#.....#.#...#  \n\
\  ###########.#.#####.###.#.#########                                                     #.###.###.###.###.#.#.#.#.###.###.#  \n\
\CQ..........#.#.....#...#.#...#.....#                                                   BX..#.......#...#.#.#.#.#.#.#.#.#.#..OH\n\
\  #.#######.###################.#.###                                                     #####.#.#####.#.###########.###.###  \n\
\  #.#.......#.....#.#...#.......#.#..PE                                                 KL....#.#.#.#.....#...#...#...#.....#  \n\
\  ###.#.#.###.#####.#.#####.#####.#.#                                                     #.#######.#########.#.###.#####.#.#  \n\
\  #.#.#.#.#.....#.......#...#.....#.#                                                     #.#...#...#...#.#.#.....#.#.#...#..GM\n\
\  #.###.###.#.#####.#.#####.#.#.###.#                                                     #.#.#.###.###.#.#.###.###.#.###.#.#  \n\
\  #.#.......#.......#.......#.#.....#                                                     #...#...........................#.#  \n\
\  #.#########.#.###.###.###.#####.###                                                     #####.#####.#.#####################  \n\
\  #.........#.#.#...#.#.#.#.#...#...#                                                   RO....#.#.#...#.#...#.#.#.#.........#  \n\
\  ###.#.#####.#####.#.#.#.#####.#####                                                     ###.###.#######.#.#.#.#.#.#.###.###  \n\
\WJ..#.#.#...#.#...#.#...#.....#.#.#..WA                                                   #.........#.....#.......#.#.#.....#  \n\
\  #.#.#.###.###.#.#######.#.#.#.#.#.#                                                     #.#######.#.###.###.###.#.#####.#.#  \n\
\  #.#.#...#.....#.#...#.#.#.#.....#.#                                                     #.#.......#.#.....#.#.#...#.....#.#  \n\
\  #.#.###.#.#.#.#####.#.###.#.#.#.#.#                                                     #.#####.###.#########.#.#.#######.#  \n\
\  #...#.....#.#.............#.#.#...#                                                     #.#.#.............#.....#.#...#....OB\n\
\  #.#.###.#.#.#####.###.#.#########.#    D       C       U           W       F   T        ###.#.###.###.#########.#.###.###.#  \n\
\  #.#.#...#.#...#.....#.#...#.#.....#    I       O       I           J       B   V        #.#.....#...#.#...#.#...#.#.......#  \n\
\  #.#####.###.#.###.###.#####.#.#.#######.#######.#######.###########.#######.###.#########.#.#######.#.#.#.#.###.###.###.#.#  \n\
\  #.....#.#.#.#...#.#.#...#.....#.#.......#...#.#.#...........#.#.#...#.........#.#.......#.#.#...#...#...#.#.....#.#.#.#.#.#  \n\
\  #.#####.#.#.#####.#.###########.#####.#.#.#.#.#.#####.###.###.#.#.#.###.#######.#.#.#.###.###.#######.#.###.#.#.#.#.#.###.#  \n\
\  #.#.#.#.#.#.#.....#.#.#...#.#.#.#.#...#...#.#.......#.#...#.#.....#.#.....#...#...#.#.#.........#.#...#.#...#.#.#.....#...#  \n\
\  ###.#.#.#.#######.#.#.#.###.#.#.#.#.#.###.#####.###.###.###.###.#####.###.###.#.###.#.#.#####.###.###.#.#.#.#.###########.#  \n\
\  #.............#...........#.....#...#.#...#.#.#...#...#.....#...#.#...#.....#...#.#.#...#...#.......#.#.#.#.#...#.........#  \n\
\  #####.#.#.#.#####.###############.#.#####.#.#.#.###.#######.###.#.#####.#######.#.#######.###.###.###.#.#####.###.###.###.#  \n\
\  #.....#.#.#...#.............#...#.#...#...#.......#.#.....#.#.#...#.#.....#.#.#.#...#.......#.#.#.#...#.#.#.#...#...#.#...#  \n\
\  ###.#.###.#####.#####.#########.#.#######.#.###.#.#####.#.#.#.###.#.###.#.#.#.#.#.###.###.#.###.###.#.###.#.#####.#.###.#.#  \n\
\  #...#.#.......#...#.....#.......#.#.#.#...#.#...#.#.....#...#.........#.#.#...#.........#.#...#.#...#.....#...#...#.#...#.#  \n\
\  ###.#####.#####.#######.#.###.#.###.#.#.###.#.#.#######.#######.#####.###.#.#.#.#####.#######.#.###.###.###.#######.#####.#  \n\
\  #.#.#.#.....#...#.......#.#...#.#.#.......#.#.#...#.#...#.#.#.#.#.....#.#...#.#.....#.#.#.........#.#.#.........#...#...#.#  \n\
\  #.###.###.###.#.#####.#.#####.###.#####.#######.###.###.#.#.#.#.###.#.#.#####.###.#####.###.#.###.###.#.#.###.#####.#.###.#  \n\
\  #...........#.#.#.....#.#.................#.#.#.....#.......#.#.#.#.#.#.......#.......#...#.#...#.#.#...#.#.....#.......#.#  \n\
\  #######.###.###.#.###.#####.#####.#.###.#.#.#.###.#####.###.#.#.#.#.#.###.#####.#####.#.###.###.###.#.#.#.###.###.#.#####.#  \n\
\  #...#.....#...#.#.#...#.....#.#...#.#...#.#.......#.......#.#.....#.#.#.....#.......#...#.#.#.....#...#.#...#.#...#.....#.#  \n\
\  ###.###.#####.#####.#.#####.#.#########.###.###.#.#####.#.###########.#.#.#####.#####.#.#.#.###########.#####.#.#.#.#.#.#.#  \n\
\  #.#.........#...#...#.....#.#...#.#.#.#...#...#.#...#.#.#.......#.....#.#...#.......#.#.#.#.#.....#.#.....#...#.#.#.#.#.#.#  \n\
\  #.#####.#####.#######.#########.#.#.#.#.#######.###.#.#.#############.#.#.#####.###.###.#.###.#####.###.###.###.#.#.#.#.###  \n\
\  #...........#.....#.#.#.................#.#.......#...#...#...#.....#.#.#.#.#.#.#.#.#.#.#...#...#.#.......#...#.#.#.#.#...#  \n\
\  #.###.#.###########.#.###.#.###.#.#.#.#.#.#.#####.#####.#.###.###.#.#.#.###.#.#.#.###.###.#####.#.###.#.#.#######.#.#######  \n\
\  #...#.#.......#.#...#.#.#.#.#.#.#.#.#.#.#.....#.....#...#...#.#.#.#.#.#.....#.......#.....#...#.#...#.#.#.....#...#.#.....#  \n\
\  #.#.###.#######.###.###.###.#.#########.###########.###.#####.#.#.#.#.#.#######.###.#.#####.###.#.#.#####.###.###.#####.###  \n\
\  #.#.#.#.......#.#.......#...#.......#.....#.#.#...#.#.#.#.....#...#...#.....#.#...#.#.............#.#.#.#.#.#...#.......#.#  \n\
\  #.###.#.###.###.#.###.#.#########.#.###.###.#.#.#.#.#.#.#.###.#.#######.#####.#.#####.#####.###.#####.#.###.#.#####.#.#.#.#  \n\
\  #...#.....#.#.#.#.#...#...#.#.#...#...#.#.......#.#.#.....#...#.....#.#...#...#.#.#...#...#.#.#.......#.#...#...#...#.#...#  \n\
\  #####.###.###.#.#######.#.#.#.#####.###.#######.#.#.#######.#####.#.#.#.#####.#.###.#.#.#####.#.#######.###.#######.###.###  \n\
\  #.#...#...#...........#.#...#...........#...#...#.....#.....#.#.#.#...#.....#...#...#...#.#.#.#.#.............#...#.#.#...#  \n\
\  #.#.#######.#.#.###.#.###.#.###########.#.#####.###########.#.#.#####.#.###.###.#.###.#.#.#.#.#####.###.#.#.###.###.#.#.###  \n\
\  #.......#...#.#.#.#.#.....#.#...#.........#.....#.......#...#.....#...#.#.#.#...#...#.#.#.#.#.......#.#.#.#.......#.#.....#  \n\
\  #.#######.#.#.#.#.###.#####.#.###########.#####.#.###.#####.#.#######.#.#.#####.###.#.###.#.#.###.###.#.#.###.#.#####.###.#  \n\
\  #...#.....#.#.#.#.....#...........#.#.#...#...#...#.#.....#.....#.....#...#.#.......#...........#.....#.#.#.#.#...#.....#.#  \n\
\  #.###.#.#########.#.#.#.#.#.###.#.#.#.#.###.#.#####.#.###.###.#####.#####.#.#.#.#.###.###.###.###.#########.#.###.#.#.#.#.#  \n\
\  #...#.#...#.......#.#.#.#.#.#...#...........#.#.........#.#.....#.....#.....#.#.#.#...#.....#...#...........#...#.#.#.#.#.#  \n\
\  #############################################.#.#####.#######.###.#####.###########.#######################################  \n\
\                                               B A     H       P   K     D           U                                         \n\
\                                               X A     Y       E   N     R           I                                         "

type Position = (Int, Int)
type Maze = Map Position Char

mazeFromString :: String -> Maze
mazeFromString string = Map.fromList . concat $ List.map (\(y, l) -> List.map (\(x, v) -> ((x, y), v)) l) (Prelude.zip [0 ..] (List.map (Prelude.zip [0 ..]) (lines string)))

labelToPosition :: Maze -> String -> [Position]
labelToPosition maze (a:b:[]) = 
  let 
    maxX = maximum . List.map fst $ Map.keys maze
    maxY = maximum . List.map snd $ Map.keys maze
  in
    [(x,y) | x <- [2..maxX - 2], y <- [2..maxY - 2],
    (maze ! (x - 2, y) == a && maze ! (x - 1, y) == b && maze ! (x, y) == '.') ||
    (maze ! (x, y) == '.' && maze ! (x + 1, y) == a && maze ! (x + 2, y) == b) ||
    (maze ! (x, y - 2) == a && maze ! (x, y - 1) == b && maze ! (x, y) == '.') ||
    (maze ! (x, y) == '.' && maze ! (x, y + 1) == a && maze ! (x, y + 2) == b)]

teleports :: Maze -> [[Position]]
teleports maze = List.filter ((2==) . List.length) . List.map (labelToPosition maze) $ [[a,b] | a <- ['A'..'Z'], b <- ['A'..'Z']]

teleport :: Maze -> Position -> Position
teleport maze (x, y) = 
  let teleports = List.filter ((2==) . List.length) . List.map (labelToPosition maze) $ [[a,b] | a <- ['A'..'Z'], b <- ['A'..'Z']]
  in head . List.filter ((x, y) /=) . head . List.filter (List.any ((x, y) == )) $ teleports

part1 :: String -> Int
part1 str = 
  let
    startPosition = (head . labelToPosition maze $ "AA")
    endPosition = (head . labelToPosition maze $ "ZZ")
    maze = mazeFromString str
    telep = teleports maze
  in
    bfs maze endPosition telep Set.empty ((0, startPosition, False) :<| Empty)
  where
    bfs maze endPosition telep visited ((distance, (x, y), justTeleported) :<| nodes)
      | (x, y) == endPosition = distance
      | justTeleported == False && (List.any (List.any ((x, y) ==)) $ telep) = bfs maze endPosition telep visited' ((distance + 1, teleport maze (x, y), True) :<| nodes)
      | otherwise = bfs maze endPosition telep visited' (nodes >< Sequence.fromList toVisit)
      where
        cell (a, b) = trace (show (distance, (x, y)) ++ " " ++ show (a, b)) $ Map.findWithDefault '#' (a, b) maze
        current = cell (x, y)
        visited' = Set.insert (x, y) visited
        toVisit = List.map (\(x', y') -> (distance + 1, (x', y'), False)) .
                    List.filter 
                    (\dir -> not (dir `Set.member` visited) && cell dir == '.') $
                    [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

--------------------------------

main :: IO ()
main = do
  print ""
