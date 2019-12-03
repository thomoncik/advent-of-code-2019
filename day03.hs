module Main (main) where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data Direction = U Int | L Int | D Int | R Int
type M = Set (Int, Int)

wire1 :: [Direction]
-- wire1 = [R 8,U 5,L 5,D 3]
-- wire1 = [R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72]
wire1 = [R 1000,U 564,L 752,D 449,R 783,D 938,L 106,U 130,R 452,U 462,R 861,U 654,L 532,D 485,R 761,U 336,L 648,U 671,L 618,U 429,R 122,D 183,L 395,U 662,R 900,U 644,L 168,D 778,L 268,U 896,L 691,D 852,L 987,U 462,R 346,U 103,R 688,U 926,R 374,D 543,R 688,D 682,R 992,D 140,L 379,D 245,L 423,D 504,R 957,U 937,L 67,D 560,L 962,U 275,R 688,D 617,L 778,U 581,R 672,D 402,R 3,U 251,R 593,U 897,L 866,U 189,L 8,D 5,R 761,U 546,R 594,D 880,L 318,U 410,L 325,U 564,L 889,U 688,L 472,D 146,R 317,D 314,L 229,U 259,R 449,D 630,L 431,U 4,R 328,D 727,R 298,D 558,R 81,D 508,L 160,U 113,L 994,U 263,L 193,D 631,R 881,D 608,L 924,U 447,R 231,U 885,L 157,D 739,R 656,D 121,R 704,U 437,L 710,D 207,R 150,U 406,R 816,U 683,R 496,D 715,L 899,U 757,L 579,D 684,L 85,D 354,R 198,D 411,R 818,U 772,L 910,U 493,R 38,D 130,L 955,U 741,R 744,D 224,L 485,U 201,L 903,D 904,R 748,U 288,R 34,U 673,R 503,D 931,L 190,U 547,L 83,D 341,R 459,U 114,L 758,U 220,L 506,U 444,L 472,D 941,L 68,D 910,R 415,U 668,L 957,U 709,R 817,U 116,R 699,D 424,R 548,D 285,R 347,U 396,R 791,U 62,L 785,D 360,L 628,U 415,L 568,D 429,R 154,D 840,L 865,U 181,L 106,D 564,L 452,U 156,L 967,D 421,R 41,U 500,L 316,D 747,R 585,D 858,L 809,U 402,L 484,U 752,R 319,D 563,R 273,U 84,R 53,U 874,L 849,U 90,R 194,D 969,R 907,D 625,L 298,D 984,R 744,U 172,R 537,D 177,L 14,D 921,L 156,U 133,R 429,D 787,R 688,U 894,L 154,U 192,R 663,D 225,L 781,U 426,R 623,D 60,L 723,D 995,R 814,D 195,L 951,D 594,R 994,D 543,L 893,U 781,R 899,U 85,R 270,U 303,R 256,U 977,R 894,U 948,R 270,D 301,L 874,D 388,R 290,U 986,L 660,D 741,L 25,U 381,R 814,D 150,R 578,D 529,R 550,D 176,R 221,D 653,R 529,U 83,R 351,D 462,R 492,U 338,R 611,D 5,L 137,D 547,R 305,U 356,R 83,D 880,R 522,U 681,R 353,D 54,R 910,U 774,L 462,U 48,L 511,U 750,R 98,U 455,R 585,D 579,L 594]

wire2 :: [Direction]
-- wire2 = [U 7,R 6,D 4,L 4]
-- wire2 = [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]
wire2 = [L 1003,U 936,R 846,U 549,L 824,D 684,R 944,U 902,R 177,U 875,L 425,U 631,L 301,U 515,L 790,D 233,R 49,U 408,L 184,D 103,R 693,D 307,L 557,D 771,L 482,D 502,R 759,D 390,L 378,U 982,L 430,U 337,L 970,U 400,R 829,U 212,L 92,D 670,R 741,D 566,L 797,U 477,L 377,U 837,R 19,U 849,L 21,D 870,L 182,U 414,L 586,U 768,L 637,U 135,R 997,U 405,L 331,D 256,L 22,D 46,L 504,D 660,L 757,U 676,L 360,D 499,R 180,D 723,L 236,U 78,R 218,U 523,L 71,D 60,L 485,U 503,L 352,D 969,R 747,U 831,L 285,D 859,L 245,D 517,L 140,U 463,L 895,U 284,L 546,U 342,R 349,D 438,R 816,U 21,L 188,U 482,L 687,D 903,L 234,U 15,L 758,D 294,R 789,D 444,L 498,D 436,L 240,D 956,L 666,U 686,R 978,D 827,R 919,U 108,R 975,D 35,R 475,U 59,L 374,U 24,L 26,D 497,R 454,D 388,R 180,D 561,R 80,D 433,R 439,D 818,R 962,D 912,R 247,U 972,R 948,D 807,R 867,D 946,R 725,U 395,R 706,U 187,L 17,U 332,L 862,D 660,L 70,U 608,R 223,D 506,R 592,U 357,R 520,D 149,L 572,D 800,L 570,D 358,R 648,U 174,R 520,U 153,L 807,U 92,R 840,U 560,L 938,D 599,R 972,D 539,R 385,D 495,L 26,D 894,L 907,D 103,L 494,U 51,L 803,D 620,L 68,D 226,R 947,U 210,R 864,D 755,L 681,D 520,L 867,D 577,R 378,D 741,L 91,D 294,L 289,D 531,L 301,U 638,L 496,U 83,L 278,D 327,R 351,D 697,L 593,U 331,R 91,D 967,R 419,D 327,R 78,U 304,R 462,D 2,L 656,D 700,L 27,D 29,L 598,U 741,L 349,D 957,R 161,U 688,R 326,D 798,L 263,U 45,L 883,U 982,R 116,D 835,L 878,U 253,L 232,D 732,R 639,D 408,R 997,D 867,R 726,D 258,L 65,D 600,L 315,U 783,L 761,U 606,R 67,D 949,L 475,U 542,L 231,U 279,L 950,U 649,L 670,D 870,L 264,U 958,R 748,D 365,R 252,D 129,R 754,U 27,R 571,D 690,L 671,U 143,L 750,U 303,L 412,U 24,L 443,D 550,R 826,U 699,L 558,U 543,L 881,D 204,R 248,D 192,R 813,U 316,L 76,D 78,R 523,U 716,L 422,D 793,R 684,D 175,L 347,D 466,L 219,D 140,L 803,U 433,R 96]

distance :: (Int, Int) -> (Int, Int) -> Int
distance (a, b) (x, y) = abs(a - x) + abs(b - y)

addIntersections :: [Direction] -> (Int, Int) -> M -> M
addIntersections [] (x,y) m = Set.insert (x,y) m
addIntersections ((U v):dirs) (x, y) m 
  | v == 0    = addIntersections dirs (x, y) (Set.insert (x,y) m)
  | otherwise = addIntersections ((U (v-1)):dirs) (x, y+1) (Set.insert (x, y) m)
addIntersections ((L v):dirs) (x, y) m 
  | v == 0    = addIntersections dirs (x, y) (Set.insert (x,y) m)
  | otherwise = addIntersections ((L (v-1)):dirs) (x-1, y) (Set.insert (x, y) m)
addIntersections ((D v):dirs) (x, y) m 
  | v == 0    = addIntersections dirs (x, y) (Set.insert (x,y) m)
  | otherwise = addIntersections ((D (v-1)):dirs) (x, y-1) (Set.insert (x, y) m)
addIntersections ((R v):dirs) (x, y) m 
  | v == 0    = addIntersections dirs (x, y) (Set.insert (x,y) m)
  | otherwise = addIntersections ((R (v-1)):dirs) (x+1, y) (Set.insert (x, y) m)

part1 :: [Direction] -> [Direction] -> Int
part1 a b = Set.findMin (Set.filter (/=0) (Set.map (distance (0,0)) (Set.intersection (addIntersections a (0, 0) Set.empty) (addIntersections b (0, 0) Set.empty))))

-------------------

type A = Map (Int, Int) Int

f :: [Direction] -> (Int, Int) -> Int -> A -> A
f [] (x,y) a m = Map.insert (x,y) a m
f ((U v):dirs) (x, y) a m 
  | v == 0    = f dirs (x, y) a (Map.insert (x,y) a m)
  | otherwise = f ((U (v-1)):dirs) (x, y+1) (a+1) (Map.insert (x, y) a m)
f ((L v):dirs) (x, y) a m 
  | v == 0    = f dirs (x, y) a (Map.insert (x,y) a m)
  | otherwise = f ((L (v-1)):dirs) (x-1, y) (a+1) (Map.insert (x, y) a m)
f ((D v):dirs) (x, y) a m 
  | v == 0    = f dirs (x, y) a (Map.insert (x,y) a m)
  | otherwise = f ((D (v-1)):dirs) (x, y-1) (a+1) (Map.insert (x, y) a m)
f ((R v):dirs) (x, y) a m 
  | v == 0    = f dirs (x, y) a (Map.insert (x,y) a m)
  | otherwise = f ((R (v-1)):dirs) (x+1, y) (a+1) (Map.insert (x, y) a m)


part2 :: [Direction] -> [Direction] -> Int
part2 a b = foldr1 min (map snd (Map.toList (Map.filter (/=0) (Map.intersectionWith (+) (f a (0, 0) 0 Map.empty) (f b (0, 0) 0 Map.empty)))))


main :: IO ()
main = do
  print $ part2 wire1 wire2
