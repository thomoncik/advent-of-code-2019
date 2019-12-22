module Main (main) where

len = 119315717514047
-- len = 10
-- len = 10007

dealPos :: Int -> Int
dealPos pos = (len - pos - 1) `mod` len

cutPos :: Int -> Int -> Int
cutPos n pos = (pos - n) `mod` len

dealWithIncrementPos :: Int -> Int -> Int
dealWithIncrementPos n pos = pos * n `mod` len

shuffleInputPos pos = dealWithIncrementPos 3 . cutPos 5193 . dealWithIncrementPos 34 . cutPos 3443 . dealWithIncrementPos 22 . dealPos . cutPos 8075 . dealPos . dealWithIncrementPos 54 . cutPos (-5777) . dealWithIncrementPos 61 . cutPos 5339 . dealWithIncrementPos 25 . cutPos (-2460) . dealWithIncrementPos 63 . cutPos (-785) . dealWithIncrementPos 18 . cutPos 3638 . dealWithIncrementPos 47 . dealPos . cutPos 6002 . dealWithIncrementPos 51 . cutPos 8271 . dealPos . cutPos (-8044) . dealWithIncrementPos 67 . dealPos . dealWithIncrementPos 55 . cutPos (-1830) . dealPos . dealWithIncrementPos 75 . cutPos (-9250) . dealWithIncrementPos 65 . cutPos 9483 . dealWithIncrementPos 55 . cutPos 5979 . dealWithIncrementPos 13 . dealPos . dealWithIncrementPos 59 . cutPos 9008 . dealWithIncrementPos 38 . cutPos (-9203) . dealWithIncrementPos 7 . dealPos . dealWithIncrementPos 71 . dealPos . dealWithIncrementPos 63 . cutPos (-3436) . dealPos . dealWithIncrementPos 5 . cutPos 6877 . dealWithIncrementPos 27 . dealPos . dealWithIncrementPos 65 . dealPos . dealWithIncrementPos 30 . cutPos 960 . dealWithIncrementPos 12 . cutPos 1015 . dealWithIncrementPos 44 . cutPos (-7726) . dealPos . cutPos (-1845) . dealWithIncrementPos 64 . cutPos (-648) . dealWithIncrementPos 59 . cutPos (-5080) . dealWithIncrementPos 36 . cutPos (-2225) . dealWithIncrementPos 9 . cutPos 4405 . dealWithIncrementPos 40 . cutPos (-8246) . dealPos . dealWithIncrementPos 32 . cutPos 6558 . dealWithIncrementPos 16 . cutPos 3168 . dealWithIncrementPos 8 . dealPos . dealWithIncrementPos 51 . cutPos (-9423) . dealWithIncrementPos 28 . cutPos 957 . dealWithIncrementPos 59 . cutPos 9112 . dealPos . dealWithIncrementPos 3 . cutPos 2157 . dealWithIncrementPos 53 . dealPos . dealWithIncrementPos 15 . cutPos 8319 . dealPos . dealWithIncrementPos 27 . cutPos (-3856) . dealWithIncrementPos 32 . dealPos . dealWithIncrementPos 36 . cutPos (-8737) $ pos

------------

modExp :: Int -> Int -> Int -> Int
modExp  x y n = mod (x^(mod y (n-1))) (n)

modInverse :: Int -> Int -> Int
modInverse a m = modExp a (m - 2) m

dealPosRev :: Int -> Int
dealPosRev x = len - 1 - x
    
cutPosRev :: Int -> Int -> Int
cutPosRev n x = (x + n + len) `mod` len

dealWithIncrementPosRev :: Int -> Int -> Int
dealWithIncrementPosRev x n = (modInverse n len) * x `mod` len

main :: IO ()
main = do
  print $ shuffleInputPos 2019 -- with len set to 10007, part1
