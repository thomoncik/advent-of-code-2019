module Main
  ( main
  ) where

import Data.List as List
import Data.List.Split as Split
import Data.Map as Map

s :: String
s = "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

test1 = "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL\n"
test2 = "4 ORE => 1 C\n3 C => 1 B\n2 B => 1 A\n1 A => 1 FUEL\n"
test3 = "1 ORE => 6 A\n1 A => 5 B\n1 A 4 B => 1 FUEL\n"
test4 = "10 ORE => 10 A\n\
         \1 ORE => 1 B\n\
         \7 A, 1 B => 1 C\n\
         \7 A, 1 C => 1 D\n\
         \7 A, 1 D => 1 E\n\
         \7 A, 1 E => 1 FUEL\n"
test5 = "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX"
input = "11 RVCS => 8 CBMDT\n29 QXPB, 8 QRGRH => 8 LGMKD\n3 VPRVD => 6 PMFZG\n1 CNWNQ, 11 MJVXS => 6 SPLM\n13 SPDRZ, 13 PMFZG => 2 BLFM\n8 QWPFN => 7 LWVB\n1 SPLM => 8 TKWQ\n2 QRGRH, 6 CNWNQ => 7 DTZW\n2 DMLT, 1 SPLM, 1 TMDK => 9 NKNS\n1 MJVXS, 1 HLBV => 7 PQCQH\n1 JZHZP, 9 LWVB => 7 MJSCQ\n29 DGFR => 7 QRGRH\n14 XFLKQ, 2 NKNS, 4 KMNJF, 3 MLZGQ, 7 TKWQ, 24 WTDW, 11 CBMDT => 4 GJKX\n4 TKWQ, 1 WLCFR => 4 PDKGT\n2 NKNS => 4 GDKL\n4 WRZST => 9 XFLKQ\n19 DGFR => 4 VPRVD\n10 MJSCQ, 4 QWPFN, 4 QXPB => 2 MLZGQ\n1 JZHZP => 7 QWPFN\n1 XFLKQ => 9 FQGVL\n3 GQGXC => 9 VHGP\n3 NQZTV, 1 JZHZP => 2 NVZWL\n38 WLCFR, 15 GJKX, 44 LGMKD, 2 CBVXG, 2 GDKL, 77 FQGVL, 10 MKRCZ, 29 WJQD, 33 BWXGC, 19 PQCQH, 24 BKXD => 1 FUEL\n102 ORE => 5 DGFR\n17 NWKLB, 1 SBPLK => 5 HRQM\n3 BWXGC => 8 TQDP\n1 TQDP => 2 PSZDZ\n2 MJVXS => 9 WNXG\n2 NBTW, 1 HRQM => 2 SVHBH\n8 CNWNQ, 1 DTZW => 4 RVCS\n4 VHGP, 20 WNXG, 2 SVHBH => 3 SPDRZ\n110 ORE => 5 TXMC\n10 QRGRH => 5 NWKLB\n1 SBPLK => 3 MJVXS\n9 DGFR => 5 RFSRL\n5 LBTV => 3 DMLT\n1 NWKLB, 1 KMNJF, 1 HDQXB, 6 LBTV, 2 PSZDZ, 34 PMFZG, 2 SVHBH => 2 WJQD\n1 RVCS => 5 MKRCZ\n14 NQZTV, 3 FPLT, 1 SJMS => 2 GQGXC\n18 RFSRL, 13 VHGP, 23 NBTW => 5 WTDW\n1 VHGP, 6 TKWQ => 7 QXPB\n1 JZHZP, 1 CNWNQ => 5 KMNJF\n109 ORE => 9 BWXGC\n2 CNWNQ, 1 PDKGT, 2 KMNJF => 5 HDQXB\n1 PDKGT, 18 WRZST, 9 MJSCQ, 3 VHGP, 1 BLFM, 1 LGMKD, 7 WLCFR => 2 BKXD\n11 MLJK => 6 FPLT\n8 DGFR, 2 TXMC, 3 WJRC => 9 SJMS\n2 SBPLK => 1 LBTV\n22 QWPFN => 4 WRZST\n5 WRZST, 22 WNXG, 1 VHGP => 7 NBTW\n7 RVCS => 9 TMDK\n1 DGFR, 14 TXMC => 5 JZHZP\n2 JZHZP => 3 SBPLK\n19 PDKGT => 8 HLBV\n195 ORE => 6 WJRC\n6 GQGXC => 8 CNWNQ\n1 NVZWL, 4 GQGXC => 2 CBVXG\n1 NVZWL, 1 KMNJF => 8 WLCFR\n153 ORE => 4 MLJK\n1 BWXGC => 6 NQZTV"

type Substance = String
type SubstanceAmounts = Map.Map Substance Int
type Reactions = Map.Map Substance (Int, SubstanceAmounts)

parse :: String -> Reactions
parse str =
  List.foldl1 Map.union $
    List.map (parseLine . chunksOf 2 . List.filter (/= "=>") . reverse . splitOn " " . List.filter (not . (`elem` ",")))
    (lines str)
  where
    parseLine ([k, v]:inputs) = Map.insert k (read v, Map.fromList $ List.map (\[x, y] -> (x, read y)) inputs) Map.empty

countOreFor :: Reactions -> Substance -> Int -> SubstanceAmounts -> (Int, SubstanceAmounts)
countOreFor _ "ORE" amount waste = (amount, waste)
countOreFor reactions substance amount waste =
  let 
    reuse = min amount (Map.findWithDefault 0 substance waste)
    amount' = amount  - reuse
    waste' = Map.update (\wst -> Just (wst - reuse)) substance waste

    (produced, inputs) = reactions ! substance
    copies = ceiling (fromIntegral amount' / fromIntegral produced)

    (ore, nw) = Map.foldrWithKey
      (\subs amnt (sm,sw) -> let next = countOreFor reactions subs (copies * amnt) sw in (sm + (fst next), snd next))
      (0, waste')
      inputs

    newWaste = Map.insert substance ((Map.findWithDefault 0 substance nw) + copies * produced - amount') nw
  in
    (ore, newWaste)

part1 str = fst $ countOreFor (parse str) "FUEL" 1 Map.empty

-----------------

mid :: Int -> Int -> Int
mid a b = (a + b) `div` 2

binSearch :: (Int -> Int) -> Int -> Int -> Int -> Int
binSearch g lower upper x
    | upper - lower < 2 = lower
    | g (mid lower upper) < x = binSearch g (mid lower upper) upper x
    | g (mid lower upper) > x = binSearch g lower (mid lower upper) x

part2 str = binSearch (\x -> fst $ countOreFor (parse str) "FUEL" x Map.empty) 1 100000000000 1000000000000

-----------------

main :: IO ()
main = do
  print $ part1 input
  print $ part2 input
