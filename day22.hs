{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Finite
import Data.Semigroup
import Data.Group (Group(..))
import GHC.TypeLits

data Technique
  = Deal
  | Cut Integer
  | DealInc Integer
  deriving (Show)

technique :: String -> Technique
technique string = parse (words string)
  where
    parse (["deal", "into", "new", "stack"]) = Deal
    parse (["cut", n]) = Cut (read n)
    parse (["deal", "with", "increment", n]) = DealInc (read n)

data Linear a =
  Linear a a
  deriving (Show)

instance Num a => Semigroup (Linear a) where
  (Linear a b) <> (Linear c d) = Linear (c * a) (c * b Prelude.+ d)

instance Num a => Monoid (Linear a) where
  mempty = Linear 1 0

eval :: Num a => Linear a -> a -> a
eval (Linear a b) x = a * x Prelude.+ b

(@#) :: Num a => Linear a -> a -> a
(@#) (Linear a b) y = (y - b) * (a ^ (maxBound @(Finite 119315717514047) - 1))

techniqueToLinear :: KnownNat n => Technique -> Linear (Finite n)
techniqueToLinear Deal = Linear (negate 1) (negate 1)
techniqueToLinear (Cut k) = Linear 1 (negate . modulo $ k)
techniqueToLinear (DealInc k) = Linear (modulo k) 0

main :: IO ()
main = do
  let
    techniques = map technique input
    shuffle :: KnownNat n => Linear (Finite n)
    shuffle = foldMap techniqueToLinear techniques
  print $ eval (shuffle @10007) 2019
  print $ 101741582076661 `stimes` shuffle @119315717514047 @# 2020

-------------------------------------------------------------------------------
-- Input
-------------------------------------------------------------------------------

input :: [String]
input = [
  "cut -8737",
  "deal with increment 36",
  "deal into new stack",
  "deal with increment 32",
  "cut -3856",
  "deal with increment 27",
  "deal into new stack",
  "cut 8319",
  "deal with increment 15",
  "deal into new stack",
  "deal with increment 53",
  "cut 2157",
  "deal with increment 3",
  "deal into new stack",
  "cut 9112",
  "deal with increment 59",
  "cut 957",
  "deal with increment 28",
  "cut -9423",
  "deal with increment 51",
  "deal into new stack",
  "deal with increment 8",
  "cut 3168",
  "deal with increment 16",
  "cut 6558",
  "deal with increment 32",
  "deal into new stack",
  "cut -8246",
  "deal with increment 40",
  "cut 4405",
  "deal with increment 9",
  "cut -2225",
  "deal with increment 36",
  "cut -5080",
  "deal with increment 59",
  "cut -648",
  "deal with increment 64",
  "cut -1845",
  "deal into new stack",
  "cut -7726",
  "deal with increment 44",
  "cut 1015",
  "deal with increment 12",
  "cut 960",
  "deal with increment 30",
  "deal into new stack",
  "deal with increment 65",
  "deal into new stack",
  "deal with increment 27",
  "cut 6877",
  "deal with increment 5",
  "deal into new stack",
  "cut -3436",
  "deal with increment 63",
  "deal into new stack",
  "deal with increment 71",
  "deal into new stack",
  "deal with increment 7",
  "cut -9203",
  "deal with increment 38",
  "cut 9008",
  "deal with increment 59",
  "deal into new stack",
  "deal with increment 13",
  "cut 5979",
  "deal with increment 55",
  "cut 9483",
  "deal with increment 65",
  "cut -9250",
  "deal with increment 75",
  "deal into new stack",
  "cut -1830",
  "deal with increment 55",
  "deal into new stack",
  "deal with increment 67",
  "cut -8044",
  "deal into new stack",
  "cut 8271",
  "deal with increment 51",
  "cut 6002",
  "deal into new stack",
  "deal with increment 47",
  "cut 3638",
  "deal with increment 18",
  "cut -785",
  "deal with increment 63",
  "cut -2460",
  "deal with increment 25",
  "cut 5339",
  "deal with increment 61",
  "cut -5777",
  "deal with increment 54",
  "deal into new stack",
  "cut 8075",
  "deal into new stack",
  "deal with increment 22",
  "cut 3443",
  "deal with increment 34",
  "cut 5193",
  "deal with increment 3"]
