module Main
  ( main
  ) where

type Int3 = (Int, Int, Int)

type Moons = ((Int3, Int3), (Int3, Int3), (Int3, Int3), (Int3, Int3))

test :: Moons
test =
  ( ((-1, 0, 2), (0, 0, 0))
  , ((2, -10, -7), (0, 0, 0))
  , ((4, -8, 8), (0, 0, 0))
  , ((3, 5, -1), (0, 0, 0)))

test2 :: Moons
test2 =
  ( ((-8, -10, 0), (0, 0, 0))
  , ((5, 5, 10), (0, 0, 0))
  , ((2, -7, 3), (0, 0, 0))
  , ((9, -8, -3), (0, 0, 0)))

input :: Moons
input =
  ( ((-9, 10, -1), (0, 0, 0))
  , ((-14, -8, 14), (0, 0, 0))
  , ((1, 5, 6), (0, 0, 0))
  , ((-19, 7, 8), (0, 0, 0)))

add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add (a, b, c) (x, y, z) = (a + x, b + y, c + z)

addVelocity :: (Int3, Int3) -> (Int3, Int3) -> (Int3, Int3)
addVelocity (l, (a, b, c)) (r, (x, y, z)) = (l, (a + x, b + y, c + z))

gravityOnPos :: Int -> Int -> Int
gravityOnPos a b
  | a < b = 1
  | a > b = -1
  | a == b = 0

singleGravity :: Int3 -> Int3 -> Int3
singleGravity (a, b, c) (x, y, z) =
  (gravityOnPos a x, gravityOnPos b y, gravityOnPos c z)

applyVelocity :: (Int3, Int3) -> (Int3, Int3)
applyVelocity (pos, vel) = (add pos vel, vel)

gravity :: Moons -> Moons
gravity ((a, av), (b, bv), (c, cv), (d, dv)) =
  ( ( a
    , av `add` (singleGravity a b) `add` (singleGravity a c) `add`
      (singleGravity a d))
  , ( b
    , bv `add` (singleGravity b a) `add` (singleGravity b c) `add`
      (singleGravity b d))
  , ( c
    , cv `add` (singleGravity c a) `add` (singleGravity c b) `add`
      (singleGravity c d))
  , ( d
    , dv `add` (singleGravity d a) `add` (singleGravity d b) `add`
      (singleGravity d c)))

velocity :: Moons -> Moons
velocity (a, b, c, d) =
  (applyVelocity a, applyVelocity b, applyVelocity c, applyVelocity d)

simulate :: Int -> Moons -> Moons
simulate 0 moons = moons
simulate n moons = simulate (n - 1) (velocity . gravity $ moons)

potentialEnergy :: (Int3, Int3) -> Int
potentialEnergy ((a, b, c), _) = abs a + abs b + abs c

kinematicEnergy :: (Int3, Int3) -> Int
kinematicEnergy (_, (a, b, c)) = abs a + abs b + abs c

totalEnergy :: (Int3, Int3) -> Int
totalEnergy x = kinematicEnergy x * potentialEnergy x

energyInSystem :: Moons -> Int
energyInSystem (a, b, c, d) =
  totalEnergy a + totalEnergy b + totalEnergy c + totalEnergy d

part1 = energyInSystem $ simulate 1000 input

------------
type MoonsOnPos = ((Int, Int), (Int, Int), (Int, Int), (Int, Int))

gp :: MoonsOnPos -> MoonsOnPos
gp ((a, av), (b, bv), (c, cv), (d, dv)) =
  ( (a, av + (gravityOnPos a b) + (gravityOnPos a c) + (gravityOnPos a d))
  , (b, bv + (gravityOnPos b a) + (gravityOnPos b c) + (gravityOnPos b d))
  , (c, cv + (gravityOnPos c a) + (gravityOnPos c b) + (gravityOnPos c d))
  , (d, dv + (gravityOnPos d a) + (gravityOnPos d b) + (gravityOnPos d c)))

vp :: MoonsOnPos -> MoonsOnPos
vp ((a, av), (b, bv), (c, cv), (d, dv)) =
  ((a + av, av), (b + bv, bv), (c + cv, cv), (d + dv, dv))

-- each dimension separately
findCycle :: MoonsOnPos -> Int
findCycle m = f m (vp . gp $ m) 1
  where
    f st moons i
      | st == moons = i
      | otherwise = f st (vp . gp $ moons) (i + 1)

part2 :: Moons -> Int
part2 (((ax, ay, az), (axv, ayv, azv)), ((bx, by, bz), (bxv, byv, bzv)), ((cx, cy, cz), (cxv, cyv, czv)), ((dx, dy, dz), (dxv, dyv, dzv))) =
  let
    cyclex = findCycle ((ax, axv), (bx, bxv), (cx, cxv), (dx, dxv))
    cycley = findCycle ((ay, ayv), (by, byv), (cy, cyv), (dy, dyv))
    cyclez = findCycle ((az, azv), (bz, bzv), (cz, czv), (dz, dzv))
  in
    lcm (lcm cyclex cycley) cyclez

main :: IO ()
main = do
  print $ part1
