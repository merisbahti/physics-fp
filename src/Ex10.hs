{-# LANGUAGE RankNTypes #-}

module Ex10 where

import Control.Monad (ap)
import Ex45 (vecSum)
import Graphics.Gnuplot.Simple
import Lib (R, Time)
import Vec (Acceleration, PosVec, Vec (Vec, xComp, yComp, zComp), Velocity, aParallel, aPerp, iHat, jHat, kHat, magnitude, negateV, positionCA, speedRateChange, vec, (*^), (^*), (^+^), (^-^))

-- 10.1
v0_ = 20 *^ iHat

v1_ = 20 *^ iHat ^-^ 9.8 *^ kHat

v_ :: Double -> Vec
v_ t = 20 *^ iHat ^-^ 9.8 *^ t *^ kHat

r_ :: Double -> Vec
r_ t = (30 *^ jHat) ^+^ (20 *^ t *^ iHat) ^-^ (4.9 *^ t *^ t *^ kHat)

-- 10.2
vecIntegral :: R -> (R -> Vec) -> R -> R -> Vec
vecIntegral dx f a b = vecSum [(f x) ^* dx | x <- [a + dx / 2, a + (3 / 2) * dx .. b - (dx / 2)]]

integral dt f a b = sum [(f t) * dt | t <- [a + dt / 2, a + (3 / 2) * dt .. b - dt / 2]]

-- 10.3

projectilePos :: PosVec -> Velocity -> Time -> PosVec
projectilePos r0 vInit = positionCA r0 vInit (9.81 *^ negateV kHat)

maxHeight :: PosVec -> Velocity -> R
maxHeight pos vel = maximum . map zComp $ pos : takeWhile ((> 0) . zComp) values
  where
    step = 0.1
    values = [projectilePos pos vel t | t <- [step, step * 2 .. 1000]]

-- 10.4
speedCA :: Velocity -> Acceleration -> Time -> R
speedCA vel acc t = magnitude $ vel ^+^ (acc ^* t)

-- 10.5
projectileVel :: Velocity -> Acceleration -> Time -> PosVec
projectileVel v0 = positionCA v0 (9.81 *^ negateV kHat)

-- 10.6
data Vec2d = Vec2d
  { xComp2d :: R,
    yComp2d :: R
  }
  deriving (Eq, Show)

magAngleFromVec2d :: Vec2d -> (R, R)
magAngleFromVec2d (Vec2d x y) = (sqrt $ x ** 2 + y ** 2, atan2 y x)

vec2dFromMagAngle :: R -> R -> Vec2d
vec2dFromMagAngle mag angle = Vec2d (mag * sin angle) (mag * cos angle)

-- 10.7

xyProj :: Vec -> Vec
xyProj (Vec x y _) = vec x y 0

-- 10.8
magAngles :: Vec -> (R, R, R)
magAngles (Vec x y z) =
  ( sqrt $ x ** 2 + y ** 2 + z ** 2,
    atan2 (sqrt $ x ** 2 + y ** 2) z,
    atan2 y x
  )

-- 10.9

gEarth = Vec 0 0 (-9.8)

-- Inital speed 25 m/s
-- 52 degrees from horizontal
vBallX = 0

vBallY = 25 * cos (52 * pi / 180)

vBallZ = 25 * sin (52 * pi / 180)

vBall0 = Vec vBallX vBallY vBallZ

vBall :: R -> Vec
vBall t = (vBall0 ^* t) ^+^ ((1 / 2) *^ gEarth ^* (t ** 2))

speedRateChangeBall :: R -> R
speedRateChangeBall t = speedRateChange (vBall t) gEarth

solved = plotFunc [] (linearScale 1000 (0, 50 :: Double)) speedRateChangeBall

vBallGraph = plotFunc [] (linearScale 1000 (0, 10 :: Double)) (zComp . vBall)

-- At around 2 secs the speedRateChange is -5

-- 10.10

rUCM :: R -> R -> R -> Vec
rUCM r omega t = r *^ (cos (omega * t) *^ iHat ^+^ sin (omega * t) *^ jHat)

vUCM :: R -> R -> R -> Vec
vUCM r omega t = omega *^ r *^ ((-1) *^ sin (omega * t) *^ iHat ^+^ cos (omega * t) *^ jHat)

aUCM :: R -> R -> R -> Vec
aUCM r omega t = (-1) *^ omega *^ omega *^ r *^ (cos (omega * t) *^ iHat ^+^ sin (omega * t) *^ jHat)

particlePositiongraphX = plotFunc [] (linearScale 1000 (0, 2 :: Double)) (xComp . rUCM 2 6)

particlePositiongraphY = plotFunc [] (linearScale 1000 (0, 2 :: Double)) (yComp . rUCM 2 6)

particlePositiongraphZ = plotFunc [] (linearScale 1000 (0, 2 :: Double)) (zComp . rUCM 2 6)

aParallel' = aParallel

-- returns 0
test1 = aParallel (vUCM 2 6 3) (aUCM 2 6 3)

-- returns 2 * 6^2
test2 = aPerp (vUCM 2 6 0) (aUCM 2 6 0)