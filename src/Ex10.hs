{-# LANGUAGE RankNTypes #-}

module Ex10 where

import Ex45 (vecSum)
import Lib (R, Time)
import Vec (Acceleration, PosVec, Vec (zComp), Velocity, iHat, jHat, kHat, magnitude, negateV, positionCA, (*^), (^*), (^+^), (^-^))

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
  { xComp :: R,
    yComp :: R
  }
  deriving (Eq, Show)

magAngleFromVec2d :: Vec2d -> (R, R)
magAngleFromVec2d (Vec2d x y) = (sqrt $ x ** 2 + y ** 2, atan2 y x)

vec2dFromMagAngle :: R -> R -> Vec2d
vec2dFromMagAngle mag angle = Vec2d (mag * sin angle) (mag * cos angle)
