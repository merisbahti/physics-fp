{-# LANGUAGE RankNTypes #-}

module Ex10 where

import Ex45 (vecSum)
import Lib (R, Time)
import Vec (Acceleration, PosVec, Vec (zComp), Velocity, iHat, jHat, kHat, magnitude, negateV, positionCA, (*^), (^*), (^+^), (^-^))

-- 10.1
v0 = 20 *^ iHat

v1 = 20 *^ iHat ^-^ 9.8 *^ kHat

v :: Double -> Vec
v t = 20 *^ iHat ^-^ 9.8 *^ t *^ kHat

r :: Double -> Vec
r t = (30 *^ jHat) ^+^ (20 *^ t *^ iHat) ^-^ (4.9 *^ t *^ t *^ kHat)

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
