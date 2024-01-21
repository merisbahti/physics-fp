module Vec (Vec (..), vec, iHat, jHat, kHat, zeroV, negateV, (^+^), (^-^), (^*), (*^), (^/), (<.>), (><), magnitude, Velocity, Acceleration, PosVec, positionCA, velocityCA, speedRateChange, aParallel, aPerp) where

import Lib (R, Time)

data Vec = Vec
  { xComp :: R,
    yComp :: R,
    zComp :: R
  }
  deriving (Eq)

vec :: R -> R -> R -> Vec
vec = Vec

instance Show Vec where
  show (Vec x y z) = "vec " ++ showDouble x ++ " " ++ showDouble y ++ " " ++ showDouble z

showDouble :: R -> String
showDouble x
  | x < 0 = "(" ++ show x ++ ")"
  | otherwise = show x

infixl 6 ^+^, ^-^

infixr 7 *^, ^*, ^/, <.>, ><

iHat :: Vec
iHat = vec 1 0 0

jHat :: Vec
jHat = vec 0 1 0

kHat :: Vec
kHat = vec 0 0 1

zeroV :: Vec
zeroV = vec 0 0 0

negateV :: Vec -> Vec
negateV (Vec x y z) = vec (-x) (-y) (-z)

(^+^) :: Vec -> Vec -> Vec
(Vec x1 y1 z1) ^+^ (Vec x2 y2 z2) = vec (x1 + x2) (y1 + y2) (z1 + z2)

(^-^) :: Vec -> Vec -> Vec
(Vec x1 y1 z1) ^-^ (Vec x2 y2 z2) = vec (x1 - x2) (y1 - y2) (z1 - z2)

(^*) :: Vec -> R -> Vec
(Vec x y z) ^* a = vec (a * x) (a * y) (a * z)

(*^) :: R -> Vec -> Vec
a *^ (Vec x y z) = vec (a * x) (a * y) (a * z)

(^/) :: Vec -> R -> Vec
(Vec x y z) ^/ a = vec (x / a) (y / a) (z / a)

(<.>) :: Vec -> Vec -> R
(Vec x1 y1 z1) <.> (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

(><) :: Vec -> Vec -> Vec
(Vec ax ay az) >< (Vec bx by bz) = Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - bx * ay)

magnitude :: Vec -> R
magnitude v = sqrt $ v <.> v

type Acceleration = Vec

type Velocity = Vec

type PosVec = Vec

positionCA :: PosVec -> Velocity -> Acceleration -> Time -> PosVec
positionCA r0 vInit a t = r0 ^+^ (vInit ^* t) ^+^ (0.5 *^ a ^* (t * t))

velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = v0 ^+^ (a0 ^* t)

speedRateChange :: Vec -> Vec -> R
speedRateChange v a = (v <.> a) / magnitude v

aParallel :: Vec -> Vec -> Vec
aParallel v a =
  let vHat = v ^/ magnitude v
   in (vHat <.> a)
        *^ vHat

aPerp :: Vec -> Vec -> Vec
aPerp v a = a ^-^ aParallel v a