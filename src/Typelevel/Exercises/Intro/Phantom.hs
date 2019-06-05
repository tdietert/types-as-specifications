{-# LANGUAGE MultiParamTypeClasses #-}

module Intro.Phantom where

----------------------------------------
-- Phantom Types: Motivation
----------------------------------------

data DistanceUnit
  = Miles
  | Kilometers
  deriving (Eq)

data Distance
  = Distance
  { distanceUnit  :: DistanceUnit
  , distanceValue :: Double
  }

addDistances :: Distance -> Distance -> Either [Char] Distance
addDistances (Distance du1 dv1) (Distance du2 dv2)
  | du1 == du2 = Right
  $ Distance {distanceUnit = du1, distanceValue = dv1 + dv2}
  | otherwise = Left "Distance units unequal!"


-- | This doesn't work very well...
-- convert :: Distance -> Distance
convert :: Double -> DistanceUnit -> DistanceUnit -> Double
convert v Miles      Kilometers = v * 1.60934
convert v Kilometers Miles      = v * 0.621371
convert v _ _                   = v

----------------------------------------
-- Phantom Types: Solution
----------------------------------------

data Kilometers
data Miles

data PDistance a
  = PDistance
  { pdistanceValue :: Double
  }

addPDistances :: PDistance a -> PDistance a -> PDistance a
addPDistances (PDistance pv1) (PDistance pv2) = PDistance (pv1 + pv2)

milesToKilometers :: PDistance Miles -> PDistance Kilometers
milesToKilometers (PDistance v) = PDistance (v * 1.60934)

kilometersToMiles :: PDistance Kilometers -> PDistance Miles
kilometersToMiles (PDistance v) = PDistance (v * 0.621371)

class PConvert a b where
  pconvert :: PDistance a -> PDistance b

instance PConvert Miles Kilometers where
  pconvert = milesToKilometers

instance PConvert Kilometers Miles where
  pconvert = kilometersToMiles

-- | But we can provide _any_ type as the phantom type...
oneDistanceUnit :: PDistance Bool
oneDistanceUnit = PDistance 1.0

-- | Is this really sensible?
instance PConvert Int Bool where
  pconvert = undefined
