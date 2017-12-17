{-

Copyright 2017 Thomas Tuegel

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.

-}


{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Approx (Approx (..), exact) where

import Numeric.Interval

import Numeric.Precision


data Approx a =
    Approx {
      -- | approximate value
      value :: a,
      -- | approximation uncertainty
      uncertainty :: Interval a
    }


-- | An exact value with no uncertainty, except the uncertainty inherent
-- in the representation.
exact :: Precision a => a -> Approx a
exact value = Approx {..} where uncertainty = precision value


instance (Num a, Ord a) => Eq (Approx a) where
    (==) (Approx x epsx) (Approx y epsy)
      | x < y = x + (sup epsx - inf epsy) >= y
      | otherwise = y + (sup epsy - inf epsx) >= x


instance (Fractional a, Precision a, Num a, Ord a) => Num (Approx a) where
    (+) (Approx x epsx) (Approx y epsy) =
        Approx z (hull epsz epsx + hull epsz epsy)
      where
        z = x + y
        epsz = precision z

    (-) (Approx x epsx) (Approx y epsy) =
        Approx z (hull epsz epsx - hull epsz epsy)
      where
        z = x - y
        epsz = precision z

    (*) (Approx x epsx) (Approx y epsy) =
        Approx z (hull epsz (x .* epsy) + hull epsz (y .* epsx) + epsx * epsy)
      where
        z = x * y
        epsz = precision z

    negate (Approx x epsx) =
        Approx z (hull epsz (negate epsx))
      where
        z = negate x
        epsz = precision z

    abs = snd . signumabs

    signum = fst . signumabs

    fromInteger = exact . fromInteger


(.*) :: (Num a, Ord a) => a -> Interval a -> Interval a
(.*) r x
  | r >= 0 = (r * inf x) ... (r * sup x)
  | otherwise = (r * sup x) ... (r * inf x)

{-# INLINE (.*) #-}

infixr 7 .*


signumabs
    :: (Fractional a, Num a, Ord a, Precision a)
    => Approx a
    -> (Approx a, Approx a)
signumabs (Approx x epsx) =
    (Approx s epss, Approx a epsa)
  where
    a = abs x
    s = signum x
    epsa =
      let
        lower =
            max (negate a)
            (case compare x 0 of
              GT -> inf epsx
              EQ -> 0
              LT -> negate (sup epsx))
        upper =
            max (sup (precision a))
            (case compare x 0 of
              GT -> sup epsx
              EQ -> max (abs (sup epsx)) (abs (inf epsx))
              LT -> negate (inf epsx))
      in
        lower ... upper
    epss = (epsx - s .* epsa) / (singleton a + epsa)

{-# INLINE signumabs #-}
