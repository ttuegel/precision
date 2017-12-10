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

module Approximate
    (
      Uncertainty, toUncertainty, leftUncertainty, rightUncertainty,
      Precision(..),
      Approx, exact, (±), value, uncertainty
    ) where

import Data.Semigroup
import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( IEEE(..) )


-- | Absolute uncertainty
data Uncertainty a = Uncertainty { leftUncertainty, rightUncertainty :: !a }
  deriving (Eq)

toUncertainty :: Num a => a -> a -> Uncertainty a
toUncertainty el er = Uncertainty (abs el) (abs er)

instance Num a => Num (Uncertainty a) where
    (+) (Uncertainty al ar) (Uncertainty bl br) =
        Uncertainty (al + bl) (ar + br)

    (*) (Uncertainty al ar) (Uncertainty bl br) =
        Uncertainty (al * bl) (ar * br)

    negate ε = Uncertainty (rightUncertainty ε) (leftUncertainty ε)

    abs = id

    signum _ = Uncertainty 1 1

    fromInteger i = Uncertainty x x
      where
        x = abs (fromInteger i)


(*.) :: (Num a, Ord a) => Uncertainty a -> a -> Uncertainty a
(*.) ε r
    | r < 0 = Uncertainty εr εl
    | otherwise = Uncertainty εl εr
  where
    rabs = abs r
    εl = leftUncertainty ε * rabs
    εr = rightUncertainty ε * rabs

infixl 7 *.


(/.) :: (Fractional a, Num a, Ord a) => Uncertainty a -> a -> Uncertainty a
(/.) ε r
    | r < 0 = Uncertainty εr εl
    | otherwise = Uncertainty εl εr
  where
    rabs = abs r
    εl = leftUncertainty ε / rabs
    εr = rightUncertainty ε / rabs

infixl 7 /.


class Precision a where
    -- | The absolute precision of the type at the given value.
    -- @absolute x@ defines a volume at @x@;
    -- there are no values except @x@ in that volume, i.e.
    -- @y = x@ is the only solution of
    -- @
    --     consistent (x ± absolute x) (y ± mempty) === True
    -- @
    -- @
    --     absolute x === fromRelative x (relative x)
    -- @
    precision
        :: a  -- ^ value
        -> Uncertainty a  -- ^ absolute precision


-- Helpers for IEEE types


precisionIEEE :: (IEEE a, Num a) => a -> Uncertainty a
precisionIEEE x = toUncertainty (x - predIEEE x) (succIEEE x - x)


instance Precision Double where
    precision = precisionIEEE


instance Precision Float where
    precision = precisionIEEE


instance Precision CDouble where
    precision = precisionIEEE


instance Precision CFloat where
    precision = precisionIEEE


data Approx a = (:±) !a !(Uncertainty a)

infix 7 :±

-- | Compare two approximate values for equality up to the specified absolute
-- uncertainty. Equality is commutative but not transitive:
-- @
--     a == b  <===>  b == a
-- @
-- but
-- @
--     a == b && b == c  =/=>  a == c
-- @
instance (Num a, Ord a) => Eq (Approx a) where
    (==) (x₁ :± ε₁) (x₂ :± ε₂)
        | x₁ <= x₂  = x₂ - x₁ <= rightUncertainty ε₁ + leftUncertainty ε₂
        | otherwise = x₁ - x₂ <= rightUncertainty ε₂ + leftUncertainty ε₁

instance (Precision a, Num a, Ord a) => Num (Approx a) where
    (x₁ :± ε₁) + (x₂ :± ε₂) = (x₁ + x₂) :± (ε₁ + ε₂)

    (x₁ :± ε₁) - (x₂ :± ε₂) = (x₁ - x₂) :± (ε₁ - ε₂)

    (x₁ :± ε₁) * (x₂ :± ε₂) = (x₁ * x₂) :± (ε₁ *. x₂ + ε₂ *. x₁ + ε₁ * ε₂)

    negate (a :± p) = negate a :± p

    abs (a :± p) = abs a :± p

    signum (a :± _) = signum a :± 0

    fromInteger = exact . fromInteger

-- | Note: @recip x@ is more accurate that @1 / x@.
instance (Precision a, Fractional a, Ord a) => Fractional (Approx a) where
    (x₁ :± ε₁) / (x₂ :± ε₂) =
        x :± ε
      where
        x = x₁ / x₂
        ε = (ε₁ - ε₂ *. x - (ε₁ * ε₂) /. x₂) /. x₂

    recip (x₁ :± ε₁) =
        x :± ε
      where
        x = recip x₁
        ε = negate ε₁ *. x *. x

    fromRational = exact . fromRational


-- | An exact value with no uncertainty, except the uncertainty inherent
-- in the representation.
exact :: Precision a => a -> Approx a
exact x = x :± precision x


(±) :: (Precision a, Num a, Ord a) => a -> a -> Approx a
x ± ε₁ =
    -- The actual uncertainty cannot be less than the actual precision
    x :± ε
  where
    εmin = precision x
    εabs = abs ε₁
    ε = Uncertainty {
          leftUncertainty = max (leftUncertainty εmin) εabs,
          rightUncertainty = max (rightUncertainty εmin) εabs
        }

infix 7 ±


-- | Retrieve the approximate value.
value :: Approx a -> a
value (x :± _) = x


-- | Report the approximation uncertainty.
uncertainty :: Approx a -> Uncertainty a
uncertainty (_ :± ε) = ε
