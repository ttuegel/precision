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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Approximate
    (
      Uncertainty(..),
      Approximate(..),
      Approx, exact, (±), value, uncertainty
    ) where

import Data.Complex ( Complex(..) )
import Data.Semigroup
import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( IEEE(..) )


-- | Absolute uncertainty
data Uncertainty a = Uncertainty { left, right :: !a }
  deriving (Eq, Functor)

instance Num a => Semigroup (Uncertainty a) where
    (<>) a b =
        Uncertainty {
          left = left a + left b,
          right = right a + right b
        }

instance Num a => Monoid (Uncertainty a) where
    mappend = (<>)
    mempty = Uncertainty { left = 0, right = 0 }


class Approximate a where
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
    absolute
        :: a  -- ^ value
        -> Uncertainty a  -- ^ absolute precision


-- Helpers for IEEE types


absoluteIEEE :: (IEEE a, Num a) => a -> Abs a
absoluteIEEE x = Abs { leftAbs = x - predIEEE x, rightAbs = succIEEE x - x }


instance Approximate Double where
    absolute = absoluteIEEE


instance Approximate Float where
    absolute = absoluteIEEE


instance Approximate CDouble where
    absolute = absoluteIEEE


instance Approximate CFloat where
    absolute = absoluteIEEE


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
        | x₁ <= x₂ = x₂ - x₁ <= right ε₁ + left ε₂
        | otherwise = x₁ - x₂ <= right ε₂ + left ε₁

instance (Approximate a, Num a) => Num (Approx a) where
    (x₁ :± ε₁) + (x₂ :± ε₂) = (x₁ + x₂) :± (ε₁ <> ε₂)

    (x₁ :± ε₁) - (x₂ :± ε₂) = (x₁ - x₂) :± (ε₁ <> ε₂)

    (x₁ :± ε₁) * (x₂ :± ε₂) =
        x :± ε
      where
        x = x₁ * x₂
        ε₁₂ = (* x₂) <$> ε₁
        ε₂₁ = (* x₁) <$> ε₂
        ε = ε₁₂ <> ε₂₁

    negate (a :± p) = negate a :± p

    abs (a :± p) = abs a :± p

    signum (a :± _) = signum a :± mempty

    fromInteger = exact . fromInteger

instance (Approximate a, Fractional a, Ord a) => Fractional (Approx a) where
    (x₁ :± ε₁) / (x₂ :± ε₂) =
        x :± ε
      where
        x = x₁ / x₂
        ε₁₂ = (/ x₂) <$> ε₁
        ε₂₁ = (* x₁) <$> ε₂
        ε = ε₁₂ <> ε₂₁

    recip (x₁ :± ε₁) =
        x :± ε
      where
        x = recip x₁
        ε = (\ε₂ -> (ε₂ * x) * x) <$> ε₁

    fromRational = exact . fromRational


-- | An exact value with no uncertainty, except the uncertainty inherent
-- in the representation.
exact :: Approximate a => a -> Approx a
exact x = x :± absolute x


(±) :: (Approximate a, Num a, Ord a) => a -> a -> Approx a
x ± ε₁ =
    -- The actual uncertainty cannot be less than the actual precision
    x :± ε
  where
    εmin = absolute x
    εabs = abs ε₁
    ε = Uncertainty {
          left = max (left εmin) εabs,
          right = max (right εmin) εabs
        }

infix 7 ±


-- | Retrieve the approximate value.
value :: Approx a -> a
value (x :± _) = x


-- | Report the approximation uncertainty.
uncertainty :: Approx a -> Uncertainty a
uncertainty (_ :± ε) = ε
