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


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Approximate where

import Data.Complex ( Complex(..) )
import Data.Semigroup
import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( IEEE(..) )


-- | Relative uncertainty
newtype Rel a = Rel { unRel :: a }
  deriving (Eq, Num, Ord)

instance Num a => Semigroup (Rel a) where
    (<>) (Rel a) (Rel b) = Rel (a + b)

instance Num a => Monoid (Rel a) where
    mappend = (<>)
    mempty = Rel 0

instance Read a => Read (Rel a) where
    readsPrec prec str =
      do
        (a, rest) <- readsPrec prec str
        pure (Rel a, rest)

instance Show a => Show (Rel a) where
    show (Rel a) = show a


-- | Absolute uncertainty
newtype Abs a = Abs { unAbs :: a }
  deriving (Eq, Num, Ord)

instance Num a => Semigroup (Abs a) where
    (<>) (Abs a) (Abs b) = Abs (a + b)

instance Num a => Monoid (Abs a) where
    mappend = (<>)
    mempty = Abs 0

instance Read a => Read (Abs a) where
    readsPrec prec str =
      do
        (a, rest) <- readsPrec prec str
        pure (Abs a, rest)

instance Show a => Show (Abs a) where
    show (Abs a) = show a


class Num a => Approximate a where
    -- | The absolute precision of the type at the given value.
    -- @absolute x@ defines a volume at @x@;
    -- there are no values except @x@ in that volume, i.e.
    -- @y = x@ is the only solution of
    -- @
    --     consistent (absolute x) x y === True
    -- @
    absolute
        :: a  -- ^ value
        -> Abs a  -- ^ absolute precision
    absolute x = fromRelative x (relative x)

    -- | The relative precision of the type at the given value.
    relative
        :: a  -- ^ value
        -> Rel a  -- ^ relative precision
    relative x = fromAbsolute x (absolute x)

    -- | At the given point, convert a relative uncertainty to an absolute
    -- uncertainty.
    -- @
    --     absolute x === fromRelative x (relative x)
    -- @
    fromRelative
        :: a  -- ^ value
        -> Rel a  -- ^ absolute precision
        -> Abs a  -- ^ relative precision

    -- | At the given point, convert an absolute uncertainty to a relative
    -- uncertainty.
    -- @
    --     relative x === fromAbsolute x (absolute x)
    -- @
    fromAbsolute
        :: a  -- ^ value
        -> Abs a  -- ^ relative precision
        -> Rel a  -- ^ absolute precision


-- Helpers for IEEE types


absoluteIEEE :: (IEEE a, Num a) => a -> Abs a
absoluteIEEE x = Abs (abs (succIEEE x - x))


relativeIEEE :: (IEEE a, Num a) => a -> Rel a
relativeIEEE 0 = Rel 1
relativeIEEE _ = Rel epsilon


fromRelativeIEEE :: (Approximate a, IEEE a, Num a) => a -> Rel a -> Abs a
fromRelativeIEEE x (Rel tol) = Abs (max (unAbs (absolute x)) (abs x * tol))


fromAbsoluteIEEE :: (Approximate a, IEEE a, Num a) => a -> Abs a -> Rel a
fromAbsoluteIEEE x (Abs tol) = Rel (min (unRel (relative x)) (tol / abs x))


instance Approximate Double where
    absolute = absoluteIEEE
    relative = relativeIEEE
    fromRelative = fromRelativeIEEE
    fromAbsolute = fromAbsoluteIEEE


instance Approximate Float where
    absolute = absoluteIEEE
    relative = relativeIEEE
    fromRelative = fromRelativeIEEE
    fromAbsolute = fromAbsoluteIEEE


instance Approximate CDouble where
    absolute = absoluteIEEE
    relative = relativeIEEE
    fromRelative = fromRelativeIEEE
    fromAbsolute = fromAbsoluteIEEE


instance Approximate CFloat where
    absolute = absoluteIEEE
    relative = relativeIEEE
    fromRelative = fromRelativeIEEE
    fromAbsolute = fromAbsoluteIEEE


data Approx a = (:±) !a !(Abs a)

infix 7 :±

instance Approximate a => Num (Approx a) where
    (a :± p) + (b :± q) = (a + b) :± (p <> q)

    (a :± p) - (b :± q) = (a - b) :± (p <> q)

    (a :± p) * (b :± q) =
        ab :± fromRelative ab (rp <> rq)
      where
        ab = a * b
        rp = fromAbsolute a p
        rq = fromAbsolute b q

    negate (a :± p) = negate a :± p

    abs (a :± p) = abs a :± p

    signum (a :± _) = signum a :± mempty

    fromInteger = approx . fromInteger


approx :: Approximate a => a -> Approx a
approx x = x :± absolute x


(±) :: Num a => a -> a -> Approx a
a ± p = a :± Abs (abs p)

infix 7 ±


-- | Compare two approximate values for equality up to the specified absolute
-- uncertainty. Consistency is commutative but not transitive:
-- @
--     consistent a b <===> consistent b a
-- @
-- but
-- @
--     consistent a b && consistent b c =/=> consistent a c
-- @
consistent :: (Num a, Ord a) => Approx a -> Approx a -> Bool
consistent (a :± p) (b :± q) = abs (a - b) <= unAbs p + unAbs q
