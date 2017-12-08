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


module Approximate where

import Data.Complex ( Complex(..) )
import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( IEEE(..) )


-- | Relative uncertainty
newtype Rel a = Rel { unRel :: a }


-- | Absolute uncertainty
newtype Abs a = Abs { unAbs :: a }


class Num a => Approximate a where
    -- | Compare two values for equality up to the specified absolute
    -- uncertainty. The consistency relation is commutative,
    -- @
    --     consistent t a b === consistent t b a
    -- @
    -- but it is not transitive
    -- @
    --     consistent t a b && consistent t b c =/=> consistent t a c
    -- @
    consistent
        :: Abs a  -- ^ tolerance
        -> a
        -> a
        -> Bool

    -- | The absolute precision of the type at the given value.
    -- @absPrecision x@ defines a volume at @x@;
    -- there are no values except @x@ in that volume, i.e.
    -- the only solution of
    -- @
    --     consistent (absolute x) x y === True
    -- @
    -- is @y = x@.
    -- @
    --     absolute x === fromRelative x (relative x)
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
    fromRelative
        :: a  -- ^ value
        -> Rel a  -- ^ absolute precision
        -> Abs a  -- ^ relative precision

    -- | At the given point, convert an absolute uncertainty to a relative
    -- uncertainty.
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
    consistent (Abs tol) x y = abs (x - y) <= tol

    absolute = absoluteIEEE
    relative = relativeIEEE
    fromRelative = fromRelativeIEEE
    fromAbsolute = fromAbsoluteIEEE


instance Approximate Float where
    consistent (Abs tol) x y = abs (x - y) <= tol

    absolute = absoluteIEEE
    relative = relativeIEEE
    fromRelative = fromRelativeIEEE
    fromAbsolute = fromAbsoluteIEEE


instance Approximate CDouble where
    consistent (Abs tol) x y = abs (x - y) <= tol

    absolute = absoluteIEEE
    relative = relativeIEEE
    fromRelative = fromRelativeIEEE
    fromAbsolute = fromAbsoluteIEEE


instance Approximate CFloat where
    consistent (Abs tol) x y = abs (x - y) <= tol

    absolute = absoluteIEEE
    relative = relativeIEEE
    fromRelative = fromRelativeIEEE
    fromAbsolute = fromAbsoluteIEEE


instance (RealFloat a, Approximate a) => Approximate (Complex a) where
    consistent (Abs (ret :+ imt)) (re0 :+ im0) (re1 :+ im1) =
        consistent (Abs ret) re0 re1 && consistent (Abs imt) im0 im1

    absolute (re :+ im) =
        Abs (unAbs (absolute re) :+ unAbs (absolute im))

    relative (re :+ im) =
        Rel (unRel (relative re) :+ unRel (relative im))

    fromRelative (re :+ im) (Rel (ret :+ imt)) =
        Abs (fromRel re ret :+ fromRel im imt)
      where
        fromRel x t = unAbs (fromRelative x (Rel t))

    fromAbsolute (re :+ im) (Abs (ret :+ imt)) =
        Rel (fromAbs re ret :+ fromAbs im imt)
      where
        fromAbs x t = unRel (fromAbsolute x (Abs t))
