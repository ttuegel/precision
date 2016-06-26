{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module Imprecise ( Imprecise(..) ) where

import Data.Complex ( Complex(..) )
import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( epsilon )


-- | A class for finite-precision types.
class ( Num (Estimate a), Ord (Estimate a)
      , Imprecise (Estimate a), Estimate (Estimate a) ~ Estimate a
      ) =>
    Imprecise a where

    -- | An estimate of values of type @a@ suitable for estimating its size
    -- and precision.
    type Estimate a

    -- | Estimate the /precision/ of a value. @precision a > precision b@
    -- indicates that @a@ is less precise than @b@. The default implementation
    -- is suitable /unless/ @Estimate a = a@.
    precision :: a -> Estimate a
    precision a = precision (limiting a)

    -- | Estimate the /size/ of the precision-limiting element of a collection
    -- of values. @limiting@ must obey
    --
    -- > precision a == precision (limiting a)
    --
    -- For floating-point numbers in the usual representations,
    -- @limiting = abs@. For collections of imprecise values, this is generally
    -- the greatest @limiting@ value of all the elements.
    limiting :: a -> Estimate a

    -- | Scale a value of by an estimate. @scale@ must obey
    --
    -- > limiting (scale x a) = abs x * limiting a
    --
    -- because most error-aware algorithms assume linearity of precision.
    scale :: Estimate a -> a -> a


instance Imprecise Double where
    type Estimate Double = Double
    precision x = abs x * epsilon
    limiting = abs
    scale = (*)


instance Imprecise Float where
    type Estimate Float = Float
    precision x = abs x * epsilon
    limiting = abs
    scale = (*)


instance Imprecise CDouble where
    type Estimate CDouble = CDouble
    precision x = abs x * epsilon
    limiting = abs
    scale = (*)


instance Imprecise CFloat where
    type Estimate CFloat = CFloat
    precision x = abs x * epsilon
    limiting = abs
    scale = (*)


instance Imprecise a => Imprecise (Complex a) where
    type Estimate (Complex a) = Estimate a
    limiting (re :+ im) = max (limiting re) (limiting im)
    scale a (re :+ im) = scale a re :+ scale a im
