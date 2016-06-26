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

    -- | The precision of @a@. @Precision a@ must be an ordered number.
    type Estimate a

    -- | Estimate the representational precision of a value of @a@. Must be
    -- a non-negative number. @precision a > precision b@ indicates that @a@
    -- is less precise than @b@. The default implementation is suitable
    -- /unless/ @Estimate a = a@.
    precision :: a -> Estimate a
    precision a = precision (limiting a)

    -- | Estimate the value of @a@ which most limits its precision. Must be
    -- a non-negative number. For floating-point numbers in the usual
    -- representations, @limiting = abs@. For collections of imprecise values,
    -- this may be the greatest @limiting@ value of all the elements.
    --
    -- > precision a == precision (limiting a)
    limiting :: a -> Estimate a

    -- | Scale a value of @a@ by an estimated value. Must obey
    --
    -- > limiting (scale x a) = abs x * limiting a
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


instance (Floating (Estimate a), Imprecise a) => Imprecise (Complex a) where

    type Estimate (Complex a) = Estimate a

    limiting (re :+ im) = max (limiting re) (limiting im)

    scale a (re :+ im) = scale a re :+ scale a im
