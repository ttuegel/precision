{-# LANGUAGE TypeFamilies #-}

module Numeric.Errors
    ( Error
    , RoundingError(..), TruncationError(..)
    ) where

import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( epsilon )


-- | The scalar error estimator of a type.
type family Error a


class RoundingError a where
    -- | Estimate the rounding error in a value.
    roundingError :: a -> Error a


class TruncationError a where
    -- | Given two consecutive terms of a sequence, estimate the error
    -- introduced by truncating the sequence.
    truncationError :: a -> a -> Error a


type instance Error Double = Double

instance RoundingError Double where
    roundingError x = abs x * epsilon

instance TruncationError Double where
    truncationError x y = abs (x - y)


type instance Error Float = Float

instance RoundingError Float where
    roundingError x = abs x * epsilon

instance TruncationError Float where
    truncationError x y = abs (x - y)


type instance Error CDouble = CDouble

instance RoundingError CDouble where
    roundingError x = abs x * epsilon

instance TruncationError CDouble where
    truncationError x y = abs (x - y)


type instance Error CFloat = CFloat

instance RoundingError CFloat where
    roundingError x = abs x * epsilon

instance TruncationError CFloat where
    truncationError x y = abs (x - y)
