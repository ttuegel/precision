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

module Numeric.Precision (Precision (..), Interval) where

import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( IEEE(..) )
import Numeric.Interval


-- | Values with finite precision.
class Ord a => Precision a where
    -- | @precision x@ is an interval in @a@ such that
    -- @
    --     min x (x + inf eps)  ==  x
    -- @
    -- and
    -- @
    --     max x (x + sup eps)  ==  x
    -- @
    precision
        :: a  -- ^ value
        -> Interval a  -- ^ absolute precision


-- | Note: Assumes default IEEE 754 rounding mode
-- (round to nearest, ties to even).
precisionIEEE :: IEEE r => r -> Interval r
precisionIEEE x
  | even signif =
      (dxl / 2) ... (dxu / 2)
  | otherwise =
      succIEEE (dxl / 2) ... predIEEE (dxu / 2)
  where
    dxl = predIEEE x - x
    dxu = succIEEE x - x
    (signif, _) = decodeFloat x

{-# INLINE precisionIEEE #-}


instance Precision Double where
    precision = precisionIEEE


instance Precision Float where
    precision = precisionIEEE


instance Precision CDouble where
    precision = precisionIEEE


instance Precision CFloat where
    precision = precisionIEEE

