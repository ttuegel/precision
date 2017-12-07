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


module Tolerance where

import Data.Complex ( Complex(..) )
import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( epsilon )


class Num a => Tolerance a where
    -- | Compare two values for equality up to the specified tolerance.
    consistent
        :: a  -- ^ tolerance
        -> a
        -> a
        -> Bool

    -- | The precision at a value is the smallest representable increment.
    -- The type admits no values between @x@ and @x + precision x@
    -- or between @x - precision x@ and @x@.
    precision
        :: a  -- ^ value
        -> a  -- ^ tolerance


instance Tolerance Double where
    consistent tol x y = abs (x - y) <= tol

    precision x = max epsilon (abs x * epsilon)


instance Tolerance Float where
    consistent tol x y = abs (x - y) <= tol

    precision x = max epsilon (abs x * epsilon)


instance Tolerance CDouble where
    consistent tol x y = abs (x - y) <= tol

    precision x = max epsilon (abs x * epsilon)


instance Tolerance CFloat where
    consistent tol x y = abs (x - y) <= tol

    precision x = max epsilon (abs x * epsilon)


instance (RealFloat a, Tolerance a) => Tolerance (Complex a) where
    consistent (ret :+ imt) (re0 :+ im0) (re1 :+ im1) =
        consistent ret re0 re1 && consistent imt im0 im1

    precision (re :+ im) = precision re :+ precision im
