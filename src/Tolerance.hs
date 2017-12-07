{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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


class (Num (Tol a), Ord (Tol a)) => Tolerance a where
    type Tol a
    consistent :: Tol a -> a -> a -> Bool


class Tolerance a => Precision a where
    precision :: a -> Tol a


class Tolerance a => Accuracy a where
    accuracy :: a -> a -> Tol a


instance Tolerance Double where
    type Tol Double = Double
    consistent tol x y = abs (x - y) <= tol

instance Precision Double where
    precision x = abs x * epsilon

instance Accuracy Double where
    accuracy x y = abs (x - y)


instance Tolerance Float where
    type Tol Float = Float
    consistent tol x y = abs (x - y) <= tol

instance Precision Float where
    precision x = abs x * epsilon

instance Accuracy Float where
    accuracy x y = abs (x - y)


instance Tolerance CDouble where
    type Tol CDouble = CDouble
    consistent tol x y = abs (x - y) <= tol

instance Precision CDouble where
    precision x = abs x * epsilon

instance Accuracy CDouble where
    accuracy x y = abs (x - y)


instance Tolerance CFloat where
    type Tol CFloat = CFloat
    consistent tol x y = abs (x - y) <= tol

instance Precision CFloat where
    precision x = abs x * epsilon

instance Accuracy CFloat where
    accuracy x y = abs (x - y)


instance Tolerance a => Tolerance (Complex a) where
    type Tol (Complex a) = Tol a
    consistent tol (re0 :+ im0) (re1 :+ im1) =
        consistent tol re0 re1 && consistent tol im0 im1

instance Precision a => Precision (Complex a) where
    precision (re :+ im) = max (precision re) (precision im)

instance Accuracy a => Accuracy (Complex a) where
    accuracy (re0 :+ im0) (re1 :+ im1) =
        max (accuracy re0 re1) (accuracy im0 im1)
