{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}

module Matrix where

import Prelude
import qualified Prelude
import qualified Data.Matrix
import qualified Data.Eigen.Matrix
import Foreign.C.Types
import qualified Numeric.LinearAlgebra.HMatrix
import qualified Numeric.LinearAlgebra.Data
import Control.Parallel.Strategies
import qualified Data.Array as Array
import Data.Array(Array, (!), range, Ix)
import GHC.Generics
import Control.Arrow

import qualified Data.Map as Map
import Data.Map(Map)




data Matrix a b v = Matrix (Array (a, b) v) deriving (Show, Eq, Ord, Functor, Generic)
instance (NFData a, NFData b, NFData v) => NFData (Matrix a b v)
type Vector a v = Matrix a () v


type Ix' a = (Ix a, Bounded a, Enum a)

fullRange :: Bounded a => (a, a)
fullRange = (minBound, maxBound)


vector_of_function f = Matrix (f_array (\(x, ()) -> f x))

{-# INLINE matrix_mult #-}
matrix_mult :: (Ix' a, Ix' b, Ix' c, Num v) => Matrix a b v -> Matrix b c v -> Matrix a c v
matrix_mult (Matrix x) (Matrix y) = Matrix (Array.array fullRange [ ((a, c), sum [ x ! (a, b) * y ! (b, c) | b <- range fullRange]) | a <- range fullRange, c <- range fullRange])

{-# INLINE matrixZipWith #-}
matrixZipWith f (Matrix a) (Matrix b) =
  Matrix (f_array (\k -> f (a ! k) (b ! k)))

{-# INLINE f_array #-}
f_array :: Ix' k => (k -> v) -> Array k v
f_array f = Array.array fullRange [(k, f k) | k <- range fullRange]

