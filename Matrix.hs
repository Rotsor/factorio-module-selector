{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Monad.Writer

import qualified Data.Set as Set
import Data.Set(Set)
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

data Solution b v =
  Solution
  {
    solutionFree :: Set b,
    solutionDecompose :: Map b (Map b v)
  } deriving Show

class Linear a where
  zero :: a
  add :: a -> a -> a
  minus :: a -> a

instance Linear Rational where
  zero = 0
  add = (+)
  minus x = -x

instance (Ord k, Linear v, Eq v) => Linear (Map k v) where
  zero = Map.empty
  add a b = Map.filter (/= zero) (Map.unionWith add a b)
  minus = fmap minus

find_kernel ::
  forall v1 b s.
  ( Linear v1
  , Eq b
  , Eq v1
  , Ord b)
  =>
  (v1 -> v1 -> s)
  -> (s -> v1 -> v1)
  -> [Map b v1] -> Solution b s
find_kernel divide mult_v1 rows = go rows where

  lookupLhs :: (Map b v1) -> b -> v1
  lookupLhs lhs b = case Map.lookup b lhs of
    Nothing -> zero
    Just x -> x

  addRow a1 a2 = (Map.unionWith add a1 a2)

  scaleRow (s :: s) a1 = fmap (mult_v1 s) a1

  minusRow m = fmap minus m
  
  remove_b :: (b, (Map b v1)) -> (Map b v1) -> (Map b v1)
  remove_b (b, row0) row1 =
    (\m -> if m Map.! b == zero then Map.delete b m else error "should be zero") $ addRow row1 (minusRow $ scaleRow (lookupLhs row1 b `divide` lookupLhs row0 b) row0)
  
  go :: [(Map b v1)] -> Solution b s
  go [] = Solution { solutionFree = Set.empty, solutionDecompose = Map.empty }
  go (row0 : rest) = case [(b, v) | (b, v) <- Map.toList row0, v /= zero] of
    [] -> -- all coefficients are 0, equation is useless
      go rest
    (chosen_b, chosen_v) : _ ->
      case go (map (remove_b (chosen_b, row0)) rest) of
        (Solution { solutionFree, solutionDecompose }) ->
          case runWriter $ traverse (\(b, v) ->
                           if b == chosen_b
                           then return Map.empty
                           else
                             case (Map.lookup b solutionDecompose) of
                               Nothing -> do
                                 tell [b]
                                 return (Map.singleton b v)
                               Just vs ->
                                 return (fmap (`mult_v1` v) vs)
                        ) (Map.toList row0) of
            (lhs, extra_free) ->
              let sum_of_lhs = Map.unionsWith add lhs in
              Solution {
                solutionFree = Set.union solutionFree (Set.fromList extra_free),
                solutionDecompose =
                  (Map.insert chosen_b (fmap ((`divide` chosen_v) . minus) sum_of_lhs) solutionDecompose)
              }
