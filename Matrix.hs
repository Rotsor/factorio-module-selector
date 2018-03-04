{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
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
import Control.Monad.Identity

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)




data Matrix a b v = Matrix (Array (a, b) v) deriving (Show, Eq, Ord, Functor, Generic)
instance (NFData a, NFData b, NFData v) => NFData (Matrix a b v)
type Vector a v = Matrix a () v

type Ix' a = (Ix a, Bounded a, Enum a)

instance (Ix' a, Ix' b) => Enum (a, b) where
  fromEnum (x, (y :: b)) = fromEnum x * Array.rangeSize (fullRange :: (b, b)) + fromEnum y
  toEnum i =
    let (d, m) = i `divMod` (Array.rangeSize (fullRange :: (b, b))) in
      (toEnum d, (toEnum m :: b))

instance (Linear v, Ix' a, Ix' b) => Linear (Matrix a b v) where
  zero = Matrix (f_array (\(_a, _b) -> zero))
  add = matrixZipWith add
  minus = fmap minus

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

type Solution b v = Map b (Map b v)

type Solution_with_trace a b v =
  -- decomposes as much products as it can, providing for each product:
  -- 1. how much of non-decomposable products are necessary
  -- 2. how much of input recipes ([a]) you need to craft
  Map b (Map b v, Map a v)

class Linear a where
  zero :: a
  add :: a -> a -> a
  minus :: a -> a

instance (Linear a, Linear b) => Linear (a, b) where
  zero = (zero, zero)
  add (a1, b1) (a2, b2) = (add a1 a2, add b1 b2)
  minus (a, b) = (minus a, minus b)

instance Linear Double where
  zero = 0
  add = (+)
  minus x = -x

instance Linear Rational where
  zero = 0
  add = (+)
  minus x = -x

instance VectorSpace Rational where
  type Scalar Rational = Rational
  scale = (*)

instance (Eq v, Ord k, VectorSpace v) => VectorSpace (Map k v) where
  type Scalar (Map k v) = Scalar v
  scale s = fmap (scale s)

instance (VectorSpace a, VectorSpace b, Scalar a ~ Scalar b) => VectorSpace (a, b) where
  type Scalar (a, b) = Scalar a
  scale s = (scale s *** scale s)

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
  go [] = Map.empty
  go (row0 : rest) = case [(b, v) | (b, v) <- Map.toList row0, v /= zero] of
    [] -> -- all coefficients are 0, equation is useless
      go rest
    (chosen_b, chosen_v) : _ ->
      case go (map (remove_b (chosen_b, row0)) rest) of
        solutionDecompose ->
          case map (\(b, v) ->
                           if b == chosen_b
                           then Map.empty
                           else
                             case (Map.lookup b solutionDecompose) of
                               Nothing ->
                                 Map.singleton b v
                               Just vs ->
                                 fmap (`mult_v1` v) vs
                        ) (Map.toList row0) of
            lhs ->
              let sum_of_lhs = Map.unionsWith add lhs in
              (Map.insert chosen_b (fmap ((`divide` chosen_v) . minus) sum_of_lhs) solutionDecompose)

class Linear a => VectorSpace a where
  type Scalar a :: *
  scale :: Scalar a -> a -> a

find_kernel_with_trace ::
  forall v a b s.
  ( VectorSpace v
  , Scalar v ~ v
  , Ord a
  , Fractional v
  , Eq b
  , Eq v
  , Num v
  , Ord b)
  =>
  (v -> v -> v)
  -> (v -> v -> v)
  -> Map a (Map b v) -> Solution_with_trace a b v
find_kernel_with_trace divide mult_v1 rows = go (map (\(a, row) -> (row, Map.singleton a 1)) $ Map.toList rows) where

  lookupLhs :: (Map b v, Map a v) -> b -> v
  lookupLhs (lhs, _) b = case Map.lookup b lhs of
    Nothing -> zero
    Just x -> x

  addRow = add

  scaleRow = scale

  minusRow = minus
  
  remove_b :: (b, (Map b v, Map a v)) -> (Map b v, Map a v) -> (Map b v, Map a v)
  remove_b (b, row0) row1 =
    (\(m, r) -> if Map.member b m then error "should be absent" else (m, r)) $ addRow row1 (minusRow $ scaleRow (lookupLhs row1 b `divide` lookupLhs row0 b) row0)
  
  go :: [(Map b v, Map a v)] -> Solution_with_trace a b v
  go [] = Map.empty
  go (row0 : rest) = case [(b, v) | (b, v) <- Map.toList (fst row0), v /= zero] of
    [] -> -- all coefficients are 0, equation is useless
      go rest
    (chosen_b, chosen_v) : _ -> runIdentity $ do
      row0 <- return $ scaleRow (recip chosen_v) row0
      chosen_v <- return $ ()
      rest <- return $ map (remove_b (chosen_b, row0)) rest
      (lhs, rhs) <- return $ row0
      row0 <- return $ ()
      return $
        case go rest of
          solutionDecompose ->
            case map (\(b, v) ->
                             if b == chosen_b
                             then zero
                             else
                               case Map.lookup b solutionDecompose of
                                 Nothing -> do
                                   (Map.singleton b v, zero) -- don't need any recipes to make the raw material
                                 Just results ->
                                   scale v results
                          ) (Map.toList lhs) of
              lhs ->
                let sum_of_lhs = foldr add zero lhs in
                (Map.insert chosen_b (add (minus sum_of_lhs) (zero, rhs)) solutionDecompose)

