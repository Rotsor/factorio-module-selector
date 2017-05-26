{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Control.Arrow(first, second)
import Data.List
import Debug.Trace
import Data.Ord
import Text.Printf
import qualified Data.Array as Array
import Data.Array(Array, (!), range, Ix)
import qualified Data.Matrix
import qualified System.Random
import Control.Monad
import System.IO(stdout, hSetBuffering, BufferMode(..))
import Control.Parallel.Strategies
import GHC.Generics
import qualified Data.Eigen.Matrix
import Foreign.C.Types
import qualified Numeric.LinearAlgebra.HMatrix
import qualified Numeric.LinearAlgebra.Data

import Matrix

data RawMaterial =
  BuriedIronRaw
  | BuriedCopperRaw
  | PetroleumGasRaw
  | LightOilRaw
  | HeavyOilRaw
  | BuriedCoalRaw
  deriving (Eq, Ord, Enum, Bounded, Show, Ix, Generic)

instance NFData RawMaterial

type RawMaterialPressure = Map Product Rat

data Product =
  IronPlate
  | CopperPlate
  | SteelPlate
  | IronOre
  | CopperOre
  | CopperCable
  | GearWheel
  | Plastic
  | Pipe
  | ElectronicCircuit
  | AdvancedCircuit
  | EngineUnit
  | ProcessingUnit
  | ElectricMiningDrill
  | SciencePack1
  | SciencePack2
  | SciencePack3
  | SciencePackProduction
  | SpeedModule
  | SpeedModule2
  | SpeedModule3
  | ProductivityModule
  | ProductivityModule2
  | ProductivityModule3
  | EfficiencyModule
  | EfficiencyModule2
  | EfficiencyModule3
  | Sulfur
  | SulfuricAcid
  | CoalLiquefaction
  | CoalLiquefactionResearch
  | Inserter
  | TransportBelt
  | AssemblingMachine1
  | AssemblingMachine2
  | AssemblingMachine3
  | ElectricFurnace
  | ElectricEngineUnit
  | Lubricant
  | StoneBrick
  | Stone
  | Energy -- in J
  | SolidFuel
  | Steam
  | Coal
  | BuriedIron
  | BuriedCopper
  | PetroleumGas
  | LightOil
  | HeavyOil
  | BuriedCoal
  deriving (Eq, Ord, Show, Enum, Bounded, Ix, Generic)

instance NFData Product where

data Time = Time Rat deriving (Eq, Ord, Show)

data FactoryKind =
  Assembly
  | Smelter
  | Chemical
  | Miner
  | Lab
  | Boiler
  | Refinery
  deriving (Show, Eq, Ord)

data Config = Config
  { 
    configSpeedBonus :: Rat,
    configProductivityBonus :: Rat,
    configEnergyBonus :: Rat
  } deriving Show

speedMultiplier x = 1 + configSpeedBonus x
productivityMultiplier x = 1 + configProductivityBonus x
energyMultiplier x = max 0.2 (1 + configEnergyBonus x)

instance Monoid Config where
  mempty = Config 0 0 0
  a `mappend` b =
    Config
    (configSpeedBonus a + configSpeedBonus b)
    (configProductivityBonus a + configProductivityBonus b)
    (configEnergyBonus a + configEnergyBonus b)

data Usability =
  Unusable | Usable

moduleToConfig SpeedModule = Config 0.2 0 0.5
moduleToConfig SpeedModule2 = Config 0.3 0 0.6
moduleToConfig SpeedModule3 = Config 0.5 0 0.7
moduleToConfig EfficiencyModule = Config 0 0 (negate 0.3)
moduleToConfig EfficiencyModule2 = Config 0 0 (negate 0.4)
moduleToConfig EfficiencyModule3 = Config 0 0 (negate 0.5)
moduleToConfig ProductivityModule = Config (negate 0.15) 0.04 0.4
moduleToConfig ProductivityModule2 = Config (negate 0.15) 0.06 0.6
moduleToConfig ProductivityModule3 = Config (negate 0.15) 0.10 0.8

allModules :: Usability -> [([Product], Config)]
allModules usability =
  [ ([], Config 0 0 0) ] ++
  (map (\m -> ([m], moduleToConfig m)) $
  ([ SpeedModule
  , EfficiencyModule
  , EfficiencyModule2
--  , EfficiencyModule3
  , SpeedModule2
--  , SpeedModule3
  ]
  ++ case usability of
   Unusable ->
     [ ProductivityModule
     , ProductivityModule2
     , ProductivityModule3
     ]
   Usable -> []))

choose k l
  | k < 0 = []
  | k == 0 = [[]]
  | otherwise = case l of
     [] -> []
     (x : xs) -> map (x:) (choose (k-1) l) ++ choose k xs

availableConfigs :: FactoryKind -> Usability -> [([Product], Config)]
availableConfigs kind usability =
  let availableModules = allModules usability in
  map mconcat $ case kind of
    Assembly -> choose 4 availableModules
    Smelter -> choose 2 availableModules
    Chemical -> choose 3 availableModules
    Miner -> choose 3 availableModules
    Lab -> choose 2 availableModules
    Boiler -> choose 0 availableModules

-- in Watt
data Power = Power { unPower :: Rat }

basePower :: FactoryKind -> Power
basePower Assembly = Power 210e3
basePower Miner = Power 90e3
basePower Smelter = Power 180e3
basePower Chemical = Power 210e3
basePower Lab = Power 60e3
basePower Boiler = Power 0
basePower Refinery = Power 420e3

baseSpeed Assembly = 1.25 -- CR-soon: allow for blue assembly machines
baseSpeed Miner = 1 -- factored in into the recipe
baseSpeed Smelter = 2
baseSpeed Chemical = 1.25
baseSpeed Lab = 1 -- CR-someday: take upgrades into account
baseSpeed Boiler = 1 -- nothing cares about this
baseSpeed Refinery = 1

p1 = ProductivityModule
p2 = ProductivityModule2
p3 = ProductivityModule3
s1 = SpeedModule
s2 = SpeedModule2
e1 = EfficiencyModule
currentModules' SciencePackProduction = [p1, p1, p1, p1]
currentModules' SciencePack3 = [p1, p1, p1, p1]
currentModules' ElectronicCircuit = [p1, p2, p2, p2]
currentModules' GearWheel = [p1, p1, p1, s1]
currentModules' CoalLiquefaction = [p1, p1]
currentModules' ElectricEngineUnit = [s1, p1, p1, p1]
currentModules' AdvancedCircuit = [p1, p1, s1]
currentModules' Plastic = [p1]
currentModules' SciencePack2 = [p1]
currentModules' _ = []

currentModules (Recipe { recipeProducts }) =
  case recipeProducts of
    [ (product, _) ] -> currentModules' product
    _ -> []
  

currentConfig :: Recipe -> Config
currentConfig = mconcat . map moduleToConfig . currentModules

scaleTime s (Time t) = Time (s * t)

coalToEnergy coal = coal * 8e6 / 2

data Recipe = Recipe
  {
    recipeProducts :: [(Product, Rat)],
    recipeMaterials :: [(Product, Rat)],
    recipeVenue :: FactoryKind,
    recipeTime :: Time
  } deriving (Eq, Ord, Show)

energy_per_steam = 30000

recipes :: [Recipe]
recipes =
  [
    Recipe [(GearWheel, 1)]  [(IronPlate, 4)] Assembly (Time 0.5),
    Recipe [(IronPlate, 1)]  [(IronOre, 1)] Smelter (Time 3.5),
    Recipe [(CopperPlate, 1)]  [(CopperOre, 1)] Smelter (Time 3.5),
    Recipe [(SteelPlate, 1)]  [(IronPlate, 10)] Smelter (Time 35),
    Recipe [(IronOre, 1)] [(BuriedIron, 1)] Miner (Time (1/0.525)),
    Recipe [(CopperOre, 1)] [(BuriedCopper, 1)] Miner (Time (1/0.525)),
    Recipe [(Coal, 1)]  [(BuriedCoal, 1)] Miner ( Time (1/0.525)),
    Recipe [(Plastic, 2)] [(PetroleumGas, 20), (Coal, 1)] Chemical (Time 1),
    Recipe [(ElectronicCircuit, 1)]  [(CopperCable, 10), (IronPlate, 2)] Assembly ( Time 0.5),
    Recipe [(AdvancedCircuit, 1)]  [(Plastic, 4), (CopperCable, 8), (ElectronicCircuit, 2)] Assembly ( Time 6),
    Recipe [(CopperCable, 2)]  [(CopperPlate, 1)] Assembly ( Time 0.5),
    Recipe [(Pipe, 1)]  [(IronPlate, 2)] Assembly ( Time 0.5),
    Recipe [(EngineUnit, 1)]  [(GearWheel, 1), (Pipe, 2), (SteelPlate, 1)] Assembly ( Time 10),
    Recipe [(ElectricMiningDrill, 1)]  [(GearWheel, 10), (IronPlate, 20), (ElectronicCircuit, 5)] Assembly ( Time 2),
    Recipe [(SciencePack3, 1)]  [(AdvancedCircuit, 1), (ElectricMiningDrill, 1), (EngineUnit, 1)] Assembly ( Time 12),
    Recipe [(SciencePack1, 1)]  [(CopperPlate, 1), (GearWheel, 1)] Assembly ( Time 5),
    Recipe [(SciencePack2, 1)]  [(Inserter, 1), (TransportBelt, 1)] Assembly ( Time 6),
    Recipe [(SciencePackProduction, 2)]  [(AssemblingMachine1, 1), (ElectricEngineUnit, 1), (ElectricFurnace, 1)] Assembly ( Time 14),
    Recipe [(SpeedModule, 1)]  [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] Assembly ( Time 15),
    Recipe [(EfficiencyModule, 1)]  [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] Assembly ( Time 15),
    Recipe [(ProductivityModule, 1)]  [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] Assembly ( Time 15),
    Recipe [(EfficiencyModule2, 1)]  [(AdvancedCircuit, 5), (EfficiencyModule, 4), (ProcessingUnit, 5)] Assembly ( Time 30),
    Recipe [(SpeedModule2, 1)]  [(AdvancedCircuit, 5), (SpeedModule, 4), (ProcessingUnit, 5)] Assembly ( Time 30),
    Recipe [(ProductivityModule2, 1)]  [(AdvancedCircuit, 5), (ProductivityModule, 4), (ProcessingUnit, 5)] Assembly ( Time 30),
    Recipe [(ProductivityModule3, 1)]  [(AdvancedCircuit, 5), (ProductivityModule2, 5), (ProcessingUnit, 5)] Assembly ( Time 60),
    Recipe [(SpeedModule3, 1)]  [(AdvancedCircuit, 5), (SpeedModule2, 5), (ProcessingUnit, 5)] Assembly ( Time 60),
    Recipe [(EfficiencyModule3, 1)]  [(AdvancedCircuit, 5), (EfficiencyModule2, 5), (ProcessingUnit, 5)] Assembly ( Time 60),
    Recipe [(ProcessingUnit, 1)]  [(AdvancedCircuit, 2), (ElectronicCircuit, 20), (SulfuricAcid, 10)] Assembly ( Time 10),
    Recipe [(SulfuricAcid, 50)]  [(IronPlate, 1), (Sulfur, 5)] Chemical ( Time 1),
    Recipe [(Sulfur, 2)]  [(PetroleumGas, 30)] Chemical ( Time 1),
    Recipe [(CoalLiquefactionResearch, 1/800)]  [(SciencePack1, 1), (SciencePack2, 1), (SciencePack3, 1), (SciencePackProduction, 1)] Lab ( Time 30),
    Recipe [(Inserter, 1)]  [(ElectronicCircuit, 1), (IronPlate, 1), (GearWheel, 1)] Assembly ( Time 0.5),
    Recipe [(TransportBelt, 2)]  [(GearWheel, 1), (IronPlate, 1)] Assembly ( Time 0.5),
    Recipe [(AssemblingMachine1, 1)]  [(GearWheel, 5), (IronPlate, 9), (ElectronicCircuit, 3)] Assembly ( Time 0.5),
    Recipe [(AssemblingMachine2, 1)]  [(AssemblingMachine1, 1), (ElectronicCircuit, 5), (GearWheel, 10), (IronPlate, 20)] Assembly ( Time 0.5),
    Recipe [(AssemblingMachine3, 1)]  [(AssemblingMachine2, 2), (SpeedModule, 4)] Assembly ( Time 0.5),
    Recipe [(ElectricFurnace, 1)]  [(AdvancedCircuit, 5), (SteelPlate, 10), (StoneBrick, 10)] Assembly ( Time 5),
    Recipe [(ElectricEngineUnit, 1)]  [(ElectronicCircuit, 2), (EngineUnit, 1), (Lubricant, 15)] Assembly ( Time 10),
    Recipe [(StoneBrick, 1)]  [(Stone, 2)] Smelter ( Time 3.5),
    Recipe [(Stone, 1)]  [] Miner ( Time 0.65), -- incorrect components
    Recipe [(Lubricant, 10)]  [(HeavyOil, 10)] Chemical (Time 1),
    
--    Recipe [(Steam, (25e6 * 0.5) / energy_per_steam)] [(SolidFuel, 1)] Boiler (Time 1), -- incorrect time, but nothing cares
    Recipe [(Steam, (8e6 * 0.5) / energy_per_steam)] [(Coal, 1)] Boiler (Time 1), -- incorrect time, but nothing cares
    Recipe [(Energy, energy_per_steam)] [(Steam, 1)] Boiler (Time 1), -- incorrect time and venue, but nothing goes wrong
    
    Recipe [(PetroleumGas, 2)] [(LightOil, 3)] Chemical (Time 5),
    
    Recipe [(SolidFuel, 1)] [(LightOil, 10)] Chemical (Time 3),
    Recipe [(LightOil, 3)] [(HeavyOil, 4)] Chemical (Time 5),
    Recipe [(HeavyOil, 35), (LightOil, 15), (PetroleumGas, 20)] [(Coal, 10), (HeavyOil, 25), (Steam, 50)] Refinery (Time 5)
  ]

scale :: Rat -> RawMaterialPressure -> RawMaterialPressure
scale s = fmap (s*)

mconcat' x = foldr add zero x

functionToMatrix :: (Num v, Ix' b, Ix' a) => (a -> [(b, v)]) -> Matrix a b v
functionToMatrix f =
  Matrix (Array.array fullRange [ ((a, b), maybe 0 id (Map.lookup b bs)) | a <- range fullRange, let bs = Map.fromListWith (+) (f a), b <- range fullRange])

recipesToMatrix :: (Recipe -> Config) -> [Map Product Rat]
recipesToMatrix configs =
  map (\recipe@(Recipe production consumption venue (Time time)) ->
         let config = configs recipe in
           let energy = (time / (speedMultiplier config * baseSpeed venue)) * unPower (basePower venue) * energyMultiplier config in
             Map.fromListWith add (consumption ++ [(Energy, energy)] ++ map (second ((* productivityMultiplier config) . negate)) production)
         ) recipes

instance (Ix' a, Ix' b) => Enum (a, b) where
  fromEnum (x, (y :: b)) = fromEnum x * Array.rangeSize (fullRange :: (b, b)) + fromEnum y
  toEnum i =
    let (d, m) = i `divMod` (Array.rangeSize (fullRange :: (b, b))) in
      (toEnum d, (toEnum m :: b))

matrix_add (Matrix x) (Matrix y) = Matrix (Array.array fullRange [(i, x ! i + y ! i) | i <- range fullRange])
matrix_negate (Matrix x) = Matrix (Array.array fullRange [(i, - x ! i) | i <- range fullRange])

matrix_subtract a b = matrix_add a (matrix_negate b)

matrix_identity :: (Ix' a, Num v) => Matrix a a v
matrix_identity = Matrix (Array.array fullRange [((a,b), if a == b then 1 else 0) | (a,b) <- range fullRange])

to_data_matrix :: forall a v . (Ix' a) => Matrix a a v -> Data.Matrix.Matrix v
to_data_matrix (Matrix m) =
  Data.Matrix.matrix
  (Array.rangeSize (fullRange :: (a, a)))
  (Array.rangeSize (fullRange :: (a, a)))
  (\(i, j) ->
     m ! (toEnum (i - 1), toEnum (j - 1)))

of_data_matrix :: forall a v . Ix' a => Data.Matrix.Matrix v -> Matrix a a v
of_data_matrix m =
  let (all_as :: [a]) = range fullRange in
  Matrix (Array.array fullRange
          [ ((i, j), v)
          | (i, r) <- zip all_as (Data.Matrix.toLists m)
          , (j, v) <- zip all_as r])

matrix_inverse_precise :: (Ix' a, Num v, Fractional v, Eq v) => Matrix a a v -> Matrix a a v
matrix_inverse_precise x = trace "inverting" $ of_data_matrix . (\(Right res) -> res) . Data.Matrix.inverse . to_data_matrix $ x


to_hmatrix :: forall a . (Ix' a) => Matrix a a Rat -> Numeric.LinearAlgebra.HMatrix.Matrix Double
to_hmatrix (Matrix m) =
  Numeric.LinearAlgebra.Data.fromLists
   $ [
     [ fromRational . toRational $ (m ! (i, j))
       | j <- range fullRange
     ]
     | i <- range fullRange
     ]

of_hmatrix :: forall a . Ix' a => Numeric.LinearAlgebra.HMatrix.Matrix Double -> Matrix a a Rat
of_hmatrix m =
  let (all_as :: [a]) = range fullRange in
  Matrix (Array.array fullRange
          [ ((i, j), fromRational . toRational $ v)
          | (i, r) <- zip all_as (Numeric.LinearAlgebra.Data.toLists m)
          , (j, v) <- zip all_as r])

matrix_inverse_hmatrix :: (Ix' a) => Matrix a a Rat -> Matrix a a Rat
matrix_inverse_hmatrix x = of_hmatrix . Numeric.LinearAlgebra.HMatrix.inv . to_hmatrix $ x

to_eigen_matrix :: forall a . (Ix' a) => Matrix a a Rat -> Data.Eigen.Matrix.Matrix Double CDouble
to_eigen_matrix (Matrix m) =
  Data.Eigen.Matrix.fromList
   [
     [ fromRational . toRational $ (m ! (toEnum (i - 1), toEnum (j - 1)))
       | j <- range fullRange
     ]
     | i <- range fullRange
     ]

of_eigen_matrix :: forall a . Ix' a => Data.Eigen.Matrix.Matrix Double CDouble -> Matrix a a Rat
of_eigen_matrix m =
  let (all_as :: [a]) = range fullRange in
  Matrix (Array.array fullRange
          [ ((i, j), fromRational . toRational $ v)
          | (i, r) <- zip all_as (Data.Eigen.Matrix.toList m)
          , (j, v) <- zip all_as r])

matrix_inverse_eigen :: (Ix' a) => Matrix a a Rat -> Matrix a a Rat
matrix_inverse_eigen x = of_eigen_matrix . Data.Eigen.Matrix.inverse . to_eigen_matrix $ x

matrix_inverse_verified inverse x =
  let y = inverse x in
    if (matrix_mult x y == Matrix (f_array (\(x, y) -> if x == y then 1 else 0))) then y else error $ "matrix inverse broken: " ++ "\n" ++ show x ++"\n" ++ show y

matrix_inverse x = matrix_inverse_verified matrix_inverse_own x

-- x = m * x + x_0
-- solve_equation :: (Ix' a, Num v, Fractional v, Eq v) => Matrix a a v -> Vector a v -> Vector a v
-- solve_equation a x0 = matrix_inverse (matrix_subtract a matrix_identity) `matrix_mult` matrix_negate x0

sparse_transpose :: (Ord a, Ord b) => Map a (Map b v) -> Map b (Map a v)
sparse_transpose m =
  fmap
  Map.fromList
  $ Map.fromListWith (++) (concatMap (\(a, m') -> map (\(b, v) -> (b, [(a, v)])) (Map.toList m')) (Map.toList m))


matrix_to_sparse :: (Ix' a, Ix' b, Ord b, Linear v, Eq v) => Matrix a b v -> [(a, (Map b v))]
matrix_to_sparse (Matrix m) =
  [ (a, Map.fromList
    [
      (b, x)
      | b <- range fullRange
      , let x = m ! (a,b)
      , x /= zero
    ])
    | a <- range fullRange
  ]


type RawProduct = Product

solvedRecipes
  :: (Recipe -> Config)
     -> Map Product (Map RawProduct Rat)
solvedRecipes configs =
  solutionDecompose (find_kernel (/) (*) (recipesToMatrix configs))

vector_lookup :: Ix' a => Vector a v -> a -> v
vector_lookup (Matrix x) a = x ! (a, ())

compute'_new :: (Recipe -> Config) -> Product -> RawMaterialPressure
compute'_new configs =
  let recipes = solvedRecipes configs in
   \product -> case Map.lookup product recipes of
     Nothing -> Map.empty
     Just m -> m

compute_recipe config =
  let compute = compute'_new config in
    \recipe ->
      mconcat'
        [ scale quantity (compute product)
        | (product, quantity) <- recipeProducts recipe
        ]

compute' f = compute'_new f

data SparseMatrix a b v = SparseMatrix [(a, Map b v)]

data MatrixError =
  Not_enough_equations
  | Contradicting_equations
  deriving Show

instance (Linear v, Ix' a, Ix' b) => Linear (Matrix a b v) where
  zero = Matrix (f_array (\(a, b) -> zero))
  add = matrixZipWith add
  minus = fmap minus

matrix_inverse_own ::
  forall a b v .
  (Ix' a, Ix' b, Num v, Fractional v, Eq v, Linear v) => Matrix a b v -> Matrix b a v
matrix_inverse_own = lastStep . better . almostDone where
  -- specialize v2 and v3 to [Vector a v]
  -- v1 becomes [v]
  specialized_solve_equation ::
      (v -> v -> v)
      -> (v -> v -> v)
      -> (v -> Vector a v -> Vector a v)
      -> (v -> Vector a v -> Vector a v)
      -> (Vector a v -> v -> Vector a v)
      -> [(Map b v, Vector a v)] -> Either MatrixError (Map b (Vector a v))
  specialized_solve_equation = solve_equation

  better :: [(Map b v, Vector a v)] -> Either MatrixError (Map b (Vector a v))
  better = specialized_solve_equation (/) (*) (\x -> fmap (*x)) (\x -> fmap (*x)) (\v x -> fmap (/x) v)

  almostDone :: Matrix a b v -> [(Map b v, Vector a v)]
  almostDone (Matrix m) =
    [ (Map.fromList
      [
        (b, x)
        | b <- range fullRange
        , let x = m ! (a,b)
        , x /= 0
      ], Matrix (f_array (\(a', ()) -> if a == a' then 1 else 0)))
      | a <- range fullRange
    ]

  lastStep :: Either MatrixError (Map b (Vector a v)) -> Matrix b a v
  lastStep (Left e) = error $ show e
  lastStep (Right m) =
    Matrix (f_array (\(b, a) -> case Map.lookup b m of
                        Nothing -> 0
                        Just (Matrix v) ->
                          v ! (a, ())
                       ))

-- in the input missing elements assumed to be 0;
-- in the output missing elements are undefined (can be whatever)
solve_equation ::
  forall v1 v2 v3 b s.
  ( Linear v1
  , Linear v2
  , Linear v3
  , Eq b
  , Eq v2
  , Eq v1
  , Ord b)
  =>
  (v1 -> v1 -> s)
  -> (s -> v1 -> v1)
  -> (s -> v2 -> v2)
  -> (v1 -> v3 -> v2)
  -> (v2 -> v1 -> v3)
  -> [(Map b v1, v2)] -> Either MatrixError (Map b v3)
solve_equation divide mult_v1 mult_v2 mult' divide' rows = go rows where

  lookupLhs :: (Map b v1, v2) -> b -> v1
  lookupLhs (lhs, _) b = case Map.lookup b lhs of
    Nothing -> zero
    Just x -> x

  addRow (a1, b1) (a2, b2) = (Map.unionWith add a1 a2, add b1 b2)

  scaleRow (s :: s) (a1, b1) = (fmap (mult_v1 s) a1, mult_v2 s b1)

  minusRow (m, v) = (fmap minus m, minus v)
  
  remove_b :: (b, (Map b v1, v2)) -> (Map b v1, v2) -> (Map b v1, v2)
  remove_b (b, row0) row1 =
    first (\m -> if m Map.! b == zero then Map.delete b m else error "should be zero") $ addRow row1 (minusRow $ scaleRow (lookupLhs row1 b `divide` lookupLhs row0 b) row0)
  
  go :: [(Map b v1, v2)] -> Either MatrixError (Map b v3)
  go [] = Right Map.empty
  go ((row0@(lhs, rhs)) : rest) = case [(b, v) | (b, v) <- Map.toList lhs, v /= zero] of
    [] -> -- all coefficients are 0
      if rhs == zero
      then
        go rest
      else
        Left Contradicting_equations
    (chosen_b, chosen_v) : _ ->
      case go (map (remove_b (chosen_b, row0)) rest) of
        Left error -> Left error
        Right assignments ->
          case traverse (\(b, v) -> if b == chosen_b then Just zero else fmap (mult' v) (Map.lookup b assignments)) (Map.toList lhs) of
            Nothing -> Left Not_enough_equations
            Just lhs ->
              let sum_of_lhs = foldr add zero lhs in
              Right (Map.insert chosen_b (divide' (add rhs (minus sum_of_lhs)) chosen_v) assignments)

computeTotalCost :: Product -> RawMaterialPressure
computeTotalCost = compute' currentConfig


usability' GearWheel = Unusable
usability' IronPlate = Unusable
usability' IronOre = Unusable
usability' CopperOre = Unusable
usability' CopperCable = Unusable
usability' ElectronicCircuit = Unusable
usability' AdvancedCircuit = Unusable
usability' Plastic = Unusable
usability' Sulfur = Unusable
usability' SulfuricAcid = Unusable
usability' EngineUnit = Unusable
usability' CopperPlate = Unusable
usability' SteelPlate = Unusable
usability' ElectricMiningDrill = Usable
usability' Pipe = Usable
usability' Inserter = Usable
usability' ElectricFurnace = Usable
usability' SciencePack1 = Unusable
usability' SciencePack2 = Unusable
usability' SciencePack3 = Unusable
usability' AssemblingMachine1 = Usable
usability' AssemblingMachine2 = Usable
usability' AssemblingMachine3 = Usable
usability' TransportBelt = Usable
usability' StoneBrick = Unusable
usability' Lubricant = Unusable
usability' SciencePackProduction = Unusable
usability' CoalLiquefaction = Unusable
usability' ElectricEngineUnit = Unusable
usability' Stone = Unusable
usability' Coal = Unusable
usability' ProcessingUnit = Unusable
usability' SpeedModule = Usable
usability' SpeedModule2 = Usable
usability' SpeedModule3 = Usable
usability' EfficiencyModule = Usable
usability' EfficiencyModule2 = Usable
usability' EfficiencyModule3 = Usable
usability' ProductivityModule = Usable
usability' ProductivityModule2 = Usable
usability' ProductivityModule3 = Usable
usability' SolidFuel = Unusable
usability' Energy = Usable
usability' x = error $ "undefined usability: " ++ show x

usability recipe =
  case recipeProducts recipe of
    [(product, _)] -> usability' product
    x -> error $ "undefined recipe usability: " ++ show recipe

evaluateTotalCost  :: RawMaterialPressure -> Rat
evaluateTotalCost f = sum [ (estimate k * v) | (k, v) <- Map.toList f, v /= zero] where
  estimate LightOil = 0.1
  estimate HeavyOil = 0.1
  estimate BuriedCoal = 1.5
  estimate BuriedIron = 1
  estimate BuriedCopper = 1
  estimate PetroleumGas = 0.1

subtract' a b = add a (minus b)

possibleSavings :: Recipe -> [(RawMaterialPressure, RawMaterialPressure, [Product])]
possibleSavings recipe =
  let venue = recipeVenue recipe in
  let Time time = recipeTime recipe in
    [ let saving_per_unit =
            add
            (compute_recipe currentConfig recipe)
            (fmap negate $ compute_recipe (\p -> if p == recipe then config else currentConfig p) recipe)
      in
        let saving_per_second =
              scale (recip $ time / speedMultiplier config) saving_per_unit
        in
          let
            cost =
              subtract' (modulesCost modules) (scale (recip (speedMultiplier (currentConfig recipe)) * speedMultiplier config) $ modulesCost (currentModules recipe))
          in
          (saving_per_second, cost, modules)
      | (modules, config) <- availableConfigs venue (usability recipe)]

newtype Rat = Rat Rational deriving (Eq, Ord, Generic, NFData, Linear, Num, Fractional, Real)

instance Show Rat where
  show (Rat x) = printf "%.5f" (fromRational x :: Double)

showModule SpeedModule = "s1"
showModule SpeedModule2 = "s2"
showModule SpeedModule3 = "s3"
showModule EfficiencyModule = "e1"
showModule EfficiencyModule2 = "e2"
showModule EfficiencyModule3 = "e3"
showModule ProductivityModule = "p1"
showModule ProductivityModule2 = "p2"
showModule ProductivityModule3 = "p3"

divv a b = if b == 0 then 1e10 else a / b

modulesCost modules = mconcat' $ map computeTotalCost modules

possibleSavings' = sortBy (comparing (\(saving, cost, _details, _modules) -> divv saving cost)) . map (
  \(x, cost, modules) -> (evaluateTotalCost x, evaluateTotalCost cost, show x, concat $ map showModule modules)) . (`using` parList rdeepseq) . possibleSavings

ff x = x

possibleSavings'' =
  [
    (ff $ divv saving cost * 3600, (ff $ saving, ff $ cost, recipe, modules))
  | recipe <- take 2 $ recipes
  , (saving, cost, details, modules) <- possibleSavings' recipe
  , cost /= 0
  ]

possibleSavings''' =
  reverse . take 50 . dropWhile (\(_, (gain, cost, _, _)) -> gain < Rat 0 && cost < Rat 0) . reverse . sortBy (comparing fst) $ possibleSavings''  

matrix_of_lists lists =
  Matrix (Array.array fullRange
          [ ((i, j), toRational v)
          | (i, r) <- zip (range fullRange) lists
          , (j, v) <- zip (range fullRange) r])

identity_matrix :: (Ix' a) => Matrix a a Rat
identity_matrix = Matrix (f_array (\(a,b) -> if a == b then 1 else 0))

main = print $ scale (1e-4) $ computeTotalCost $ CoalLiquefactionResearch
--main = print $ length possibleSavings'''
-- main = mapM_ print $ possibleSavings'''
{-
main = do
 flip mapM_ (range fullRange :: [Product]) $ \product -> do
  let configs =
        (\p -> if p == GearWheel
          then Config {configSpeedBonus = 0, configProductivityBonus = 0, configEnergyBonus = 0}
          else currentConfig p)
  let x = compute' configs GearWheel
  let y = compute' configs GearWheel
  print $ x
  print $ y
  print $ x == y
-}


