{-# LANGUAGE TypeFamilies #-}
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
import Control.Arrow(first, second, (&&&))
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
  | PiercingRoundMagazine
  | FirearmRoundMagazine
  | Grenade
  | GunTurret
  | SciencePack1
  | SciencePack2
  | SciencePack3
  | SciencePackMilitary
  | SciencePackProduction
  | SciencePackHighTech
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
  | ResearchCoalLiquefaction
  | ResearchLaserTurretDamage5
  | ResearchRocketSilo
  | ResearchNuclearPower
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
  | LaserTurret
  | Battery
  
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

newtype Time = Time { unTime :: Rat } deriving (Eq, Ord, Show, NFData)

instance Linear Time where
  zero = Time 0
  add (Time x) (Time y) = Time (add x y)
  minus (Time x) = Time (minus x)

instance VectorSpace Time where
  type Scalar Time = Rat
  scale x (Time t) = Time (x * t)

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
    configEnergyBonus :: Rat,
    configSpeedPreMultiplier :: Rat,
    configEnergyPreMultiplier :: Rat
  } deriving Show

speedMultiplier x = configSpeedPreMultiplier x * (1 + configSpeedBonus x)
productivityMultiplier x = 1 + configProductivityBonus x
energyMultiplier x = configEnergyPreMultiplier x * max 0.2 (1 + configEnergyBonus x)

instance Monoid Config where
  mempty = Config 0 0 0 1 1
  a `mappend` b =
    Config
    (configSpeedBonus a + configSpeedBonus b)
    (configProductivityBonus a + configProductivityBonus b)
    (configEnergyBonus a + configEnergyBonus b)
    (configSpeedPreMultiplier a * configSpeedPreMultiplier b)
    (configEnergyPreMultiplier a * configEnergyPreMultiplier b)

data Usability =
  Unusable | Usable

moduleToConfig SpeedModule = Config 0.2 0 0.5 1 1
moduleToConfig SpeedModule2 = Config 0.3 0 0.6 1 1
moduleToConfig SpeedModule3 = Config 0.5 0 0.7 1 1
moduleToConfig EfficiencyModule = Config 0 0 (negate 0.3) 1 1
moduleToConfig EfficiencyModule2 = Config 0 0 (negate 0.4) 1 1
moduleToConfig EfficiencyModule3 = Config 0 0 (negate 0.5) 1 1
moduleToConfig ProductivityModule = Config (negate 0.15) 0.04 0.4 1 1
moduleToConfig ProductivityModule2 = Config (negate 0.15) 0.06 0.6 1 1
moduleToConfig ProductivityModule3 = Config (negate 0.15) 0.10 0.8 1 1
moduleToConfig AssemblingMachine3 = Config 0 0 0 (1.25/0.75) (210/150)

allModules :: Usability -> [([Product], Config)]
allModules usability =
  [ ([], Config 0 0 0 1 1) ] ++
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
    Assembly ->
      map (([AssemblingMachine3], moduleToConfig AssemblingMachine3):) (choose 4 availableModules)
      ++ choose 2 availableModules
    Smelter -> choose 2 availableModules
    Chemical -> choose 3 availableModules
    Miner -> choose 3 availableModules
    Lab -> choose 2 availableModules
    Boiler -> choose 0 availableModules
    Refinery -> choose 3 availableModules

-- in Watt
data Power = Power { unPower :: Rat }

basePower :: FactoryKind -> Power
basePower Assembly = Power 150e3
basePower Miner = Power 90e3
basePower Smelter = Power 180e3
basePower Chemical = Power 210e3
basePower Lab = Power 60e3
basePower Boiler = Power 0
basePower Refinery = Power 420e3


labUpgrades = 1.5 -- +20% +30%

baseSpeed Assembly = 0.75 -- blue by default, upgraded with a pseudomodule
baseSpeed Miner = 1 -- factored in into the recipe -- CR-someday: take productivity upgrades into account
baseSpeed Smelter = 2
baseSpeed Chemical = 1.25
baseSpeed Lab = labUpgrades -- CR-someday: take upgrades into account
baseSpeed Boiler = 1 -- this is wrong; the only thing that depends on this is the thing that reports the number of boilers
baseSpeed Refinery = 1

currentConfig :: Recipe -> Config
currentConfig = mconcat . map moduleToConfig . currentModules

scaleTime s (Time t) = Time (s * t)

coalToEnergy coal = coal * 8e6 / 2

data Recipe = Recipe
  {
    recipeName :: RecipeName,
    recipeProducts :: [(Product, Rat)],
    recipeMaterials :: [(Product, Rat)],
    recipeVenue :: FactoryKind,
    recipeTime :: Time
  } deriving (Eq, Ord, Show)

data RecipeName =
  ProductRecipe Product
  | LiquefactionRecipe
  | BurnRecipe Product
  deriving (Eq, Ord)

instance Show RecipeName where
  show (ProductRecipe product) = show product
  show (LiquefactionRecipe) = "Liquefaction"
  show (BurnRecipe product) = "Burn" ++ show product

energy_per_steam = 30000

recipes :: [Recipe]
recipes =
  [
    r GearWheel 1 [(IronPlate, 4)] Assembly (Time 0.5),
    r IronPlate 1 [(IronOre, 1)] Smelter (Time 3.5),
    r CopperPlate 1 [(CopperOre, 1)] Smelter (Time 3.5),
    r SteelPlate 1 [(IronPlate, 10)] Smelter (Time 35),
    r IronOre 1[(BuriedIron, 1)] Miner (Time (1/0.525)),
    r CopperOre 1[(BuriedCopper, 1)] Miner (Time (1/0.525)),
    r Coal 1 [(BuriedCoal, 1)] Miner ( Time (1/0.525)),
    r Plastic 2[(PetroleumGas, 20), (Coal, 1)] Chemical (Time 1),
    r ElectronicCircuit 1 [(CopperCable, 10), (IronPlate, 2)] Assembly (Time 0.5),
    r AdvancedCircuit 1 [(Plastic, 4), (CopperCable, 8), (ElectronicCircuit, 2)] Assembly (Time 6),
    r CopperCable 2 [(CopperPlate, 1)] Assembly (Time 0.5),
    r Pipe 1 [(IronPlate, 2)] Assembly (Time 0.5),
    r EngineUnit 1 [(GearWheel, 1), (Pipe, 2), (SteelPlate, 1)] Assembly (Time 10),
    r ElectricMiningDrill 1 [(GearWheel, 10), (IronPlate, 20), (ElectronicCircuit, 5)] Assembly (Time 2),
    r SciencePack3 1 [(AdvancedCircuit, 1), (ElectricMiningDrill, 1), (EngineUnit, 1)] Assembly (Time 12),
    r SciencePack1 1 [(CopperPlate, 1), (GearWheel, 1)] Assembly (Time 5),
    r SciencePack2 1 [(Inserter, 1), (TransportBelt, 1)] Assembly (Time 6),
    r SciencePackProduction 2 [(ElectricEngineUnit, 1), (ElectricFurnace, 1)] Assembly (Time 14),
    r PiercingRoundMagazine 1 [(FirearmRoundMagazine, 1), (SteelPlate, 1), (CopperPlate, 5)] Assembly (Time 3),
    r FirearmRoundMagazine 1 [(IronPlate, 4)] Assembly (Time 1),
    r Grenade 1 [(IronPlate, 5), (Coal, 10)] Assembly (Time 8),
    r GunTurret 1[(GearWheel, 10), (CopperPlate, 10), (IronPlate, 20)] Assembly (Time 8),
    r SciencePackMilitary 2 [(PiercingRoundMagazine, 1), (Grenade, 1), (GunTurret, 1)] Assembly (Time 10),
    r SpeedModule 1 [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] Assembly (Time 15),
    r EfficiencyModule 1 [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] Assembly (Time 15),
    r ProductivityModule 1 [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] Assembly (Time 15),
    r EfficiencyModule2 1 [(AdvancedCircuit, 5), (EfficiencyModule, 4), (ProcessingUnit, 5)] Assembly (Time 30),
    r SpeedModule2 1 [(AdvancedCircuit, 5), (SpeedModule, 4), (ProcessingUnit, 5)] Assembly (Time 30),
    r ProductivityModule2 1 [(AdvancedCircuit, 5), (ProductivityModule, 4), (ProcessingUnit, 5)] Assembly (Time 30),
    r ProductivityModule3 1 [(AdvancedCircuit, 5), (ProductivityModule2, 5), (ProcessingUnit, 5)] Assembly (Time 60),
    r SpeedModule3 1 [(AdvancedCircuit, 5), (SpeedModule2, 5), (ProcessingUnit, 5)] Assembly (Time 60),
    r EfficiencyModule3 1 [(AdvancedCircuit, 5), (EfficiencyModule2, 5), (ProcessingUnit, 5)] Assembly (Time 60),
    r ProcessingUnit 1 [(AdvancedCircuit, 2), (ElectronicCircuit, 20), (SulfuricAcid, 10)] Assembly (Time 10),
    r SulfuricAcid 50 [(IronPlate, 1), (Sulfur, 5)] Chemical (Time 1),
    r Sulfur 2 [(PetroleumGas, 30)] Chemical (Time 1),
    r ResearchCoalLiquefaction (1/800) [(SciencePack1, 1), (SciencePack2, 1), (SciencePack3, 1), (SciencePackProduction, 1)] Lab (Time 30),
    r ResearchNuclearPower (1/4000) [(SciencePack1, 1), (SciencePack2, 1), (SciencePack3, 1)] Lab (Time 30),
    r ResearchLaserTurretDamage5 (1/800)
      [(SciencePack1, 1), (SciencePack2, 1), (SciencePack3, 1), (SciencePackProduction, 1), (SciencePackHighTech, 1)] Lab (Time 60),
    r ResearchRocketSilo (1/4000)
      [ (SciencePack1, 1)
      , (SciencePack2, 1)
      , (SciencePack3, 1)
      , (SciencePackProduction, 1)
      , (SciencePackHighTech, 1)
      , (SciencePackMilitary, 1)] Lab (Time 60),
    r Inserter 1 [(ElectronicCircuit, 1), (IronPlate, 1), (GearWheel, 1)] Assembly (Time 0.5),
    r TransportBelt 2 [(GearWheel, 1), (IronPlate, 1)] Assembly (Time 0.5),
    r AssemblingMachine1 1 [(GearWheel, 5), (IronPlate, 9), (ElectronicCircuit, 3)] Assembly (Time 0.5),
    r AssemblingMachine2 1 [(AssemblingMachine1, 1), (ElectronicCircuit, 5), (GearWheel, 10), (IronPlate, 20)] Assembly (Time 0.5),
    r AssemblingMachine3 1 [(AssemblingMachine2, 2), (SpeedModule, 4)] Assembly (Time 0.5),
    r ElectricFurnace 1 [(AdvancedCircuit, 5), (SteelPlate, 10), (StoneBrick, 10)] Assembly (Time 5),
    r ElectricEngineUnit 1 [(ElectronicCircuit, 2), (EngineUnit, 1), (Lubricant, 15)] Assembly (Time 10),
    r StoneBrick 1 [(Stone, 2)] Smelter (Time 3.5),
    r Stone 1 [] Miner (Time 0.65), -- incorrect components
    r Lubricant 10 [(HeavyOil, 10)] Chemical (Time 1),
    r LaserTurret 1 [(Battery, 12), (ElectronicCircuit, 20), (SteelPlate, 20)] Assembly (Time 20),
    r Battery 1[(CopperPlate, 1), (IronPlate, 1), (SulfuricAcid, 40)] Chemical (Time 5),
    r SciencePackHighTech 2[(Battery, 1), (CopperCable, 30), (ProcessingUnit, 3), (SpeedModule, 1)] Assembly (Time 14),
    
    Recipe (BurnRecipe SolidFuel) [(Steam, (25e6 * 0.5) / energy_per_steam)] [(SolidFuel, 1)] Boiler (Time 1), -- incorrect time, but nothing cares
--    Recipe (BurnRecipe Coal) [(Steam, (8e6 * 0.5) / energy_per_steam)] [(Coal, 1)] Boiler (Time 1), -- incorrect time, but nothing cares
    r Energy energy_per_steam [{- (Steam, 1) -} ] Boiler (Time 1), -- incorrect time and venue, but nothing goes wrong
    
    r PetroleumGas 2 [(LightOil, 3)] Chemical (Time 5),
    
    r SolidFuel 1 [(LightOil, 10)] Chemical (Time 3),
    r LightOil 3[(HeavyOil, 4)] Chemical (Time 5),
    Recipe LiquefactionRecipe [(HeavyOil, 35), (LightOil, 15), (PetroleumGas, 20)] [(Coal, 10), (HeavyOil, 25), (Steam, 50)] Refinery (Time 5)
  ] where
  r product quantity ingredients venue time = Recipe (ProductRecipe product) [(product, quantity)] ingredients venue time

mconcat' x = foldr add zero x

functionToMatrix :: (Num v, Ix' b, Ix' a) => (a -> [(b, v)]) -> Matrix a b v
functionToMatrix f =
  Matrix (Array.array fullRange [ ((a, b), maybe 0 id (Map.lookup b bs)) | a <- range fullRange, let bs = Map.fromListWith (+) (f a), b <- range fullRange])

recipesByName = Map.fromListWith (error "multiple recipes with the same name") (map (\recipe -> (recipeName recipe, recipe)) recipes)

recipesToMatrix :: (Recipe -> Config) -> Map RecipeName (Map Product Rat)
recipesToMatrix configs = 
  fmap (\recipe@(Recipe _recipeName production consumption venue (Time time)) ->
         (
         let config = configs recipe in
           let energy = (time / (speedMultiplier config * baseSpeed venue)) * unPower (basePower venue) * energyMultiplier config in
             Map.fromListWith add (fmap (second negate) (consumption ++ [(Energy, energy)]) ++ map (second ((* productivityMultiplier config))) production)
         )) recipesByName

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
     -> Map Product (Map RawProduct Rat, Map RecipeName Rat)
solvedRecipes configs =
  find_kernel_with_trace (/) (*) (recipesToMatrix configs)

currentSolvedRecipes = solvedRecipes currentConfig

vector_lookup :: Ix' a => Vector a v -> a -> v
vector_lookup (Matrix x) a = x ! (a, ())

compute'_new :: (Recipe -> Config) -> Product -> RawMaterialPressure
compute'_new configs =
  let recipes = fmap fst $ solvedRecipes configs in
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
usability' SciencePackHighTech = Unusable
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
usability' PetroleumGas = Unusable
usability' ResearchCoalLiquefaction = Unusable
usability' ResearchRocketSilo = Unusable
usability' ResearchNuclearPower = Unusable
usability' ResearchLaserTurretDamage5 = Unusable
usability' LightOil = Unusable
usability' HeavyOil = Unusable
usability' Steam = Unusable
usability' LaserTurret = Usable
usability' Battery = Unusable
usability' PiercingRoundMagazine = Usable
usability' FirearmRoundMagazine = Usable
usability' Grenade = Usable
usability' GunTurret = Usable
usability' SciencePackMilitary = Unusable
usability' x = error $ "undefined usability: " ++ show x

usability recipe =
  case recipeName recipe of
    ProductRecipe product -> usability' product
    LiquefactionRecipe -> Unusable
    BurnRecipe product -> Unusable

evaluateTotalCost  :: RawMaterialPressure -> Rat
evaluateTotalCost f = sum [ (estimate k * v) | (k, v) <- Map.toList f, v /= zero] where
--  estimate LightOil = 0.1
--  estimate HeavyOil = 0.1
  estimate BuriedCoal = 1.5
  estimate BuriedIron = 1
  estimate BuriedCopper = 1
  estimate product = error $ "don't know how much this is worth: " ++ show product
--  estimate PetroleumGas = 0.1

subtract' a b = add a (minus b)

data RecipeImprovement =
  RecipeImprovement
  {
    recipeImprovement_saving_per_execution :: RawMaterialPressure,
    recipeImprovement_execution_time :: Time,
    recipeImprovement_cost_per_execution_per_second :: RawMaterialPressure
  }
  deriving Generic

instance NFData RecipeImprovement

possibleSavings :: Recipe -> [([Product], RecipeImprovement)]
possibleSavings recipe = (`using` parList rdeepseq) $
  let venue = recipeVenue recipe in
  let Time time = recipeTime recipe in
    [ let saving_per_execution =
            add
            (compute_recipe currentConfig recipe)
            (fmap negate $ compute_recipe (\p -> if p == recipe then config else currentConfig p) recipe)
      in
        let execution_time_old =
              time / (speedMultiplier (currentConfig recipe) * baseSpeed venue)
        in
        let execution_time =
              time / (speedMultiplier config * baseSpeed venue)
        in
          let
            modules_cost_per_execution_per_second_old =
              scale execution_time_old (modulesCost (currentModules recipe))
          in
          let
            modules_cost_per_execution_per_second_new =
              scale execution_time (modulesCost modules)
          in
          let
            cost_per_execution_per_second =
              add (modules_cost_per_execution_per_second_new) (minus modules_cost_per_execution_per_second_old)
          in
          (modules, RecipeImprovement {
              recipeImprovement_saving_per_execution = saving_per_execution,
              recipeImprovement_execution_time = Time execution_time,
              recipeImprovement_cost_per_execution_per_second = cost_per_execution_per_second })
      | (modules, config) <- availableConfigs venue (usability recipe)]

newtype Rat = Rat Rational deriving (Eq, Ord, Generic, NFData, Linear, Num, Fractional, Real)

instance VectorSpace Rat where
  type Scalar Rat = Rat
  scale (Rat x) (Rat y) = Rat (x * y)

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
showModule AssemblingMachine3 = "+"

divv a b = if b == 0 then 1e10 else a / b

modulesCost modules = mconcat' $ map computeTotalCost modules

partition_market l
  =
  ( p (\(gain, cost) -> gain >= 0 && cost <= 0) (\(gain, cost) -> gain - cost) -- free money
  , p (\(gain, cost) -> gain > 0 && cost >= 0) (\(gain, cost) -> cost / gain) -- buy
  , p (\(gain, cost) -> gain < 0 && cost < 0) (\(gain, cost) -> gain / cost)
  ) where
  p predicate order = sortBy (comparing (order . extractGL)) $ filter (predicate . extractGL) l
  extractGL (_, _, gain, cost) = (gain, cost)

allRecipeNames = [recipeName recipe | recipe <- recipes]

possibleSavings' (Time totalTime) executions_per_second =
  [ (recipeName, (concatMap showModule modules), saving, cost + installationCost)
  | recipe <- recipes
  , recipeName <- return $ recipeName recipe
  , executions_per_second <- return (executions_per_second recipeName)
  , (modules, RecipeImprovement {
              recipeImprovement_saving_per_execution,
              recipeImprovement_execution_time,
              recipeImprovement_cost_per_execution_per_second}) <- possibleSavings recipe
  , let saving = (totalTime *) $ evaluateTotalCost $ scale executions_per_second recipeImprovement_saving_per_execution
  , let cost = evaluateTotalCost $ scale executions_per_second recipeImprovement_cost_per_execution_per_second
  ]

installationCost = 1000

desiredMaterials =
  [ (ResearchNuclearPower,  1)
  , (PiercingRoundMagazine, 10000)
  ]

lookup0 m k = case Map.lookup k m of
  Nothing -> zero
  Just x -> x

effectiveExecutionTime recipeName =
  let recipe = (recipesByName Map.! recipeName) in
  unTime (recipeTime recipe) / (speedMultiplier (currentConfig recipe) * baseSpeed (recipeVenue recipe))

rCols =
  [ ("Efficiency", (\(_, _, gain, cost) -> show (gain / cost)))
  , ("Name", (\(name, _, _, _) -> show name))
  , ("Mod", (\(_, mod, _, _) -> show mod))
  , ("Gain", (\(_, _, gain, _) -> show gain))
  , ("Cost", (\(_, _, _, cost) -> show cost))
  ]

pad n l = replicate (n - length l) ' ' ++ l

printTable :: [[String]] -> [String]
printTable =
  map concat . transpose . map (\col -> let maxl = maximum (map length col) in map (pad (maxl + 1)) col) . transpose

printTableG :: Ord a => [a] -> [(String, (a -> String))] -> IO ()
printTableG l cols =
  let title = map fst cols in
  let showA row = map (($row) . snd) cols in
  mapM_ putStrLn $ printTable (title : map showA l)

printRs l = printTableG l rCols

report =
  let totalTime = Time (4500) in
  let futureFactor = 3 in
  let
   (total_cost_per_second, executions_per_second) =
    foldr add zero (
      map
        (\(product, amount) ->
            scale (recip $ unTime totalTime) (scale amount $ currentSolvedRecipes Map.! product)
            ) desiredMaterials)
  in
  let savings = possibleSavings' (scale futureFactor totalTime) (lookup0 executions_per_second) in
  let
    negative_executions_per_second =
      filter
       ((<0) . snd)
        (Map.toList executions_per_second)
  in
   do
    let (free_money, buys, sells) = partition_market savings
    mapM_ print negative_executions_per_second
    print "total factories:"
    let factories k = flip fmap (Map.lookup k executions_per_second ) (* effectiveExecutionTime k)
    printTableG (sortBy (comparing factories) allRecipeNames)
      [ ("Name", show)
      , ("Factories", maybe "<none>" show . factories)
      , ("Price",
          \k ->
            case k of
              ProductRecipe product ->
                show $ evaluateTotalCost $ computeTotalCost product
              _ -> "<complex>")
      ]
    print "total cost"
    print (scale (unTime totalTime) total_cost_per_second)
    print "iron plate execs/m"
    print (60 * executions_per_second Map.! ProductRecipe IronPlate)
    print "free money"
    printRs free_money
    print "buys"
    printRs (take 20 buys)
    print "sells"
    printRs (take 20 sells)
    

matrix_of_lists lists =
  Matrix (Array.array fullRange
          [ ((i, j), toRational v)
          | (i, r) <- zip (range fullRange) lists
          , (j, v) <- zip (range fullRange) r])

identity_matrix :: (Ix' a) => Matrix a a Rat
identity_matrix = Matrix (f_array (\(a,b) -> if a == b then 1 else 0))

p1 = ProductivityModule
p2 = ProductivityModule2
p3 = ProductivityModule3
s1 = SpeedModule
s2 = SpeedModule2
e1 = EfficiencyModule
p = AssemblingMachine3

-- todo:
{-currentModules' EngineUnit = [p, s1, p1, p1, p1]
currentModules' Sulfur = [p2, p2, p2]

currentModules' ResearchRocketSilo = [p2, p2]
currentModules' SulfuricAcid = [p3, p3, p3]
currentModules' ProcessingUnit = [p, p3, p3, p3, s2]
currentModules' ElectricEngineUnit = [p, s1, p1, p1, p1]
currentModules' CopperCable = [p, s1, p1, p1, p1]
currentModules' SciencePackMilitary = [p, s1, p2, p2, p2]
currentModules' SciencePackProduction = [p, s2, p3, p3, p3]
currentModules' SciencePackHighTech = [p, s2, p3, p3, p3]
currentModules' SciencePack3 = [p, p2, p2, p2, s1]
currentModules' ElectronicCircuit = [p, s1, p2, p2, p2]
currentModules' GearWheel = [p, p2, p2, p2, s1]
currentModules' ResearchCoalLiquefaction = [p1, p1]
currentModules' ResearchLaserTurretDamage5 = [p1, p1]
currentModules' AdvancedCircuit = [p, p1, p1, p1, s1]
currentModules' Plastic = [p2, p2, p2]
currentModules' SciencePack2 = [p1]
currentModules' IronPlate = [e1, e1]
currentModules' CopperPlate = [e1, e1]
currentModules' SteelPlate = [e1, e1]
currentModules' Battery = [p2, p2, p2] -}

currentModules' _ = []

currentModules recipe =
  case recipeName recipe of
    ProductRecipe product -> currentModules' product
    LiquefactionRecipe -> []
    _ -> []  

main = report
--main =
--  print $ solvedRecipes currentConfig Map.! SulfuricAcid
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


