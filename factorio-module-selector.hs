{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Data.Map as Map
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

data RawMaterial =
  BuriedIron
  | BuriedCopper
  | PetroleumGas
  | LightOil
  | HeavyOil
  | BuriedCoal
  deriving (Eq, Ord, Enum, Bounded, Show, Ix, Generic)

instance NFData RawMaterial

type RawMaterialPressure = Vector RawMaterial Rational

allOfThem = [minBound..maxBound]

vector_of_function f = Matrix (f_array (\(x, ()) -> f x))

matrixZipWith f (Matrix a) (Matrix b) =
  Matrix (f_array (\k -> a ! k + b ! k))

negateRawMaterialPressure x = fmap negate x
addRawMaterialPressure f g = matrixZipWith (+) f g

data IntermediateProduct =
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
  | Coal
  deriving (Eq, Ord, Show, Enum, Bounded, Ix, Generic)

instance NFData IntermediateProduct where

data Product =
  Raw RawMaterial
  | Intermediate IntermediateProduct
  deriving (Eq, Ord)

instance Bounded Product where
  minBound = Raw minBound
  maxBound = Intermediate maxBound

instance Enum Product where
  fromEnum (Raw raw) = fromEnum raw
  fromEnum (Intermediate intermediate) = Array.rangeSize (minBound, maxBound :: RawMaterial) + fromEnum intermediate

  toEnum x
    | x < Array.rangeSize (minBound, maxBound :: RawMaterial) = Raw (toEnum x)
    | otherwise = Intermediate (toEnum (x - Array.rangeSize (minBound, maxBound :: RawMaterial)))

instance Ix Product where
  range (Raw a, Raw b) = map Raw (range (a, b))
  range (Raw a, Intermediate b) = map Raw (range (a, maxBound)) ++ map Intermediate (range (minBound, b))
  range (Intermediate a, Intermediate b) = map Intermediate (range (a, b))

  index (Raw a, _) (Raw b) = Array.index (a, maxBound) b
  index (Raw a, Intermediate b) (Intermediate x) = Array.rangeSize (a, maxBound) + Array.index (minBound, b) x
  index (Intermediate a, Intermediate b) (Intermediate x) = Array.index (a, b) x

  inRange (Intermediate _, _) (Raw _) = False
  inRange (_, Raw _) (Intermediate _) = False
  inRange (Raw a, Raw b) (Raw x) = Array.inRange (a, b) x
  inRange (Intermediate a, Intermediate b) (Intermediate x) = Array.inRange (a, b) x
  inRange (Raw a, Intermediate b) (Raw x) = Array.inRange (a, maxBound) x
  inRange (Raw a, Intermediate b) (Intermediate x) = Array.inRange (minBound, b) x

data Time = Time Rational

data FactoryKind =
  Assembly
  | Smelter
  | Chemical
  | Miner
  | Lab
  | Boiler

data Config = Config
  { 
    configSpeedBonus :: Rational,
    configProductivityBonus :: Rational,
    configEnergyBonus :: Rational
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

allModules :: Usability -> [([IntermediateProduct], Config)]
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

availableConfigs :: FactoryKind -> Usability -> [([IntermediateProduct], Config)]
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
data Power = Power { unPower :: Rational }

basePower :: FactoryKind -> Power
basePower Assembly = Power 210e3
basePower Miner = Power 90e3
basePower Smelter = Power 180e3
basePower Chemical = Power 210e3
basePower Lab = Power 60e3
basePower Boiler = Power 0

baseSpeed Assembly = 1.25 -- CR-someday: allow for blue assembly machines
baseSpeed Miner = 1 -- factored in into the recipe
baseSpeed Smelter = 2
baseSpeed Chemical = 1.25
baseSpeed Lab = 1 -- CR-someday: take upgrades into account
baseSpeed Boiler = 1 -- nothing cares about this

p1 = ProductivityModule
p2 = ProductivityModule2
p3 = ProductivityModule3
s1 = SpeedModule
s2 = SpeedModule2
e1 = EfficiencyModule
currentModules SciencePackProduction = [p1, p1, p1, p1]
currentModules SciencePack3 = [p1, p1, p1, p1]
currentModules ElectronicCircuit = [p1, p2, p2, p2]
currentModules GearWheel = [p1, p1, p1, s1]
currentModules CoalLiquefaction = [p1, p1]
currentModules ElectricEngineUnit = [s1, p1, p1, p1]
currentModules AdvancedCircuit = [p1, p1, s1]
currentModules Plastic = [p1]
currentModules SciencePack2 = [p1]
{- 
currentModules GearWheel = [s1, p2, p2, p2]
currentModules AdvancedCircuit = [p1, p1, p1, s1]
currentModules Plastic = [p1, p1, e1]
currentModules CopperCable = [p1, e1, e1, e1]
currentModules EngineUnit = [p1, e1, e1, e1]
currentModules Pipe = [e1, e1, e1]
currentModules SciencePack3 = [s1, p1, p1, p1]
currentModules ElectricMiningDrill = [e1, e1, e1]
currentModules SteelPlate = [e1, e1]
currentModules CopperPlate = [e1, e1]
currentModules IronPlate = [e1, e1]
currentModules CopperOre = [e1, e1]
currentModules IronOre = [e1, e1] -}

{-currentModules GearWheel = [p1, p1, p1, p1]
currentModules Pipe = [p1, p1, p1, s1]
currentModules AdvancedCircuit = [p1, p1, p1, s1]
currentModules Plastic = [e1, p1, p1]
currentModules CopperCable = [e1, e1, e1, p1]
currentModules SciencePack3 = [e1, e1, e1]
currentModules ElectricMiningDrill = [e1, e1, e1]
currentModules EngineUnit = [e1, e1, e1, p1]
-}
currentModules _ = []

currentConfig :: IntermediateProduct -> Config
currentConfig = mconcat . map moduleToConfig . currentModules

is :: [(IntermediateProduct, Rational)] -> [(Product, Rational)]
is = map (first Intermediate)

scaleTime s (Time t) = Time (s * t)

coalToEnergy coal = coal * 8e6 / 2

data Matrix a b v = Matrix (Array (a, b) v) deriving (Show, Eq, Ord, Functor, Generic)
instance (NFData a, NFData b, NFData v) => NFData (Matrix a b v)
type Vector a v = Matrix a () v

fullRange :: Bounded a => (a, a)
fullRange = (minBound, maxBound)

type Ix' a = (Ix a, Bounded a, Enum a)

matrix_mult :: (Ix' a, Ix' b, Ix' c, Num v) => Matrix a b v -> Matrix b c v -> Matrix a c v
matrix_mult (Matrix x) (Matrix y) = Matrix (Array.array fullRange [ ((a, c), sum [ x ! (a, b) * y ! (b, c) | b <- range fullRange]) | a <- range fullRange, c <- range fullRange])

matrix_mult' :: (Ix' a, Ix' b, Num v) => Matrix a b v -> Vector b v -> Vector a v
matrix_mult' = matrix_mult

recipe :: IntermediateProduct -> ([(Product, Rational)], FactoryKind, Time)
recipe =
  let many n (rs, kind, time) = (map (second (/n)) rs, kind, scaleTime (recip n) time) in
    let
       recipe GearWheel = (is [(IronPlate, 4)], Assembly, Time 0.5)
       recipe IronPlate = (is [(IronOre, 1)], Smelter, Time 3.5)
       recipe CopperPlate = (is [(CopperOre, 1)], Smelter, Time 3.5)
       recipe SteelPlate = (is [(IronPlate, 10)], Smelter, Time 35)
       recipe IronOre = ([(Raw BuriedIron, 1)], Miner, Time (1/0.525))
       recipe CopperOre = ([(Raw BuriedCopper, 1)], Miner, Time (1/0.525))
       recipe Coal = ([(Raw BuriedCoal, 1)], Miner, Time (1/0.525))
       recipe Plastic = many 2 ([(Raw PetroleumGas, 20), (Intermediate Coal, 1)], Chemical, Time 1)
       recipe ElectronicCircuit = (is [(CopperCable, 10), (IronPlate, 2)], Assembly, Time 0.5)
       recipe AdvancedCircuit = (is [(Plastic, 4), (CopperCable, 8), (ElectronicCircuit, 2)], Assembly, Time 6)
       recipe CopperCable = many 2 (is [(CopperPlate, 1)], Assembly, Time 0.5)
       recipe Pipe = (is [(IronPlate, 2)], Assembly, Time 0.5)
       recipe EngineUnit = (is [(GearWheel, 1), (Pipe, 2), (SteelPlate, 1)], Assembly, Time 10)
       recipe ElectricMiningDrill = (is [(GearWheel, 10), (IronPlate, 20), (ElectronicCircuit, 5)], Assembly, Time 2)
       recipe SciencePack3 = (is [(AdvancedCircuit, 1), (ElectricMiningDrill, 1), (EngineUnit, 1)], Assembly, Time 12)
       recipe SciencePack1 = (is [(CopperPlate, 1), (GearWheel, 1)], Assembly, Time 5)
       recipe SciencePack2 = (is [(Inserter, 1), (TransportBelt, 1)], Assembly, Time 6)
       recipe SciencePackProduction = many 2 (is [(AssemblingMachine1, 1), (ElectricEngineUnit, 1), (ElectricFurnace, 1)], Assembly, Time 14)
       recipe SpeedModule = (is [(AdvancedCircuit, 5), (ElectronicCircuit, 5)], Assembly, Time 15)
       recipe EfficiencyModule = (is [(AdvancedCircuit, 5), (ElectronicCircuit, 5)], Assembly, Time 15)
       recipe ProductivityModule = (is [(AdvancedCircuit, 5), (ElectronicCircuit, 5)], Assembly, Time 15)
       recipe EfficiencyModule2 = (is [(AdvancedCircuit, 5), (EfficiencyModule, 4), (ProcessingUnit, 5)], Assembly, Time 30)
       recipe SpeedModule2 = (is [(AdvancedCircuit, 5), (SpeedModule, 4), (ProcessingUnit, 5)], Assembly, Time 30)
       recipe ProductivityModule2 = (is [(AdvancedCircuit, 5), (ProductivityModule, 4), (ProcessingUnit, 5)], Assembly, Time 30)
       recipe ProductivityModule3 = (is [(AdvancedCircuit, 5), (ProductivityModule2, 5), (ProcessingUnit, 5)], Assembly, Time 60)
       recipe SpeedModule3 = (is [(AdvancedCircuit, 5), (SpeedModule2, 5), (ProcessingUnit, 5)], Assembly, Time 60)
       recipe EfficiencyModule3 = (is [(AdvancedCircuit, 5), (EfficiencyModule2, 5), (ProcessingUnit, 5)], Assembly, Time 60)
       recipe ProcessingUnit = (is [(AdvancedCircuit, 2), (ElectronicCircuit, 20), (SulfuricAcid, 10)], Assembly, Time 10)
       recipe SulfuricAcid = many 50 (is [(IronPlate, 1), (Sulfur, 5)], Chemical, Time 1)
       recipe Sulfur = many 2 ([(Raw PetroleumGas, 30)], Chemical, Time 1)
       recipe CoalLiquefaction = many (1/800) (is [(SciencePack1, 1), (SciencePack2, 1), (SciencePack3, 1), (SciencePackProduction, 1)], Lab, Time 30)
       recipe Inserter = (is [(ElectronicCircuit, 1), (IronPlate, 1), (GearWheel, 1)], Assembly, Time 0.5)
       recipe TransportBelt = many 2 (is [(GearWheel, 1), (IronPlate, 1)], Assembly, Time 0.5)
       recipe AssemblingMachine1 = (is [(GearWheel, 5), (IronPlate, 9), (ElectronicCircuit, 3)], Assembly, Time 0.5)
       recipe AssemblingMachine2 = (is [(AssemblingMachine1, 1), (ElectronicCircuit, 5), (GearWheel, 10), (IronPlate, 20)], Assembly, Time 0.5)
       recipe AssemblingMachine3 = (is [(AssemblingMachine2, 2), (SpeedModule, 4)], Assembly, Time 0.5)
       recipe ElectricFurnace = (is [(AdvancedCircuit, 5), (SteelPlate, 10), (StoneBrick, 10)], Assembly, Time 5)
       recipe ElectricEngineUnit = (is [(ElectronicCircuit, 2), (EngineUnit, 1), (Lubricant, 15)], Assembly, Time 10)
       recipe StoneBrick = (is [(Stone, 2)], Smelter, Time 3.5)
       recipe Stone = (is [], Miner, Time 0.65) -- incorrect components
       recipe Lubricant = ([(Raw PetroleumGas, 5)], Chemical, Time 1) -- incorrect components
       recipe Energy = many (25e6 * 0.5) ([(Raw LightOil, 1)], Boiler, Time 1) -- incorrect time, but nothing cares
       -- recipe Energy = many (25e6 * 0.5) ([(Intermediate SolidFuel, 1)], Boiler, Time 1) -- incorrect time, but nothing cares
       recipe SolidFuel = ([(Raw LightOil, 10)], Chemical, Time 3)
       recipe x = error $ "undefined recipe for: " ++ show x
  in
  recipe

scale :: Rational -> RawMaterialPressure -> RawMaterialPressure
scale s f = vector_of_function (\x -> vector_lookup f x * s)

mconcat' = foldr addRawMaterialPressure (vector_of_function (\_ -> 0))

functionToMatrix :: (Num v, Ix' b, Ix' a) => (a -> [(b, v)]) -> Matrix a b v
functionToMatrix f =
  Matrix (Array.array fullRange [ ((a, b), maybe 0 id (Map.lookup b bs)) | a <- range fullRange, let bs = Map.fromListWith (+) (f a), b <- range fullRange])

recipesToMatrix :: (IntermediateProduct -> Config) -> Matrix IntermediateProduct Product Rational
recipesToMatrix configs =
  functionToMatrix
  (\product ->
     let config = configs product in
     let (components, kind, Time time) = recipe product in
       let energy = (time / (speedMultiplier config * baseSpeed kind)) * unPower (basePower kind) * energyMultiplier config in
       map (second (* (recip $ productivityMultiplier config)))
         (components ++ [(Intermediate Energy, energy)])
  )

f_array :: Ix' k => (k -> v) -> Array k v
f_array f = Array.array fullRange [(k, f k) | k <- range fullRange]

instance (Ix' a, Ix' b) => Enum (a, b) where
  fromEnum (x, (y :: b)) = fromEnum x * Array.rangeSize (fullRange :: (b, b)) + fromEnum y
  toEnum i =
    let (d, m) = i `divMod` (Array.rangeSize (fullRange :: (b, b))) in
      (toEnum d, (toEnum m :: b))

recipesToMatrix' :: (IntermediateProduct -> Config) -> (Matrix IntermediateProduct IntermediateProduct Rational, Vector RawMaterial (Vector IntermediateProduct Rational))
recipesToMatrix' configs =
  let
    m :: Array (IntermediateProduct, Product) Rational
    Matrix m = recipesToMatrix configs
  in
    (Matrix (f_array (\(a, b) -> m ! (a, Intermediate b)))
    ,Matrix (f_array (\(b, ()) -> Matrix (f_array (\(a, ()) -> m ! (a, Raw b))))))

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

matrix_inverse :: (Ix' a, Num v, Fractional v, Eq v) => Matrix a a v -> Matrix a a v
matrix_inverse x = trace "inverting" $ of_data_matrix . (\(Right res) -> res) . Data.Matrix.inverse . to_data_matrix $ x

--matrix_inverse_approx :: (Ix' a, Num v, Fractional v, Eq v) => Matrix a a Rational -> Matrix a a Rational
--matrix_inverse_approx x = trace "inverting" $ of_data_matrix . (\(Right res) -> res) . Data.Eigen.Matrix.inverse . to_data_matrix $ x

-- x = m * x + x_0
solve_equation :: (Ix' a, Num v, Fractional v, Eq v) => Matrix a a v -> Vector a v -> Vector a v
solve_equation a x0 = matrix_inverse (matrix_subtract a matrix_identity) `matrix_mult` matrix_negate x0

solvedRecipes
  :: (IntermediateProduct -> Config)
     -> Vector RawMaterial (Vector IntermediateProduct Rational)
solvedRecipes configs =
  let (a, x0) = recipesToMatrix' configs in
    let a' = matrix_inverse (matrix_subtract a matrix_identity) in
      fmap (\x0 -> a' `matrix_mult` matrix_negate x0) x0

vector_lookup :: Ix' a => Vector a v -> a -> v
vector_lookup (Matrix x) a = x ! (a, ())

compute' :: (IntermediateProduct -> Config) -> IntermediateProduct -> RawMaterialPressure
compute' configs =
  let recipes = (solvedRecipes configs) in
   \product -> fmap (\v -> vector_lookup v product) recipes

compute'_old :: (IntermediateProduct -> Config) -> IntermediateProduct -> RawMaterialPressure
compute'_old configs product = case recipe product of
  (components, kind, Time time) ->
    let energy = (time / (speedMultiplier config * baseSpeed kind)) * unPower (basePower kind) * energyMultiplier config in
     scale (recip $ productivityMultiplier config) $
     foldr addRawMaterialPressure (vector_of_function (\_ -> 0))
      [ scale quantity
        (case product of
           Raw raw -> vector_of_function (\x -> if x == raw then 1 else 0)
           Intermediate intermediate -> computeTotalCost intermediate
        )
        |
        (product, quantity) <- (components ++ [(Intermediate Energy, energy) | energy /= 0])
      ]
    where
      config = configs product

computeTotalCost :: IntermediateProduct -> RawMaterialPressure
computeTotalCost = compute' currentConfig

usability GearWheel = Unusable
usability IronPlate = Unusable
usability IronOre = Unusable
usability CopperOre = Unusable
usability CopperCable = Unusable
usability ElectronicCircuit = Unusable
usability AdvancedCircuit = Unusable
usability Plastic = Unusable
usability Sulfur = Unusable
usability SulfuricAcid = Unusable
usability EngineUnit = Unusable
usability CopperPlate = Unusable
usability SteelPlate = Unusable
usability ElectricMiningDrill = Usable
usability Pipe = Usable
usability Inserter = Usable
usability ElectricFurnace = Usable
usability SciencePack1 = Unusable
usability SciencePack2 = Unusable
usability SciencePack3 = Unusable
usability AssemblingMachine1 = Usable
usability AssemblingMachine2 = Usable
usability AssemblingMachine3 = Usable
usability TransportBelt = Usable
usability StoneBrick = Unusable
usability Lubricant = Unusable
usability SciencePackProduction = Unusable
usability CoalLiquefaction = Unusable
usability ElectricEngineUnit = Unusable
usability Stone = Unusable
usability Coal = Unusable
usability ProcessingUnit = Unusable
usability SpeedModule = Usable
usability SpeedModule2 = Usable
usability SpeedModule3 = Usable
usability EfficiencyModule = Usable
usability EfficiencyModule2 = Usable
usability EfficiencyModule3 = Usable
usability ProductivityModule = Usable
usability ProductivityModule2 = Usable
usability ProductivityModule3 = Usable
usability SolidFuel = Unusable
usability Energy = Usable
usability x = error $ "undefined usability: " ++ show x

evaluateTotalCost  :: RawMaterialPressure -> Rational
evaluateTotalCost f = sum [ (fromRational (estimate x * vector_lookup f x)) | x <- allOfThem] where
  estimate LightOil = 0.1
  estimate HeavyOil = 0.1
  estimate BuriedCoal = 1.5
  estimate BuriedIron = 1
  estimate BuriedCopper = 1
  estimate PetroleumGas = 0.1

subtractRawMaterialPressure x y = addRawMaterialPressure x (fmap negate y)

possibleSavings product =
  let (_, kind, Time time) = recipe product in
    [ let saving_per_unit =
            addRawMaterialPressure
            (compute' currentConfig product)
            (fmap negate $ compute' (\p -> if p == product then config else currentConfig product) product)
      in
        let saving_per_second =
              scale (recip $ time / speedMultiplier config) saving_per_unit
        in
          let
            cost =
              subtractRawMaterialPressure (modulesCost modules) (scale (recip (speedMultiplier (currentConfig product)) * speedMultiplier config) $ modulesCost (currentModules product))
          in
          (saving_per_second, cost, modules)
      | (modules, config) <- availableConfigs kind (usability product)]

newtype Dub = Dub Rational deriving (Eq, Ord, Generic, NFData)

instance Show Dub where
  show (Dub x) = printf "%.5f" (fromRational x :: Double)

showTotalCost :: RawMaterialPressure -> String
showTotalCost f = show [ (x, Dub (vector_lookup f x)) | x <- allOfThem]

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
  \(x, cost, modules) -> (evaluateTotalCost x, evaluateTotalCost cost, showTotalCost x, concat $ map showModule modules)) . (`using` parList rdeepseq) . possibleSavings

ff x = Dub x

possibleSavings'' =
  [
    (ff $ divv saving cost * 3600, (ff $ saving, ff $ cost, product, modules))
  | product <- take 1 $ range fullRange
  , (saving, cost, details, modules) <- possibleSavings' product
  , cost /= 0
  ]

possibleSavings''' =
  reverse . take 50 . dropWhile (\(_, (gain, cost, _, _)) -> gain < Dub 0 && cost < Dub 0) . reverse . sortBy (comparing fst) $ possibleSavings''  

-- main = mapM_ print $ possibleSavings'''

main = do
  let x = IronPlate
  print $ compute' currentConfig x
  print $ compute'_old currentConfig x
  print $ compute'_old currentConfig x == compute' currentConfig x
