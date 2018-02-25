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
import Data.Maybe
import Debug.Trace
import Data.Ord
import Text.Printf
import qualified Data.Array as Array
import Data.Array(Array, (!), range, Ix)
import qualified Data.Matrix
import qualified System.Random
import Control.Monad
import Control.Monad.Identity
import System.IO(stdout, hSetBuffering, BufferMode(..))
import Control.Parallel.Strategies
import GHC.Generics
import qualified Data.Eigen.Matrix
import Foreign.C.Types
import qualified Numeric.LinearAlgebra.HMatrix
import qualified Numeric.LinearAlgebra.Data
import Control.Arrow

import Matrix

data RawMaterial =
  BuriedIronRaw
  | BuriedCopperRaw
  | BuriedStoneRaw
--  | PetroleumGasRaw
--  | LightOilRaw
--  | HeavyOilRaw
  | CrudeOilRaw
  | BuriedCoalRaw
  | PollutionRaw
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
  | SteelFurnace
  | ElectricFurnace
  | ElectricEngineUnit
  | Lubricant
  | StoneBrick
  | Stone
  | LaserTurret
  | Battery
  | ChemicalPlant
  | OilRefinery
  | LabBuilding
  | BoilerBuilding
  | SteamEngineBuilding
  | SolarFacilityBuilding
  | Roboport
  | Substation
  | Accumulator
  | SolarPanel
  | StoneFurnace
  
  | ElectricalEnergy -- in J
  | ChemicalEnergy -- in J
  | SolidFuel
  | Steam
  | Coal
  | BuriedIron
  | BuriedCopper
  | PetroleumGas
  | LightOil
  | HeavyOil
  | CrudeOil
  | BuriedCoal
  | BuriedStone
  | Pollution
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

data VenueKind =
  AssemblyVenueKind
  | SmelterVenueKind
  | ChemicalVenueKind
  | MinerVenueKind
  | LabVenueKind
  | BoilerVenueKind
  | SteamEngineVenueKind
  | SolarPowerVenueKind
  | RefineryVenueKind
  | NoVenueVenueKind
  deriving (Show, Ord, Eq) 
venuesByKind :: VenueKind -> [Venue]
venuesByKind AssemblyVenueKind = [Assembly2, Assembly3]
venuesByKind SmelterVenueKind = [SmelterBurner, SmelterElectric]
venuesByKind ChemicalVenueKind = [Chemical]
venuesByKind MinerVenueKind = [Miner]
venuesByKind LabVenueKind = [Lab]
venuesByKind BoilerVenueKind = [Boiler]
venuesByKind SteamEngineVenueKind = [SteamEngine]
venuesByKind SolarPowerVenueKind = [SolarFacility]
venuesByKind RefineryVenueKind = [Refinery]
venuesByKind NoVenueVenueKind = [NoVenue]

data Venue =
  Assembly2
  | Assembly3
  | SmelterBurner
  | SmelterElectric
  | Chemical
  | Miner
  | Lab
  | Boiler
  | SteamEngine
  | SolarFacility
  | Refinery
  | NoVenue
  deriving (Show, Eq, Ord, Generic)

data ModuleConfig = ModuleConfig
  {
    configSpeedBonus :: Rat,
    configProductivityBonus :: Rat,
    configEnergyBonus :: Rat,
    configPollutionBonus :: Rat
  } deriving (Show, Generic)

data Config = Config
  {
    configVenue :: Venue,
    configModules :: ModuleConfig,
    configModuleMaterials :: [Product]
  } deriving Generic


instance NFData ModuleConfig
instance NFData Config

speedMultiplier c = (1 + configSpeedBonus x) where x = configModules c
productivityMultiplier c = 1 + configProductivityBonus x where x = configModules c
energyMultiplier c = max 0.2 (1 + configEnergyBonus x) where x = configModules c
pollutionMultiplier c = (1 + configPollutionBonus x) where x = configModules c

instance Monoid ModuleConfig where
  mempty = ModuleConfig 0 0 0 0
  a `mappend` b =
    ModuleConfig
      {
        configSpeedBonus = (configSpeedBonus a + configSpeedBonus b),
        configProductivityBonus = (configProductivityBonus a + configProductivityBonus b),
        configEnergyBonus = (configEnergyBonus a + configEnergyBonus b),
        configPollutionBonus = (configPollutionBonus a + configPollutionBonus b)
      }

data Usability =
  Unusable | Usable

moduleToConfig SpeedModule = ModuleConfig 0.2 0 0.5 0
moduleToConfig SpeedModule2 = ModuleConfig 0.3 0 0.6 0
moduleToConfig SpeedModule3 = ModuleConfig 0.5 0 0.7 0
moduleToConfig EfficiencyModule = ModuleConfig 0 0 (negate 0.3) 0
moduleToConfig EfficiencyModule2 = ModuleConfig 0 0 (negate 0.4) 0
moduleToConfig EfficiencyModule3 = ModuleConfig 0 0 (negate 0.5) 0
moduleToConfig ProductivityModule = ModuleConfig (negate 0.15) 0.04 0.4 0
moduleToConfig ProductivityModule2 = ModuleConfig (negate 0.15) 0.06 0.6 0
moduleToConfig ProductivityModule3 = ModuleConfig (negate 0.15) 0.10 0.8 0

allModules :: Usability -> [[([Product], ModuleConfig)]]
allModules usability =
  [[ ([], mempty) ]] ++
  (map (\ms -> map (\m -> ([m], moduleToConfig m)) ms) $
  (map return
  [ SpeedModule
  , EfficiencyModule
--  , EfficiencyModule2
--  , EfficiencyModule3
  , SpeedModule2
--  , SpeedModule3
  ]
  ++ [case usability of
   Unusable ->
     [ ProductivityModule
     , ProductivityModule2
     , ProductivityModule3
     ]
   Usable -> []]))

choose' :: Int -> [[a]] -> [[a]]
choose' k l
  | k < 0 = []
  | k == 0 = [[]]
  | otherwise = case l of
    [] -> []
    (xc : xs) -> choose' k xs ++ (do
      i <- [1..k]
      (x :: a) <- xc
      rest <- choose' (k - i) xs
      return $ replicate i x ++ rest)

choose k l
  | k < 0 = []
  | k == 0 = [[]]
  | otherwise = case l of
     [] -> []
     (x : xs) -> map (x:) (choose (k-1) l) ++ choose k xs

moduleSlots :: Venue -> Int
moduleSlots venue = case venue of
        Assembly2 -> 2
        Assembly3 -> 4
        SmelterElectric -> 2
        SmelterBurner -> 0
        Chemical -> 3
        Miner -> 3
        Lab -> 2
        Boiler -> 0
        SteamEngine -> 0
        SolarFacility -> 0
        Refinery -> 3
        NoVenue -> 0

availableConfigs :: VenueKind -> Usability -> [Config]
availableConfigs venueKind usability =
  let venues = venuesByKind venueKind in
  let availableModules = allModules usability in
  do
    venue <- venues
    modules' <- choose' (moduleSlots venue) availableModules
    let (modules, moduleConfig) = mconcat modules'
    return $ (Config {
      configVenue = venue,
      configModules = moduleConfig,
      configModuleMaterials = modules })

-- in Watt
data Power =
  ElectricalPower { unPower :: Rat }
  | ChemicalPower Rat

baseBoilerPower = 3.6e6

basePower :: Venue -> Power
basePower Assembly2 = ElectricalPower 150e3
basePower Assembly3 = ElectricalPower 210e3
basePower Miner = ElectricalPower 90e3
basePower SmelterElectric = ElectricalPower 180e3
basePower SmelterBurner = ChemicalPower 180e3
basePower Chemical = ElectricalPower 210e3
basePower Lab = ElectricalPower 60e3
basePower Boiler = ChemicalPower baseBoilerPower
basePower SteamEngine = ElectricalPower 0
basePower SolarFacility = ElectricalPower 0
basePower Refinery = ElectricalPower 420e3
basePower NoVenue = ElectricalPower 0

basePollution Assembly2 = 2.4
basePollution Assembly3 = 1.8
basePollution Miner = 9
basePollution SmelterElectric = 0.9
basePollution SmelterBurner = 3.6
basePollution Chemical = 1.8
basePollution Lab = 0
basePollution Boiler = 27.6923
basePollution Refinery = 3.6
basePollution SolarFacility = 0
basePollution SteamEngine = 0
basePollution NoVenue = 0

labUpgrades = 1.5 -- +20% +30%

baseSpeed SolarFacility = 1
baseSpeed Assembly2 = 0.75
baseSpeed Assembly3 = 1.25
baseSpeed Miner = 1 -- factored in into the recipe -- CR-someday: take productivity upgrades into account
baseSpeed SmelterElectric = 2
baseSpeed SmelterBurner = 2
baseSpeed Chemical = 1.25
baseSpeed Lab = labUpgrades
baseSpeed Boiler = 1 -- this is factored into the recipe
baseSpeed SteamEngine = 1 -- this is factored into the recipe
baseSpeed Refinery = 1
baseSpeed NoVenue = 1 -- this is meaningless

currentConfig :: Recipe -> Config
currentConfig = \recipe ->
  let (venue, modules) = currentModules recipe in
  Config
    {
      configVenue = venue,
      configModules = mconcat . map moduleToConfig $ modules,
      configModuleMaterials = modules
    }

scaleTime s (Time t) = Time (s * t)

coalToEnergy coal = coal * 8e6 / 2

data Recipe = Recipe
  {
    recipeName :: RecipeName,
    recipeProducts :: [(Product, Rat)],
    recipeMaterials :: [(Product, Rat)],
    recipeVenueKind :: VenueKind,
    recipeTime :: Time
  } deriving (Eq, Ord, Show)

data RecipeName =
  ProductRecipe Product
  | LiquefactionRecipe
  | AdvancedOilProcessing
  | BoilerRecipe
  | UseAsFuelRecipe Product
  deriving (Eq, Ord)

instance Show RecipeName where
  show (ProductRecipe product) = show product
  show (LiquefactionRecipe) = "Liquefaction"
  show (AdvancedOilProcessing) = "AdvancedOilProcessing"
  show BoilerRecipe = "BoilerRecipe"
  show (UseAsFuelRecipe product) = "UseAsFuel" ++ show product

energy_per_steam = 30000

recipes :: [Recipe]
recipes =
  let assembly = AssemblyVenueKind in
  let smelter = SmelterVenueKind in
  [
    r GearWheel 1 [(IronPlate, 4)] assembly (Time 0.5),
    r IronPlate 1 [(IronOre, 1)] smelter (Time 3.5),
    r CopperPlate 1 [(CopperOre, 1)] smelter (Time 3.5),
    r SteelPlate 1 [(IronPlate, 10)] smelter (Time 35),
    r IronOre 1[(BuriedIron, 1)] MinerVenueKind (Time (1/0.525)),
    r CopperOre 1[(BuriedCopper, 1)] MinerVenueKind (Time (1/0.525)),
    r Coal 1 [(BuriedCoal, 1)] MinerVenueKind (Time (1/0.525)),
    r Plastic 2[(PetroleumGas, 20), (Coal, 1)] ChemicalVenueKind (Time 1),
    r ElectronicCircuit 1 [(CopperCable, 10), (IronPlate, 2)] assembly (Time 0.5),
    r AdvancedCircuit 1 [(Plastic, 4), (CopperCable, 8), (ElectronicCircuit, 2)] assembly (Time 6),
    r CopperCable 2 [(CopperPlate, 1)] assembly (Time 0.5),
    r Pipe 1 [(IronPlate, 2)] assembly (Time 0.5),
    r EngineUnit 1 [(GearWheel, 1), (Pipe, 2), (SteelPlate, 1)] assembly (Time 10),
    r ElectricMiningDrill 1 [(GearWheel, 10), (IronPlate, 20), (ElectronicCircuit, 5)] assembly (Time 2),
    r SciencePack3 1 [(AdvancedCircuit, 1), (ElectricMiningDrill, 1), (EngineUnit, 1)] assembly (Time 12),
    r SciencePack1 1 [(CopperPlate, 1), (GearWheel, 1)] assembly (Time 5),
    r SciencePack2 1 [(Inserter, 1), (TransportBelt, 1)] assembly (Time 6),
    r SciencePackProduction 2 [(ElectricEngineUnit, 1), (ElectricFurnace, 1)] assembly (Time 14),
    r PiercingRoundMagazine 1 [(FirearmRoundMagazine, 1), (SteelPlate, 1), (CopperPlate, 5)] assembly (Time 3),
    r FirearmRoundMagazine 1 [(IronPlate, 4)] assembly (Time 1),
    r Grenade 1 [(IronPlate, 5), (Coal, 10)] assembly (Time 8),
    r GunTurret 1[(GearWheel, 10), (CopperPlate, 10), (IronPlate, 20)] assembly (Time 8),
    r SciencePackMilitary 2 [(PiercingRoundMagazine, 1), (Grenade, 1), (GunTurret, 1)] assembly (Time 10),
    r SpeedModule 1 [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] assembly (Time 15),
    r EfficiencyModule 1 [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] assembly (Time 15),
    r ProductivityModule 1 [(AdvancedCircuit, 5), (ElectronicCircuit, 5)] assembly (Time 15),
    r EfficiencyModule2 1 [(AdvancedCircuit, 5), (EfficiencyModule, 4), (ProcessingUnit, 5)] assembly (Time 30),
    r SpeedModule2 1 [(AdvancedCircuit, 5), (SpeedModule, 4), (ProcessingUnit, 5)] assembly (Time 30),
    r ProductivityModule2 1 [(AdvancedCircuit, 5), (ProductivityModule, 4), (ProcessingUnit, 5)] assembly (Time 30),
    r ProductivityModule3 1 [(AdvancedCircuit, 5), (ProductivityModule2, 5), (ProcessingUnit, 5)] assembly (Time 60),
    r SpeedModule3 1 [(AdvancedCircuit, 5), (SpeedModule2, 5), (ProcessingUnit, 5)] assembly (Time 60),
    r EfficiencyModule3 1 [(AdvancedCircuit, 5), (EfficiencyModule2, 5), (ProcessingUnit, 5)] assembly (Time 60),
    r ProcessingUnit 1 [(AdvancedCircuit, 2), (ElectronicCircuit, 20), (SulfuricAcid, 10)] assembly (Time 10),
    r SulfuricAcid 50 [(IronPlate, 1), (Sulfur, 5)] ChemicalVenueKind (Time 1),
    r Sulfur 2 [(PetroleumGas, 30)] ChemicalVenueKind (Time 1),
    r ResearchCoalLiquefaction (1/800) [(SciencePack1, 1), (SciencePack2, 1), (SciencePack3, 1), (SciencePackProduction, 1)] LabVenueKind (Time 30),
    r ResearchNuclearPower (1/4000) [(SciencePack1, 1), (SciencePack2, 1), (SciencePack3, 1)] LabVenueKind (Time 30),
    r ResearchLaserTurretDamage5 (1/800)
      [(SciencePack1, 1), (SciencePack2, 1), (SciencePack3, 1), (SciencePackProduction, 1), (SciencePackHighTech, 1)] LabVenueKind (Time 60),
    r ResearchRocketSilo (1/4000)
      [ (SciencePack1, 1)
      , (SciencePack2, 1)
      , (SciencePack3, 1)
      , (SciencePackProduction, 1)
      , (SciencePackHighTech, 1)
      , (SciencePackMilitary, 1)] LabVenueKind (Time 60),
    r Inserter 1 [(ElectronicCircuit, 1), (IronPlate, 1), (GearWheel, 1)] assembly (Time 0.5),
    r TransportBelt 2 [(GearWheel, 1), (IronPlate, 1)] assembly (Time 0.5),
    r AssemblingMachine1 1 [(GearWheel, 5), (IronPlate, 9), (ElectronicCircuit, 3)] assembly (Time 0.5),
    r AssemblingMachine2 1 [(AssemblingMachine1, 1), (ElectronicCircuit, 5), (GearWheel, 10), (IronPlate, 20)] assembly (Time 0.5),
    r AssemblingMachine3 1 [(AssemblingMachine2, 2), (SpeedModule, 4)] assembly (Time 0.5),
    r SteelFurnace 1 [(SteelPlate, 6), (StoneBrick, 10)] assembly (Time 3),
    r ElectricFurnace 1 [(AdvancedCircuit, 5), (SteelPlate, 10), (StoneBrick, 10)] assembly (Time 5),
    r ElectricEngineUnit 1 [(ElectronicCircuit, 2), (EngineUnit, 1), (Lubricant, 15)] assembly (Time 10),
    r StoneBrick 1 [(Stone, 2)] smelter (Time 3.5),
    r Stone 1 [(BuriedStone, 1)] MinerVenueKind (Time 0.65),
    r Lubricant 10 [(HeavyOil, 10)] ChemicalVenueKind (Time 1),
    r LaserTurret 1 [(Battery, 12), (ElectronicCircuit, 20), (SteelPlate, 20)] assembly (Time 20),
    r Battery 1[(CopperPlate, 1), (IronPlate, 1), (SulfuricAcid, 40)] ChemicalVenueKind (Time 5),
    r SciencePackHighTech 2 [(Battery, 1), (CopperCable, 30), (ProcessingUnit, 3), (SpeedModule, 1)] assembly (Time 14),
    r ChemicalPlant 1 [(ElectronicCircuit, 5), (GearWheel, 5), (Pipe, 5), (SteelPlate, 5)] assembly (Time 5),
    r OilRefinery 1 [(ElectronicCircuit, 10), (GearWheel, 10), (Pipe, 10), (SteelPlate, 15), (StoneBrick, 10)] assembly (Time 8),
    r LabBuilding 1 [(ElectronicCircuit, 10), (GearWheel, 10), (TransportBelt, 4)] assembly (Time 2),
    r BoilerBuilding 1 [(Pipe, 4), (StoneFurnace, 1)] assembly (Time 0.5),
    r StoneFurnace 1 [(Stone, 5)] assembly (Time 0.5),
    r SteamEngineBuilding 1 [(GearWheel, 10), (IronPlate, 50), (Pipe, 5)] assembly (Time 0.5),
    
    Recipe BoilerRecipe [(Steam, (baseBoilerPower * 0.5) / energy_per_steam)] [] BoilerVenueKind (Time 1),
--    Recipe (UseAsFuelRecipe SolidFuel) [(Steam, (25e6 * 0.5) / energy_per_steam)] [(SolidFuel, 1)] BoilerVenueKind (Time 1), -- incorrect time, but nothing cares
    Recipe (UseAsFuelRecipe Coal) [(ChemicalEnergy, 8e6)] [(Coal, 1)] NoVenueVenueKind (Time 1), -- time is meaningless here
--    r ElectricalEnergy 1 [(Steam, 1/energy_per_steam)] SteamEngineVenueKind (Time (1/900e3)),
    r ElectricalEnergy (42e3 * 176) [] SolarPowerVenueKind (Time 1),
    r SolarFacilityBuilding 1 [(SolarPanel, 176), (Accumulator, 166), (Substation, 10), (Roboport, 1)] NoVenueVenueKind (Time 1),
    r Roboport 1 [(AdvancedCircuit, 45), (GearWheel, 45), (SteelPlate, 45)] assembly (Time 5),
    r Substation 1 [(AdvancedCircuit, 5), (CopperPlate, 5), (SteelPlate, 10)] assembly (Time 0.5),
    r Accumulator 1 [(Battery, 5), (IronPlate, 2)] assembly (Time 10),
    r SolarPanel 1 [(CopperPlate, 5), (ElectronicCircuit, 15), (SteelPlate, 5)] assembly (Time 10),
    
    r PetroleumGas 2 [(LightOil, 3)] ChemicalVenueKind (Time 5),
    
    r SolidFuel 1 [(LightOil, 10)] ChemicalVenueKind (Time 3),
    r LightOil 3[(HeavyOil, 4)] ChemicalVenueKind (Time 5),
    Recipe AdvancedOilProcessing [(HeavyOil, 10), (LightOil, 45), (PetroleumGas, 55)] [(CrudeOil, 100)] RefineryVenueKind (Time 5)
--    Recipe LiquefactionRecipe [(HeavyOil, 35), (LightOil, 15), (PetroleumGas, 20)] [(Coal, 10), (HeavyOil, 25), (Steam, 50)] RefineryVenueKind (Time 5)
  ] where
  r product quantity ingredients venues time = Recipe (ProductRecipe product) [(product, quantity)] ingredients venues time

mconcat' x = foldr add zero x

functionToMatrix :: (Num v, Ix' b, Ix' a) => (a -> [(b, v)]) -> Matrix a b v
functionToMatrix f =
  Matrix (Array.array fullRange [ ((a, b), maybe 0 id (Map.lookup b bs)) | a <- range fullRange, let bs = Map.fromListWith (+) (f a), b <- range fullRange])

recipesByName = Map.fromListWith (error "multiple recipes with the same name") (map (\recipe -> (recipeName recipe, recipe)) recipes)

recipesToMatrix :: (Recipe -> Config) -> Map RecipeName (Map Product Rat)
recipesToMatrix configs = 
  fmap (\recipe@(Recipe _recipeName production consumption venueKind (Time time)) ->
         (
         let config = configs recipe in
         let venue = configVenue config in
           let
             energy_and_pollution =
               let multiplier = (time / (speedMultiplier config * baseSpeed venue)) * energyMultiplier config in
               let pollution = basePollution venue in
               case basePower venue of
                 ElectricalPower basePower ->
                   [(ElectricalEnergy, basePower * multiplier), (Pollution, pollution * multiplier)]
                 ChemicalPower basePower ->
                   [(ChemicalEnergy, basePower * multiplier), (Pollution, pollution * multiplier)]
           in
             Map.fromListWith add (fmap (second negate) (consumption ++ energy_and_pollution) ++ map (second ((* productivityMultiplier config))) production)
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
currentRecipeMatrix = recipesToMatrix currentConfig

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
usability' ElectricalEnergy = Usable
usability' PetroleumGas = Unusable
usability' ResearchCoalLiquefaction = Unusable
usability' ResearchRocketSilo = Unusable
usability' ResearchNuclearPower = Unusable
usability' ResearchLaserTurretDamage5 = Unusable
usability' LightOil = Unusable
usability' HeavyOil = Unusable
usability' CrudeOil = Unusable
usability' Steam = Unusable
usability' LaserTurret = Usable
usability' Battery = Unusable
usability' PiercingRoundMagazine = Usable
usability' FirearmRoundMagazine = Usable
usability' Grenade = Usable
usability' GunTurret = Usable
usability' SciencePackMilitary = Unusable
usability' SteelFurnace = Usable
usability' StoneFurnace = Usable
usability' LabBuilding = Usable
usability' SteamEngineBuilding = Usable
usability' BoilerBuilding = Usable
usability' OilRefinery = Usable
usability' ChemicalPlant = Usable
usability' SolarFacilityBuilding = Usable
usability' SolarPanel = Usable
usability' Accumulator = Usable
usability' Substation = Usable
usability' Roboport = Usable
usability' x = error $ "undefined usability: " ++ show x

usability recipe =
  case recipeName recipe of
    ProductRecipe product -> usability' product
    LiquefactionRecipe -> Unusable
    AdvancedOilProcessing -> Unusable
    UseAsFuelRecipe product -> Unusable

evaluateTotalCost  :: RawMaterialPressure -> Rat
evaluateTotalCost f = sum [ (estimate k * v) | (k, v) <- Map.toList f, v /= zero] where
--  estimate LightOil = 0.1
  estimate CrudeOil = 0.1
  estimate HeavyOil = 0.1
  estimate BuriedCoal = 1
  estimate BuriedIron = 1
  estimate BuriedCopper = 1
  estimate BuriedStone = 0.3
  estimate Pollution = 0.0025
  estimate product = error $ "don't know how much this is worth: " ++ show product
--  estimate PetroleumGas = 0.1

subtract' a b = add a (minus b)

instance NFData Venue

data Assessment = Assessment {
  totalRawMaterials :: RawMaterialPressure,
  totalCapitalSeconds :: RawMaterialPressure
} deriving (Generic, Show)

instance NFData Assessment

instance Linear Assessment where
  zero = Assessment {
    totalRawMaterials = zero,
    totalCapitalSeconds = zero
    }
  a `add` b =
    Assessment
      {
        totalRawMaterials = add (totalRawMaterials a) (totalRawMaterials b),
        totalCapitalSeconds = add (totalCapitalSeconds a) (totalCapitalSeconds b)
      }
  minus a = 
    Assessment
      {
        totalRawMaterials = minus (totalRawMaterials a),
        totalCapitalSeconds = minus (totalCapitalSeconds a)
      }

instance VectorSpace Assessment where
  type Scalar Assessment = Rat
  scale x a =
    Assessment
      {
        totalRawMaterials = scale x (totalRawMaterials a),
        totalCapitalSeconds = scale x (totalCapitalSeconds a)
      }

capitalUsePerExecution configs recipe =
    let config = configs recipe in
    let Time time = recipeTime recipe in
    let execution_time = time / (speedMultiplier config * baseSpeed (configVenue config)) in
    let facility_cost = capitalCost config in
    scale execution_time facility_cost

type CapitalUse = RawMaterialPressure

assessConfigs' :: (Recipe -> Config) -> Map Product Rat -> (Map RecipeName CapitalUse, Assessment)
assessConfigs' configs =
  let solved = (solvedRecipes configs) in
  \demand -> runIdentity $ do
    (rawMaterials, executions) <- return $ dot_product_with scale demand solved
    capitalSeconds <- return $ Map.mapWithKey (\recipeName executions ->
        let Just recipe = Map.lookup recipeName recipesByName in
        scale executions (capitalUsePerExecution configs recipe)
      ) executions
    return $ (capitalSeconds, Assessment
      {
        totalRawMaterials = rawMaterials,
        totalCapitalSeconds = mconcat' (Map.elems capitalSeconds)
      })

assessConfigs configs =
  let a = assessConfigs' configs in
  \demand -> snd (a demand)

possibleSavings :: Map Product Rat -> Recipe -> [(Config, Assessment)]
possibleSavings demand recipe = (`using` parListChunk 10 rdeepseq) $
  let venueKind = recipeVenueKind recipe in
  let assessment_base = assessConfigs currentConfig demand in
  let Time time = recipeTime recipe in
    [ let assessment_tip = assessConfigs (\p -> if p == recipe then config else currentConfig p) demand in
      (config, add assessment_tip (minus assessment_base))
      | config <- availableConfigs venueKind (usability recipe)]

newtype Rat = Rat Double deriving (Eq, Ord, Generic, NFData, Linear, Num, Fractional, Real)

instance VectorSpace Rat where
  type Scalar Rat = Rat
  scale (Rat x) (Rat y) = Rat (x * y)

dot_product_with :: (Ord k, Linear a, Linear b, Linear c) => (a -> b -> c) -> Map k a -> Map k b -> c
dot_product_with f m1 m2 = foldr add zero $ Map.elems $
  Map.mergeWithKey
    (\k a b -> Just (f a b))
    (\_ -> Map.empty)
    (\_ -> Map.empty)
    m1
    m2

instance Show Rat where
  show (Rat x) = printf "%.4f" (fromRational $ toRational x :: Double)

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

venueBuilding :: Venue -> [Product]
venueBuilding venue = case venue of
  Assembly2 -> [AssemblingMachine2]
  Assembly3 -> [AssemblingMachine3]
  Miner -> [ElectricMiningDrill]
  SmelterElectric -> [ElectricFurnace]
  SmelterBurner -> [SteelFurnace]
  Chemical -> [ChemicalPlant]
  Lab -> [LabBuilding]
  Boiler -> [BoilerBuilding]
  SteamEngine -> [SteamEngineBuilding]
  SolarFacility -> [SolarFacilityBuilding]
  NoVenue -> []
  Refinery -> [OilRefinery]

capitalCost config =
  mconcat' $ map computeTotalCost (configModuleMaterials config ++ venueBuilding (configVenue config))

x = x
partition_market l
  =
  ( p (\(gain, cost) -> gain >= 0 && cost <= 0) (\(gain, cost) -> gain - cost) -- free money
  , p (\(gain, cost) -> gain > 0 && cost >= 0) (\(gain, cost) -> cost / gain) -- buy
  , p (\(gain, cost) -> gain < 0 && cost < 0) (\(gain, cost) -> gain / cost)
  ) where
  p predicate order = sortBy (comparing (order . extractGL)) $ filter (predicate . extractGL) l
  extractGL (_, _, gain, cost) = (gain, cost)

allRecipeNames = [recipeName recipe | recipe <- recipes]

venueKind_of_venue venue = case venue of
  Assembly2 -> AssemblyVenueKind
  Assembly3 -> AssemblyVenueKind
  SmelterBurner -> SmelterVenueKind
  SmelterElectric -> SmelterVenueKind
  Chemical -> ChemicalVenueKind
  Miner -> MinerVenueKind
  Lab -> LabVenueKind
  Boiler -> BoilerVenueKind
  SteamEngine -> SteamEngineVenueKind
  Refinery -> RefineryVenueKind
  NoVenue -> NoVenueVenueKind
  
isVenueDefault venue =
  venue == currentDefaultVenue (venueKind_of_venue venue)
possibleSavings' demand (Time totalTime) =
  let showVenue' venue | isVenueDefault venue = "" | otherwise = "+" in
  [ (recipeName, showVenue' venue ++ (concatMap showModule modules), saving, cost + installationCost)
  | recipe <- recipes
  , recipeName <- return $ recipeName recipe
  , (config, Assessment {
              totalRawMaterials,
              totalCapitalSeconds}) <- possibleSavings demand recipe
  , let venue = configVenue config
  , let modules = configModuleMaterials config
  , let saving = evaluateTotalCost $ minus totalRawMaterials
  , let cost = evaluateTotalCost $ scale (recip totalTime) totalCapitalSeconds
  ]

installationCost = 1000

desiredMaterials =
  [ (ResearchNuclearPower,  1)
  , (PiercingRoundMagazine, 10000)
  , (ProductivityModule2, 200)
  , (SciencePackProduction, 1000)
  , (SciencePackHighTech, 500)
  , (SciencePackMilitary, 500)
  ]

lookup0 m k = case Map.lookup k m of
  Nothing -> zero
  Just x -> x

currentRecipeVenue recipe =
  let config = (currentConfig recipe) in
  configVenue config

currentEffectiveExecutionTime recipeName =
  let recipe = (recipesByName Map.! recipeName) in
  unTime (recipeTime recipe)
     / (speedMultiplier (currentConfig recipe) * baseSpeed (currentRecipeVenue recipe))

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

interestingProducts = []


print_config_details totalTime demand configs =
  let solved = solvedRecipes configs in
  let matrix = recipesToMatrix configs in
  let
   (total_cost_per_second, executions_per_second) =
    foldr add zero (
      map
        (\(product, amount) ->
            scale (recip $ unTime totalTime) (scale amount $ solved Map.! product)
            ) desiredMaterials)
  in
  let
    negative_executions_per_second =
      filter
       ((<0) . snd)
        (Map.toList executions_per_second)
  in
  let
   compute_total_cost product =
    let recipes = fmap fst $ solved in
    lookup0 recipes product
  in
  let
   effective_execution_time recipeName =
    let recipe = (recipesByName Map.! recipeName) in
    unTime (recipeTime recipe)
       / (speedMultiplier (configs recipe) * baseSpeed (configVenue (configs recipe)))
  in
  do
    mapM_ print negative_executions_per_second
    print "total factories:"
    let factories k = flip fmap (Map.lookup k executions_per_second) (* effective_execution_time k)
    let (capital_use_per_recipe, Assessment raw total_capital_use) = assessConfigs' configs demand
    let scale_capital_use = scale (recip $ unTime totalTime)
    printTableG (sortBy (comparing factories) allRecipeNames) $
      [ ("Name", show)
      , ("Factories", maybe "<none>" show . factories)
      , ("Price",
          \k ->
            case k of
              ProductRecipe product ->
                show $ evaluateTotalCost $ computeTotalCost product
              _ -> "<complex>")
      , ("Capital", (\k -> show $ evaluateTotalCost $ scale_capital_use (lookup0 capital_use_per_recipe k)))
      ] ++ flip map interestingProducts (\product -> (show product, (\k -> show $ lookup0 (matrix Map.! k) product * (lookup0 executions_per_second k))))
    print "total cost"
    print $ evaluateTotalCost (scale (unTime totalTime) total_cost_per_second)
    print "total capital"
    print (evaluateTotalCost $ scale_capital_use total_capital_use)

report =
  let mkConfig assembly modules = Config assembly (mconcat $ map moduleToConfig modules) modules in
  let mkConfigs r config = (\p -> if recipeName p == r then config else currentConfig p) in
  let futureFactor = 3 in
  let totalTime = Time (4500 * futureFactor) in
  let
   demand =
    Map.fromList desiredMaterials
  in
  let savings = possibleSavings' (scale futureFactor demand) totalTime in
   do
    let (free_money, buys, sells) = partition_market savings
    print_config_details totalTime demand currentConfig
    print "free money"
    printRs (take 20 free_money)
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

p1 = ModuleEnhancement ProductivityModule
p2 = ModuleEnhancement ProductivityModule2
p3 = ModuleEnhancement ProductivityModule3
s1 = ModuleEnhancement SpeedModule
s2 = ModuleEnhancement SpeedModule2
e1 = ModuleEnhancement EfficiencyModule
p = VenueEnhancement Assembly3

currentDefaultVenue :: VenueKind -> Venue
currentDefaultVenue AssemblyVenueKind = Assembly2
currentDefaultVenue SmelterVenueKind = SmelterElectric
currentDefaultVenue venueKind = case venuesByKind venueKind of
  [ venue ] -> venue
  _ -> error "ambiguous venue"

data CumulativeEnhancement = CumulativeEnhancement
  {
    cumulativeEnhancementVenue :: Maybe Venue,
    cumulativeEnhancementModules :: [Product]
  }

equal_if_Just (Just a) (Just b)
  | a == b = Just a
  | otherwise = error "two different venues chosen at the same time"
equal_if_Just Nothing b = b
equal_if_Just a Nothing = a

instance Monoid CumulativeEnhancement where
  mempty = CumulativeEnhancement
    {
      cumulativeEnhancementVenue = Nothing,
      cumulativeEnhancementModules = []
    }
  mappend a b = CumulativeEnhancement
    {
      cumulativeEnhancementVenue =
        equal_if_Just (cumulativeEnhancementVenue a) (cumulativeEnhancementVenue b),
      cumulativeEnhancementModules =
        cumulativeEnhancementModules a ++ cumulativeEnhancementModules b
    }

data Enhancement =
  ModuleEnhancement Product
  | VenueEnhancement Venue

collectEnhancements :: [Enhancement] -> CumulativeEnhancement
collectEnhancements = mconcat . map toCumulative where
  toCumulative (ModuleEnhancement m) = CumulativeEnhancement
    {
      cumulativeEnhancementVenue = Nothing,
      cumulativeEnhancementModules = [m]
    }
  toCumulative (VenueEnhancement v) = CumulativeEnhancement
    {
      cumulativeEnhancementVenue = Just v,
      cumulativeEnhancementModules = []
    }

currentEnhancements ProcessingUnit = [p, s1, p2, p2, p2]
currentEnhancements GearWheel = [p, p2, p1, p1, s1]
--currentEnhancements ElectronicCircuit = [p, p2, p2, p1, s1]
--currentEnhancements SciencePack3 = [p, p2, p2, p1, s1]
-- currentEnhancements SciencePackHighTech = [p, p2, p2, p2, p2]
-- rentEnhancements SciencePackProduction = [p, p2, p2, p2, s1]
currentEnhancements ResearchNuclearPower = [p1, p1]
currentEnhancements Plastic = [p2, p2, p2]
currentEnhancements SciencePackMilitary = [p, p2, p2, p1, s1]
currentEnhancements SulfuricAcid = [p2, p2, p2]
currentEnhancements AdvancedCircuit = [p, e1, e1, p1, p1]

-- todo: insert these:
currentEnhancements EngineUnit = [e1, e1]
currentEnhancements SciencePack1 = [e1, e1]
currentEnhancements SciencePack2 = [e1, e1]
currentEnhancements LightOil = [e1, e1, e1]
currentEnhancements CopperCable = [e1, e1]
currentEnhancements PetroleumGas = [e1, e1, e1]
currentEnhancements IronOre = [e1, e1, e1]
currentEnhancements CopperOre = [e1, e1, e1]

currentEnhancements SciencePackHighTech = [p, p3, p3, p3, s2]
currentEnhancements SciencePackProduction = [p, p2, p2, p2, s1]
currentEnhancements ElectronicCircuit = [p, p2, p2, p2, s1]
currentEnhancements SciencePack3 = [p, p2, p2, p2, s1]

currentEnhancements _ = []

trivial recipe =
  (currentDefaultVenue (recipeVenueKind recipe), [])

currentModules recipe =
  case recipeName recipe of
    ProductRecipe product ->
      let enhancements = collectEnhancements $ currentEnhancements product in
      let
        venue =
          fromMaybe
            (currentDefaultVenue (recipeVenueKind recipe))
            (cumulativeEnhancementVenue enhancements)
      in
      (venue, cumulativeEnhancementModules enhancements)
    LiquefactionRecipe -> trivial recipe
    AdvancedOilProcessing ->
      ((currentDefaultVenue (recipeVenueKind recipe)), cumulativeEnhancementModules (collectEnhancements [e1, e1, e1]))
    _ -> trivial recipe

--main = print $ computeTotalCost SciencePack3
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


