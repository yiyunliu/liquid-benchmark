module Main where

import           Language.Haskell.Liquid.Liquid
import           System.Exit
import           Control.Monad
import           Options.Applicative
import           Control.Exception
import qualified Data.Map.Strict               as Map
import           Numeric.Statistics.Moment
import           System.Process
import           System.TimeIt
import           Pipes
import qualified Pipes.Prelude                 as P


data Config = Config
  { cores :: Int
  , benchType :: BenchmarkType
  , numTimes :: Int
  , fast :: Bool }

config :: Parser Config
config = Config
  <$> option auto
    ( long "cores"
   <> help "Number of cores used to solve constraints"
   <> showDefault
   <> value 4
   <> metavar "INT")
  <*> benchmarkType
  <*> option auto
    ( long "times"
   <> help "Number of times to run the benchmark"
   <> showDefault
   <> value 3
   <> metavar "INT")
  <*> switch
    ( long "fast"
   <> help "Leave out the tests that take too much time or space to finish."
   <> short 'f')

benchmarkType :: Parser BenchmarkType
benchmarkType = vrdt <|> liquidBase

vrdt :: Parser BenchmarkType
vrdt = flag' VRDT
  (long "vrdt"
  <> help "Run the benchmark for vrdt")

liquidBase :: Parser BenchmarkType
liquidBase = flag' LiquidBase
  (long "liquid-base"
  <> help "Run the benchmark for liquid-base")

data BenchmarkType = VRDT | LiquidBase 

mkBenchFromConfig :: Config -> Producer (String, [Double]) IO ()
mkBenchFromConfig (Config cores VRDT _ fast) =
  benchmarkAll cores vrdtDir (if fast then vrdtModules else vrdtModules ++ vrdtModulesSlow)
mkBenchFromConfig (Config cores LiquidBase _ _) =
  benchmarkAll cores liquidBaseDir $
  dependencies ++ functors ++ functorsSlow ++ semigroups ++ foldables


vrdtDir :: String
vrdtDir = "vrdt/vrdt/src/"

liquidBaseDir :: String
liquidBaseDir = "liquid-base/liquid-base/src/"

benchmarkAll :: Int -> String -> [String] -> Producer (String, [Double]) IO ()
benchmarkAll cores dir files =
  lift
      (callCommand $ "find " <> dir <> " -name \".liquid\" | xargs rm -rf")
    >> (each files >-> forever (benchmarkOne cores dir))

benchmarkOne :: Int -> String -> Pipe String (String, [Double]) IO ()
benchmarkOne cores dir = do
  file   <- await
  (t, _) <- timeItT $ lift (removeTmpAndRunLiquid cores dir file)
  yield (file, [t])

removeTmpAndRunLiquid :: Int -> String -> String -> IO ()
removeTmpAndRunLiquid cores dir str = do
  putStrLn $ unwords ("liquid":allArgs)
  handle handler
    $  liquid allArgs
 where
  handler ExitSuccess = pure ()
  handler e           = throw e

  allArgs = 
      ["-i", dir]
   <> ["--cores=" ++ show cores]
   <> args
   <> [dir <> str]

  args =
    words
      "--typeclass --ghc-option=-XTypeFamilies --ghc-option=-XFlexibleContexts --ghc-option=-XBangPatterns --ghc-option=-XTypeFamilyDependencies --ghc-option=-cpp"


dependencies :: [FilePath]
dependencies =
  [ "Data/Semigroup/Classes.hs"
  , "Data/Functor/Classes.hs"
  , "Data/Any.hs"
  , "Data/Functor/Const.hs"
  , "Data/Dual.hs"
  , "Data/Either.hs"
  , "Data/Endo.hs"
  , "Data/Function.hs"
  , "Data/Functor/Identity.hs"
  , "Data/Functor/State.hs"
  , "Data/List.hs"
  , "Data/List/NonEmpty.hs"
  , "Data/Maybe.hs"
  , "Data/PNat.hs"
  , "Data/Reader.hs"
  , "Liquid/ProofCombinators.hs"
  ]


functors :: [FilePath]
functors =
  [ "Data/Either/Functor.hs" -- VMonad 3
  , "Data/Functor/Const/Functor.hs" -- VFunctor 1
  , "Data/Functor/Identity/Functor.hs" -- VMonad 3
  , "Data/Functor/State/Functor.hs" -- VFunctor 1
  , "Data/List/Functor.hs" -- VMonad 3
  , "Data/Maybe/Functor.hs" -- VMonad 3
  , "Data/Reader/Functor.hs" -- VApplicative 2
  ]

functorsSlow :: [FilePath]
functorsSlow = pure "Data/Successors/Functor.hs"

semigroups :: [FilePath]
semigroups =
  [ "Data/All/Semigroup.hs" -- VMonoid 2
  , "Data/Any/Semigroup.hs" -- VMonoid 2
  , "Data/Dual/Semigroup.hs" -- VMonoid 2
  , "Data/Endo/Semigroup.hs" -- VMonoid 2
  , "Data/Functor/Identity/Semigroup.hs" -- VMonoid 2
  , "Data/List/Semigroup.hs" -- VMonoid 2
  , "Data/Maybe/Semigroup.hs" -- VMonoid 2
  , "Data/Num/Semigroup.hs" -- VSemigroup 1
  , "Data/PNat/Semigroup.hs" -- VMonoid 2
  ]

foldables :: [FilePath]
foldables =
  [ "Data/Functor/Const/Foldable.hs"
  , "Data/Functor/Identity/Foldable.hs"
  , "Data/List/Foldable.hs"
  , "Data/Maybe/Foldable.hs"
  ]

vrdtModules :: [FilePath]
vrdtModules =
  [
  -- Dependencies
    "Liquid/Data/Maybe.hs"
  , "Liquid/ProofCombinators.hs"
  , "Liquid/Data/Map.hs"
  , "Liquid/Data/Map/Props.hs"
  , "Liquid/Data/List.hs"
  , "VRDT/Class.hs"
  , "VRDT/Internal.hs"
  , "VRDT/Class/Proof.hs" -- dependency of twopmap

  -- Benchmarks
  , "VRDT/Max.hs"
  , "VRDT/Min.hs"
  , "VRDT/Sum.hs"
  , "VRDT/LWW.hs"
  , "VRDT/MultiSet.hs"
  , "VRDT/MultiSet/Proof.hs"
--   , "VRDT/TwoPMap.hs"
  , "Event/Types.hs"
--   , "VRDT/CausalTree.hs"
  ]

vrdtModulesSlow :: [FilePath]
vrdtModulesSlow = ["VRDT/CausalTree.hs", "VRDT/TwoPMap.hs"]

opts :: ParserInfo Config
opts = info (config <**> helper)
  (fullDesc
  <> progDesc "Run the benchmark for vrdt or liquid-base"
  <> header "liquid-benchmark - a driver for running LH typeclass benchmark")

main :: IO ()
main = do

  cfg <- execParser opts
  res <- P.fold (\m (path, t) -> Map.insertWith (++) path t m)
                mempty
                id
                (replicateM_ (numTimes cfg) (mkBenchFromConfig cfg))
  mapM_
      (\(filepath, times) -> do
        putStrLn filepath
        print times
        putStrLn $ show (mean times) <> " (" <> show (stddev times) <> ")"
        putStrLn ""
      )
    $ Map.toList res
