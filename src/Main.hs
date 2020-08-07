module Main where

import           Criterion.Main
import           Language.Haskell.Liquid.Liquid
import           Control.DeepSeq                (force)
import           System.Exit
import           Control.Monad
import           Control.Exception
import qualified Data.Map.Strict               as Map
import           Numeric.Statistics.Moment
import           System.Process
import           System.TimeIt
import           Pipes
import qualified Pipes.Prelude                 as P

vrdtDir :: String
vrdtDir = "vrdt/vrdt/src/"

benchmarkAll :: [String] -> Producer (String, [Double]) IO ()
benchmarkAll files =
  lift
      (callCommand $ "find " <> vrdtDir <> " | grep \".liquid\" | xargs rm -rf")
    >> (each files >-> forever benchmarkOne)

benchmarkOne :: Pipe String (String, [Double]) IO ()
benchmarkOne = do
  file   <- await
  (t, _) <- timeItT $ lift (removeTmpAndRunLiquid vrdtDir file)
  yield (file, [t])

removeTmpAndRunLiquid :: String -> String -> IO ()
removeTmpAndRunLiquid dir str =
  handle handler
    $  liquid
    $  ["-i", dir]
    <> args
    <> [dir <> str]

 where
  handler ExitSuccess = pure ()
  handler e           = throw e

  args =
    words
      "--cores=12 --typeclass --ghc-option=-XTypeFamilies --ghc-option=-XFlexibleContexts --ghc-option=-XBangPatterns --ghc-option=-XTypeFamilyDependencies --ghc-option=-cpp"


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

makeBench :: FilePath -> Benchmark
makeBench f =
  bench f $ nfIO (removeTmpAndRunLiquid "liquid-base/liquid-base/src" f)

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
  , "VRDT/TwoPMap.hs"
  , "Event/Types.hs"
  , "VRDT/CausalTree.hs"
  ]


main :: IO ()
main = do
  res <- P.fold (\m (path, t) -> force $ Map.insertWith (++) path t m)
                mempty
                id
                (replicateM_ 1 (benchmarkAll vrdtModules))
  mapM_
      (\(filepath, times) -> do
        putStrLn filepath
        print times
        putStrLn $ show (mean times) <> " (" <> show (stddev times) <> ")"
        putStrLn ""
      )
    $ Map.toList res
