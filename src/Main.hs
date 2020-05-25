module Main where

import           Criterion.Main
import           Language.Haskell.Liquid.Liquid
import           System.Exit
import           System.Directory
import           Control.DeepSeq
import           Control.Monad
import           Control.Exception
-- import           Criterion.Measurement (time_)
import           Criterion.Types
import qualified Data.Map as Map
import           Numeric.Statistics.Moment
import           System.Process
import           System.TimeIt

benchmark :: Int -> [String] -> IO (Map.Map String [Double])
benchmark n files = helper mempty n files

  where
    helper acc c _ | c <= 0 = return acc
    helper acc c []        = do
        -- delete temp
        callCommand $ "find " <> dir <> " | grep \".liquid\" | xargs rm -rf"
        
        helper acc (c-1) files
    helper acc c (f:fs)    = do
      t <- fmap fst $ timeItT $ do
        res <- removeTmpAndRunLiquid dir f
        return $ force res

      helper (Map.insertWith (++) f [t] acc) c fs

    -- WARNING: Be careful with this string! It is sent to a shell command.
    dir = "vrdt/vrdt/src/"


removeTmpAndRunLiquid :: String -> String -> IO ()
removeTmpAndRunLiquid dir str =
  handle handler
    -- . withCurrentDirectory dir -- "liquid-base/liquid-base/src"
    $ liquid $ ["-i", dir] <> args <> [dir <> str]

  where
    handler ExitSuccess = pure ()
    handler e           = throw e

    args = words "--typeclass --ghc-option=-XTypeFamilies --ghc-option=-XFlexibleContexts --ghc-option=-XBangPatterns --ghc-option=-XTypeFamilyDependencies --ghc-option=-cpp"


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
makeBench f = bench f $ nfIO (removeTmpAndRunLiquid "liquid-base/liquid-base/src" f)

vrdt = "VRDT/Class.hs"
strongconvergence = "VRDT/Class/Proof.hs"
multiset = "VRDT/MultiSet.hs" -- , "vrdt/vrdt/src/VRDT/MultiSet/Internal.hs"]


vrdtModules :: [FilePath]
vrdtModules = [
  -- Dependencies
    "Liquid/Data/Maybe.hs"
  , "Liquid/ProofCombinators.hs"
  , "Liquid/Data/Map.hs"
  , "Liquid/Data/Map/Props.hs"
  , "Liquid/Data/List.hs"
  , "VRDT/Class.hs"
  

  -- Benchmarks
  , "VRDT/Max.hs"
  
  , "VRDT/Min.hs"
  
  , "VRDT/Sum.hs"
  
  , "VRDT/LWW.hs"

  , "VRDT/MultiSet/Internal.hs"
  , "VRDT/MultiSet.hs"
  , "VRDT/MultiSet/Proof.hs"
  
  , "VRDT/Class/Proof.hs" -- dependency of twopmap

  , "VRDT/Internal.hs" -- despite the module name, it's only used in twopmap
  , "TwoPMap/Internal.hs"
  , "TwoPMap/LemmaID.hs"
  , "TwoPMap/LemmaDD.hs"
  , "TwoPMap/LemmaAD.hs"
  , "TwoPMap/LemmaDA.hs"
  , "TwoPMap/LemmaDI.hs"
  , "TwoPMap/LemmaII.hs"
  , "TwoPMap/LemmaAA.hs"
  , "TwoPMap/LemmaIA.hs"
  , "TwoPMap/LemmaAI.hs"
  , "VRDT/TwoPMap.hs"
  ]


main :: IO ()
-- main = defaultMainWith
--   defaultConfig
--   [ bgroup "Dependencies" $ fmap makeBench dependencies
--   , bgroup "Semigroup" $ fmap makeBench semigroups
--   , bgroup "Functor" $ fmap makeBench functors
--   -- very very slow!
--   , bgroup "Foldable" $ fmap makeBench foldables
--   , bgroup "Succs"    [makeBench "Data/Successors/Functor.hs"]
--   ]
main = do

  -- Map FilePath [Time]
  res <- benchmark n vrdtModules
  -- let res = Map.fromList [("test", [1,2,3])]
  mapM_ (\(filepath, times) -> do
      putStrLn filepath
      putStrLn $ show times
      putStrLn $ show (mean times) <> " (" <> show (stddev times) <> ")"
      putStrLn ""
    
    ) $ Map.toList res

  where
    n = 3


