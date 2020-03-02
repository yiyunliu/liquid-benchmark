module Main where

import           Criterion.Main
import           Language.Haskell.Liquid.Liquid
import           System.Exit
import           System.Directory
import           Control.Monad
import           Control.Exception
import           Criterion.Types

removeTmpAndRunLiquid :: String -> IO ()
removeTmpAndRunLiquid str =
  handle ((\_ -> pure ()) :: ExitCode -> IO ())
    . withCurrentDirectory "liquid-base/liquid-base/src"
    $ liquid [str]




functors :: [FilePath]
functors =
  [ "Data/Either/Functor.hs" -- VMonad
  , "Data/Functor/Const/Functor.hs" -- VFunctor
  , "Data/Functor/Identity/Functor.hs" -- VMonad
  , "Data/Functor/State/Functor.hs" -- VFunctor
  , "Data/List/Functor.hs" -- VMonad
  , "Data/Maybe/Functor.hs" -- VMonad
  , "Data/Reader/Functor.hs" -- VApplicative
  ]

semigroups :: [FilePath]
semigroups =
  [ "Data/All/Semigroup.hs" -- VMonoid
  , "Data/Any/Semigroup.hs" -- VMonoid
  , "Data/Dual/Semigroup.hs" -- VMonoid 
  , "Data/Endo/Semigroup.hs" -- VMonoid
  , "Data/Functor/Identity/Semigroup.hs" -- VMonoid
  , "Data/List/Semigroup.hs" -- VMonoid
  , "Data/Maybe/Semigroup.hs" -- VMonoid
  , "Data/Num/Semigroup.hs" -- VSemigroup
  , "Data/PNat/Semigroup.hs" -- VMonoid
  ]

foldables :: [FilePath]
foldables =
  [ "Data/Functor/Const/Foldable.hs"
  , "Data/Functor/Identity/Foldable.hs"
  , "Data/List/Foldable.hs"
  , "Data/Maybe/Foldable.hs"
  ]

makeBench :: FilePath -> Benchmark
makeBench f = bench f $ nfIO (removeTmpAndRunLiquid f)

main :: IO ()
main = defaultMainWith
  defaultConfig
  [ bgroup "Semigroup" $ fmap makeBench semigroups
  , bgroup "Functor" $ fmap makeBench functors
  -- very very slow!
  -- , bgroup "Succs"    [makeBench "Data/Successors/Functor.hs"]
  , bgroup "Foldable" $ fmap makeBench foldables
  ]


