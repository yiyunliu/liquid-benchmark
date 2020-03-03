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
  handle handler
    . withCurrentDirectory "liquid-base/liquid-base/src"
    $ liquid [str]

  where
    handler ExitSuccess = pure ()
    handler e           = throw e




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
makeBench f = bench f $ nfIO (removeTmpAndRunLiquid f)

main :: IO ()
main = defaultMainWith
  defaultConfig
  [ bgroup "Semigroup" $ fmap makeBench semigroups
  , bgroup "Functor" $ fmap makeBench functors
  -- very very slow!
  , bgroup "Foldable" $ fmap makeBench foldables
  , bgroup "Succs"    [makeBench "Data/Successors/Functor.hs"]
  ]


