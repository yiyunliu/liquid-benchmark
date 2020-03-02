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
  [ "Data/Either/Functor.hs"
  , "Data/Functor/Const/Functor.hs"
  , "Data/Functor/Identity/Functor.hs"
  , "Data/Functor/State/Functor.hs"
  , "Data/List/Functor.hs"
  , "Data/Maybe/Functor.hs"
  , "Data/Reader/Functor.hs"
  ]

semigroups :: [FilePath]
semigroups =
  [ "Data/All/Semigroup.hs"
  , "Data/Any/Semigroup.hs"
  , "Data/Dual/Semigroup.hs"
  , "Data/Endo/Semigroup.hs"
  , "Data/Functor/Identity/Semigroup.hs"
  , "Data/List/Semigroup.hs"
  , "Data/Maybe/Semigroup.hs"
  , "Data/Num/Semigroup.hs"
  , "Data/PNat/Semigroup.hs"
  ]


makeBench :: FilePath -> Benchmark
makeBench f = bench f $ nfIO (removeTmpAndRunLiquid f)

main :: IO ()
main = defaultMainWith
  defaultConfig
  [ bgroup "Semigroup" $ fmap makeBench semigroups
  , bgroup "Functor" $ fmap makeBench functors
  -- very very slow!
  , bgroup "Succs" [makeBench "Data/Successors/Functor.hs"]
  , bgroup "Foldable" [makeBench "Data/Foldable/Classes.hs"]
  ]


