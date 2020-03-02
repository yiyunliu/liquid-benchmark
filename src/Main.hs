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
  flip catch ((\_ -> pure ()) :: ExitCode -> IO ())
    . withCurrentDirectory "liquid-base/liquid-base/src"
    $ do

        b <- doesPathExist tmpPath
        when b (removeDirectoryRecursive tmpPath)
        liquid [str]
  where tmpPath = str <> "/.liquid"



functors :: [FilePath]
functors =
  [ "Data/Either/Functor.hs"
  , "Data/Functor/Const/Functor.hs"
  , "Data/Functor/Identity/Functor.hs"
  , "Data/Functor/State/Functor.hs"
  , "Data/List/Functor.hs"
  , "Data/Maybe/Functor.hs"
  , "Data/Reader/Functor.hs"
  -- , "Data/Successors/Functor.hs"
  ]

makeBench :: FilePath -> Benchmark
makeBench f = bench f $ nfIO (removeTmpAndRunLiquid f)

main :: IO ()
main =
  defaultMainWith defaultConfig { resamples = 5 } $ fmap makeBench functors


