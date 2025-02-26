{-# LANGUAGE CPP #-}

import           Clash.Driver
import           Clash.Driver.Types

import           Clash.GHC.PartialEval
import           Clash.GHC.Evaluator

import qualified Control.Concurrent.Supply    as Supply
import           Control.DeepSeq              (deepseq)
import           Data.Binary                  (decode)
import           Data.List                    (partition)
import           System.Environment           (getArgs)

import qualified Data.ByteString.Lazy as B

import           SerialiseInstances
import           BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let (idirs0,tests0) = partition ((== "-i") . take 2) args
  let tests1 | null tests0 = defaultTests
             | otherwise = tests0
      idirs1 = ".":map (drop 2) idirs0

  res <- mapM (benchFile idirs1) tests1
  deepseq res $ return ()

benchFile :: [FilePath] -> FilePath -> IO ()
benchFile idirs src = do
  supplyN <- Supply.newSupply
  env <- setupEnv src
  putStrLn $ "Doing normalization of " ++ src
  let (bindingsMap,tcm,tupTcm,primMap,reprs,topEntityNames,topEntity) = env
      primMap' = fmap (fmap unremoveBBfunc) primMap
      res :: BindingMap
      res = normalizeEntity reprs bindingsMap primMap' tcm tupTcm typeTrans
                   ghcEvaluator
                   evaluator
                   topEntityNames (opts idirs) supplyN topEntity
  res `deepseq` putStrLn ".. done\n"

setupEnv :: FilePath -> IO NormalizationInputs
setupEnv src = do
  let bin = src ++ ".bin"
  putStrLn $ "Reading from: " ++ bin
  decode <$> B.readFile bin
