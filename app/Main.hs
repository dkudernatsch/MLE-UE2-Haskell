{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import Lib

import qualified Control.Monad.Random as RND

import System.Console.CmdArgs

main :: IO ()
main = do
   kfoldArgs <- cmdArgs kfold
   dataSetResult <- readCsv (csvFile kfoldArgs)
   case dataSetResult of
      Left err -> print err
      Right dataSet -> do
        let rndDataset = shuffle dataSet
        dataset <- RND.evalRandIO rndDataset
        -- 10 folds with k=7
        let result = runKFold (read . folds $ kfoldArgs) (read . k $ kfoldArgs) dataset
        mapM_ print result


data KFold = KFold
  { folds::String
  , k::String
  , csvFile::FilePath
  } deriving(Typeable, Data, Show)

kfold = Main.KFold
  { folds = def &= name "f" &= name "folds" &= opt "10" &= typ "INT"
  , k = def &= name "k" &= opt "7" &= typ "INT"
  , csvFile = def &= args &= typ "FILE"
  } &= help "KFolf Knn classification"