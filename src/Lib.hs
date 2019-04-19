{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import           Csv
import           DataSet
import           Errors
import           KFold
import           Knn

import           Data.List
import Control.Monad

import qualified Data.Vector as V
import qualified Control.Monad.Random as RND


runProgram:: DataSet -> [[(ValidationResult, Int)]]
runProgram dataSetResult =
  let kfold = fromDataSet dataSetResult 10
      step1 = kFoldSteps kfold
      valid = map validateStep step1
      valid2 = fmap (group.sort) valid
      counts = (map . map) (\l-> (head l, length l))
  in counts valid2

someFunc :: IO ()
someFunc = do
  dataSetResult <- readCsv "C:\\Users\\Daniel\\IdeaProjects\\mle-ue2\\data\\iris.data"
  case dataSetResult of
    Left err -> print err
    Right dataSet -> do
      let rndDataset = shuffle dataSet
      dataset <- RND.evalRandIO rndDataset
      let result = runProgram dataset
      mapM_ print result

