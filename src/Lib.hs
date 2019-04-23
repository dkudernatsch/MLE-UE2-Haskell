{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runKFold
  , module Csv
  , module DataSet
  , module Knn
  , module KFold
  ) where

import           Csv
import           DataSet
import           Errors
import           KFold
import           Knn

import           Data.List
import           Control.Monad

import qualified Data.Vector as V


runKFold:: Int -> Int -> DataSet -> [(ValidationResult, Double)]
runKFold folds k dataSetResult =
  let kfold = fromDataSet dataSetResult folds
      steps = kFoldSteps kfold
      valid = map (validateStep k) steps
      valid2 = fmap (group.sort) valid
      counts = (map . map) (\l-> (head l, length l))
  in summarizeResults . counts $ valid2

