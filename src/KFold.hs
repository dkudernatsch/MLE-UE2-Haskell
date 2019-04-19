{-# LANGUAGE GADTs #-}

module KFold where

import           DataSet
import           Knn

newtype KFold =
  KFold [DataSet]
  deriving (Show, Eq)

fromDataSet :: DataSet -> Int -> KFold
fromDataSet d i = KFold (split d i)

kFoldSteps :: KFold -> [KFoldStep (Header, [DataEntry])]
kFoldSteps (KFold d) = map (kFoldStep d) steps
  where
    kFoldStep d i = KFoldStep (normalize m std train) (uncurry DataSet (normalize m std test))
      where
        train = without d i
        test = single d i
        m = mean train
        std = stdDev train
    single d i = d !! i
    without d i = take (i - 1) d ++ drop (i + 1) d
    steps = [0 .. length d - 1]

data KFoldStep t where
  KFoldStep :: (TrainingsData t) => t -> DataSet -> KFoldStep t


validateStep :: (TrainingsData t) => KFoldStep t -> [ValidationResult]
validateStep (KFoldStep train test) = map (\d -> ValidationResult (label d) (classify tree 2 d)) testData
  where
    testData = getData test
    tree = buildTree train

data ValidationResult = ValidationResult
  { actual    :: Label
  , predicted :: Label
  } deriving (Show, Eq, Ord)
