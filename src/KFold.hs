{-# LANGUAGE GADTs #-}

module KFold where

import           DataSet
import           Knn
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Ord as ORD
import qualified Data.Eq as EQU

import Data.Function (on)

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


validateStep :: (TrainingsData t) => Int -> KFoldStep t -> [ValidationResult]
validateStep k (KFoldStep train test) = map (\d -> ValidationResult (label d) (classify tree k d)) testData
  where
    testData = getData test
    tree = buildTree train

data ValidationResult = ValidationResult
  { actual    :: Label
  , predicted :: Label
  } deriving (Show, Eq, Ord)

summarizeResults :: [[(ValidationResult, Int)]] -> [(ValidationResult, Double)]
summarizeResults l = (toAvg . length $ l) . sums <$> (grped . concat) l
  where
    toAvg l (r, i) = (r, fromIntegral i / fromIntegral l)
    sums = foldl1 (\r1 r2 -> (ValidationResult (actual . fst $ r1) (predicted . fst $ r2), snd r1 + snd r2))
    grped = L.groupBy ((==) `on` fst) . L.sortBy (ORD.compare `on` fst)