{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module DataSet where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as CSV
import qualified Data.List.Split      as LS
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified System.Random.Shuffle as SH

import qualified Control.Monad.Random as RND

import           Errors



class TrainingsData d where
  getHeader :: TrainingsData d => d -> Header
  getData :: TrainingsData d => d -> [DataEntry]

instance TrainingsData DataSet where
  getHeader = header
  getData = data_lines

instance (TrainingsData d) => TrainingsData [d] where
  getHeader = getHeader . head
  getData = (=<<) getData

instance TrainingsData (Header, [DataEntry]) where
  getHeader (h, _) = h
  getData (_, d) = d

normalize :: (TrainingsData t) => V.Vector Double -> V.Vector Double -> t -> (Header, [DataEntry])
normalize mean stddev t = (getHeader t, map (mapData (norm mean stddev)) (getData t))
  where
    norm :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double
    norm m s d = V.zipWith (/) (V.zipWith (-) d m) s

stdDev :: (TrainingsData t) => t -> V.Vector Double
stdDev t = V.map (\sum -> sqrt (sum / (fromIntegral . (+ (-1)) . length $ datas))) sum
  where
    sum = foldl1 vAdd (map mapRow datas)
    mapRow v = V.zipWith (\v1 v2 -> (v1 - v2) ^ 2) v (mean t)
    vAdd = V.zipWith (+)
    datas = map dataPoint (getData t)

mean :: (TrainingsData t) => t -> V.Vector Double
mean t = V.map (\sum -> sum / (fromIntegral . length $ datas)) (sum datas)
  where
    sum = foldl1 vAdd
    datas = map dataPoint (getData t)
    vAdd = V.zipWith (+)

data DataSet = DataSet
  { header     :: Header
  , data_lines :: DataLines
  } deriving (Eq, Show)

split :: DataSet -> Int -> [DataSet]
split (DataSet h d) i = map (DataSet h) (LS.chunksOf (length d `div` i) d)

shuffle :: (RND.MonadRandom m) => DataSet -> m DataSet
shuffle (DataSet h d) = RND.liftM (DataSet h) (SH.shuffleM d)


newtype Header =
  Header [T.Text]
  deriving (Eq, Show)

type DataLines = [DataEntry]

newtype Label =
  Label T.Text
  deriving (Eq, Show, Ord)

intoLabel :: T.Text -> Label
intoLabel = Label

data DataEntry = DataEntry
  { dataPoint :: V.Vector Double
  , label     :: Label
  } deriving (Eq, Show)

mapData :: (V.Vector Double -> V.Vector Double) -> DataEntry -> DataEntry
mapData f d = d {dataPoint = f (dataPoint d)}


irisDataSet :: DataSet
irisDataSet =
  DataSet
    { header = Header ["sepal length", "sepal width", "petal length", "petal width", "label"]
    , data_lines =
        [ DataEntry {dataPoint = V.fromList [5.1, 3.5, 1.4, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.9, 3.0, 1.4, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.7, 3.2, 1.3, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.6, 3.1, 1.5, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.0, 3.6, 1.4, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.4, 3.9, 1.7, 0.4], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.6, 3.4, 1.4, 0.3], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.0, 3.4, 1.5, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.4, 2.9, 1.4, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.9, 3.1, 1.5, 0.1], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.4, 3.7, 1.5, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.8, 3.4, 1.6, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.8, 3.0, 1.4, 0.1], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.3, 3.0, 1.1, 0.1], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.8, 4.0, 1.2, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.7, 4.4, 1.5, 0.4], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.4, 3.9, 1.3, 0.4], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.1, 3.5, 1.4, 0.3], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.7, 3.8, 1.7, 0.3], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.1, 3.8, 1.5, 0.3], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.4, 3.4, 1.7, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.1, 3.7, 1.5, 0.4], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.6, 3.6, 1.0, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.1, 3.3, 1.7, 0.5], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.8, 3.4, 1.9, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.0, 3.0, 1.6, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.0, 3.4, 1.6, 0.4], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.2, 3.5, 1.5, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.2, 3.4, 1.4, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.7, 3.2, 1.6, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.8, 3.1, 1.6, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.4, 3.4, 1.5, 0.4], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.2, 4.1, 1.5, 0.1], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.5, 4.2, 1.4, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.9, 3.1, 1.5, 0.1], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.0, 3.2, 1.2, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.5, 3.5, 1.3, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.9, 3.1, 1.5, 0.1], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.4, 3.0, 1.3, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.1, 3.4, 1.5, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.0, 3.5, 1.3, 0.3], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.5, 2.3, 1.3, 0.3], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.4, 3.2, 1.3, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.0, 3.5, 1.6, 0.6], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.1, 3.8, 1.9, 0.4], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.8, 3.0, 1.4, 0.3], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.1, 3.8, 1.6, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [4.6, 3.2, 1.4, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.3, 3.7, 1.5, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [5.0, 3.3, 1.4, 0.2], label = Label "Iris-setosa"}
        , DataEntry {dataPoint = V.fromList [7.0, 3.2, 4.7, 1.4], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.4, 3.2, 4.5, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.9, 3.1, 4.9, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.5, 2.3, 4.0, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.5, 2.8, 4.6, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.7, 2.8, 4.5, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.3, 3.3, 4.7, 1.6], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [4.9, 2.4, 3.3, 1.0], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.6, 2.9, 4.6, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.2, 2.7, 3.9, 1.4], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.0, 2.0, 3.5, 1.0], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.9, 3.0, 4.2, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.0, 2.2, 4.0, 1.0], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.1, 2.9, 4.7, 1.4], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.6, 2.9, 3.6, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.7, 3.1, 4.4, 1.4], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.6, 3.0, 4.5, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.8, 2.7, 4.1, 1.0], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.2, 2.2, 4.5, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.6, 2.5, 3.9, 1.1], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.9, 3.2, 4.8, 1.8], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.1, 2.8, 4.0, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.3, 2.5, 4.9, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.1, 2.8, 4.7, 1.2], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.4, 2.9, 4.3, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.6, 3.0, 4.4, 1.4], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.8, 2.8, 4.8, 1.4], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.7, 3.0, 5.0, 1.7], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.0, 2.9, 4.5, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.7, 2.6, 3.5, 1.0], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.5, 2.4, 3.8, 1.1], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.5, 2.4, 3.7, 1.0], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.8, 2.7, 3.9, 1.2], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.0, 2.7, 5.1, 1.6], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.4, 3.0, 4.5, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.0, 3.4, 4.5, 1.6], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.7, 3.1, 4.7, 1.5], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.3, 2.3, 4.4, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.6, 3.0, 4.1, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.5, 2.5, 4.0, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.5, 2.6, 4.4, 1.2], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.1, 3.0, 4.6, 1.4], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.8, 2.6, 4.0, 1.2], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.0, 2.3, 3.3, 1.0], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.6, 2.7, 4.2, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.7, 3.0, 4.2, 1.2], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.7, 2.9, 4.2, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.2, 2.9, 4.3, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.1, 2.5, 3.0, 1.1], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [5.7, 2.8, 4.1, 1.3], label = Label "Iris-versicolor"}
        , DataEntry {dataPoint = V.fromList [6.3, 3.3, 6.0, 2.5], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [5.8, 2.7, 5.1, 1.9], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.1, 3.0, 5.9, 2.1], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.3, 2.9, 5.6, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.5, 3.0, 5.8, 2.2], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.6, 3.0, 6.6, 2.1], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [4.9, 2.5, 4.5, 1.7], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.3, 2.9, 6.3, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.7, 2.5, 5.8, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.2, 3.6, 6.1, 2.5], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.5, 3.2, 5.1, 2.0], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.4, 2.7, 5.3, 1.9], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.8, 3.0, 5.5, 2.1], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [5.7, 2.5, 5.0, 2.0], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [5.8, 2.8, 5.1, 2.4], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.4, 3.2, 5.3, 2.3], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.5, 3.0, 5.5, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.7, 3.8, 6.7, 2.2], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.7, 2.6, 6.9, 2.3], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.0, 2.2, 5.0, 1.5], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.9, 3.2, 5.7, 2.3], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [5.6, 2.8, 4.9, 2.0], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.7, 2.8, 6.7, 2.0], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.3, 2.7, 4.9, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.7, 3.3, 5.7, 2.1], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.2, 3.2, 6.0, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.2, 2.8, 4.8, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.1, 3.0, 4.9, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.4, 2.8, 5.6, 2.1], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.2, 3.0, 5.8, 1.6], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.4, 2.8, 6.1, 1.9], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.9, 3.8, 6.4, 2.0], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.4, 2.8, 5.6, 2.2], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.3, 2.8, 5.1, 1.5], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.1, 2.6, 5.6, 1.4], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [7.7, 3.0, 6.1, 2.3], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.3, 3.4, 5.6, 2.4], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.4, 3.1, 5.5, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.0, 3.0, 4.8, 1.8], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.9, 3.1, 5.4, 2.1], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.7, 3.1, 5.6, 2.4], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.9, 3.1, 5.1, 2.3], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [5.8, 2.7, 5.1, 1.9], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.8, 3.2, 5.9, 2.3], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.7, 3.3, 5.7, 2.5], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.7, 3.0, 5.2, 2.3], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.3, 2.5, 5.0, 1.9], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.5, 3.0, 5.2, 2.0], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [6.2, 3.4, 5.4, 2.3], label = Label "Iris-virginica"}
        , DataEntry {dataPoint = V.fromList [5.9, 3.0, 5.1, 1.8], label = Label "Iris-virginica"}
        ]
    }
