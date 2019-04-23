{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module DataSet where

import qualified Data.ByteString.Lazy  as BL
import qualified Data.Csv              as CSV
import qualified Data.List.Split       as LS
import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified System.Random.Shuffle as SH

import qualified Control.Monad.Random  as RND

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
