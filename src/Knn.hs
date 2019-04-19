module Knn where

import qualified Data.KdTree.Static as KD
import qualified Data.Vector        as V

import           Data.List
import           Data.Ord

import           DataSet

type Tree = KD.KdTree Double DataEntry

dataEntryAsList :: DataEntry -> [Double]
dataEntryAsList = V.toList . dataPoint

buildTree :: (TrainingsData t) => t -> Tree
buildTree t = KD.build dataEntryAsList (getData t)

kNearestNeighbours :: Tree -> Int -> DataEntry -> [DataEntry]
kNearestNeighbours = KD.kNearest

classify :: Tree -> Int -> DataEntry -> Label
classify t k d = label . head $ max
  where
    max = maximumBy (comparing length) grped
    grped = groupBy (\d1 d2 -> label d1 == label d2) nearest
    nearest = kNearestNeighbours t k d
