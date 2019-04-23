{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import DataSet
import Knn

import Control.DeepSeq (NFData, rnf)
import qualified Data.Vector as V

--irisData = buildTree
--
main :: IO ()
main =
  defaultMain
    [ bgroup
        "single classify"
        [ bench "iris k 1" $ nf (classify (buildTree irisData) 1) (DataEntry {dataPoint = V.fromList [5.1, 3.5, 1.4, 0.2], label = Label "Iris-setosa"})
        , bench "iris k 5" $ nf (classify (buildTree irisData) 5) (DataEntry {dataPoint = V.fromList [5.1, 3.5, 1.4, 0.2], label = Label "Iris-setosa"})
        , bench "iris k 10" $ nf (classify (buildTree irisData) 10) (DataEntry {dataPoint = V.fromList [5.1, 3.5, 1.4, 0.2], label = Label "Iris-setosa"})
        , bench "redwine k 1" $ nf (classify (buildTree wineData) 1) (DataEntry { dataPoint = V.fromList [8.2, 0.5, 0.35, 2.9, 7.7e-2, 21.0, 127.0, 0.9976, 3.23, 0.62, 9.4], label = Label "5"})
        , bench "redwine k 5" $ nf (classify (buildTree wineData) 5) (DataEntry { dataPoint = V.fromList [8.2, 0.5, 0.35, 2.9, 7.7e-2, 21.0, 127.0, 0.9976, 3.23, 0.62, 9.4], label = Label "5"})
        , bench "redwine k 10" $ nf (classify (buildTree wineData) 10) (DataEntry { dataPoint = V.fromList [8.2, 0.5, 0.35, 2.9, 7.7e-2, 21.0, 127.0, 0.9976, 3.23, 0.62, 9.4], label = Label "5"})
        ]
    , bgroup
        "batch classify"
        [ bench "redwine 1500 k5" $ nf (fmap (classify (buildTree wineData) 5)) (take 1500 wineDataLines)
        , bench "redwine 15000 k5" $ nf (fmap (classify (buildTree wineData) 5)) (take 15000 wineDataLines)
        , bench "redwine 150000 k5" $ nf (fmap (classify (buildTree wineData) 5)) (take 150000 wineDataLines)
        , bench "iris 1500 k5" $ nf (fmap (classify (buildTree irisData) 5)) (take 1500 irisDataLines)
        , bench "iris 15000 k5" $ nf (fmap (classify (buildTree irisData) 5)) (take 15000 irisDataLines)
        , bench "iris 150000 k5" $ nf (fmap (classify (buildTree irisData) 5)) (take 150000 irisDataLines)
        ]
    ]

instance NFData Label where
  rnf x = seq x ()

irisDataLines = cycle . data_lines $ irisData

wineDataLines = cycle . data_lines $ wineData

irisData =
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

wineData =
  DataSet
    { header =
        Header
          [ "fixed acidity"
          , "volatile acidity"
          , "citric acid"
          , "residual sugar"
          , "chlorides"
          , "free sulfur dioxide"
          , "total sulfur dioxide"
          , "density"
          , "pH"
          , "sulphates"
          , "alcohol"
          , "quality"
          ]
    , data_lines =
        [ DataEntry
            { dataPoint = V.fromList [7.4, 0.7, 0.0, 1.9, 7.6e-2, 11.0, 34.0, 0.9978, 3.51, 0.56, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.88, 0.0, 2.6, 9.8e-2, 25.0, 67.0, 0.9968, 3.2, 0.68, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.76, 4.0e-2, 2.3, 9.2e-2, 15.0, 54.0, 0.997, 3.26, 0.65, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.2, 0.28, 0.56, 1.9, 7.5e-2, 17.0, 60.0, 0.998, 3.16, 0.58, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.7, 0.0, 1.9, 7.6e-2, 11.0, 34.0, 0.9978, 3.51, 0.56, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.66, 0.0, 1.8, 7.5e-2, 13.0, 40.0, 0.9978, 3.51, 0.56, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.6, 6.0e-2, 1.6, 6.9e-2, 15.0, 59.0, 0.9964, 3.3, 0.46, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.65, 0.0, 1.2, 6.5e-2, 15.0, 21.0, 0.9946, 3.39, 0.47, 10.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.58, 2.0e-2, 2.0, 7.3e-2, 9.0, 18.0, 0.9968, 3.36, 0.57, 9.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.5, 0.36, 6.1, 7.1e-2, 17.0, 102.0, 0.9978, 3.35, 0.8, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.58, 8.0e-2, 1.8, 9.7e-2, 15.0, 65.0, 0.9959, 3.28, 0.54, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.5, 0.36, 6.1, 7.1e-2, 17.0, 102.0, 0.9978, 3.35, 0.8, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.615, 0.0, 1.6, 8.9e-2, 16.0, 59.0, 0.9943, 3.58, 0.52, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.61, 0.29, 1.6, 0.114, 9.0, 29.0, 0.9974, 3.26, 1.56, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.62, 0.18, 3.8, 0.176, 52.0, 145.0, 0.9986, 3.16, 0.88, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.62, 0.19, 3.9, 0.17, 51.0, 148.0, 0.9986, 3.17, 0.93, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.28, 0.56, 1.8, 9.2e-2, 35.0, 103.0, 0.9969, 3.3, 0.75, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.56, 0.28, 1.7, 0.368, 16.0, 56.0, 0.9968, 3.11, 1.28, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.59, 8.0e-2, 4.4, 8.6e-2, 6.0, 29.0, 0.9974, 3.38, 0.5, 9.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.32, 0.51, 1.8, 0.341, 17.0, 56.0, 0.9969, 3.04, 1.08, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.22, 0.48, 1.8, 7.7e-2, 29.0, 60.0, 0.9968, 3.39, 0.53, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.39, 0.31, 2.3, 8.2e-2, 23.0, 71.0, 0.9982, 3.52, 0.65, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.43, 0.21, 1.6, 0.106, 10.0, 37.0, 0.9966, 3.17, 0.91, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.49, 0.11, 2.3, 8.4e-2, 9.0, 67.0, 0.9968, 3.17, 0.53, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.4, 0.14, 2.4, 8.5e-2, 21.0, 40.0, 0.9968, 3.43, 0.63, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.39, 0.16, 1.4, 8.0e-2, 11.0, 23.0, 0.9955, 3.34, 0.56, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.41, 0.24, 1.8, 8.0e-2, 4.0, 11.0, 0.9962, 3.28, 0.59, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.43, 0.21, 1.6, 0.106, 10.0, 37.0, 0.9966, 3.17, 0.91, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.71, 0.0, 1.9, 8.0e-2, 14.0, 35.0, 0.9972, 3.47, 0.55, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.645, 0.0, 2.0, 8.2e-2, 8.0, 16.0, 0.9964, 3.38, 0.59, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.675, 7.0e-2, 2.4, 8.9e-2, 17.0, 82.0, 0.9958, 3.35, 0.54, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.685, 0.0, 2.5, 0.105, 22.0, 37.0, 0.9966, 3.46, 0.57, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.655, 0.12, 2.3, 8.3e-2, 15.0, 113.0, 0.9966, 3.17, 0.66, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.605, 0.12, 10.7, 7.3e-2, 40.0, 83.0, 0.9993, 3.45, 0.52, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.2, 0.32, 0.25, 1.8, 0.103, 13.0, 50.0, 0.9957, 3.38, 0.55, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.645, 0.0, 5.5, 8.6e-2, 5.0, 18.0, 0.9986, 3.4, 0.55, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.6, 0.14, 2.4, 8.6e-2, 3.0, 15.0, 0.9975, 3.42, 0.6, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.38, 0.28, 2.1, 6.6e-2, 13.0, 30.0, 0.9968, 3.23, 0.73, 9.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [5.7, 1.13, 9.0e-2, 1.5, 0.172, 7.0, 19.0, 0.994, 3.5, 0.48, 9.8]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.45, 0.36, 5.9, 7.4e-2, 12.0, 87.0, 0.9978, 3.33, 0.83, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.45, 0.36, 5.9, 7.4e-2, 12.0, 87.0, 0.9978, 3.33, 0.83, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.61, 0.3, 2.8, 8.8e-2, 17.0, 46.0, 0.9976, 3.26, 0.51, 9.3]
            , label = Label "4"
            }
        , DataEntry
            {dataPoint = V.fromList [7.5, 0.49, 0.2, 2.6, 0.332, 8.0, 14.0, 0.9968, 3.21, 0.9, 10.5], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.66, 0.22, 2.2, 6.9e-2, 9.0, 23.0, 0.9968, 3.3, 1.2, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.67, 2.0e-2, 1.8, 5.0e-2, 5.0, 11.0, 0.9962, 3.48, 0.52, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [4.6, 0.52, 0.15, 2.1, 5.4e-2, 8.0, 65.0, 0.9934, 3.9, 0.56, 13.1]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.935, 0.43, 2.2, 0.114, 22.0, 114.0, 0.997, 3.25, 0.73, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.29, 0.52, 1.6, 0.113, 12.0, 37.0, 0.9969, 3.25, 0.58, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.4, 0.23, 1.6, 6.6e-2, 5.0, 12.0, 0.9958, 3.34, 0.56, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.31, 0.37, 1.4, 7.4e-2, 12.0, 96.0, 0.9954, 3.32, 0.58, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.66, 0.26, 1.7, 7.4e-2, 4.0, 23.0, 0.9971, 3.15, 0.74, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.52, 4.0e-2, 2.2, 6.9e-2, 8.0, 15.0, 0.9956, 3.4, 0.63, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.5, 4.0e-2, 2.1, 6.8e-2, 6.0, 14.0, 0.9955, 3.39, 0.64, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.38, 0.36, 3.0, 8.1e-2, 30.0, 119.0, 0.997, 3.2, 0.56, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.51, 0.15, 2.8, 0.11, 33.0, 73.0, 0.9955, 3.17, 0.63, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.62, 4.0e-2, 3.8, 8.4e-2, 25.0, 45.0, 0.9978, 3.34, 0.53, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.42, 0.57, 3.4, 7.0e-2, 4.0, 10.0, 0.9971, 3.04, 0.63, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.63, 0.12, 5.1, 0.111, 50.0, 110.0, 0.9983, 3.26, 0.77, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.59, 0.18, 2.3, 7.6e-2, 17.0, 54.0, 0.9975, 3.43, 0.59, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.39, 0.31, 2.4, 7.4e-2, 9.0, 46.0, 0.9962, 3.41, 0.54, 9.4]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [8.8, 0.4, 0.4, 2.2, 7.9e-2, 19.0, 52.0, 0.998, 3.44, 0.64, 9.2], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.69, 0.49, 1.8, 0.115, 20.0, 112.0, 0.9968, 3.21, 0.71, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.52, 0.16, 1.9, 8.5e-2, 12.0, 35.0, 0.9968, 3.38, 0.62, 9.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.735, 5.0e-2, 2.0, 8.1e-2, 13.0, 54.0, 0.9966, 3.39, 0.57, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.725, 5.0e-2, 4.65, 8.6e-2, 4.0, 11.0, 0.9962, 3.41, 0.39, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.725, 5.0e-2, 4.65, 8.6e-2, 4.0, 11.0, 0.9962, 3.41, 0.39, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.52, 0.11, 1.5, 7.9e-2, 11.0, 39.0, 0.9968, 3.42, 0.58, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.705, 7.0e-2, 1.6, 7.6e-2, 6.0, 15.0, 0.9962, 3.44, 0.58, 10.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.32, 0.57, 2.0, 7.4e-2, 27.0, 65.0, 0.9969, 3.28, 0.79, 10.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.705, 5.0e-2, 1.9, 7.4e-2, 8.0, 19.0, 0.9962, 3.34, 0.95, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.63, 8.0e-2, 1.9, 7.6e-2, 15.0, 27.0, 0.9967, 3.32, 0.54, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.67, 0.23, 2.1, 8.8e-2, 17.0, 96.0, 0.9962, 3.32, 0.48, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.69, 0.22, 1.9, 8.4e-2, 18.0, 94.0, 0.9961, 3.31, 0.48, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.675, 0.26, 2.1, 8.4e-2, 11.0, 43.0, 0.9976, 3.31, 0.53, 9.2]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.32, 0.54, 2.5, 9.4e-2, 28.0, 83.0, 0.9984, 3.28, 0.82, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.41, 0.64, 2.2, 9.3e-2, 9.0, 42.0, 0.9986, 3.54, 0.66, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.41, 0.64, 2.2, 9.3e-2, 9.0, 42.0, 0.9986, 3.54, 0.66, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.785, 0.0, 2.4, 0.104, 14.0, 30.0, 0.9966, 3.52, 0.55, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.75, 0.12, 2.0, 8.6e-2, 12.0, 80.0, 0.9958, 3.38, 0.52, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.625, 0.2, 1.5, 8.0e-2, 27.0, 119.0, 0.9972, 3.16, 1.12, 9.1]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.45, 0.2, 1.6, 6.9e-2, 3.0, 15.0, 0.9958, 3.41, 0.56, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.43, 0.7, 1.9, 0.464, 22.0, 67.0, 0.9974, 3.13, 1.28, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.5, 0.47, 2.0, 8.6e-2, 21.0, 73.0, 0.997, 3.36, 0.57, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.67, 0.26, 1.8, 0.401, 16.0, 51.0, 0.9969, 3.16, 1.14, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.3, 0.48, 1.8, 6.9e-2, 18.0, 61.0, 0.9959, 3.44, 0.78, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.55, 0.15, 2.2, 7.6e-2, 19.0, 40.0, 0.9961, 3.41, 0.59, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.49, 0.28, 1.9, 0.11, 20.0, 136.0, 0.9972, 2.93, 1.95, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.49, 0.26, 1.9, 6.2e-2, 9.0, 31.0, 0.9966, 3.39, 0.64, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.39, 0.44, 2.1, 0.107, 34.0, 125.0, 0.9978, 3.14, 1.22, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.62, 8.0e-2, 1.8, 7.6e-2, 8.0, 24.0, 0.9978, 3.48, 0.53, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.52, 0.26, 1.9, 7.9e-2, 42.0, 140.0, 0.9964, 3.23, 0.54, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.49, 0.28, 1.9, 0.11, 20.0, 136.0, 0.9972, 2.93, 1.95, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.49, 0.29, 2.0, 0.11, 19.0, 133.0, 0.9972, 2.93, 1.98, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.49, 0.26, 1.9, 6.2e-2, 9.0, 31.0, 0.9966, 3.39, 0.64, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.0, 1.02, 4.0e-2, 1.4, 4.5e-2, 41.0, 85.0, 0.9938, 3.75, 0.48, 10.5]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [4.7, 0.6, 0.17, 2.3, 5.8e-2, 17.0, 106.0, 0.9932, 3.85, 0.6, 12.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.775, 0.0, 3.0, 0.102, 8.0, 23.0, 0.9965, 3.45, 0.56, 10.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.5, 0.25, 2.0, 7.0e-2, 3.0, 22.0, 0.9963, 3.25, 0.63, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.9, 6.0e-2, 2.5, 7.9e-2, 5.0, 10.0, 0.9967, 3.39, 0.56, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.545, 0.18, 1.9, 8.0e-2, 13.0, 35.0, 0.9972, 3.3, 0.59, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.61, 0.3, 2.1, 8.4e-2, 11.0, 50.0, 0.9972, 3.4, 0.61, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.5, 0.3, 1.9, 7.5e-2, 8.0, 22.0, 0.9959, 3.31, 0.56, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.545, 0.18, 1.9, 8.0e-2, 13.0, 35.0, 0.9972, 3.3, 0.59, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.575, 0.22, 2.1, 7.7e-2, 12.0, 65.0, 0.9967, 3.29, 0.51, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.49, 0.24, 2.2, 7.0e-2, 5.0, 36.0, 0.996, 3.33, 0.48, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.575, 0.22, 2.1, 7.7e-2, 12.0, 65.0, 0.9967, 3.29, 0.51, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.41, 0.68, 1.7, 0.467, 18.0, 69.0, 0.9973, 3.08, 1.31, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.63, 0.31, 1.7, 8.8e-2, 15.0, 64.0, 0.9969, 3.46, 0.79, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.33, 0.53, 2.5, 9.1e-2, 18.0, 80.0, 0.9976, 3.37, 0.8, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.785, 0.52, 2.0, 0.122, 37.0, 153.0, 0.9969, 3.21, 0.69, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.56, 0.19, 1.8, 0.104, 12.0, 47.0, 0.9964, 3.19, 0.93, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.62, 9.0e-2, 2.2, 8.4e-2, 11.0, 108.0, 0.9964, 3.15, 0.66, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.6, 0.1, 2.2, 8.5e-2, 14.0, 111.0, 0.9964, 3.15, 0.66, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.31, 0.44, 2.3, 8.0e-2, 22.0, 46.0, 0.9988, 3.32, 0.67, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.56, 0.19, 1.8, 0.104, 12.0, 47.0, 0.9964, 3.19, 0.93, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.4, 0.31, 2.2, 9.0e-2, 13.0, 62.0, 0.9966, 3.07, 0.63, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.54, 0.28, 1.9, 7.7e-2, 11.0, 40.0, 0.9978, 3.39, 0.61, 10.0]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [7.8, 0.56, 0.12, 2.0, 8.2e-2, 7.0, 28.0, 0.997, 3.37, 0.5, 9.4], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.55, 4.0e-2, 2.2, 0.119, 14.0, 56.0, 0.9962, 3.21, 0.6, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.69, 8.0e-2, 1.8, 9.7e-2, 22.0, 89.0, 0.9959, 3.34, 0.54, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 1.07, 9.0e-2, 1.7, 0.178, 10.0, 89.0, 0.9962, 3.3, 0.57, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.55, 4.0e-2, 2.2, 0.119, 14.0, 56.0, 0.9962, 3.21, 0.6, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.695, 0.0, 2.5, 7.5e-2, 3.0, 13.0, 0.998, 3.49, 0.52, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.71, 0.0, 2.6, 8.0e-2, 11.0, 34.0, 0.9976, 3.44, 0.53, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.5, 0.17, 1.6, 8.2e-2, 21.0, 102.0, 0.996, 3.39, 0.48, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.62, 4.0e-2, 1.9, 0.146, 27.0, 90.0, 0.9984, 3.16, 0.7, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 1.33, 0.0, 1.7, 8.1e-2, 3.0, 12.0, 0.9964, 3.53, 0.49, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 1.33, 0.0, 1.8, 8.2e-2, 3.0, 12.0, 0.9964, 3.54, 0.48, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.59, 0.16, 1.8, 6.5e-2, 3.0, 16.0, 0.9962, 3.42, 0.92, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.38, 0.15, 1.8, 7.2e-2, 6.0, 19.0, 0.9955, 3.42, 0.57, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.745, 0.56, 2.0, 0.118, 30.0, 134.0, 0.9968, 3.24, 0.66, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.5, 9.0e-2, 2.3, 4.9e-2, 17.0, 99.0, 0.9937, 3.63, 0.63, 13.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.5, 9.0e-2, 2.3, 4.9e-2, 17.0, 99.0, 0.9937, 3.63, 0.63, 13.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.5, 1.0e-2, 1.5, 6.0e-2, 17.0, 26.0, 0.9952, 3.4, 0.58, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 1.04, 5.0e-2, 2.2, 8.4e-2, 13.0, 29.0, 0.9959, 3.22, 0.55, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.745, 0.11, 1.9, 9.0e-2, 16.0, 63.0, 0.9965, 3.19, 0.82, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.715, 0.15, 1.8, 8.9e-2, 10.0, 52.0, 0.9968, 3.23, 0.77, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.415, 0.36, 2.0, 8.1e-2, 13.0, 45.0, 0.9972, 3.48, 0.64, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.56, 0.19, 2.1, 8.1e-2, 15.0, 105.0, 0.9962, 3.33, 0.54, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.56, 0.19, 2.0, 8.1e-2, 17.0, 108.0, 0.9962, 3.32, 0.54, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.745, 0.11, 1.9, 9.0e-2, 16.0, 63.0, 0.9965, 3.19, 0.82, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.715, 0.15, 1.8, 8.9e-2, 10.0, 52.0, 0.9968, 3.23, 0.77, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.2, 0.34, 0.0, 1.8, 5.0e-2, 27.0, 63.0, 0.9916, 3.68, 0.79, 14.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.39, 8.0e-2, 1.7, 6.6e-2, 3.0, 20.0, 0.9954, 3.34, 0.58, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.2, 0.34, 0.0, 1.8, 5.0e-2, 27.0, 63.0, 0.9916, 3.68, 0.79, 14.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.67, 0.55, 1.8, 0.117, 32.0, 141.0, 0.9968, 3.17, 0.62, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.8, 0.68, 2.0e-2, 1.8, 8.7e-2, 21.0, 94.0, 0.9944, 3.54, 0.52, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.49, 0.26, 1.6, 0.236, 10.0, 88.0, 0.9968, 3.11, 0.8, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.49, 0.1, 2.3, 7.4e-2, 12.0, 30.0, 0.9959, 3.42, 0.58, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.4, 0.44, 2.8, 8.9e-2, 11.0, 43.0, 0.9975, 3.53, 0.61, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.33, 0.47, 2.1, 7.7e-2, 5.0, 11.0, 0.9958, 3.33, 0.53, 10.3]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [9.2, 0.52, 1.0, 3.4, 0.61, 32.0, 69.0, 0.9996, 2.74, 2.0, 9.4], label = Label "4"}
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.6, 3.0e-2, 1.8, 9.5e-2, 25.0, 99.0, 0.995, 3.35, 0.54, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.6, 3.0e-2, 1.8, 9.5e-2, 25.0, 99.0, 0.995, 3.35, 0.54, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.43, 0.42, 5.5, 7.0e-2, 29.0, 129.0, 0.9973, 3.42, 0.72, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.43, 0.42, 5.5, 7.1e-2, 28.0, 128.0, 0.9973, 3.42, 0.71, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.43, 0.42, 5.5, 7.0e-2, 29.0, 129.0, 0.9973, 3.42, 0.72, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.43, 0.42, 5.5, 7.1e-2, 28.0, 128.0, 0.9973, 3.42, 0.71, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.68, 0.0, 2.2, 7.3e-2, 12.0, 22.0, 0.9969, 3.48, 0.5, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.6, 0.18, 1.9, 7.9e-2, 18.0, 86.0, 0.9968, 3.59, 0.57, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.95, 3.0e-2, 2.0, 9.0e-2, 7.0, 20.0, 0.9959, 3.2, 0.56, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.68, 2.0e-2, 1.3, 7.2e-2, 9.0, 20.0, 0.9965, 3.17, 1.08, 9.2]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.53, 4.0e-2, 1.7, 7.6e-2, 17.0, 31.0, 0.9964, 3.33, 0.56, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.6, 0.26, 7.3, 7.0e-2, 36.0, 121.0, 0.9982, 3.37, 0.49, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.59, 0.26, 7.2, 7.0e-2, 35.0, 121.0, 0.9981, 3.37, 0.49, 9.4]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.8, 0.63, 0.48, 1.7, 0.1, 14.0, 96.0, 0.9961, 3.19, 0.62, 9.5], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.64, 0.1, 2.1, 8.5e-2, 18.0, 101.0, 0.9956, 3.34, 0.52, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.55, 3.0e-2, 1.6, 7.2e-2, 17.0, 42.0, 0.9956, 3.37, 0.48, 9.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.63, 7.0e-2, 2.1, 8.9e-2, 11.0, 44.0, 0.9953, 3.47, 0.55, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.705, 0.24, 1.8, 0.36, 15.0, 63.0, 0.9964, 3.0, 1.59, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.885, 3.0e-2, 1.8, 5.8e-2, 4.0, 8.0, 0.9972, 3.36, 0.33, 9.1]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.42, 0.17, 2.0, 7.3e-2, 6.0, 18.0, 0.9972, 3.29, 0.61, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.42, 0.17, 2.0, 7.3e-2, 6.0, 18.0, 0.9972, 3.29, 0.61, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.62, 5.0e-2, 1.9, 6.8e-2, 24.0, 42.0, 0.9961, 3.42, 0.57, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.38, 0.21, 2.0, 8.0e-2, 7.0, 35.0, 0.9961, 3.33, 0.47, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.5, 4.0e-2, 1.5, 8.5e-2, 19.0, 49.0, 0.9958, 3.35, 0.78, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.38, 0.21, 2.0, 8.0e-2, 7.0, 35.0, 0.9961, 3.33, 0.47, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.52, 0.42, 2.3, 8.7e-2, 8.0, 38.0, 0.9972, 3.58, 0.61, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.805, 0.0, 2.5, 6.8e-2, 7.0, 20.0, 0.9969, 3.48, 0.56, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.61, 0.14, 2.4, 6.7e-2, 10.0, 42.0, 0.9969, 3.19, 0.59, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.61, 0.14, 2.4, 6.7e-2, 10.0, 42.0, 0.9969, 3.19, 0.59, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.61, 0.49, 2.0, 0.27, 23.0, 110.0, 0.9972, 3.12, 1.02, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.73, 2.0e-2, 2.5, 7.6e-2, 16.0, 42.0, 0.9972, 3.44, 0.52, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.61, 0.2, 1.8, 7.7e-2, 11.0, 65.0, 0.9971, 3.54, 0.58, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.62, 0.21, 1.9, 7.9e-2, 8.0, 62.0, 0.997, 3.52, 0.58, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.31, 0.57, 2.0, 0.111, 26.0, 85.0, 0.9971, 3.26, 0.53, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.39, 0.48, 2.0, 8.2e-2, 14.0, 67.0, 0.9972, 3.34, 0.55, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.705, 0.1, 2.6, 8.4e-2, 9.0, 26.0, 0.9976, 3.39, 0.49, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.5, 0.33, 2.0, 8.4e-2, 15.0, 143.0, 0.9968, 3.2, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.49, 0.32, 1.9, 8.2e-2, 17.0, 144.0, 0.9968, 3.2, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.5, 0.35, 2.9, 7.7e-2, 21.0, 127.0, 0.9976, 3.23, 0.62, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.37, 0.25, 1.9, 7.4e-2, 21.0, 49.0, 0.9974, 3.57, 0.62, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.63, 0.12, 3.8, 9.9e-2, 16.0, 126.0, 0.9969, 3.28, 0.61, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.55, 0.21, 2.2, 7.1e-2, 7.0, 28.0, 0.9964, 3.28, 0.55, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.55, 0.21, 2.2, 7.1e-2, 7.0, 28.0, 0.9964, 3.28, 0.55, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.59, 0.33, 2.0, 7.4e-2, 24.0, 120.0, 0.9968, 3.25, 0.54, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.58, 0.3, 2.4, 7.4e-2, 15.0, 55.0, 0.9968, 3.46, 0.59, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.3, 0.6, 2.0, 6.7e-2, 12.0, 27.0, 0.9981, 3.11, 0.97, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.4, 0.835, 8.0e-2, 1.2, 4.6e-2, 13.0, 93.0, 0.9924, 3.57, 0.85, 13.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 1.09, 6.0e-2, 2.1, 6.1e-2, 12.0, 31.0, 0.9948, 3.51, 0.43, 11.4]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.32, 0.47, 1.4, 5.6e-2, 9.0, 24.0, 0.99695, 3.22, 0.82, 10.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.37, 0.48, 2.1, 9.7e-2, 39.0, 145.0, 0.9975, 3.04, 1.03, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.5, 0.11, 1.5, 7.5e-2, 16.0, 49.0, 0.99545, 3.36, 0.79, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.42, 0.35, 1.6, 8.8e-2, 16.0, 39.0, 0.9961, 3.34, 0.55, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.43, 0.36, 1.6, 8.9e-2, 14.0, 37.0, 0.99615, 3.34, 0.56, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.8, 0.3, 0.74, 2.6, 9.5e-2, 9.0, 28.0, 0.9994, 3.2, 0.77, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [12.8, 0.3, 0.74, 2.6, 9.5e-2, 9.0, 28.0, 0.9994, 3.2, 0.77, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.57, 0.31, 1.8, 6.9e-2, 26.0, 120.0, 0.99625, 3.29, 0.53, 9.3]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.8, 0.44, 0.28, 2.7, 0.1, 18.0, 95.0, 0.9966, 3.22, 0.67, 9.4], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [11.0, 0.3, 0.58, 2.1, 5.4e-2, 7.0, 19.0, 0.998, 3.31, 0.88, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.53, 0.6, 2.0, 3.9e-2, 5.0, 19.0, 0.99585, 3.3, 0.86, 12.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.725, 0.24, 2.8, 8.3e-2, 10.0, 62.0, 0.99685, 3.35, 0.56, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.44, 0.64, 2.1, 5.9e-2, 5.0, 15.0, 0.998, 3.21, 0.67, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.57, 0.26, 2.2, 6.0e-2, 28.0, 65.0, 0.9959, 3.3, 0.43, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.735, 8.0e-2, 2.4, 9.2e-2, 10.0, 41.0, 0.9974, 3.24, 0.71, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.49, 0.49, 5.6, 6.0e-2, 26.0, 121.0, 0.9974, 3.34, 0.76, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.625, 0.16, 2.0, 0.101, 13.0, 49.0, 0.9962, 3.14, 0.57, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.725, 0.22, 2.2, 7.2e-2, 11.0, 41.0, 0.9967, 3.36, 0.55, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.49, 0.19, 1.9, 7.6e-2, 10.0, 44.0, 0.9957, 3.39, 0.54, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.53, 0.33, 2.4, 8.0e-2, 24.0, 144.0, 0.99655, 3.3, 0.6, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.34, 0.37, 2.0, 8.2e-2, 24.0, 58.0, 0.9964, 3.34, 0.59, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.53, 0.26, 2.0, 0.101, 16.0, 72.0, 0.9957, 3.15, 0.57, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.61, 4.0e-2, 1.5, 5.7e-2, 5.0, 10.0, 0.99525, 3.42, 0.6, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.645, 0.25, 2.0, 8.3e-2, 8.0, 28.0, 0.99815, 3.28, 0.6, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.635, 0.36, 2.0, 8.9e-2, 15.0, 55.0, 0.99745, 3.31, 0.57, 10.4]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.43, 0.25, 2.6, 7.3e-2, 29.0, 63.0, 0.99615, 3.37, 0.58, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.59, 0.5, 2.0, 0.337, 27.0, 81.0, 0.9964, 3.04, 1.61, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.82, 0.14, 2.6, 8.9e-2, 9.0, 23.0, 0.9984, 3.39, 0.63, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.43, 0.25, 2.6, 7.3e-2, 29.0, 63.0, 0.99615, 3.37, 0.58, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.52, 0.25, 2.6, 8.1e-2, 10.0, 37.0, 0.99685, 3.46, 0.5, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.2, 0.48, 4.0e-2, 1.6, 5.4e-2, 19.0, 106.0, 0.9927, 3.54, 0.62, 12.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.38, 6.0e-2, 1.8, 7.8e-2, 12.0, 49.0, 0.99625, 3.37, 0.52, 9.9]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [8.5, 0.37, 0.2, 2.8, 9.0e-2, 18.0, 58.0, 0.998, 3.34, 0.7, 9.6], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.52, 0.25, 2.6, 8.1e-2, 10.0, 37.0, 0.99685, 3.46, 0.5, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 1.0, 9.0e-2, 2.3, 6.5e-2, 7.0, 37.0, 0.99685, 3.32, 0.55, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.63, 0.0, 1.9, 9.7e-2, 14.0, 38.0, 0.99675, 3.37, 0.58, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.63, 0.0, 1.9, 9.7e-2, 14.0, 38.0, 0.99675, 3.37, 0.58, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.645, 0.0, 1.9, 9.7e-2, 15.0, 39.0, 0.99675, 3.37, 0.58, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.63, 0.0, 1.9, 9.7e-2, 14.0, 38.0, 0.99675, 3.37, 0.58, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 1.0, 9.0e-2, 2.3, 6.5e-2, 7.0, 37.0, 0.99685, 3.32, 0.55, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.635, 0.37, 1.7, 0.263, 5.0, 62.0, 0.9971, 3.0, 1.09, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.0, 0.38, 0.56, 2.1, 9.3e-2, 6.0, 24.0, 0.99925, 3.14, 0.71, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.58, 0.1, 1.8, 0.102, 28.0, 109.0, 0.99565, 3.08, 0.49, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [15.0, 0.21, 0.44, 2.2, 7.5e-2, 10.0, 24.0, 1.00005, 3.07, 0.84, 9.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [15.0, 0.21, 0.44, 2.2, 7.5e-2, 10.0, 24.0, 1.00005, 3.07, 0.84, 9.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.66, 0.0, 2.0, 8.4e-2, 6.0, 23.0, 0.9983, 3.61, 0.96, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.68, 7.0e-2, 1.9, 7.5e-2, 16.0, 51.0, 0.99685, 3.38, 0.52, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.6, 0.17, 2.3, 7.2e-2, 11.0, 73.0, 0.9963, 3.2, 0.45, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.53, 6.0e-2, 1.7, 7.4e-2, 9.0, 39.0, 0.99615, 3.35, 0.48, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.66, 0.0, 2.0, 8.4e-2, 6.0, 23.0, 0.9983, 3.61, 0.96, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.32, 0.44, 1.6, 6.3e-2, 16.0, 37.0, 0.9985, 3.22, 0.78, 10.0]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [7.1, 0.6, 0.0, 1.8, 7.4e-2, 16.0, 34.0, 0.9972, 3.47, 0.7, 9.9], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.35, 0.48, 3.1, 9.0e-2, 5.0, 21.0, 0.9986, 3.17, 0.53, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.775, 0.42, 1.9, 9.2e-2, 8.0, 86.0, 0.9959, 3.23, 0.59, 9.5]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.1, 0.6, 0.0, 1.8, 7.4e-2, 16.0, 34.0, 0.9972, 3.47, 0.7, 9.9], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.57, 0.23, 3.2, 7.3e-2, 17.0, 119.0, 0.99675, 3.26, 0.57, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.34, 0.37, 2.2, 7.5e-2, 5.0, 13.0, 0.998, 3.22, 0.62, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.695, 0.0, 2.1, 7.5e-2, 12.0, 56.0, 0.9968, 3.49, 0.67, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.41, 0.76, 1.8, 0.611, 8.0, 45.0, 0.9968, 3.06, 1.26, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.31, 0.47, 2.6, 8.5e-2, 14.0, 33.0, 0.99965, 3.36, 0.8, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.33, 0.23, 1.7, 7.7e-2, 18.0, 45.0, 0.99625, 3.29, 0.65, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.975, 4.0e-2, 2.0, 8.7e-2, 12.0, 67.0, 0.99565, 3.35, 0.6, 9.4]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.52, 3.0e-2, 1.7, 7.0e-2, 10.0, 35.0, 0.99575, 3.34, 0.57, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.37, 0.23, 1.8, 7.7e-2, 23.0, 49.0, 0.9963, 3.28, 0.67, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.5, 0.56, 0.49, 2.4, 6.4e-2, 5.0, 27.0, 0.9999, 3.08, 0.87, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.8, 0.26, 0.52, 1.8, 7.1e-2, 6.0, 10.0, 0.9968, 3.2, 0.72, 10.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.87, 0.0, 3.3, 9.6e-2, 26.0, 61.0, 1.00025, 3.6, 0.72, 9.8]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.35, 0.46, 3.6, 7.8e-2, 15.0, 37.0, 0.9973, 3.35, 0.86, 12.8]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.54, 4.0e-2, 3.0, 7.7e-2, 7.0, 27.0, 0.9987, 3.69, 0.91, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.18, 0.51, 4.0, 0.104, 4.0, 23.0, 0.9996, 3.28, 0.97, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.545, 6.0e-2, 4.0, 8.7e-2, 27.0, 61.0, 0.9965, 3.36, 0.67, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.18, 0.51, 4.0, 0.104, 4.0, 23.0, 0.9996, 3.28, 0.97, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.9, 0.37, 0.58, 4.0, 7.1e-2, 17.0, 65.0, 0.99935, 3.22, 0.78, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.715, 0.2, 2.4, 7.6e-2, 10.0, 38.0, 0.99735, 3.31, 0.64, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.65, 0.18, 7.0, 8.8e-2, 27.0, 94.0, 0.99915, 3.38, 0.77, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.545, 6.0e-2, 4.0, 8.7e-2, 27.0, 61.0, 0.9965, 3.36, 0.67, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.54, 4.0e-2, 3.0, 7.7e-2, 7.0, 27.0, 0.9987, 3.69, 0.91, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.18, 0.51, 4.0, 0.104, 4.0, 23.0, 0.9996, 3.28, 0.97, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.32, 0.45, 6.4, 7.3e-2, 5.0, 13.0, 0.9976, 3.23, 0.82, 12.6]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.4, 0.32, 5.6, 8.7e-2, 10.0, 47.0, 0.9991, 3.38, 0.77, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.4, 0.26, 0.44, 3.6, 7.1e-2, 6.0, 19.0, 0.9986, 3.12, 0.82, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.27, 0.68, 3.5, 0.358, 5.0, 10.0, 0.9972, 3.25, 1.08, 9.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.52, 0.12, 3.0, 6.7e-2, 12.0, 53.0, 0.9971, 3.36, 0.57, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.4, 0.32, 5.6, 8.7e-2, 10.0, 47.0, 0.9991, 3.38, 0.77, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.59, 7.0e-2, 3.4, 0.102, 32.0, 71.0, 1.00015, 3.31, 0.71, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.59, 7.0e-2, 3.4, 0.102, 32.0, 71.0, 1.00015, 3.31, 0.71, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.0, 0.45, 0.55, 2.0, 7.3e-2, 25.0, 49.0, 0.9997, 3.1, 0.76, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.4, 0.12, 3.0, 9.2e-2, 29.0, 53.0, 0.9967, 3.37, 0.7, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.52, 9.0e-2, 2.5, 9.1e-2, 20.0, 49.0, 0.9976, 3.34, 0.86, 10.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.42, 0.53, 3.3, 0.105, 33.0, 98.0, 1.001, 3.2, 0.95, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.52, 9.0e-2, 2.5, 9.1e-2, 20.0, 49.0, 0.9976, 3.34, 0.86, 10.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.0, 0.2, 0.48, 2.0, 0.343, 6.0, 18.0, 0.9979, 3.3, 0.71, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.55, 0.23, 2.7, 9.1e-2, 18.0, 48.0, 0.9994, 3.22, 0.64, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.36, 0.25, 2.4, 9.8e-2, 5.0, 16.0, 0.9964, 3.41, 0.6, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [13.3, 0.34, 0.52, 3.2, 9.4e-2, 17.0, 53.0, 1.0014, 3.05, 0.81, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.5, 0.46, 2.5, 7.3e-2, 5.0, 27.0, 1.0001, 3.05, 0.64, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.83, 0.37, 2.6, 8.6e-2, 26.0, 70.0, 0.9981, 3.16, 0.52, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.63, 6.0e-2, 2.0, 8.3e-2, 8.0, 29.0, 0.99855, 3.67, 0.73, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.65, 2.0e-2, 2.3, 9.4e-2, 5.0, 31.0, 0.9993, 3.67, 0.8, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.67, 6.0e-2, 2.1, 8.0e-2, 8.0, 33.0, 0.99845, 3.68, 0.71, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.53, 6.0e-2, 2.6, 8.6e-2, 20.0, 44.0, 0.9965, 3.38, 0.59, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.18, 0.48, 1.5, 6.8e-2, 7.0, 15.0, 0.9973, 3.22, 0.64, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.705, 0.12, 2.6, 9.2e-2, 12.0, 28.0, 0.9994, 3.51, 0.72, 10.0]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.4, 0.67, 0.12, 1.6, 0.186, 5.0, 21.0, 0.996, 3.39, 0.54, 9.5], label = Label "5"}
        , DataEntry
            {dataPoint = V.fromList [8.4, 0.65, 0.6, 2.1, 0.112, 12.0, 90.0, 0.9973, 3.2, 0.52, 9.2], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.53, 0.48, 2.5, 6.3e-2, 6.0, 25.0, 0.9998, 3.12, 0.59, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.62, 0.32, 2.2, 8.2e-2, 7.0, 54.0, 0.9966, 3.36, 0.52, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.41, 0.42, 2.4, 0.213, 6.0, 14.0, 0.9994, 3.19, 0.62, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.43, 0.44, 2.4, 0.214, 5.0, 12.0, 0.9994, 3.19, 0.63, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.29, 0.38, 1.7, 6.2e-2, 9.0, 30.0, 0.9968, 3.41, 0.53, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.53, 0.48, 2.5, 6.3e-2, 6.0, 25.0, 0.9998, 3.12, 0.59, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.53, 0.24, 2.0, 7.2e-2, 15.0, 105.0, 0.996, 3.27, 0.54, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.46, 0.31, 2.8, 9.3e-2, 19.0, 98.0, 0.99815, 3.32, 0.63, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.47, 0.3, 3.0, 7.6e-2, 30.0, 135.0, 0.9976, 3.3, 0.53, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.36, 0.29, 2.6, 8.7e-2, 26.0, 72.0, 0.99645, 3.39, 0.68, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.35, 0.29, 2.5, 9.6e-2, 20.0, 53.0, 0.9962, 3.42, 0.65, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.56, 0.23, 3.4, 0.102, 37.0, 92.0, 0.9996, 3.3, 0.65, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.77, 0.12, 2.9, 8.2e-2, 30.0, 74.0, 0.99865, 3.3, 0.64, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.66, 0.39, 3.2, 8.3e-2, 21.0, 59.0, 0.9989, 3.37, 0.71, 11.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.77, 0.12, 2.9, 8.2e-2, 30.0, 74.0, 0.99865, 3.3, 0.64, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.66, 0.39, 3.2, 8.3e-2, 21.0, 59.0, 0.9989, 3.37, 0.71, 11.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.61, 0.26, 3.4, 9.0e-2, 25.0, 87.0, 0.99975, 3.24, 0.62, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.62, 5.0e-2, 2.3, 7.9e-2, 6.0, 18.0, 0.99735, 3.29, 0.63, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.59, 0.42, 2.8, 9.0e-2, 35.0, 73.0, 0.999, 3.28, 0.7, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.49, 0.2, 11.0, 7.1e-2, 13.0, 50.0, 1.0015, 3.16, 0.69, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.49, 0.2, 11.0, 7.1e-2, 13.0, 50.0, 1.0015, 3.16, 0.69, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.53, 0.66, 3.65, 0.121, 6.0, 14.0, 0.9978, 3.05, 0.74, 11.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.44, 0.5, 4.5, 0.107, 5.0, 13.0, 0.998, 3.28, 0.83, 11.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [13.4, 0.27, 0.62, 2.6, 8.2e-2, 6.0, 21.0, 1.0002, 3.16, 0.67, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.46, 0.39, 2.0, 6.1e-2, 7.0, 15.0, 0.9981, 3.18, 0.62, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.36, 0.64, 2.9, 0.122, 10.0, 41.0, 0.998, 3.23, 0.66, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.36, 0.64, 2.9, 0.122, 10.0, 41.0, 0.998, 3.23, 0.66, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.58, 0.28, 3.2, 6.6e-2, 21.0, 114.0, 0.9973, 3.22, 0.54, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.56, 8.0e-2, 2.1, 0.105, 16.0, 44.0, 0.9958, 3.13, 0.52, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.65, 1.0e-2, 2.5, 7.8e-2, 17.0, 38.0, 0.9963, 3.34, 0.74, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.695, 0.53, 3.4, 0.128, 7.0, 21.0, 0.9992, 3.17, 0.84, 12.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.43, 0.45, 1.9, 5.2e-2, 6.0, 16.0, 0.9948, 3.35, 0.7, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.43, 0.32, 2.8, 8.0e-2, 29.0, 58.0, 0.9974, 3.31, 0.64, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.4, 0.49, 0.58, 3.0, 0.103, 28.0, 99.0, 1.0008, 3.16, 1.0, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.5, 0.28, 0.54, 2.3, 8.2e-2, 12.0, 29.0, 0.9997, 3.11, 1.36, 9.8]
            , label = Label "7"
            }
        , DataEntry
            {dataPoint = V.fromList [12.2, 0.34, 0.5, 2.4, 6.6e-2, 10.0, 21.0, 1.0, 3.12, 1.18, 9.2], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.42, 0.48, 2.7, 6.5e-2, 5.0, 18.0, 0.9972, 3.21, 0.87, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.9, 0.39, 0.47, 1.8, 0.118, 6.0, 14.0, 0.9982, 3.3, 0.75, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.9, 0.39, 0.47, 1.8, 0.118, 6.0, 14.0, 0.9982, 3.3, 0.75, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.57, 0.5, 2.6, 8.2e-2, 6.0, 32.0, 1.0006, 3.12, 0.78, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.685, 0.0, 1.9, 6.7e-2, 40.0, 63.0, 0.9979, 3.6, 0.81, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.815, 2.0e-2, 2.7, 7.2e-2, 17.0, 34.0, 0.9955, 3.58, 0.89, 12.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [13.8, 0.49, 0.67, 3.0, 9.3e-2, 6.0, 15.0, 0.9986, 3.02, 0.93, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.56, 0.31, 2.8, 8.9e-2, 15.0, 46.0, 0.9979, 3.11, 0.92, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.785, 0.0, 2.6, 9.3e-2, 11.0, 28.0, 0.9994, 3.36, 0.86, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.67, 0.22, 2.7, 0.107, 17.0, 34.0, 1.0004, 3.28, 0.98, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.795, 0.0, 2.6, 9.6e-2, 11.0, 26.0, 0.9994, 3.35, 0.83, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.665, 0.0, 2.4, 9.0e-2, 8.0, 19.0, 0.9974, 3.27, 0.73, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [13.5, 0.53, 0.79, 4.8, 0.12, 23.0, 77.0, 1.0018, 3.18, 0.77, 13.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.21, 0.4, 1.4, 6.6e-2, 40.5, 165.0, 0.9912, 3.25, 0.59, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.75, 1.0e-2, 2.4, 7.8e-2, 17.0, 32.0, 0.9955, 3.55, 0.61, 12.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.41, 0.52, 3.0, 8.0e-2, 29.0, 55.0, 1.0001, 3.26, 0.88, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.42, 0.66, 2.95, 0.116, 12.0, 29.0, 0.997, 3.24, 0.75, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.43, 0.66, 3.1, 0.109, 10.0, 23.0, 1.0, 3.15, 0.85, 10.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [12.6, 0.38, 0.66, 2.6, 8.8e-2, 10.0, 41.0, 1.001, 3.17, 0.68, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.7, 0.23, 2.0, 9.9e-2, 14.0, 81.0, 0.9973, 3.19, 0.7, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.45, 0.31, 2.6, 8.6e-2, 21.0, 50.0, 0.9982, 3.37, 0.91, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.58, 0.66, 2.5, 7.2e-2, 6.0, 37.0, 0.9992, 3.05, 0.56, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.5, 0.46, 0.63, 2.0, 7.1e-2, 6.0, 15.0, 0.9988, 2.99, 0.87, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.8, 0.615, 0.66, 5.8, 8.3e-2, 7.0, 42.0, 1.0022, 3.07, 0.73, 10.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.42, 0.5, 3.4, 0.107, 7.0, 21.0, 0.9979, 3.26, 0.93, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.8, 0.615, 0.66, 5.8, 8.3e-2, 7.0, 42.0, 1.0022, 3.07, 0.73, 10.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.575, 0.61, 2.6, 7.6e-2, 11.0, 24.0, 1.0, 3.16, 0.69, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.34, 0.52, 2.8, 0.159, 15.0, 75.0, 0.9998, 3.18, 0.64, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.27, 0.53, 2.4, 7.4e-2, 6.0, 18.0, 0.9962, 3.2, 1.13, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.765, 2.0e-2, 2.3, 6.3e-2, 35.0, 63.0, 0.9975, 3.57, 0.78, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.24, 0.4, 1.6, 5.6e-2, 11.0, 25.0, 0.9967, 3.32, 0.87, 8.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.28, 0.48, 1.8, 6.7e-2, 26.0, 46.0, 0.9967, 3.32, 1.04, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.55, 0.22, 2.2, 0.106, 12.0, 72.0, 0.9959, 3.05, 0.63, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [14.0, 0.41, 0.63, 3.8, 8.9e-2, 6.0, 47.0, 1.0014, 3.01, 0.81, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.54, 0.71, 4.4, 0.124, 6.0, 15.0, 0.9984, 3.01, 0.83, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.45, 0.5, 3.0, 7.8e-2, 19.0, 47.0, 1.0003, 3.26, 1.11, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.27, 0.53, 2.4, 7.4e-2, 6.0, 18.0, 0.9962, 3.2, 1.13, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.4, 0.625, 0.66, 6.2, 8.8e-2, 6.0, 24.0, 0.9988, 3.11, 0.99, 13.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.42, 0.38, 2.5, 9.4e-2, 24.0, 60.0, 0.9979, 3.31, 0.7, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.26, 0.42, 2.0, 8.0e-2, 11.0, 27.0, 0.9974, 3.21, 0.8, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [13.7, 0.415, 0.68, 2.9, 8.5e-2, 17.0, 43.0, 1.0014, 3.06, 0.8, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.26, 0.42, 2.0, 8.0e-2, 11.0, 27.0, 0.9974, 3.21, 0.8, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.26, 0.42, 2.0, 8.0e-2, 11.0, 27.0, 0.9974, 3.21, 0.8, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.51, 0.28, 2.1, 8.7e-2, 23.0, 54.0, 0.998, 3.42, 0.74, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.63, 7.0e-2, 2.4, 9.0e-2, 11.0, 37.0, 0.9979, 3.43, 0.76, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.54, 0.26, 2.0, 8.8e-2, 23.0, 48.0, 0.9981, 3.41, 0.74, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.66, 0.15, 1.9, 7.9e-2, 17.0, 42.0, 0.9972, 3.31, 0.54, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.46, 0.26, 1.9, 8.8e-2, 23.0, 53.0, 0.9981, 3.43, 0.74, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.38, 0.31, 2.5, 9.6e-2, 16.0, 49.0, 0.9982, 3.19, 0.7, 10.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.85, 5.0e-2, 1.4, 4.5e-2, 12.0, 88.0, 0.9924, 3.56, 0.82, 12.9]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [13.7, 0.415, 0.68, 2.9, 8.5e-2, 17.0, 43.0, 1.0014, 3.06, 0.8, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.37, 0.52, 2.0, 8.2e-2, 6.0, 26.0, 0.998, 3.18, 0.51, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.665, 0.61, 2.0, 0.112, 13.0, 95.0, 0.997, 3.16, 0.54, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.7, 0.6, 0.65, 2.3, 6.3e-2, 6.0, 25.0, 0.9997, 3.03, 0.57, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.0, 0.37, 0.76, 4.2, 6.6e-2, 7.0, 38.0, 1.0004, 3.22, 0.6, 13.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.735, 2.0e-2, 7.9, 0.122, 68.0, 124.0, 0.9994, 3.47, 0.53, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.59, 0.59, 2.6, 8.7e-2, 13.0, 49.0, 0.9988, 3.18, 0.65, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.59, 0.59, 2.6, 8.7e-2, 13.0, 49.0, 0.9988, 3.18, 0.65, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.765, 0.22, 2.3, 6.4e-2, 9.0, 42.0, 0.9963, 3.1, 0.55, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.735, 2.0e-2, 7.9, 0.122, 68.0, 124.0, 0.9994, 3.47, 0.53, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.26, 0.3, 1.7, 5.9e-2, 20.0, 38.0, 0.9949, 3.29, 0.47, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.2, 0.48, 0.54, 2.6, 8.5e-2, 19.0, 64.0, 1.0, 3.1, 0.61, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.4, 0.6, 0.49, 2.7, 8.5e-2, 10.0, 41.0, 0.9994, 3.15, 0.63, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.69, 5.0e-2, 2.7, 7.5e-2, 15.0, 27.0, 0.9974, 3.26, 0.61, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.31, 0.46, 1.4, 5.9e-2, 11.0, 25.0, 0.9966, 3.36, 0.76, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.44, 0.47, 2.5, 6.3e-2, 9.0, 28.0, 0.9981, 3.24, 0.65, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.0, 0.39, 0.66, 3.0, 9.3e-2, 12.0, 30.0, 0.9996, 3.18, 0.63, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.34, 0.58, 3.7, 0.174, 6.0, 16.0, 0.997, 3.19, 0.7, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.5, 0.46, 0.49, 4.5, 7.0e-2, 26.0, 49.0, 0.9981, 3.05, 0.57, 9.6]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.43, 0.34, 2.5, 8.0e-2, 26.0, 86.0, 0.9987, 3.38, 0.62, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.45, 0.35, 2.4, 8.0e-2, 23.0, 78.0, 0.9987, 3.38, 0.62, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.735, 0.16, 1.9, 0.1, 15.0, 77.0, 0.9966, 3.27, 0.64, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.4, 0.53, 6.7, 9.7e-2, 6.0, 19.0, 0.9986, 3.27, 0.82, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.52, 0.34, 2.7, 8.7e-2, 24.0, 122.0, 0.9982, 3.26, 0.61, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.725, 0.24, 6.6, 0.117, 31.0, 134.0, 1.0014, 3.32, 1.07, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.48, 0.64, 2.2, 0.111, 6.0, 20.0, 0.997, 3.26, 0.66, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.58, 0.12, 1.9, 9.1e-2, 34.0, 124.0, 0.9956, 3.44, 0.48, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.38, 0.51, 2.0, 0.121, 7.0, 20.0, 0.9996, 3.24, 0.76, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.77, 0.0, 1.8, 6.6e-2, 34.0, 52.0, 0.9976, 3.62, 0.68, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.56, 0.33, 2.4, 8.9e-2, 35.0, 67.0, 0.9972, 3.28, 0.73, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.84, 3.0e-2, 2.3, 5.9e-2, 32.0, 48.0, 0.9952, 3.52, 0.56, 12.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.96, 0.2, 2.0, 4.7e-2, 15.0, 60.0, 0.9955, 3.36, 0.44, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.24, 0.47, 2.1, 6.6e-2, 6.0, 24.0, 0.9978, 3.15, 0.9, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.96, 0.2, 2.0, 4.7e-2, 15.0, 60.0, 0.9955, 3.36, 0.44, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.84, 3.0e-2, 2.3, 5.9e-2, 32.0, 48.0, 0.9952, 3.52, 0.56, 12.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.67, 8.0e-2, 2.1, 4.5e-2, 19.0, 48.0, 0.9949, 3.49, 0.49, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.78, 0.22, 1.9, 7.7e-2, 6.0, 32.0, 0.9988, 3.26, 0.56, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.52, 0.33, 1.3, 7.0e-2, 9.0, 30.0, 0.9978, 3.24, 0.6, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.8, 0.84, 0.63, 2.4, 8.8e-2, 13.0, 35.0, 0.9997, 3.1, 0.6, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.24, 0.47, 2.1, 6.6e-2, 6.0, 24.0, 0.9978, 3.15, 0.9, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.55, 0.35, 2.2, 7.4e-2, 21.0, 66.0, 0.9974, 3.25, 0.56, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.37, 0.69, 2.3, 7.8e-2, 12.0, 24.0, 0.9958, 3.0, 0.65, 12.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.3, 0.39, 0.63, 2.3, 9.1e-2, 6.0, 18.0, 1.0004, 3.16, 0.49, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.41, 0.55, 3.2, 7.6e-2, 22.0, 54.0, 0.9996, 3.15, 0.89, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.3, 0.39, 0.63, 2.3, 9.1e-2, 6.0, 18.0, 1.0004, 3.16, 0.49, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.67, 0.3, 2.0, 6.0e-2, 38.0, 62.0, 0.9958, 3.26, 0.56, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.45, 0.73, 3.2, 6.6e-2, 6.0, 22.0, 0.9986, 3.17, 0.66, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.41, 0.55, 3.2, 7.6e-2, 22.0, 54.0, 0.9996, 3.15, 0.89, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.62, 0.18, 1.5, 6.2e-2, 7.0, 50.0, 0.9951, 3.08, 0.6, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.6, 0.31, 0.72, 2.2, 7.2e-2, 6.0, 29.0, 0.9987, 2.88, 0.82, 9.8]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.4, 0.65, 2.15, 6.8e-2, 7.0, 27.0, 0.9988, 3.06, 0.68, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [15.6, 0.685, 0.76, 3.7, 0.1, 6.0, 43.0, 1.0032, 2.95, 0.68, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.44, 0.49, 2.7, 7.7e-2, 11.0, 19.0, 0.9963, 3.23, 0.63, 11.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [5.3, 0.57, 1.0e-2, 1.7, 5.4e-2, 5.0, 27.0, 0.9934, 3.57, 0.84, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.735, 0.1, 2.1, 7.9e-2, 6.0, 31.0, 0.9986, 3.23, 0.56, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.5, 0.38, 0.6, 2.6, 8.1e-2, 31.0, 72.0, 0.9996, 3.1, 0.73, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.48, 0.29, 2.1, 0.127, 6.0, 16.0, 0.9968, 3.22, 0.72, 11.2]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [8.6, 0.53, 0.22, 2.0, 0.1, 7.0, 27.0, 0.9967, 3.2, 0.56, 10.2], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.39, 0.69, 2.8, 9.5e-2, 17.0, 35.0, 0.9994, 3.1, 0.61, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.39, 0.69, 2.8, 9.5e-2, 17.0, 35.0, 0.9994, 3.1, 0.61, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.37, 0.53, 1.8, 0.413, 9.0, 26.0, 0.9979, 3.06, 1.06, 9.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.56, 3.0e-2, 1.7, 8.4e-2, 18.0, 35.0, 0.9968, 3.44, 0.63, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.33, 0.63, 2.8, 8.4e-2, 5.0, 22.0, 0.9998, 3.26, 0.74, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.23, 0.4, 1.6, 6.3e-2, 21.0, 67.0, 0.9952, 3.5, 0.63, 11.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.3, 0.62, 0.67, 5.2, 8.6e-2, 6.0, 19.0, 0.9988, 3.22, 0.69, 13.4]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.59, 0.39, 2.3, 9.5e-2, 5.0, 22.0, 0.9986, 3.37, 0.58, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.63, 0.21, 2.7, 9.7e-2, 29.0, 65.0, 0.9988, 3.28, 0.58, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.33, 0.63, 2.8, 8.4e-2, 5.0, 22.0, 0.9998, 3.26, 0.74, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.58, 0.66, 2.2, 7.4e-2, 10.0, 47.0, 1.0008, 3.25, 0.57, 9.0]
            , label = Label "3"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.43, 0.52, 2.3, 8.3e-2, 14.0, 23.0, 0.9976, 3.35, 0.61, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.615, 0.22, 2.6, 8.7e-2, 6.0, 19.0, 0.9982, 3.26, 0.61, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.0, 0.26, 0.68, 2.55, 8.5e-2, 10.0, 25.0, 0.997, 3.18, 0.61, 11.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.66, 0.7, 2.2, 9.8e-2, 25.0, 129.0, 0.9972, 3.08, 0.53, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.315, 0.54, 2.1, 8.4e-2, 5.0, 15.0, 0.9987, 2.98, 0.7, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.29, 0.4, 2.9, 9.8e-2, 10.0, 26.0, 1.0006, 3.48, 0.91, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.5, 0.42, 2.0, 6.9e-2, 21.0, 51.0, 0.9982, 3.16, 0.72, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.46, 0.45, 2.6, 6.5e-2, 7.0, 18.0, 0.9947, 3.32, 0.79, 14.0]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [11.4, 0.36, 0.69, 2.1, 9.0e-2, 6.0, 21.0, 1.0, 3.17, 0.62, 9.2], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.82, 2.0e-2, 1.2, 7.0e-2, 36.0, 48.0, 0.9952, 3.2, 0.58, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [13.0, 0.32, 0.65, 2.6, 9.3e-2, 15.0, 47.0, 0.9996, 3.05, 0.61, 10.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.54, 0.42, 2.4, 8.1e-2, 25.0, 52.0, 0.997, 3.2, 0.71, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.5, 0.37, 0.55, 2.6, 8.3e-2, 25.0, 68.0, 0.9995, 3.15, 0.82, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.35, 0.55, 2.1, 6.2e-2, 5.0, 14.0, 0.9971, 3.26, 0.79, 10.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.28, 0.51, 1.7, 8.0e-2, 10.0, 24.0, 0.9982, 3.2, 0.89, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.68, 0.24, 2.2, 8.7e-2, 5.0, 28.0, 0.9988, 3.14, 0.6, 10.2]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [9.3, 0.27, 0.41, 2.0, 9.1e-2, 6.0, 16.0, 0.998, 3.28, 0.7, 9.7], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.24, 0.49, 1.8, 7.5e-2, 6.0, 20.0, 0.9977, 3.18, 1.06, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.68, 0.24, 2.2, 8.7e-2, 5.0, 28.0, 0.9988, 3.14, 0.6, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.685, 0.11, 2.7, 7.7e-2, 6.0, 31.0, 0.9984, 3.19, 0.7, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.28, 0.39, 15.5, 6.9e-2, 6.0, 23.0, 1.0026, 3.12, 0.66, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.3, 0.56, 2.8, 8.0e-2, 6.0, 17.0, 0.9964, 3.15, 0.92, 11.7]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.36, 0.59, 2.2, 0.152, 6.0, 18.0, 0.9986, 3.04, 1.05, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.36, 0.6, 2.2, 0.152, 7.0, 18.0, 0.9986, 3.04, 1.06, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.44, 0.68, 4.1, 0.114, 6.0, 24.0, 0.997, 3.06, 0.66, 13.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.67, 0.39, 1.9, 5.4e-2, 6.0, 17.0, 0.9976, 3.17, 0.47, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.67, 0.39, 1.9, 5.4e-2, 6.0, 17.0, 0.9976, 3.17, 0.47, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.645, 0.36, 1.8, 5.3e-2, 5.0, 14.0, 0.9982, 3.17, 0.42, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.32, 0.55, 2.8, 8.1e-2, 35.0, 67.0, 1.0002, 3.32, 0.92, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.39, 0.4, 2.6, 7.3e-2, 10.0, 26.0, 0.9984, 3.34, 0.75, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.775, 0.27, 2.8, 7.8e-2, 24.0, 56.0, 0.9984, 3.31, 0.67, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.41, 0.5, 2.5, 5.5e-2, 12.0, 25.0, 0.9952, 3.34, 0.79, 13.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.4, 0.51, 2.6, 5.2e-2, 13.0, 27.0, 0.995, 3.32, 0.9, 13.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.69, 0.31, 3.0, 8.6e-2, 23.0, 81.0, 1.0002, 3.48, 0.74, 11.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.39, 0.23, 8.3, 5.1e-2, 28.0, 91.0, 0.9952, 3.44, 0.55, 12.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.35, 0.53, 2.6, 7.0e-2, 5.0, 16.0, 0.9972, 3.15, 0.65, 11.0]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.52, 0.25, 1.9, 8.1e-2, 14.0, 38.0, 0.9984, 3.43, 0.65, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.34, 0.32, 2.5, 9.0e-2, 43.0, 113.0, 0.9966, 3.32, 0.79, 11.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.35, 0.53, 2.6, 7.0e-2, 5.0, 16.0, 0.9972, 3.15, 0.65, 11.0]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.69, 0.31, 3.0, 8.6e-2, 23.0, 81.0, 1.0002, 3.48, 0.74, 11.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.52, 0.25, 1.9, 8.1e-2, 14.0, 38.0, 0.9984, 3.43, 0.65, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.44, 0.73, 6.55, 7.4e-2, 38.0, 76.0, 0.999, 3.17, 0.85, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.44, 0.73, 6.55, 7.4e-2, 38.0, 76.0, 0.999, 3.17, 0.85, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.26, 0.47, 1.9, 7.8e-2, 6.0, 24.0, 0.9976, 3.18, 1.04, 10.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.24, 0.42, 1.8, 7.7e-2, 6.0, 22.0, 0.9976, 3.21, 1.05, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.49, 0.63, 2.9, 7.2e-2, 10.0, 26.0, 0.9968, 3.16, 0.78, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.24, 0.46, 1.8, 7.5e-2, 6.0, 21.0, 0.9976, 3.25, 1.02, 10.8]
            , label = Label "7"
            }
        , DataEntry
            {dataPoint = V.fromList [11.2, 0.67, 0.55, 2.3, 8.4e-2, 6.0, 13.0, 1.0, 3.17, 0.71, 9.5], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.59, 0.31, 2.2, 9.0e-2, 26.0, 62.0, 0.9994, 3.18, 0.63, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [13.3, 0.29, 0.75, 2.8, 8.4e-2, 23.0, 43.0, 0.9986, 3.04, 0.68, 11.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [12.4, 0.42, 0.49, 4.6, 7.3e-2, 19.0, 43.0, 0.9978, 3.02, 0.61, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.59, 0.31, 2.2, 9.0e-2, 26.0, 62.0, 0.9994, 3.18, 0.63, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.4, 0.48, 2.1, 0.125, 15.0, 49.0, 0.998, 3.03, 0.81, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.51, 0.64, 2.4, 0.107, 6.0, 15.0, 0.9973, 3.09, 0.66, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.51, 0.64, 2.4, 0.107, 6.0, 15.0, 0.9973, 3.09, 0.66, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.655, 0.49, 6.1, 0.122, 34.0, 151.0, 1.001, 3.31, 1.14, 9.3]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [12.5, 0.6, 0.49, 4.3, 0.1, 5.0, 14.0, 1.001, 3.25, 0.74, 11.9], label = Label "6"}
        , DataEntry
            {dataPoint = V.fromList [10.4, 0.61, 0.49, 2.1, 0.2, 5.0, 16.0, 0.9994, 3.16, 0.63, 8.4], label = Label "3"}
        , DataEntry
            { dataPoint = V.fromList [10.9, 0.21, 0.49, 2.8, 8.8e-2, 11.0, 32.0, 0.9972, 3.22, 0.68, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.365, 0.49, 2.5, 8.8e-2, 39.0, 106.0, 0.9966, 3.36, 0.78, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.25, 0.49, 2.7, 8.8e-2, 15.0, 33.0, 0.9982, 3.42, 0.9, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.41, 0.49, 2.0, 8.8e-2, 16.0, 43.0, 0.998, 3.48, 0.64, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.39, 0.49, 2.3, 9.9e-2, 47.0, 133.0, 0.9979, 3.38, 0.99, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.4, 0.49, 2.5, 8.5e-2, 38.0, 142.0, 0.9978, 3.22, 0.55, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.43, 0.49, 2.4, 8.6e-2, 23.0, 116.0, 0.9976, 3.23, 0.64, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.64, 0.24, 2.8, 0.105, 29.0, 53.0, 0.9998, 3.24, 0.67, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.365, 0.49, 2.5, 8.8e-2, 39.0, 106.0, 0.9966, 3.36, 0.78, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.38, 0.49, 2.5, 9.7e-2, 33.0, 85.0, 0.9962, 3.39, 0.77, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.42, 0.49, 2.6, 8.4e-2, 32.0, 55.0, 0.9988, 3.34, 0.75, 8.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.63, 0.24, 2.4, 7.7e-2, 6.0, 33.0, 0.9974, 3.09, 0.57, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.22, 0.24, 2.1, 7.8e-2, 1.0, 28.0, 0.999, 3.41, 0.87, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.38, 0.49, 2.7, 9.8e-2, 12.0, 42.0, 1.0004, 3.16, 0.61, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.38, 0.49, 2.7, 9.8e-2, 12.0, 42.0, 1.0004, 3.16, 0.61, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.27, 0.24, 2.1, 7.2e-2, 15.0, 33.0, 0.9956, 3.22, 0.66, 12.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.48, 0.24, 2.7, 0.102, 13.0, 32.0, 1.0, 3.28, 0.56, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.22, 0.24, 2.1, 7.8e-2, 1.0, 28.0, 0.999, 3.41, 0.87, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.63, 0.24, 2.4, 7.7e-2, 6.0, 33.0, 0.9974, 3.09, 0.57, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.825, 0.24, 2.1, 8.4e-2, 5.0, 13.0, 0.9972, 3.37, 0.77, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.9, 0.35, 0.49, 5.8, 6.6e-2, 5.0, 35.0, 1.0014, 3.2, 0.66, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.2, 0.5, 0.74, 5.15, 0.1, 5.0, 17.0, 0.9996, 3.22, 0.62, 11.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.59, 0.24, 3.3, 0.101, 20.0, 47.0, 0.9988, 3.26, 0.67, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.46, 0.49, 6.3, 6.4e-2, 5.0, 17.0, 0.9988, 3.21, 0.73, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.715, 0.24, 2.1, 7.0e-2, 5.0, 20.0, 0.9966, 3.12, 0.59, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.2, 0.66, 0.24, 2.5, 8.5e-2, 16.0, 53.0, 0.9993, 3.06, 0.72, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [14.3, 0.31, 0.74, 1.8, 7.5e-2, 6.0, 15.0, 1.0008, 2.86, 0.79, 8.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.47, 0.49, 2.6, 9.4e-2, 38.0, 106.0, 0.9982, 3.08, 0.59, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.55, 0.24, 2.0, 7.8e-2, 10.0, 28.0, 0.9983, 3.45, 0.78, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.31, 0.49, 2.5, 6.7e-2, 6.0, 21.0, 0.9987, 3.26, 0.86, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.4, 0.35, 0.49, 2.6, 7.9e-2, 27.0, 69.0, 0.9994, 3.12, 0.75, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.53, 0.49, 1.9, 0.171, 6.0, 25.0, 0.9975, 3.27, 0.61, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.51, 1.0e-2, 2.1, 7.4e-2, 9.0, 25.0, 0.9958, 3.33, 0.56, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.43, 0.24, 2.8, 9.2e-2, 14.0, 45.0, 0.998, 3.19, 0.73, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.46, 0.24, 2.7, 9.2e-2, 14.0, 44.0, 0.998, 3.12, 0.74, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.0, 1.04, 0.24, 1.6, 5.0e-2, 32.0, 96.0, 0.9934, 3.74, 0.62, 11.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [15.5, 0.645, 0.49, 4.2, 9.5e-2, 10.0, 23.0, 1.00315, 2.92, 0.74, 11.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [15.5, 0.645, 0.49, 4.2, 9.5e-2, 10.0, 23.0, 1.00315, 2.92, 0.74, 11.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.9, 0.53, 0.49, 4.6, 0.118, 10.0, 17.0, 1.0002, 3.07, 0.56, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [15.6, 0.645, 0.49, 4.2, 9.5e-2, 10.0, 23.0, 1.00315, 2.92, 0.74, 11.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.9, 0.53, 0.49, 4.6, 0.118, 10.0, 17.0, 1.0002, 3.07, 0.56, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [13.0, 0.47, 0.49, 4.3, 8.5e-2, 6.0, 47.0, 1.0021, 3.3, 0.68, 12.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.7, 0.6, 0.49, 2.8, 7.5e-2, 5.0, 19.0, 0.9994, 3.14, 0.57, 11.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.44, 0.49, 2.4, 7.8e-2, 26.0, 121.0, 0.9978, 3.23, 0.58, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.54, 0.49, 2.9, 9.4e-2, 41.0, 110.0, 0.9982, 3.08, 0.61, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.29, 0.49, 2.7, 9.2e-2, 25.0, 60.0, 0.9971, 3.31, 0.61, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [13.0, 0.47, 0.49, 4.3, 8.5e-2, 6.0, 47.0, 1.0021, 3.3, 0.68, 12.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.7, 0.6, 0.49, 2.8, 7.5e-2, 5.0, 19.0, 0.9994, 3.14, 0.57, 11.4]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [8.7, 0.7, 0.24, 2.5, 0.226, 5.0, 15.0, 0.9991, 3.32, 0.6, 9.0], label = Label "6"}
        , DataEntry
            {dataPoint = V.fromList [8.7, 0.7, 0.24, 2.5, 0.226, 5.0, 15.0, 0.9991, 3.32, 0.6, 9.0], label = Label "6"}
        , DataEntry
            {dataPoint = V.fromList [9.8, 0.5, 0.49, 2.6, 0.25, 5.0, 20.0, 0.999, 3.31, 0.79, 10.7], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.36, 0.24, 2.2, 9.5e-2, 19.0, 42.0, 0.9946, 3.57, 0.57, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.35, 0.49, 3.3, 7.0e-2, 10.0, 37.0, 1.0003, 3.32, 0.91, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.36, 0.24, 2.2, 9.5e-2, 19.0, 42.0, 0.9946, 3.57, 0.57, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.24, 0.49, 2.4, 7.5e-2, 10.0, 28.0, 0.9978, 3.14, 0.61, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.59, 0.49, 2.1, 7.0e-2, 14.0, 47.0, 0.9991, 3.3, 0.56, 9.6]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.34, 0.49, 3.2, 7.8e-2, 20.0, 78.0, 0.9992, 3.19, 0.7, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.3, 0.27, 0.49, 3.1, 7.9e-2, 28.0, 46.0, 0.9993, 3.2, 0.8, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.5, 0.24, 2.3, 0.103, 6.0, 14.0, 0.9978, 3.34, 0.52, 10.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.44, 0.49, 2.8, 8.3e-2, 18.0, 111.0, 0.9982, 3.3, 0.6, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.47, 0.49, 2.9, 8.5e-2, 17.0, 110.0, 0.9982, 3.29, 0.6, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.31, 0.49, 2.2, 6.3e-2, 18.0, 40.0, 0.9976, 3.14, 0.51, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.3, 0.5, 0.49, 2.2, 8.9e-2, 5.0, 14.0, 1.0002, 3.19, 0.44, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.3, 0.5, 0.49, 2.2, 8.9e-2, 5.0, 14.0, 1.0002, 3.19, 0.44, 9.6]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [11.7, 0.49, 0.49, 2.2, 8.3e-2, 5.0, 15.0, 1.0, 3.19, 0.43, 9.2], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [12.0, 0.28, 0.49, 1.9, 7.4e-2, 10.0, 21.0, 0.9976, 2.98, 0.66, 9.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.8, 0.33, 0.49, 3.4, 9.3e-2, 54.0, 80.0, 1.0002, 3.3, 0.76, 10.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.51, 0.24, 2.4, 9.1e-2, 8.0, 38.0, 0.998, 3.47, 0.66, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.31, 0.49, 2.7, 9.4e-2, 16.0, 47.0, 0.9986, 3.12, 1.02, 10.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.73, 0.24, 1.9, 0.108, 18.0, 102.0, 0.9967, 3.26, 0.59, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.0, 0.42, 0.24, 2.0, 6.0e-2, 19.0, 50.0, 0.9917, 3.72, 0.74, 14.0]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.29, 0.49, 2.6, 5.9e-2, 5.0, 13.0, 0.9976, 3.05, 0.74, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.45, 0.49, 2.6, 8.4e-2, 21.0, 75.0, 0.9987, 3.35, 0.57, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.39, 0.49, 1.7, 7.0e-2, 23.0, 149.0, 0.9922, 3.12, 0.5, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.45, 0.49, 2.6, 8.4e-2, 21.0, 75.0, 0.9987, 3.35, 0.57, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.49, 0.58, 3.5, 9.4e-2, 9.0, 43.0, 1.0004, 3.29, 0.58, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.72, 0.17, 2.6, 9.6e-2, 20.0, 38.0, 0.9978, 3.4, 0.53, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.595, 0.41, 7.9, 8.6e-2, 30.0, 109.0, 0.9998, 3.27, 0.57, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.4, 0.4, 0.51, 2.0, 5.9e-2, 6.0, 24.0, 0.9994, 3.04, 0.6, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.9, 0.58, 0.58, 1.9, 7.1e-2, 5.0, 18.0, 0.998, 3.09, 0.63, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.585, 0.18, 2.1, 7.8e-2, 5.0, 30.0, 0.9967, 3.2, 0.48, 9.8]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [12.7, 0.59, 0.45, 2.3, 8.2e-2, 11.0, 22.0, 1.0, 3.0, 0.7, 9.3], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.915, 0.27, 2.1, 8.8e-2, 7.0, 23.0, 0.9962, 3.26, 0.47, 10.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [13.2, 0.46, 0.52, 2.2, 7.1e-2, 12.0, 35.0, 1.0006, 3.1, 0.56, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.835, 0.0, 2.6, 8.1e-2, 6.0, 14.0, 0.9975, 3.3, 0.52, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [13.2, 0.46, 0.52, 2.2, 7.1e-2, 12.0, 35.0, 1.0006, 3.1, 0.56, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.58, 0.13, 2.9, 9.6e-2, 14.0, 63.0, 0.9984, 3.17, 0.62, 9.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.6, 0.13, 2.6, 8.5e-2, 6.0, 24.0, 0.9984, 3.31, 0.59, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.41, 0.48, 4.6, 7.2e-2, 10.0, 20.0, 0.9973, 3.34, 0.79, 12.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.48, 0.41, 3.3, 9.2e-2, 26.0, 52.0, 0.9982, 3.31, 0.53, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.65, 0.37, 5.1, 0.11, 11.0, 65.0, 1.0026, 3.32, 0.64, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.36, 0.19, 3.2, 7.5e-2, 15.0, 39.0, 0.9956, 3.56, 0.52, 12.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.24, 0.54, 2.5, 8.3e-2, 25.0, 57.0, 0.9983, 3.39, 0.54, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [13.2, 0.38, 0.55, 2.7, 8.1e-2, 5.0, 16.0, 1.0006, 2.98, 0.54, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.64, 0.0, 2.4, 7.7e-2, 18.0, 29.0, 0.9965, 3.32, 0.6, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.39, 0.38, 1.5, 5.8e-2, 10.0, 29.0, 0.9962, 3.26, 0.74, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.755, 0.18, 2.2, 0.148, 10.0, 103.0, 0.9969, 2.87, 1.36, 10.2]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [9.6, 0.6, 0.5, 2.3, 7.9e-2, 28.0, 71.0, 0.9997, 3.5, 0.57, 9.7], label = Label "5"}
        , DataEntry
            {dataPoint = V.fromList [9.6, 0.6, 0.5, 2.3, 7.9e-2, 28.0, 71.0, 0.9997, 3.5, 0.57, 9.7], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.31, 0.51, 2.2, 7.9e-2, 14.0, 28.0, 0.9982, 3.03, 0.93, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.4, 0.46, 0.5, 2.7, 0.122, 4.0, 17.0, 1.0006, 3.13, 0.7, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.3, 0.37, 0.41, 2.3, 8.8e-2, 6.0, 16.0, 0.9988, 3.09, 0.8, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.54, 0.24, 3.4, 7.6e-2, 16.0, 112.0, 0.9976, 3.27, 0.61, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.56, 0.23, 3.4, 7.8e-2, 14.0, 104.0, 0.9976, 3.28, 0.62, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.58, 0.22, 1.9, 8.0e-2, 9.0, 32.0, 0.9974, 3.13, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.51, 0.25, 2.9, 7.7e-2, 21.0, 45.0, 0.9974, 3.49, 0.96, 12.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.69, 0.0, 5.6, 0.124, 21.0, 58.0, 0.9997, 3.46, 0.72, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.69, 0.0, 5.6, 0.124, 21.0, 58.0, 0.9997, 3.46, 0.72, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.6, 0.29, 2.2, 9.8e-2, 5.0, 15.0, 0.9988, 3.36, 0.49, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.6, 0.29, 2.2, 9.8e-2, 5.0, 15.0, 0.9988, 3.36, 0.49, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.54, 0.26, 2.5, 9.7e-2, 7.0, 31.0, 0.9976, 3.27, 0.6, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.685, 0.23, 2.3, 0.111, 20.0, 84.0, 0.9964, 3.21, 0.61, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.54, 0.26, 2.5, 9.7e-2, 7.0, 31.0, 0.9976, 3.27, 0.6, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.28, 0.54, 2.7, 0.105, 5.0, 19.0, 0.9988, 3.25, 0.63, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.41, 0.14, 3.0, 8.7e-2, 21.0, 43.0, 0.9964, 3.32, 0.57, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.935, 0.22, 3.4, 0.105, 11.0, 86.0, 1.001, 3.43, 0.64, 11.3]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.35, 0.21, 1.9, 7.3e-2, 46.0, 102.0, 0.9964, 3.27, 0.58, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.84, 0.0, 1.4, 6.5e-2, 24.0, 33.0, 0.9954, 3.27, 0.55, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.88, 0.28, 2.4, 8.6e-2, 30.0, 147.0, 0.9979, 3.24, 0.53, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.885, 0.27, 2.3, 8.4e-2, 31.0, 145.0, 0.9978, 3.24, 0.53, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.915, 0.12, 2.2, 0.143, 7.0, 23.0, 0.9964, 3.35, 0.65, 10.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.29, 0.35, 1.9, 6.7e-2, 25.0, 57.0, 0.997, 3.18, 1.36, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.54, 0.45, 2.3, 7.1e-2, 16.0, 40.0, 0.9991, 3.39, 0.62, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.59, 0.44, 2.3, 7.1e-2, 21.0, 68.0, 0.9992, 3.46, 0.63, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.54, 0.45, 2.3, 7.1e-2, 16.0, 40.0, 0.9991, 3.39, 0.62, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.59, 0.44, 2.3, 7.1e-2, 21.0, 68.0, 0.9992, 3.46, 0.63, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.54, 0.45, 2.3, 7.1e-2, 16.0, 40.0, 0.9991, 3.39, 0.62, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.64, 0.1, 6.0, 0.115, 5.0, 11.0, 0.9984, 3.37, 0.69, 10.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.67, 5.0e-2, 3.6, 0.107, 6.0, 20.0, 0.9972, 3.4, 0.63, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.845, 1.0e-2, 2.2, 7.0e-2, 5.0, 14.0, 0.9967, 3.32, 0.58, 11.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.48, 0.3, 2.8, 6.6e-2, 10.0, 28.0, 0.9964, 3.33, 0.67, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.42, 0.27, 8.6, 6.8e-2, 24.0, 148.0, 0.9948, 3.16, 0.57, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.43, 0.39, 2.2, 0.106, 8.0, 32.0, 0.9986, 2.89, 0.5, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.88, 0.25, 2.5, 0.104, 35.0, 155.0, 1.001, 3.41, 0.67, 11.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [15.9, 0.36, 0.65, 7.5, 9.6e-2, 22.0, 71.0, 0.9976, 2.98, 0.84, 14.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.33, 0.59, 2.8, 7.9e-2, 9.0, 30.0, 0.9976, 3.12, 0.54, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.47, 0.47, 2.4, 7.4e-2, 7.0, 29.0, 0.9979, 3.08, 0.46, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.55, 0.17, 2.9, 8.7e-2, 20.0, 53.0, 1.0004, 3.14, 0.61, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.43, 0.39, 2.2, 0.106, 8.0, 32.0, 0.9986, 2.89, 0.5, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.0, 0.5, 0.59, 1.4, 7.3e-2, 23.0, 42.0, 0.998, 2.92, 0.68, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.52, 7.0e-2, 1.4, 7.4e-2, 5.0, 20.0, 0.9973, 3.32, 0.81, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.84, 2.0e-2, 4.4, 9.6e-2, 5.0, 13.0, 0.997, 3.41, 0.57, 11.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.52, 7.0e-2, 1.4, 7.4e-2, 5.0, 20.0, 0.9973, 3.32, 0.81, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.42, 0.31, 1.6, 8.0e-2, 15.0, 42.0, 0.9978, 3.31, 0.64, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.57, 6.0e-2, 1.6, 7.6e-2, 9.0, 27.0, 0.9972, 3.36, 0.7, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.28, 0.46, 1.8, 5.0e-2, 5.0, 13.0, 0.9974, 3.04, 0.79, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.1, 0.4, 0.52, 2.0, 9.2e-2, 15.0, 54.0, 1.0, 3.03, 0.66, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.59, 0.14, 2.0, 8.4e-2, 25.0, 48.0, 0.9981, 3.14, 0.56, 9.7]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [8.3, 0.49, 0.36, 1.8, 0.222, 6.0, 16.0, 0.998, 3.18, 0.6, 9.5], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [11.3, 0.34, 0.45, 2.0, 8.2e-2, 6.0, 15.0, 0.9988, 2.94, 0.66, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.73, 0.43, 2.3, 5.9e-2, 15.0, 31.0, 0.9966, 3.15, 0.57, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.3, 0.34, 0.45, 2.0, 8.2e-2, 6.0, 15.0, 0.9988, 2.94, 0.66, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.4, 0.24, 2.5, 8.3e-2, 30.0, 45.0, 0.9959, 3.26, 0.58, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.73, 0.21, 1.7, 7.4e-2, 5.0, 13.0, 0.9968, 3.2, 0.52, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 1.24, 0.34, 2.0, 7.9e-2, 32.0, 151.0, 0.998, 3.15, 0.53, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.73, 0.21, 1.7, 7.4e-2, 5.0, 13.0, 0.9968, 3.2, 0.52, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.4, 0.41, 2.2, 8.4e-2, 7.0, 17.0, 0.9984, 3.08, 0.67, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.41, 0.39, 2.2, 6.4e-2, 12.0, 31.0, 0.9984, 3.26, 0.65, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.4, 0.41, 2.2, 8.4e-2, 7.0, 17.0, 0.9984, 3.08, 0.67, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.8, 0.11, 2.3, 8.4e-2, 12.0, 31.0, 0.9979, 3.4, 0.48, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.78, 0.1, 2.6, 8.1e-2, 45.0, 87.0, 0.9983, 3.48, 0.53, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.26, 0.45, 3.3, 6.0e-2, 20.0, 49.0, 0.9972, 3.13, 0.54, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [13.3, 0.43, 0.58, 1.9, 7.0e-2, 15.0, 40.0, 1.0004, 3.06, 0.49, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.45, 0.23, 2.2, 9.4e-2, 16.0, 29.0, 0.9962, 3.21, 0.49, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.46, 0.31, 2.25, 7.8e-2, 32.0, 58.0, 0.998, 3.33, 0.54, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.78, 0.23, 2.6, 5.9e-2, 5.0, 15.0, 0.997, 3.37, 0.56, 11.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.98, 0.32, 2.3, 7.8e-2, 35.0, 152.0, 0.998, 3.25, 0.48, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.78, 0.23, 2.6, 5.9e-2, 5.0, 15.0, 0.997, 3.37, 0.56, 11.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.65, 0.18, 1.8, 7.0e-2, 13.0, 40.0, 0.997, 3.44, 0.6, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.64, 0.23, 3.1, 9.5e-2, 13.0, 38.0, 0.9998, 3.28, 0.59, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.66, 4.0e-2, 1.6, 3.9e-2, 4.0, 9.0, 0.9962, 3.4, 0.47, 9.4]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [8.1, 0.38, 0.48, 1.8, 0.157, 5.0, 17.0, 0.9976, 3.3, 1.05, 9.4], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [7.4, 1.185, 0.0, 4.25, 9.7e-2, 5.0, 14.0, 0.9966, 3.63, 0.54, 10.7]
            , label = Label "3"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.92, 0.24, 2.6, 8.7e-2, 12.0, 93.0, 0.9998, 3.48, 0.54, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.49, 0.51, 2.0, 0.422, 16.0, 62.0, 0.9979, 3.03, 1.17, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.48, 0.32, 2.8, 8.4e-2, 21.0, 122.0, 0.9984, 3.32, 0.62, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.47, 0.31, 2.7, 8.4e-2, 24.0, 125.0, 0.9984, 3.31, 0.61, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.1, 0.47, 2.0e-2, 1.3, 3.4e-2, 18.0, 44.0, 0.9921, 3.9, 0.62, 12.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.65, 2.0e-2, 2.1, 6.6e-2, 8.0, 25.0, 0.9972, 3.47, 0.67, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.65, 2.0e-2, 2.1, 6.6e-2, 8.0, 25.0, 0.9972, 3.47, 0.67, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.615, 0.28, 3.2, 8.7e-2, 18.0, 72.0, 1.0001, 3.31, 0.53, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.8, 0.38, 0.55, 2.1, 7.1e-2, 5.0, 19.0, 0.9986, 3.11, 0.62, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 1.02, 0.43, 2.9, 7.6e-2, 26.0, 88.0, 0.9984, 3.08, 0.57, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.65, 2.0e-2, 2.1, 6.6e-2, 8.0, 25.0, 0.9972, 3.47, 0.67, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.64, 2.0e-2, 2.1, 6.7e-2, 9.0, 23.0, 0.997, 3.47, 0.67, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.38, 0.48, 2.6, 7.3e-2, 22.0, 84.0, 0.9972, 3.32, 0.7, 9.6]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.765, 4.0e-2, 1.6, 7.8e-2, 4.0, 14.0, 0.998, 3.29, 0.54, 9.7]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 1.035, 0.15, 6.0, 7.3e-2, 11.0, 54.0, 0.999, 3.37, 0.49, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.78, 8.0e-2, 2.0, 9.3e-2, 10.0, 19.0, 0.9956, 3.4, 0.47, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.49, 0.19, 3.0, 7.7e-2, 16.0, 37.0, 0.9966, 3.37, 0.51, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.545, 0.12, 2.5, 6.8e-2, 11.0, 35.0, 0.996, 3.34, 0.61, 11.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.31, 0.47, 1.6, 6.2e-2, 13.0, 33.0, 0.9983, 3.27, 0.66, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 1.025, 0.43, 2.8, 8.0e-2, 21.0, 84.0, 0.9985, 3.06, 0.57, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.565, 0.34, 3.0, 9.3e-2, 16.0, 112.0, 0.9998, 3.38, 0.61, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.69, 0.0, 3.2, 8.4e-2, 13.0, 33.0, 0.9992, 3.36, 0.45, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.43, 0.36, 2.3, 7.5e-2, 10.0, 48.0, 0.9976, 3.34, 0.46, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.74, 0.28, 2.6, 7.8e-2, 21.0, 77.0, 0.998, 3.28, 0.51, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.49, 0.18, 2.7, 6.9e-2, 13.0, 34.0, 0.9967, 3.29, 0.48, 9.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.43, 0.36, 2.3, 7.5e-2, 10.0, 48.0, 0.9976, 3.34, 0.46, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.46, 0.11, 2.6, 7.9e-2, 12.0, 49.0, 0.9968, 3.21, 0.57, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.56, 4.0e-2, 2.0, 8.2e-2, 10.0, 22.0, 0.9976, 3.22, 0.44, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.66, 0.0, 3.9, 8.6e-2, 17.0, 45.0, 0.9976, 3.46, 0.54, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.56, 4.0e-2, 2.0, 8.2e-2, 10.0, 22.0, 0.9976, 3.22, 0.44, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.48, 0.24, 2.85, 9.4e-2, 35.0, 106.0, 0.9982, 3.1, 0.53, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.42, 8.0e-2, 2.7, 8.4e-2, 15.0, 48.0, 0.9968, 3.21, 0.59, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.31, 0.3, 2.2, 5.3e-2, 36.0, 127.0, 0.9965, 2.94, 1.62, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 1.115, 0.1, 3.1, 8.6e-2, 5.0, 12.0, 0.9958, 3.54, 0.6, 11.2]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.66, 0.17, 3.0, 7.7e-2, 5.0, 13.0, 0.9976, 3.29, 0.55, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.72, 9.0e-2, 2.8, 8.4e-2, 18.0, 49.0, 0.9994, 3.43, 0.72, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.57, 2.0e-2, 1.8, 6.7e-2, 4.0, 11.0, 0.997, 3.46, 0.68, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.57, 2.0e-2, 1.8, 6.7e-2, 4.0, 11.0, 0.997, 3.46, 0.68, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.865, 3.0e-2, 3.2, 7.1e-2, 27.0, 58.0, 0.995, 3.61, 0.49, 12.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.55, 0.66, 2.3, 0.387, 12.0, 37.0, 0.9982, 3.17, 0.67, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.875, 0.13, 3.45, 8.8e-2, 4.0, 14.0, 0.9994, 3.44, 0.52, 11.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.835, 3.0e-2, 2.1, 9.2e-2, 10.0, 19.0, 0.9966, 3.39, 0.47, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.45, 0.34, 2.7, 8.2e-2, 16.0, 72.0, 0.998, 3.55, 0.6, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.56, 0.2, 2.0, 7.5e-2, 9.0, 39.0, 0.9987, 3.48, 0.62, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.965, 0.1, 2.1, 0.112, 11.0, 22.0, 0.9963, 3.26, 0.5, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.965, 0.1, 2.1, 0.112, 11.0, 22.0, 0.9963, 3.26, 0.5, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.59, 0.0, 2.5, 9.3e-2, 19.0, 58.0, 1.0002, 3.5, 0.65, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.46, 0.23, 2.8, 9.2e-2, 28.0, 104.0, 0.9983, 3.1, 0.56, 9.2]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [9.0, 0.69, 0.0, 2.4, 8.8e-2, 19.0, 38.0, 0.999, 3.35, 0.6, 9.3], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.76, 0.29, 4.2, 7.5e-2, 12.0, 16.0, 0.9965, 3.45, 0.68, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.53, 0.24, 2.6, 7.8e-2, 28.0, 139.0, 0.99788, 3.21, 0.57, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.615, 0.0, 1.9, 6.5e-2, 9.0, 18.0, 0.9972, 3.46, 0.65, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.41, 0.58, 2.8, 9.6e-2, 25.0, 101.0, 1.00024, 3.13, 0.53, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.39, 0.54, 2.7, 9.5e-2, 21.0, 101.0, 1.0001, 3.13, 0.51, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.51, 0.18, 2.1, 7.0e-2, 12.0, 28.0, 0.99768, 3.52, 0.73, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.34, 0.38, 2.5, 8.0e-2, 12.0, 57.0, 0.9978, 3.3, 0.47, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.33, 0.4, 2.6, 8.3e-2, 16.0, 68.0, 0.99782, 3.3, 0.48, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.5, 0.18, 2.1, 7.1e-2, 12.0, 31.0, 0.99761, 3.52, 0.72, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.51, 0.18, 2.1, 7.0e-2, 12.0, 28.0, 0.99768, 3.52, 0.73, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.65, 0.1, 2.9, 8.9e-2, 17.0, 40.0, 0.99803, 3.29, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.65, 0.1, 2.9, 8.9e-2, 17.0, 40.0, 0.99803, 3.29, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.54, 0.13, 2.5, 9.7e-2, 24.0, 66.0, 0.99785, 3.39, 0.61, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.65, 0.1, 2.9, 8.9e-2, 17.0, 40.0, 0.99803, 3.29, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.48, 0.68, 1.7, 0.415, 14.0, 32.0, 0.99656, 3.09, 1.06, 9.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.91, 7.0e-2, 1.9, 5.8e-2, 22.0, 47.0, 0.99525, 3.51, 0.43, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.98, 1.0e-2, 2.0, 5.7e-2, 15.0, 33.0, 0.99488, 3.6, 0.46, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.87, 0.0, 2.2, 8.4e-2, 10.0, 31.0, 0.99656, 3.25, 0.5, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.87, 0.0, 2.2, 8.4e-2, 10.0, 31.0, 0.99656, 3.25, 0.5, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.42, 0.21, 2.5, 9.2e-2, 33.0, 88.0, 0.99823, 3.19, 0.52, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.58, 0.25, 2.8, 7.5e-2, 9.0, 104.0, 0.99779, 3.23, 0.57, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.655, 0.26, 2.0, 9.6e-2, 5.0, 35.0, 0.99738, 3.25, 0.42, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.7, 0.0, 1.7, 6.9e-2, 8.0, 19.0, 0.99701, 3.31, 0.53, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.655, 0.26, 2.0, 9.6e-2, 5.0, 35.0, 0.99738, 3.25, 0.42, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.68, 0.11, 2.8, 9.3e-2, 11.0, 44.0, 0.99888, 3.31, 0.55, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.67, 0.1, 3.0, 9.1e-2, 12.0, 48.0, 0.99888, 3.31, 0.54, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.59, 0.18, 2.9, 8.9e-2, 12.0, 74.0, 0.99738, 3.14, 0.54, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.6, 0.32, 2.7, 0.103, 13.0, 98.0, 0.99938, 3.45, 0.62, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.59, 2.0e-2, 2.3, 8.2e-2, 24.0, 94.0, 0.99744, 3.55, 0.53, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.72, 1.0e-2, 1.9, 7.6e-2, 7.0, 32.0, 0.99668, 3.39, 0.54, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.59, 2.0e-2, 2.3, 8.2e-2, 24.0, 94.0, 0.99744, 3.55, 0.53, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.685, 0.26, 2.4, 8.2e-2, 23.0, 143.0, 0.9978, 3.28, 0.55, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.57, 0.27, 2.3, 8.2e-2, 23.0, 144.0, 0.99782, 3.27, 0.55, 9.4]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.9, 0.4, 0.29, 1.8, 0.157, 1.0, 44.0, 0.9973, 3.3, 0.92, 9.5], label = Label "6"}
        , DataEntry
            {dataPoint = V.fromList [7.9, 0.4, 0.3, 1.8, 0.157, 2.0, 45.0, 0.99727, 3.31, 0.91, 9.5], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [7.2, 1.0, 0.0, 3.0, 0.102, 7.0, 16.0, 0.99586, 3.43, 0.46, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.765, 0.18, 2.4, 0.243, 5.5, 48.0, 0.99612, 3.4, 0.6, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.635, 0.17, 2.4, 0.241, 6.0, 18.0, 0.9961, 3.4, 0.59, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.43, 0.3, 3.4, 7.9e-2, 7.0, 34.0, 0.99788, 3.36, 0.61, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.52, 3.0e-2, 2.6, 7.6e-2, 21.0, 92.0, 0.99745, 3.5, 0.6, 9.8]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.0, 0.57, 0.0, 2.0, 0.19, 12.0, 45.0, 0.99676, 3.31, 0.6, 9.4], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.46, 0.14, 2.4, 0.114, 9.0, 37.0, 0.99732, 3.66, 0.65, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.82, 5.0e-2, 2.4, 8.1e-2, 26.0, 96.0, 0.99814, 3.36, 0.53, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.46, 0.14, 2.4, 0.114, 9.0, 37.0, 0.99732, 3.66, 0.65, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.59, 1.0e-2, 2.5, 7.7e-2, 20.0, 85.0, 0.99746, 3.55, 0.59, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.35, 0.41, 2.3, 8.3e-2, 11.0, 61.0, 0.9982, 3.21, 0.5, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.35, 0.41, 2.3, 8.3e-2, 11.0, 61.0, 0.9982, 3.21, 0.5, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.56, 0.24, 2.2, 7.9e-2, 19.0, 58.0, 0.9991, 3.18, 0.56, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.56, 0.24, 2.2, 7.9e-2, 19.0, 58.0, 0.9991, 3.18, 0.56, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.63, 0.17, 2.9, 9.9e-2, 21.0, 119.0, 0.998, 3.09, 0.52, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.37, 0.43, 2.6, 8.2e-2, 18.0, 82.0, 0.99708, 3.33, 0.68, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.64, 0.17, 2.9, 8.4e-2, 25.0, 130.0, 0.99818, 3.23, 0.54, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.61, 2.0e-2, 2.5, 8.1e-2, 17.0, 87.0, 0.99745, 3.48, 0.6, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.6, 0.0, 2.6, 5.5e-2, 7.0, 13.0, 0.99639, 3.38, 0.56, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.27, 0.54, 2.3, 6.5e-2, 7.0, 26.0, 0.99531, 3.17, 0.53, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.89, 0.3, 2.6, 0.132, 7.0, 60.0, 0.99786, 2.99, 1.18, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.46, 0.31, 2.5, 0.126, 24.0, 64.0, 0.99746, 3.1, 0.74, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.37, 0.44, 1.6, 3.8e-2, 21.0, 42.0, 0.99526, 3.24, 0.81, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.5, 0.34, 3.6, 8.2e-2, 5.0, 14.0, 0.9987, 3.29, 0.52, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.5, 0.34, 3.6, 8.2e-2, 5.0, 14.0, 0.9987, 3.29, 0.52, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.61, 8.0e-2, 4.0, 8.2e-2, 26.0, 108.0, 0.99641, 3.25, 0.51, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.55, 9.0e-2, 3.3, 6.8e-2, 8.0, 17.0, 0.99735, 3.23, 0.44, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.1, 0.585, 0.0, 1.7, 4.4e-2, 14.0, 86.0, 0.99264, 3.56, 0.94, 12.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.56, 8.0e-2, 2.5, 0.114, 14.0, 46.0, 0.9971, 3.24, 0.66, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.52, 0.22, 2.7, 8.4e-2, 4.0, 18.0, 0.99682, 3.26, 0.57, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.28, 0.4, 2.4, 5.2e-2, 4.0, 10.0, 0.99356, 3.33, 0.7, 12.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.25, 0.39, 2.0, 4.1e-2, 4.0, 10.0, 0.99386, 3.27, 0.71, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.28, 0.4, 2.4, 5.2e-2, 4.0, 10.0, 0.99356, 3.33, 0.7, 12.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.53, 0.12, 1.9, 0.165, 4.0, 12.0, 0.99702, 3.26, 0.86, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.48, 0.31, 2.8, 7.0e-2, 4.0, 15.0, 0.99693, 3.22, 0.55, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.49, 0.1, 2.6, 6.8e-2, 4.0, 14.0, 0.99562, 3.3, 0.47, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [12.9, 0.5, 0.55, 2.8, 7.2e-2, 7.0, 24.0, 1.00012, 3.09, 0.68, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.45, 0.33, 2.5, 9.9e-2, 20.0, 38.0, 0.99818, 3.24, 0.71, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.39, 0.24, 2.1, 0.102, 4.0, 7.0, 0.99462, 3.44, 0.58, 11.4]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [12.6, 0.41, 0.54, 2.8, 0.103, 19.0, 41.0, 0.99939, 3.21, 0.76, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.45, 0.33, 2.5, 9.9e-2, 20.0, 38.0, 0.99818, 3.24, 0.71, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.51, 0.19, 3.2, 8.1e-2, 8.0, 30.0, 0.9984, 3.23, 0.58, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.29, 0.42, 1.6, 8.4e-2, 19.0, 27.0, 0.99545, 3.28, 0.73, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.715, 0.0, 2.35, 7.1e-2, 21.0, 47.0, 0.99632, 3.29, 0.45, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.66, 0.15, 3.2, 9.7e-2, 9.0, 59.0, 0.99976, 3.28, 0.54, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.685, 0.0, 1.9, 9.9e-2, 9.0, 22.0, 0.99606, 3.34, 0.6, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [4.9, 0.42, 0.0, 2.1, 4.8e-2, 16.0, 42.0, 0.99154, 3.71, 0.74, 14.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.54, 0.13, 2.0, 7.6e-2, 15.0, 36.0, 0.9973, 3.61, 0.64, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.54, 0.13, 2.0, 7.6e-2, 15.0, 36.0, 0.9973, 3.61, 0.64, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.48, 0.28, 2.8, 6.8e-2, 6.0, 16.0, 0.99682, 3.24, 0.53, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.46, 0.14, 2.8, 7.6e-2, 15.0, 37.0, 0.99624, 3.36, 0.49, 10.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.27, 0.34, 2.3, 5.0e-2, 4.0, 8.0, 0.9951, 3.4, 0.64, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.46, 0.14, 2.8, 7.6e-2, 15.0, 37.0, 0.99624, 3.36, 0.49, 10.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.57, 9.0e-2, 2.3, 6.5e-2, 34.0, 45.0, 0.99417, 3.46, 0.74, 12.7]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.61, 8.0e-2, 2.1, 7.1e-2, 16.0, 24.0, 0.99376, 3.56, 0.77, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.685, 7.0e-2, 2.5, 5.8e-2, 5.0, 9.0, 0.99632, 3.38, 0.55, 10.9]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.61, 8.0e-2, 2.1, 7.1e-2, 16.0, 24.0, 0.99376, 3.56, 0.77, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.44, 0.42, 1.5, 0.145, 34.0, 48.0, 0.99832, 3.38, 0.86, 9.9]
            , label = Label "3"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.47, 0.44, 1.6, 0.147, 36.0, 51.0, 0.99836, 3.38, 0.86, 9.9]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.685, 0.26, 1.6, 8.8e-2, 16.0, 23.0, 0.99694, 3.32, 0.47, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.665, 0.1, 1.5, 6.6e-2, 27.0, 55.0, 0.99655, 3.39, 0.51, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.28, 0.28, 2.4, 1.2e-2, 36.0, 100.0, 0.99064, 3.26, 0.39, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.28, 0.28, 2.4, 1.2e-2, 36.0, 100.0, 0.99064, 3.26, 0.39, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.31, 0.35, 1.6, 7.5e-2, 9.0, 28.0, 0.99672, 3.24, 0.83, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.5, 4.0e-2, 2.2, 9.2e-2, 13.0, 26.0, 0.99647, 3.46, 0.47, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.42, 0.47, 2.65, 8.5e-2, 9.0, 34.0, 0.99736, 3.24, 0.77, 12.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.66, 0.0, 3.0, 0.115, 21.0, 31.0, 0.99629, 3.45, 0.63, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.5, 0.45, 2.6, 0.119, 34.0, 68.0, 0.99708, 3.23, 0.72, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.685, 0.35, 2.0, 8.8e-2, 9.0, 92.0, 0.9963, 3.28, 0.62, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.25, 0.46, 1.7, 6.2e-2, 26.0, 42.0, 0.9959, 3.18, 0.83, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.64, 0.21, 1.8, 8.1e-2, 14.0, 31.0, 0.99689, 3.59, 0.66, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.64, 0.21, 1.8, 8.1e-2, 14.0, 31.0, 0.99689, 3.59, 0.66, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.68, 0.16, 1.8, 7.8e-2, 12.0, 39.0, 0.9977, 3.5, 0.7, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.64, 0.21, 1.8, 8.1e-2, 14.0, 31.0, 0.99689, 3.59, 0.66, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.63, 0.21, 1.6, 8.0e-2, 12.0, 32.0, 0.99689, 3.58, 0.66, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.43, 0.44, 1.9, 8.5e-2, 9.0, 22.0, 0.99708, 3.28, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.43, 0.44, 1.9, 8.5e-2, 9.0, 22.0, 0.99708, 3.28, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.42, 0.32, 2.5, 8.0e-2, 26.0, 122.0, 0.99801, 3.22, 1.07, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.36, 0.39, 1.5, 8.0e-2, 41.0, 55.0, 0.99652, 3.47, 0.73, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.36, 0.39, 1.5, 8.0e-2, 41.0, 55.0, 0.99652, 3.47, 0.73, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.735, 2.0e-2, 2.5, 7.1e-2, 10.0, 14.0, 0.99538, 3.51, 0.71, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.36, 0.39, 1.5, 8.0e-2, 41.0, 55.0, 0.99652, 3.47, 0.73, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.26, 0.34, 2.5, 7.3e-2, 16.0, 47.0, 0.99594, 3.4, 0.78, 11.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.7, 0.28, 0.47, 1.7, 5.4e-2, 17.0, 32.0, 0.99686, 3.15, 0.67, 10.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.56, 0.22, 1.8, 7.4e-2, 15.0, 24.0, 0.99438, 3.4, 0.82, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.62, 6.0e-2, 2.7, 7.7e-2, 15.0, 85.0, 0.99746, 3.51, 0.54, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.8, 1.01, 0.66, 2.0, 3.9e-2, 15.0, 88.0, 0.99357, 3.66, 0.6, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.42, 0.32, 2.7, 6.7e-2, 7.0, 25.0, 0.99628, 3.24, 0.44, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.62, 6.0e-2, 2.5, 7.8e-2, 17.0, 84.0, 0.99746, 3.51, 0.53, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.62, 6.0e-2, 2.7, 7.7e-2, 15.0, 85.0, 0.99746, 3.51, 0.54, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.635, 7.0e-2, 2.6, 7.7e-2, 16.0, 86.0, 0.99748, 3.51, 0.54, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.49, 0.22, 2.3, 7.1e-2, 13.0, 24.0, 0.99438, 3.41, 0.83, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.51, 0.23, 2.0, 7.2e-2, 13.0, 22.0, 0.99438, 3.4, 0.84, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.56, 0.22, 1.8, 7.4e-2, 15.0, 24.0, 0.99438, 3.4, 0.82, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.63, 3.0e-2, 2.0, 8.0e-2, 27.0, 43.0, 0.99578, 3.44, 0.64, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.715, 1.0e-2, 2.1, 6.4e-2, 31.0, 43.0, 0.99371, 3.41, 0.57, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.56, 3.0e-2, 1.5, 8.6e-2, 36.0, 46.0, 0.99522, 3.53, 0.57, 10.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.35, 0.24, 2.0, 6.7e-2, 28.0, 48.0, 0.99576, 3.43, 0.54, 10.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.21, 0.37, 1.6, 6.7e-2, 6.0, 10.0, 0.99552, 3.23, 0.58, 11.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.38, 0.46, 2.1, 0.104, 6.0, 10.0, 0.99664, 3.12, 0.65, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.31, 0.4, 2.8, 0.109, 7.0, 16.0, 0.99614, 3.31, 0.79, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.47, 0.0, 2.2, 6.7e-2, 7.0, 14.0, 0.99517, 3.4, 0.58, 10.9]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.715, 1.0e-2, 2.1, 6.4e-2, 31.0, 43.0, 0.99371, 3.41, 0.57, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.61, 0.19, 4.0, 9.4e-2, 30.0, 69.0, 0.99787, 3.22, 0.5, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.6, 4.0e-2, 2.5, 7.6e-2, 18.0, 88.0, 0.99745, 3.53, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.56, 0.18, 1.6, 7.8e-2, 10.0, 21.0, 0.99576, 3.15, 0.49, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.715, 0.0, 2.1, 6.8e-2, 30.0, 35.0, 0.99533, 3.48, 0.65, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.31, 0.29, 3.1, 0.194, 14.0, 26.0, 0.99536, 3.22, 0.78, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.6, 4.0e-2, 2.5, 7.6e-2, 18.0, 88.0, 0.99745, 3.53, 0.55, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.61, 0.19, 4.0, 9.4e-2, 30.0, 69.0, 0.99787, 3.22, 0.5, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.75, 0.14, 2.5, 8.6e-2, 9.0, 30.0, 0.99824, 3.34, 0.64, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.8, 0.12, 2.4, 8.3e-2, 8.0, 28.0, 0.99836, 3.33, 0.65, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.52, 0.38, 2.6, 6.6e-2, 29.0, 56.0, 0.99577, 3.15, 0.79, 12.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.57, 0.0, 2.5, 7.2e-2, 32.0, 64.0, 0.99491, 3.43, 0.56, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.9, 0.34, 6.6, 0.112, 23.0, 99.0, 1.00289, 3.22, 0.68, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.34, 0.24, 2.0, 7.1e-2, 30.0, 52.0, 0.99576, 3.44, 0.58, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.66, 3.0e-2, 2.3, 7.8e-2, 16.0, 86.0, 0.99743, 3.53, 0.57, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.45, 0.23, 1.9, 8.2e-2, 10.0, 18.0, 0.99774, 3.22, 0.65, 9.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.66, 3.0e-2, 2.3, 7.8e-2, 16.0, 86.0, 0.99743, 3.53, 0.57, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.63, 3.0e-2, 2.2, 8.0e-2, 17.0, 88.0, 0.99745, 3.53, 0.58, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.59, 1.0e-2, 2.3, 8.0e-2, 27.0, 43.0, 0.9955, 3.42, 0.58, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.31, 0.39, 2.4, 7.8e-2, 17.0, 43.0, 0.99444, 3.31, 0.77, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.59, 1.0e-2, 2.3, 8.0e-2, 27.0, 43.0, 0.9955, 3.42, 0.58, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.31, 0.39, 2.4, 7.8e-2, 17.0, 43.0, 0.99444, 3.31, 0.77, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 1.02, 2.0e-2, 3.4, 8.4e-2, 6.0, 11.0, 0.99892, 3.48, 0.49, 11.0]
            , label = Label "3"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.31, 0.36, 2.6, 5.6e-2, 10.0, 39.0, 0.99562, 3.4, 0.69, 11.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.635, 0.1, 2.4, 8.0e-2, 16.0, 33.0, 0.99736, 3.58, 0.69, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.635, 0.1, 2.4, 8.0e-2, 16.0, 33.0, 0.99736, 3.58, 0.69, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.59, 6.0e-2, 6.0, 6.0e-2, 11.0, 18.0, 0.9962, 3.41, 0.59, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.59, 6.0e-2, 6.0, 6.0e-2, 11.0, 18.0, 0.9962, 3.41, 0.59, 10.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.58, 0.2, 3.0, 8.1e-2, 15.0, 115.0, 0.998, 3.23, 0.59, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.54, 0.27, 2.6, 8.4e-2, 12.0, 78.0, 0.9964, 3.39, 0.71, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.56, 0.0, 2.2, 7.9e-2, 6.0, 9.0, 0.9948, 3.59, 0.54, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.52, 0.13, 2.4, 7.8e-2, 34.0, 61.0, 0.99528, 3.43, 0.59, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.305, 0.39, 1.2, 5.9e-2, 7.0, 11.0, 0.99331, 3.29, 0.52, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.38, 0.48, 3.8, 0.132, 3.0, 11.0, 0.99577, 3.23, 0.57, 13.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.28, 0.46, 9.0, 0.114, 3.0, 9.0, 0.99901, 3.18, 0.6, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.46, 0.44, 2.9, 6.5e-2, 4.0, 8.0, 0.99674, 3.33, 0.62, 12.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.395, 0.46, 4.6, 9.4e-2, 3.0, 10.0, 0.99639, 3.27, 0.64, 12.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.305, 0.39, 1.2, 5.9e-2, 7.0, 11.0, 0.99331, 3.29, 0.52, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.315, 0.4, 2.2, 7.9e-2, 3.0, 6.0, 0.99512, 3.27, 0.67, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.3, 0.715, 0.19, 1.5, 0.161, 7.0, 62.0, 0.99395, 3.62, 0.61, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.41, 0.31, 8.8, 8.4e-2, 26.0, 45.0, 0.99824, 3.38, 0.64, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.36, 0.32, 2.2, 8.1e-2, 32.0, 79.0, 0.9964, 3.3, 0.72, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.62, 0.12, 1.8, 7.2e-2, 38.0, 46.0, 0.99504, 3.38, 0.89, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.41, 0.37, 2.3, 9.1e-2, 10.0, 23.0, 0.99786, 3.24, 0.56, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.36, 0.32, 2.2, 8.1e-2, 32.0, 79.0, 0.9964, 3.3, 0.72, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.62, 0.12, 1.8, 7.2e-2, 38.0, 46.0, 0.99504, 3.38, 0.89, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.41, 0.31, 8.8, 8.4e-2, 26.0, 45.0, 0.99824, 3.38, 0.64, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.47, 0.27, 2.3, 5.5e-2, 14.0, 28.0, 0.99516, 3.18, 0.8, 11.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.22, 0.36, 1.9, 6.4e-2, 53.0, 77.0, 0.99604, 3.47, 0.87, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.24, 0.33, 2.3, 6.1e-2, 52.0, 73.0, 0.99786, 3.47, 0.9, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.67, 0.19, 2.2, 9.3e-2, 11.0, 75.0, 0.99736, 3.2, 0.59, 9.2]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.47, 0.27, 2.3, 5.5e-2, 14.0, 28.0, 0.99516, 3.18, 0.8, 11.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.33, 0.38, 3.3, 6.3e-2, 10.0, 19.0, 0.99468, 3.3, 0.73, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.61, 1.0e-2, 1.9, 8.0e-2, 8.0, 25.0, 0.99746, 3.69, 0.73, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.61, 1.0e-2, 2.0, 7.4e-2, 13.0, 38.0, 0.99748, 3.48, 0.65, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.4, 0.29, 1.9, 7.8e-2, 29.0, 66.0, 0.9971, 3.45, 0.59, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.61, 1.0e-2, 2.0, 7.4e-2, 13.0, 38.0, 0.99748, 3.48, 0.65, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.61, 1.0e-2, 1.9, 8.0e-2, 8.0, 25.0, 0.99746, 3.69, 0.73, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.3, 0.38, 2.3, 6.0e-2, 19.0, 72.0, 0.99543, 3.39, 0.72, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.3, 0.38, 2.3, 6.0e-2, 19.0, 72.0, 0.99543, 3.39, 0.72, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.0, 0.63, 0.5, 1.4, 7.1e-2, 6.0, 26.0, 0.99791, 3.07, 0.6, 10.4]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.38, 0.38, 2.8, 6.8e-2, 23.0, 42.0, 0.99356, 3.34, 0.72, 12.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.46, 0.17, 1.6, 7.3e-2, 7.0, 11.0, 0.99425, 3.61, 0.54, 11.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.33, 0.52, 2.2, 7.4e-2, 13.0, 25.0, 0.99509, 3.36, 0.76, 12.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.27, 0.49, 5.0, 8.2e-2, 9.0, 17.0, 0.99484, 3.19, 0.52, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.43, 0.4, 2.6, 9.2e-2, 13.0, 52.0, 0.99834, 3.22, 0.64, 10.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.5, 0.34, 2.3, 9.4e-2, 10.0, 45.0, 0.99864, 3.24, 0.6, 9.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.3, 0.49, 3.8, 9.0e-2, 11.0, 24.0, 0.99498, 3.27, 0.64, 12.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.44, 0.42, 2.0, 7.1e-2, 7.0, 20.0, 0.99566, 3.14, 0.79, 11.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.44, 0.58, 4.1, 9.2e-2, 11.0, 24.0, 0.99745, 3.29, 0.99, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.28, 0.48, 2.1, 9.3e-2, 6.0, 12.0, 0.99408, 3.26, 0.62, 12.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.12, 0.45, 1.8, 7.5e-2, 10.0, 21.0, 0.99552, 3.41, 0.76, 11.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.12, 0.45, 1.8, 7.5e-2, 10.0, 21.0, 0.99552, 3.41, 0.76, 11.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.12, 0.45, 1.8, 7.5e-2, 10.0, 21.0, 0.99552, 3.41, 0.76, 11.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.28, 0.48, 2.1, 9.3e-2, 6.0, 12.0, 0.99408, 3.26, 0.62, 12.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.31, 0.4, 2.2, 5.8e-2, 6.0, 10.0, 0.99536, 3.31, 0.68, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.34, 0.48, 2.1, 5.2e-2, 5.0, 9.0, 0.99458, 3.2, 0.69, 12.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.43, 0.4, 2.7, 8.2e-2, 6.0, 11.0, 0.99538, 3.44, 0.54, 12.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.21, 0.52, 1.9, 9.0e-2, 9.0, 23.0, 0.99648, 3.36, 0.67, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.36, 0.52, 2.1, 0.111, 5.0, 10.0, 0.99568, 3.31, 0.62, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.37, 0.52, 2.0, 8.8e-2, 12.0, 51.0, 0.99613, 3.29, 0.58, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.57, 0.12, 2.3, 0.12, 25.0, 36.0, 0.99519, 3.47, 0.71, 11.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.59, 5.0e-2, 2.0, 8.9e-2, 12.0, 32.0, 0.99735, 3.36, 0.61, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.47, 0.27, 1.9, 5.8e-2, 18.0, 38.0, 0.99518, 3.16, 0.85, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.56, 0.14, 1.6, 7.8e-2, 7.0, 18.0, 0.99592, 3.27, 0.62, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.57, 2.0e-2, 2.1, 0.115, 6.0, 16.0, 0.99654, 3.38, 0.69, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.27, 0.39, 2.0, 0.1, 20.0, 27.0, 0.99546, 3.15, 0.69, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.47, 0.27, 1.9, 5.8e-2, 18.0, 38.0, 0.99518, 3.16, 0.85, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.34, 0.4, 2.4, 6.5e-2, 24.0, 48.0, 0.99554, 3.34, 0.86, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.38, 0.41, 2.4, 0.103, 6.0, 10.0, 0.99604, 3.13, 0.58, 11.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.66, 0.2, 2.1, 9.7e-2, 23.0, 113.0, 0.99733, 3.13, 0.48, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.4, 0.43, 2.4, 6.8e-2, 29.0, 46.0, 0.9943, 3.2, 0.6, 12.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.56, 9.0e-2, 2.9, 7.9e-2, 7.0, 22.0, 0.99669, 3.46, 0.61, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.26, 0.48, 1.9, 6.6e-2, 6.0, 10.0, 0.99724, 3.33, 0.87, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.26, 0.48, 1.9, 6.6e-2, 6.0, 10.0, 0.99724, 3.33, 0.87, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.38, 0.5, 2.4, 0.104, 6.0, 13.0, 0.99643, 3.22, 0.65, 11.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.34, 0.44, 1.7, 7.9e-2, 6.0, 12.0, 0.99605, 3.52, 0.63, 10.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.33, 0.41, 5.9, 7.3e-2, 7.0, 13.0, 0.99658, 3.3, 0.62, 12.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.41, 0.3, 2.1, 8.3e-2, 35.0, 72.0, 0.997, 3.44, 0.52, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.41, 0.3, 2.1, 8.3e-2, 35.0, 72.0, 0.997, 3.44, 0.52, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.59, 0.29, 2.6, 0.109, 31.0, 119.0, 0.99801, 3.15, 0.5, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.4, 0.32, 3.6, 6.1e-2, 9.0, 29.0, 0.99416, 3.28, 0.49, 11.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [12.2, 0.45, 0.49, 1.4, 7.5e-2, 3.0, 6.0, 0.9969, 3.13, 0.63, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.5, 0.3, 1.9, 6.5e-2, 8.0, 17.0, 0.99774, 3.32, 0.71, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.86, 0.26, 1.9, 7.9e-2, 13.0, 28.0, 0.99712, 3.25, 0.62, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.52, 0.32, 2.1, 7.0e-2, 51.0, 70.0, 0.99418, 3.34, 0.82, 12.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.5, 0.3, 1.9, 6.5e-2, 8.0, 17.0, 0.99774, 3.32, 0.71, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.2, 0.45, 0.49, 1.4, 7.5e-2, 3.0, 6.0, 0.9969, 3.13, 0.63, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.58, 0.0, 2.0, 6.4e-2, 7.0, 11.0, 0.99562, 3.45, 0.58, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.34, 0.39, 1.4, 6.6e-2, 3.0, 7.0, 0.9947, 3.19, 0.55, 11.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.36, 0.3, 1.6, 8.0e-2, 35.0, 70.0, 0.99693, 3.44, 0.5, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.39, 0.12, 1.7, 9.7e-2, 19.0, 27.0, 0.99596, 3.16, 0.49, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.295, 0.4, 1.5, 7.3e-2, 14.0, 21.0, 0.99556, 3.14, 0.51, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.39, 0.12, 1.7, 9.7e-2, 19.0, 27.0, 0.99596, 3.16, 0.49, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.34, 0.28, 2.0, 8.2e-2, 31.0, 68.0, 0.99694, 3.45, 0.48, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.4, 0.1, 2.0, 7.6e-2, 30.0, 47.0, 0.99554, 3.36, 0.48, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.34, 0.28, 2.0, 8.2e-2, 31.0, 68.0, 0.99694, 3.45, 0.48, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.35, 0.45, 2.5, 9.2e-2, 20.0, 88.0, 0.99918, 3.15, 0.43, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.6, 6.0e-2, 2.0, 7.9e-2, 19.0, 41.0, 0.99697, 3.39, 0.62, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.66, 0.0, 2.2, 8.7e-2, 3.0, 11.0, 0.99378, 3.71, 0.63, 12.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.66, 0.0, 2.2, 8.7e-2, 3.0, 11.0, 0.99378, 3.71, 0.63, 12.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.84, 0.34, 1.4, 5.0e-2, 4.0, 10.0, 0.99554, 3.12, 0.48, 9.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.69, 0.0, 1.65, 5.5e-2, 7.0, 12.0, 0.99162, 3.47, 0.53, 12.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.43, 0.3, 2.2, 6.2e-2, 6.0, 12.0, 0.99495, 3.44, 0.72, 11.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.35, 0.38, 1.5, 5.8e-2, 31.0, 47.0, 0.99676, 3.26, 0.82, 10.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.29, 0.33, 2.05, 6.3e-2, 13.0, 27.0, 0.99516, 3.26, 0.84, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.36, 0.32, 1.8, 6.7e-2, 4.0, 8.0, 0.9928, 3.36, 0.55, 12.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.43, 0.29, 1.6, 8.1e-2, 27.0, 45.0, 0.99603, 3.25, 0.54, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.36, 0.32, 1.8, 6.7e-2, 4.0, 8.0, 0.9928, 3.36, 0.55, 12.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.29, 0.33, 2.05, 6.3e-2, 13.0, 27.0, 0.99516, 3.26, 0.84, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.3, 0.34, 2.0, 6.4e-2, 12.0, 25.0, 0.99516, 3.26, 0.84, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.35, 0.4, 3.6, 0.11, 12.0, 24.0, 0.99549, 3.23, 0.7, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.5, 0.36, 2.8, 0.116, 26.0, 55.0, 0.99722, 3.18, 0.68, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.28, 0.45, 1.7, 6.7e-2, 7.0, 12.0, 0.99354, 3.25, 0.55, 12.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.32, 0.31, 2.0, 8.8e-2, 12.0, 19.0, 0.9957, 3.17, 0.55, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 1.005, 0.15, 2.1, 0.102, 11.0, 32.0, 0.99604, 3.23, 0.48, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.71, 0.0, 1.6, 9.2e-2, 22.0, 31.0, 0.99635, 3.38, 0.58, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.58, 0.16, 2.0, 0.12, 3.0, 7.0, 0.99454, 3.22, 0.58, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.39, 0.46, 2.2, 7.5e-2, 14.0, 27.0, 0.99598, 3.06, 0.84, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.38, 0.4, 2.2, 6.8e-2, 12.0, 28.0, 0.99486, 3.27, 0.75, 12.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.18, 0.37, 0.9, 4.9e-2, 36.0, 109.0, 0.99007, 2.89, 0.44, 12.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.18, 0.37, 0.9, 4.9e-2, 36.0, 109.0, 0.99007, 2.89, 0.44, 12.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.5, 0.14, 1.8, 7.8e-2, 10.0, 23.0, 0.99636, 3.53, 0.61, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.3, 0.36, 0.66, 2.4, 0.123, 3.0, 8.0, 0.99642, 3.2, 0.53, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.3, 0.36, 0.66, 2.4, 0.123, 3.0, 8.0, 0.99642, 3.2, 0.53, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.51, 9.0e-2, 2.1, 6.2e-2, 4.0, 9.0, 0.99584, 3.35, 0.54, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.32, 0.42, 2.3, 9.8e-2, 3.0, 9.0, 0.99506, 3.27, 0.55, 12.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.58, 1.0e-2, 1.8, 8.8e-2, 12.0, 18.0, 0.99568, 3.32, 0.56, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.83, 0.0, 2.8, 9.5e-2, 17.0, 43.0, 0.99822, 3.33, 0.6, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.31, 0.32, 1.9, 6.6e-2, 14.0, 36.0, 0.99364, 3.41, 0.56, 12.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.795, 0.0, 2.2, 6.5e-2, 28.0, 52.0, 0.99378, 3.49, 0.52, 11.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.34, 0.21, 2.5, 7.5e-2, 41.0, 68.0, 0.99586, 3.37, 0.54, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.58, 1.0e-2, 1.8, 8.8e-2, 12.0, 18.0, 0.99568, 3.32, 0.56, 10.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.59, 0.0, 2.1, 9.1e-2, 9.0, 14.0, 0.99488, 3.42, 0.55, 11.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.55, 1.0e-2, 1.8, 9.3e-2, 9.0, 15.0, 0.99514, 3.35, 0.58, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.82, 0.0, 4.1, 9.5e-2, 5.0, 14.0, 0.99854, 3.36, 0.53, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.57, 8.0e-2, 2.6, 8.9e-2, 14.0, 27.0, 0.99592, 3.3, 0.59, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.745, 0.18, 2.5, 7.7e-2, 15.0, 48.0, 0.99739, 3.2, 0.47, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.1, 0.37, 0.34, 2.4, 8.5e-2, 5.0, 17.0, 0.99683, 3.17, 0.65, 10.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.31, 0.34, 2.5, 8.2e-2, 26.0, 35.0, 0.99356, 3.22, 0.59, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.91, 0.1, 1.8, 7.4e-2, 20.0, 56.0, 0.99672, 3.35, 0.56, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.41, 0.41, 6.2, 7.8e-2, 25.0, 42.0, 0.9953, 3.24, 0.77, 12.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.5, 0.21, 2.2, 8.8e-2, 21.0, 39.0, 0.99692, 3.33, 0.83, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.965, 0.0, 2.2, 8.8e-2, 16.0, 32.0, 0.99756, 3.58, 0.67, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.49, 0.19, 1.7, 7.9e-2, 13.0, 26.0, 0.99547, 3.38, 0.64, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.5, 0.21, 2.2, 8.8e-2, 21.0, 39.0, 0.99692, 3.33, 0.83, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.5, 0.39, 0.41, 8.9, 6.9e-2, 18.0, 39.0, 0.99859, 3.29, 0.81, 10.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.39, 0.33, 3.3, 4.6e-2, 12.0, 53.0, 0.99294, 3.36, 0.62, 12.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.44, 0.0, 1.4, 7.0e-2, 32.0, 38.0, 0.99438, 3.32, 0.58, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.78, 0.0, 1.7, 7.6e-2, 33.0, 45.0, 0.99612, 3.31, 0.62, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.43, 0.17, 1.8, 8.2e-2, 27.0, 51.0, 0.99634, 3.49, 0.64, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.49, 0.36, 1.7, 8.1e-2, 3.0, 14.0, 0.99702, 3.27, 0.78, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.5, 0.36, 1.8, 8.4e-2, 6.0, 17.0, 0.99704, 3.27, 0.77, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.43, 0.17, 1.8, 8.2e-2, 27.0, 51.0, 0.99634, 3.49, 0.64, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.46, 0.59, 1.4, 0.414, 16.0, 45.0, 0.99702, 3.03, 1.34, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.605, 5.0e-2, 2.4, 7.3e-2, 19.0, 25.0, 0.99258, 3.56, 0.55, 12.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.33, 0.42, 2.3, 7.0e-2, 9.0, 20.0, 0.99426, 3.38, 0.77, 12.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.64, 0.27, 2.0, 9.5e-2, 5.0, 77.0, 0.99747, 3.13, 0.62, 9.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.64, 0.27, 2.0, 9.5e-2, 5.0, 77.0, 0.99747, 3.13, 0.62, 9.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.48, 0.53, 4.0, 0.101, 3.0, 10.0, 0.99586, 3.21, 0.59, 12.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.42, 0.25, 3.9, 0.104, 28.0, 90.0, 0.99784, 3.15, 0.57, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.53, 0.57, 2.4, 9.3e-2, 30.0, 52.0, 0.9971, 3.19, 0.76, 11.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.48, 0.53, 4.0, 0.101, 3.0, 10.0, 0.99586, 3.21, 0.59, 12.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.23, 0.57, 1.8, 7.4e-2, 3.0, 8.0, 0.9981, 3.14, 0.7, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.4, 0.5, 1.8, 7.1e-2, 7.0, 16.0, 0.99462, 3.21, 0.69, 12.5]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.38, 0.44, 1.9, 9.8e-2, 6.0, 15.0, 0.9956, 3.3, 0.64, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.29, 0.65, 2.4, 7.5e-2, 6.0, 17.0, 0.99565, 3.22, 0.63, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.74, 9.0e-2, 2.0, 6.7e-2, 5.0, 10.0, 0.99418, 3.28, 0.57, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.61, 0.18, 2.4, 8.3e-2, 6.0, 20.0, 0.9963, 3.29, 0.6, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.52, 8.0e-2, 2.4, 7.0e-2, 13.0, 26.0, 0.99358, 3.4, 0.72, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.31, 0.53, 2.2, 6.0e-2, 3.0, 10.0, 0.99572, 3.02, 0.83, 10.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.31, 0.53, 2.2, 6.0e-2, 3.0, 10.0, 0.99572, 3.02, 0.83, 10.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.62, 0.35, 2.8, 8.6e-2, 28.0, 52.0, 0.997, 3.31, 0.62, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.3, 0.33, 0.45, 1.5, 5.7e-2, 19.0, 37.0, 0.99498, 3.18, 0.89, 11.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.77, 0.2, 8.1, 9.8e-2, 30.0, 92.0, 0.99892, 3.2, 0.58, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.35, 0.26, 1.8, 8.3e-2, 33.0, 75.0, 0.9968, 3.4, 0.58, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.62, 0.33, 2.7, 8.8e-2, 16.0, 37.0, 0.9972, 3.31, 0.58, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.77, 0.2, 8.1, 9.8e-2, 30.0, 92.0, 0.99892, 3.2, 0.58, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.25, 0.34, 2.0, 7.1e-2, 45.0, 67.0, 0.99769, 3.44, 0.86, 10.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.32, 0.56, 2.0, 7.3e-2, 3.0, 8.0, 0.99534, 3.15, 0.73, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.37, 0.65, 6.4, 8.0e-2, 3.0, 8.0, 0.99817, 3.27, 0.58, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.37, 0.65, 6.4, 8.0e-2, 3.0, 8.0, 0.99817, 3.27, 0.58, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.3, 0.68, 8.3, 5.0e-2, 37.5, 278.0, 0.99316, 3.01, 0.51, 12.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.3, 0.27, 0.56, 1.4, 4.7e-2, 3.0, 8.0, 0.99471, 3.16, 0.51, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.3, 0.68, 8.3, 5.0e-2, 37.5, 289.0, 0.99316, 3.01, 0.51, 12.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.38, 0.3, 1.8, 7.3e-2, 31.0, 70.0, 0.99685, 3.42, 0.59, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.42, 0.45, 2.4, 7.2e-2, 32.0, 59.0, 0.99617, 3.33, 0.77, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.38, 0.3, 1.8, 7.3e-2, 31.0, 70.0, 0.99685, 3.42, 0.59, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.48, 8.0e-2, 1.8, 7.4e-2, 40.0, 64.0, 0.99529, 3.12, 0.49, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.34, 0.4, 4.7, 5.5e-2, 3.0, 9.0, 0.99738, 3.38, 0.66, 11.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.19, 0.42, 1.6, 5.7e-2, 18.0, 30.0, 0.994, 3.29, 0.69, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.41, 0.54, 1.5, 9.5e-2, 22.0, 41.0, 0.99735, 3.02, 0.76, 9.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.41, 0.54, 1.5, 9.5e-2, 22.0, 41.0, 0.99735, 3.02, 0.76, 9.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.26, 0.54, 1.9, 8.3e-2, 42.0, 74.0, 0.99451, 2.98, 0.63, 11.8]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.34, 0.42, 2.0, 8.6e-2, 8.0, 19.0, 0.99546, 3.35, 0.6, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.54, 9.0e-2, 2.0, 8.1e-2, 10.0, 16.0, 0.99479, 3.43, 0.59, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.31, 0.36, 2.2, 7.9e-2, 11.0, 31.0, 0.99615, 3.33, 0.86, 12.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.725, 9.0e-2, 5.5, 0.117, 9.0, 17.0, 0.99655, 3.35, 0.49, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.4, 0.4, 0.47, 2.5, 8.7e-2, 6.0, 20.0, 0.99772, 3.15, 0.5, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.725, 9.0e-2, 5.5, 0.117, 9.0, 17.0, 0.99655, 3.35, 0.49, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.52, 0.38, 1.5, 9.6e-2, 5.0, 18.0, 0.99666, 3.2, 0.52, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.31, 0.45, 2.1, 0.216, 5.0, 16.0, 0.99358, 3.15, 0.81, 12.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.52, 0.38, 1.5, 9.6e-2, 5.0, 18.0, 0.99666, 3.2, 0.52, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.34, 0.42, 2.1, 7.2e-2, 23.0, 36.0, 0.99392, 3.11, 0.78, 12.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.49, 0.27, 2.1, 7.1e-2, 14.0, 25.0, 0.99388, 3.35, 0.63, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.48, 9.0e-2, 1.7, 7.8e-2, 18.0, 30.0, 0.99402, 3.45, 0.54, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.49, 0.27, 2.1, 7.1e-2, 14.0, 25.0, 0.99388, 3.35, 0.63, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.48, 0.34, 2.2, 7.3e-2, 16.0, 25.0, 0.9936, 3.28, 0.66, 12.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.57, 0.28, 2.1, 4.8e-2, 13.0, 49.0, 0.99374, 3.41, 0.6, 12.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.23, 0.42, 1.9, 6.9e-2, 9.0, 17.0, 0.99376, 3.21, 0.54, 12.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.3, 0.41, 2.0, 6.8e-2, 10.0, 24.0, 0.99523, 3.27, 0.85, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.78, 0.1, 3.3, 9.0e-2, 4.0, 13.0, 0.99855, 3.36, 0.49, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.8, 0.47, 0.43, 2.1, 0.171, 27.0, 66.0, 0.9982, 3.17, 0.76, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.53, 0.0, 1.4, 7.0e-2, 6.0, 14.0, 0.99593, 3.25, 0.64, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.4, 0.42, 0.27, 2.0, 9.2e-2, 23.0, 55.0, 0.99471, 3.78, 0.64, 12.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.33, 0.41, 1.5, 5.6e-2, 6.0, 35.0, 0.99396, 3.29, 0.71, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.9, 0.24, 0.39, 1.6, 7.4e-2, 3.0, 10.0, 0.99698, 3.12, 0.59, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.0, 0.4, 0.5, 4.3, 4.6e-2, 29.0, 80.0, 0.9902, 3.49, 0.66, 13.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.69, 7.0e-2, 2.5, 9.1e-2, 15.0, 21.0, 0.99572, 3.38, 0.6, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.69, 7.0e-2, 2.5, 9.1e-2, 15.0, 21.0, 0.99572, 3.38, 0.6, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.69, 7.0e-2, 2.5, 9.1e-2, 15.0, 21.0, 0.99572, 3.38, 0.6, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.39, 0.12, 2.1, 6.5e-2, 14.0, 24.0, 0.99252, 3.3, 0.53, 13.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.66, 0.0, 2.5, 6.6e-2, 7.0, 15.0, 0.99256, 3.52, 0.58, 12.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.54, 0.34, 2.5, 7.6e-2, 8.0, 17.0, 0.99235, 3.2, 0.72, 13.1]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.5, 0.0, 1.8, 6.2e-2, 21.0, 28.0, 0.99352, 3.44, 0.55, 12.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.47, 0.0, 1.4, 5.5e-2, 27.0, 33.0, 0.9922, 3.45, 0.48, 12.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.7, 0.4, 0.37, 1.9, 8.1e-2, 17.0, 29.0, 0.99674, 3.12, 0.65, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.58, 0.0, 2.2, 9.6e-2, 3.0, 13.0, 0.99557, 3.62, 0.62, 11.5]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.24, 0.35, 1.7, 5.5e-2, 13.0, 27.0, 0.99394, 3.14, 0.59, 11.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [5.8, 0.29, 0.26, 1.7, 6.3e-2, 3.0, 11.0, 0.9915, 3.39, 0.54, 13.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.76, 0.0, 2.9, 7.2e-2, 26.0, 52.0, 0.99379, 3.51, 0.6, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.43, 0.33, 2.7, 9.5e-2, 28.0, 89.0, 0.9984, 3.22, 0.68, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.43, 0.35, 3.3, 9.2e-2, 24.0, 70.0, 0.99798, 3.21, 0.69, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.6, 0.0, 1.9, 5.8e-2, 5.0, 10.0, 0.9977, 3.18, 0.63, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.19, 0.21, 1.7, 4.5e-2, 57.0, 135.0, 0.99341, 3.32, 0.44, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.36, 0.34, 1.8, 7.5e-2, 18.0, 38.0, 0.9933, 3.38, 0.88, 13.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.48, 7.0e-2, 5.5, 8.9e-2, 10.0, 18.0, 0.99684, 3.37, 0.68, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.28, 0.35, 1.7, 6.1e-2, 6.0, 15.0, 0.99524, 3.3, 0.74, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.25, 0.43, 1.7, 6.7e-2, 22.0, 50.0, 0.9946, 3.38, 0.6, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.52, 0.45, 2.0, 8.0e-2, 6.0, 13.0, 0.99774, 3.22, 0.76, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.52, 0.45, 2.0, 8.0e-2, 6.0, 13.0, 0.99774, 3.22, 0.76, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.41, 0.15, 3.7, 0.104, 29.0, 94.0, 0.99786, 3.14, 0.58, 9.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.51, 0.24, 2.0, 7.9e-2, 16.0, 86.0, 0.99764, 3.34, 0.64, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.4, 0.3, 1.7, 8.0e-2, 33.0, 79.0, 0.9969, 3.41, 0.65, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.38, 0.32, 2.5, 8.0e-2, 24.0, 71.0, 0.99624, 3.27, 0.85, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.45, 0.11, 2.4, 4.3e-2, 6.0, 12.0, 0.99354, 3.3, 0.65, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.22, 0.3, 1.8, 6.5e-2, 16.0, 20.0, 0.99672, 3.61, 0.82, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.32, 0.23, 2.3, 6.6e-2, 35.0, 70.0, 0.99588, 3.43, 0.62, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.2, 0.43, 2.5, 7.6e-2, 31.0, 51.0, 0.99672, 3.53, 0.81, 10.4]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [7.8, 0.5, 0.12, 1.8, 0.178, 6.0, 21.0, 0.996, 3.28, 0.87, 9.8], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.41, 0.45, 6.2, 7.1e-2, 6.0, 14.0, 0.99702, 3.21, 0.49, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.39, 0.42, 2.0, 8.6e-2, 9.0, 21.0, 0.99526, 3.39, 0.66, 11.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.35, 0.47, 2.0, 6.1e-2, 6.0, 11.0, 0.99585, 3.23, 0.52, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.33, 0.32, 2.8, 6.7e-2, 4.0, 12.0, 0.99473, 3.3, 0.76, 12.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.58, 0.23, 2.5, 4.4e-2, 16.0, 70.0, 0.99352, 3.46, 0.65, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.6, 0.25, 2.2, 0.118, 9.0, 38.0, 0.99616, 3.15, 0.53, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.42, 0.35, 2.1, 8.3e-2, 17.0, 38.0, 0.99622, 3.23, 0.66, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.58, 0.0, 2.2, 0.1, 50.0, 63.0, 0.99544, 3.59, 0.68, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.6, 0.25, 2.2, 0.118, 9.0, 38.0, 0.99616, 3.15, 0.53, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.18, 0.51, 1.75, 7.1e-2, 45.0, 88.0, 0.99524, 3.33, 0.76, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [5.1, 0.51, 0.18, 2.1, 4.2e-2, 16.0, 101.0, 0.9924, 3.46, 0.87, 12.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.41, 0.43, 2.8, 7.6e-2, 22.0, 54.0, 0.99572, 3.42, 1.16, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.41, 0.43, 2.2, 0.11, 11.0, 37.0, 0.99728, 3.16, 0.67, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.6, 0.36, 0.57, 2.3, 8.7e-2, 6.0, 20.0, 0.99676, 3.14, 0.72, 11.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.45, 0.43, 1.4, 7.6e-2, 12.0, 21.0, 0.99551, 3.21, 0.75, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.32, 0.42, 2.3, 7.5e-2, 12.0, 19.0, 0.99434, 3.14, 0.71, 11.8]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.785, 0.24, 1.7, 7.8e-2, 10.0, 21.0, 0.99692, 3.29, 0.67, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.785, 0.24, 1.7, 7.8e-2, 10.0, 21.0, 0.99692, 3.29, 0.67, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.44, 0.5, 1.9, 0.369, 15.0, 38.0, 0.99634, 3.01, 1.1, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.54, 0.26, 2.0, 0.111, 7.0, 60.0, 0.99709, 2.94, 0.98, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.33, 0.39, 2.5, 7.4e-2, 29.0, 48.0, 0.99528, 3.32, 0.88, 12.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.34, 0.27, 2.8, 6.7e-2, 8.0, 44.0, 0.99384, 3.21, 0.56, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.5, 0.29, 2.3, 8.6e-2, 5.0, 14.0, 0.99502, 3.32, 0.62, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.36, 0.34, 1.6, 6.2e-2, 5.0, 12.0, 0.99667, 3.2, 0.67, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.59, 0.0, 2.2, 7.8e-2, 26.0, 44.0, 0.99522, 3.42, 0.68, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.42, 0.46, 2.1, 7.4e-2, 5.0, 16.0, 0.99649, 3.27, 0.74, 12.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.36, 0.31, 1.7, 7.9e-2, 26.0, 65.0, 0.99716, 3.46, 0.62, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.36, 0.31, 1.7, 7.9e-2, 26.0, 65.0, 0.99716, 3.46, 0.62, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.61, 0.0, 2.2, 9.5e-2, 48.0, 59.0, 0.99541, 3.61, 0.7, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.88, 3.0e-2, 5.6, 7.9e-2, 23.0, 47.0, 0.99572, 3.58, 0.5, 11.2]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.66, 0.0, 2.4, 5.2e-2, 6.0, 11.0, 0.99318, 3.35, 0.66, 12.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.915, 0.0, 2.1, 4.1e-2, 17.0, 78.0, 0.99346, 3.68, 0.73, 11.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.35, 0.33, 2.4, 7.6e-2, 11.0, 47.0, 0.99599, 3.27, 0.81, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.35, 0.33, 2.4, 7.6e-2, 11.0, 47.0, 0.99599, 3.27, 0.81, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.39, 0.43, 1.65, 6.8e-2, 5.0, 11.0, 0.99478, 3.19, 0.46, 11.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.4, 0.4, 2.5, 6.8e-2, 41.0, 54.0, 0.99754, 3.38, 0.86, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.66, 7.0e-2, 1.6, 7.0e-2, 16.0, 61.0, 0.99572, 3.29, 0.6, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.64, 0.23, 2.1, 8.0e-2, 11.0, 119.0, 0.99538, 3.36, 0.7, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.43, 0.3, 2.0, 8.5e-2, 6.0, 39.0, 0.99346, 3.33, 0.46, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.8, 3.0e-2, 7.8, 7.9e-2, 6.0, 12.0, 0.9963, 3.52, 0.5, 12.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.43, 0.3, 2.0, 8.5e-2, 6.0, 39.0, 0.99346, 3.33, 0.46, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.64, 0.23, 2.1, 8.0e-2, 11.0, 119.0, 0.99538, 3.36, 0.7, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.955, 5.0e-2, 1.8, 7.5e-2, 5.0, 19.0, 0.99616, 3.3, 0.44, 9.6]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.4, 0.57, 4.6, 8.0e-2, 6.0, 20.0, 0.99652, 3.28, 0.57, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.885, 0.0, 2.3, 0.166, 6.0, 12.0, 0.99551, 3.56, 0.51, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.25, 0.37, 2.5, 6.3e-2, 11.0, 41.0, 0.99439, 3.52, 0.8, 12.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.885, 0.0, 2.3, 0.166, 6.0, 12.0, 0.99551, 3.56, 0.51, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.745, 0.12, 1.8, 0.114, 15.0, 64.0, 0.99588, 3.22, 0.59, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.43, 0.22, 1.8, 7.8e-2, 21.0, 56.0, 0.99633, 3.52, 0.6, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.58, 0.23, 2.3, 7.6e-2, 23.0, 94.0, 0.99686, 3.21, 0.58, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.57, 0.21, 1.5, 6.9e-2, 4.0, 9.0, 0.99458, 3.16, 0.54, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.26, 0.26, 2.0, 5.2e-2, 19.0, 77.0, 0.9951, 3.15, 0.79, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.58, 0.23, 2.3, 7.6e-2, 23.0, 94.0, 0.99686, 3.21, 0.58, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.57, 0.21, 1.5, 6.9e-2, 4.0, 9.0, 0.99458, 3.16, 0.54, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.34, 0.36, 1.9, 6.5e-2, 5.0, 10.0, 0.99419, 3.27, 0.54, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.42, 0.39, 1.8, 6.8e-2, 6.0, 12.0, 0.99516, 3.35, 0.69, 11.7]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.74, 0.19, 5.8, 0.111, 33.0, 76.0, 0.99878, 3.14, 0.55, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.36, 0.46, 2.1, 7.4e-2, 24.0, 44.0, 0.99534, 3.4, 0.85, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.36, 0.46, 2.1, 7.4e-2, 24.0, 44.0, 0.99534, 3.4, 0.85, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.36, 0.46, 2.1, 7.4e-2, 24.0, 44.0, 0.99534, 3.4, 0.85, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.72, 0.55, 1.7, 0.136, 24.0, 52.0, 0.99752, 3.35, 0.94, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.36, 0.46, 2.1, 7.4e-2, 24.0, 44.0, 0.99534, 3.4, 0.85, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.39, 0.43, 2.0, 7.1e-2, 14.0, 24.0, 0.99428, 3.45, 0.87, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.65, 2.0e-2, 2.1, 7.8e-2, 8.0, 15.0, 0.99498, 3.35, 0.62, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.44, 0.15, 2.1, 7.6e-2, 22.0, 53.0, 0.9957, 3.32, 0.62, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.65, 2.0e-2, 2.1, 7.8e-2, 8.0, 15.0, 0.99498, 3.35, 0.62, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.6, 0.38, 0.42, 1.9, 7.1e-2, 5.0, 13.0, 0.99659, 3.15, 0.75, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.33, 0.46, 1.9, 8.1e-2, 6.0, 9.0, 0.99628, 3.1, 0.48, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.8, 0.27, 0.46, 2.1, 9.5e-2, 20.0, 29.0, 0.99488, 3.26, 0.56, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.57, 0.31, 2.0, 7.9e-2, 10.0, 79.0, 0.99677, 3.29, 0.69, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.34, 0.37, 1.9, 5.7e-2, 43.0, 74.0, 0.99408, 3.23, 0.81, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.4, 0.31, 1.9, 8.2e-2, 8.0, 24.0, 0.996, 3.24, 0.69, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.39, 0.4, 1.3, 4.4e-2, 25.0, 50.0, 0.99478, 3.2, 0.83, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.9, 0.32, 0.52, 1.8, 0.132, 17.0, 44.0, 0.99734, 3.28, 0.77, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.9, 0.32, 0.52, 1.8, 0.132, 17.0, 44.0, 0.99734, 3.28, 0.77, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.53, 0.22, 2.2, 7.8e-2, 33.0, 89.0, 0.99678, 3.26, 0.46, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.5, 0.36, 0.47, 2.2, 7.4e-2, 9.0, 23.0, 0.99638, 3.23, 0.76, 12.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [12.6, 0.39, 0.49, 2.5, 8.0e-2, 8.0, 20.0, 0.9992, 3.07, 0.82, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.46, 0.23, 2.6, 9.1e-2, 18.0, 77.0, 0.99922, 3.15, 0.51, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.58, 3.0e-2, 4.1, 8.0e-2, 27.0, 46.0, 0.99592, 3.02, 0.47, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.58, 0.25, 2.0, 0.104, 8.0, 21.0, 0.99769, 3.27, 0.72, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.1, 0.42, 0.0, 1.8, 4.4e-2, 18.0, 88.0, 0.99157, 3.68, 0.73, 13.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.43, 0.29, 2.1, 7.5e-2, 19.0, 66.0, 0.99718, 3.4, 0.64, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.18, 0.34, 2.7, 6.6e-2, 15.0, 58.0, 0.9947, 3.37, 0.78, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.815, 1.0e-2, 2.6, 7.4e-2, 48.0, 90.0, 0.99621, 3.38, 0.62, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.43, 0.29, 2.1, 7.5e-2, 19.0, 66.0, 0.99718, 3.4, 0.64, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.23, 0.37, 2.2, 5.7e-2, 14.0, 36.0, 0.99614, 3.23, 0.49, 9.3]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.75, 1.0e-2, 2.2, 5.9e-2, 11.0, 18.0, 0.99242, 3.39, 0.4, 12.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.33, 0.32, 12.9, 5.4e-2, 6.0, 113.0, 0.99572, 3.3, 0.56, 11.5]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.55, 0.0, 1.7, 7.0e-2, 7.0, 17.0, 0.99659, 3.26, 0.64, 9.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.75, 1.0e-2, 2.2, 5.9e-2, 11.0, 18.0, 0.99242, 3.39, 0.4, 12.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.73, 0.0, 2.5, 8.1e-2, 12.0, 24.0, 0.99798, 3.38, 0.46, 9.6]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.67, 0.0, 4.3, 5.7e-2, 11.0, 20.0, 0.99488, 3.45, 0.56, 11.8]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.61, 0.2, 1.7, 7.6e-2, 36.0, 60.0, 0.99494, 3.1, 0.4, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.37, 0.39, 2.5, 7.9e-2, 28.0, 65.0, 0.99729, 3.16, 0.59, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.4, 0.41, 2.0, 5.8e-2, 15.0, 40.0, 0.99414, 3.22, 0.6, 12.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.56, 0.22, 2.4, 8.2e-2, 10.0, 86.0, 0.9983, 3.37, 0.62, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.29, 0.25, 13.4, 6.7e-2, 72.0, 160.0, 0.99721, 3.33, 0.54, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.55, 0.19, 1.8, 8.2e-2, 15.0, 34.0, 0.99655, 3.49, 0.68, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.74, 7.0e-2, 1.7, 8.6e-2, 15.0, 48.0, 0.99502, 3.12, 0.48, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.55, 0.19, 1.8, 8.2e-2, 15.0, 34.0, 0.99655, 3.49, 0.68, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.41, 0.33, 2.2, 8.1e-2, 22.0, 36.0, 0.9949, 3.41, 0.75, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.6, 1.0e-2, 2.3, 7.9e-2, 24.0, 37.0, 0.99514, 3.4, 0.61, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.6, 1.0e-2, 2.3, 7.9e-2, 24.0, 37.0, 0.99514, 3.4, 0.61, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.58, 0.14, 2.2, 7.7e-2, 27.0, 60.0, 0.9963, 3.28, 0.59, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.72, 0.0, 1.8, 0.123, 6.0, 14.0, 0.99627, 3.45, 0.58, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.66, 0.0, 1.4, 9.6e-2, 6.0, 13.0, 0.99569, 3.43, 0.58, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.7, 6.0e-2, 1.9, 7.9e-2, 20.0, 35.0, 0.99628, 3.4, 0.69, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.64, 2.0e-2, 2.4, 6.9e-2, 26.0, 46.0, 0.99358, 3.47, 0.45, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.59, 0.22, 1.8, 8.2e-2, 43.0, 60.0, 0.99499, 3.1, 0.42, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.58, 0.28, 4.8, 8.5e-2, 12.0, 69.0, 0.99633, 3.32, 0.7, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.64, 0.0, 2.7, 0.123, 15.0, 33.0, 0.99538, 3.44, 0.63, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.64, 0.0, 2.7, 0.123, 15.0, 33.0, 0.99538, 3.44, 0.63, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.635, 0.68, 1.8, 0.403, 19.0, 56.0, 0.99632, 3.02, 1.15, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 1.02, 0.0, 2.0, 8.3e-2, 17.0, 24.0, 0.99437, 3.59, 0.55, 11.2]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.45, 0.38, 2.5, 8.1e-2, 34.0, 66.0, 0.99726, 3.15, 0.58, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.78, 0.0, 2.2, 8.9e-2, 13.0, 26.0, 0.9978, 3.37, 0.46, 9.6]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.37, 0.32, 1.8, 6.6e-2, 26.0, 51.0, 0.99456, 3.38, 0.72, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.57, 5.0e-2, 2.3, 8.1e-2, 16.0, 36.0, 0.99564, 3.38, 0.6, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.57, 5.0e-2, 2.3, 8.1e-2, 16.0, 36.0, 0.99564, 3.38, 0.6, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.4, 0.43, 0.5, 2.3, 6.8e-2, 13.0, 19.0, 0.996, 3.1, 0.87, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.41, 0.31, 2.0, 7.9e-2, 21.0, 51.0, 0.99668, 3.47, 0.55, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.5, 0.49, 3.0e-2, 1.8, 4.4e-2, 28.0, 87.0, 0.9908, 3.5, 0.82, 14.0]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [5.0, 0.38, 1.0e-2, 1.6, 4.8e-2, 26.0, 60.0, 0.99084, 3.7, 0.75, 14.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.44, 0.2, 1.6, 4.9e-2, 24.0, 64.0, 0.9935, 3.38, 0.57, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.46, 0.0, 1.9, 7.7e-2, 25.0, 44.0, 0.99385, 3.5, 0.53, 11.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.58, 0.2, 2.0, 7.3e-2, 34.0, 44.0, 0.99494, 3.1, 0.43, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.58, 0.13, 2.1, 0.102, 17.0, 36.0, 0.9944, 3.24, 0.53, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.715, 0.22, 2.3, 7.5e-2, 13.0, 81.0, 0.99688, 3.24, 0.54, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.5, 0.4, 0.4, 6.3, 5.0e-2, 3.0, 10.0, 0.99566, 3.28, 0.56, 12.0]
            , label = Label "4"
            }
        , DataEntry
            {dataPoint = V.fromList [7.0, 0.69, 0.0, 1.9, 0.114, 3.0, 10.0, 0.99636, 3.35, 0.6, 9.7], label = Label "6"}
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.715, 0.22, 2.3, 7.5e-2, 13.0, 81.0, 0.99688, 3.24, 0.54, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.8, 0.3, 0.39, 1.7, 6.2e-2, 3.0, 9.0, 0.9948, 3.14, 0.57, 11.5]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.46, 0.2, 1.9, 7.7e-2, 28.0, 54.0, 0.9956, 3.37, 0.64, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.46, 0.2, 1.9, 7.7e-2, 28.0, 54.0, 0.9956, 3.37, 0.64, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.765, 0.0, 2.0, 8.4e-2, 9.0, 22.0, 0.99619, 3.33, 0.68, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.63, 0.28, 2.7, 9.6e-2, 17.0, 69.0, 0.99734, 3.26, 0.63, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.42, 0.19, 2.3, 7.1e-2, 18.0, 36.0, 0.99476, 3.39, 0.56, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.3, 0.37, 0.5, 1.8, 9.0e-2, 20.0, 47.0, 0.99734, 3.15, 0.57, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.16, 0.44, 2.5, 6.8e-2, 17.0, 31.0, 0.99328, 3.35, 0.54, 12.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.6, 8.0e-2, 2.6, 5.6e-2, 3.0, 7.0, 0.99286, 3.22, 0.37, 13.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.6, 0.3, 4.5, 6.8e-2, 20.0, 110.0, 0.99914, 3.3, 1.17, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.6, 0.3, 4.5, 6.8e-2, 20.0, 110.0, 0.99914, 3.3, 1.17, 10.2]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.6, 0.74, 0.0, 1.9, 0.1, 6.0, 12.0, 0.99521, 3.36, 0.59, 11.0], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.635, 0.1, 2.1, 7.3e-2, 25.0, 60.0, 0.99638, 3.29, 0.75, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.395, 0.13, 2.4, 5.6e-2, 14.0, 28.0, 0.99362, 3.62, 0.67, 12.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.755, 0.0, 1.9, 8.4e-2, 6.0, 12.0, 0.99672, 3.34, 0.49, 9.7]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.635, 0.1, 2.1, 7.3e-2, 25.0, 60.0, 0.99638, 3.29, 0.75, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.63, 0.0, 4.3, 9.3e-2, 51.0, 77.5, 0.99558, 3.2, 0.45, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.63, 0.0, 4.3, 9.3e-2, 51.0, 77.5, 0.99558, 3.2, 0.45, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.53, 0.14, 2.1, 6.4e-2, 15.0, 29.0, 0.99323, 3.35, 0.61, 12.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.7, 0.6, 0.0, 1.4, 6.3e-2, 11.0, 18.0, 0.99191, 3.45, 0.56, 12.2]
            , label = Label "6"
            }
        , DataEntry
            {dataPoint = V.fromList [7.6, 1.58, 0.0, 2.1, 0.137, 5.0, 9.0, 0.99476, 3.5, 0.4, 10.9], label = Label "3"}
        , DataEntry
            { dataPoint = V.fromList [5.2, 0.645, 0.0, 2.15, 8.0e-2, 15.0, 28.0, 0.99444, 3.78, 0.61, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.86, 7.0e-2, 2.0, 0.1, 20.0, 57.0, 0.99598, 3.6, 0.74, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.37, 0.32, 2.1, 6.4e-2, 4.0, 15.0, 0.99576, 3.3, 0.8, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.28, 0.44, 1.8, 8.1e-2, 28.0, 68.0, 0.99501, 3.36, 0.66, 11.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.79, 0.21, 2.3, 8.7e-2, 21.0, 68.0, 0.9955, 3.12, 0.44, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.61, 0.26, 1.9, 7.3e-2, 24.0, 88.0, 0.99612, 3.3, 0.53, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.69, 0.32, 2.5, 8.8e-2, 22.0, 91.0, 0.9979, 3.29, 0.62, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.68, 9.0e-2, 3.9, 6.8e-2, 15.0, 29.0, 0.99524, 3.41, 0.52, 11.1]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.69, 0.32, 2.5, 8.8e-2, 22.0, 91.0, 0.9979, 3.29, 0.62, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.62, 0.1, 1.4, 7.1e-2, 27.0, 63.0, 0.996, 3.28, 0.61, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.61, 0.26, 1.9, 7.3e-2, 24.0, 88.0, 0.99612, 3.3, 0.53, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.51, 0.15, 3.0, 6.4e-2, 12.0, 27.0, 0.9929, 3.33, 0.59, 12.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 1.18, 0.21, 1.9, 8.3e-2, 14.0, 41.0, 0.99532, 3.34, 0.47, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.36, 0.21, 2.3, 8.6e-2, 20.0, 65.0, 0.99558, 3.4, 0.54, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.36, 0.21, 2.4, 8.6e-2, 24.0, 69.0, 0.99556, 3.4, 0.53, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.63, 0.27, 2.0, 8.3e-2, 17.0, 91.0, 0.99616, 3.26, 0.58, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.4, 0.74, 0.0, 1.2, 4.1e-2, 16.0, 46.0, 0.99258, 4.01, 0.59, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.44, 0.46, 2.2, 9.1e-2, 10.0, 41.0, 0.99638, 3.18, 0.69, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.63, 0.27, 2.0, 8.3e-2, 17.0, 91.0, 0.99616, 3.26, 0.58, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.76, 0.68, 1.7, 0.414, 18.0, 64.0, 0.99652, 2.9, 1.33, 9.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.7, 0.66, 0.34, 2.6, 9.4e-2, 12.0, 88.0, 0.99796, 3.26, 0.66, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.0, 0.74, 0.0, 1.2, 4.1e-2, 16.0, 46.0, 0.99258, 4.01, 0.59, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.34, 0.42, 1.8, 5.8e-2, 9.0, 18.0, 0.99392, 3.18, 0.55, 11.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.36, 0.39, 1.8, 6.0e-2, 21.0, 55.0, 0.99495, 3.18, 0.82, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.46, 0.24, 1.7, 7.7e-2, 18.0, 34.0, 0.9948, 3.39, 0.6, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.46, 0.24, 1.7, 7.7e-2, 18.0, 34.0, 0.9948, 3.39, 0.6, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.46, 0.24, 1.7, 7.7e-2, 18.0, 34.0, 0.9948, 3.39, 0.6, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.46, 0.24, 1.7, 7.7e-2, 18.0, 34.0, 0.9948, 3.39, 0.6, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.52, 0.11, 1.8, 7.3e-2, 13.0, 38.0, 0.9955, 3.34, 0.52, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.6, 0.26, 2.1, 8.3e-2, 17.0, 91.0, 0.99616, 3.29, 0.56, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.6, 0.26, 2.1, 8.3e-2, 17.0, 91.0, 0.99616, 3.29, 0.56, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.87, 0.26, 3.8, 0.107, 31.0, 67.0, 0.99668, 3.26, 0.46, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.39, 0.1, 1.7, 7.5e-2, 6.0, 25.0, 0.99581, 3.09, 0.43, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.1, 0.775, 0.22, 2.2, 7.9e-2, 12.0, 48.0, 0.9976, 3.18, 0.51, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.835, 0.0, 2.0, 0.166, 4.0, 11.0, 0.99608, 3.39, 0.52, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.58, 2.0e-2, 2.4, 6.9e-2, 19.0, 40.0, 0.99387, 3.38, 0.66, 12.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.5, 0.0, 1.4, 5.7e-2, 15.0, 26.0, 0.99448, 3.36, 0.45, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.5, 0.0, 1.4, 5.7e-2, 15.0, 26.0, 0.99448, 3.36, 0.45, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.5, 0.0, 1.4, 5.7e-2, 15.0, 26.0, 0.99448, 3.36, 0.45, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.51, 2.0e-2, 1.7, 8.4e-2, 13.0, 31.0, 0.99538, 3.36, 0.54, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.51, 2.0e-2, 1.7, 8.4e-2, 13.0, 31.0, 0.99538, 3.36, 0.54, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.51, 2.0e-2, 1.7, 8.4e-2, 13.0, 31.0, 0.99538, 3.36, 0.54, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.54, 2.0e-2, 1.7, 8.5e-2, 17.0, 31.0, 0.99589, 3.37, 0.51, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.51, 2.0e-2, 1.7, 8.4e-2, 13.0, 31.0, 0.99538, 3.36, 0.54, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.5, 0.42, 0.48, 2.6, 7.7e-2, 8.0, 20.0, 0.99852, 3.09, 0.53, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.44, 0.24, 2.3, 6.3e-2, 10.0, 28.0, 0.99613, 3.25, 0.53, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.59, 1.0e-2, 2.1, 5.6e-2, 5.0, 13.0, 0.99472, 3.52, 0.56, 11.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.655, 3.0e-2, 1.8, 7.8e-2, 7.0, 12.0, 0.99587, 3.34, 0.39, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.655, 3.0e-2, 1.8, 7.8e-2, 7.0, 12.0, 0.99587, 3.34, 0.39, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.57, 0.0, 2.8, 8.1e-2, 21.0, 41.0, 0.99518, 3.41, 0.52, 10.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.0, 0.6, 0.29, 2.0, 6.9e-2, 32.0, 73.0, 0.99654, 3.34, 0.57, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.62, 1.0e-2, 2.3, 6.5e-2, 8.0, 46.0, 0.99332, 3.32, 0.51, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.645, 3.0e-2, 1.9, 8.6e-2, 14.0, 57.0, 0.9969, 3.37, 0.46, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.645, 3.0e-2, 1.9, 8.6e-2, 14.0, 57.0, 0.9969, 3.37, 0.46, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.58, 3.0e-2, 2.3, 7.7e-2, 7.0, 28.0, 0.99568, 3.35, 0.52, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.32, 0.25, 1.8, 8.6e-2, 5.0, 32.0, 0.99464, 3.36, 0.44, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.34, 0.25, 1.8, 8.4e-2, 4.0, 28.0, 0.99464, 3.36, 0.44, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.43, 0.24, 2.5, 7.8e-2, 27.0, 67.0, 0.99648, 3.6, 0.59, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.64, 0.17, 5.4, 0.168, 52.0, 98.0, 0.99736, 3.28, 0.5, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.475, 0.4, 1.4, 9.1e-2, 6.0, 28.0, 0.99704, 3.07, 0.65, 10.0333333333333]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.2, 0.54, 0.31, 2.3, 0.112, 11.0, 38.0, 0.99699, 3.24, 0.56, 10.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.85, 0.14, 2.5, 9.3e-2, 13.0, 54.0, 0.99724, 3.36, 0.54, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.6, 0.475, 0.4, 1.4, 9.1e-2, 6.0, 28.0, 0.99704, 3.07, 0.65, 10.0333333333333]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.83, 0.27, 2.0, 8.0e-2, 11.0, 63.0, 0.99652, 3.29, 0.48, 9.8]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.605, 2.0e-2, 1.9, 9.6e-2, 10.0, 31.0, 0.995, 3.46, 0.53, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.5, 9.0e-2, 2.2, 0.115, 10.0, 42.0, 0.9971, 3.18, 0.62, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.74, 8.0e-2, 1.7, 9.4e-2, 10.0, 45.0, 0.99576, 3.24, 0.5, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.54, 0.3, 2.2, 8.8e-2, 9.0, 105.0, 0.99725, 3.25, 1.18, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.77, 0.32, 2.1, 7.9e-2, 16.0, 74.0, 0.99656, 3.27, 0.5, 9.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.61, 0.0, 1.6, 6.9e-2, 4.0, 8.0, 0.99396, 3.33, 0.37, 10.4]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.78, 0.51, 1.7, 0.415, 12.0, 66.0, 0.99623, 3.0, 1.17, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.58, 0.56, 3.1, 0.153, 5.0, 14.0, 0.99476, 3.21, 1.03, 11.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.78, 0.51, 1.7, 0.415, 12.0, 66.0, 0.99623, 3.0, 1.17, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.75, 0.27, 3.8, 0.11, 34.0, 89.0, 0.99664, 3.24, 0.45, 9.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.815, 0.0, 1.2, 0.267, 16.0, 29.0, 0.99471, 3.32, 0.51, 9.8]
            , label = Label "3"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.56, 0.26, 2.0, 8.3e-2, 13.0, 100.0, 0.99586, 3.26, 0.52, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.885, 0.2, 1.4, 8.6e-2, 7.0, 31.0, 0.9946, 3.11, 0.46, 10.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.2, 0.49, 0.26, 2.3, 9.0e-2, 23.0, 74.0, 0.9953, 3.71, 0.62, 12.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.45, 0.15, 2.0, 7.8e-2, 10.0, 28.0, 0.99609, 3.29, 0.51, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.57, 2.0e-2, 2.6, 7.7e-2, 11.0, 35.0, 0.99557, 3.36, 0.62, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.57, 2.0e-2, 2.6, 7.7e-2, 11.0, 35.0, 0.99557, 3.36, 0.62, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.83, 9.0e-2, 1.8, 7.4e-2, 4.0, 25.0, 0.99534, 3.38, 0.45, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.6, 0.22, 2.1, 8.0e-2, 25.0, 105.0, 0.99613, 3.3, 0.49, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.6, 0.22, 2.1, 8.0e-2, 25.0, 105.0, 0.99613, 3.3, 0.49, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.755, 0.15, 1.8, 0.107, 20.0, 84.0, 0.99593, 3.19, 0.5, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.81, 0.25, 3.4, 7.6e-2, 34.0, 85.0, 0.99668, 3.19, 0.42, 9.2]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.4, 0.64, 7.0e-2, 1.8, 0.1, 8.0, 23.0, 0.9961, 3.3, 0.58, 9.6], label = Label "5"}
        , DataEntry
            {dataPoint = V.fromList [7.4, 0.64, 7.0e-2, 1.8, 0.1, 8.0, 23.0, 0.9961, 3.3, 0.58, 9.6], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.64, 0.31, 6.1, 8.3e-2, 7.0, 49.0, 0.99718, 3.35, 0.68, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.48, 2.0e-2, 2.2, 8.0e-2, 36.0, 111.0, 0.99524, 3.1, 0.53, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.49, 0.0, 2.3, 6.8e-2, 15.0, 33.0, 0.99292, 3.58, 0.59, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.64, 0.22, 2.4, 9.4e-2, 5.0, 33.0, 0.99612, 3.37, 0.58, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.62, 6.0e-2, 1.3, 7.0e-2, 5.0, 12.0, 0.9942, 3.17, 0.48, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.52, 0.25, 2.0, 7.8e-2, 19.0, 59.0, 0.99612, 3.3, 0.48, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.57, 0.14, 3.9, 7.0e-2, 27.0, 73.0, 0.99669, 3.32, 0.48, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.6, 0.685, 0.1, 1.6, 9.2e-2, 3.0, 12.0, 0.99745, 3.31, 0.65, 9.55]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.7, 0.675, 0.1, 1.6, 9.0e-2, 4.0, 11.0, 0.99745, 3.31, 0.65, 9.55]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.59, 0.26, 2.0, 8.0e-2, 17.0, 104.0, 0.99584, 3.28, 0.52, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.6, 0.12, 2.2, 8.3e-2, 13.0, 28.0, 0.9966, 3.52, 0.62, 10.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.67, 0.0, 2.2, 6.8e-2, 10.0, 24.0, 0.9956, 3.42, 0.72, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.69, 0.21, 2.1, 8.0e-2, 33.0, 141.0, 0.9962, 3.25, 0.51, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.69, 0.21, 2.1, 8.0e-2, 33.0, 141.0, 0.9962, 3.25, 0.51, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.3, 0.42, 2.0, 5.2e-2, 6.0, 24.0, 0.9963, 3.44, 0.82, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.33, 0.33, 1.7, 6.1e-2, 3.0, 13.0, 0.996, 3.23, 1.1, 10.0]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.5, 0.39, 2.6, 8.2e-2, 12.0, 46.0, 0.9985, 3.43, 0.62, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.28, 0.3, 2.0, 6.2e-2, 18.0, 34.0, 0.9952, 3.28, 0.9, 11.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.24, 0.34, 5.1, 6.2e-2, 8.0, 22.0, 0.9974, 3.22, 0.94, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.51, 0.0, 2.1, 6.4e-2, 40.0, 54.0, 0.995, 3.54, 0.93, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.29, 0.36, 2.2, 4.8e-2, 35.0, 53.0, 0.995, 3.27, 1.01, 12.4]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.51, 0.0, 2.1, 6.4e-2, 40.0, 54.0, 0.995, 3.54, 0.93, 10.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.96, 0.0, 1.8, 8.2e-2, 5.0, 16.0, 0.9936, 3.5, 0.44, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.47, 0.4, 2.4, 7.1e-2, 8.0, 19.0, 0.9963, 3.56, 0.73, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.24, 0.34, 5.1, 6.2e-2, 8.0, 22.0, 0.9974, 3.22, 0.94, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.57, 0.25, 2.0, 0.104, 12.0, 89.0, 0.9963, 3.04, 0.9, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.32, 0.59, 2.2, 7.7e-2, 3.0, 15.0, 0.9994, 3.2, 0.78, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.58, 0.0, 1.6, 6.5e-2, 8.0, 18.0, 0.9966, 3.56, 0.84, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.32, 0.59, 2.2, 7.7e-2, 3.0, 15.0, 0.9994, 3.2, 0.78, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.34, 0.33, 2.5, 6.4e-2, 21.0, 37.0, 0.9952, 3.35, 0.77, 12.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.53, 1.0e-2, 1.6, 7.7e-2, 3.0, 19.0, 0.995, 3.16, 0.46, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.64, 0.21, 2.2, 7.7e-2, 32.0, 133.0, 0.9956, 3.27, 0.45, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.53, 1.0e-2, 1.6, 7.7e-2, 3.0, 19.0, 0.995, 3.16, 0.46, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.4, 0.18, 1.6, 7.9e-2, 24.0, 58.0, 0.9965, 3.34, 0.58, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.54, 0.0, 2.1, 7.9e-2, 39.0, 55.0, 0.9956, 3.39, 0.84, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.53, 9.0e-2, 3.9, 0.123, 14.0, 31.0, 0.9968, 3.5, 0.67, 11.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.26, 0.37, 1.4, 7.6e-2, 8.0, 23.0, 0.9974, 3.26, 0.7, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.3, 0.26, 0.37, 1.4, 7.6e-2, 8.0, 23.0, 0.9974, 3.26, 0.7, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.23, 0.37, 1.8, 4.6e-2, 23.0, 60.0, 0.9971, 3.41, 0.71, 12.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.41, 0.33, 2.5, 7.8e-2, 6.0, 23.0, 0.9957, 3.3, 0.58, 11.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.64, 0.0, 1.9, 7.2e-2, 27.0, 55.0, 0.9962, 3.31, 0.63, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.18, 0.4, 2.2, 4.9e-2, 38.0, 67.0, 0.996, 3.33, 0.93, 11.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.41, 0.24, 1.8, 6.6e-2, 18.0, 47.0, 0.9956, 3.37, 0.62, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.43, 0.31, 2.1, 6.9e-2, 13.0, 74.0, 0.9958, 3.26, 0.54, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.44, 0.0, 1.6, 4.2e-2, 3.0, 11.0, 0.9944, 3.48, 0.85, 11.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.4, 0.16, 1.8, 6.9e-2, 11.0, 25.0, 0.9955, 3.42, 0.74, 10.1]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.54, 0.37, 15.4, 0.214, 55.0, 95.0, 1.00369, 3.18, 0.77, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.2, 0.54, 0.37, 15.4, 0.214, 55.0, 95.0, 1.00369, 3.18, 0.77, 9.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.38, 0.38, 1.6, 0.169, 27.0, 90.0, 0.99914, 3.15, 0.65, 8.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.915, 0.29, 4.8, 7.0e-2, 15.0, 39.0, 0.99577, 3.53, 0.54, 11.1]
            , label = Label "5"
            }
        , DataEntry
            {dataPoint = V.fromList [7.0, 0.59, 0.0, 1.7, 5.2e-2, 3.0, 8.0, 0.996, 3.41, 0.47, 10.3], label = Label "5"}
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.67, 2.0e-2, 2.2, 7.2e-2, 31.0, 92.0, 0.99566, 3.32, 0.68, 11.0666666666667]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.37, 0.32, 2.0, 6.2e-2, 15.0, 28.0, 0.9947, 3.23, 0.73, 11.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.785, 0.19, 5.2, 9.4e-2, 19.0, 98.0, 0.99713, 3.16, 0.52, 9.56666666666667]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.63, 2.0e-2, 1.9, 7.8e-2, 18.0, 30.0, 0.99712, 3.4, 0.75, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.58, 0.2, 1.75, 5.8e-2, 8.0, 22.0, 0.99322, 3.38, 0.49, 11.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.67, 2.0e-2, 2.2, 7.2e-2, 31.0, 92.0, 0.99566, 3.32, 0.68, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.785, 0.19, 5.2, 9.4e-2, 19.0, 98.0, 0.99713, 3.16, 0.52, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.63, 2.0e-2, 1.9, 7.8e-2, 18.0, 30.0, 0.99712, 3.4, 0.75, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.67, 0.0, 1.9, 8.0e-2, 22.0, 39.0, 0.99701, 3.4, 0.74, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.58, 1.0e-2, 1.9, 8.0e-2, 40.0, 54.0, 0.99683, 3.4, 0.73, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.38, 0.31, 2.0, 5.6e-2, 15.0, 29.0, 0.99472, 3.23, 0.76, 11.3]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.37, 0.32, 2.0, 6.2e-2, 15.0, 28.0, 0.9947, 3.23, 0.73, 11.3]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.32, 0.44, 2.7, 0.104, 8.0, 17.0, 0.99732, 3.33, 0.78, 11.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.58, 2.0e-2, 2.0, 6.2e-2, 37.0, 53.0, 0.99374, 3.35, 0.76, 11.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.49, 0.33, 1.9, 7.4e-2, 27.0, 85.0, 0.99706, 3.41, 0.58, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.7, 0.45, 0.63, 2.2, 7.3e-2, 7.0, 23.0, 0.99974, 3.21, 0.69, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.9, 0.0, 1.6, 5.2e-2, 9.0, 17.0, 0.99467, 3.5, 0.63, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.54, 6.0e-2, 1.8, 5.0e-2, 38.0, 89.0, 0.99236, 3.3, 0.5, 10.55]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.49, 0.33, 1.9, 7.4e-2, 27.0, 85.0, 0.99706, 3.41, 0.58, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.29, 0.4, 1.7, 6.7e-2, 8.0, 20.0, 0.99603, 3.39, 0.6, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.2, 0.35, 1.7, 5.4e-2, 7.0, 15.0, 0.99458, 3.32, 0.8, 11.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.42, 9.0e-2, 2.3, 5.4e-2, 34.0, 64.0, 0.99724, 3.41, 0.68, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.785, 0.0, 2.1, 6.0e-2, 6.0, 13.0, 0.99664, 3.59, 0.61, 10.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.64, 3.0e-2, 2.3, 7.5e-2, 14.0, 31.0, 0.99545, 3.36, 0.58, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.63, 1.0e-2, 2.4, 7.6e-2, 14.0, 39.0, 0.99522, 3.34, 0.53, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.59, 0.1, 1.7, 6.3e-2, 34.0, 53.0, 0.9958, 3.41, 0.67, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.59, 0.1, 1.7, 6.3e-2, 34.0, 53.0, 0.9958, 3.41, 0.67, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.48, 0.32, 2.1, 6.2e-2, 31.0, 54.0, 0.99728, 3.3, 0.65, 10.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 1.04, 8.0e-2, 2.3, 6.7e-2, 19.0, 32.0, 0.99648, 3.52, 0.57, 11.0]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.48, 0.32, 2.1, 6.2e-2, 31.0, 54.0, 0.99728, 3.3, 0.65, 10.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.98, 5.0e-2, 2.1, 6.1e-2, 20.0, 49.0, 0.99705, 3.31, 0.55, 9.7]
            , label = Label "3"
            }
        , DataEntry
            { dataPoint = V.fromList [10.0, 0.69, 0.11, 1.4, 8.4e-2, 8.0, 24.0, 0.99578, 2.88, 0.47, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.7, 8.0e-2, 3.75, 6.7e-2, 8.0, 16.0, 0.99334, 3.43, 0.52, 12.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.6, 0.35, 0.6, 2.6, 7.3e-2, 23.0, 44.0, 0.99656, 3.38, 0.79, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.6, 8.0e-2, 1.8, 7.1e-2, 14.0, 45.0, 0.99336, 3.38, 0.54, 11.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.5, 0.5, 13.8, 0.205, 48.0, 82.0, 1.00242, 3.16, 0.75, 8.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.3, 0.47, 0.11, 2.2, 4.8e-2, 16.0, 89.0, 0.99182, 3.54, 0.88, 13.5666666666667]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [9.9, 0.5, 0.5, 13.8, 0.205, 48.0, 82.0, 1.00242, 3.16, 0.75, 8.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.3, 0.47, 0.11, 2.2, 4.8e-2, 16.0, 89.0, 0.99182, 3.54, 0.88, 13.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.875, 5.0e-2, 5.7, 8.2e-2, 3.0, 14.0, 0.99808, 3.4, 0.52, 10.2]
            , label = Label "3"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.28, 0.6, 3.0, 0.104, 10.0, 22.0, 0.99828, 3.39, 0.68, 10.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.62, 3.0e-2, 1.5, 8.0e-2, 6.0, 13.0, 0.99498, 3.66, 0.62, 10.1]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.2, 0.28, 0.6, 3.0, 0.104, 10.0, 22.0, 0.99828, 3.39, 0.68, 10.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.58, 0.54, 2.1, 0.114, 3.0, 9.0, 0.99719, 3.33, 0.57, 10.3]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [8.1, 0.33, 0.44, 1.5, 4.2e-2, 6.0, 12.0, 0.99542, 3.35, 0.61, 10.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.91, 6.0e-2, 2.0, 6.0e-2, 4.0, 11.0, 0.99592, 3.53, 0.64, 10.9]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.655, 0.16, 2.1, 7.4e-2, 8.0, 25.0, 0.99606, 3.37, 0.55, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.68, 0.21, 2.1, 7.0e-2, 9.0, 23.0, 0.99546, 3.38, 0.6, 10.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.64, 5.0e-2, 1.9, 6.6e-2, 9.0, 17.0, 0.99496, 3.52, 0.78, 10.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.54, 4.0e-2, 1.7, 4.9e-2, 5.0, 13.0, 0.9942, 3.72, 0.58, 11.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.57, 0.1, 2.1, 4.8e-2, 4.0, 11.0, 0.99448, 3.44, 0.76, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.22, 0.49, 1.8, 3.9e-2, 8.0, 18.0, 0.99344, 3.39, 0.56, 12.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.54, 4.0e-2, 1.7, 4.9e-2, 5.0, 13.0, 0.9942, 3.72, 0.58, 11.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.65, 6.0e-2, 1.6, 5.0e-2, 6.0, 18.0, 0.99348, 3.57, 0.54, 11.95]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.54, 0.26, 1.9, 8.9e-2, 23.0, 147.0, 0.99636, 3.26, 0.59, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.31, 9.0e-2, 1.4, 6.6e-2, 15.0, 28.0, 0.99459, 3.42, 0.7, 10.0]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.43, 2.0e-2, 1.9, 8.0e-2, 15.0, 28.0, 0.99492, 3.35, 0.81, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.7, 0.54, 0.26, 1.9, 8.9e-2, 23.0, 147.0, 0.99636, 3.26, 0.59, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.74, 3.0e-2, 2.3, 5.4e-2, 7.0, 16.0, 0.99508, 3.45, 0.63, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.895, 4.0e-2, 2.3, 6.8e-2, 7.0, 13.0, 0.99582, 3.53, 0.58, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.74, 3.0e-2, 2.3, 5.4e-2, 7.0, 16.0, 0.99508, 3.45, 0.63, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.725, 4.0e-2, 1.5, 7.6e-2, 8.0, 15.0, 0.99508, 3.26, 0.53, 9.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.82, 0.29, 4.3, 8.3e-2, 21.0, 64.0, 0.99642, 3.16, 0.53, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.585, 0.18, 2.4, 7.8e-2, 15.0, 60.0, 0.99638, 3.31, 0.54, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.44, 0.39, 2.5, 7.7e-2, 6.0, 14.0, 0.99555, 3.51, 0.69, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.38, 0.57, 2.3, 0.106, 5.0, 12.0, 0.99605, 3.36, 0.55, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.76, 2.0e-2, 1.8, 7.8e-2, 6.0, 12.0, 0.996, 3.55, 0.63, 9.95]
            , label = Label "3"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.81, 5.0e-2, 2.0, 7.0e-2, 6.0, 14.0, 0.99562, 3.51, 0.66, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.38, 0.57, 2.3, 0.106, 5.0, 12.0, 0.99605, 3.36, 0.55, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.27, 0.6, 2.1, 7.4e-2, 17.0, 25.0, 0.99814, 3.38, 0.72, 10.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.18, 0.4, 1.8, 6.2e-2, 7.0, 20.0, 0.9941, 3.28, 0.7, 11.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.36, 0.21, 2.2, 4.7e-2, 26.0, 48.0, 0.99661, 3.47, 0.77, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.69, 4.0e-2, 2.1, 6.8e-2, 19.0, 27.0, 0.99712, 3.44, 0.67, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.79, 4.0e-2, 2.2, 6.1e-2, 11.0, 17.0, 0.99588, 3.53, 0.65, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.56, 0.15, 1.8, 7.8e-2, 17.0, 65.0, 0.99294, 3.33, 0.6, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.84, 0.21, 4.1, 7.4e-2, 16.0, 65.0, 0.99842, 3.53, 0.72, 9.23333333333333]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.84, 0.21, 4.1, 7.4e-2, 16.0, 65.0, 0.99842, 3.53, 0.72, 9.25]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.32, 0.25, 2.3, 7.1e-2, 23.0, 58.0, 0.99633, 3.42, 0.97, 10.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.53, 6.0e-2, 2.0, 6.3e-2, 29.0, 44.0, 0.99489, 3.38, 0.83, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.47, 0.46, 2.2, 0.114, 7.0, 20.0, 0.99647, 3.32, 0.63, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.7, 8.0e-2, 2.6, 0.106, 14.0, 27.0, 0.99665, 3.44, 0.58, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.53, 6.0e-2, 2.0, 6.3e-2, 29.0, 44.0, 0.99489, 3.38, 0.83, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.48, 0.2, 1.9, 8.2e-2, 9.0, 23.0, 0.99585, 3.39, 0.43, 9.05]
            , label = Label "4"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.32, 0.25, 2.3, 7.1e-2, 23.0, 58.0, 0.99633, 3.42, 0.97, 10.6]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.48, 0.25, 2.0, 7.6e-2, 29.0, 61.0, 0.9953, 3.34, 0.6, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.42, 0.19, 2.0, 7.5e-2, 22.0, 47.0, 0.99522, 3.39, 0.78, 10.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.48, 8.0e-2, 2.1, 6.4e-2, 18.0, 34.0, 0.99552, 3.33, 0.64, 9.7]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.47, 8.0e-2, 2.2, 6.4e-2, 18.0, 38.0, 0.99553, 3.3, 0.65, 9.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.53, 7.0e-2, 1.7, 7.1e-2, 15.0, 24.0, 0.9951, 3.29, 0.66, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.9, 0.29, 0.49, 2.2, 9.6e-2, 21.0, 59.0, 0.99714, 3.31, 0.67, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.69, 8.0e-2, 2.1, 6.3e-2, 42.0, 52.0, 0.99608, 3.42, 0.6, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.44, 9.0e-2, 2.2, 6.3e-2, 9.0, 18.0, 0.99444, 3.42, 0.69, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.705, 0.1, 2.8, 8.1e-2, 13.0, 28.0, 0.99631, 3.6, 0.66, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.53, 0.13, 2.0, 5.8e-2, 18.0, 22.0, 0.99573, 3.21, 0.68, 9.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.39, 0.3, 1.9, 7.4e-2, 32.0, 84.0, 0.99717, 3.39, 0.61, 9.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.56, 0.14, 2.4, 6.4e-2, 13.0, 29.0, 0.99397, 3.42, 0.62, 11.7]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.55, 0.13, 2.2, 7.5e-2, 15.0, 35.0, 0.9959, 3.36, 0.59, 9.7]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.53, 8.0e-2, 1.9, 7.7e-2, 24.0, 45.0, 0.99528, 3.6, 0.68, 10.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.4, 0.58, 8.0e-2, 1.9, 5.9e-2, 20.0, 31.0, 0.99484, 3.5, 0.64, 10.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.64, 9.0e-2, 2.5, 8.1e-2, 15.0, 26.0, 0.99538, 3.57, 0.63, 12.0]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.39, 0.32, 1.8, 6.5e-2, 34.0, 60.0, 0.99714, 3.46, 0.78, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.52, 8.0e-2, 4.4, 7.1e-2, 11.0, 32.0, 0.99646, 3.56, 0.63, 11.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.25, 0.29, 2.2, 5.4e-2, 19.0, 49.0, 0.99666, 3.4, 0.76, 10.9]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.855, 2.0e-2, 1.9, 6.4e-2, 29.0, 38.0, 0.99472, 3.3, 0.56, 10.75]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [11.1, 0.44, 0.42, 2.2, 6.4e-2, 14.0, 19.0, 0.99758, 3.25, 0.57, 10.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.4, 0.37, 0.43, 2.3, 6.3e-2, 12.0, 19.0, 0.9955, 3.17, 0.81, 11.2]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.5, 0.63, 0.33, 1.8, 5.9e-2, 16.0, 28.0, 0.99531, 3.36, 0.64, 10.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.57, 2.0e-2, 2.0, 7.2e-2, 17.0, 26.0, 0.99575, 3.36, 0.61, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.6, 0.1, 1.6, 4.8e-2, 12.0, 26.0, 0.99306, 3.55, 0.51, 12.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [11.2, 0.4, 0.5, 2.0, 9.9e-2, 19.0, 50.0, 0.99783, 3.1, 0.58, 10.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.36, 0.3, 1.8, 7.4e-2, 17.0, 24.0, 0.99419, 3.24, 0.7, 11.4]
            , label = Label "8"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.68, 0.0, 2.3, 8.7e-2, 17.0, 26.0, 0.99783, 3.45, 0.53, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.1, 0.67, 0.0, 2.3, 8.3e-2, 18.0, 27.0, 0.99768, 3.44, 0.54, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.68, 1.0e-2, 3.7, 0.103, 32.0, 54.0, 0.99586, 3.51, 0.66, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.735, 0.0, 2.2, 8.0e-2, 18.0, 28.0, 0.99765, 3.41, 0.6, 9.4]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.855, 2.0e-2, 2.4, 6.2e-2, 15.0, 23.0, 0.99627, 3.54, 0.6, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.56, 0.17, 1.7, 6.5e-2, 15.0, 24.0, 0.99514, 3.44, 0.68, 10.55]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.88, 4.0e-2, 2.2, 6.6e-2, 12.0, 20.0, 0.99636, 3.53, 0.56, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.855, 2.0e-2, 2.4, 6.2e-2, 15.0, 23.0, 0.99627, 3.54, 0.6, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.9, 0.63, 0.33, 6.7, 0.235, 66.0, 115.0, 0.99787, 3.22, 0.56, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.6, 0.26, 2.0, 8.0e-2, 31.0, 131.0, 0.99622, 3.21, 0.52, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.6, 0.26, 2.0, 8.0e-2, 31.0, 131.0, 0.99622, 3.21, 0.52, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.8, 0.6, 0.26, 2.0, 8.0e-2, 31.0, 131.0, 0.99622, 3.21, 0.52, 9.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.695, 0.13, 2.0, 7.6e-2, 12.0, 20.0, 0.99546, 3.29, 0.54, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.695, 0.13, 2.0, 7.6e-2, 12.0, 20.0, 0.99546, 3.29, 0.54, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.695, 0.13, 2.0, 7.6e-2, 12.0, 20.0, 0.99546, 3.29, 0.54, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.67, 2.0e-2, 1.9, 6.1e-2, 26.0, 42.0, 0.99489, 3.39, 0.82, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.16, 0.64, 2.1, 5.9e-2, 24.0, 52.0, 0.99494, 3.34, 0.71, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.695, 0.13, 2.0, 7.6e-2, 12.0, 20.0, 0.99546, 3.29, 0.54, 10.1]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.0, 0.56, 0.13, 1.6, 7.7e-2, 25.0, 42.0, 0.99629, 3.34, 0.59, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.51, 0.14, 1.9, 5.6e-2, 15.0, 34.0, 0.99396, 3.48, 0.57, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.36, 0.53, 2.2, 0.23, 19.0, 35.0, 0.9934, 3.37, 0.93, 12.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.4, 0.38, 0.14, 2.2, 3.8e-2, 15.0, 25.0, 0.99514, 3.44, 0.65, 11.1]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.3, 0.69, 0.32, 2.2, 6.9e-2, 35.0, 104.0, 0.99632, 3.33, 0.51, 9.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.58, 0.2, 2.4, 7.5e-2, 15.0, 50.0, 0.99467, 3.58, 0.67, 12.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.6, 0.31, 0.78, 13.9, 7.4e-2, 23.0, 92.0, 0.99677, 3.39, 0.48, 10.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.52, 0.4, 2.2, 6.0e-2, 12.0, 20.0, 0.99474, 3.26, 0.64, 11.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [8.0, 0.3, 0.63, 1.6, 8.1e-2, 16.0, 29.0, 0.99588, 3.3, 0.78, 10.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.7, 0.15, 5.1, 7.6e-2, 13.0, 27.0, 0.99622, 3.54, 0.6, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.67, 0.15, 1.8, 0.118, 13.0, 20.0, 0.9954, 3.42, 0.67, 11.3]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.56, 9.0e-2, 1.7, 5.3e-2, 24.0, 32.0, 0.99402, 3.54, 0.6, 11.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [7.4, 0.35, 0.33, 2.4, 6.8e-2, 9.0, 26.0, 0.9947, 3.36, 0.6, 11.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.56, 9.0e-2, 1.7, 5.3e-2, 24.0, 32.0, 0.99402, 3.54, 0.6, 11.3]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.1, 0.715, 0.1, 2.6, 5.3e-2, 13.0, 27.0, 0.99362, 3.57, 0.5, 11.9]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.46, 0.29, 2.1, 7.4e-2, 32.0, 98.0, 0.99578, 3.33, 0.62, 9.8]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.7, 0.32, 0.44, 2.4, 6.1e-2, 24.0, 34.0, 0.99484, 3.29, 0.8, 11.6]
            , label = Label "7"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.39, 0.44, 2.6, 6.6e-2, 22.0, 48.0, 0.99494, 3.3, 0.84, 11.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.5, 0.31, 0.41, 2.4, 6.5e-2, 34.0, 60.0, 0.99492, 3.34, 0.85, 11.4]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.8, 0.61, 0.11, 1.8, 6.6e-2, 18.0, 28.0, 0.99483, 3.55, 0.66, 10.9]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [7.2, 0.66, 0.33, 2.5, 6.8e-2, 34.0, 102.0, 0.99414, 3.27, 0.78, 12.8]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.6, 0.725, 0.2, 7.8, 7.3e-2, 29.0, 79.0, 0.9977, 3.29, 0.54, 9.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.55, 0.15, 1.8, 7.7e-2, 26.0, 35.0, 0.99314, 3.32, 0.82, 11.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.4, 0.74, 9.0e-2, 1.7, 8.9e-2, 16.0, 26.0, 0.99402, 3.67, 0.56, 11.6]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.51, 0.13, 2.3, 7.6e-2, 29.0, 40.0, 0.99574, 3.42, 0.75, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.8, 0.62, 8.0e-2, 1.9, 6.8e-2, 28.0, 38.0, 0.99651, 3.42, 0.82, 9.5]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.2, 0.6, 8.0e-2, 2.0, 9.0e-2, 32.0, 44.0, 0.9949, 3.45, 0.58, 10.5]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.55, 0.1, 2.2, 6.2e-2, 39.0, 51.0, 0.99512, 3.52, 0.76, 11.2]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [6.3, 0.51, 0.13, 2.3, 7.6e-2, 29.0, 40.0, 0.99574, 3.42, 0.75, 11.0]
            , label = Label "6"
            }
        , DataEntry
            { dataPoint = V.fromList [5.9, 0.645, 0.12, 2.0, 7.5e-2, 32.0, 44.0, 0.99547, 3.57, 0.71, 10.2]
            , label = Label "5"
            }
        , DataEntry
            { dataPoint = V.fromList [6.0, 0.31, 0.47, 3.6, 6.7e-2, 18.0, 42.0, 0.99549, 3.39, 0.66, 11.0]
            , label = Label "6"
            }
        ]
    }