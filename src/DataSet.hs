module DataSet where

import qualified Data.Text as T
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Errors


data DataSet = DataSet
        { header :: Header
        , data_lines :: DataLines
        } deriving (Eq, Show)

newtype Header
    = Header [T.Text]
    deriving (Eq, Show)

type DataLines = [DataEntry]

data DataEntry = DataEntry {
    dataPoint :: [Double],
    label :: T.Text
} deriving (Eq, Show)
