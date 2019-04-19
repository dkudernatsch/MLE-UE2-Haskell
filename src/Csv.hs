module Csv where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as CSV
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Vector          as V

import           Control.Exception    (throw)
import           Control.Monad        (mzero)
import           DataSet

readCsv :: String -> IO (Either String DataSet)
readCsv path = fmap readCsvData (BL.readFile path)

readCsvData :: BL.ByteString -> Either String DataSet
readCsvData str = do
  records <- CSV.decode CSV.NoHeader str
  let head = V.head records
  let tail = V.tail records
  let headerP = CSV.parseRecord head :: CSV.Parser Header
  let datasP = V.mapM CSV.parseRecord tail :: CSV.Parser (V.Vector DataEntry)
  do header <- CSV.runParser headerP
     datas <- CSV.runParser datasP
     return (DataSet header (V.toList datas))

instance CSV.FromRecord Header where
  parseRecord v = Header <$> mapM CSV.parseField (V.toList v)

instance CSV.FromRecord DataEntry where
  parseRecord v
    | length v > 1 = DataEntry <$> V.mapM CSV.parseField (V.init v) <*> (CSV.parseField . V.last $ v)
    | otherwise = mzero

instance CSV.FromField Label where
  parseField f = intoLabel <$> CSV.parseField f
