module Csv where

import qualified Data.Text as T
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V


import DataSet
import Control.Monad (mzero)

readFromCsv :: String -> IO (Either String (V.Vector DataEntry))
readFromCsv path = do
    file <- BL.readFile path
    return $ CSV.decode CSV.HasHeader file


instance CSV.FromRecord Header where
    parseRecord v = Header <$> mapM CSV.parseField (V.toList v)

instance CSV.FromRecord DataEntry where
    parseRecord v
        | length v > 1 =
            DataEntry <$> mapM CSV.parseField (V.toList . V.init $ v) <*> (CSV.parseField . V.last $ v)
        | otherwise = mzero