{-# OverloadedStrings #-}

module CsvSpec where

import qualified Csv as SUT
import Test.Hspec (it, shouldBe)
import DataSet (DataSet, Header, DataSet)
import Data.Vector (Vector)