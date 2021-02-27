{-# LANGUAGE OverloadedStrings #-}
module Main 
    (main
    )
    where

import ParseCsv 
import Lib
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.Csv
import qualified Data.Csv as Cassava
--vector
import qualified Data.Vector as V


-- This part parses the data 
main :: IO ()
main = do
    csvData <- BL.readFile "corona_test_small.csv" --small test file, manually change this to input a bigger csv file
    case decodeByName csvData of
       Left err -> putStrLn err --output error if cannot read file
       Right (_, v) -> do
            let listoflist = (V.toList $ V.map valuesToList v)
            print listoflist   
       --bring tree together


