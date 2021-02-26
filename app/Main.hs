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
       Right (h, v) -> do       
           putStrLn $ show h    --produce header of columns
           V.forM_ v $ \ p ->   -- print the data
                print (test_date p, cough p, fever p, sore_throat p, shortness_of_breath p, head_ache p, changeString(corona_result p), changeString2(age_60_and_above p), changeString(gender p), changeString2(test_indication p))


