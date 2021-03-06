{-# LANGUAGE OverloadedStrings #-}

module Main 
    (main
    )
    where

import ParseCsv 
import DecisionTree
import RandomForest

import Lib
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
-- text
import Text.CSV
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.Csv
import qualified Data.Csv as Cassava
--vector
import qualified Data.Vector as V
import Data.Matrix as MT


-- This part parses the data 
main :: IO ()
main = do
    let list = []
    csvData <- BL.readFile "test-3.csv" --small test file, manually change this to input a bigger csv file
    case decodeByName csvData of
        Left err -> putStrLn err --output error if cannot read file
        Right (_, v) -> do
            let listoflist = (V.toList $ V.map valuesToList v)
            let dat = listToMatrix listoflist
            
            putStr "\nAnswer the following questions with Yes or No.\n"

            s1 <- ask "\nAre you coughing?\n"
            s2 <- ask "\nDo you have a fever?\n"
            s3 <- ask "\nDo you have a sore throat?\n"
            s4 <- ask "\nDo you have shortness of breath?\n"
            s5 <- ask "\nDo you have a headache?\n"
            s6 <- ask "\nAre you 60 or older?\n"
            s7 <- ask "\nAre you male?\n"

            let syms = s1:s2:s3:s4:s5:s6:s7:[]

            putStr "\nThe algorithm is working.\n"

            result <- predictRF syms 20 dat

            if result == 1
                then putStr ("\nRandomForest prediction: \nCOVID-19 Positive\n\n")
                else putStr ("\nRandomForest prediction: \nCOVID-19 Negative\n\n")


-- Asks a question to the user given a string
ask :: Num b => String -> IO b
ask q =
    do
        putStr q
        line <- getLine 
        let ans = line
        if (isYes ans || isNo ans)
            then
                if isYes ans
                    then return 1
                    else return 0
            else
                ask q


-- Determines if a given string means yes
isYes ans = ans == "Yes" || ans == "yes" || ans == "YES"

-- Determines if a given string means no
isNo ans = ans == "No" || ans == "no" || ans == "NO"