{-# LANGUAGE OverloadedStrings #-}
module Main 
    (main
    )
    where

import DecisionTree
import ParseCsv 
import RandomForest ( predictRF )
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
            
            putStr "Answer the following questions with Yes or No."

            s1 <- ask "Are you coughing?"
            s2 <- ask "Do you have a fever?"
            s3 <- ask "Do you have a sore throat?"
            s4 <- ask "Do you have shortness of breath?"
            s5 <- ask "Do you have a headache?"
            s6 <- ask "Are you 60 or older?"
            s7 <- ask "Are you male?"

            let syms = s1:s2:s3:s4:s5:s6:s7:[]

            putStr "The algorithm is working."

            result <- predictRF syms 23 dat

            if result == 1
                then putStr ("RandomForest prediction: Positive")
                else putStr ("RandomForest prediction: Negative")

ask q =
    do
        putStr q
        line <- getLine 
        let ans = read line
        if (isYes ans || isNo ans)
            then
                if isYes ans
                    then return 1
                    else return 0
            else
                ask q


isYes ans = ans == "Yes" || ans == "yes" || ans == "YES"
isNo ans = ans == "No" || ans == "no" || ans == "NO"