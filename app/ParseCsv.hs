{-# LANGUAGE OverloadedStrings #-}
module ParseCsv where

import Lib
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
--import Data.List.Split

import Data.Csv
import qualified Data.Csv as Cassava
--vector
import qualified Data.Vector as V
import Data.Matrix as MT



-- !Int == Strictly type Int ; !String == strictly type String
-- define the data types to represent a Person
data Person = Person
    {cough  :: !Int
    , fever  :: !Int
    , sore_throat :: !Int
    , shortness_of_breath :: !Int
    , head_ache :: !Int
    , age_60_and_above :: !Int
    , gender_male1_female0 :: !Int
    , corona_result :: !Int
    }

-- decode by parsing a CSV row and how to assemble the parsed values
instance FromNamedRecord Person where
    parseNamedRecord p = pure Person
        Person 
        <$> p .: "cough"
        <*> p .: "fever"
        <*> p .: "sore_throat"
        <*> p .: "shortness_of_breath"
        <*> p .: "head_ache"
        <*> p .: "age_60_and_above"
        <*> p .: "gender_male1_female0"
        <*> p .: "corona_result"


--Gives the csv a header when printed
instance DefaultOrdered Person where
  headerOrder _ =
    Cassava.header
      ["cough"
        , "fever"
        , "sore_throat"
        , "shortness_of_breath"
        , "head_ache"
        , "age_60_and_above"
        , "gender_male1_female0"
        , "corona_result"
      ]

--instance FromField Data.Time.Day where
--    parseField = parseTimeM True defaultTimeLocale "%d/%m/%Y" . show

--changes the following columns:
-- corona_result : positive = 1 ; negative = 0
-- gender       : female = 1 ; male = 0
--changeString :: String -> Int
--changeString x = if (x == "positive" || x == "female") 
--                    then 1
--                    else 0

--changes the following columns:
-- age_60_and_above : Yes = 1 ; No = 0 ; None = 0
--changeString2 :: String -> Int
--changeString2 x = if (x == "Yes" || x == "Abroad" || x == "Contact with confirmed") 
--                    then 1
--                    else 0

--changes the values of Person into a list of Integers
valuesToList :: Person -> [Int]
valuesToList (Person c1 c2 c3 c4 c5 c6 c7 c8) = [c1, c2, c3, c4, c5, c6, c7, c8]

-- prints the parsed csv file 
runCSV ::  IO ()
runCSV = do
    csvData <- BL.readFile "test-2.csv" --small test file, manually change this to input a bigger csv file
    case decodeByName csvData of
        Left err -> putStrLn err --output error if cannot read file
        Right (h, v) -> do             
            putStrLn $ show h    --produce header of columns
            V.forM_ v $ \ p ->   -- print the data
                print (cough p, fever p, sore_throat p, shortness_of_breath p, head_ache p, age_60_and_above p, gender_male1_female0 p, corona_result p)


