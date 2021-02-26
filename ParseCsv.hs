{-# LANGUAGE OverloadedStrings #-}
module ParseCsv where

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

-- !Int == Strictly type Int ; !String == strictly type String
data Person = Person
    { --test_date   :: !Data.Time.Day
    test_date   :: !String
    , cough  :: !Int
    , fever  :: !Int
    , sore_throat :: !Int
    , shortness_of_breath :: !Int
    , head_ache :: !Int
    , corona_result :: !String
    , age_60_and_above :: !String
    , gender :: !String
    , test_indication :: !String
    }

instance FromNamedRecord Person where
    parseNamedRecord p = pure Person
        Person 
        <$> p .: "test_date"
        <*> p .: "cough"
        <*> p .: "fever"
        <*> p .: "sore_throat"
        <*> p .: "shortness_of_breath"
        <*> p .: "head_ache"
        <*> p .: "corona_result"
        <*> p .: "age_60_and_above"
        <*> p .: "gender"
        <*> p .: "test_indication"


--Gives the csv a header when printed
instance DefaultOrdered Person where
  headerOrder _ =
    Cassava.header
      ["test_date" 
        , "cough"
        , "fever"
        , "sore_throat"
        , "shortness_of_breath"
        , "head_ache"
        , "corona_result"
        , "age_60_and_above"
        , "gender"
        , "test_indication"
      ]

--instance FromField Data.Time.Day where
--    parseField = parseTimeM True defaultTimeLocale "%d/%m/%Y" . show


