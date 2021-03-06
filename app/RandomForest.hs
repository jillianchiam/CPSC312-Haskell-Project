module RandomForest where

import qualified System.Random as R
import qualified Data.Vector as V

import Data.Matrix as MT
import Control.Monad.Random
import System.Random

import DecisionTree
import ParseCsv

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv


-- Converts a csv file to a matrix
csvToMatrix :: FilePath -> IO (Matrix Int)
csvToMatrix str = do
    csvData <- BL.readFile str
    case decodeByName csvData of
        Left err -> return (listToMatrix [[0]])
        Right (_, v) -> do
            let listoflist = (V.toList $ V.map valuesToList v)
            let dat = listToMatrix listoflist

            return dat


-- Randomly Sample a single element from a list. Sourced from https://github.com/chris-taylor/aima-haskell/blob/master/src/AI/Util/Util.hs
sampleOne :: MonadRandom m => [b] -> m b
sampleOne xs = do
    n <- getRandomR (0, length xs - 1)
    return (xs !! n)

-- Randomly Sample n elements with replacement from a list. Sourced from https://github.com/chris-taylor/aima-haskell/blob/master/src/AI/Util/Util.hs
sampleWithReplacement :: (Eq t, Num t, MonadRandom m) => t -> [a] -> m [a]
sampleWithReplacement 0 xs = return []
sampleWithReplacement n xs = do
    y  <- sampleOne xs
    ys <- sampleWithReplacement (n-1) xs
    return (y:ys)


-- Create a bootstrap sample from a given matrix of observations. 
bootstrapSample :: MonadRandom m => Matrix a -> m (Matrix a)
bootstrapSample m = do
    y <- sampleWithReplacement (MT.nrows m) (toLists m)

    return (MT.fromLists y)


-- Accumulator function for growForest
growForestAcc :: (MonadRandom m, Integral a, Num t, Num b, Eq t) => [DTree Int b] -> t -> Matrix a -> m [DTree Int b]
growForestAcc accTrees 0 _ = return accTrees
growForestAcc accTrees nTrees m = do
    sample <- bootstrapSample m
    let tree = growTree sample
    growForestAcc (accTrees ++ [tree]) (nTrees-1) m
    
-- Grow a random forest - a collection of trees grown from bootstrap samples.
growForest :: (MonadRandom m, Integral a, Num t, Num b, Eq t) => t -> Matrix a -> m [DTree Int b]    
growForest nTrees m = do growForestAcc [] nTrees m

{-
predictRFs v nTrees m = do
    forest <- growForest nTrees m
    let p = map (flip predict v) forest
    return p
-}

-- Finds the most common element of a list containing only 1s and 0s.
findModeList :: (Integral a, Foldable t, Num p) => t a -> p
findModeList l = if fromIntegral(sum l) >= (fromIntegral(length l) / 2) then 1 else 0


-- Given a vector of new predictors, number of trees to grow, and a data matrix, predictRF returns the randomforest prediction of the response variable
predictRF :: (MonadRandom m, Integral a1, Num t, Num b, Num a2, Eq t, Eq a2) => [a2] -> t -> Matrix a1 -> m b
predictRF v nTrees m = do
    forest <- growForest nTrees m
    let p = map (flip predict v) forest
    return (findModeList p)


-- Given a vector of new predictors, number of trees to grow, and a csv file name, predictRF returns the randomforest prediction of the response variable
predictRFcsv :: (Num t, Num b, Num a2, Eq t, Eq a2) => [a2] -> t -> FilePath -> IO b
predictRFcsv v nTrees str = do
    m <- csvToMatrix str
    x <- predictRF v nTrees m
    return x