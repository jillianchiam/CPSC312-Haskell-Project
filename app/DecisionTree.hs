module DecisionTree where
import qualified Data.Vector as V
import Data.Matrix as MT


data DTree var b = Prediction b
                 | Node var (DTree var b) (DTree var b)
        deriving (Show, Read)

-- Change a list to a matrix
listToMatrix :: [[a]] -> Matrix a
listToMatrix = MT.fromLists

-- Splits on a given variable i and returns the purity of the split (determined using the Gini index)
splitOnVar :: (Fractional a2, Eq a1, Num a1) => Int -> Matrix a1 -> a2
splitOnVar i m = purity
    where

        vi = getCol i m
        vY = getCol (MT.ncols m) m

        vZeros = V.filter (\(x,y) -> x == 0) $ V.zip vi vY
        proportionYesZeros = fromIntegral(length $ V.filter (\(x,y) -> y == 1) vZeros) / fromIntegral(length vZeros)
        purityZeros = proportionYesZeros * (1-proportionYesZeros) * 2

        vOnes = V.filter (\(x,y) -> x == 1) $ V.zip vi vY
        proportionYesOnes = fromIntegral(length $ V.filter (\(x,y) -> y == 1) vOnes) / fromIntegral(length vOnes)
        purityOnes = proportionYesOnes * (1-proportionYesOnes) * 2

        purity = purityZeros + purityOnes


-- Determines the best predictor to make a split on.
bestSplit :: (Eq a, Num b, Num a) => Matrix a -> b
bestSplit m = 
    if and $ V.map (\x -> x == 1) purityCompare
        then 0
        else snd $ V.minimumBy (\ (x,y) (x',y')-> compare x x') (V.zip purityCompare (V.enumFromN 1 (MT.ncols m - 1))) 
    where
        purityCompare = V.map (\x -> if isNaN x == True then 1 else x) $ V.map (\x -> splitOnVar x m) (V.enumFromN 1 (MT.ncols m - 1))
        

-- Accumulator function for doBestSplit
doBestSplitAccum :: (Eq a, Num a) => Matrix a -> [[a]] -> [[a]] -> [Int] -> ([Matrix a], Int)
doBestSplitAccum m accOnes accZeros [] = ([MT.fromLists accOnes, MT.fromLists accZeros], best) where best = bestSplit m       
doBestSplitAccum m accOnes accZeros (x:xs) = 
    if (thisRow V.! (best - 1)) == 1
        then doBestSplitAccum m (accOnes ++ [V.toList thisRow]) accZeros xs
        else doBestSplitAccum m accOnes (accZeros ++ [V.toList thisRow]) xs
    
    where
        best = bestSplit m
        thisRow = MT.getRow x m


-- Splits a matrix on the predictor given by bestSplit, then removes that predictor, resulting in two matrices, each with one less predictor than before. Also returns the removed predictor.
doBestSplit :: (Eq a, Num a) => Matrix a -> ([Matrix a], Int)
doBestSplit m = doBestSplitAccum m [] [] (V.toList $ V.enumFromN 1 (MT.nrows m))


-- Remove a given column from a Matrix
removeCol :: Int -> Matrix a -> Matrix a
removeCol c m = 
    if c == 1 
        then (submatrix 1 (MT.nrows m) 2 (MT.ncols m) m)
        else (submatrix 1 (MT.nrows m) 1 (c - 1) m) <|> (submatrix 1 (MT.nrows m) (c + 1) (MT.ncols m) m)

-- Find the mode of the last column of a matrix, where the column only contains 1s or 0s.
findMode :: (Integral a, Num p) => Matrix a -> p
findMode m1 = if fromIntegral(V.sum v) >= (fromIntegral(V.length v) / 2) then 1 else 0
    where 
        v = MT.getCol (MT.ncols m1) m1

-- Takes a matrix m and returns an unpruned decision tree
growTree :: (Integral a, Num b) => Matrix a -> DTree Int b
growTree m = Node b (if bestSplit m1 == 0 then Prediction (findMode m1) else growTree m1) (if bestSplit m2 == 0 then Prediction (findMode m2) else growTree m2)
    where 
        ([m1,m2],b) = doBestSplit m


-- Given a vector of predictor values and a decision tree, predict returns the predicted response value.
predict :: (Eq a, Num a) => DTree var p -> [a] -> p
predict (Prediction b) _ = b
predict (Node var lt rt) (x:xs)
    | x == 1 = predict lt xs
    | x == 0 = predict rt xs