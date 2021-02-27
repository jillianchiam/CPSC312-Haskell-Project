module DecisionTree where
import qualified Data.Vector as V
import Data.Matrix as MT


-- We can use any data as long as it is purely categorical and indexed by 1s and 0s.
listOfData = [[0,0,0,0,0,0,1,0],[0,1,0,0,0,0,1,0],[0,0,0,0,0,1,0,0],[0,0,0,0,0,0,1,1],[0,1,0,0,0,0,1,0],[1,0,0,0,0,0,1,0],[1,1,0,0,0,0,1,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[1,1,0,0,0,0,1,0],[1,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[1,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[1,0,0,0,0,0,1,0],[1,0,0,0,0,0,0,1],[0,1,0,0,0,0,0,0],[1,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0],[1,1,0,0,0,0,1,1],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,1,1,0],[0,0,0,0,0,1,1,1],[0,0,0,0,0,1,0,0],[1,1,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[1,0,0,0,0,1,0,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,1,1],[0,0,0,0,0,0,1,1],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,1,0,1,1,0],[1,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,1,1,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[1,0,0,0,0,0,0,0],[1,1,0,0,0,0,0,0],[1,1,0,0,0,0,1,0],[0,1,0,0,0,0,0,0],[1,0,0,0,0,0,0,1],[0,0,0,0,0,1,1,0],[0,0,0,0,0,1,0,0],[0,0,0,0,0,0,0,1],[0,0,0,0,0,0,1,0],[0,1,0,0,0,0,1,0],[1,1,0,0,0,0,1,0],[1,0,0,0,0,0,1,0],[1,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[1,0,0,0,0,0,1,0],[1,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[1,0,0,0,0,0,0,0],[1,0,0,0,0,0,1,0],[1,0,0,0,0,0,0,0],[1,0,0,0,0,0,1,0],[1,0,0,0,0,0,1,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[1,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[1,1,0,0,0,0,1,0],[1,1,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[1,0,0,0,0,0,0,0],[1,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0],[0,0,1,0,0,0,1,0],[0,0,0,0,0,1,1,0],[1,0,0,0,0,0,0,1],[0,0,1,0,0,0,0,0],[1,0,1,0,1,0,1,0],[1,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0]]

dat = MT.fromLists listOfData
numVars = MT.ncols dat

-- Splits on a given variable i and returns the purity of the split (determined using the Gini index)
splitOnVar :: (Fractional a2, Eq a1, Num a1) => Int -> Matrix a1 -> a2
splitOnVar i m = purity
    where

        vi = getCol i m
        vY = getCol numVars m

        vZeros = V.filter (\(x,y) -> x == 0) $ V.zip vi vY
        proportionYesZeros = fromIntegral(length $ V.filter (\(x,y) -> y == 1) vZeros) / fromIntegral(length vZeros)
        purityZeros = proportionYesZeros * (1-proportionYesZeros) * 2

        vOnes = V.filter (\(x,y) -> x == 1) $ V.zip vi vY
        proportionYesOnes = fromIntegral(length $ V.filter (\(x,y) -> y == 1) vOnes) / fromIntegral(length vOnes)
        purityOnes = proportionYesOnes * (1-proportionYesOnes) * 2

        purity = purityZeros + purityOnes


-- Determines the best predictor to make a split on.
bestSplit :: (Eq a, Num b, Num a) => Matrix a -> b
bestSplit m = snd $ V.minimumBy (\ (x,y) (x',y')-> compare x x') (V.zip purityCompare (V.enumFromN 1 (numVars-1))) 
    where
        purityCompare = V.map (\x -> splitOnVar x m) (V.enumFromN 1 (numVars-1))


-- TODO:

-- Perform the split on predictor chosen by BestSplit. Should give 2 matrices or a rearranged version of the original matrix
-- function: doBestSplit m
--doBestSplit :: (Fractional a, Eq a1, Num a1) => Matrix a1 -> V.Vector b -> V.Vector (a, b)
doBestSplit m =  V.zip(V.map (\x -> splitOnVar x m) (V.enumFromN 1 (numVars-1)))


-- Some recursive call to doBestSplit m until some stopping criterion is reached
-- function: growTree stopCriterion m
--        or growTree m

-- idea: save the splits made in a accumulator, say, bestSplitCollection. Then use this to create the decision rule at the end.
-- Pretty sure this is easier than looking through all the matrices we created and trying to determine the decision rule.

-- Some function that sets the decision rule. i.e. a Predict function
-- function: predict 


-- After this, make a new module, RandomForest, where we will
-- Create boostrapped samples
-- Grow trees using boostrapped samples, except only choose between (floor(sqrt(7))) = 2 predictors at each split (will probably 
-- have to edit the bestSplit function to only choose between 2 randomly sampled predictors).
-- Average the bagged estimates to receive the Random Forest decision rule/prediction

-- Do some stuff in Main to bring it all together

-- Done