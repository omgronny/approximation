module Approximation (linearInterpolation, getIndexesForPoints, lagrangiaInterpolation) where

--------------------------------------------------------------------------------

linearInterpolation :: [Double] -> [Double] -> [Double] -> [Double]
linearInterpolation inputX inputY pointsToPredict = doLinearInterpolation inputX inputY pointsToPredict (getIndexesForPoints inputX pointsToPredict)

doLinearInterpolation :: [Double] -> [Double] -> [Double] -> [Int] -> [Double]
doLinearInterpolation inputX inputY pointsToPredict indexesForPointsToPredict =
  map predict (zip pointsToPredict indexesForPointsToPredict)
  where
    predict :: (Double, Int) -> Double
    predict (point, index) = do
      let a = (inputY !! (index + 1) - inputY !! index) / (inputX !! (index + 1) - inputX !! (index))
      let b = inputY !! index - a * inputX !! index
      a * point + b

getIndexesForPoints :: [Double] -> [Double] -> [Int]
getIndexesForPoints inputX pointsToPredict =
  doGetIndexesForPoints [] 0 0
  where
    doGetIndexesForPoints :: [Int] -> Int -> Int -> [Int]
    doGetIndexesForPoints indexes thisPredictIndex thisInputIndex
      | thisPredictIndex == length pointsToPredict = reverse indexes
      | pointsToPredict !! thisPredictIndex <= inputX !! thisInputIndex = doGetIndexesForPoints ((thisInputIndex - 1) : indexes) (thisPredictIndex + 1) thisInputIndex
      | otherwise = doGetIndexesForPoints indexes thisPredictIndex (thisInputIndex + 1)

--------------------------------------------------------------------------------

lagrangiaInterpolation :: [Double] -> [Double] -> [Double] -> [Double]
lagrangiaInterpolation inputX inputY pointsToPredict =
  map predict pointsToPredict
  where
    -- L(x) = sum_i (yi * li(x))
    predict :: Double -> Double
    predict x = sum (map (calcCoef x) (zip inputX inputY))

    -- li(x) = mul_j ( (x - xi) / (xi - xj) )
    calcCoef :: Double -> (Double, Double) -> Double
    calcCoef x (inpX, inpY) = inpY * (foldl (foldSkip x inpX) 1 inputX) / (foldl (foldSkip inpX inpX) 1 inputX)

    foldSkip :: Double -> Double -> Double -> Double -> Double
    foldSkip x forbid fl xi
      | forbid == xi = fl
      | otherwise = fl * (x - xi)
