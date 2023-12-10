module Runner (interpolate) where

import Approximation (lagrangiaInterpolation, linearInterpolation)
import Cli (readPoint, writePoints)
import Generator (generatePointsFromBegin, generatePointsToEnd)

--------------------------------------------------------------------------------

interpolate :: Int -> Double -> [String] -> IO ()
interpolate windowSize freq methods = run windowSize freq methods [] [] True

run :: Int -> Double -> [String] -> [Double] -> [Double] -> Bool -> IO ()
run windowSize freq methods windowX windowY inBegin = do
  inp <- readPoint
  case inp of
    Just (pointX, pointY) -> doRun (windowX ++ [pointX]) (windowY ++ [pointY]) pointX
    Nothing -> do
      let pointsToPredict = generatePointsToEnd (head windowX) (windowX !! (length windowX - 1)) freq
      interpolateAndPrint methods windowX windowY pointsToPredict
  where
    doRun :: [Double] -> [Double] -> Double -> IO ()
    doRun windowX' windowY' pointX
      | length windowX' < windowSize = do
          let pointsToPredict =
                if and [inBegin, (length windowX' > 1)]
                  then generatePointsFromBegin (head windowX') pointX freq
                  else []
          interpolateAndPrint methods windowX' windowY' pointsToPredict

          run windowSize freq methods windowX' windowY' inBegin
      | otherwise = do
          let pointsToPredict = [(head windowX' + pointX) / 2]
          interpolateAndPrint methods windowX' windowY' pointsToPredict

          run windowSize freq methods (tail windowX') (tail windowY') False

    interpolateAndPrint :: [String] -> [Double] -> [Double] -> [Double] -> IO ()
    interpolateAndPrint [] _ _ _ = putStr ""
    interpolateAndPrint (hMethod : tMethods) windowX' windowY' pointsToPredict = do
      let result = getMethod hMethod windowX' windowY' pointsToPredict
      writePoints hMethod pointsToPredict result
      interpolateAndPrint tMethods windowX' windowY' pointsToPredict

    getMethod method
      | method == "lagrangia" = lagrangiaInterpolation
      | otherwise = linearInterpolation
