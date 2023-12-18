module Runner (interpolate) where

import Approximation (lagrangiaInterpolation, linearInterpolation)
import Cli (readPoints, writePoints)
import Generator (generatePointsFromBegin, generatePointsToEnd)

--------------------------------------------------------------------------------

interpolate :: Int -> Double -> [String] -> IO ()
interpolate windowSize freq methods = do
  inp <- readPoints
  let (windowX, windowY) = unzip inp
  startPoints (take windowSize windowX) (take windowSize windowY)
  doRun windowX windowY
  where
    doRun windowX windowY = do
      let windowX' = (take windowSize $ windowX)
      let windowY' = (take windowSize $ windowY)
      let pointX = windowX' !! (length windowX' - 1)

      if length windowX' < windowSize
        then do
          let pointsToPredict = generatePointsToEnd (head windowX') pointX freq
          interpolateAndPrint methods windowX' windowY' pointsToPredict
        else do
          interpolateAndPrint methods windowX' windowY' [(head windowX' + pointX) / 2]
          doRun (tail windowX) (tail windowY)

    startPoints windowX windowY = do
      let pointsToPredict = generatePointsFromBegin (head windowX) (windowX !! (length windowX - 1)) freq
      interpolateAndPrint methods windowX windowY pointsToPredict

    interpolateAndPrint [] _ _ _ = putStr ""
    interpolateAndPrint (hMethod : tMethods) windowX' windowY' pointsToPredict = do
      let result = getMethod hMethod windowX' windowY' pointsToPredict
      writePoints hMethod pointsToPredict result
      interpolateAndPrint tMethods windowX' windowY' pointsToPredict

    getMethod method
      | method == "lagrangia" = lagrangiaInterpolation
      | otherwise = linearInterpolation
