module Runner(interpolate) where

import Cli(readPoint, writePoints)

import Approximation(linearInterpolation, lagrangiaInterpolation)

--------------------------------------------------------------------------------

interpolate :: Int -> Double -> String -> IO()
interpolate windowSize freq method = run windowSize freq method [] [] True

run :: Int -> Double -> String -> [Double] -> [Double] -> Bool -> IO()
run windowSize freq method windowX windowY inBegin = do
    print $ "next value:"

    inp <- readPoint
    case inp of
        Just (pointX, pointY) -> doRun (windowX ++ [pointX]) (windowY ++ [pointY]) pointX
        Nothing -> do
            print $ "end of input"
            let pointsToPredict = generatePointsToEnd (head windowX) (windowX!!(length windowX - 1)) 0.1
            interpolateAndPrint windowX windowY pointsToPredict

    where
        doRun :: [Double] -> [Double] -> Double -> IO()
        doRun windowX' windowY' pointX
            | length windowX' < windowSize = do
                let pointsToPredict = if and [inBegin, (length windowX' > 1)]
                    then
                        generatePointsFromBegin (head windowX') pointX freq
                    else
                        []
                interpolateAndPrint windowX' windowY' pointsToPredict

                run windowSize freq method windowX' windowY' inBegin

            | otherwise = do
                let pointsToPredict = [((head windowX') + pointX) / 2]
                interpolateAndPrint windowX' windowY' pointsToPredict

                run windowSize freq method (tail windowX') (tail windowY') False

        interpolateAndPrint :: [Double] -> [Double] -> [Double] -> IO()
        interpolateAndPrint windowX' windowY' pointsToPredict = do
            let result = getMethod windowX' windowY' pointsToPredict
            writePoints pointsToPredict result

        generatePointsFromBegin :: Double -> Double -> Double -> [Double]
        generatePointsFromBegin from to step
            | from == to = []
            | otherwise = doGeneratePoints from ((from + to) / 2) step [from + step]

        generatePointsToEnd :: Double -> Double -> Double -> [Double]
        generatePointsToEnd from to step
            | from == to = []
            | otherwise = doGeneratePoints ((from + to) / 2) to step [from + step]

        doGeneratePoints :: Double -> Double -> Double -> [Double] -> [Double]
        doGeneratePoints h t step acc
            | (head acc) + step >= t = reverse acc
            | otherwise = doGeneratePoints h t step ((head acc + step) : acc)

        getMethod
            | method == "lagrangia" = lagrangiaInterpolation
            | otherwise = linearInterpolation

