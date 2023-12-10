module Generator(generatePointsFromBegin, generatePointsToEnd) where

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
