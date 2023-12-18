module Generator (generatePointsFromBegin, generatePointsToEnd) where

generatePointsFromBegin :: (Floating a) => (Ord a) => a -> a -> a -> [a]
generatePointsFromBegin from to step
  | from == to = []
  | otherwise = doGeneratePoints from ((from + to) / 2) step [from + step]

generatePointsToEnd :: (Floating a) => (Ord a) => a -> a -> a -> [a]
generatePointsToEnd from to step
  | from == to = []
  | otherwise = doGeneratePoints ((from + to) / 2) to step [from + step]

doGeneratePoints :: (Floating a) => (Ord a) => a -> a -> a -> [a] -> [a]
doGeneratePoints h t step acc
  | (head acc) + step >= t = reverse acc
  | otherwise = doGeneratePoints h t step ((head acc + step) : acc)
