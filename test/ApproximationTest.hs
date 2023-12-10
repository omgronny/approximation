import Approximation (getIndexesForPoints, lagrangiaInterpolation, linearInterpolation)
import Test.HUnit (Test (..), assertEqual, runTestTTAndExit)

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests =
  TestList
    [ TestLabel "linear: test getIndexesForPoints" test1,
      TestLabel "linear: test simple" test2,
      TestLabel "lagrangia: test simple" test3
    ]

test1 :: Test
test1 =
  TestCase
    ( do
        let indexes = getIndexesForPoints [1, 2, 5, 6, 8, 15] [4, 9]
        assertEqual "Test assert" indexes [1, 4]
    )

test2 :: Test
test2 =
  TestCase
    ( do
        let predicted = linearInterpolation [0, 2] [0, 2] [1]
        assertEqual "Test assert" predicted [1]

        let predictedNegative = linearInterpolation [0, 2] [0, -2] [1]
        assertEqual "Test assert" predictedNegative [-1]

        let predicted = linearInterpolation [0, 2, 4] [0, -4, 5] [1, 3]
        assertEqual "Test assert" predicted [-2, 0.5]
    )

test3 :: Test
test3 =
  TestCase
    ( do
        let predicted = lagrangiaInterpolation [0, 2] [0, 2] [1]
        assertEqual "Test assert" predicted [1]

        let predictedNegative = lagrangiaInterpolation [0, 2] [0, -2] [1]
        assertEqual "Test assert" predictedNegative [-1]

        let predicted = lagrangiaInterpolation [0, 2, 4] [0, -4, 5] [1, 3]
        assertEqual "Test assert" predicted [-3.625, -1.125]
    )
