import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Unsafe
import Graphics.Matplotlib
import System.IO.Temp

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

tryit m = unsafePerformIO $ withSystemTempFile "a.png" (\file _ -> figure file m)

unitTests = testGroup "Unit tests"
  [ testCase "m1" $ tryit m1 @?= Right ""
  , testCase "m2" $ tryit m2 @?= Right ""
  , testCase "m3" $ tryit m3 @?= Right ""
  , testCase "m4" $ tryit m4 @?= Right ""
  , testCase "m5" $ tryit m5 @?= Right ""
  -- TODO This test case is broken
  -- , testCase "m6" $ tryit m6 @?= Right ""
  , testCase "m7" $ tryit m7 @?= Right ""
  ]


-- 

m1 :: Matplotlib
m1 = do
  let l = [7, 5, 0, 8, 2, 9, 1, 8, 2, 7, 5, 7, 3, 9, 1, 3, 3, 7, 0, 7, 0, 7, 1, 9, 1, 0, 5, 4, 8, 0, 7, 4, 1, 9, 4, 2, 5, 7, 3, 1, 9, 1, 2, 2, 3, 7, 0, 0, 3, 8, 0, 3, 1, 1, 9, 7, 2, 3, 4, 2, 3, 5, 9, 8, 0, 8, 6, 7, 0, 4, 0, 0, 2, 2, 4, 1, 7, 6, 9, 3, 1, 0, 6, 4, 1, 7, 4, 8, 7, 1, 3, 2, 7, 9, 4, 3, 8, 5, 3]
  histogram (l :: [Int]) (4 :: Int) []

m2 :: Matplotlib
m2 = do
  let l = [7, 5, 0, 8, 2, 9, 1, 8, 2, 7, 5, 7, 3, 9, 1, 3, 3, 7, 0, 7, 0, 7, 1, 9, 1, 0, 5, 4, 8, 0, 7, 4, 1, 9, 4, 2, 5, 7, 3, 1, 9, 1, 2, 2, 3, 7, 0, 0, 3, 8, 0, 3, 1, 1, 9, 7, 2, 3, 4, 2, 3, 5, 9, 8, 0, 8, 6, 7, 0, 4, 0, 0, 2, 2, 4, 1, 7, 6, 9, 3, 1, 0, 6, 4, 1, 7, 4, 8, 7, 1, 3, 2, 7, 9, 4, 3, 8, 5, 3]
  histogram (l :: [Int]) (4 :: Int) [K "cumulative" "True"]

m3 = scatter [1::Int, 2, 3, 4] [3::Int, 4, 5, 6] []

degreesRadians a = a * pi / 180.0
radiansDegrees a = a * 180.0 / pi

m4 = contourF (\a b -> sin (degreesRadians a) + cos (degreesRadians b)) (-100) 100 (-200) 200 10

m5 = histogram [(4 :: Double), 4, 2, 4, 3, 4, 4, 2, 2, 3, 2, 0, 1, 1, 0, 4, 2, 3, 5, 4, 3, 1, 0, 1, 3, 2, 1, 3, 1, 1, 1, 1, 0, 2, 2, 1, 3, 3, 1, 4, 2, 0, 2, 0, 3, 1, 1, 0, 1, 1, 0, 2, 1, 1, 7, 3, 1, 4, 1, 3, 5, 2, 2, 5, 2, 3, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] (7 :: Int) []
   % yLabel "number of queries"
   % xLabel "true positives"

m6 = subplotBarsLabelled
       [[(40 :: Double), 50, 20, 50], [10, 20, 30, 40], [40, 50, 20, 50]]
       ["a", "b", "c", "d"] []
      % addTitle "Wee a title" []
      % xLabel "X"
      % yLabel "Y"

m7 = densityBandwidth [-2.1, -1.3, -0.4, 1.9, 5.1, 6.2] 2.25 (Just (-6, 11))
