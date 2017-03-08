{-# language ExtendedDefaultRules, ScopedTypeVariables, QuasiQuotes #-}

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Unsafe
import Graphics.Matplotlib
import System.IO.Temp
import System.Random
import Text.RawString.QQ
import Data.List
import Data.List.Split

-- * Random values for testing

uniforms :: (Random a, Num a) => [a]
uniforms = randoms (mkStdGen 42)
uniforms' lo hi = randomRs (lo,hi) (mkStdGen 42)

-- * Not so random values to enable some fully-reproducible tests

xs = [-0.54571992,  1.48409716, -0.57545561,  2.13058156, -0.75740497,
      -1.27879086, -0.96008858, -1.65482373, -1.69086194, -1.41925464,
       0.68144401,  1.44847131,  1.12124327,  1.32056244, -0.4555279 ,
       1.96002923, -1.34810771,  0.01513306,  1.25298883, -1.07541677,
       0.60920278, -0.13978163,  0.3975209 , -0.15211044,  0.0534633 ,
      -0.39498474, -1.15320186,  0.6795936 ,  0.50704333,  1.52951037,
       0.90867393, -0.24833115,  1.39816295, -0.28642814,  0.96951915,
      -1.20438266, -0.32642955, -0.62982941,  0.7245042 , -1.03169685,
      -0.00542761,  0.54247125, -1.11559132, -2.6829048 , -0.13370841,
      -0.74111166,  0.59198725,  2.73711931,  1.82122485, -0.73915212,
       0.88290489, -1.17307876,  0.06753304,  0.40150672,  1.54455801,
      -0.31133243,  1.66844302, -0.1290924 ,  0.89657699,  0.41181393,
       2.13382656,  1.58577659, -1.02449069, -1.10245954, -0.59691196,
      -0.63040161, -0.51541836,  0.04139408,  0.54203055, -2.09082082,
      -0.41295376, -0.77509336,  0.47612065, -1.69680096,  0.90195265,
       0.23798858, -0.05112783,  1.00645056, -0.67012513,  0.52017487,
      -0.42251472,  0.96513844,  1.00298933,  0.18257527,  0.54599979,
      -1.50321042,  0.03949817,  0.35286613,  1.86994544,  1.16249707,
       0.57421291,  1.21151469,  1.74863421,  0.42287859, -1.22785845,
      -0.61650528,  1.76743253, -0.45818694, -1.16560907,  0.0677502 ]
ys = [ 1.28231455,  1.13480471,  0.57738712,  0.10268954,  1.00162163,
      -0.85453571, -1.61049343,  1.33194242,  0.12054943, -0.56568776,
       2.11550009,  0.03663454,  0.24889313,  0.85458325,  0.77326592,
       0.58815223, -0.79997005,  0.54979418,  0.47711544,  0.73004143,
      -0.65704545,  1.1946521 , -0.31119444, -0.0958055 ,  0.37838453,
       1.01281301, -0.53364162,  2.84609607,  0.09363483, -0.14821451,
      -0.0481863 , -3.58277731, -1.7168244 , -0.87526525, -0.65430073,
       1.0284506 , -0.81397895,  0.34868379, -0.51671293,  0.92879285,
       0.04099886,  1.0828335 ,  1.25991492, -1.48901447,  0.43657503,
       0.78191509,  0.16633587,  1.99411663, -0.25542794, -0.43377353,
      -0.82871869, -0.0402321 , -0.06278027,  0.28066445,  0.01185443,
       1.42640101, -0.16627931,  0.82021257, -0.66684095, -0.21289723,
      -1.25974667, -0.28681327, -2.11039334, -0.2722768 , -0.51622958,
       0.01324637, -0.29277708,  1.35916036, -0.09089638, -1.00619256,
       0.62707331,  1.17105748, -0.85636353, -0.6243519 ,  0.1720141 ,
      -0.15715394,  1.13488465,  2.43996937,  2.08224839, -0.23676918,
      -0.24924999,  1.21629376, -0.12748227,  0.78319565, -0.10528614,
       0.60177749, -1.03490762, -0.59163218,  0.16414076,  2.22783012,
      -0.55178235, -0.69915414,  1.35454045,  0.42931902, -1.33656935,
      -0.8023867 , -2.81354854,  0.39553427, -0.22235586, -1.34302011]

-- * Generate normally distributed random values; taken from normaldistribution==1.1.0.3

-- | Box-Muller method for generating two normally distributed
-- independent random values from two uniformly distributed
-- independent random values.
boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2

-- | Convert a list of uniformly distributed random values into a
-- list of normally distributed random values. The Box-Muller
-- algorithms converts values two at a time, so if the input list
-- has an uneven number of element the last one will be discarded.
boxMullers :: Floating a => [a] -> [a]
boxMullers (u1:u2:us) = n1:n2:boxMullers us where (n1,n2) = boxMuller u1 u2
boxMullers _          = []

-- | Plural variant of 'normal', producing an infinite list of
-- random values instead of returning a new generator. This function
-- is analogous to 'Random.randoms'.
normals = boxMullers $ randoms (mkStdGen 42)

-- | Analogous to 'normals' but uses the supplied (mean, standard
-- deviation).
normals' (mean, sigma) g = map (\x -> x * sigma + mean) $ normals

-- * Tests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- | Test one plot; right now we just test that the command executed without
-- errors. We should visually compare plots somehow.
testPlot name fn = testCase name $ tryit fn @?= Right ""
  where tryit fn = unsafePerformIO $ withSystemTempFile "a.png" (\file _ -> figure file fn)

-- | This generates examples from the test cases
testPlot' name fn = testCase name $ tryit fn name @?= Right ""
  where tryit fn name = unsafePerformIO $ do
          c <- code fn
          print c
          figure ("/tmp/imgs/" ++ name ++ ".png") fn

unitTests = testGroup "Unit tests"
  [ testPlot "histogram" m1
  , testPlot "cumulative" m2
  , testPlot "scatter" m3
  , testPlot "contour" m4
  , testPlot "labelled-histogram" m5
  -- TODO This test case is broken
  -- , testPlot "sub-bars" m6
  , testPlot "density-bandwidth" m7
  , testPlot "density" m8
  , testPlot "line-function" m9
  , testPlot "quadratic" m10
  , testPlot "projections" m11
  , testPlot "line-options" m12
  , testPlot "corr" mxcorr
  , testPlot "tex" mtex
  , testPlot "show-matrix" mmat
  , testPlot "legend" mlegend
  , testPlot "hist2DLog" mhist2DLog
  , testPlot "eventplot" meventplot
  , testPlot "errorbar" merrorbar
  , testPlot "boxplot" mboxplot
  ]

-- * These tests are fully-reproducible, the output must be identical every time

m1 :: Matplotlib
m1 = histogram xs 8

m2 :: Matplotlib
m2 = histogram xs 8 @@ [o2 "cumulative" "True"]

m3 = scatter xs ys

degreesRadians a = a * pi / 180.0
radiansDegrees a = a * 180.0 / pi

m4 = contourF (\a b -> sin (degreesRadians a) + cos (degreesRadians b)) (-100) 100 (-200) 200 10

m5 = histogram xs 7
   % yLabel "number of queries"
   % xLabel "true positives"

m6 = subplotBarsLabelled
       [[40, 50, 20, 50], [10, 20, 30, 40], [40, 50, 20, 50]]
       ["a", "b", "c", "d"] []
      % title "Wee a title"
      % xLabel "X"
      % yLabel "Y"

m7 = densityBandwidth [2.1, 1.3, 0.4, 1.9, 5.1, 6.2] 1.5 (Just (-6, 11))
  % ylim 0 0.2

m8 = density [2.1, 1.3, 0.4, 1.9, 5.1, 6.2] (Just (-6, 11))

m9 = lineF (\x -> x**2) [0,0.01..1]

m10 = plotMapLinear (\x -> x**2) (-2) 2 100 @@ [o1 "'.'"] % title "Quadratic function"

m11 = projectionsF (\a b -> cos (degreesRadians a) + sin (degreesRadians b)) (-100) 100 (-200) 200 10

m12 = plot [1,2,3,4,5,6] [1,3,2,5,2,4] @@ [o1 "'go-'", o2 "linewidth" "2"]

-- * These tests can be random and may not be exactly the same every time

-- | http://matplotlib.org/examples/pylab_examples/xcorr_demo.html
mxcorr = xacorr xs ys [o2 "usevlines" "True", o2 "maxlags" "50", o2 "normed" "True", o2 "lw" "2"]
  where (xs :: [Double]) = take 100 normals
        (ys :: [Double]) = take 100 normals

-- | http://matplotlib.org/examples/pylab_examples/tex_unicode_demo.html
mtex = plotMapLinear cos 0 1 100
  % setTeX True
  % setUnicode True
  % xLabel [r|\textbf{time (s)}|]
  % yLabel [r|\textit{Velocity (\u00B0/sec)}|] @@ [o2 "fontsize" "16"]
  % title [r|\TeX\ is Number $\displaystyle\sum_{n=1}^\infty\frac{-e^{i\pi}}{2^n}$!"|] @@ [o2 "fontsize" "16", o2 "color" "'r'"]
  % grid True

mmat = pcolor (take 10 $ chunksOf 8 uniforms) @@ [o2 "edgecolors" "'k'", o2 "linewidth" "1"]

-- | http://matplotlib.org/examples/pylab_examples/legend_demo3.html
mlegend = plotMapLinear (\x -> x ** 2) 0 1 100 @@ [o2 "label" "'x^2'"]
  % plotMapLinear (\x -> x ** 3) 0 1 100 @@ [o2 "label" "'x^3'"]
  % legend @@ [o2 "fancybox" "True", o2 "shadow" "True", o2 "title" "'Legend'", o2 "loc" "'upper left'"]

-- | http://matplotlib.org/examples/pylab_examples/hist2d_log_demo.html
mhist2DLog = histogram2D x y @@ [o2 "bins" "40", o2 "norm" "mcolors.LogNorm()"]
  % colorbar
  where (x:y:_) = chunksOf 10000 normals

meventplot = plot xs ys
  % mp # "ax.add_collection(mcollections.EventCollection(data[0], linelength=0.05))"
  % mp # "ax.add_collection(mcollections.EventCollection(data[1], orientation='vertical', linelength=0.05))"
  % text 0.1 0.6 "Ticks mark the actual data points"
  where xs = sort $ take 10 uniforms
        ys = map (\x -> x ** 2) xs

merrorbar = errorbar xs ys errs @@ [o2 "errorevery" "2"]
  where xs = [0.1,0.2..4]
        ys = map (\x -> exp $ -x) xs
        errs = map (\x -> 0.1 + 0.1 * sqrt x) xs

mboxplot = subplots @@ [o2 "ncols" "2", o2 "sharey" "True"]
  % setSubplot "0"
  % boxplot (take 3 $ chunksOf 10 $ map (* 2) $ normals) @@ [o2 "labels" "['X', 'Y', 'Z']"]
  % setSubplot "1"
  % boxplot (take 3 $ chunksOf 10 $ map (* 2) $ normals) @@ [o2 "labels" "['A', 'B', 'C']", o2 "showbox" "False", o2 "showcaps" "False"]

