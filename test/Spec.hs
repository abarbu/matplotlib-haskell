{-# language ExtendedDefaultRules, ScopedTypeVariables, QuasiQuotes, ParallelListComp #-}

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import System.IO.Unsafe
import Graphics.Matplotlib
import System.IO.Temp
import System.Random
import Text.RawString.QQ
import Data.List
import Data.List.Split
import Control.Monad
import Test.Tasty.Golden.Advanced
import qualified Data.ByteString as BS
import System.Process
import System.IO
import Numeric.AD
import System.Directory

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

pdfBivariateNormal x y sigmax sigmay mux muy sigmaxy =
  1/(2*pi*sigmax*sigmay*(sqrt(1-rho^2)))*exp(-z/(2*(1-rho^2)))
  where rho = sigmaxy/(sigmax*sigmay)
        z = (x-mux)^2/sigmax^2-(2*rho*(x-mux)*(y-muy))/(sigmax*sigmay)+(y-muy)^2/sigmay^2

-- * Tests

main = do
  createDirectoryIfMissing False "/tmp/imgs/"
  defaultMain $ tests "All tests" testPlot

main' = do
  createDirectoryIfMissing False "/tmp/imgs/"
  defaultMain $ tests "Golden tests" testPlotGolden

main'' = do
  createDirectoryIfMissing False "/tmp/imgs/"
  defaultMain $ testGroup "All tests" [tests "Execution tests" testPlot
                                      , toneDownTests "Unreliable across machines" $ tests "Golden tests" testPlotGolden]

tests name f = testGroup name   [basicTests f,
                                 toneDownTests "Can fail with old matplotlib or data" $ fragileTests f,
                                 ignoreTest $ failingTests f]

toneDownTests reason tests = wrapTest (liftM (\x -> x { resultOutcome = Success
                                                     , resultShortDescription =
                                                       case resultOutcome x of
                                                         Success -> resultShortDescription x
                                                         _ -> reason ++ ": " ++ resultShortDescription x
                                                     })) tests

testPlotGolden name fn =
         unsafePerformIO $ tmp (\filename ->
                                 return $ goldenTest
                                   name
                                   (BS.readFile ref)
                                   (file filename fn >> BS.readFile filename)
                                   (\g n ->
                                       tmp (\gfile ->
                                               tmp (\nfile -> do
                                                       BS.writeFile gfile g
                                                       BS.writeFile nfile n
                                                       (code, stdout, stderr) <-
                                                         readProcessWithExitCode "/usr/bin/compare" ["-metric"
                                                                                                    ,"PSNR"
                                                                                                    ,gfile
                                                                                                    ,nfile
                                                                                                    ,"null"] ""
                                                       case (stderr, reads stderr) of
                                                         ("inf", _) -> return Nothing
                                                         (_, [(x :: Double, _)]) ->
                                                           if x < 25 then
                                                             return $ Just $ "Images very different; PSNR too low " ++ show x else
                                                             return Nothing)))
                                   (BS.writeFile ref))
  where ref = "imgs/" ++ name ++ ".png"
        tmp f = withSystemTempFile "a.png" (\filename h -> hClose h >> f filename)

-- | Test one plot; right now we just test that the command executed without
-- errors. We should visually compare plots somehow.
testPlot' name fn = testCase name $ tryit fn @?= Right ""
  where tryit fn = unsafePerformIO $ withSystemTempFile "a.png" (\filename _ -> file filename fn)

-- | This generates examples from the test cases
testPlot name fn = testCase name $ tryit fn name @?= Right ""
  where tryit fn name = unsafePerformIO $ do
          file ("/tmp/imgs/" ++ name ++ ".png") fn

basicTests f = testGroup "Basic tests"
  [ f "histogram" mhistogram
  , f "cumulative" mcumulative
  , f "scatter" mscatter
  , f "contour" mcontour
  , f "labelled-histogram" mlabelledHistogram
  , f "density-bandwidth" mdensityBandwidth
  , f "density" mdensity
  , f "line-function" mlineFunction
  , f "quadratic" mQuadratic
  , f "projections" mProjections
  , f "line-options" mlineOptions
  , f "corr" mxcorr
  , f "show-matrix" mmat
  , f "legend" mlegend
  , f "hist2DLog" mhist2DLog
  , f "eventplot" meventplot
  , f "errorbar" merrorbar
  , f "scatterhist" mscatterHist
  , f "histMulti" mhistMulti
  , f "spines" mspines
  , f "hists" mhists
  , f "hinton" mhinton
  , f "integral" mintegral
  , f "quiver" mquiver
  , f "quiver-fancy" mquiverFancy
  , f "polar" mpolar
  , f "bivariateNormal" mbivariateNormal
  , f "pcolorlog" mpcolorlog
  , f "pie" mpie
  , f "stacked" mstacked
  , f "annotation" mannotation
  , f "streamplot" mstreamplot
  ]

fragileTests f = testGroup "Fragile tests"
  [ f "tex" mtex -- TODO Fails on circle ci (with latex)
    -- TODO Fails on circle ci (labels is not valid; matplotlib too old)
  , f "boxplot" mboxplot
    -- TODO Fails on circle ci (no violin plots; matplotlib too old)
  , f "violinplot" mviolinplot
    -- TODO Needs a fairly recent matplotlib; too old for circleci
  , f "griddata" mgriddata
    -- TODO Needs access to a data file
  , f "images" mimages
  ]

failingTests f = testGroup "Failing tests"
  [
    -- TODO This test case is broken
    f "sub-bars" msubBars
  ]

-- * These tests are fully-reproducible, the output must be identical every time

mhistogram :: Matplotlib
mhistogram = histogram xs 8

mcumulative = histogram xs 8 @@ [o2 "cumulative" True]

mscatter = scatter xs ys

degreesRadians a = a * pi / 180.0
radiansDegrees a = a * 180.0 / pi

mcontour = contourF (\a b -> sin (degreesRadians a) + cos (degreesRadians b)) (-100) 100 (-200) 200 10

mlabelledHistogram = histogram xs 7
   % ylabel "number of queries"
   % xlabel "true positives"

-- TODO Broken test
msubBars = subplotBarsLabelled
       [[40, 50, 20, 50], [10, 20, 30, 40], [40, 50, 20, 50]]
       ["a", "b", "c", "d"] []
      % title "Wee a title"
      % xlabel "X"
      % ylabel "Y"

mdensityBandwidth = densityBandwidth [2.1, 1.3, 0.4, 1.9, 5.1, 6.2] 1.5 (Just (-6, 11))
  % ylim 0 0.2

mdensity = density [2.1, 1.3, 0.4, 1.9, 5.1, 6.2] (Just (-6, 11))

mlineFunction = lineF (\x -> x**2) [0,0.01..1]

mQuadratic = plotMapLinear (\x -> x**2) (-2) 2 100 @@ [o1 "."] % title "Quadratic function"

mProjections = projectionsF (\a b -> cos (degreesRadians a) + sin (degreesRadians b)) (-100) 100 (-200) 200 10

mlineOptions = plot [1,2,3,4,5,6] [1,3,2,5,2,4] @@ [o1 "go-", o2 "linewidth" 2]

-- * These tests can be random and may not be exactly the same every time

-- | http://matplotlib.org/examples/pylab_examples/xcorr_demo.html
mxcorr = xacorr xs ys [o2 "usevlines" True, o2 "maxlags" 50, o2 "normed" True, o2 "lw" 2]
  where (xs :: [Double]) = take 100 normals
        (ys :: [Double]) = take 100 normals

-- | http://matplotlib.org/examples/pylab_examples/tex_unicode_demo.html
mtex = setTeX True
  % figure
  % addSubplot 1 1 1
  % plotMapLinear cos 0 1 100
  % xlabel [r|\textbf{time (s)}|]
  % ylabel "\\textit{Velocity (°/sec)}" @@ [o2 "fontsize" 16]
  % title [r|\TeX\ is Number $\displaystyle\sum_{n=1}^\infty\frac{-e^{i\pi}}{2^n}$!"|] @@ [o2 "fontsize" 16, o2 "color" "r"]
  % grid True

mmat = pcolor (take 10 $ chunksOf 8 uniforms) @@ [o2 "edgecolors" "k", o2 "linewidth" 1]

-- | http://matplotlib.org/examples/pylab_examples/legend_demo3.html
mlegend = plotMapLinear (\x -> x ** 2) 0 1 100 @@ [o2 "label" "x^2"]
  % plotMapLinear (\x -> x ** 3) 0 1 100 @@ [o2 "label" "x^3"]
  % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Legend", o2 "loc" "upper left"]

-- | http://matplotlib.org/examples/pylab_examples/hist2d_log_demo.html
mhist2DLog = histogram2D x y @@ [o2 "bins" 40, o2 "norm" $ lit "mcolors.LogNorm()"]
  % setAx
  % colorbar
  where (x:y:_) = chunksOf 10000 normals

meventplot = plot xs ys
  % mp # "ax.add_collection(mcollections.EventCollection(data[0], linelength=0.05))"
  % mp # "ax.add_collection(mcollections.EventCollection(data[1], orientation='vertical', linelength=0.05))"
  % text 0.1 0.6 "Ticks mark the actual data points"
  where xs = sort $ take 10 uniforms
        ys = map (\x -> x ** 2) xs

merrorbar = errorbar xs ys (Nothing :: Maybe [Double]) (Just errs) @@ [o2 "errorevery" 2]
  where xs = [0.1,0.2..4]
        ys = map (\x -> exp $ -x) xs
        errs = [map (\x -> 0.1 + 0.1 * sqrt x) xs, map (\x -> 0.1 + 0.1 * sqrt x) ys]

mboxplot = subplots @@ [o2 "ncols" 2, o2 "sharey" True]
  % setSubplot "0"
  % boxplot (take 3 $ chunksOf 10 $ map (* 2) $ normals) @@ [o2 "labels" ["X", "Y", "Z"]]
  % setSubplot "1"
  % boxplot (take 3 $ chunksOf 10 $ map (* 2) $ normals) @@ [o2 "labels" ["A", "B", "C"], o2 "showbox" False, o2 "showcaps" False]

mviolinplot = subplots @@ [o2 "ncols" 2, o2 "sharey" True]
  % setSubplot "0"
  % violinplot (take 3 $ chunksOf 100 $ map (* 2) $ normals)
  % setSubplot "1"
  % violinplot (take 3 $ chunksOf 100 $ map (* 2) $ normals) @@ [o2 "showmeans" True, o2 "showmedians" True, o2 "vert" False]

-- | http://matplotlib.org/examples/pylab_examples/scatter_hist.html
mscatterHist = figure @@ [o1 0]
  % setSizeInches 8 8
  -- The scatter plot
  % axes @@ [o1 ([left, bottom', width, height] :: [Double])]
  % scatter  x y
  % xlim (-lim) lim
  % ylim (-lim) lim
  -- The histogram on the right (x)
  % axes @@ [o1 [left, bottom_h, width, 0.2]]
  % mp # "ax.xaxis.set_major_formatter(mticker.NullFormatter())"
  % histogram x bins
  % xlim (-lim) lim
  -- The histogram on top (y)
  % axes @@ [o1 [left_h, bottom', 0.2, height]]
  % mp # "ax.yaxis.set_major_formatter(mticker.NullFormatter())"
  % histogram y bins @@ [o2 "orientation" "horizontal"]
  % ylim (-lim) lim
  where left = 0.1
        width = 0.65
        bottom' = 0.1
        height = 0.65
        bottom_h = left + width + 0.02
        left_h = left + width + 0.02
        [x, y] = take 2 $ chunksOf 1000 $ map (* 2) $ normals
        binwidth = 0.25
        xymax = maximum [maximum $ map abs x, maximum $ map abs y]
        lim = ((fromIntegral $ round $ xymax / binwidth) + 1) * binwidth
        bins = [-lim,-lim+binwidth..(lim + binwidth)]

mhistMulti = subplots @@ [o2 "nrows" 2, o2 "ncols" 2]
  % setSubplot 0
  % histogram x nrBins @@ [o2 "density" 1, o2 "histtype" "bar", o2 "color" ["red", "tan", "lime"], o2 "label" ["red", "tan", "lime"]]
  % legend @@ [o2 "prop" $ lit "{'size': 10}"]
  % title "bars with legend"
  % setSubplot 1
  % histogram x nrBins @@ [o2 "density" 1, o2 "histtype" "bar", o2 "stacked" True]
  % title "stacked bar"
  % setSubplot 2
  % histogram x nrBins @@ [o2 "histtype" "step", o2 "stacked" True, o2 "fill" False]
  % title "stacked bar"
  % setSubplot 3
  % histogram (map (\x -> take x normals) [2000, 5000, 10000]) nrBins @@ [o2 "histtype" "bar"]
  % title "different sample sizes"
  % tightLayout
  where nrBins = 10
        x = take 3 $ chunksOf 1000 $ normals

mspines = plot x y @@ [o1 "k--"]
  % plot x y' @@ [o1 "ro"]
  % xlim 0 (2 * pi)
  % xticks [0 :: Double, pi, 2*pi]
  % xtickLabels (map raw ["0", "$\\pi$", "2$\\pi$"])
  % ylim (-1.5) 1.5
  % yticks [-1 :: Double, 0, 1]
  % spine "left"
  % spineSetBounds (-1) 1
  % spine "right"
  % spineSetVisible False
  % spine "top"
  % spineSetVisible False
  % axisYTicksPosition "left"
  % axisXTicksPosition "bottom"
  where x  = mapLinear (\x -> x) 0 (2 * pi) 50
        y  = map sin x
        y' = zipWith (\a b -> a + 0.1*b) y normals

mhists = h 10 1.5
       % h 4 1
       % h 15 2
       % h 6 0.5
  where ns mu var = map (\x -> mu + x * var) $ take 1000 normals
        h mu var = histogram (ns mu var) 25 @@ [o2 "histtype" "stepfilled"
                                             ,o2 "alpha" 0.8
                                             ,o2 "density" True]

mhinton = mp # "ax.patch.set_facecolor('gray')"
  % setAspect @@ [o1 "equal", o1 "box"]
  % mp # "ax.xaxis.set_major_locator(plot.NullLocator())"
  % mp # "ax.yaxis.set_major_locator(plot.NullLocator())"
  % foldl (\a (x,y,w) -> a % f x y w) mp m
  % mp # "ax.autoscale_view()"
  % mp # "ax.invert_yaxis()"
  where m = [ (x,y,w) | x <- [0..19], y <- [0..19] | w <- (map (\x -> x - 0.5) normals) ]
        maxWeight = maximum $ map (\(_,_,v) -> abs v) m
        f x y w = mp # "ax.add_patch(plot.Rectangle("
                     # "[" # (x - size / 2) # "," # (y - size / 2) # "]"
                     # ", " # size # ", " # size
                     # ", facecolor='" # color # "', edgecolor='" # color # "'))"
          where color = if w > 0 then "white" else "black"
                size  = sqrt $ abs w / maxWeight

mintegral = subplots
  % plot x y @@ [o1 "r", o2 "linewidth" 2]
  % ylim 0 (maximum y)
  % mp # "ax.add_patch(plot.Polygon(" # ([(a, 0)] ++ zip ix iy ++ [(b,0)]) ## "))"
    @@ [o2 "facecolor" "0.9", o2 "edgecolor" "0.5"]
  % text (0.5 * (a + b)) 30 [r|$\int_a^b f(x)\mathrm{d}x$|]
    @@ [o2 "horizontalalignment" "center", o2 "fontsize" 20]
  % figText 0.9 0.05 "$x$"
  % figText 0.1 0.9  "$y$"
  % spine "right"
  % spineSetVisible False
  % spine "top"
  % spineSetVisible False
  % axisXTicksPosition "bottom"
  % xticks (a, b)
  % xtickLabels (raw "$a$", raw "$b$")
  % yticks ([] :: [Double])
  where func x = (x - 3) * (x - 5) * (x - 7) + 85
        -- integral limits
        a = 2 :: Double
        b = 9 :: Double
        (x :: [Double]) = mapLinear (\x -> x) 0 10 100
        y = map func x
        -- shaded region
        (ix :: [Double]) = mapLinear (\x -> x) a b 100
        iy = map func ix

mquiver = quiver x y u v (Nothing :: Maybe [Double]) @@ [o2 "units" "width"]
  % quiverKey 0.9 0.93 2 (raw [r|$2 \frac{m}{s}$|])
    @@ [o2 "labelpos" "E", o2 "coordinates" "figure"]
  % xlim (-0.2) 6.4
  % ylim (-0.2) 6.4
  where m = [ (x,y,cos x,sin y) | x <- [0,0.2..2*pi], y <- [0,0.2..2*pi] ]
        x = map (\(x,_,_,_) -> x) m
        y = map (\(_,x,_,_) -> x) m
        u = map (\(_,_,x,_) -> x) m
        v = map (\(_,_,_,x) -> x) m

mquiverFancy = quiver x y u v (Just mag) @@ [o2 "units" "x"
                                            ,o2 "pivot" "tip"
                                            ,o2 "width" 0.022
                                            ,o2 "scale" (1 / 0.15)]
  % quiverKey 0.9 0.93 1 (raw [r|$2 \frac{m}{s}$|])
    @@ [o2 "labelpos" "E", o2 "coordinates" "figure"]
  % scatter x y @@ [o2 "color" "k", o2 "s" 5]
  % xlim (-0.2) 6.4
  % ylim (-0.2) 6.4
  where m = [ (x,y,cos x,sin y) | x <- [0,0.2..2*pi], y <- [0,0.2..2*pi] ]
        x = map (\(x,_,_,_) -> x) m
        y = map (\(_,x,_,_) -> x) m
        u = map (\(_,_,x,_) -> x) m
        v = map (\(_,_,_,x) -> x) m
        mag = zipWith (\x x' -> sqrt(x**2 + x'**2)) u v

mpolar = rc "grid" @@ [o2 "color" "#316931", o2 "linewidth" 1, o2 "linestyle" "-"]
  % rc "xtick" @@ [o2 "labelsize" 15]
  % rc "ytick" @@ [o2 "labelsize" 15]
  % figure @@ [o2 "figsize" (8::Int,8::Int)]
  % addAxes @@ [o1 [0.1, 0.1, 0.8, 0.8::Double], o2 "projection" "polar"
               -- TODO My matplotlib doesn't seem to have this property
               -- , o2 "facecolor" "#d5de9c"
               ]
  % plot theta r @@ [o2 "color" "#ee8d18", o2 "lw" 3, o2 "label" "a line"]
  % plot (map (\x -> 0.5*x) theta) r
         @@ [o2 "color" "blue", o2 "ls" "--", o2 "lw" 3, o2 "label" "another line"]
  % legend
  where r = [0,0.01..3.0]
        theta = map (\x -> 2*pi*x) r

mbivariateNormal =
  imshow vs @@ [o2 "interpolation" "bilinear"
               ,o2 "cmap" $ raw "RdYlGn"
               ,o2 "origin" "lower"
               ,o2 "extent" [-3::Double, 3, -3, 3]
               ,o2 "vmin" $ (0-) $ maximum $ map abs vs'
               ,o2 "vmax" $ maximum $ map abs vs']
  where delta = 0.025::Double
        xs = [-3.0,-3.0+delta..3.0]
        ys = [-3.0,-3.0+delta..3.0]
        vs = [[pdfBivariateNormal x y 1.5 0.5 1.0 1.0 0.0
               - pdfBivariateNormal x y 1.0 1.0 0.0 0.0 0.0
              | x <- xs]
              | y <- ys]
        vs' = foldl' (++) [] vs

-- TODO This is subtly broken
mimages = -- figure @@ [o2 "figsize" (10::Int,10::Int)]
  -- %
  subplots @@ [o2 "nrows" 1, o2 "ncols" 2]
  % setSubplot 0
  % imshow "data/heightmap.png" @@ [o2 "interpolation" "nearest"]
  % setSubplot 1
  % mp # "ls = mcolors.LightSource(azdeg=315, altdeg=45)"
  % mp # "ax.imshow(ls.shade(img, cmap=cm.gist_earth))"
  -- TODO This doesn't work on my matplab version
  -- vert_exag=0.05,
  --, blend_mode='overlay'
  % xlabel "overlay blend mode"

mpcolorlog = figure
  % addSubplot 2 1 1
  % pcolor3 xs' ys' vs @@ [o2 "cmap" $ raw "PuBu_r"]
  % colorbar
  % addSubplot 2 1 2
  % pcolor3 xs' ys' vs @@ [o2 "norm"
                            (lit $ "mcolors.LogNorm(vmin="++(show $ minimum vs')++
                              ", vmax="++(show $ maximum vs')++")")
                          ,o2 "cmap" $ raw "PuBu_r"]
  % colorbar
  where delta = 0.1::Double
        xs = [-3.0,-3.0+delta..3.0]
        ys = [-3.0,-3.0+delta..3.0]
        vs = [[pdfBivariateNormal x y 0.1 0.2 1.0 1.0 0.0
               + 0.1 * pdfBivariateNormal x y 1.0 1.0 0.0 0.0 0.0
              | x <- xs]
              | y <- ys]
        xs' = [[ x | x <- xs]| y <- ys]
        ys' = [[ y | x <- xs]| y <- ys]
        vs' = foldl' (++) [] vs

mpie = pie [15, 30, 45, 10 :: Double]
  @@ [o2 "explode" [0, 0.05, 0, 0 :: Double]
     ,o2 "labels" ["Frogs", "Hogs", "Dogs", "Logs"]
     ,o2 "autopct" "%.0f%%"
     ,o2 "shadow" True]

-- | http://matplotlib.org/examples/pylab_examples/bar_stacked.html
mstacked =
  -- TODO The locations of the bars is off
    bar [0..4] ms @@ [o1 width, o2 "color" "#d62728", o2 "yerr" mStd, o2 "label" "ms"]
  % bar [0..4] ws @@ [o1 width, o2 "bottom" ms,       o2 "yerr" wStd, o2 "label" "ws"]
  % xticks [0..4 :: Int]
  % xtickLabels "['G1', 'G2', 'G3', 'G4', 'G5']"
  % title "Scores"
  % ylabel "Score"
  % yticks [0,10..80 :: Int]
  % legend
  where ms    = [20 :: Double, 35, 30, 35, 27]
        ws    = [25 :: Double, 32, 34, 20, 25]
        mStd  = [2 :: Double, 3, 4, 1, 2]
        wStd  = [3 :: Double, 5, 2, 3, 3]
        width = 0.35 :: Double

mannotation = -- figure @@ [o2 "figsize" (10::Int,10::Int)]
  -- TODO This is subtly broken
  -- TODO Dictionaries
    plot t s @@ [o2 "lw" 3]
  % xlim (-1) 5
  % ylim (-4) 3
  % annotate "straight" @@ [o2 "xy" [0, 1::Double], o2 "xycoords" "data", o2 "xytext" [-50, 30 :: Double]
                           ,o2 "textcoords" "offset points", o2 "arrowprops" (lit "dict(arrowstyle='->')")]
  % annotate "arc3,\\nrad 0.2" @@ [o2 "xy" [0.5, -1::Double], o2 "xycoords" "data", o2 "xytext" [-80, -60 :: Double]
                                 ,o2 "textcoords" "offset points"
                                 ,o2 "arrowprops" (lit "dict(arrowstyle='->', connectionstyle='arc3,rad=.2')")]
  % annotate "arc,\\nangle 50" @@ [o2 "xy" [1, 1::Double], o2 "xycoords" "data", o2 "xytext" [-90, 50 :: Double]
                                  ,o2 "textcoords" "offset points"
                                  ,o2 "arrowprops" (lit "dict(arrowstyle='->', connectionstyle='arc,angleA=0,armA=50,rad=10')")]
  % annotate "arc,\\narms" @@ [o2 "xy" [1.5, -1::Double], o2 "xycoords" "data", o2 "xytext" [-80, -60 :: Double]
                              ,o2 "textcoords" "offset points"
                              ,o2 "arrowprops" (lit "dict(arrowstyle='->', connectionstyle='arc,angleA=0,armA=40,angleB=-90,armB=30,rad=7')")]
  % annotate "angle,\\nangle 90" @@ [o2 "xy" [2, 1::Double], o2 "xycoords" "data", o2 "xytext" [-70, 30 :: Double]
                   ,o2 "textcoords" "offset points"
                   ,o2 "arrowprops" (lit "dict(arrowstyle='->', connectionstyle='angle,angleA=0,angleB=90,rad=10')")]
  % annotate "angle3,\\nangle -90" @@ [o2 "xy" [2.5, -1::Double], o2 "xycoords" "data", o2 "xytext" [-80, -60 :: Double]
                   ,o2 "textcoords" "offset points"
                   ,o2 "arrowprops" (lit "dict(arrowstyle='->', connectionstyle='angle3,angleA=0,angleB=-90')")]
  % annotate "angle,\\nround" @@ [o2 "xy" [3, 1::Double], o2 "xycoords" "data", o2 "xytext" [-60, 30 :: Double]
                   ,o2 "textcoords" "offset points"
                   ,o2 "bbox" (lit "dict(boxstyle='round', fc='0.8')")
                   ,o2 "arrowprops" (lit "dict(arrowstyle='->', connectionstyle='angle,angleA=0,angleB=90,rad=10')")]
  % annotate "angle,\\nround4" @@ [o2 "xy" [3.5, -1::Double], o2 "xycoords" "data", o2 "xytext" [-70, -80 :: Double]
                   ,o2 "textcoords" "offset points"
                   ,o2 "bbox" (lit "dict(boxstyle='round4,pad=.5', fc='0.8')")
                   ,o2 "size" 20
                   ,o2 "arrowprops" (lit "dict(arrowstyle='->', connectionstyle='angle,angleA=0,angleB=-90,rad=10')")]
  % annotate "angle,\\nshrink" @@ [o2 "xy" [4, 1::Double], o2 "xycoords" "data", o2 "xytext" [-60, 30 :: Double]
                   ,o2 "textcoords" "offset points"
                   ,o2 "bbox" (lit "dict(boxstyle='round', fc='0.8')")
                   ,o2 "arrowprops" (lit "dict(arrowstyle='->', connectionstyle='angle,angleA=0,angleB=90,rad=10')")]
  -- TODO This annotation doesn't render correctly on my matplotlib version
  % annotate "" @@ [o2 "xy" [4, 1::Double], o2 "xycoords" "data", o2 "xytext" [4.5, -1 :: Double]
                   ,o2 "textcoords" "offset points"
                   ,o2 "arrowprops" (lit "dict(arrowstyle='<->', connectionstyle='bar', ec='k', shrinkA=5, shrinkB=5)")]
  where t = [0, 0.01 .. 5.0 :: Double]
        s = map (\x -> cos $ 2*pi*x) t

mstreamplot = streamplot xs ys xs' ys' @@ [o2 "linewidth" mag']
  -- useful for seeing the energy landscape
  -- pcolor3 xmat ymat vs
  where delta = 0.05::Double
        xs = [-3.0,-3.0+delta..3.0]
        ys = [-3.0,-3.0+delta..3.0]
        xmat = [[ x | x <- xs]| y <- ys]
        ymat = [[ y | x <- xs]| y <- ys]
        ms = [[grad' (\[xv,yv] ->
                         pdfBivariateNormal xv yv 0.4 0.7 1.0 1.0 0.0
                         + pdfBivariateNormal xv yv 1.0 1.0 (-1.0) (-1.0) 0.0)
                [x,y]
              | x <- xs]
             | y <- ys] :: [[(Double, [Double])]]
        vs  = map2 (\(v, _)      -> v) ms
        xs' = map2 (\(_, [x, _]) -> x) ms
        ys' = map2 (\(_, [_, y]) -> y) ms
        mag' = zipWith (\lx ly -> zipWith (\x y -> 5 * (log $ 1 + (sqrt $ x*x + y*y))) lx ly) xs' ys'
        map2 f l = map (\r -> map f r) l

mgriddata = readData (x, y, z, xi, yi)
  -- TODO This requires a lot of manual indexing. Next big API change will be to
  -- have references to loaded data.
  % mp # "data.append(interpolate.griddata((data[0], data[1]), data[2], tuple(np.meshgrid(data[3], data[4])), method='cubic', rescale=True))"
  % mp # "plot.sci(ax.contour(data[3], data[4], data[5], 15, linewidths=0.5, colors='k'))"
  % mp # "plot.sci(ax.contourf(data[3], data[4], data[5], 15, vmax=abs(data[5]).max(), vmin=-abs(data[5]).max()))"
  % colorbar
  % scatter x y @@ [o2 "marker" "o", o2 "s" 5, o2 "zorder" 10]
  % xlim (-2) 2
  % ylim (-2) 2
  % title "Grid interpolation"
  where [x, y] = take 2 $ chunksOf 200 $ map (\x -> 4 * (x - 0.5)) $ uniforms
        z = zipWith (\x y -> x*(exp $ -(x**2) - y**2)) x y
        xi = mapLinear (\x -> x) (-2.1) 2.1 300
        yi = mapLinear (\x -> x) (-2.1) 2.1 300
