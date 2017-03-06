{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, OverlappingInstances #-}

-- Matplotlib bindings and an interface to easily bind to new portions of the
-- API. The most essential parts of Matplotlib are wrapped and exposed to
-- Haskell through an interface that allows extenisbility. Code is generated on
-- the fly and python is called.
--
-- This is not a very Haskell-ish library. Type safety is non-existent, it's
-- easy to generate incorrect Python code, in exchange for being able to bind to
-- arbitrary matplotlib APIs with ease, so it's also easy to generate correct
-- python code.
--
-- The generated code follows a few simple conventions.  data is always loaded
-- into a data variable that is a python array. Data is transffered via
-- json. This data variable is indexed by various rendering commands.
--
-- Many functions take an optional argument list.
--
-- Functions which start with the word data operate on the data array, arguments
-- are python code that should access that array. Most other functions take
-- haskell objects and load them into python.

module Graphics.Matplotlib where
import System.IO.Temp
import System.Process
import GHC.Generics
import Data.Aeson
import Control.Monad
import System.IO
import qualified Data.ByteString.Lazy as B
import Data.List
import Control.Exception

-- | A handle internal function
mapLinear :: (Double -> b) -> Double -> Double -> Double -> [b]
mapLinear f s e n = map (\v -> f $ s + (v * (e - s) / n)) [0..n]

-- | The wrapper type for a matplotlib computation. This is opaque.
data Matplotlib = Matplotlib [MplotCommand]

-- | A maplotlib command, right now we have a very shallow embedding essentially
-- dealing in opaque strings containing python code as well as the ability to
-- load data. The loaded should be a json object.
data MplotCommand
  = LoadData B.ByteString
  | Exec String
  deriving (Show, Eq, Ord)

-- | Throughout the API we need to accept options in order to expose
-- matplotlib's many configuration options.
data Option =
   -- | results in a=b
  K String String
  -- | just inserts the option verbatim as an argument at the end of the function
  | B String

toPy (LoadData _) = error "withMplot needed to load data"
toPy (Exec str)   = str

-- | The io action is given a list of python commands to execute (note that
-- these are commands in the sense of lines of python code; each inidivudal line
-- may not be parseable on its own
withMplot :: Matplotlib -> ([String] -> IO a) -> IO a
withMplot (Matplotlib cs) f = preload cs []
  where
    preload [] cmds = f $ map toPy $ reverse cmds
    preload ((LoadData obj):cs) cmds =
          withSystemTempFile "data.json"
            (\dataFile dataHandle -> do
                B.hPutStr dataHandle obj
                hClose dataHandle
                preload cs $ ((map Exec $ pyReadData dataFile) ++ cmds))
    preload (c:cs) cmds = preload cs (c:cmds)
  
--- Python operations

-- | Run python given a code string.
python codeStr =
  catch (withSystemTempFile "code.py"
         (\codeFile codeHandle -> do
             forM_ codeStr (hPutStrLn codeHandle)
             hClose codeHandle
             Right <$> readProcess "/usr/bin/python3" [codeFile] ""))
         (\e -> return $ Left $ show (e :: IOException))

pyIncludes = ["import matplotlib"
             -- TODO Provide a way to set the render backend
             -- ,"matplotlib.use('GtkAgg')"
             ,"import matplotlib.path as mpath"
             ,"import matplotlib.patches as mpatches"
             ,"import matplotlib.pyplot as plot"
             ,"import matplotlib.mlab as mlab"
             ,"from matplotlib import cm"
             ,"from mpl_toolkits.mplot3d import axes3d"
             ,"import numpy as np"
             ,"import os"
             ,"import sys"
             ,"import json"
             ,"import random, datetime"
             ,"from matplotlib.dates import DateFormatter, WeekdayLocator"]

pyReadData filename = ["data = json.loads(open('" ++ filename ++ "').read())"]

pyDetach = ["pid = os.fork()"
           ,"if(pid != 0):"
           ,"  exit(0)"]

pyOnscreen = ["plot.draw()"
             ,"plot.show()"]

pyFigure output = ["plot.savefig('" ++ output ++ "')"]

--- Running a plot

-- | Show a plot, blocks until the figure is closed
onscreen m = withMplot m (\str -> python $ pyIncludes ++ str ++ pyDetach ++ pyOnscreen)

-- | Print the python code that would be executed
code m = withMplot m (\str -> return $ Right $ unlines $ pyIncludes ++ str ++ pyDetach ++ pyOnscreen)

-- | Save to a file
figure filename m = withMplot m (\str -> python $ pyIncludes ++ str ++ pyFigure filename)

-- Creating matplotlib computations

mplot s = Matplotlib [Exec s]

infixl 5 %
-- | combine two matplotlib commands
(%) :: Matplotlib -> Matplotlib -> Matplotlib
(Matplotlib a) % (Matplotlib b) = Matplotlib (a ++ b)

infixl 6 #
-- | Values which can be combined together to form a matplotlib command. Right
-- now matplotlib commands are strings and this is just a helper to construct
-- them.
class MplotValue val where
  (#) :: String -> val -> String
instance MplotValue String where
  s # b = s ++ b
instance MplotValue [String] where
  m # [] = m
  m # (x:xs) = m # x # "," # xs
instance MplotValue Double where
  s # b = s ++ show b
instance MplotValue Int where
  s # b = s ++ show b
instance MplotValue (String, String) where
  m # (n, v) = m # n # "=" # v
instance (MplotValue x) => MplotValue (x, x) where
  m # (n, v) = m # n # "=" # v
instance (MplotValue (x, y)) => MplotValue [(x, y)] where
  m # [] = m
  m # (x:xs) = m # x # "," # xs
instance MplotValue Option where
  m # (B a) = m # a
  m # (K a b) = m # a # "=" # b
instance MplotValue [Option] where
  m # [] = m
  m # (x:xs) = m # x # "," # xs

--- Convenience & Data

-- | Plot the 'a' and 'b' entries of the data object
dataPlot :: (MplotValue val, MplotValue val1) => val1 -> val -> [Option] -> Matplotlib
dataPlot a b opts =
  mplot $ "p = plot.plot(data[" # a # "], data[" # b # "]" # options opts # ")"

-- | Plot the Haskell objects 'x' and 'y' as a line
plot :: (ToJSON t, ToJSON t1) => t1 -> t -> [Option] -> Matplotlib
plot x y opt =
  readData (x, y) % dataPlot (0::Int) (1::Int) opt

-- | Plot & show onscreen
showPlot :: (ToJSON t, ToJSON t1) => t1 -> t -> [Option] -> IO (Either String String)
showPlot x y opt = onscreen (plot x y opt)

-- | Load the given data into the 'data' array
readData :: ToJSON a => a -> Matplotlib
readData d = Matplotlib [LoadData $ encode d]

-- ;;; Options

-- | An internal helper to convert a list of options to the python code that
-- applies those options in a call.
options :: [Option] -> String
options [] = ""
options xs = "," # xs

def o [] = [o]
def o l@((B _):_) = l
def o (x:xs) = x : def o xs

-- | Show grid lines
gridLines = mplot "ax.grid(True)"

-- | Plot x against y where x is a date.
--   xunit is something like 'weeks', yearStart, monthStart, dayStart are an offset to x.
-- TODO This isn't general enough; it's missing some settings about the format. The call is also a mess.
dateLine :: (ToJSON t1, ToJSON t2) => t1 -> t2 -> String -> (Int, Int, Int) -> Matplotlib
dateLine x y xunit (yearStart, monthStart, dayStart) =
    readData (x, y)
  % mplot ("data[0] = [datetime.timedelta("#xunit#"=i) + datetime.datetime("#yearStart#","#monthStart#","#dayStart#") for i in data[0]]")
  % dataPlot (0::Int) (1::Int) [B "-"]
  % mplot "ax.xaxis.set_major_formatter(DateFormatter('%B'))"
  % mplot "ax.xaxis.set_minor_locator(WeekdayLocator(byweekday=6))"
  
-- | Add a label to the x axis
xLabel label = mplot $ "plot.xlabel('" # label # "')"

-- | Add a label to the y axis
yLabel label = mplot $ "plot.ylabel('" # label # "')"

-- | Add a label to the z axis
zLabel label = mplot $ "plot.zlabel('" # label # "')"

-- | Create a histogram for the 'a' entry of the data array
dataHistogram a bins opts = mplot $ "plot.hist(data[" # a # "]," # bins # options opts # ")"

-- | Plot a histogram for the given values with 'bins'
histogram values bins opts = readData [values] % dataHistogram (0::Int) bins opts

-- | Plot & show the histogram
showHistogram values bins opts = onscreen $ histogram values bins opts

-- | Create a scatter plot accessing the given fields of the data array
dataScatter a b opts = dataPlot a b $ def (B "'.'") opts

-- | Plot the given values as a scatter plot
scatter x y opts = plot x y $ def (B "'.'") opts

-- | Plot and show a scatter plot
showScatter x y opts = showPlot x y $ def (B "'.'") opts

-- | Create a line accessing the given entires of the data array
dataLine a b opts = dataPlot a b $ def (B "'-'") opts

-- | Plot a line
line x y opts = plot x y $ def (B "'-'") opts

-- | Plot a line given a function that will be executed for each element of
-- given list. The list provides the x values, the function the y values.
lineF f l opts = plot l (map f l) $ def (B "'-'") opts

-- | Plot and show a line
showLine x y opts = showPlot x y $ def (B "'-'") opts

-- | Create a 3D contour
contour xs ys zs =
  readData (xs, ys, zs)
  % axis3DProjection
  % surface (0::Int) (1::Int) (2::Int)
  % contourRaw (0::Int) (1::Int) (2::Int) (maximum2 xs) (maximum2 ys) (minimum2 zs)
  % axis3DLabels xs ys zs

-- | Create a 3D projection
projections xs ys zs =
  readData (xs, ys, zs)
  % axis3DProjection
  % contourRaw (0::Int) (1::Int) (2::Int) (maximum2 xs) (maximum2 ys) (minimum2 zs)
  % axis3DLabels xs ys zs

-- | Given a grid of x and y values and a number of steps call the given
-- function and plot the 3D contour
contourF f xStart xEnd yStart yEnd steps = contour xs ys zs
  where xs = mapLinear (\x -> (mapLinear (\y -> x) yStart yEnd steps)) xStart xEnd steps
        ys = mapLinear (\x -> (mapLinear (\y -> y) yStart yEnd steps)) xStart xEnd steps
        zs = mapLinear (\x -> (mapLinear (\y -> f x y) yStart yEnd steps)) xStart xEnd steps

-- | Given a grid of x and y values and a number of steps call the given
-- function and plot the 3D projection
projectionsF f xStart xEnd yStart yEnd steps = projections xs ys zs
  where xs = mapLinear (\x -> (mapLinear (\y -> x) yStart yEnd steps)) xStart xEnd steps
        ys = mapLinear (\x -> (mapLinear (\y -> y) yStart yEnd steps)) xStart xEnd steps
        zs = mapLinear (\x -> (mapLinear (\y -> f x y) yStart yEnd steps)) xStart xEnd steps

-- | Enable 3D projection
axis3DProjection = mplot $ "ax = plot.figure().gca(projection='3d')"

-- | Plot a 3D wireframe accessing the given elements of the data array
wireframe a b c = mplot $ "ax.plot_wireframe(np.array(data[" # a # "]), np.array(data[" # b # "]), np.array(data[" # c # "]), rstride=1, cstride=1)"

-- | Plot a 3D surface accessing the given elements of the data array
surface a b c = mplot $ "ax.plot_surface(np.array(data[" # a # "]), np.array(data[" # b # "]), np.array(data[" # c # "]), rstride=1, cstride=1, cmap=cm.Blues, alpha=0.3)"

-- | Plot a contour accessing the given elements of the data array
contourRaw a b c maxA maxB minC =
  mplot ("ax.contour(data[" # a # "], data[" # b # "], data[" # c # "], zdir='z', offset=" # minC # ")")
  % mplot ("ax.contour(data[" # a # "], data[" # b # "], data[" # c # "], zdir='x', offset=-" # maxA # ")")
  % mplot ("ax.contour(data[" # a # "], data[" # b # "], data[" # c # "], zdir='y', offset=" # maxB #")")

-- | Smallest element of a list of lists
minimum2 l = minimum $ minimum l
-- | Largest element of a list of lists
maximum2 l = maximum $ maximum l

-- | Label and set limits of a set of 3D axis
-- TODO This is a mess, does both more and less than it claims.
axis3DLabels xs ys zs =
  mplot "ax.set_xlabel('X')"
  % mplot ("ax.set_xlim3d(" # minimum2 xs # ", " # maximum2 xs # ")")
  % mplot "ax.set_ylabel('Y')"
  % mplot ("ax.set_ylim3d(" # minimum2 ys # ", " # maximum2 ys # ")")
  % mplot "ax.set_zlabel('Z')"
  % mplot ("ax.set_zlim3d(" # minimum2 zs # ", " # maximum2 zs # ")")

-- | Draw a bag graph in a subplot
-- TODO Why do we need this?
subplotDataBar a width offset opts =
  mplot $ "ax.bar(np.arange(len(data[" # a # "]))+" # offset # ", data[" # a # "], " # width # options opts # ")"

-- | Create a subplot with the coordinates (r,c,f)
addSubplot r c f opts = mplot $ "ax = plot.figure().add_subplot(" # r # c # f # options opts # ")"

-- | Access a subplot with the coordinates (r,c,f)
mplotSubplot r c f opts = mplot $ "ax = plot.subplot(" # r # "," # c # "," # f # options opts # ")"

-- | The default bar with
barDefaultWidth nr = 1.0 / (fromIntegral nr + 1)

-- | Create a set of labelled bars of a given height
subplotBarsLabelled valuesList labels optsList =
  subplotBars valuesList optsList
  % axisXTickSpacing (length $ head $ valuesList) ((1.0 - barDefaultWidth (length valuesList) / 2.0) :: Double) []
  % axisXTickLabels labels []

-- | Create a subplot and a set of labelled bars
-- TODO This is a mess..
subplotBars valuesList optsList =
  readData valuesList
  % addSubplot (1::Int) (1::Int) (1::Int) []
  % (let (width :: Double) = barDefaultWidth (length valuesList) in
       foldl1 (%) (zipWith3 (\vs opts i -> subplotDataBar i width (width * i) opts) valuesList optsList [0..]))

-- | Add a title
addTitle s opts = mplot $ "plot.title('" # s # options opts # "')"

-- | Set the spacing of ticks on the x axis
axisXTickSpacing nr width opts = mplot $ "ax.set_xticks(np.arange(" # nr # ")+" # width # options opts # ")"

-- | Set the labels on the x axis
axisXTickLabels labels opts = mplot $ "ax.set_xticklabels( (" # labels # ") " # options opts # " )"

-- | Update the data array to linearly interpolate between array entries
interpolate a b n =
  (mplot $ "data[" # b # "] = mlab.stineman_interp(np.linspace(data[" # a # "][0],data[" # a # "][-1]," # n # "),data[" # a # "],data[" # b # "],None)")
  % (mplot $ "data[" # a # "] = np.linspace(data[" # a # "][0],data[" # a # "][-1]," # n # ")")

-- | Plot x against y interpolating with n steps
plotInterpolated x y n opts =
  readData (x, y)
  % interpolate (0::Int) (1::Int) n
  % (dataPlot (0::Int) (1::Int) $ def (B "-") opts)

-- | Square up the aspect ratio of a plot.
squareAxes = mplot "plot.axes().set_aspect('equal')"

-- | Set the rotation of the labels on the x axis to the given number of degrees
roateAxesLabels degrees = mplot "labels = plot.axes().get_xticklabels()"
   % mplot "for label in labels:"
   % mplot ("    label.set_rotation("#degrees#")")

-- | Set the x labels to be vertical
verticalAxes = mplot "labels = plot.axes().get_xticklabels()"
   % mplot "for label in labels:"
   % mplot "    label.set_rotation('vertical')"

-- | Set the x scale to be logarithmic
logX = mplot "plot.axes().set_xscale('log')"

-- | Set the y scale to be logarithmic
logY = mplot "plot.axes().set_yscale('log')"

-- | Set limits on the x axis
xlim l u = mplot $ "plot.xlim([" # l # "," # u # "])"

-- | Set limits on the y axis
ylim l u = mplot $ "plot.ylim([" # l # "," # u # "])"

-- | A handy function to plot a line between two points give a function and a number o steps
plotMapLinear f s e n opts = line xs ys opts
  where xs = mapLinear (\x -> x) s e n
        ys = mapLinear (\x -> f x) s e n

-- | Plot a line between 0 and the length of the array with the given y values
line1 y opts = line [0..length y] y opts

matShow d opts =
  readData d
  % (mplot $ "plot.matshow(data" # options opts # ")")

-- | Plot a KDE of the given functions with an optional start/end and a bandwidth h
densityBandwidth l h maybeStartEnd =
  plotMapLinear f (case maybeStartEnd of
                    Nothing -> minimum l
                    (Just (start, _)) -> start)
                  (case maybeStartEnd of
                    Nothing -> maximum l
                    (Just (_, end)) -> end)
                  100
                  []
  where f x = sum (map (\xi -> gaussianPdf x xi h) l) / ((fromIntegral $ length l) * h)
        gaussianPdf x mu sigma = exp (- sqr (x - mu) / (2 * sigma)) / sqrt (2 * pi * sigma)
        sqr x = x * x

-- | Plot a KDE of the given functions; a good bandwith will be chosen automatically
density l maybeStartEnd =
  densityBandwidth l (((4 * (variance ** 5)) / (fromIntegral $ 3 * length l)) ** (1 / 5) / 3) maybeStartEnd
  where mean = foldl' (+) 0 l / (fromIntegral $ length l)
        variance = foldl' (+) 0 (map (\x -> sqr (x - mean)) l) / (fromIntegral $ length l)
        sqr x = x * x
