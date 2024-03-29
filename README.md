![matplotlib contour plot](https://github.com/abarbu/matplotlib-haskell/raw/master/imgs/contour.png)

# Matplotlib for Haskell

[![Build Status](https://github.com/abarbu/matplotlib-haskell/actions/workflows/CI.yaml/badge.svg)](https://github.com/abarbu/matplotlib-haskell/actions/workflows/CI.yaml)
[![Hackage](https://img.shields.io/hackage/v/matplotlib.svg)](https://hackage.haskell.org/package/matplotlib)

Haskell bindings to Python's Matplotlib. It's high time that Haskell had a
fully-fledged plotting library! Examples below.

[Documentation is available on Hackage](https://hackage.haskell.org/package/matplotlib).

In GHCi:

```haskell
:set -XExtendedDefaultRules
import Graphics.Matplotlib
onscreen $ contourF (\a b -> sin (a*pi/180.0) + cos (b*pi/180.0)) (-100) 100 (-200) 200 10
```

Or in a standalone file

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

import Graphics.Matplotlib

main = onscreen $ contourF (\a b -> sin (a*pi/180.0) + cos (degreesRadians b)) (-100) 100 (-200) 200 10
```

We need `-XExtendedDefaultRules` to avoid having to manually having to specify certain types.

### Installation

You will need several python libraries to run this code which can be installed
on Ubuntu machines with the following command:

```bash
sudo apt-get install -y python3 python3-pip python3-matplotlib python3-numpy python3-tk python-mpltoolkits.basemap python3-scipy dvipng
```

If you're using conda

```bash
conda install -y matplotlib scipy tk
```

If you have instructions for other machines or OSes let me know. We require the
`python3` binary to be available somewhere in the PATH. We run with 
`env python3`.

Once you have the prerequisites you can install using the standard incantation

```bash
stack install matplotlib
```

If you use LaTeX markup you will need the requisite packages

```bash
sudo apt-get install -y texlive-full
```

Or with conda

```bash
conda install -y -c conda-forge texlive-core
```

### Examples

Click on any of the examples below to go to the corresponding test that
generates it. Depending on your matplotlib version default colors might be
different.

[![integral][img_integral]][url_integral]
[![griddata][img_griddata]][url_griddata]
[![streamplot][img_streamplot]][url_streamplot]
[![hist2DLog][img_hist2DLog]][url_hist2DLog]
[![quadratic][img_quadratic]][url_quadratic]
[![spines][img_spines]][url_spines]
[![annotation][img_annotation]][url_annotation]
[![corr][img_corr]][url_corr]
[![bivariateNormal][img_bivariateNormal]][url_bivariateNormal]
[![images][img_images]][url_images]
[![labelled-histogram][img_labelled-histogram]][url_labelled-histogram]
[![projections][img_projections]][url_projections]
[![histogram][img_histogram]][url_histogram]
[![pcolorlog][img_pcolorlog]][url_pcolorlog]
[![scatter][img_scatter]][url_scatter]
[![stacked][img_stacked]][url_stacked]
[![legend][img_legend]][url_legend]
[![errorbar][img_errorbar]][url_errorbar]
[![line-options][img_line-options]][url_line-options]
[![quiver-fancy][img_quiver-fancy]][url_quiver-fancy]
[![contour][img_contour]][url_contour]
[![boxplot][img_boxplot]][url_boxplot]
[![show-matrix][img_show-matrix]][url_show-matrix]
[![scatterhist][img_scatterhist]][url_scatterhist]
[![hinton][img_hinton]][url_hinton]
[![density][img_density]][url_density]
[![violinplot][img_violinplot]][url_violinplot]
[![histMulti][img_histMulti]][url_histMulti]
[![cumulative][img_cumulative]][url_cumulative]
[![polar][img_polar]][url_polar]
[![hists][img_hists]][url_hists]
[![tex][img_tex]][url_tex]
[![eventplot][img_eventplot]][url_eventplot]
[![line-function][img_line-function]][url_line-function]
[![density-bandwidth][img_density-bandwidth]][url_density-bandwidth]
[![quiver][img_quiver]][url_quiver]
[![pie][img_pie]][url_pie]

[img_violinplot]: https://i.imgur.com/iBOfnuL.png "violinplot"
[url_violinplot]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L299
[img_contour]: https://i.imgur.com/KoAIf9Z.png "contour"
[url_contour]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L223
[img_tex]: https://i.imgur.com/bR8r579.png "tex"
[url_tex]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L258
[img_scatterhist]: https://i.imgur.com/9ZIVotE.png "scatterhist"
[url_scatterhist]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L306
[img_line-options]: https://i.imgur.com/Fahp7QA.png "line-options"
[url_line-options]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L248
[img_griddata]: https://i.imgur.com/SH83pJK.png "griddata"
[url_griddata]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L603
[img_pcolorlog]: https://i.imgur.com/ZLUoUqy.png "pcolorlog"
[url_pcolorlog]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L496
[img_cumulative]: https://i.imgur.com/u5I8NYF.png "cumulative"
[url_cumulative]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L216
[img_annotation]: https://i.imgur.com/9tdHiaT.png "annotation"
[url_annotation]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L540
[img_density]: https://i.imgur.com/KS2OhbH.png "density"
[url_density]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L240
[img_line-function]: https://i.imgur.com/zkpfQqW.png "line-function"
[url_line-function]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L242
[img_boxplot]: https://i.imgur.com/KigvYSc.png "boxplot"
[url_boxplot]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L293
[img_show-matrix]: https://i.imgur.com/ajY0A9l.png "show-matrix"
[url_show-matrix]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L268
[img_histMulti]: https://i.imgur.com/FxEI3EI.png "histMulti"
[url_histMulti]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L335
[img_streamplot]: https://i.imgur.com/IfHLmkC.png "streamplot"
[url_streamplot]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L583
[img_pie]: https://i.imgur.com/ljgWXf6.png "pie"
[url_pie]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L517
[img_corr]: https://i.imgur.com/GnBpDJL.png "corr"
[url_corr]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L253
[img_projections]: https://i.imgur.com/IlK7Oy3.png "projections"
[url_projections]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L246
[img_scatter]: https://i.imgur.com/dceKS4I.png "scatter"
[url_scatter]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L218
[img_legend]: https://i.imgur.com/X46KiUJ.png "legend"
[url_legend]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L271
[img_density-bandwidth]: https://i.imgur.com/Qgjvrox.png "density-bandwidth"
[url_density-bandwidth]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L237
[img_bivariateNormal]: https://i.imgur.com/fTSfEzo.png "bivariateNormal"
[url_bivariateNormal]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L466
[img_hinton]: https://i.imgur.com/m9a4IwL.png "hinton"
[url_hinton]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L381
[img_quadratic]: https://i.imgur.com/E4AafPD.png "quadratic"
[url_quadratic]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L244
[img_histogram]: https://i.imgur.com/X37Rmy4.png "histogram"
[url_histogram]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L213
[img_polar]: https://i.imgur.com/4DAOrF1.png "polar"
[url_polar]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L451
[img_quiver]: https://i.imgur.com/TcayDLc.png "quiver"
[url_quiver]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L424
[img_quiver-fancy]: https://i.imgur.com/NsOFHhx.png "quiver-fancy"
[url_quiver-fancy]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L435
[img_stacked]: https://i.imgur.com/rWIyizX.png "stacked"
[url_stacked]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L524
[img_spines]: https://i.imgur.com/BryQOY9.png "spines"
[url_spines]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L353
[img_hist2DLog]: https://i.imgur.com/2fL8oEX.png "hist2DLog"
[url_hist2DLog]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L276
[img_integral]: https://i.imgur.com/PkepIKR.png "integral"
[url_integral]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L397
[img_errorbar]: https://i.imgur.com/gi0zEiz.png "errorbar"
[url_errorbar]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L288
[img_labelled-histogram]: https://i.imgur.com/lCVEpge.png "labelled-histogram"
[url_labelled-histogram]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L225
[img_eventplot]: https://i.imgur.com/UMT1yku.png "eventplot"
[url_eventplot]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L281
[img_hists]: https://i.imgur.com/KurE2Sr.png "hists"
[url_hists]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L372
[img_images]: https://i.imgur.com/R1fhDXC.png "images"
[url_images]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L483
