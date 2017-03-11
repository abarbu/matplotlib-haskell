# Matplotlib

[![Build Status](https://img.shields.io/circleci/project/github/abarbu/matplotlib-haskell.svg)](circleci.com/gh/abarbu/matplotlib-haskell)
[![Hackage](https://img.shields.io/hackage/v/matplotlib.svg)](https://hackage.haskell.org/package/matplotlib)

Haskell bindings to Python's Matplotlib. It's high time that Haskell had a
fully-fledged plotting library!

![matplotlib contour plot](https://github.com/abarbu/matplotlib-haskell/raw/master/imgs/contour.png)

[Documentation is available on Hackage](https://hackage.haskell.org/package/matplotlib). For
more examples see the tests.

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}
import Matplotlib

onscreen $ contourF (\a b -> sin (degreesRadians a) + cos (degreesRadians b)) (-100) 100 (-200) 200 10
```

We need `-XExtendedDefaultRules` to avoid having to manually having to specify certain types.

### Installation

You will need several python libraries to run this code which can be installed
on Ubuntu machines with the following command:

```bash
sudo apt-get install -y python3-pip python3-matplotlib python3-numpy python-mpltoolkits.basemap
```

If you have instructions for other machines or OSes let me know. We require
`/usr/bin/python3` to be available; the path isn't configurable right now.

Once you have the prerequisites you can install using the standard incantation

```bash
cabal install matplotlib
```

### Examples

Click on any of the examples below to go to the corresponding test that
generates it. Depending on your matplotlib version default colors might be
different.

[![quadratic][img_quadratic]][url_quadratic]
[![labelled-histogram][img_labelled-histogram]][url_labelled-histogram]
[![projections][img_projections]][url_projections]
[![histogram][img_histogram]][url_histogram]
[![pcolorlog][img_pcolorlog]][url_pcolorlog]
[![scatter][img_scatter]][url_scatter]
[![streamplot][img_streamplot]][url_streamplot]
[![hist2DLog][img_hist2DLog]][url_hist2DLog]
[![stacked][img_stacked]][url_stacked]
[![legend][img_legend]][url_legend]
[![errorbar][img_errorbar]][url_errorbar]
[![griddata][img_griddata]][url_griddata]
[![line-options][img_line-options]][url_line-options]
[![quiver-fancy][img_quiver-fancy]][url_quiver-fancy]
[![annotation][img_annotation]][url_annotation]
[![integral][img_integral]][url_integral]
[![contour][img_contour]][url_contour]
[![boxplot][img_boxplot]][url_boxplot]
[![show-matrix][img_show-matrix]][url_show-matrix]
[![scatterhist][img_scatterhist]][url_scatterhist]
[![hinton][img_hinton]][url_hinton]
[![density][img_density]][url_density]
[![violinplot][img_violinplot]][url_violinplot]
[![histMulti][img_histMulti]][url_histMulti]
[![spines][img_spines]][url_spines]
[![corr][img_corr]][url_corr]
[![cumulative][img_cumulative]][url_cumulative]
[![polar][img_polar]][url_polar]
[![hists][img_hists]][url_hists]
[![tex][img_tex]][url_tex]
[![eventplot][img_eventplot]][url_eventplot]
[![line-function][img_line-function]][url_line-function]
[![density-bandwidth][img_density-bandwidth]][url_density-bandwidth]
[![bivariateNormal][img_bivariateNormal]][url_bivariateNormal]
[![quiver][img_quiver]][url_quiver]
[![pie][img_pie]][url_pie]
[![images][img_images]][url_images]

[img_pcolorlog]: https://i.imgur.com/O8hU7Rl.png "pcolorlog"
[url_pcolorlog]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L495
[img_griddata]: https://i.imgur.com/Lh2xOPf.png "griddata"
[url_griddata]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L602
[img_streamplot]: https://i.imgur.com/AveIggT.png "streamplot"
[url_streamplot]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L582
[img_hist2DLog]: https://i.imgur.com/PnPyJzO.png "hist2DLog"
[url_hist2DLog]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L275
[img_cumulative]: https://i.imgur.com/W9JinZC.png "cumulative"
[url_cumulative]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L217
[img_eventplot]: https://i.imgur.com/2chP6qg.png "eventplot"
[url_eventplot]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L280
[img_quiver]: https://i.imgur.com/7G7r9gu.png "quiver"
[url_quiver]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L423
[img_labelled-histogram]: https://i.imgur.com/cOCIlpy.png "labelled-histogram"
[url_labelled-histogram]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L226
[img_line-options]: https://i.imgur.com/0Jr83hl.png "line-options"
[url_line-options]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L249
[img_violinplot]: https://i.imgur.com/MaWL0BD.png "violinplot"
[url_violinplot]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L298
[img_density]: https://i.imgur.com/v7GoIno.png "density"
[url_density]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L241
[img_tex]: https://i.imgur.com/liZci2C.png "tex"
[url_tex]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L259
[img_scatterhist]: https://i.imgur.com/DG3obq8.png "scatterhist"
[url_scatterhist]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L305
[img_boxplot]: https://i.imgur.com/aaoMEql.png "boxplot"
[url_boxplot]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L292
[img_histogram]: https://i.imgur.com/yO6dxEO.png "histogram"
[url_histogram]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L214
[img_images]: https://i.imgur.com/3ZqQDxR.png "images"
[url_images]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L482
[img_polar]: https://i.imgur.com/BTXgvqa.png "polar"
[url_polar]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L450
[img_histMulti]: https://i.imgur.com/hO2CFGQ.png "histMulti"
[url_histMulti]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L334
[img_annotation]: https://i.imgur.com/cPSB8BX.png "annotation"
[url_annotation]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L539
[img_corr]: https://i.imgur.com/fh4Bv2X.png "corr"
[url_corr]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L254
[img_scatter]: https://i.imgur.com/qg7W3oc.png "scatter"
[url_scatter]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L219
[img_hinton]: https://i.imgur.com/RPkTFIJ.png "hinton"
[url_hinton]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L380
[img_quiver-fancy]: https://i.imgur.com/VQ5RrfJ.png "quiver-fancy"
[url_quiver-fancy]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L434
[img_quadratic]: https://i.imgur.com/fs96snF.png "quadratic"
[url_quadratic]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L245
[img_line-function]: https://i.imgur.com/5REiBVO.png "line-function"
[url_line-function]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L243
[img_integral]: https://i.imgur.com/OxGr14f.png "integral"
[url_integral]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L396
[img_contour]: https://i.imgur.com/kMxGGot.png "contour"
[url_contour]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L224
[img_projections]: https://i.imgur.com/laubT7H.png "projections"
[url_projections]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L247
[img_legend]: https://i.imgur.com/EmpXjUY.png "legend"
[url_legend]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L270
[img_spines]: https://i.imgur.com/fc9xwh5.png "spines"
[url_spines]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L352
[img_density-bandwidth]: https://i.imgur.com/e48WAPS.png "density-bandwidth"
[url_density-bandwidth]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L238
[img_bivariateNormal]: https://i.imgur.com/ngX1N2R.png "bivariateNormal"
[url_bivariateNormal]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L465
[img_pie]: https://i.imgur.com/DSRqr14.png "pie"
[url_pie]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L516
[img_show-matrix]: https://i.imgur.com/bd6x6Es.png "show-matrix"
[url_show-matrix]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L267
[img_hists]: https://i.imgur.com/hJaxZJt.png "hists"
[url_hists]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L371
[img_errorbar]: https://i.imgur.com/ooTcL40.png "errorbar"
[url_errorbar]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L287
[img_stacked]: https://i.imgur.com/TIqlAtR.png "stacked"
[url_stacked]: https://github.com/abarbu/matplotlib-haskell/blob/master/test/Spec.hs#L523
