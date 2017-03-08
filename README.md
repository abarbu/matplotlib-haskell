# Matplotlib

[![Build Status](https://img.shields.io/circleci/project/github/abarbu/matplotlib-haskell.svg)](circleci.com/gh/abarbu/matplotlib-haskell)
[![Hackage](https://img.shields.io/hackage/v/Matplotlib.svg)](https://hackage.haskell.org/package/Matplotlib)

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
sudo apt-get install -y python3-matplotlib python3-numpy python-mpltoolkits.basemap
```

If you have instructions for other machines or OSes let me know. We require
`/usr/bin/python3` to be available; the path isn't configurable right now.

Once you have the prerequisites you can install using the standard incantation

```bash
cabal install matplotlib
```
