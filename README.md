# Matplotlib

[![Build Status](http://circleci-badges-max.herokuapp.com/img/abarbu/matplotlib-haskell/master?token=468e8942459ca5f34089fb5c29a478ffb6d531af)](https://circleci.com/gh/abarbu/matplotlib-haskell/tree/master)

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

If you have instructions for other machines or OSes let me know.
