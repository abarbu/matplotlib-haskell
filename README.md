# Matplotlib

[![Build Status](http://circleci-badges-max.herokuapp.com/img/abarbu/matplotlib-haskell/master?token=468e8942459ca5f34089fb5c29a478ffb6d531af)](https://circleci.com/gh/abarbu/matplotlib-haskell/tree/master)

Haskell bindings to Python's Matplotlib. It's high time that Haskell had a
fully-fledged plotting library!

![matplotlib contour plot](https://github.com/abarbu/matplotlib-haskell/raw/master/imgs/contour.png)

More info and docs forthcoming. For now see the tests for some examples.

Note that the API will undergo one large change in the next few days to make
optional arguments more pervasive and less of a hassle and to eliminate some
necessary type annotations. After this it will stabilize.

```haskell
import Matplotlib

onscreen $ contourF (\a b -> sin (degreesRadians a) + cos (degreesRadians b)) (-100) 100 (-200) 200 10
```

### Installation

You will need several python libraries to run this code which can be installed
on Ubuntu machines with the following command:

```bash
sudo apt-get install -y python3-matplotlib python3-numpy python-mpltoolkits.basemap
```

If you have instructions for other machines or OSes let me know.
