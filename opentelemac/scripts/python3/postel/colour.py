r"""@author TELEMAC-MASCARET Consortium

   @brief
         new colormaps from old: stack,
         truncate builtin cmaps / files / numpy arrays

What's a colormap or cmap in matplotlib ?
Mainly a bar or array of 256 colors, rgb or rgba values 0 .. 1,
used in
    pl.imshow( a 2d numpy array, cmap=cmap, ... )
    pl.colorbar()
Cmaps can be indexed with () like
    cmap( .25 ),  cmap( [0, .25, .5] ),  cmap( np.linspace( ... ))
to get rgb values.

The functions below return cmaps:
    get_cmap(): "Blues" ... builtins / filename / numpy array
    array_cmap(): a numpy array, n x 3 or 4  ints 0..255 or floats 0..1
    truncate_colormap(): subset
    stack_colormap(): A B -> bottom half, A, top half B.
    band_colormap(): e.g. 10 bands

See also
     http://matplotlib.org/api/colors_api.html   $matplotlib/colors.py
     http://matplotlib.org/api/cm_api.html       $matplotlib/cm.py
     http://en.wikipedia.org/wiki/Indexed_color
"""
from __future__ import division
from __future__ import print_function
import numpy as np
from matplotlib import pyplot as pl, cm, colors

def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=256):
    """
    @brief : extract a subset of a colormap as a new colormap in matplotlib
    :param cmap name or file or ndarray
    :param minval value by default is 0.0
    :param maxval value by default is 1.0
    :return: a new colormap in matplotlib
    as seen on http://stackoverflow.com/questions/
    18926031/how-to-extract-a-subset-of
    -a-colormap-as-a-new-colormap-in-matplotlib
    """
    cmap = get_cmap(cmap)
    name = "%s-trunc-%.2g-%.2g" % (cmap.name, minval, maxval)
    return colors.LinearSegmentedColormap.from_list(\
                  name, cmap(np.linspace(minval, maxval, n)))

def stack_colormap(cmap1, cmap2, n=256):
    """
    @brief : vertical stack of two color map
    :param cmap1 is the first color map
    :param cmap2 is the second color map
    :param n number of color in the map
    :return: a new colormap in matplotlib
    """
    cmap1 = get_cmap(cmap1)
    cmap2 = get_cmap(cmap2)
    name = "%s-%s" % (cmap1.name, cmap2.name)
    lin = np.linspace(0, 1, n)
    return array_cmap(np.vstack((cmap1(lin), cmap2(lin))), name, n=n)

def get_cmap(cmap, name=None, n=256):
    """
    @brief : Get the desired color map
    :param cmap is the color map
    :param name
        - a name "Blues" "BuGn_r" ... of a builtin cmap (case-sensitive)
        - or a filename, np.loadtxt() n x 3 or 4  ints 0..255 or floats 0..1
        - or a cmap already
        - or a numpy array.
        See http://wiki.scipy.org/Cookbook/Matplotlib/Show_colormaps
        or in IPython, pl.cm.<tab>
    :param n number of color in the map
    :return: a new colormap in matplotlib
    """
    if isinstance(cmap, colors.Colormap):
        return cmap

    found = False
    try:
        found = isinstance(cmap, basestring)
    except NameError:
        found = isinstance(cmap, str)
    if found:
        if cmap in cm.cmap_d:
            return pl.get_cmap(cmap)  # "Blues" ...
        cmap_tmp = np.loadtxt(cmap, delimiter=None)  # None: white space
        name = name or cmap.split("/")[-1] .split(".")[0]  # .../xx.csv -> xx
    else:
        cmap_tmp = cmap  # numpy array or array-like

    return array_cmap(cmap_tmp, name, n=n)

def array_cmap(array, name=None, n=256):
    """
    @brief : Get the desired color map
    :param array is a numpy array
    :param name
    :param n number of color in the map
    :return: a new colormap in matplotlib
    """
    array = np.asanyarray(array)
    assert array.ndim == 2  and array.shape[1] in (3, 4), \
             "array must be n x 3 or 4, not %s" % str(array.shape)
    amin, amax = array.min(), array.max()
    if array.dtype.kind == "i":
        assert 0 <= amin < amax <= 255, \
                 "amin %d  amax %d must be in 0 .. 255" % (amin, amax)
        array = array / 255.  # not /=
    else:
        assert 0 <= amin < amax <= 1, \
                "amin %g  amax %g must be in 0 .. 1" % (amin, amax)
    return colors.LinearSegmentedColormap.from_list\
           (name or "noname", array, N=n)

def save_cmap(outfile, cmap):
    """
    @brief : Save the cmap configuration in a file
             a file of 256 x 4 ints 0 .. 255
             to load it, np.loadtxt() or get_cmap( filename )
    :param outfile file to save the cmap into
    :param cmap is the color map
    """
    cmap = get_cmap(cmap)
    tmp = cmap(np.linspace(0, 1, 256))
    np.savetxt(outfile, tmp * 255, fmt="%4.0f",\
              header="colormap %s" % cmap.name)

def band_colormap(cmap, nband=10):
    """
    @brief : Create a colormap with the desired number of band

    :param nband number of partition in the color map
    :param cmap is the color map
    """
    cmap = get_cmap(cmap)
    hband = .5 / nband
    tmp = cmap(np.linspace(hband, 1 - hband, nband))
    name = "%s-band-%d" % (cmap.name, nband)
    return array_cmap(tmp, name, n=nband)
