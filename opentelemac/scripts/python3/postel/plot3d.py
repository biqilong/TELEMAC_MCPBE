#!/usr/bin/python3
"""
Contains function to plot stuff
"""
from utils.exceptions import TelemacException
from postel.plot2d import set_extrema

import numpy as np
import matplotlib.cm as cm
from mpl_toolkits.mplot3d import Axes3D

def plot3d_scalar_map(fig, axe, mesh, data,\
        x_label='', y_label='', data_name='data',\
        vmin=None, vmax=None, nv=10, levels=None, \
        colorbar=True, cbar_ticks=None, cbar_axe=None, \
        cmap_name='jet', **kwargs):
    """
    Plot a 3d representation of a triangle mesh with a data as z coordinates
    with default title (2D mesh (%d triangles, %d nodes) with <data_name> (%s))
    and xlabel and ylabel (X/Y-coordinates (m))

    @param fig (matplotlib.figure) matplotlib figure structure
    @param axe (matplotlib.axes3d) matplotlib axes on which to draw
    @param mesh (matplotlib.tri.Triangulation or tuple)
    triangular mesh or regular x,y grid
    @param data (numpy.array) Value to plot
    @param data_name (string) Name of the data to display
    @param colorbar (bool) show colorbar (default: True)
    @param cbar_ticks (np.array) ticks of the colorbar
    @param cbar_axe (matplotlib.axes) axe used for colorbar (default: None)
    @param vmin (float) Minimal value of data to plot
    @param vmax (float) Maximal value of data to plot
    @param nv (integer) Number of sample for colorbar range (default 10)
    @param linewidth (float) thickness of the plots line (default 0.)
    @param cmap_name (string) Name of the coloring map to use for the data
    @param kwargs (dict) rest of optional arguments given to trisurf
    """
    # Checking we indeed have a 3d Axes
    if not isinstance(axe, Axes3D):
        raise TelemacException(\
                "axe must be a Axes3D instance it is %s", type(axe))

    # Getting cmap object from name
    cmap = cm.get_cmap(name=cmap_name, lut=None)

    # Setting levels with min, max and nv values if levels is not prescribed
    if vmax is not None or vmin is not None:
        assert levels is None
        vmin, vmax = set_extrema(data, vmin, vmax)
        levels = np.linspace(vmin, vmax, nv)

    # Plotting triangle surface
    img = axe.plot_trisurf(mesh, data, vmin=vmin, vmax=vmax,
                           cmap=cmap, **kwargs)

    # Adding color bar if requested
    if colorbar:
        if levels is not None and cbar_ticks is None:
            cbar_ticks = levels

        cbar = fig.colorbar(img, ax=axe, cax=cbar_axe,
                            ticks=cbar_ticks, shrink=0.5)

        if data_name is not None:
            cbar.set_label(data_name)

    # Default axis names
    axe.set_xlabel(x_label)
    axe.set_ylabel(y_label)
