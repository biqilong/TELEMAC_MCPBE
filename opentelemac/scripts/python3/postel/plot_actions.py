#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium

   @brief Function for plot.py
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from os import path
from data_manip.computation.triangulation import triangulation_from_data
from postel.plot2d import plot2d_triangle_mesh, plot2d_annotate_bnd,\
                plot2d_annotate_liq_bnd, plot2d_scalar_map, \
                plot2d_scalar_filled_contour
from postel.plot1d import plot1d
from postel.plot3d import plot3d_scalar_map
from utils.exceptions import TelemacException
import argparse
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

def add_options_fig(parser):
    """
    Add options for a figure (x_label, y_label, title, fig_name, fig_size...)

    @param parser (ArgumentParser) The parser

    @returns the updated parser
    """
    # Options to save the file instead of displaying it
    parser.add_argument(\
        "-f", "--figure-name",
        dest="fig_name", default="",
        help="If given the figure will be saved in fig_name instead "\
             "of beeing displayed")

    parser.add_argument(\
        "--figure-size",
        dest="fig_size", default=None, type=int, nargs=2,
        help="Size of the figure generated tuple of integer space "\
             "separated 20 12")

    return parser

def add_options_var(subparser):
    """
    Defines options for var action

    @param subparser (ArgumentParser) The subparser

    @returns the update subparser
    """
    parser = subparser.add_parser('var',\
            help='Plot a scalar map for a given variable and time/record')

    parser.add_argument("input_file", default=None, \
        help="Name of the input file extension also defines the input format")
    parser.add_argument(
        "-v", "--var",
        dest="var", default="",
        help="Name of the variable to display")
    parser.add_argument(
        "-r", "--record",
        dest="record", type=int, default=0,
        help="Record to display (If -1 is given will return the last one)")
    parser.add_argument(
        "-t", "--time",
        dest="time", type=float, default=None,
        help="Time to display (will take the closest record)")
    parser.add_argument(
        "--mesh", action="store_true",
        dest="mesh", default=False,
        help="Adds the mesh to the display")
    parser = add_options_fig(parser)

    return subparser

def arg_points(string):
    """
    Definition of a point for argparse
    """
    n_coords = string.count(",") + 1

    if n_coords == 2:
        try:
            x, y = map(float, string.split(','))
            return x, y
        except:
            raise argparse.ArgumentTypeError("Points must be x,y")
    elif n_coords == 3:
        try:
            x, y, z = map(float, string.split(','))
            return x, y, z
        except:
            raise argparse.ArgumentTypeError("Points must be x,y,z")
    else:
        raise argparse.ArgumentTypeError("Points must be either x,y or x,y,z")

def add_options_timeseries_on_polyline(subparser):
    """
    Defines options for time_poly action

    @param subparser (ArgumentParser) The subparser

    @returns the update subparser
    """
    parser = subparser.add_parser('time_poly',\
            help='Plot a timeseries on a polyline')

    parser.add_argument("input_file", default=None, \
        help="Name of the input file extension also defines the input format")
    parser.add_argument(
        "-v", "--var",
        dest="var", default="",
        help="Name of the variable to display")
    parser.add_argument(
        "--poly",
        dest="poly", type=arg_points, nargs='+',
        help="List of points for the polyline x,y space separated")

    parser = add_options_fig(parser)

    return subparser

def add_options_history(subparser):
    """
    Defines options for history action

    @param subparser (ArgumentParser) The subparser

    @returns the update subparser
    """
    parser = subparser.add_parser('history',\
            help='Plot history for a list of points')

    parser.add_argument("input_file", default=None, \
        help="Name of the input file extension also defines the input format")
    parser.add_argument(
        "-v", "--var",
        dest="var", default="",
        help="Name of the variable to display")
    parser.add_argument(
        "--points",
        dest="points", type=arg_points, nargs='+',
        help="List of points for which to plot history x,y space separated")

    parser = add_options_fig(parser)

    return subparser

def add_options_mesh2d(subparser):
    """
    Defines options for mesh2 action

    @param subparser (ArgumentParser) The subparser

    @returns the update subparser
    """
    parser = subparser.add_parser('mesh2d', \
            help='Plot a 2d mesh can add boundary or liquid boundary info')

    parser.add_argument("input_file", default=None, \
        help="Name of the input file extension also defines the input format")
    # the boundary file option
    parser.add_argument(
        "-b", "--boundary-file",
        dest="bnd_file", default=None,
        help="Name of the boundary file")

    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--bnd", action="store_true",
        dest="bnd", default=False,
        help="Adding type of boundary for each boundary node")
    group.add_argument(
        "--liq-bnd", action="store_true",
        dest="liq_bnd", default=False,
        help="Adding number of liquid boundary for each boundary node")

    parser = add_options_fig(parser)

    return subparser

def add_options_3d_scalar_map(subparser):
    """
    Defines options for 3d_scalar_map action

    @param subparser (ArgumentParser) The subparser

    @returns the update subparser
    """
    parser = subparser.add_parser('3d_scalar_map', \
            help='Plot a 3d representation of a 2D variable '\
                 'using variable values as Z coordinates')

    parser.add_argument("input_file", default=None, \
        help="Name of the input file extension also defines the input format")

    parser.add_argument(
        "-v", "--var",
        dest="var", default="",
        help="Name of the variable to display")
    parser.add_argument(
        "-r", "--record",
        dest="record", type=int, default=-1,
        help="Record to display (If -1 is given will return the last one)")
    parser.add_argument(
        "-t", "--time",
        dest="time", type=float, default=None,
        help="Time to display (will take the closest record)")
    parser.add_argument(
        "--mesh", action="store_true",
        dest="mesh", default=False,
        help="Adds the mesh to the display")
    parser = add_options_fig(parser)

    return subparser

def list_of_points(arg):
    """
    Change the string argument in a 2 dimension array.

    @param arg (string)

    @returns a list of list
    """
    res = [[float(v) for v in r.lstrip('(').rstrip(')').split(',')]\
           for r in arg.replace(' ', '').split(';')]
    return res

def add_options_vertical_slice(subparser):
    """
    Defines options for v_slice action

    @param subparser (ArgumentParser) The subparser

    @returns the update subparser
    """
    parser = subparser.add_parser('v_slice', \
            help='Plot a vertical slice of a 3d mesh along a polyline')

    parser.add_argument("input_file", default=None, \
        help="Name of the input file extension also defines the input format")

    parser.add_argument(
        "-v", "--var",
        dest="var", default="",
        help="Name of the variable to display")

    parser.add_argument("--poly",
                        dest='poly',
                        type=arg_points,
                        nargs='+',
                        help="Choose the  points (xi,yi) where to extract \
                              use --poly='x1,y1 x2,y2...'")
    parser.add_argument(
        "-r", "--record",
        dest="record", type=int, default=-1,
        help="Record to display (If -1 is given will return the last one)")
    parser.add_argument(
        "-t", "--time",
        dest="time", type=float, default=None,
        help="Time to display (will take the closest record)")
    parser.add_argument(
        "--mesh", action="store_true",
        dest="mesh", default=False,
        help="Adds the mesh to the display")
    parser = add_options_fig(parser)
    return subparser

def add_options_horizontal_slice(subparser):
    """
    Defines options for h_slice action

    @param subparser (ArgumentParser) The subparser

    @returns the update subparser
    """
    parser = subparser.add_parser('h_slice', \
            help='Plot a horizontal slice of a 3d mesh along a plane number')

    parser.add_argument("input_file", default=None, \
        help="Name of the input file extension also defines the input format")

    parser.add_argument(
        "-v", "--var",
        dest="var", default="",
        help="Name of the variable to display")

    parser.add_argument("--plane",
                        dest='plane',
                        type=int,
                        help="Plane number to slice")
    parser.add_argument(
        "-r", "--record",
        dest="record", type=int, default=-1,
        help="Record to display (If -1 is given will return the last one)")
    parser.add_argument(
        "-t", "--time",
        dest="time", type=float, default=None,
        help="Time to display (will take the closest record)")
    parser.add_argument(
        "--mesh", action="store_true",
        dest="mesh", default=False,
        help="Adds the mesh to the display")
    parser = add_options_fig(parser)
    return subparser

def plot_var(res, var, record=-1, time=None, add_mesh=False,
             fig_size=None, fig_name=''):
    """
    Plot a scalar map for the given variable and time record

    @param res (TelemacFile) Structure to file from which data will be read
    @param var (str) Name of the variable to plot
    @param record (str) Record to plot
    @param time (str) If >= 0.0 will get nearest record to that time (This
    overwrites record)
    @param add_mesh (boolean) If True overlay the mesh on top of the scalar map
    @param fig_size (2-uple) Size of figure default (12, 7)
    @param fig_name (str) If not empty save the plot in that file instead of
    showing it
    """
    # If time is positive searched for record
    if time is not None:
        record = res.get_time_record(time)
        time = res.times[record]
    else:
        time = res.times[record]

    if var not in res.varnames:
        raise TelemacException("{} is not in :\n{}".format(var, res.varnames))

    var_value = res.get_data_value(var, record)

    fig, ax = plt.subplots(figsize=fig_size)

    if add_mesh:
        plot2d_triangle_mesh(ax, res.tri, x_label='X (m)', y_label='Y (m)',
                             color='k', linewidth=0.1)

    plot2d_scalar_map(fig, ax, res.tri, var_value, data_name=var)

    ax.set_title('%s:\nPlotting at time %f (record %d)'%(\
                        path.basename(res.file_name),
                        time,
                        record))

    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    plt.close(fig)

def plot_mesh2d(res, display_bnd=False,
                display_liq_bnd=False, fig_size=None, fig_name=''):
    """
    Plot a 2d triangular mesh with either boundary conditions or liquid
    boundary number

    @param input_file (string) File from wich to read the mesh
    @param bnd_file (string) Name of the boundary file
               (only used if display_bnd or display_liq_bnd is True)
    @param display_bnd (boolean) If True display boundary type for each
        boundary node
    @param display_liq bnd (boolean) If True display liquidi boundary number
        for each boundary node
    @param fig_size (2-uple) Size of figure default (12, 7)
    @param fig_name (str) If not empty save the plot in that file instead of
    showing it
    """
    if (display_bnd or display_liq_bnd) and res.boundary_file == '':
        raise TelemacException(\
                "bnd_file is mandatory if using --bnd or --liq-bnd")

    fig, ax = plt.subplots(figsize=fig_size)

    # Plot mesh
    plot2d_triangle_mesh(ax, res.tri, x_label='X (m)', y_label='Y (m)',
                         color='k', linewidth=0.1)

    if display_bnd:
        bnd_info = res.get_bnd_info()
        # Add boundary info
        plot2d_annotate_bnd(ax, res.tri, bnd_info,
                            markersize=1.5, marker='o')
    elif display_liq_bnd:
        liq_bnd_info = res.get_liq_bnd_info()
        # Add liquid boundary info
        plot2d_annotate_liq_bnd(ax, res.tri, liq_bnd_info,
                                markersize=1.5, marker='x')

    ax.set_title('%s:\n2D mesh (%d triangles, %d nodes)' % (\
                        path.basename(res.file_name),
                        len(res.tri.triangles),
                        len(res.tri.x)))
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    plt.close(fig)

def plot_timeseries_on_polyline(\
        res, var_name, poly, records=None, poly_discret=None,
        fig_size=None, fig_name=''):
    """
    Plot a value over a polyline for a range of time

    @param res (TelemacFile) Struct of the file from which to extract the data
    @param var_name (str) Name of the variable fro which to extract the data
    @param poly (list) List of polyline points
    @param records (list) List of record for which to extrac the data
    @param poly_discret (list) Number of dicsretization point for each segment
    of the polyline (if None will use miminal mesh size as step)
    @param fig_size (2-uple) Size of figure default (12, 7)
    @param fig_name (str) If not empty saving in that file
    """
    if poly_discret is None:
        poly_discret = res.discretize_polyline(poly)

    # Getting water depth values over time for each discretized points of the
    # polyline
    _, abs_curv, values_polylines = \
            res.get_timeseries_on_polyline(poly, var_name, poly_discret)

    #Initialising figure
    fig, ax = plt.subplots(figsize=fig_size)

    if records is None:
        records = range(0, res.ntimestep)

    for record in records:
        # plot over the polyline of the initial condition
        plot1d(ax, abs_curv, values_polylines[:, record],
            x_label='y (m)',
            y_label=var_name,
            plot_label='t={} s'.format(res.get_data_time(record)))

    # Displaying legend
    ax.legend()

    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    plt.close(fig)

def plot_history(\
        res, var_name, points, records=None,
        fig_size=None, fig_name=''):
    """
    Plot values of points over a range of records

    @param res (TelemacFile) Struct of the file from which to extract the data
    @param var_name (str) Name of the variable fro which to extract the data
    @param points (list) List of points (x,y) in 2d (x,y,z) in 3d
    @param records (list) List of record for which to extrac the data
    @param fig_size (2-uple) Size of figure default (12, 7)
    @param fig_name (str) If not empty saving in that file
    """
    # Getting water depth values over time for each discretized points of the
    # polyline
    data = res.get_timeseries_on_points(points, var_name)

    #Initialising figure
    fig, ax = plt.subplots(figsize=fig_size)

    if records is None:
        records = range(0, res.ntimestep)

    for i, point in enumerate(points):
        # plot over the polyline of the initial condition
        plot1d(ax, res.times[records], data[i, :],
               x_label='time (s)',
               y_label=var_name,
               plot_label='point={}'.format(point))

    # Displaying legend
    ax.legend()

    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    plt.close(fig)

def plot_3d_scalar_map(res, varname, record=-1, time=None,
                       fig_size=None, fig_name=''):
    """
    Plot a scalar map using values as z coordinates

    @param res (TelemacFile) Struct to file from which data will be read
    @param varname (str) Name of the variable to plot
    @param record (str) Record to plot
    @param time (str) If >= 0.0 will get nearest record to that time (This
    overwrites record)
    @param fig_size (2-uple) Size of figure
    @param fig_name (str) If not empty save the plot in that file instead of
    showing it
    """

    # If time is positive searched for record
    if time is not None:
        rrecord = res.get_time_record(time)
        ttime = time
    else:
        rrecord = record
        ttime = res.times[record]

    data = res.get_data_value(varname, rrecord)

    # Initialisaing matplotlb figure
    fig = plt.figure(figsize=fig_size)

    # Adding a 3d axe
    axe = Axes3D(fig)

    #Plotting mesh
    plot3d_scalar_map(fig, axe, res.tri, data,
                      x_label='X (m)', y_label='Y (m)', data_name=varname)

    axe.set_title('{} at time {} (s)'.format(varname, ttime))

    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    plt.close(fig)

def plot_vertical_slice(res, varname, poly, poly_discret=None,
                        record=-1, time=None, add_mesh=False,
                        xlabel='X(m)', ylabel='Z(m)',
                        fig_size=None, fig_name=''):
    """
    Plot a vertical slice of a 3d mesh

    @param res (TelemacFile) Struct to file from which data will be read
    @param varname (str) Name of the variable to plot
    @param poly (list) List of polyline points
    @param poly_discret (list) Number of dicsretization point for each segment
    @param record (str) Record to plot
    @param time (str) If >= 0.0 will get nearest record to that time (This
    overwrites record)
    @param add_mesh (boolean) If True overlay the mesh on top of the scalar map
    @param fig_size (2-uple) Size of figure
    @param fig_name (str) If not empty save the plot in that file instead of
    showing it
    """
    # If time is positive searched for record
    if time is not None:
        rrecord = res.get_time_record(time)
        ttime = time
    else:
        rrecord = record
        ttime = res.times[record]

    # Z should the first variable
    namez = res.varnames[0]

    # Building poly discretization
    if poly_discret is None:
        poly_discret = res.discretize_polyline(poly)

    _, abs_curv, values_z = \
        res.get_data_values_on_vertical_plan(poly, namez, \
                                             poly_discret, rrecord)
    _, _, values = \
        res.get_data_values_on_vertical_plan(poly, varname, \
                                             poly_discret, rrecord)

    mesh = triangulation_from_data(abs_curv, values_z)

    data = values.flatten()

    fig, axe = plt.subplots(figsize=fig_size)

    # Add the mesh to the plot if asked for
    if add_mesh:
        plot2d_triangle_mesh(axe, mesh, x_label=xlabel, y_label=ylabel)

    plot2d_scalar_filled_contour(fig, axe, mesh, data, data_name=varname)

    title = '{} at time {} (s)'.format(varname, ttime)
    axe.set_title(title)
    axe.legend()

    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    plt.close(fig)

def plot_horizontal_slice(res, varname, plane, record=-1, time=None,
                          add_mesh=False, fig_size=None, fig_name=''):
    """
    Plat an horizontal slice of a 3d mesh for given plane number

    @param file_name (TelemacFile) Struct to file from which data will be read
    @param varname (str) Name of the variable to plot
    @param plane (int) Number of the plane from which to extrac
    @param record (str) Record to plot
    @param time (str) If >= 0.0 will get nearest record to that time (This
    overwrites record)
    @param add_mesh (boolean) If True overlay the mesh on top of the scalar map
    @param fig_size (2-uple) Size of figure
    @param fig_name (str) If not empty save the plot in that file instead of
    showing it
    """
    # If time is positive searched for record
    if time is not None:
        rrecord = res.get_time_record(time)
        ttime = time
    else:
        rrecord = record
        ttime = res.times[record]

    if plane < 0:
        iplane = res.nplan + plane
    else:
        iplane = plane

    data = res.get_data_values_on_horizontal_plan(iplane, rrecord, varname)

    fig, axe = plt.subplots(figsize=fig_size)

    if add_mesh:
        plot2d_triangle_mesh(axe, res.tri, x_label='X (m)', y_label='Y (m)')

    plot2d_scalar_filled_contour(fig, axe, res.tri, data, data_name=varname)

    title = '{} at time {} (s)'.format(varname, ttime)
    axe.set_title(title)

    axe.legend()

    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    plt.close(fig)
