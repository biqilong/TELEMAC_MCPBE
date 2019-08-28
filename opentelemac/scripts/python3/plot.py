#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium

   @brief Simple plots
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from postel.plot_actions import \
        add_options_var, plot_var,\
        add_options_mesh2d, plot_mesh2d,\
        add_options_vertical_slice, plot_vertical_slice, \
        add_options_horizontal_slice, plot_horizontal_slice,\
        add_options_3d_scalar_map, plot_3d_scalar_map, \
        plot_timeseries_on_polyline, add_options_timeseries_on_polyline, \
        add_options_history, plot_history
from data_manip.extraction.telemac_file import TelemacFile
from argparse import ArgumentParser

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

def main():
    """
    Main function of the converter
    """
# ~~~~ Defines arguments
    parser = ArgumentParser()
    subparser = parser.add_subparsers(help='plot command to do', dest='command')

    subparser = add_options_mesh2d(subparser)
    subparser = add_options_var(subparser)
    subparser = add_options_3d_scalar_map(subparser)
    subparser = add_options_vertical_slice(subparser)
    subparser = add_options_horizontal_slice(subparser)
    subparser = add_options_timeseries_on_polyline(subparser)
    subparser = add_options_history(subparser)

    options = parser.parse_args()

    command = options.command

#   Actions to run
    if command == 'mesh2d':
        res = TelemacFile(options.input_file, bnd_file=options.bnd_file)
        plot_mesh2d(res, options.bnd,
                    options.liq_bnd, options.fig_size, options.fig_name)
    elif command == 'var':
        res = TelemacFile(options.input_file)
        plot_var(res, options.var, options.record,
                 options.time, options.mesh, options.fig_size, options.fig_name)
    elif command == '3d_scalar_map':
        res = TelemacFile(options.input_file)
        plot_3d_scalar_map(\
                res, options.var, options.record,
                options.time, options.fig_size, options.fig_name)
    elif command == 'v_slice':
        res = TelemacFile(options.input_file)
        plot_vertical_slice(\
                res, options.var,
                options.poly, record=options.record,
                time=options.time, add_mesh=options.mesh,
                fig_size=options.fig_size, fig_name=options.fig_name)
    elif command == 'h_slice':
        res = TelemacFile(options.input_file)
        plot_horizontal_slice(\
                res, options.var,
                options.plane, options.record,
                options.time, options.mesh, options.fig_size, options.fig_name)
    elif command == 'time_poly':
        res = TelemacFile(options.input_file)
        plot_timeseries_on_polyline(\
                res, options.var, options.poly,
                fig_size=options.fig_size, fig_name=options.fig_name)
    elif command == 'history':
        res = TelemacFile(options.input_file)
        plot_history(res, options.var, options.points,
                     fig_size=options.fig_size, fig_name=options.fig_name)
    else:
        parser.print_help()

    sys.exit(0)

if __name__ == "__main__":
    main()
