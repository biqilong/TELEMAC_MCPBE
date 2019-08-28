#!/usr/bin/env python3
"""
 extract a result from telemac in some points and write it to CSV format.
 To be used after source $HOMETEL/configs/pysource....sh
 then type extract.py -h
"""

# Imports
import argparse
from os import sys, path
from data_manip.extraction.telemac_file import TelemacFile
from postel.plot1d import plot1d_history
import numpy as np

def infores(res, spectre):
    """
    Gives information about file result.
    @param res (TelemacFile) contains information about results of Telemac
     """
    print('--------------------------------------------------------')
    print(res.title)
    print('--------------------------------------------------------')
    print('Number of Time steps: ', res.ntimestep)
    print('--------------------------------------------------------')
    print('Domain: [', min(res.meshx), ',', max(res.meshx), ']x[',\
                       min(res.meshy), ',', max(res.meshy), ']')
    print('--------------------------------------------------------')
    if spectre > 0:
        points = res.get_list_spectrum_points()
        print(' List of nodes recorded in the file :', points)
        print('--------------------------------------------------------')
    else:
        print('variable names', res.varnames)
        print('--------------------------------------------------------')

def plotres(absc, data, xlabel, points, name, file=None):
    """
    Plot data along abscissae (time, frequency or angles)
    for npoints points

    @param absc (numpy array)
    @param data (nparray)
    @param xlabel (string)
    @param points (list)
    @param name (string)
    """
    import matplotlib.pyplot as plt
    # Initializing figure
    _, axe = plt.subplots(figsize=(10, 5))

    # for each point adding an history plot with a label point
    for i, point in enumerate(points):
        plot1d_history(axe, absc, data[i, :],\
                   x_label=xlabel,\
                   y_label=name,\
                   plot_label='point_{}'.format(point))
    axe.legend()
    if file != None:
        plt.savefig(file)
        plt.close(1)
        print('Saving figure ', name, ' in ', file)
    else:
        plt.show()
    del plt

def csvsave(csv_file, absc, data, name, npoints):
    """
    Save data along abscissae (time, frequency or angles)
    for npoints points in a file on csv format

    @param csv_file (string)
    @param absc (numpy array)
    @param data (numpy array)
    @param name (string)
    @param npoints (int)
    """

    # Adding time values as the first column
    csv_data = np.vstack((absc, data))
    header = 'Time'
    for i in range(npoints):
        header = header+';'+name.replace(' ', '_')+'_p'+str(i+1)

# Saving as csv file (using .T on array to have n lines and npoints+1 columns)
    np.savetxt(csv_file, csv_data.T, delimiter=";", header=header)
    print('Data about ', name, ' saved in', csv_file)
    print('--------------------------------------------------------')

def list_of_points(arg):
    """
    Change the string argument in a 2 dimension array.
    @param arg (string)
    """
    res = [[float(v) for v in r.lstrip('(').rstrip(')').split(',')]\
           for r in arg.replace(' ', '').split(';')]
    return res

def spectral_points(arg):
    """
    Change the string argument in a list of integer
    @param arg (string)
    """
    res = [int(v) for v in arg.replace(' ', '').split(',')]
    return res


def main():
    """
    program principal
    """
    parser = argparse.ArgumentParser(description='specify extraction')
    parser.add_argument("file_name",
                        default=None,
                        help="the telemacd result file")
    parser.add_argument("-c", "--csvfile",
                        dest='csv_file',
                        default=None,
                        help="save data int the csv file")
    parser.add_argument("-i", "--info", action="store_true",
                        dest='info',
                        default=False,
                        help="shall we display information")
    parser.add_argument("-g", "--graph", action="store_true",
                        dest='graph',
                        default=False,
                        help="shall we plot the results ")
    parser.add_argument("--png",
                        dest='png_file',
                        default=None,
                        help="Save a plot of data in a png file")
    parser.add_argument("-v", "--Variablechoice",
                        dest='var',
                        default=0,
                        help="Choose the name of variable to extract\
                              by its number (run extract.py file -i before)")
    parser.add_argument("-t", "--timestep",
                        dest='time',
                        default=-1,
                        help="Choose the time step to plot\
                              (run extract.py file -i before)")
    parser.add_argument("-p", "--points",
                        dest='points',
                        type=list_of_points,
                        help="Choose the  points (xi,yi) where to extract \
                              use --points '(x1,y1);(x2,y2)...'")
    parser.add_argument("--spe",
                        dest='spectre',
                        default=0,
                        help="choose the method to extract data \n\
                              0 classical extraction \n\
                              1 spectrum analysis \n \
                              2 angular analysis ")
    parser.add_argument("--spp",
                        dest='sppoints',
                        type=spectral_points,
                        help="Choose the nodes where the spectrum analysis \
                              is to do use   \
                              -spp 'n1,n2,n3...' where ni is a node number")
    csv = False
    png = False
    args = parser.parse_args()
    choivar = int(args.var)
    csv_file = args.csv_file
    file_name = args.file_name
    spectre = int(args.spectre)
    if args.csv_file != None:
        csv_file = args.csv_file
        csv = True
    if args.png_file != None:
        png = True
    info = args.info
    graph = args.graph
    if path.isfile(file_name):
        res = TelemacFile(file_name)
    else:
        print('File ',file_name, ' does not exist')
        sys.exit(0)
    name = res.varnames[choivar]
    if info:
        infores(res, spectre)
    if graph or csv or png:
        if spectre == 0:
            if args.points is None:
                print('if you want a graph or a Csv file',\
                      'you must define at least a point')
                print()
                print('                 use the option \
                      -p "(x1,y1);(x2,y2);..."')
                print()
                print('--------------------------------\
                      ------------------------')
                sys.exit(0)
            points = np.array(args.points)

    # Getting array of times values
            absc = res.times
            data = res.get_timeseries_on_points(points, name)
            xlabel = 'Time/Date'
            for i, point in enumerate(points):
                if np.isnan(data[i, 0]):
                    print('-------------------------------\
                           -------------------------')
                    print('point ', point, ' seems to be out of data definition\
                          its value is nan')
                    print('-------------------------------\
                           -------------------------')
        elif spectre == 1:
            if args.sppoints is None:
                print('if you want a graph or a Csv file\
                       you must define at least a point')
                print()
                print('                 use the option -spp "n1 n2 n3"')
                print()
                print('-----------------------------------\
                       ---------------------')
                sys.exit(0)
            time = int(args.time)
            if time == -1:
                time = res.ntimestep-1
            points = args.sppoints
            freqs, _ = res.get_spectrum_freq()
            data = np.zeros((len(points), len(freqs)), dtype=np.float64)
            for i, point in enumerate(points):
                freqs, data[i, :] = res.get_spectrum(point, res.ntimestep-1)
            absc = np.transpose(freqs)
            xlabel = 'frequency'
            name = 'Spectrum'
        elif spectre == 2:
            if args.sppoints is None:
                print('if you want a graph or a Csv file',\
                      'you must define at least a point')
                print()
                print('                 use the option -spp "n1 n2 n3"')
                print()
                print('----------------------------------\
                       ----------------------')
                sys.exit(0)
            points = args.sppoints
            freqs, _ = res.get_spectrum_freq()
            nfreq = len(freqs)
            ntheta = res.npoin2//nfreq
            data = np.zeros((len(points), ntheta), dtype=np.float64)
            for i, point in enumerate(points):
                angles, data[i, :] = res.get_angular_dispersion(point,\
                                     res.ntimestep-1, radian=True)
            absc = np.transpose(angles)
            xlabel = 'angles'
            name = 'Angular dispersion'
        if graph:
            plotres(absc, data, xlabel, points, name)
        if png:
            plotres(absc, data, xlabel, points, name, file=args.png_file)
        if csv:
            csvsave(csv_file, absc, data, name, len(points))
    del res

if __name__ == "__main__":
    main()
