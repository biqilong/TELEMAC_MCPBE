#!/usr/bin/env python
"""Provides a function to plot the validation example for YEN from Telemac csv-files

We use the python built in csv-reader. There is no CSV standard we asume that there
is a Telemac standard

Please notice:
     - The evolution must be devided by 0.0544
     - The following lines are needed on a server without graphical screen
          import matplotlib as mpl
          mpl.use('Agg')

"""
import matplotlib as mpl
mpl.use('Agg')
import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.tri as tri
from os import environ, path, sep

from data_manip.formats.selafin import Selafin

__authors__ = "Rebekka Kopmann, Leopold Stadler"
__maintainer__ = "Rebekka Kopmann"
__version__ = "1.0"

def get_position_of_variable(varnames, search_name):

    for i, name in enumerate(varnames):
        if name.strip() == search_name:
            return i

    print("Exit Variable %s not found in varnames" %(search_name))
    sys.exit()

    return

def main(selafinfile, in_title):
    #########################################################################
    #
    # factor is used for scaling (evolution/h0_factor = evolutionCut)
    # See the paper of Yen et al. 1995
    #########################################################################

    slf = Selafin(selafinfile)
    mesh = np.array(slf.ikle3)
    triang = tri.Triangulation(slf.meshx, slf.meshy, mesh)
    timePos = len(slf.tags["times"]) -1

    values = slf.get_values(timePos)
    varPos = get_position_of_variable(slf.varnames, 'CUMUL BED EVOL')
    evolution = values[varPos]
    evolution_interpolator = tri.LinearTriInterpolator(triang, evolution)

    #########################################################################
    #
    # Read the reference file
    #
    #########################################################################
    h0_factor = 0.0544

    #load profile 90
    data_profile_90 = np.genfromtxt("yen_90profilexy-koor.dat",
                                    names=["x", "y", "z"])
    profileRef90X = data_profile_90["x"]
    profileRef90Y = data_profile_90["y"]
    profileRef90Z = data_profile_90["z"]
    evolutionCut_90 = evolution_interpolator.__call__(profileRef90X,
                                                      profileRef90Y)/h0_factor

    distance_profile_90 = profileRef90Y - 46.1371

    #load profile 180
    data_profile_180 = np.genfromtxt("yen_180profilexy-koor.dat",
                                     names=["x", "y", "z"])
    profileRef180X = data_profile_180["x"]
    profileRef180Y = data_profile_180["y"]
    profileRef180Z = data_profile_180["z"]
    evolutionCut_180 = evolution_interpolator.__call__(profileRef180X,
                                                       profileRef180Y)/h0_factor

    distance_profile_180 = profileRef180X - 104.682


    #########################################################################
    #
    # Plot with matplotlib
    #
    #########################################################################

    file_path = path.join(environ["HOMETEL"], "examples", "gaia", "yen-t2d", "img")

    #plot 90
    plt.plot(distance_profile_90, profileRef90Z, "o-", color="darkorange",
             label="Reference")
    plt.plot(distance_profile_90, evolutionCut_90, "<--", color="green",
             label="Simulation")
    plt.xlabel("Distance [m]")
    plt.ylabel("Normalised evolution [-]")
    plt.grid()
    plt.xlim([0.0, 1.0])
    plt.legend()
    title = in_title +"_90"
    plt.title(title)
    plt.savefig(file_path + sep + title + ".pdf")
    plt.savefig(file_path + sep + title + ".png")
    plt.clf()

    #plot 180
    plt.plot(distance_profile_180, profileRef180Z, "o-", color="darkorange",
             label="Reference")
    plt.plot(distance_profile_180, evolutionCut_180, "<--", color="green",
             label="Simulation")
    plt.xlabel("Distance [m]")
    plt.ylabel("Normalised evolution [-]")
    plt.grid()
    plt.xlim([0.0, 1.0])
    plt.legend()
    title = in_title + "_180"
    plt.title(title)
    plt.savefig(file_path + sep + title + ".pdf")
    plt.savefig(file_path + sep + title + ".png")
    plt.clf()


if __name__ == "__main__":

    if len(sys.argv) < 2:
        sys.exit("No arguments please give an input file and outputname")

    main(sys.argv[1], sys.argv[2])


