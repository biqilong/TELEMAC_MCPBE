#!/usr/bin/env python
"""Provides a function to plot the validation example for YEN from Telemac csv-files

We use the python built in csv-reader. There is no CSV standard we asume that there
is a Telemac standard

Please notice:
    - The following lines are needed on a server without graphical screen
        import matplotlib as mpl
        mpl.use('Agg')

"""
from __future__ import print_function
import matplotlib as mpl
mpl.use('Agg')
from os import getcwd,path,sep
import sys
import csv
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.tri as tri

################################################################################
#####    Will allow to use pytel scripts from the test case folder    ##########
################################################################################
actualPath =  getcwd()
pathsplit = actualPath.split(sep)
pytelpath = ""
pytelpath += sep

for i in range(len(pathsplit)):
    if "example" in pathsplit[i]:
        break
    if len(pathsplit[i]) > 0:
        pytelpath = path.join(pytelpath,pathsplit[i])

pytelpath = path.join(pytelpath,'scripts','python3')
sys.path.append(pytelpath)

################################################################################
#####    Dependencies towards other pytel/modules                     ##########
################################################################################

from data_manip.formats.selafin import Selafin

__authors__="Rebekka Kopmann, Leopold Stadler"
__maintainer__ = "Rebekka Kopmann"
__version__ = "1.0"



def getPositionOfVariable(varnames,searchName):

    for i,name in enumerate(varnames):
      if name.strip() == searchName:
         return i

    print("Exit Variable %s not found in varnames" %(searchName))
    sys.exit()

    return

if __name__ == "__main__":

    if len(sys.argv) < 2:
      sys.exit("No arguments please give an input file and outputname")

    #########################################################################
    #
    # factor is used for scaling (evolution/h0_factor = evolutionCut)
    # See the paper of Yen et al. 1995
    #########################################################################

    selafinfile = sys.argv[1]
    slf = Selafin(selafinfile)
    mesh = np.array(slf.ikle3)
    meshx = slf.meshx
    meshy = slf.meshy
    triang = tri.Triangulation(meshx,meshy,mesh)
    timePos = len(slf.tags["times"]) -1

    values = slf.get_values(timePos)
    varPos = getPositionOfVariable(slf.varnames,'MEAN DIAMETER M')
    evolution = values[varPos]
    evolution_interpolator = tri.LinearTriInterpolator(triang,evolution)

    #########################################################################
    #
    # Read the reference file
    #
    #########################################################################

    #load profile 90
    # simulation must be multiplied by 1000 for mm
    data_profile_90 =  np.genfromtxt("yen_90profilexy-dm.dat",names=["x","y","z"])
    profileRef90X = data_profile_90["x"]
    profileRef90Y = data_profile_90["y"]
    profileRef90Z = data_profile_90["z"]
    evolutionCut_90 = evolution_interpolator.__call__(profileRef90X,profileRef90Y)*1000

    distance_profile_90 = profileRef90Y - 46.1371

    #########################################################################
    #
    # Plot with matplotlib
    #
    #########################################################################

    #plot 90
    plt.plot(distance_profile_90, profileRef90Z, "o-",color="darkorange", label = "Reference")
    plt.plot(distance_profile_90, evolutionCut_90,"<--",color="green", label = "Simulation")
    plt.xlabel("Distance [m]")
    plt.ylabel("Mean diameter [mm]")
    plt.grid()
    plt.xlim([0.0,1.0])
    plt.legend()
    title = sys.argv[2] +"_90"
    plt.title(title)
    plt.savefig(title + ".pdf")
    plt.savefig(title + ".png")
    plt.clf()
