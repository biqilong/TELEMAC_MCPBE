#!/usr/bin/env python
"""Provides a function to plot a contour plot for the validation
   example YEN from Telemac

We use a png file that includes the solution from the original paper.

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
import matplotlib.image as image
from os import environ, path, sep

################################################################################
#####    Dependencies towards other pytel/modules                     ##########
################################################################################

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
    """
    main function
    """
    slf = Selafin(selafinfile)
    mesh = np.array(slf.ikle3)
    triang = tri.Triangulation(slf.meshx, slf.meshy, mesh)
    timePos = len(slf.tags["times"]) -1

    values = slf.get_values(timePos)
    varPos = get_position_of_variable(slf.varnames, 'CUMUL BED EVOL')
    evolution = values[varPos]

    #scale evolution
    h0_factor = 0.0544
    evolution = evolution/h0_factor

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.axis('off')

    levels = np.asarray([-0.5, -0.25, 0, 0.25, 0.5])
    triplotcontourf = plt.tricontourf(slf.meshx, slf.meshy, mesh, evolution,
                                      levels, extend='both', alpha=0.6)

    cbar = plt.colorbar()
    cbar.set_label('NORMALISED EVOLUTION')
    #plt.legend()

    data_xmin = 96.6819 -0.12
    data_xmax = 105.6819 + 0.4
    data_ymin = 42.6371 +1
    data_ymax = 42.6371 + 1.0
    plt.xlim([data_xmin, data_xmax])
    plt.ylim([data_ymin, data_ymax])


    title = in_title + "EvolutionR05"
    im = image.imread('PaperDataRun5.png')

    xmin = data_xmin -0.2
    xmax = data_xmax -0.055

    ymin = data_ymin -3.3
    ymax = data_ymax + 3.83

    plt.axes().set_aspect('equal', 'datalim')
    myaximage = ax.imshow(im, aspect=1, extent=[xmin, xmax, ymin, ymax],
                          zorder=+1)
    plt.tight_layout()
    file_path = path.join(environ["HOMETEL"], "examples", "gaia", "yen-t2d", "img")
    plt.savefig(file_path + sep + title + ".pdf", dpi=300)
    plt.savefig(file_path + sep + title + ".png", dpi=300)
    #plt.show()
    #plt.close()
    plt.clf()

if __name__ == "__main__":

    if len(sys.argv) < 2:
        sys.exit("No arguments please give an input file and outputname")

    #########################################################################
    #
    # factor is used for scaling (evolution/h0_factor = evolutionCut)
    # See the paper of Yen et al. 1995
    #########################################################################
    main(sys.argv[1], sys.argv[2])
