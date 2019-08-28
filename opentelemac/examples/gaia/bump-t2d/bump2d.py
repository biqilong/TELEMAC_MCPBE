#!/usr/bin/env python
"""Calculates the spread angle computed with the bottom 0.1 m isolines of near-the-end and last time step


Please notice:
    - The following lines are needed on a server without graphical screen
        import matplotlib as mpl
        mpl.use('Agg')

"""
import matplotlib as mpl
mpl.use('Agg')
from os import getcwd, path, sep
import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.tri as tri
from math import atan, degrees

################################################################################
#####    Will allow to use pytel scripts from the test case folder    ##########
################################################################################
#actualPath = getcwd()
#pathsplit = actualPath.split(sep)
#pytelpath = ""
#pytelpath += sep
#
#for i in range(len(pathsplit)):
#    if "example" in pathsplit[i]:
#        break
#    if len(pathsplit[i]) > 0:
#        pytelpath = path.join(pytelpath, pathsplit[i])
#
#pytelpath = path.join(pytelpath, 'scripts', 'python27')
#sys.path.append(pytelpath)


################################################################################
#####    Dependencies towards other pytel/modules                     ##########
################################################################################

from data_manip.formats.selafin import Selafin

__authors__ = "Rebekka Kopmann, Leopold Stadler"
__maintainer__ = "Rebekka Kopmann"
__version__ = "1.0"



def get_position_of_variable(varnames, search_name):
    """
    Return position of a variable

    @param varnames (list) List of variables names
    @param search_name (string) The one we are looking for
    """

    for i, name in enumerate(varnames):
        if name.strip() == search_name:
            return i

    print("Exit Variable %s not found in varnames"%(search_name))
    sys.exit()

    return

def main(selafinfile, title):

    #########################################################################
    #
    # factor is used for scaling (evolution/h0_factor = evolutionCut)
    # See the paper of Yen et al. 1995
    #########################################################################

    slf = Selafin(selafinfile)
    mesh = np.array(slf.ikle3)
    triang = tri.Triangulation(slf.meshx, slf.meshy, mesh)

    #last time step
    last_index = 1
    time_pos = len(slf.tags["times"]) - last_index
    values = slf.get_values(time_pos)
    var_pos = get_position_of_variable(slf.varnames, 'BOTTOM')
    bottom_end = values[var_pos]
    t_end = slf.tags["times"][time_pos]
    t_end = "t = %1.2f h" %(t_end/3600)

    #near the end
    near_last_index = 3
    time_pos = len(slf.tags["times"]) - near_last_index
    values = slf.get_values(time_pos)
    var_pos = get_position_of_variable(slf.varnames, 'BOTTOM')
    bottom = values[var_pos]
    t_near_end = slf.tags["times"][time_pos]
    t_near_end = "t = %1.2f h" %(t_near_end/3600)


    fig = plt.figure()
    ax = fig.add_subplot(111)
    #ax.axis('off')
    levels = np.asarray([-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9])


    #first we extract contour at the end
    triplotcontour = plt.tricontour(slf.meshx, slf.meshy, mesh, bottom_end,
                                    levels, extend='both', alpha=0.0)
    triplotcontourf = plt.tricontourf(slf.meshx, slf.meshy, mesh, bottom_end,
                                      levels, extend='both')
    plt.triplot(slf.meshx, slf.meshy, mesh, 'k-', alpha=0.1)

    cbar = plt.colorbar()
    cbar.set_label('BOTTOM')

    print(triplotcontour.levels)
    level = 4
    print("plot at level ", triplotcontour.levels[level])

    p = triplotcontour.collections[4].get_paths()[0]
    v = p.vertices
    plt.plot(v[:, 0], v[:, 1], "k-.", label=t_end)

    minXYEnd = [10.0E+20, 10.0E+20]
    for xy in v:
        if xy[1] < minXYEnd[1]:
            minXYEnd[0] = xy[0]
            minXYEnd[1] = xy[1]

    #second we extract contour near the end
    triplotcontour = plt.tricontour(slf.meshx, slf.meshy, mesh, bottom,
                                    levels, extend='both', alpha=0.0)
    print(triplotcontour.levels)
    level = 4
    print("plot at level ", triplotcontour.levels[level])

    p = triplotcontour.collections[4].get_paths()[0]
    v = p.vertices
    plt.plot(v[:, 0], v[:, 1], "k--", label=t_near_end)

    minXY = [10.0E+20, 10.0E+20]
    for xy in v:
       if minXY[1] > xy[1]:
          minXY[0] = xy[0]
          minXY[1] = xy[1]

    plt.plot(minXY[0], minXY[1], "ko")
    plt.plot(minXYEnd[0], minXYEnd[1], "ko")
    plt.plot([0.0, 1000.0], [0.0, 0.0], "k-")
    plt.plot([minXY[0], minXYEnd[0]], [minXY[1], minXYEnd[1]], "g-")


    #line through bottom (a) and bottom end (b)
    a0 = minXY[0]
    a1 = minXY[1]
    b0 = minXYEnd[0]
    b1 = minXYEnd[1]

    m = (a1-b1)/(a0 - b0)
    b = a1-m*a0

    #spreading angle
    alpha = degrees(atan(abs(a1-b1)/abs(a0-b0)))
    print("alpha", alpha)

    xLine = [400, 500, 900]
    yLine = [value *m + b for value in xLine]
    plt.plot(xLine, yLine, "k-")
    plt.legend()
    plt.axes().set_aspect('equal')
    #myaximage = ax.imshow(im, aspect='auto', extent=[xmin, xmax, ymin, ymax], zorder=-1)
    #plt.tight_layout()
    plt.xlabel("x (m)")
    plt.ylabel("y (m)")
    plt.xlim([0, 1000])
    plt.ylim([-500, 500])
    plt.title(r"spread angle $\alpha$ = %1.2f"%(alpha))
    plt.savefig(title + ".pdf", dpi=300)
    plt.savefig(title + ".png", dpi=300)
    #plt.show()
    #plt.close()
    plt.clf()

if __name__ == "__main__":

   if len(sys.argv) < 2:
      sys.exit("No arguments please give an input file and outputname")

   main(sys.argv[1], sys.argv[2])


