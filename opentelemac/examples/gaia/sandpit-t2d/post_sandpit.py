#!/usr/bin/env python
#

__author__="jcp"
__date__ ="$23-Apr-2013 10:24:01$"

from os import getcwd,path,sep
import sys
import numpy as np
from numpy import array,ones
import matplotlib.pyplot as plt

################################################################################
#####    Will allow to use pytel scripts from the test case folder    ##########
################################################################################

YouAreHere =  getcwd()
pathsplit = YouAreHere.split(sep)
p = pathsplit[0]+sep
for i in range(len(pathsplit[0:-6])):
   i+=1
   p = path.join(p,pathsplit[i])
pytelpath = path.join(path.join(p,'scripts') ,'python3')
sys.path.append(pytelpath)

################################################################################
#####    Dependencies towards other pytel/modules                     ##########
################################################################################

from data_manip.formats.selafin import Selafin
from data_manip.extraction.parser_selafin import subset_variables_slf, get_value_polyline_slf
from data_manip.extraction.parser_csv import CSV
from pretel.meshes import cross_mesh, slice_mesh
from utils.parser_strings import parse_array_point

################################################################################
#####                    MAIN PROGRAM                                 ##########
################################################################################

if __name__ == "__main__":

    ############################################################################
    #####                    Importing data                               ######
    ############################################################################

    # Cross section from Selafin file

    slf = Selafin("sis_sandpit.slf")
    slf.set_kd_tree()
    slf.set_mpl_tri()

    variable = 'bottom:line'
    coordinates = '(50.0;0.5)(130.0;0.5)'
    timef = [20]

    vars = subset_variables_slf(variable,slf.varnames)
    xyo = []; zpo = []
    for xyi,zpi in parse_array_point(coordinates,slf.nplan):
       if type(xyi) == type(()):
           xyo.append(xyi)
       else:
           xyo.append( (slf.meshx[xyi],slf.meshy[xyi]) )
       for p in zpi:
           if p not in zpo: zpo.append(p)
    xys,support2d = slice_mesh(xyo,slf.ikle2,slf.meshx,slf.meshy,slf.tree)
    support3d = []
    for s2d in support2d: support3d.append( (s2d,zpo) )
    data = get_value_polyline_slf(slf.file,slf.tags,timef,support3d,slf.nvar,slf.npoin3,slf.nplan,vars)
    # Distance d-axis
    distot = 0.0
    d = [ distot ]
    for xy in range(len(xys)-1):
        distot += np.sqrt( np.power(xys[xy+1][0]-xys[xy][0],2) + np.power(xys[xy+1][1]-xys[xy][1],2) )
        d.append(distot)

    Xaxis = d
    BedFinal = data[0][0][0]

    # Experiment data from CSV file
    # always write the variable name in lower case

    csv = CSV()
    csv.get_file_content('experiment_data.csv')
    x,(vars,values) = csv.get_columns('x;bedinitial;bedmeasured')

    BedInitial = values[0]*10
    BedMeasured = values[1]*10

    ############################################################################
    #####                        CALCS                                    ######
    ############################################################################

    o = ones(81)*50
    Xmodel = Xaxis + o

    ############################################################################
    #####             Wrtting data in CSV file                            ######
    ############################################################################

    # the first two values of a column must be the variable name and the
    # variable unit (without any space)

    #ModelBed = ['ModelBedLevel','m'] + BedFinal.tolist()
    #ExpBed = ['MeasuredBedLevel','m'] + BedMeasured.tolist()

    #columns = [ModelBed,ExpBed]
    #putDataCSV('Bed_level.csv',columns)

    ############################################################################
    #####                      PLOTS                                      ######
    ############################################################################

    fig = plt.figure()

    plt.plot(x[1]*10,BedInitial,'k',label='Initial bed')
    plt.plot(x[1]*10,BedMeasured, 'b', label='Measured')
    plt.plot(Xmodel,BedFinal, 'r', label='Modelled')
    plt.xlabel('location(m)')
    plt.ylabel('Bed level (m)')

    plt.legend(loc='lower right')
    plt.title('Comparison of model and experiment morphology' )
    filename = 'Sandpit_2_profiles'+ '.png'
    plt.savefig(filename, dpi=100)
    print('Wrote file', filename)
    plt.clf()

    sys.exit(0)
