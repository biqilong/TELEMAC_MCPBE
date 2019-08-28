
# coding: utf-8

# <span style="float:left;">Licence CC BY-SA</span><span style="float:right;">Fabrice Zaoui - Cedric Goeury&nbsp;</span><br/>
# ___
import sys
from os import path, environ
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api'))):
    print("  -> TelApy not available doing nothing")
    sys.exit(0)

# This tutorial is intended for people who want an example showing how to optimize a Telemac 2D case with a genetic algorithm based on Genop Python module.

## Import Optimizer from TelApy

# In[ ]:

from TelApy.tools import genop


# The module 'genop' is located in the '$HOMETEL/scripts/python27/TelApy/tools' directory. If an error occurs while attempting to import, check the value of the environment variable PYTHONPATH.
#

# 'genop' uses a meta-heuristic (the Genetic Algorithm) for a mono-objective optimization.

# In[ ]:

#help(genop)


## The Telemac2d test case

# A problem is created with the 'estimation' example located in '$HOMETEL/examples/telemac2d/estimation'.

# The test case 'estimation' is dedicated to the automatic calibration problem. This Telemac-2D model is composed of 551 triangular mesh elements. On the upstream of the model, an imposed flow type boundary condition is used. On downstream, a water depth is applied.

# The script 'studyT2d.py' has been written to manage this case with the Telemac API (initialization of the case, run of the case, get some important values on the physical variables).

# In[ ]:

from TelApy.tools.studyT2d import StudyTelemac2D
import numpy as np
import os


## Goal

# The calibration aims at minimizing the error between observations and Telemac computations on the water depths. Six observations are available on the middle of the domain. A norm is used to evaluate the error as follows:

# In[ ]:

def estimation(CHESTR):
    YObs = [5.000000000000000000e-01, 7.517873224063527093e-01,             7.517873219825667030e-01, 7.517873219442824384e-01,             7.517873221409325790e-01, 7.517873218929342904e-01]
    YObs = np.asmatrix(np.ravel(YObs)).T # six observations
    Fx = study.HX(CHESTR[0]) # Telemac computation with new friction coefficient
    Res = np.linalg.norm(YObs -Fx) # The norm evaluation
    return np.array(Res)


## Reading the case

# In[ ]:

# Changing of directory : 'examples' directory of the Telemac sources
HOMETEL = os.environ.get('HOMETEL')
os.chdir(HOMETEL + '/examples/telemac2d/estimation')


# In[ ]:

#  Telemac 2d files
studyFiles={'t2d.f'   :'user_fortran',            't2d.cas' :'t2d_estimation_basic.cas',            'f2d.slf' :'f2d_estimation.slf',            't2d.geo' :'geo_estimation.slf' }


# In[ ]:

# Observation times
obs_time = [0.0, 2000.0, 4000.0, 6000.0, 8000.0, 10000.0]
# A polygon is defined to get the observation node
poly = [(246.114, 57.3554), (261.13, 57.0189), (261.802, 45.018), (245.666, 45.3545)]


## Initialization

# In[ ]:

# Class Instantiation
study = StudyTelemac2D(studyFiles, obs_time, poly)


## Run the optimization

# WARNINGS: The optimization will take time because the required number of Telemac runs is significant.

# In[ ]:

# pointer to the function
fcost = estimation
# number of variables
nvar = 1
# lower and upper bounds for variable (optimizing in the range [20., 60.])
bounds = np.zeros((nvar, 2))
bounds[0, 0] = 20.
bounds[0, 1] = 60.

# instantiation of a Genop problem with default values for parameters
mypb = genop.Genop()
# initialization of the problem for Genop
error = mypb.initialize(fcost, nvar, bounds)
if error:
    raise Exception('Unable to initialize Genop')

# run optimization in parallel mode
# ...comments: change parameter nproc depending on your machine
fcost, xopt = mypb.optimize(nproc=8)


# In[ ]:

# Print all the optimal solutions found for the friction coefficient
print(xopt)


# In[ ]:

# Print the corresponding errors (2-norm)
print(fcost)

