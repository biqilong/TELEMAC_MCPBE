
# coding: utf-8

# <span style="float:left;">Licence CC BY-SA</span><span style="float:right;">Fabrice Zaoui&nbsp;</span><br/>
# ___

# This tutorial is intended for people who want to use the Genop optimizer. Genop (**Gen**etic **op**timizer) is a Python package implementing the Genetic Algorithm (GA) for a mono-objective minimization. GA is a derivative-free optimizer.  This metaheuristic mimics the natural evolution with the repeated application of operators (selection, mutation, crossover, etc.) in order to evolve a set of solutions towards the optimality. People interested in this class of algorithms may refer to [Genetic Algorithms](https://en.wikipedia.org/wiki/Genetic_algorithm) or [Genetic Programming](http://geneticprogramming.com/) for more information.
#
# Although Genop was primarly designed for the Telemac system, it is user-friendly and open to other fields of applications where derivatives are difficult to assess. Indeed, the software claims to be easy to figure out, efficient to run and supported.
#

## Genop as a Python package

# Genop can be imported as a Python v2 package. It is located with the Telemac distribution in the sub-folder:
#
# $HOMETEL/scripts/python27/TelApy/tools

# In[6]:

import sys
from os import path, environ
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api'))):
    print("  -> TelApy not available doing nothing")
    sys.exit(0)
# Changing of directory
import os
HOMETEL = os.environ.get('HOMETEL')
if HOMETEL:
    os.chdir(HOMETEL + '/scripts/python27/TelApy/tools')
else:
    raise Exception('Unable to change of directory:     please check the HOMETEL value.')


# Note: In order to import the package from any other path, one can change the PYTHONPATH environment variable.
#
# Once the Genop package is correctly located, it can be imported:

# In[7]:

from TelApy.tools.genop import genop


# Some basic information on Genop package can be obtained with:

# In[8]:

#help(genop)


# and attributes reachable from it with:

# In[9]:

dir(genop)


## Cost function

# The problem under consideration is formulated as a mathematical optimization seeking to minimize a cost function. With Genop the user has to define the cost function as a Python function taking as argument the (numpy array) vector of variables and returning the corresponding cost function value.
#
# For example, in the parent directory of Genop some test functions are defined in the file 'simul.py'

# In[10]:

import TelApy.tools.simul as simul


# In[11]:

#get_ipython().run_cell_magic(u'bash', u'', u'cat simul.py')


# Six analytical functions are defined in this test file. They can be all optimized with Genop in one shot using the script 'test_genop.py':

# In[12]:

#get_ipython().run_cell_magic(u'bash', u'', u'python test_genop.py')


## Problem definition with Genop

# Suppose that the problem concerns the minimization of the [Rosenbrock function](https://en.wikipedia.org/wiki/Rosenbrock_function):

# $$ F(x_1,x_2) = (1-x_1)^2 + 100 \times (x_2-x_1^2)^2$$

# This functions of two variables has a global optimum at $$(x_1,x_2)=(1,1)$$ where $$F_{opt}(1,1)=0$$

# As the function is already implemented in 'simul.py' the definition of the Genop problem will be:

# In[13]:

import numpy as np

# pointer to the function
f = simul.rosenbrock
# number of variables
n = 2
# lower and upper bounds for variable (optimizing in the range [-5., 5.])
bounds = np.zeros((n, 2))
bounds[0, 0] = -5.
bounds[0, 1] = 5.
bounds[1, 0] = -5.
bounds[1, 1] = 5.
# instantiation of a Genop problem with default values for parameters
mypb = genop.Genop()
# initialization of the problem for Genop
error = mypb.initialize(f, n, bounds)
if error:
    raise Exception('Unable to initialize Genop')


## Optimizing with Genop

# The Genop minimization with default valuesis straightforward:

# In[14]:

fcost, xopt = mypb.optimize()


# Genop returns all the values of optimal cost functions *fcost* and associated variables *xopt* throughout the minimization process.

# In[15]:

import matplotlib.pyplot as plt
plt.switch_backend('agg')

#get_ipython().magic(u'matplotlib inline')
plt.plot(fcost)
plt.grid()
plt.xlabel('Iteration number')
plt.ylabel('Cost function value')
plt.title('Rosenborck function minimized with Genop')
plt.show()


# In[16]:

x1, x2 = np.meshgrid(np.linspace(-5.,5.,201),np.linspace(-5.,5.,201))
plt.contourf(x1, x2, simul.rosenbrock([x1,x2]), np.linspace(0.,3000.,200))
plt.plot(*zip(*xopt), lw=4, color="green", marker="o", markerfacecolor="white")
plt.show()


### Default optimization parameters

# Usefull parameters for the optimization process can be changed by the user with the instantiation of a new Genop object:

# mypb = genop.Genop(popsize=20, pbcross=0.8, pbmut=0.05)

# where:
# * popsize is the size of the population
# * pbcross is the probability crossover
# * pbmut is the probability mutation

# It is also possible to change the maximum number of generations with the call to the optimize method:

# fcost, xopt = mypb.optimize(nbgen=30)

### Parallelism

# Althought GA requires many calls to cost functions assessment, it can be easily parallelised since each call is independant from other.
#
# Genop implements the *multiprocessing* Python package for this purpose.
#
# The number of processors to be used is indicated when calling the optimize method:

# fcost, xopt = mypb.optimize(nproc=2)
