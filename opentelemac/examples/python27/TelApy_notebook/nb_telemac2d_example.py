
# coding: utf-8

# <span style="float:left;">Licence CC BY-SA</span><span style="float:right;">Fabrice Zaoui&nbsp;</span><br/>
# ___

# This tutorial is intended for people who want an example showing how to run Telemac 2D in an interactive mode with the help of the Python programming language.

## Import Telemac2D as a Python module

# In[2]:

import sys
from os import path, environ
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api'))):
    print("  -> TelApy not available doing nothing")
    sys.exit(0)


# The module 'TelApy.api.t2d' is located in the '$HOMETEL/scripts/python27/TelApy/api' directory. If an error occurs while attempting to import, check the value of the environment variable PYTHONPATH. Alternatively you can also work with the file 'apiT2d.py' listed in your working directory.
#

## Problem instantiation

# A problem is created from the "breach" example

# In[3]:

# Changing of directory : 'examples' directory of the Telemac sources
import os
HOMETEL = os.environ.get('HOMETEL')
os.chdir(HOMETEL + '/examples/telemac2d/breach')

# Instantiation of a Telemac2d object from the test case named 'breach'
# - steering file : 't2d_breach.cas'
# - language : french (1) or english (2: default)
from TelApy.api.t2d import Telemac2d
from mpi4py import MPI
my_problem = Telemac2d('t2d_breach.cas', lang=1, comm=MPI.COMM_WORLD)


# An object 'my_problem' is now created.

## Read the steering file

# In[4]:

my_problem.set_case()


## State initialisation

# In[5]:

my_problem.init_state_default()


## Mesh view
import matplotlib.pyplot as plt
plt.switch_backend('agg')

# In[6]:

my_figure = my_problem.show_mesh()


## State view

# In[7]:

my_new_figure = my_problem.show_state()


## Get the number of time steps

# In[8]:

number_of_time_steps = my_problem.get('MODEL.NTIMESTEPS')


## New array to save the value of a water depth

# In[9]:

import numpy as np

H_save = np.zeros((number_of_time_steps,))


## Get the node number corresponding to the coordinates (2030, 40)

# In[10]:

H_node = my_problem.get_node(2030., 40.)
print H_node


## Simulation and save the water depth

# In[11]:

for i in xrange(number_of_time_steps):
    sys.stdout.write('\r')
    # the exact output you're looking for:
    percent = 100*i/(max(number_of_time_steps-1,1))
    sys.stdout.write("[%-20s] %d%%" % ('='*(percent/5), percent))
    sys.stdout.flush()

    my_problem.run_one_time_step()

    H_save[i] = my_problem.get('MODEL.WATERDEPTH', i=H_node-1)


## Plot the water depth vs time

# In[12]:


plt.plot(H_save)
plt.grid()
plt.xlabel("Number of time steps")
plt.ylabel("Water level (m)")
plt.show()


## And finally, delete the case

# In[13]:

ierr = my_problem.finalize()

del my_problem

