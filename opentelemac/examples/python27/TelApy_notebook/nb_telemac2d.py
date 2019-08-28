
# coding: utf-8

# <span style="float:left;">Licence CC BY-SA</span><span style="float:right;">Fabrice Zaoui&nbsp;</span><br/>
# ___

# This tutorial is intended for people who want to run Telemac 2D in an interactive mode with the help of the Python programming language. The interactive mode means that communication with Telemac becomes possible throughout the simulation without having to stop it. One can easily set or get the value of any variables at each time step with the help of special communication functions: the [Application Programming Interfaces](https://en.wikipedia.org/wiki/Application_programming_interface) (API).
#
# By using the APIs, research and engineering with Telemac is encouraged in every fields where communication is crucial: optimization, code coupling, control system, sensitivity analysis and so on.

## Telemac 2D as a Python module

### Building the interface

# When using Telemac in a classic way (i.e. without API), you first have to compile all the Fortran sources of Telemac with (for example) the following command:

# In[ ]:

#get_ipython().run_cell_magic(u'bash', u'', u'compileTELEMAC.py')


# *Note: if an error occurs, check for the environment variables HOMETEL and SYSTELCFG before running the previous command.*

# If the compilation is successful, you will then run Telemac on your test case providing a steering file:

# In[ ]:

#get_ipython().run_cell_magic(u'bash', u'', u'telemac2d.py my_steering_file.cas')


# *Note: as the name of the steering file here is fictive, no computation is done*

import sys
from os import path, environ
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api'))):
    print("  -> TelApy not available doing nothing")
    sys.exit(0)
# The previous command requires all the static librairies to be linked with the Fortran user file in an executable. When using the API with Python, it is the dynamic libraries of the Telemac system that are used.

# The interface between the Telemac libraries and the Python language is automatically created using [f2py](http://docs.scipy.org/doc/numpy-dev/f2py/). This corresponds to the compilation of the Telemac API for Python. It has to be done by adding :
# options: api
# in the configuration file (systel.cfg)

### Using the interface

# Telemac/Python is now ready to be used as a hydrodynamic 2D solver with the import system:

# In[ ]:

import TelApy.api.t2d


# The module 'TelApy.api.t2d' is located in the '$HOMETEL/scripts/python27/TelApy/api' directory. If an error occurs while attempting to import, check the value of the environment variable PYTHONPATH. Alternatively you can also work with the file 'apiT2d.py' listed in your working directory.
#

# In[ ]:

#get_ipython().run_cell_magic(u'bash', u'', u'echo $PYTHONPATH')


# The module 'TelApy.api.t2d' defines a class named 'Telemac2d'. The user will have to instantiate an object from this class in order to run Telemac with all the facilities offered by the API.

# In[ ]:

# Changing of directory : 'examples' directory of the Telemac sources
import os
HOMETEL = os.environ.get('HOMETEL')
os.chdir(HOMETEL + '/examples/telemac2d/breach')

# Instantiation of a Telemac2d object from the test case named 'breach'
# - steering file : 't2d_breach.cas'
# - language : french (1) or english (2: default)
from TelApy.api.t2d import Telemac2d
from mpi4py import MPI
my_case = Telemac2d('t2d_breach.cas', lang=1, comm=MPI.COMM_WORLD)
my_case


# An object 'my_case' is now created. This object offers some useful methods to communicate with the Telemac computational kernel. These (non exaustive) methods are seen with:

# In[ ]:

dir(my_case)


## API description

# The methods listed above give access to the Fortran Telemac API and to some extra functionalities based on Python packages like 'numpy' or 'matplotlib'. The list is still uncomplete and will be improved in future.

#### Get the value of a Telemac variable

# In[ ]:

#help(my_case.get)


#### Get a node number

# In[ ]:

#help(my_case.get_node)


#### Get an element number

# In[ ]:

#help(my_case.get_elem)


#### Error message from Telemac

# In[ ]:

#help(my_case.get_error_message)


#### Mesh coordinates and connectivity

# In[ ]:

#help(my_case.get_mesh)


#### Hydraulic state

# In[ ]:

#help(my_case.get_state)


# In[ ]:

#help(my_case.set_state)


#### Read the steering file

# In[ ]:

#help(my_case.set_case)


#### State initialisation

# In[ ]:

#help(my_case.init_state_default)


#### Save a state

# In[ ]:

#help(my_case.save_state)


#### Back to a saved state

# In[ ]:

#help(my_case.restore_state)


#### Run Telemac for one time step

# In[ ]:

#help(my_case.run_one_time_step)


#### Run Telemac for all the time steps

# In[ ]:

#help(my_case.run_all_time_steps)


#### Change the value of a Telemac variable

# In[ ]:

#help(my_case.set)


#### Plotting the mesh

# In[ ]:

#help(my_case.show_mesh)


#### Plotting the state

# In[ ]:

#help(my_case.show_state)


## Telemac Variables

# All the variables and parameters available with the API to set or get a value can be seen with:

# In[ ]:

varnames, varinfo = my_case.list_variables()


# In[ ]:

for a, b in zip(varnames, varinfo):
    print a + '\t\t\t' + b


## Example of use

# Knowing now the API and the variables, a Telemac computation on the test example named 'breach' could be:

### Read the steering file and do allocations

# In[ ]:

my_case.set_case()


### Initialisation

# In[ ]:

my_case.init_state_default()


### Mesh view

# In[ ]:

#get_ipython().magic(u'matplotlib inline')
import matplotlib.pyplot as plt
plt.switch_backend('agg')
my_figure = my_case.show_mesh(visu2d=False)


### Simulation

# In[ ]:

for i in xrange(100):
    my_case.run_one_time_step()


### Current state

#### Values

# In[ ]:

import numpy as np

h, u, v = my_case.get_state()

print 'Mean water height (m) = ', h.mean()
print 'Max velocity (m/s) = ', np.amax(np.sqrt(u * u + v * v))


#### Graph

# In[ ]:

my_new_figure = my_case.show_state(show=True)


#### Save

# In[ ]:

my_case.save_state()
my_case.hsave
my_case.usave
my_case.vsave


#### Change

# In[ ]:

# to null velocity
number_of_nodes = my_case.get('MODEL.NPOIN')
for i in xrange(number_of_nodes):
    my_case.set('MODEL.VELOCITYU', 0., i=i)
    my_case.set('MODEL.VELOCITYV', 0., i=i)

h, u, v = my_case.get_state()
print 'Mean velocity (m/s) = ', np.mean(np.sqrt(u * u + v * v))


#### Restore

# In[ ]:

err = my_case.restore_state()

h, u, v = my_case.get_state()
print 'Max velocity (m/s) = ', np.amax(np.sqrt(u * u + v * v))


### Parameters

# In[ ]:

# bottom elevation at node no. 102
my_case.get('MODEL.BOTTOMELEVATION', 102)


# In[ ]:

# friction coefficient (Strickler value) at node no. 500
my_case.get('MODEL.CHESTR', 500)


### Deletion

# In[ ]:

my_case.finalize()
del my_case

