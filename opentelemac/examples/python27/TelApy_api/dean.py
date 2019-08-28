#!/usr/bin/env python
"""
Example of a TelApy telemac2d run (test case pildepon)
"""
from __future__ import print_function
import sys
from os import path, environ
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api'))):
    print("  -> TelApy not available doing nothing")
    sys.exit(0)
from TelApy.api.wac import Tomawac
from mpi4py import MPI
from os import path, chdir, environ, getcwd

def main():
    """
    Main function of script
    """
    # Creation of the instance Telemac3d
    comm = MPI.COMM_WORLD

    root = environ.get('HOMETEL', path.join('..', '..', '..'))

    pwd = getcwd()

    chdir(path.join(root, 'examples', 'tomawac', 'dean'))

    wac = Tomawac('tom_dean.cas', user_fortran='user_fortran', comm=comm)
    wac.set_case()
    # Initalization
    wac.init_state_default()
    # Run all time steps
    wac.run_all_time_steps()
    # Running gretel
    comm.Barrier()
    # Ending the run
    wac.finalize()
    # Instance delete
    del wac
    chdir(pwd)

if __name__ == "__main__":
    main()
    main()
