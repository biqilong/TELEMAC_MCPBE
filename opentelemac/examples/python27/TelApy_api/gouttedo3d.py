#!/usr/bin/env python
"""
Example of a TelApy telemac3d run (test case gouttedo)
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
from TelApy.api.t3d import Telemac3d
from mpi4py import MPI
from os import path, chdir, environ, getcwd
# Creation of the instance Telemac3d
def main():
    """
    Main function of script
    """
    comm = MPI.COMM_WORLD

    root = environ.get('HOMETEL', path.join('..', '..', '..'))

    pwd = getcwd()

    chdir(path.join(root, 'examples', 'telemac3d', 'gouttedo'))

    t3d = Telemac3d('t3d_gouttedo.cas', user_fortran='user_fortran', comm=comm)

    t3d.set_case()
    # Initalization
    varnames, varinfo = t3d.list_variables()
    for name, info in zip(varnames, varinfo):
        print(name)
        print(info)

    t3d.init_state_default()
    # Run all time steps
    t3d.run_all_time_steps()
    # Ending the run
    t3d.finalize()
    # Instance delete
    del t3d
    chdir(pwd)

if __name__ == "__main__":
    main()
    main()
