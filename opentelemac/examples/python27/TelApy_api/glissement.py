#!/usr/bin/env python
"""
Example of a TelApy sisyphe run (test case bosse-analyt)
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
from TelApy.api.sis import Sisyphe
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

    chdir(path.join(root, 'examples', 'sisyphe', 'glissement'))

    sis = Sisyphe("sis_glissement.cas", user_fortran='user_fortran', comm=comm)

    sis.set_case()

    varnames, varinfo = sis.list_variables()
    for name, info in zip(varnames, varinfo):
        print(name)
        print(info)

    sis.init_state_default()

    sis.run_all_time_steps()

    sis.finalize()

    del sis

    chdir(pwd)

if __name__ == "__main__":
    main()
    main()
