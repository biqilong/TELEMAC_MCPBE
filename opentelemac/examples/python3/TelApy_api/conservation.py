#!/usr/bin/env python3
"""
Example of a telapy telemac2d run (test case gouttedo)
"""
import sys
from os import path, chdir, environ, getcwd
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api', 'lib', 'api.pyf'))):
    print("  -> telapy not available doing nothing")
    sys.exit(0)

from telapy.api.cpl_t2d_sis import CplT2dSis
from mpi4py import MPI
import numpy as np

def main(recompile=True):
    """
    Main function of script

    @param recompile (Boolean) If True recompiling user fortran

    @retuns Value of ... at the end of the simulation
    """
    comm = MPI.COMM_WORLD

    root = environ.get('HOMETEL', path.join('..', '..', '..'))

    pwd = getcwd()

    chdir(path.join(root, 'examples', 'sisyphe', 'conservation'))

    # Creation of the instance Telemac3d
    study = CplT2dSis('t2d_conservation_api.cas', 'sis_conservation.cas',
                      user_fortran='user_fortran',
                      comm=comm, stdout=0, recompile=recompile)
    study.set_case()
    # Initalization
    study.init_state_default()
    # Run all time steps
    study.run_all_time_steps()
    val = study.t2d.get_array("MODEL.BOTTOMELEVATION")
    # Ending the run
    study.finalize()
    # Instance delete
    del study
    chdir(pwd)

    return val

if __name__ == "__main__":
    VAL1 = main()
    print("First run passed")
    VAL2 = main(recompile=False)
    print("Second run passed")
    assert np.array_equal(VAL1, VAL2)
    print("My work is done")
