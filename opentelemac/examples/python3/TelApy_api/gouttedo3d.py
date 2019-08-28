#!/usr/bin/env python3
"""
Example of a telapy telemac3d run (test case gouttedo)
"""
import sys
from os import path, chdir, environ, getcwd
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api', 'lib', 'api.pyf'))):
    print("  -> telapy not available doing nothing")
    sys.exit(0)
from telapy.api.t3d import Telemac3d
from mpi4py import MPI
import numpy as np

# Creation of the instance Telemac3d
def main(recompile=True):
    """
    Main function of script

    @param recompile (Boolean) If True recompiling user fortran

    @retuns Value of ... at the end of the simulation
    """
    comm = MPI.COMM_WORLD

    root = environ.get('HOMETEL', path.join('..', '..', '..'))

    pwd = getcwd()

    chdir(path.join(root, 'examples', 'telemac3d', 'gouttedo'))

    study = Telemac3d('t3d_gouttedo.cas', user_fortran='user_fortran',
                      comm=comm, stdout=0, recompile=recompile)

    study.set_case()
    # Initalization

    study.init_state_default()
    # Run all time steps
    ntimesteps = study.get("MODEL.NTIMESTEPS")
    for _ in range(ntimesteps):
        study.run_one_time_step()
        tmp = study.get_array("MODEL.IKLE")
        study.set_array("MODEL.IKLE", tmp)
        tmp2 = study.get_array("MODEL.IKLE")
        diff = abs(tmp2 - tmp)
        assert np.amax(diff) == 0
        tmp = study.get_array("MODEL.WATERDEPTH")
        study.set_array("MODEL.WATERDEPTH", tmp)
        tmp2 = study.get_array("MODEL.WATERDEPTH")
        diff = abs(tmp2 - tmp)
        assert np.amax(diff) < 1e-8
    # Ending the run
    study.finalize()
    # Instance delete
    del study
    chdir(pwd)

    return tmp2

if __name__ == "__main__":
    VAL1 = main()
    print("First run passed")
    VAL2 = main(recompile=False)
    print("Second run passed")
    assert np.array_equal(VAL1, VAL2)
    print("My work is done")
