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
from telapy.api.art import Artemis
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

    chdir(path.join(root, 'examples', 'artemis', 'bj78'))

    # Creation of the instance Telemac3d
    study = Artemis('art_bj78.cas', user_fortran='f90', comm=comm, stdout=0,
                    recompile=recompile)
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

        tmp = study.mpi_get_array("MODEL.X")
        study.mpi_set_array("MODEL.X", tmp)
        tmp2 = study.mpi_get_array("MODEL.X")
        diff = abs(tmp2 - tmp)
        assert np.amax(diff) < 1e-8
    val = study.get_array("MODEL.WAVEPHASE")
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
