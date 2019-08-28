#!/usr/bin/env python3
r"""
    @author TELEMAC-MASCARET Consortium
    @brief Function to run a steering file using the api
"""
# Class Telemac2d import
from telapy.api.t2d import Telemac2d
from telapy.api.t3d import Telemac3d
from telapy.api.art import Artemis
from telapy.api.wac import Tomawac
from mpi4py import MPI
from argparse import ArgumentParser
import string


SCRIPT_TEMPLATE = """\
#!/usr/bin/env python3
# Class {module} import
from __future__ import print_function
from telapy.api.{short} import {module}
from mpi4py import MPI
# Creation of the instance Telemac2d
STEERING_FILE = "{steering_file}"
USER_FORTRAN = "{fortran_file}"
COMM = MPI.COMM_WORLD
# Initialising {module} instance
STUDY = {module}(STEERING_FILE, user_fortran=USER_FORTRAN, comm=COMM)
# Reading steering file
STUDY.set_case()
# Doing initialisation
STUDY.init_state_default()
# Time step loop
STUDY.run_all_time_steps()
# Ending computation
STUDY.finalize()
# Deleting {module} instance
del STUDY
"""

SHORT = {'telemac2d':'t2d',
         'telemac3d':'t3d',
         'tomawac':'wac',
         'sisyphe':'sis',
         'artemis':'art'}


def run(module, steering_file, fortran_file):
    """
    Running a full study

    @param module Name of the module
    @param steering_file Name of the steering file
    @param fortran_file Name of the fortran file
    @param double_run If true running main computation twice
    """
    # Creation of the instance Telemac2d
    comm = MPI.COMM_WORLD
    fortran = None if fortran_file == '' else fortran_file
    if module == "telemac2d":
        study = Telemac2d(steering_file, user_fortran=fortran, comm=comm)
    elif module == "telemac3d":
        study = Telemac3d(steering_file, user_fortran=fortran, comm=comm)
    elif module == "artemis":
        study = Artemis(steering_file, user_fortran=fortran, comm=comm)
    elif module == "tomawac":
        study = Tomawac(steering_file, user_fortran=fortran, comm=comm)
    # Running telemac
    study.set_case()
    study.init_state_default()
    study.run_all_time_steps()
    comm.Barrier()
    study.finalize()
    # Instance delete
    del study

def dump_script(module, steering_file, fortran_file, script_file):
    """
    Running a full study

    @param module Name of the module
    @param steering_file Name of the steering file
    @param fortran_file Name of the fortran file
    @param double_run If true running main computation twice
    """

    script = SCRIPT_TEMPLATE.format(\
            steering_file=steering_file,
            fortran_file=None if fortran_file == '' else fortran_file,
            module=string.capwords(module),
            short=SHORT[module])

    with open(script_file, 'w') as fobj:
        fobj.write(script)

if __name__ == "__main__":
    # Define a parser for the program options
    PARSER = ArgumentParser()
    PARSER.add_argument(\
             "module",
             choices=['telemac2d', 'telemac3d', 'artemis', 'tomawac'],
             help="name of the steering file")
    PARSER.add_argument(\
             "steering_file",
             help="name of the steering file")
    PARSER.add_argument(\
             "-f", "--fortran-file",
             dest="fortran_file",
             default="",
             help="name of the fortran file")
    PARSER.add_argument(\
             "--double-run",
             dest="double_run",
             action="store_true",
             help="Running main computation twice")
    PARSER.add_argument(\
             "-o", "--output-script",
             dest="output_script",
             default="",
             help="Will generate a python script running the case")
    # reading the options
    ARGS = PARSER.parse_args()

    if ARGS.output_script != '':
        dump_script(ARGS.module, ARGS.steering_file, ARGS.fortran_file,
                    ARGS.output_script)
    else:
        run(ARGS.module, ARGS.steering_file, ARGS.fortran_file)
        if ARGS.double_run:
            run(ARGS.module, ARGS.steering_file, ARGS.fortran_file)



    print("My work is done")
