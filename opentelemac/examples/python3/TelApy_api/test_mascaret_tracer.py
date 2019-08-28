#!/usr/bin/env python3
"""
Example of a telapy mascaret run (test case test1)
"""
import sys
import numpy as np
from os import path, chdir, environ, getcwd
from telapy.api.masc import Mascaret
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api', 'lib', 'api.pyf'))):
    print("  -> telapy not available doing nothing")
    sys.exit(0)


def main():
    root = environ.get('HOMETEL', path.join('..', '..', '..'))

    pwd = getcwd()

    chdir(path.join(root, 'examples', 'mascaret', 'Test_Tracer'))
    # Model create
    masc = Mascaret()
    masc.create_mascaret(iprint=0)

    #  Mascaret files & import
    files_name = ['Cas_5_THERMIC.xcas', 'mascaret0.geo', 'mascaret0.lig',
                  'mascaret0_0.loi', 'mascaret0_1.loi', 'Cas_5_THERMIC.lis',
                  'Cas_5_THERMIC.opt', 'mascaret0.conc',
                  'mascaret0_tracer0.loi',
                  'mascaret0.phy', 'mascaret0.met', 'Cas_5_THERMIC.tra_lis',
                  'Cas_5_THERMIC.tra_opt']
    files_type = ['xcas', 'geo', 'lig', 'loi', 'loi', 'listing', 'res',
                  'tracer_conc', 'tracer_loi', 'tracer_parphy', 'tracer_meteo',
                  'tracer_listing', 'tracer_res']
    study_files = [files_name, files_type]
    masc.import_model(study_files[0], study_files[1])

    # Initialization
    masc.init_hydro_from_file('mascaret0.lig')

    # Set state from file
    masc.init_tracer_state()

    # Set state from a vector
    # nb_sect, _, _ = masc.get_var_size('Model.X')
    # nb_trac = masc.get('Model.Tracer.Number')
    # Conc = np.zeros((nb_sect,nb_trac),dtype=np.float64)
    # Conc[:,:] = 0.0
    # masc.init_tracer(Conc)

    # Computation
    InitTime = masc.get('Model.InitTime')
    EndTime = masc.get('Model.MaxCompTime')
    DT = masc.get('Model.DT')
    masc.compute(InitTime, EndTime, DT)

    # Get some results
    _, _ = masc.get_hydro()

    # Tracer results
    nb_sect, nb_trac, _ = masc.get_var_size('State.Tracer.Concentration')
    Conc = np.zeros((nb_sect, nb_trac), dtype=float)
    Conc = masc.get_tracer()
    conc_get1 = masc.get('State.Tracer.Concentration', 0, 0)
    conc_get2 = masc.get('State.Tracer.Concentration', 0, nb_trac-1)
    conc_get3 = masc.get('State.Tracer.Concentration', nb_sect-1, nb_trac-1)

    # Delete Mascaret
    masc.delete_mascaret()

    chdir(pwd)


if __name__ == "__main__":
    main()
    main()
