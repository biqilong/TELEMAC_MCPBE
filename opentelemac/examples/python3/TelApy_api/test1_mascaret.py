#!/usr/bin/env python3
"""
Example of a telapy mascaret run (test case test1)
"""
import sys
from telapy.api.masc import Mascaret
from os import path, chdir, environ, getcwd
if(not path.exists(path.join(environ.get('HOMETEL', ''),
                             'builds',
                             environ.get('USETELCFG', ''),
                             'wrap_api', 'lib', 'api.pyf'))):
    print("  -> telapy not available doing nothing")
    sys.exit(0)


def main():
    "Main function"
    root = environ.get('HOMETEL', path.join('..', '..', '..'))

    pwd = getcwd()

    chdir(path.join(root, 'examples', 'mascaret', 'Test1'))
    # Model create
    masc = Mascaret()
    masc.create_mascaret(iprint=0)

    #  Mascaret files & import
    files_name = ['sarap.xcas', 'geometrie', 'hydrogramme.loi',
                  'limnigramme.loi', 'mascaret0.lis', 'mascaret0.opt']
    files_type = ['xcas', 'geo', 'loi', 'loi', 'listing', 'res']
    study_files = [files_name, files_type]
    masc.import_model(study_files[0], study_files[1])

    # Initialization
    npoin = masc.get_var_size('Model.X')[0]
    masc.init_hydro([0.]*npoin, [0.]*npoin)

    # Steady state computation with one step
    masc.compute(0., 1., 1.)

    # Get some results
    _, _ = masc.get_hydro()

    # Delete Mascaret
    masc.delete_mascaret()

    chdir(pwd)

if __name__ == "__main__":
    main()
    main()
