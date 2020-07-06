#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium

@brief Run the partiotionning step
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
import sys
from os import path
import argparse
# ~~> dependencies towards other pytel/modules
from config import add_config_argument, update_config, CFGS
from execution.telemac_cas import TelemacCas
from utils.exceptions import TelemacException


def translate(cas):
    """
    Translating cas_file in french and english

    @param cas (TelemacCas) steering file
    """
    print("\n ~> Translating {}".format(path.basename(cas.file_name)))
    cas.write_fr_gb()

def sort(cas, keep_comments):
    """
    Sorting steering file using rubriques order

    @param cas (TelemacCas) steering file

    @parma keep_comments (bool) If true sort will append comments from original
    file at the end of the file
    """

    root, ext = path.splitext(cas.file_name)

    out_file = root + "_sorted" + ext

    print("\n ~> Sorting {}".format(path.basename(cas.file_name)))
    cas.write(out_file, keep_comments=keep_comments)

def check_cas(cas):
    """
    Does some check on the steering file

    @param cas (TelemacCas) steering file
    """
    if cas.lang == 'fr':
        default = 'DEFAUT'
    else:
        default = 'DEFAUT1'

    print("\n~> Checking steering file")

    print("\nChecking default values:\n")
    for key in cas.values:
        data = cas.dico.data[key]
        if default in data:
            if cas.values[key] == data[default]:
                print('{} set to default value {}'.format(key, data[default]))

def main():
    """ Main function of manip_cas.py """
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nLoading Options and Configurations\n'+72*'~'+'\n')
    parser = argparse.ArgumentParser(
        description='Translate a keyword')
    parser = add_config_argument(parser)
    parser.add_argument(
        "module",
        choices=['postel3d', 'telemac2d', 'telemac3d', 'tomawac', 'artemis',
                 'sisyphe', 'waqtel', 'khione', 'stbtel'],
        help="Name of the module for which to translate")
    parser.add_argument(
        "-t", "--translate", action="store_true",
        dest="translate", default=False,
        help="Generate a french and english version of the steering file "
             "(cas_file suffixed with _fr and _gb)")

    parser.add_argument(
        "-s", "--sort", action="store_true",
        dest="sort", default=False,
        help="Rewrites the steering file using rubriques to sort the keywords "
             "cas_file suffixed with _sorted")

    parser.add_argument(
        "--keep-comments", action="store_true",
        dest="keep_comments", default=False,
        help="When sorting will append all original comments "
             "at the end of the file")

    parser.add_argument(
        "cas_file",
        help="Name of the steering file to read")

    args = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_config(args)
    cfg = CFGS.configs[CFGS.cfgname]
    CFGS.compute_execution_info()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Searching for the dictionary associated with the steering case
    dico_file = path.join(cfg['MODULES'][args.module]['path'],
                          args.module+'.dico')
    if not path.exists(dico_file):
        raise TelemacException(\
            'Could not find the dictionary file: {}'.format(dico_file))
    cas = TelemacCas(args.cas_file, dico_file, check_files=False)

    check_cas(cas)

    if args.translate:
        translate(cas)
    if args.sort:
        sort(cas, args.keep_comments)

    print('\n\nMy work is done\n\n')
    sys.exit(0)


if __name__ == "__main__":
    main()
