#!/usr/bin/env python
"""
   Function for validation of the Python API
"""

from __future__ import print_function
import os
import glob
import shutil
import argparse
import filecmp
from os import path, sep, walk, chdir, remove, environ, mkdir, \
               listdir
from config import parseConfigFile
# ~~> dependencies towards other pytel/modules
from utils.messages import MESSAGES
from parsers.parserKeywords import scanCAS, readCAS, getKeyWord, \
                                   scanDICO, getIOFilesSubmit
from utils.files import getFileContent


MODULE_HANDLED = ['telemac2d', 'telemac3d', 'artemis', 'tomawac']
KEY_RES = {'telemac2d':'RESULTS FILE', \
           'telemac3d':'3D RESULT FILE',
           'tomawac':'2D RESULTS FILE',
           'artemis':'RESULTS FILE'}

def run_telemac_api(module, cas, ncsize, user_fortran):
    """
    Running a cas using the api

    @param cas The name of the steering file
    @param ncsize Number of parallel processors
    @param ncsize Name of the user fortran None if none
    """
    cmd = "mpiexec -n {ncsize} template.py {module} {cas} {fortran} > run.log"

    if user_fortran is not None:
        fortran = " -f "+user_fortran
    else:
        fortran = ''

    print(cmd.format(module=module, cas=cas, ncsize=ncsize, fortran=fortran))
    os.system(cmd.format(module=module, cas=cas, ncsize=ncsize,
                         fortran=fortran))

    passed = False
    with open('run.log', 'r') as fobj:
        for line in fobj.readlines():
            if "My work is done" in line:
                passed = True

    return passed

def run_telemac_normal(module, cas, ncsize):
    """
    Normale run of telemac

    @param cas The name of the steering file
    @param ncsize Number of parallel processors
    """
    cmd = module+'.py --ncsize='+str(ncsize)+' '+cas+' > run.log'
    print(cmd)
    os.system(cmd)

    passed = False
    with open('run.log', 'r') as fobj:
        for line in fobj.readlines():
            if "My work is done" in line:
                passed = True

    return passed

def process_config(config_name, config_file, root_dir):
    """
       Main function

       :param: config_name Name of the telemac configuration
       :param: config_file Name of the configuration file
       :param: root_dir Path to the root folder of telemac
    """

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # path to the root
    hometel = ''
    if 'HOMETEL' in environ:
        hometel = environ['HOMETEL']
    if root_dir == '':
        root_dir = hometel
    # user configuration name
    usetelcfg = ''
    if 'USETELCFG' in environ:
        usetelcfg = environ['USETELCFG']
    if config_name == '':
        config_name = usetelcfg
    # user configuration file
    systelcfg = path.join(hometel, 'configs')
    if 'SYSTELCFG' in environ:
        systelcfg = environ['SYSTELCFG']
    if config_file != '':
        systelcfg = config_file
    if path.isdir(systelcfg):
        systelcfg = path.join(systelcfg, 'systel.cfg')
    config_file = systelcfg

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
    if not path.isfile(config_file):
        print('\nNot able to get to the configuration file: '\
              + config_file + '\n')
        dircfg = path.abspath(path.dirname(config_file))
        if path.isdir(dircfg):
            print(' ... in directory: ' + dircfg + '\n ... use instead: ')
            _, _, filenames = walk(dircfg).next()
            for fle in filenames:
                _, tail = path.splitext(fle)
                if tail == '.cfg':
                    print('    +> '+ fle)
        raise Exception('Error in configuration file')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xcptss = MESSAGES()
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
    cfgs = parseConfigFile(config_file, config_name)

    for cfgname in cfgs:
        # still in lower case
        if 'root' not in cfgs[cfgname]:
            cfgs[cfgname]['root'] = root_dir
        # parsing for proper naming
        #cfg = parseConfig_CompileTELEMAC(cfgs[cfgname])
    return xcptss, root_dir

def copy_file_to_tmp(test_dir, tmp_dir, module, root_dir, skip_test):
    """
       Copy all the files needed by the test case into the temporary folder

       @param test_dir path to the test case to validate
       @param tmp_dir path to the test case temporary folder
       @param test_dir Name of the module
       @param test_dir Root directory of the installation
       @param skip_test Test cases to skip
    """

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    if not path.exists(tmp_dir):
        mkdir(tmp_dir)
    else:
        shutil.rmtree(tmp_dir)
        mkdir(tmp_dir)
    chdir(tmp_dir)

    # Getting list on input/output files from the dictionary
    dico_file = path.join(root_dir, 'sources', module, module+'.dico')
    frgb, dico = scanDICO(dico_file)
    ifs, _ = getIOFilesSubmit(frgb, dico)
    # Getting list of steering file
    cas_files_path = glob.glob(test_dir +sep+'*.cas')

    list_file = []
    for cas_file in cas_files_path:
        if path.basename(cas_file) in skip_test:
            continue
        shutil.copyfile(cas_file, path.basename(cas_file))
        cas = readCAS(scanCAS(getFileContent(cas_file)), dico, frgb)
        user_fortran = None
        # Looping on input files
        for key in ifs:
            value, _ = getKeyWord(key, cas, dico, frgb)
            if value != []:
                ffile = value[0].strip("'")
                # if we have a user fortran
                if 'FORTRAN' in key:
                    if path.exists(path.join(tmp_dir, ffile)) and\
                        not path.isfile(path.join(tmp_dir, ffile)):
                        shutil.rmtree(path.join(tmp_dir, ffile))
                    user_fortran = ffile
                    if path.isfile(test_dir+sep+ffile):
                        shutil.copyfile(test_dir+sep+ffile, ffile)
                    else:
                        shutil.copytree(test_dir+sep+ffile, ffile)
                else:
                    shutil.copyfile(test_dir+sep+ffile, ffile)
        list_file.append((path.basename(cas_file), user_fortran))
    return list_file

def get_result_file_name(module, cas_file):
    """
       Returns the name of the result file for a given case

       :param module: name of the telemac module
       :param cas_file: name of the telemac steering file
    """
    dico_file = path.join(environ['HOMETEL'], 'sources', module, module+'.dico')
    frgb, dico = scanDICO(dico_file)
    cas = readCAS(scanCAS(getFileContent(cas_file)), dico, frgb)
    value, _ = getKeyWord(KEY_RES[module], cas, dico, frgb)
    res_file = value[0].strip("'")
    return res_file

def main(config_name, config_file, modules, example, nncsize, clean):
    """
    Main function
    """
    # Running main function
    _, root_dir = process_config(config_name, config_file, '')

    if path.exists('ValidationTelApy.log'):
        remove('ValidationTelApy.log')
    fichier = open('ValidationTelApy.log', 'a')
    fichier.write("-----Listing Validation TelApy-------\n")

    seq_only = {}
    skip_test = {}
    # Specifcation for each module
    for module in MODULE_HANDLED:
        seq_only[module] = []
        skip_test[module] = []

    # Sequential only test cases
    seq_only['telemac2d'].append('t2d_hydraulic_jump_v1p0.cas')
    seq_only['telemac2d'].append('t2d_hydraulic_jump_v2p0.cas')
    seq_only['telemac2d'].append('t2d_wesel.cas')
    seq_only['telemac2d'].append('t2d_wesel_pos.cas')
    seq_only['telemac2d'].append('t2d_delwaq.cas')
    seq_only['telemac2d'].append('t2d_ruptmoui.cas')
    seq_only['telemac2d'].append('t2d_triangular_shelf.cas')
    seq_only['telemac2d'].append('t2d_island.cas')
    seq_only['telemac2d'].append('t2d_tide-jmj_real_gen.cas')
    seq_only['telemac2d'].append('t2d_tide-jmj_type_gen.cas')
    seq_only['telemac2d'].append('t2d_dambreak_v1p0.cas')

    seq_only['telemac3d'].append('t3d_delwaq.cas')
    seq_only['telemac3d'].append('t3d_pluie.cas')
    seq_only['telemac3d'].append('t3d_tide-jmj_real_gen.cas')

    seq_only['artemis'].append('none')

    seq_only['tomawac'].append('tom_turning_wind.cas')
    seq_only['tomawac'].append('tom_manche.cas')
    seq_only['tomawac'].append('tom_manchelim.cas')
    # Test case that can not work with api

    # Using homere_adj not handle by api
    skip_test['telemac2d'].append('estimation')
    # Reruning telemac from homere not handled by api
    skip_test['telemac2d'].append('convergence')
    # Case that are not run by validation
    skip_test['telemac2d'].append('t2d_tide-jmj_type_med.cas')
    skip_test['telemac2d'].append('t2d_tide-ES_real.cas')

    # Non telemac3d case in folder
    skip_test['telemac3d'].append('t2d_canal.cas')
    skip_test['telemac3d'].append('p3d_amr.cas')
    skip_test['telemac3d'].append('p3d_bump.cas')
    skip_test['telemac3d'].append('p3d_canal.cas')
    skip_test['telemac3d'].append('p3d_cooper.cas')
    skip_test['telemac3d'].append('p3d_depot.cas')
    skip_test['telemac3d'].append('p3d_flume_slope.cas')
    skip_test['telemac3d'].append('p3d_gouttedo.cas')
    skip_test['telemac3d'].append('p3d_lock-hydro.cas')
    skip_test['telemac3d'].append('p3d_lock-nonhydro.cas')
    skip_test['telemac3d'].append('p3d_nonlinearwave.cas')
    skip_test['telemac3d'].append('p3d_piledepon.cas')
    skip_test['telemac3d'].append('p3d_piledepon-nonhydro.cas')
    skip_test['telemac3d'].append('p3d_pluie.cas')
    skip_test['telemac3d'].append('p3d_rouse.cas')
    skip_test['telemac3d'].append('p3d_stratification.cas')
    skip_test['telemac3d'].append('p3d_tetra.cas')
    skip_test['telemac3d'].append('p3d_vent.cas')
    skip_test['telemac3d'].append('p3d_V.cas')
    # Coupling test case
    skip_test['telemac3d'].append('depot')
    skip_test['telemac3d'].append('heat_exchange')

    # Artemis animated test case
    skip_test['artemis'].append('art_bj78_animated.cas')
    skip_test['artemis'].append('art_creocean_animated.cas')
    skip_test['artemis'].append('art_creocean_2.cas')
    skip_test['artemis'].append('art_creocean.cas')

    # Tomawac coupled test cases
    skip_test['tomawac'].append('3Dcoupling')

    for module in modules:
        fichier.write("-- For module " + module + "\n")
        module_dir = path.join(root_dir, 'examples', module)
        list_test_case = []
        if example != '':
            list_test_case.append(example)
        else:
            list_test_case = sorted(listdir(module_dir))

        # Sequential only test_case
        for i, test_case in enumerate(list_test_case):
            if test_case in skip_test[module]:
                continue
            case_dir = path.join(module_dir, test_case)
            tmp_dir = path.join(case_dir, 'tmp')
            print("<"+str(i+1)+"/"+str(len(list_test_case))+'> '+str(test_case))
            fichier.write('Running test case '+test_case+'\n')
            list_file = copy_file_to_tmp(case_dir, tmp_dir, module, \
                                         root_dir, skip_test[module])

            chdir(tmp_dir)

            for cas, fortran in list_file:
                #
                # Running Telemac based on TelApy
                #
                if cas in skip_test[module]:
                    continue
                # Get results names
                res_file = get_result_file_name(module, cas)
                api_res_file = res_file+'_api'

                # Running in sequential mode
                # if the case does not run in parallel
                if cas in seq_only[module]:
                    ncsize = 1
                else:
                    ncsize = nncsize
                passed_api = run_telemac_api(module, cas, ncsize, fortran)

                if passed_api:
                    shutil.move(res_file, api_res_file)
                # Running Telemac classical way
                #
                passed_normal = run_telemac_normal(module, cas, ncsize)

                #
                # Result comparison between api and
                #  classical Telemac computation
                #
                if not passed_normal:
                    fichier.write('   Normal run crashed\n')
                if not passed_api:
                    fichier.write('   Api run crashed\n')
                if not passed_api or not passed_normal:
                    fichier.write(str(cas)+'                       FAILED'+'\n')
                    continue
                if not path.exists(res_file):
                    fichier.write('   Missing '+res_file+"\n")
                    fichier.write(str(cas)+'                       FAILED'+'\n')
                    continue
                if not path.exists(api_res_file):
                    fichier.write('   Missing '+api_res_file+"\n")
                    fichier.write(str(cas)+'                       FAILED'+'\n')
                    continue
                compare = filecmp.cmp(res_file, api_res_file)

                if compare:
                    fichier.write(str(cas)+'                       PASSED'+'\n')
                else:
                    fichier.write(str(cas)+'                       FAILED'+'\n')

            if clean:
                chdir(module_dir+sep+test_case)
                shutil.rmtree(module_dir+sep+test_case+sep+'tmp')

        fichier.write('my work is done '+'\n')

if __name__ == "__main__":
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nLoading Options and Configurations\n'+72*'~'+'\n')
    PARSER = argparse.ArgumentParser(\
        description='Make the validation of Telemac-Mascaret API '\
            'and/or executable using the API')
    PARSER.add_argument(\
        "-c", "--configname",
        dest='configName',
        default="",
        help="specify configuration name, default is randomly \
                    found in the configuration file")
    PARSER.add_argument(\
        "-f", "--configfile",
        dest='configFile',
        default="",
        help="specify configuration file, default is systel.cfg")
    PARSER.add_argument(\
        "-m", "--module",
        dest='modules',
        default="telemac2d",
        help="specify the list of folder to validate seprated by ,")
    PARSER.add_argument(\
        "--clean",
        action="store_true",
        dest="clean",
        default=False,
        help="Remove tmp folders")
    PARSER.add_argument(\
        "-n", "--cnsize",
        dest='ncsize',
        default=4,
        help="specify the number of processor the test case will be run with")
    PARSER.add_argument(\
        "-e", "--example",
        dest='example',
        default="",
        help="specify the name of the test case to compute")
    ARGS = PARSER.parse_args()
    main(ARGS.configName, ARGS.configFile, ARGS.modules.split(','),
         ARGS.example, ARGS.ncsize, ARGS.clean)

