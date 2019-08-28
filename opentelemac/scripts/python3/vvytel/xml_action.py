r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
import sys
from os import path, chdir, system
from utils.files import create_directories
from utils.exceptions import TelemacException
from shutil import copy
from argparse import Namespace
from execution.parser_mascaret import scan_xcas
from execution.run_mascaret import create_mascaret_files
from execution.telemac_cas import TelemacCas
from vvytel.telemac_xml_tools import copy_file_to_valid_dir

# _______________________________________________________//          \\
# _______________________________________________________>> ACTION <<
#                                                        \\          //
# for action in xmlRoot.findall("action"):
#if xml_child.tag == "action":
def xml_action(dicos, bypass, dc, do, rank, reports, xcpt, xml_child,
               xml_config, xml_file):
    report = {'type': 'action'}
    report.update(dc.filtrkeys)
    updated = False
    action = xml_child
    # ~~ Step 1. Common checks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    #    - keys
    target_file = do.add_action(action, rank)
    report.update({\
         'xref': do.active['xref'],
         'updt': False,
         'fail': False,
         'warn': False,
         'rank': do.active['rank'],
         'value': 0,
         'title': 'This was ignored'})
    #    - driving files
    one_found = False
    if path.isfile(path.join(do.active['path'], target_file)):
        one_found = True
    for ddir in sys.path:
        if path.isfile(path.join(ddir, target_file)):
            one_found = True
    if not one_found:
        raise TelemacException(\
             'Could not find your target file' + target_file)
    #    - configurations
    one_found = False
    for cfgname in xml_config:
        # TODO: parsing ranges of configurations
        if "config" in do.active.keys() and not one_found:
            cfg_found = True
            for cfg in do.active["config"].split(','):
                cfg_found = cfg_found and (cfg in cfgname)
            if cfg_found:
                one_found = True
                do.active["config"] = cfgname
            else:
                continue
        else:
            one_found = True
    if not one_found:
        raise TelemacException(\
             'Could not find the specified configuration made up '
             'of the following string(s) ' + do.active["config"] +
             ' on your system.\n        ... do check action ' +
             do.active['xref'] + ' within ' +
             path.basename(xml_file))
    # ~~ Step 2. Loop over configurations ~~~~~~~~~~~~~~~~~~~~
    for cfgname in xml_config:
        cfg = xml_config[cfgname]['cfg']
        if "config" in do.active.keys():
            if do.active["config"] not in cfgname:
                continue
        do.add_cfg(cfgname, cfg)
        # ~~> Create the safe
        create_directories(do.active['safe'])
        # ~~ Step 3a. Deals with TELEMAC launchers ~~~~~~~~~~~~~
        if do.active["code"] == 'mascaret':
            do.availacts = "run"
            doable = xml_config[cfgname]['options'].todos
            if doable == '':
                doable = do.active["do"]
            if doable == '' or doable == 'all':
                doable = do.availacts
            if "run" in doable.split(';'):
                mascfiles = []
                mascfiles = scan_xcas(path.join(\
                        do.active['path'], do.active['target']))
                for mascfile in mascfiles:
                    copy(path.join(do.active['path'], mascfile),
                         path.join(do.active['safe'], mascfile))
                copy(path.join(do.active['path'], do.active['target']),
                     path.join(do.active['safe'], do.active['target']))
                chdir(do.active['safe'])
                create_mascaret_files(cfg, do.active['target'])
                mascaretexe = path.join(\
                        cfg['root'], 'builds', cfgname, 'bin',\
                        'mascaret'+cfg['SYSTEM']['sfx_exe'])+\
                        ' FichierCas.txt'
                # TODO: replace by normal run same as other modules
                updated = system(mascaretexe)
        elif do.active["code"] in cfg['MODULES']:
            do.availacts = "translate;run;compile;princi"
            # ~~> Manage target_file and other inputs
            cas_file = path.join(do.active['path'], target_file)
            dico_file = path.join(cfg['MODULES'][do.active['code']]['path'],
                                  do.active['code'] + '.dico')
            cas = TelemacCas(cas_file, dico_file)
            # ~~> Parse user defined keywords
            if do.active['set'] != '':
                for iset in do.active['set'].split('|'):
                    if ':' in iset:
                        key, val = iset.split(':')
                    elif '=' in  iset:
                        key, val = iset.split('=')
                    else:
                        raise TelemacException(\
                          "Error in set {} should be syntax key[=:]val"\
                          .format(iset))
                    cas.set(key.strip(), val.strip(), convert=True)
            # ~~> Parse other special keys
            if do.active["ncsize"] != '':
                cas.set('PROCESSEURS PARALLELES', int(do.active["ncsize"]))
            do.update_cfg({'cas': cas})
            # ~~> Define config-split storage
            sortie_files = copy_file_to_valid_dir(cas, do.active['safe'])
            if sortie_files != []:
                do.update_cfg({'sortie': sortie_files})
            do.update_cfg({'input': cas.in_files})
            do.update_cfg({'output': cas.out_files})
            # ~~> Case of coupling
            cplages = cas.get('COUPLING WITH', default='').split(',')
            links = {}
            for cplage in cplages:
                for mod in cfg['MODULES']:
                    if mod in cplage.lower():
                        # ~~> Extract the CAS File name
                        keyword = mod.upper() + ' STEERING FILE'
                        cas_file_plage = cas.get(keyword)
                        cas_file_plage = path.join(path.dirname(cas_file),
                                                   cas_file_plage)
                        if not path.isfile(cas_file_plage):
                            raise TelemacException(\
                                 'missing coupling CAS file for {}: {}'\
                                 .format(mod, cas_file_plage))
                        # ~~> Read the DICO File
                        dico_file_plage = path.join(cfg['MODULES'][mod]['path'],
                                                    mod + '.dico')
                        cas_plage = TelemacCas(cas_file_plage, dico_file_plage)
                        # ~~> Fill-in the safe
                        sortie_plage = copy_file_to_valid_dir(\
                                cas_plage, do.active['safe'])
                        links.update({mod: {}})
                        links[mod].update({\
                                  'code': mod,
                                  'target': path.basename(cas_file_plage),
                                  'cas': cas_plage,
                                  'input': cas_plage.in_files,
                                  'output': cas_plage.out_files,
                                  'sortie': sortie_plage})
                        if sortie_plage != []:
                            links[mod].update({'sortie': sortie_plage})
            if links != {}:
                do.update_cfg({"links": links})
            # ~~> Complete all actions
            # options.todos takes: translate;run;compile and none
            doable = xml_config[cfgname]['options'].todos
            if doable == '':
                doable = do.active["do"]
            if doable == '' or doable == 'all':
                doable = do.availacts
            # ~~> Action type A. Translate the CAS file
            if "translate" in doable.split(';'):
                # - exchange keywords between dictionaries
                do.translate_cas(cfg['REBUILD'])
            # ~~> Action type B. Analysis of the CAS file
            # TODO:
            # - comparison with DEFAULT values of the DICTIONARY
            # if "cas" in doable.split(';'):
            # - comparison of dictionnaries betwen configurations
            # if "dico" in doable.split(';'):
            # ~~> Action type C. Analysis of the PRINCI file
            if "princi" in doable.split(';'):
                # - comparison with standard source files
#                specs = Values()
                specs = Namespace()
                specs.unified = False
                specs.ndiff = False
                specs.html = True
                specs.ablines = True
                specs.context = False
                do.diff_princi(specs, cfg, cfg['REBUILD'])
            # TODO: - comparison of subroutines between action items
            # ~~> Action type E. Running CAS files
            if "run" in doable.split(';'):
                updated = do.run_cas(xml_config[cfgname]['options'],
                                     cfg['REBUILD'])
        # ~~ Step 3b. Deals with execute launchers ~~~~~~~~~~~~~
        elif do.active["code"] == 'exec':
            do.availacts = "exec"
            # ~~> Complete all actions
            # options.todos takes: exec and none
            doable = xml_config[cfgname]['options'].todos
            if doable == '':
                doable = do.active["code"]
            if doable == '' or doable == 'all':
                doable = do.availacts
            # ~~> Action type E. Running exec
            if "exec" in doable.split(';'):
                # - simply run the exec as stated
                updated = do.run_command()
    if updated:
        report.update({'updt': updated, 'title': 'My work is done'})
    if xcpt != []:
        raise TelemacException(\
              'looking at actions in xml_file: ' + xml_file+
              str(xcpt))
    one_found = False
    for i in range(len(reports)):
        # /!\ You are sure there is one xref (?)
        if reports[i]['xref'] == report['xref']:
            reports[i] = report
            one_found = True
    if not one_found:
        reports.append(report)
    return reports, xcpt
