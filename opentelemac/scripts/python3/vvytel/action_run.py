r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
# -*- coding: utf-8 -*-
import sys
from os import walk, path, remove, listdir, chdir
from copy import deepcopy
from argparse import Namespace
from socket import gethostname
from vvytel.telemac_xml_tools import find_targets
from execution.run_cas import run_study
from vvytel.action import Action
from utils.files import match_safe, copy_file, get_file_content, \
                        put_file_content, add_file_content, diff_text_files
from utils.messages import Messages
from utils.exceptions import TelemacException
from compilation.parser_fortran import get_principal_wrap_names, \
                                       filter_principal_wrap_names
from postel.parser_output import get_latest_output_files
###############################################################################
# dicos is defined in the main script!!!
# _____                      ____________________________________________
# ____/ Keyword parse  /___________________________________________/
#
###############################################################################
class ActionRun(Action):
    """
    actionRUN is to do with the modules of the TELEMAC system and
        other execution (pre- and post-processes).
        . It understands what a PRINCI and what a CAS file and will do
        the necessary steps to run modules accordingly.
        . It includes specific methods to do with CAS and PRINCI
        . It will organise the tranfer of files (inputs and outputs)
    """
    availkeys = deepcopy(Action.availkeys)
    availkeys.update({'dico': '', "ncsize": 0, "code": ''})
###############################################################################
    def __init__(self, xml_file, title='', bypass=True):
        """
        Initialize variables.
        """
        Action.__init__(self, title, bypass)
        # those you reset
        self.path = path.dirname(xml_file)
       # those you need to see in the XML file
        self.active["target"] = None
        self.active["code"] = None
        self.active["xref"] = None
        self.active["do"] = None
        self.code = None
###############################################################################
    def add_action(self, actions, rank=''):
        """
        Adds actions defined in input.
        """
        target = Action.add_action(self, actions, rank)
        self.active['path'] = self.path
        self.code = self.active["code"]
        return target
###############################################################################
    def add_cfg(self, cfgname, cfg):
        """
        Adds configuration name.
        """
        if not (self.active["code"] == 'exec' or \
                  self.active["code"] in cfg['MODULES']):
            raise TelemacException(\
                    '... do not know about: {} in '
                    'configuration: {}'.format(self.active["code"],
                                               cfgname))

        Action.add_cfg(self, cfgname, cfg)
        Action.update_cfg(self, {\
                  "links": {},
                  'code': self.active["code"],
                  'deprefs': self.active['deprefs'],
                  'outrefs': self.active['outrefs']})
###############################################################################
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                      CAS related Methods
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~ todo: difference of CAS file with default keys ~~~~~~~~~~~~~
    def diff_cas(self):
        """ Does nothing """
        updated = False
        return updated
###############################################################################
    # ~~ Translate the CAS file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def translate_cas(self, rebuild):
        """
        Function calling trans_cas.
        """
        updated = False
        if not "translate" in self.availacts.split(';'):
            return updated
        xref = self.active["xref"]
        cfgname = self.active['cfg']
        active = self.dids[xref][cfgname]
        # ~~> principal CAS file
        cas_file = path.join(active['path'], active["target"])
        oneup = path.dirname(active['safe'])  # copied one level up
        if match_safe(cas_file, active["target"] + '.??', oneup, rebuild):
            print('        ~> translate cas file: ' + active["target"])
            # /!\ removes comments at end of lines
            active['cas'].write_fr_gb(output_dir=oneup)
        # ~~> associated CAS files
        for mod in active["links"]:
            link = active["links"][mod]
            cas_file = path.join(active['path'], link['target'])
            if match_safe(cas_file, link['target'] + '.??', oneup, rebuild):
                print('        ~> translate cas file: ' + link['target'])
                # /!\ removes comments at end of lines
                link['cas'].write_fr_gb(output_dir=oneup)
                updated = True
        return updated
###############################################################################
    def run_cas(self, options, rebuild):
        """
        Runs the CAS file.

        @param options (optparse.Values) runcode options (--ncsize,
        --walltime...)
        @param rebuild (boolean) If true re-run the case
        """
        updated = False
        if not "run" in self.availacts.split(';'):
            return updated
        # ~~> prepare dependencies
        for out_file in self.active["deprefs"]:
            if out_file in self.dids:
                if self.active['cfg'] in self.dids[out_file]:
                    layer = find_targets(\
                            self.dids[out_file][self.active['cfg']],
                            self.active["deprefs"][out_file])
                    if layer != []:
                        if path.isfile(layer[0][0]):
                            copy_file(layer[0][0], self.active['safe'])
                        else:
                            raise TelemacException(\
                                'Could not find reference to the '
                                'dependant: '+ layer[0][0])
                else:
                    raise TelemacException(\
                         'Could not find the configuration ' +
                         self.active['cfg'] + 'for the dependant: ' + out_file)
            else:
                if self.active["where"] != '':
                    if path.exists(path.join(self.active["where"], out_file)):
                        copy_file(path.join(self.active["where"], out_file),
                                  self.active['safe'])
                elif path.exists(path.join(self.path, out_file)):
                    copy_file(path.join(self.path, out_file),
                              self.active['safe'])
                else:
                    raise TelemacException(\
                        'Could not find reference to the dependant: ' + \
                        out_file)
        # ~~> associated secondary inputs
        for xref in self.active["deprefs"]:
            if xref in self.dids.keys():
                cfgname = self.active['cfg']
                if cfgname in self.dids[xref]:
                    active = self.dids[xref][cfgname]
                    if self.active["deprefs"][xref].lower() == "sortie":
                        if path.isfile(active["sortie"]):
                            copy_file(active["sortie"], self.active['safe'])
                        else:
                            raise TelemacException(\
                                  'Could not find reference to the '
                                  'sortie: ' + active["sortie"])
                    elif self.active["deprefs"][xref] in active["outrefs"]:
                        outref = active["outrefs"][self.active["deprefs"][xref]]
                        if path.exists(path.join(active['safe'], outref)):
                            copy_file(path.join(active['safe'], outref),
                                      self.active['safe'])
                        else:
                            raise TelemacException(\
                                    'I cannot see your output file ' +
                                    self.active["deprefs"][xref])
                    else:
                        layer = []
                        for out_file in active["output"]:
                            layer = find_targets(active,
                                                 self.active["deprefs"][xref])
                            if layer != []:
                                if path.isfile(layer[0][0]):
                                    copy_file(layer[0][0],
                                              self.active['safe'])
                                else:
                                    raise TelemacException(\
                                      'Could not find reference to the '\
                                      'dependant: ' + layer[0][0])
                        for mod in active["links"]:
                            for _ in active["links"][mod]["out_files"]:
                                layer = find_targets({\
                                   'code': mod,\
                                   'output':active["links"][mod]["out_files"]},\
                                                   self.active["deprefs"][xref])
                                if layer != []:
                                    if path.isfile(layer[0][0]):
                                        copy_file(layer[0][0],
                                                  self.active['safe'])
                                    else:
                                        raise TelemacException(\
                                     'Could not find reference to the '\
                                     'dependant: ' + layer[0][0])
                        if layer == []:
                            raise TelemacException(\
                        'Could not find reference to the linked file: ' +\
                        self.active["deprefs"][xref])
        # ~~> prepare options as if run from command line
        specs = Namespace()
        specs.config_name = options.config_name
        specs.config_file = options.config_file
        specs.sortie_file = True
        specs.tmpdirectory = True
        specs.root_dir = options.root_dir
        specs.w_dir = options.w_dir
        specs.compileonly = False
        specs.nozip = False
        if options.hosts != '':
            specs.hosts = options.hosts
        else:
            specs.hosts = gethostname().split('.')[0]
        specs.split = options.split
        specs.run = options.run
        specs.merge = options.merge
        specs.jobname = options.jobname
        specs.hpc_queue = options.hpc_queue
        specs.walltime = options.walltime
        specs.email = options.email
        specs.mpi = options.mpi
        if options.ncsize != 0 and self.active["ncsize"] != '':
            self.active["ncsize"] = str(options.ncsize)
        specs.ncsize = int(self.active["ncsize"])
        specs.nctile = 0  # default but should not be used for validation
        specs.ncnode = 0  # default but should not be used for validation
        specs.bypass = self.bypass
        specs.use_link = options.use_link
        # ~~> check on sorties and run
        cas_file = path.join(self.active['path'], self.active["target"])
        sac_file = path.join(self.active['safe'], self.active["target"])
        sortie_files = get_latest_output_files(sac_file)
        outputs = self.dids[self.active["xref"]][self.active['cfg']]['output']
        cas = self.dids[self.active["xref"]][self.active['cfg']]['cas']
        if match_safe(cas_file, self.active["target"] + '_*??h??min??s*.sortie',
                      self.active['safe'], rebuild):
            print('      +> running cas file: ' + self.active["target"])
            for k in outputs:
                submit = outputs[k].split(";")
                fle = cas.values[k]
                # In case k is read and write
                if 'LIT' in submit[4]:
                    continue
                match_safe('', path.basename(fle), self.active['safe'], 2)
            sortie_files = run_study(sac_file, self.active["code"], specs)
            updated = True
        if sortie_files != []:
            self.update_cfg({'sortie': sortie_files})
        # ~~> associated secondary outputs
        for outref in self.active["outrefs"]:
            out_file = self.active["outrefs"][outref]
            if path.exists(path.join(self.active['safe'], out_file)):
                copy_file(path.join(self.active['safe'], out_file),
                          path.join(self.active['path'],
                                    self.active["xref"]))
            else:
                raise TelemacException(\
                        'I cannot see your output file ' + out_file +\
                        ' from within action reference ' +\
                        self.active["xref"])
        print('\n\n... this work is done (I mean I have dealt with reference '+\
              self.active["xref"] + ')\n\n')
        return updated
###############################################################################
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                   PRINCI related Methods
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def diff_princi(self, options, cfg, rebuild):
        """
        Highlights user PRINCI differences.
        Runs the action "princi" which will create an html containing
        the difference between a user fortran and its version in the sources

        @param options ??
        @param cfg Configuration information
        @param rebuild ??
        """
        updated = False
        if not "princi" in self.availacts.split(';'):
            return updated
        xref = self.active["xref"]
        cfgname = self.active['cfg']
        active = self.dids[xref][cfgname]
        oneup = path.dirname(active['safe'])  # copied one level up
        # ~~> principal PRINCI file
        princin_file = active['cas'].get('FORTRAN FILE')
        if princin_file != '':
            princin_file = path.join(active['path'], princin_file)
            if path.isfile(princin_file):
                html_file = path.join(oneup, path.splitext(\
                                     path.basename(princin_file))[0] + '.html')
                if match_safe(html_file, path.basename(html_file),
                              oneup, rebuild):
                    # ~~> Scans the principal user PRINCI file
                    print('        ~> scanning your PRINCI file: ' + \
                            path.basename(princin_file))
                    p_files = get_principal_wrap_names(princin_file)
                    if p_files == []:
                        raise TelemacException(\
                          'I could not recognised entities in your PRINCI: '+\
                          princin_file)
                    else:
                        print('          +> found:')
                        for _, p_file in p_files:
                            print('              - ' + p_file)
                    # ~~> Scans the entire system
                    out_files = {}
                    for mod in cfg['MODULES']:
                        dirpath, _, filenames = \
                                next(walk(cfg['MODULES'][mod]['path']))
                        for fle in filenames:
                            n, ext = path.splitext(fle)
                            # Only looking for fortran files
                            if ext.lower() not in ['.f', '.f90']:
                                continue
                            for _, p_file in p_files:
                                if p_file.lower() == n:
                                    out_files.update(\
                                         filter_principal_wrap_names(\
                                          [p_file], [path.join(dirpath, fle)]))
                    if out_files == {}:
                        print(r'/!\ warning: could not relate the following in '
                              'your PRINCI with the system: ' + princin_file)
                        for out_file in out_files:
                            print('              x ' + out_file)
                    else:
                        print('          +> found:')
                        for out_file in out_files:
                            print('              - ' + out_file)
                    # ~~> Save temporarily for subsequent difference
                    princi_name, princi_ext = path.splitext(\
                            path.basename(princin_file))
                    orin_file = path.join(oneup, cfgname, princi_name + \
                                        '.original' + princi_ext)
                    put_file_content(orin_file, [])
                    for _, p_file in p_files:
                        if p_file in out_files:
                            add_file_content(orin_file, \
                                         get_file_content(out_files[p_file]))
                    # ~~> Process difference and write output into an HTML file
                    diff = diff_text_files(orin_file, princin_file, options)
                    remove(orin_file)
                    with open(html_file, 'w', encoding='utf-8') as fle:
                        fle.writelines(diff)
                    print('         ~> comparison successful ! created: ' + \
                            path.basename(html_file))
                    updated = True
            elif path.isdir(princin_file):
                file_to_diff = {}
                for fle in listdir(princin_file):
                    if fle.lower().endswith((".f", ".f90")):
                        file_to_diff[fle] = ''
                # Search for orginals
                for mod in cfg['MODULES']:
                    dirpath, _, filenames = \
                            next(walk(cfg['MODULES'][mod]['path']))
                    for fle in filenames:
                        # Only looking for fortran files
                        if fle.lower().endswith((".f", ".f90")):
                            if fle in file_to_diff:
                                file_to_diff[fle] = path.join(dirpath, fle)
                print('          +> found:')
                for fle, orin_file in file_to_diff.items():
                    if orin_file != '':
                        print('              - ' + fle)
                        html_file = path.join(oneup, fle + '.html')
                        with open(html_file, 'w', encoding='utf-8') as ffle:
                            ffle.writelines(diff_text_files(\
                                    orin_file, path.join(princin_file, fle),\
                                    options))
            else:
                raise TelemacException(\
                  'I could not find your PRINCI file: ' + princin_file)
        # ~~> associated PRINCI file
        # TODO: case of coupling with multiple PRINCI files
        return updated
###############################################################################
    def run_command(self):
        """
        Direct executions.
        """
        updated = False
        # If python execution replace call by call to python3
        if 'python ' in self.active["do"]:
            self.active["do"] = self.active["do"].replace("python ", "python3 ")
        print('     +> executing your command:\n        ' + self.active["do"])
        mes = Messages(size=10)
        # ~~> copy of inputs
        in_file = ''
        exe_cmd = self.active["do"]
        if path.isfile(path.join(self.active['path'], self.active["target"])):
            in_file = path.join(self.active['path'], self.active["target"])
            copy_file(in_file, self.active['safe'])
        else:
            # /!\ in that case, you do not copy the target locally
            for elem in sys.path:
                if path.isfile(path.join(elem, self.active["target"])):
                    in_file = path.join(elem, self.active["target"])
            if in_file == '':
                raise TelemacException(\
                  'Could not find reference to the target: ' + in_file)
            exe_cmd = self.active["do"].replace(self.active["target"], in_file)
        for out_file in self.active["deprefs"]:
            if out_file in self.dids:
                if self.active['cfg'] in self.dids[out_file]:
                    layer = find_targets(\
                            self.dids[out_file][self.active['cfg']],
                            self.active["deprefs"][out_file])
                    if layer != []:
                        if path.isfile(layer[0][0]):
                            copy_file(layer[0][0], self.active['safe'])
                        else:
                            raise TelemacException(\
                        'Could not find reference to the dependant: '+\
                        layer[0][0])
                else:
                    raise TelemacException(\
                        'Could not find the configuration ' + \
                        self.active['cfg'] + 'for the dependant: ' + out_file)
            else:
                if self.active["where"] != '':
                    if path.exists(path.join(self.active["where"], out_file)):
                        copy_file(path.join(self.active["where"], out_file),
                                  self.active['safe'])
                elif path.exists(path.join(self.path, out_file)):
                    copy_file(path.join(self.path, out_file),
                              self.active['safe'])
                else:
                    raise TelemacException(\
                       'Could not find reference to the dependant: ' + \
                       out_file)
        # ~~> associated secondary inputs
        for xref in self.active["deprefs"]:
            if xref in self.dids.keys():
                cfgname = self.active['cfg']
                if cfgname in self.dids[xref]:
                    active = self.dids[xref][cfgname]
                    if self.active["deprefs"][xref].lower() == "sortie":
                        copy_file(active["sortie"], self.active['safe'])
                    elif self.active["deprefs"][xref] in active["outrefs"]:
                        if path.exists(path.join(active['safe'],\
                           active["outrefs"][self.active["deprefs"][xref]])):
                            copy_file(path.join(\
                          active['safe'],
                          active["outrefs"][self.active["deprefs"][xref]]),\
                          self.active['safe'])
                        else:
                            raise TelemacException(\
                                 'I cannot see your output file ' + \
                                 self.active["deprefs"][xref])
                    else:
                        layer = []
                        for out_file in active["output"]:
                            layer = find_targets(active,
                                                 self.active["deprefs"][xref])
                            if layer != []:
                                if path.isfile(layer[0][0]):
                                    copy_file(layer[0][0],
                                              self.active['safe'])
                                else:
                                    raise TelemacException(\
                                      'Could not find reference to the '\
                                                'dependant: ' + layer[0][0])
                        if layer == []:
                            raise TelemacException(\
                       'Could not find reference to the linked file: ' +\
                       self.active["deprefs"][xref])
        # ~~> execute command locally
        chdir(self.active['safe'])
        tail, code = mes.run_cmd(exe_cmd, self.bypass)
        updated = True
        if code != 0:
            raise TelemacException(\
                 'Could not run your command (' + exe_cmd + \
                 ').\n        ' + tail)
        # ~~> copy of outputs /!\ you are replacing one config by another ... do
        # the same as for run_cas
        for out_file in self.active["outrefs"]:
            if path.exists(path.join(self.active['safe'],
                                     self.active["outrefs"][out_file])):
                copy_file(path.join(self.active['safe'],
                                    self.active["outrefs"][out_file]),
                          self.active['path'])
            else:
                raise TelemacException(\
                        'I cannot see your output file ' + out_file +\
                        ': ' + self.active["outrefs"][out_file])
        return updated
