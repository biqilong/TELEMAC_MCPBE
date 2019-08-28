r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
# -*- coding: utf-8 -*-
# _____                        _____________________________________
# ____/ Primary Class: ACTION /____________________________________/
#
from os import path
from copy import deepcopy
from vvytel.telemac_xml_tools import get_xml_keys
from utils.exceptions import TelemacException
###############################################################################
class Action(object):
    """
    In the classes below, the double quoted keys refer to the keys
    of the XML file. Contrarily, the single quoted keys in
    because they are internal to the python scripts.
    In the XML file, you can have multiple actions and each action
    will be associated with multiple configurations:
    did.keys() => array [xref] for each action
    did[xref].keys() => array [cfgname] for each possible configuration
    did[xref][cfgname].keys() =>
        - 'target', basename of the CAS file
        - 'cas', scanned CAS file
        - 'code', name of the module
        - 'title', title of the action (xref)
        - 'cmaps', refer to the directory 'colour_maps' for colour plotting
        - 'deprefs', refer to the files that need copying from path to safe
        - 'outrefs', refer to the files that need copying from safe to path
        - 'where' will not default to xref anymore, allowing files to
        be shared and located at the same place between actions
    """
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                                                General Methods
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    availacts = ''
    availkeys = {'path': '', 'safe': '', 'cfg': '',\
                "target": '', "xref": '', "do": '', "rank": '', "set": '',\
                "title": '', "deprefs": '', "outrefs": '', "where": ''}
    ############################################################################
    def __init__(self, title='', bypass=True):
        """
        Initialize variables.
        """
        self.active = {}
        if title != '':
            self.active["title"] = title
        self.bypass = bypass
        self.dids = {}
        self.path = ''
        self.safe = ''
    ############################################################################
    def add_action(self, actions, rank=''):
        """
        Adds actions defined in input.
        """
        self.active.update(deepcopy(self.availkeys))
        self.active['path'] = self.path
        self.active['safe'] = self.safe
        self.active = get_xml_keys(actions, self.active)
        if self.active["xref"] in self.dids:
            raise TelemacException(\
                  'you are getting me confused, this xref'\
                  ' already exists: ' + self.active["xref"])
        self.dids.update({self.active["xref"]: {}})
        if self.active["rank"] == '':
            self.active["rank"] = rank
        if self.active["rank"] == '':
            self.active["rank"] = '953'
        self.active["rank"] = int(self.active["rank"])
        found = False
        found = isinstance(self.active["deprefs"], str)
        if found:
            deprefs = {}
            if self.active["deprefs"] != '':
                for depref in self.active["deprefs"].split(';'):
                    if ':' in depref:
                        ref, dep = depref.split(':')
                    # ref becomes the name itself if no dependencies to other
                    # actions
                    else:
                        ref = depref
                        dep = depref
                    deprefs.update({ref: dep})
            self.active["deprefs"] = deprefs
        found = False
        found = isinstance(self.active["outrefs"], str)
        if found:
            outrefs = {}
            if self.active["outrefs"] != '':
                for outref in self.active["outrefs"].split(';'):
                    if ':' not in outref:
                        outrefs.update({self.active["xref"]: outref})
                    else:
                        ref, out = outref.split(':')
                        outrefs.update({ref: out})
            self.active["outrefs"] = outrefs
        return self.active["target"]
    ############################################################################
    def add_cfg(self, cfgname, cfg):
        """
        Adds configuration name.
        """
        self.active['cfg'] = cfgname
        if self.active["where"] != '':
            self.active['safe'] = path.join(self.active['path'],
                                            self.active["where"], cfgname)
        else:
            self.active['safe'] = path.join(self.active['path'],
                                            self.active["xref"], cfgname)
        self.dids[self.active["xref"]].update({cfgname: {\
         'target': self.active["target"],
         'safe': self.active['safe'],
         'path': self.active['path'],
         'title': self.active["title"],
         'set': self.active["set"],
         'cmaps': path.join(cfg['pytel'], 'postel', 'color_maps_xml')}})
    ############################################################################
    def update_cfg(self, cfgname):
        """
        Updates configuration name.
        """
        self.dids[self.active["xref"]][self.active['cfg']].update(cfgname)
