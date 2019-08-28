r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
# -*- coding: utf-8 -*-
from copy import deepcopy
from os import path
from vvytel.action import Action
###############################################################################
class ActionGet(Action):
    """
    Function related to loading data in memory for future use.
    """
    availkeys = deepcopy(Action.availkeys)
    availkeys.update({'type': ''})
    ############################################################################
    def __init__(self, xml_file, title='', bypass=True):
        """
        Initialize variables.
        """
        Action.__init__(self, title, bypass)
        # those you reset
        self.path = path.dirname(xml_file)
        # those you need to see in the XML file
        self.active["target"] = None
        self.active["xref"] = None
        self.active["type"] = None
    ############################################################################
    def add_action(self, actions, rank=''):
        """
        Adds actions defined in input.
        """
        target = Action.add_action(self, actions, rank)
        self.active['path'] = self.path
        return target
    ############################################################################
    def add_cfg(self, cfgname, cfg):
        """
        Adds configuration name.
        """
        Action.add_cfg(self, cfgname, cfg)
        self.active['path'] = self.path
        Action.update_cfg(self, {\
              "type": self.active["type"],
              "target": path.join(self.active['path'],
                                  self.active["target"])})
