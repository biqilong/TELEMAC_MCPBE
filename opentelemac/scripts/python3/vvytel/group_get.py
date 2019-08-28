r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
# -*- coding: utf-8 -*-
# _____                                    ___________________________________
# ____/ Secondary Class groupGET /__________________________________/
#
from copy import deepcopy
from os import path
from vvytel.groups import Groups
###############################################################################
class GroupGet(Groups):
    """
    Class using Groups class as argument.
    """
    availkeys = deepcopy(Groups.availkeys)
    availkeys.update({'path': '',
                      'safe': '',
                      'cfg': '',
                      "time": '[-1]',
                      "extract": '',
                      "sample": '',
                      "vars": '',
                      'saveas': '',
                      "target": '',
                      "do": '',
                      "rank": '',
                      "deprefs": '',
                      "outrefs": '',
                      "where": '',
                      "type": '',
                      "config": 'distinct'})
    groupkeys = deepcopy(Groups.groupkeys)
    groupkeys.update({"vars": '',
                      "time": '',
                      "extract": '',
                      "sample": '',
                      "config": '',
                      "where": ''})
    avaylkeys = deepcopy(Groups.avaylkeys)
    avaylkeys.update({"title": '', "target": '', "deco": ''})
###############################################################################
    def __init__(self, xml_file, title='', bypass=True):
        """
        Initialize variables.
        """
        Groups.__init__(self, title, bypass)
        # those you reset
        self.path = path.dirname(xml_file)
        self.safe = self.path
        self.order = []
###############################################################################
    def add_group(self, draw, rank=''):
        """
        Adds draw to rank.
        """
        Groups.add_group(self, draw)
        self.active['path'] = self.path
        self.active['safe'] = self.safe
        if self.dids[self.active['type']][self.tasks["xref"]]['rank'] == '':
            self.dids[self.active['type']][self.tasks["xref"]]['rank'] = rank
        if self.dids[self.active['type']][self.tasks["xref"]]['rank'] == '':
            self.dids[self.active['type']][self.tasks["xref"]]['rank'] = '953'
        self.dids[self.active['type']][self.tasks["xref"]]['rank'] = \
                int(self.dids[self.active['type']][self.tasks["xref"]]['rank'])
        self.order.append(self.tasks["xref"])
###############################################################################
    def distribute_deco(self, subtask):
        """
        Distributes decoration.
        """
        vrs = subtask["vars"].split(';')
        for i, val in enumerate(vrs):
            if ':' not in val:
                vrs[i] = val + ':xyz'  # :xyz is not used
        subtask["vars"] = ';'.join(vrs)
        return subtask
