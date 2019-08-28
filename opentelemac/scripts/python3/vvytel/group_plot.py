r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
# -*- coding: utf-8 -*-
# _____                                     __________________________________
# ____/ Secondary Class GroupPlot /_________________________________/
#
#
from copy import deepcopy
from os import path
from vvytel.groups import Groups
###############################################################################
class GroupPlot(Groups):
    """
    Class using Groups class as argument.
    Class related to plotting and producing PNG (mainly).
    """
    availkeys = deepcopy(Groups.availkeys)
    availkeys.update({\
              'path': '',
              'safe': '',
              'cfg': '',
              "time": '[-1]',
              "extract": '',
              "vars": '',
              'saveas': 'png',
              "sample": '',
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
                      "where": '',
                      "type": ''})
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
    def add_draw(self, draw, rank=''):
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
        self.dids[self.active['type']][self.tasks["xref"]]['deco'] = \
                  self.tasks["deco"]
        self.order.append(self.tasks["xref"])
###############################################################################
    def distribute_deco(self, subtask):
        """
        Distributes decoration.
        """
        vrs = subtask["vars"].split(';')
        for i, val in enumerate(vrs):
            if ':' not in val:
                vrs[i] = val + ':' + self.tasks["deco"]
        subtask["vars"] = ';'.join(vrs)
        return subtask
