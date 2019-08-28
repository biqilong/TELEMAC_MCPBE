r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
# -*- coding: utf-8 -*-
from copy import deepcopy
from os import path
from vvytel.groups import Groups
###############################################################################
class GroupCast(Groups):
    """
    Class using Groups class as argument.
    """
    availkeys = deepcopy(Groups.availkeys)
    availkeys.update({'path': '',
                      'safe': '',
                      'cfg': '',
                      "time": '[-1]',
                      "extract": '',
                      "vars": [],
                      "sample": '',
                      "target": '',
                      "rank": '',
                      "deprefs": '',
                      "outrefs": '',
                      "where": '',
                      "type": '',
                      "config": 'oneofall'})
    groupkeys = deepcopy(Groups.groupkeys)
    groupkeys.update({"vars": '',
                      "time": '',
                      "extract": '',
                      "sample": '',
                      "config": '',
                      "where": '',
                      "type": ''})
    avaylkeys = deepcopy(Groups.avaylkeys)
    avaylkeys.update({"title": '', "target": ''})
###############################################################################
    def __init__(self, xml_file, title='', bypass=True):
        """
        Initialize variables.
        """
        Groups.__init__(self, title, bypass)
        # those you reset
        self.path = path.dirname(xml_file)
        self.safe = self.path
###############################################################################
    def add_cast(self, cast, rank=''):
        """
        Adds cast.
        """
        Groups.add_group(self, cast)
        self.active['path'] = self.path
        self.active['safe'] = self.safe
        if self.dids[self.active['type']][self.tasks["xref"]]['rank'] == '':
            self.dids[self.active['type']][self.tasks["xref"]]['rank'] = rank
        if self.dids[self.active['type']][self.tasks["xref"]]['rank'] == '':
            self.dids[self.active['type']][self.tasks["xref"]]['rank'] = '953'
        self.dids[self.active['type']][self.tasks["xref"]]['rank'] = \
                 int(self.dids[self.active['type']][self.tasks["xref"]]['rank'])
###############################################################################
    def add_python_task(self, code, nametask='python'):
        """
        Adds code to 'python' task.
        """
        self.tasks.update({nametask: code.text})
###############################################################################
    def add_return_task(self, code, nametask='return'):
        """
        Adds code to 'return' task.
        """
        self.tasks.update({nametask: {}})
        for key in code.attrib:
            self.tasks[nametask].update({key: code.attrib[key]})
###############################################################################
    def add_variable_task(self, code, nametask):
        """
        Adds code to any task.
        """
        subtasks = {'xref': nametask}
        for key in self.groupkeys:
            subtasks.update({key: self.tasks[key]})
        for key in self.avaylkeys:
            subtasks.update({key: self.avaylkeys[key]})
        for key in code.attrib:
            subtasks.update({key: code.attrib[key]})
        self.tasks['vars'].append(subtasks)
        return len(self.tasks['vars']) - 1, 'vars'
###############################################################################
    def update(self, d):
        self.dids[self.active["type"]][self.active["xref"]].update(d)
