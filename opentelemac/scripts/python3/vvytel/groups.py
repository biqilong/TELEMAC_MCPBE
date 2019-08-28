r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
# -*- coding: utf-8 -*-
# _____                             _____________________________________
# ____/ Primary Class: Groups /____________________________________/
#
from copy import deepcopy
from vvytel.telemac_xml_tools import get_xml_keys
from utils.exceptions import TelemacException
###############################################################################
class Groups(object):
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
         - 'cmaps', refer to the directory 'ColourMaps' for colour plotting
         - 'deprefs', refer to the files that need copying from path to safe
         - 'outrefs', refer to the files that need copying from safe to path
         - 'where' will not default to xref anymore, allowing files to
         be shared and located at the same place between groups
    """
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                           General Methods
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    availacts = ''
    availkeys = {"xref": None, "deco": ''}
    groupkeys = {}
    avaylkeys = {}
    filtrkeys = {}
###############################################################################
    def __init__(self, title='', bypass=True):
        """
        Initialize variables.
        """
        self.active = deepcopy(self.availkeys)
        if title != '':
            self.active["title"] = title
        self.bypass = bypass
        self.dids = {}
        # those you need to see in the XML file
        self.active["target"] = None
        self.active["code"] = None
        self.active["do"] = None
        self.active["type"] = None
        # additional entities
        self.tasks = {}
###############################################################################
    def add_group_type(self, group):
        """
        Adds group to 'type'.
        """
        self.dids.update({group: {}})
        self.active['type'] = group
###############################################################################
    def add_group(self, group, rank=''):
        """
        Adds group to rank.
        """
        tasks = deepcopy(self.availkeys)
        self.tasks = get_xml_keys(group, tasks)
        self.active['xref'] = self.tasks["xref"]
        if self.tasks["xref"] in self.dids[self.active['type']]:
            raise TelemacException(\
                   'you are getting me confused, this xref '
                   'already exists: ' + self.tasks["xref"])
        self.dids[self.active['type']].update({self.tasks["xref"]: self.tasks})
###############################################################################
    def update(self, cfgname):
        """
        Update configuration
        """
        self.dids[self.active['type']][self.active['xref']].update(cfgname)
###############################################################################
    def add_sub_task(self, layer, nametask='layers'):
        """
        Adds layer to 'layers' subtask.
        """
        # ~~> set default from the upper grouper
        subtasks = {}
        for k in self.groupkeys:
            subtasks.update({k: self.tasks[k]})
        for k in self.avaylkeys:
            subtasks.update({k: self.avaylkeys[k]})
        # ~~> reset from layer
        subtasks = get_xml_keys(layer, subtasks)
        # ~~> filling-in remaining gaps
        subtasks = self.distribute_deco(subtasks)
        for k in subtasks:
            if k in self.filtrkeys:
                if subtasks[k] != '':
                    self.filtrkeys[k] = subtasks[k]
        # ~~> adding subtask to the list of tasks
        if nametask in self.tasks:
            self.tasks[nametask].append(subtasks)
        else:
            self.tasks.update({nametask: [subtasks]})
        return len(self.tasks[nametask]) - 1, nametask
###############################################################################
    def target_sub_task(self, target, index=0, nametask='layers'):
        """
        Selects nametask from 'file_name'.
        """
        self.tasks[nametask][index].update({'file_name': target})
        if nametask not in self.dids[self.active['type']][self.active['xref']]:
            self.dids[self.active['type']][self.active['xref']].update(\
                     {nametask: [self.tasks[nametask][index]]})
###############################################################################
    def distribute_deco(self, subtask):
        """
        Distributes decoration.
        """
        return subtask
###############################################################################
    def deco_tasks(self, deco={}, index=0, nametask='layers'):
        """
        Sets default.
        """
        self.tasks[nametask][index]['deco'] = deco
