r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
# -*- coding: utf-8 -*-
# _____                                _____________________________________
# ____/ Secondary Class: DECO /____________________________________/
#
from os import path
from copy import deepcopy
from vvytel.groups import Groups
###############################################################################
class GroupDeco(Groups):
    """
    Class using Groups class as argument.
    """
    availkeys = deepcopy(Groups.availkeys)
    groupkeys = deepcopy(Groups.groupkeys)
    filtrkeys = {"title": '', "contact": '', "author": ''}
###############################################################################
    def __init__(self, xml_file, title='', bypass=True):
        """
        Initialize variables.
        """
        Groups.__init__(self, title, bypass)
        # those you reset
        self.path = path.dirname(xml_file)
        # those you need to see in the XML file
        self.active["deco"] = {}
###############################################################################
    def add_draw(self, deco):
        """
        Adds decoration.
        """
        Groups.add_group(self, deco)
        self.active['path'] = self.path
###############################################################################
    def add_look_task(self, layer, nametask='look'):
        """
        Adds layer to 'look' task.
        """
        # self.avaylkeys = deepcopy(Groups.avaylkeys)
        self.avaylkeys = {}
        for key in layer.attrib:
            self.avaylkeys.update({key: layer.attrib[key]})
        return Groups.add_sub_task(self, layer, nametask)
###############################################################################
    def add_data_task(self, layer, nametask='data'):
        """
        Adds layer to 'data' task.
        """
        self.avaylkeys = deepcopy(Groups.avaylkeys)
        self.avaylkeys.update(self.filtrkeys)
        return Groups.add_sub_task(self, layer, nametask)
