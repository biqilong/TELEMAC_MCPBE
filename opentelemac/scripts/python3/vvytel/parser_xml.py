r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief
"""
from __future__ import print_function
# _____             ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path, chdir
try:
    # Available in python 3.5
    from math import gcd
except ImportError:
    from fractions import gcd
from utils.exceptions import TelemacException
from postel.caster import Caster
from vvytel.group_deco import GroupDeco
from vvytel.action_run import ActionRun
from vvytel.group_plot import GroupPlot
from vvytel.group_get import GroupGet
from vvytel.group_cast import GroupCast
from vvytel.xml_action import xml_action
from vvytel.xml_cast import xml_cast
from vvytel.xml_save import xml_save
from vvytel.xml_plot import xml_plot
####
# Import below not used but must be kept for XML calls
####
from vvytel.math_xml_tools import mapdiff, checkval
from vvytel.telemac_xml_tools import copy_cas
###############################################################################
#    Global dictionnaries to avoid having to read these more than once
#    The keys are the full path to the dictionnaries and therefore
#        allows for <root> and <version> to change
DICOS = {}
# _____                            ________________________________________
# ____/ XML Parser Toolbox /_______________________________________/
#
"""
    Assumes that the directory color_maps_xml is in postel/ (i.e.
    ~root/pytel/postel.)
"""
def check_xml(xml_file, xml_root, xml_config):
    """
    Check tag and rank information from an xml and compare to the one given by
    the user to see if the xml whould be ran

    @param xml_file (string) Path to the xml file
    @param xml_root (xmlstruct) root of xml read through xml.etree
    @param xml_config (dict) Contains configuration information

    @return (rank_nok, tag_nok) (Boolean,Boolean) If True according to
              (rank, tag) the xml shoulb be run
    """
    # ~~ Simplying Ranking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rank_nok = True
    if "rank" in xml_root.keys():
        rank = xml_root.attrib["rank"]
    else:
        raise TelemacException("Missing rank in xml file"+xml_file)
    rankdo = int(rank)
    rankdont = 0
    one_cfg = list(xml_config.keys())[0]
    if xml_config[one_cfg]['options'].rank != '':
        rankdont = int(xml_config[one_cfg]['options'].rank)
    if rankdont == 1:
        rank_nok = False
    if gcd(rankdont, rankdo) == 1:
        rank_nok = False

    tag_nok = False
    # Checking tag to see if the xml should be run
    if 'tags' in xml_root.keys():
        tags = xml_root.attrib["tags"].split(';')
        opt_tags = xml_config[one_cfg]['options'].tags.split(';')
        for tag in tags:
            # If -tag that means that the xml should not be run
            if '-'+tag in opt_tags:
                tag_nok = False
                break
            # Checking that at least one of the tags is in opt_tags
            tag_nok = tag_nok or tag in opt_tags
    else:
        raise TelemacException("Missing tag in xml file:\n"+xml_file)

    return rank_nok, tag_nok
###############################################################################
def run_xml(xml_file, xml_config, reports, bypass, run_only):
    """
    Run the content of a validation xml

    @param xml_file (string) Path to the xml file
    @param xml_config (dict) Contains configuration information
    @param reports (dict) Contains report information
    @param bypass (boolean) If True bypass errors
    @param run_only (boolean) If True only the 'action' part of the xml is done

    @returns (dict) The updated reports
    """
    xcpt = []  # try all keys for full report
    # ~~ Parse xml_file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    import xml.etree.ElementTree as XML
    f = open(xml_file, 'r')
    xml_tree = XML.parse(f)  # may need to try first and report error
    xml_root = xml_tree.getroot()
    f.close()
    # ~~ Checking if we should run the xml
    rank_nok, tag_nok = check_xml(xml_file, xml_root, xml_config)
    # If nothing to do leaving
    if not rank_nok:
        print('     > nothing to do here at this stage (rank)')
        return reports

    if not tag_nok:
        print('     > nothing to do here at this stage (tag)')
        return reports

    rank = xml_root.attrib["rank"]
    one_cfg = list(xml_config.keys())[0]
    pytel = xml_config[one_cfg]['cfg']['pytel']
    pwd = path.dirname(xml_file)
    print('... interpreting XML test specification file: ' + \
            path.basename(xml_file))
    # ~~ Decoration process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    #     This needs to be developed further
    #
    title = ""
    dc = GroupDeco(xml_file, title, bypass)
    dc.add_group_type("deco")
    for decoing in xml_root.findall("deco"):
        # ~~ Step 1. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~
        dc.add_group(decoing)
        # ~~ Step 2. Cumul looks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if len(decoing.findall("look")) > 1:
            raise TelemacException(\
                 'you can only have one look in deco referenced: ' + \
                 dc.tasks["xref"])
        #if len(decoing.findall("look")) > 0:
        if decoing.findall("look"):
            look = decoing.findall("look")[0]
            dc.add_look_task(look)
            for i in range(len(dc.tasks["look"])):
                for cmap in ["cmap", "cmaps", "colourbar"]:
                    if cmap in dc.tasks["look"][i].keys():
                        file_name = dc.tasks["look"][i][cmap]
                        if file_name.split('.')[-1] == 'xml':
                            if not path.exists(file_name) and \
                                path.exists(path.join(pytel, 'postel',
                                                      'color_maps_xml',
                                                      file_name)):
                                dc.tasks["look"][i][cmap] =\
                                path.join(pytel, 'postel',
                                          'color_maps_xml', file_name)
                            else:
                                raise TelemacException(\
                                "... Could not access/read expected colour "
                                "map file content: {}".format(file_name))

        # ~~ Step 2. Cumul decos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if len(decoing.findall("data")) > 1:
            raise TelemacException(\
                    'You can only have one data in deco referenced: ' +\
                    dc.tasks["xref"])
        if decoing.findall("data"):
            data = decoing.findall("data")[0]
            dc.add_data_task(data)
        dc.update(dc.tasks)
    if xcpt != []:
        raise TelemacException(\
                'Looking at deco in xml_file: ' + xml_file+
                str(xcpt))
    # ~~ Looping logic in order of appearance ~~~~~~~~~~~~~~~~~~~~~~~
    do = ActionRun(xml_file, title, bypass)
    save = GroupGet(xml_file, title, bypass)
    for type_save in ["save1d", "save2d", "save3d"]:
        save.add_group_type(type_save)
    plot = GroupPlot(xml_file, title, bypass)
    for type_plot in ["plot1d", "plot2d", "plot3d", "plotpv"]:
        plot.add_group_type(type_plot)
    cast = GroupCast(xml_file, title, bypass)
    cast.add_group_type("cast")
    # ~~> Create casts for future references
    caster = Caster({'object': {}, 'obdata': {}})
    # ~~ The new frontier ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    space = locals()
    for xml_child in xml_root:
        chdir(pwd)
        # ~~ Main action process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #
        #     Whether an action is carried out or not, it is known through:
        #         xml_config[cfgname]['options'].todos
        #     CAS file will still be loaded to register various other files
        #         for possible subsequent extraction, plotting or analysis
        #     TODO: limit the number of path / safe duplication
        # _______________________________________________________//          \\
        # _______________________________________________________>> ACTION <<
        #                                                        \\          //
        # for action in xml_root.findall("action"):
        if xml_child.tag == "action":
            reports, xcpt = xml_action(DICOS, bypass, dc, do, rank, reports,
                                       xcpt, xml_child, xml_config, xml_file)
            # _____________________________________________________//        \\
            # ______________________________________________________>> CAST <<
            #                                                      \\        //
        if xml_child.tag[0:4] == "cast" and not run_only:
            reports, xcpt, space = xml_cast(\
                    bypass, cast, caster, dc, do, rank,
                    reports, save, space, xcpt, xml_child,
                    xml_config, xml_file)
            # _____________________________________________________//        \\
            # ______________________________________________________>> SAVE <<
            #                                                      \\        //
            # did has all the IO references and the latest sortie files
            # for extracting in xml_root.findall(type_save):
        if xml_child.tag[0:4] == "save" and not run_only:
            reports, xcpt = xml_save(\
                    bypass, cast, caster, dc, do, rank,
                    reports, save, type_plot, xcpt, xml_child, xml_config,
                    xml_file)
            # _____________________________________________________//        \\
            # ______________________________________________________>> PLOT <<
            #                                                      \\        //
            # for ploting in xml_root.findall(type_plot):
        if xml_child.tag[0:4] == "plot" and not run_only:
            reports, xcpt = xml_plot(\
                    bypass, cast, caster, dc, do, plot, rank,
                    reports, save, xcpt, xml_child, xml_config, xml_file)
    # ~~ Final report summary ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return reports
# _____                 ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#
__author__ = "Sebastien E. Bourban; Juliette C. Parisi; David H. Roscoe"
__date__ = "$2-Aug-2011 11:51:36$"
