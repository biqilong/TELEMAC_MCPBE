# -*- coding: utf-8 -*-
"""
Created on Thu Jun 14 11:09:11 2018

@author: dalledonne
"""
from os import path
from utils.exceptions import TelemacException
from vvytel.telemac_xml_tools import find_targets
from postel.mtlplots.myplot1d import DECO_DEFAULT as deco_default_1d
from postel.mtlplots.myplot2d import DECO_DEFAULT as deco_default_2d
try:
    from mayavi import mlab
    MAYAVI_AVAIL = True
except ImportError:
    MAYAVI_AVAIL = False
if MAYAVI_AVAIL:
    from postel.mtlplots.myplot3d import DECO_DEFAULT as deco_default_3d
from postel.mtlplots.dumper_1d import Dumper1D
from postel.mtlplots.dumper_2d import Dumper2D
from postel.mtlplots.dumper_3d import Dumper3D
    # _____________________________________________________//        \\
    # ______________________________________________________>> SAVE <<
    #                                                      \\        //
    # did has all the IO references and the latest sortie files
    # for extracting in xml_root.findall(type_save):
#if xml_child.tag[0:4] == "save" and not run_only:
def xml_save(bypass, cast, caster, dc, do, rank, reports, save, type_plot, xcpt,
             xml_child, xml_config, xml_file):
    report = {'type': 'save'}
    report.update(dc.filtrkeys)
    updated = False
    # /!\ type_save should be in ['save1d','save2d','save3d']
    type_save = xml_child.tag
    # TODO: This will eventually be removed
    if "type" not in xml_child.attrib:
        if len(type_save) == 4:
            raise TelemacException(\
                'Do not know what dimension your saved data will '\
                'be in: ' + xml_child.attrib["xref"])
        elif len(type_save) == 6:  # defaults for each type
            if type_plot[4:6] == '1d':
                xml_child.set("type", type_plot[4:6] + ':history')
            if type_plot[4:6] == '2d':
                xml_child.set("type", type_plot[4:6] + ':p-section')
            if type_plot[4:6] == '3d':
                xml_child.set("type", type_plot[4:6] + ':i-surface')
        else:
            raise TelemacException(\
                'The type of your saved data should be in: '\
                '["save1d","save2d","save3d"] for ' + \
                xml_child.attrib["xref"])
    else:
        if len(type_save) == 4:
            if len(xml_child.attrib["type"].split(':')) == 2:
                type_save = type_save + xml_child.attrib["type"].split(':')[0]
            else:
                raise TelemacException(\
                    'Do not know what dimension your saved data will '\
                    'be in: ' + xml_child.attrib["xref"])
        elif len(type_save) == 6:
            if len(xml_child.attrib["type"].split(':')) == 2:
                if type_save[4:6] != xml_child.attrib["type"].split(':')[0]:
                    raise TelemacException(\
                        'Inconsistency in the dimension of your plotted'\
                        ' data in: ' + xml_child.attrib["xref"])
            else:
                xml_child.attrib["type"] = type_save[4:6] + ":" + \
                                           xml_child.attrib["type"]
        else:
            raise TelemacException(\
                    'the type of your saved data should be in: '\
                    '["save1d","save2d","save3d"] for ' +\
                    xml_child.attrib["xref"])
    save.active['type'] = type_save
    extracting = xml_child
    # ~~ Step 1. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~
    save.add_group(extracting, rank)
    report.update({'xref': save.active['xref'],
                   'updt': False,
                   'fail': False,
                   'warn': False,
                   'rank': save.active['rank'],
                   'value': 0,
                   'title': 'This was ignored'})
    # ~~> Temper with rank but still gather intelligence
    _ = save.dids[type_save][save.active['xref']]['rank']
    # ~~> Default output formats
    if save.dids[type_save][save.active['xref']]['saveas'] == '':
        if type_save[4:6] == "1d":
            save.dids[type_save][save.active['xref']]['saveas'] = 'csv'
        if type_save[4:6] == "2d":
            save.dids[type_save][save.active['xref']]['saveas'] = 'slf'
        if type_save[4:6] == "3d":
            save.dids[type_save][save.active['xref']]['saveas'] = 'slf'
    # ~~ Step 2. Cumul layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for layer in extracting.findall("layer"):
        index, namex = save.add_sub_task(layer)
        target = save.tasks[namex][index]["target"]
        # ~~> round up targets and their configurations looking in exes and
        # does
        xref = target
        src = ''
        if len(target.split(':')) > 1:
            xref, src = target.split(':')
        if xref in cast.dids['cast']:
            if src == '':
                src = save.tasks[namex][index]['vars'].split(';')[0]\
                                                                  .split(':')[0]
            layers = {}
            one_found = False
            findlayer = []
            for cfgname in xml_config:
                for var in cast.dids['cast'][xref]['vars']:
                    if src == var['xref']:
                        one_found = True
                        findlayer = [[src], '', '']
                if findlayer != []:
                    layers.update({cfgname: findlayer})
            if layers == {}:
                raise TelemacException(\
                        'Could not find reference to save the cast: ' + \
                        xref + ':' + src)
            else:
                save.target_sub_task(layers, index, namex)
        elif src != '':
            if xref in do.dids:
                layers = {}
                one_found = False
                for cfgname in xml_config:
                    one_found = True
                    if cfgname not in do.dids[xref]:
                        continue
                    findlayer = find_targets(do.dids[xref][cfgname], src)
                    if findlayer != []:
                        layers.update({cfgname: findlayer})
                if one_found and layers == {}:
                    raise TelemacException(\
                        'could not find reference to extract within '\
                        'actions: ' + xref + ':' + src)
                else:
                    save.target_sub_task(layers, index, namex)
            else:
                raise TelemacException(\
                    'Could not find reference to extract the action: '\
                    + xref + ':' + src)
        else:
            if save.tasks[namex][index]["where"] != '':
                if path.exists(path.join(\
                        save.tasks[namex][index]["where"], target)):
                    findlayer = {}
                    tmp = path.splitext(path.basename(target))[1].lower()[1:]
                    for cfgname in xml_config:
                        findlayer.update({cfgname:[[path.join(\
                                save.tasks[namex][index]["where"],\
                                target)], '', tmp]})
                    save.target_sub_task(findlayer, index, namex)
                else:
                    raise TelemacException(\
                            'Could not find reference to extract the '\
                            'action: ' + target + ' where ' +\
                            save.tasks[namex][index]["where"])
            elif path.exists(path.join(save.path, target)):
                findlayer = {}
                tmp = path.splitext(path.basename(target))[1].lower()[1:]
                for cfgname in xml_config:
                    findlayer.update({cfgname: [[\
                              path.join(save.path, target)], '', tmp]})
                save.target_sub_task(findlayer, index, namex)
            else:
                raise TelemacException(\
                    'Could not find reference to extract the action: '\
                    + target)
        # ~~> round up decos, replacing the name by the associated deco dico
        # > at the action level
        if type(save.tasks['deco']) == type(''):
            if save.tasks['deco'] != '':
                decos = save.tasks['deco'].split(';')
                save.tasks['deco'] = {}
                for deco in decos:
                    if deco in dc.dids['deco']:
                        save.tasks['deco'].update(dc.dids['deco'][deco])
                    else:
                        raise TelemacException(\
                          'could not find reference to deco tag <' + \
                          deco + '> for figure "' +\
                          save.tasks['xref'] + '"')
            elif save.tasks['xref'] in dc.dids['deco']:
                save.tasks['deco'] = dc.dids['deco'][save.tasks['xref']]
            else:
                if type_save[4:6] == '1d':
                    save.tasks['deco'] = deco_default_1d
                if type_save[4:6] == '2d':
                    save.tasks['deco'] = deco_default_2d
                if type_save[4:6] == '3d':
                    if not MAYAVI_AVAIL:
                        return None, None
                    save.tasks['deco'] = deco_default_3d
        # > at the layer level
        if type(save.tasks[namex][index]['deco']) == type(''):
            if save.tasks[namex][index]['deco'] != '':
                decos = save.tasks[namex][index]['deco'].split(';')
                save.tasks[namex][index]['deco'] = {}
                for deco in decos:
                    if deco in dc.dids['deco']:
                        for name in dc.dids['deco'][deco]:
                            if name in ['look', 'data']:
                                save.tasks[namex][index]['deco']\
                                     .update(dc.dids['deco'][deco][name][0])
                    else:
                        raise TelemacException(\
                            'could not find reference to deco tag <' + \
                            deco + '> for figure "' +\
                            save.tasks['xref'] + '"')
            else:
                if type_save[4:6] == '1d':
                    save.tasks[namex][index]['deco'] = deco_default_1d
                if type_save[4:6] == '2d':
                    save.tasks[namex][index]['deco'] = deco_default_2d
                if type_save[4:6] == '3d':
                    save.tasks[namex][index]['deco'] = deco_default_3d
    save.update(save.tasks)
    if xcpt != []:
        raise TelemacException(\
            'looking at extractions in xml_file: ' + xml_file+
            str(xcpt))
    # ~~ Matrix distribution by extraction types ~~~~~~~~~~~~~~~~~~~~
    xref = save.tasks['xref']
    task = save.dids[type_save][xref]
    if not "layers" in task:
        return None, None
    one_found = False
    for layer in task["layers"]:
        if layer['file_name'] != {}:
            one_found = True
    if not one_found:
        return None, None
    print('     +> reference: ' + xref + ' of type ' + type_save)
    # now done with strings as arrays proved to be too challenging
    xlayers = ''
    for layer in task["layers"]:
        if layer['config'] == 'together':
            xys = []
            for x in xlayers.split('|'):
                xys.append((x + ';' + ':'.join(layer['file_name'].keys()))\
                    .strip(';'))
            xlayers = '|'.join(xys)
        elif layer['config'] == 'distinct':
            ylayers = layer['file_name'].keys()
            xys = []
            for i in range(len(ylayers)):
                for x in xlayers.split('|'):
                    xys.append((x + ';' + ylayers[i]).strip(';'))
            xlayers = '|'.join(xys)
        elif layer['config'] == 'oneofall':
            xys = []
            # /!\ you are sure to have at least one (?)
            cfg = list(layer['file_name'])[0]
            for x in xlayers.split('|'):
                xys.append((x + ';' + cfg).strip(';'))
            xlayers = '|'.join(xys)
        else:
            if layer['config'] in layer['file_name']:
                xys = []
                for x in xlayers.split('|'):
                    xys.append((x + ';' + layer['config']).strip(';'))
            xlayers = '|'.join(xys)
    nb_file = 0
    alayers = xlayers.split('|')
    for cfglist in alayers:
        # ~~> Figure name
        if len(alayers) == 1:
            extract_name = '.'.join([xref.replace(' ', '_'), task['saveas']])
        else:
            nb_file += 1
            extract_name = '.'.join(\
                    [xref.replace(' ', '_'), str(nb_file), task['saveas']])
        print('         ~> saved as: ' + extract_name)
        extract_name = path.join(path.dirname(xml_file), extract_name)
        # ~~> Create Figure
        if type_save[4:6] == "1d":
            figure = Dumper1D(caster, task)
        if type_save[4:6] == "2d":
            figure = Dumper2D(caster, task)
        if type_save[4:6] == "3d":
            if not MAYAVI_AVAIL:
                return None, None
            figure = Dumper3D(caster, task)
        for layer, cfgs in zip(task["layers"], cfglist.split(';')):
            for cfg in cfgs.split(':'):
                for fle in layer['file_name'][cfg][0]:
                    figure.add(layer['file_name'][cfg][2],\
                               {'file': fle,
                                'deco': {},
                                'xref': xref,
                                'vars': layer["vars"],
                                'extract': layer["extract"],
                                'sample': layer["sample"],
                                'type': task['type'],
                                'time': layer["time"]})
        figure.save(extract_name)
        updated = True
    if updated:
        report.update({'updt': updated, 'title': 'My work is done'})
    if xcpt != []:
        raise TelemacException(\
            'Looking at savings in xml_file: ' + xml_file+
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
