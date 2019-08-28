# -*- coding: utf-8 -*-
"""
Created on Thu Jun 14 12:03:22 2018

@author: dalledonne
"""
from os import path
from utils.exceptions import TelemacException
from vvytel.telemac_xml_tools import find_targets
from postel.mtlplots.figure_1d import Figure1D
from postel.mtlplots.figure_2d import Figure2D
from postel.mtlplots.myplot2d import DECO_DEFAULT as deco_default_2d
try:
    from mayavi import mlab
    MAYAVI_AVAIL = True
except ImportError:
    MAYAVI_AVAIL = False
if MAYAVI_AVAIL:
    from postel.mtlplots.myplot3d import DECO_DEFAULT as deco_default_3d
    from postel.mtlplots.figure_3d import Figure3D

    # _____________________________________________________//        \\
    # ______________________________________________________>> PLOT <<
    #                                                      \\        //
    # for ploting in xml_root.findall(type_plot):
#if xml_child.tag[0:4] == "plot" and not run_only:
def xml_plot(bypass, cast, caster, dc, do, plot, rank, reports, save,
             xcpt, xml_child, xml_config, xml_file):
    report = {'type': 'plot'}
    report.update(dc.filtrkeys)
    updated = False
    # /!\ type_plot should be in ['plot1d','plot2d','plot3d','plotpv']
    type_plot = xml_child.tag
    if "type" not in xml_child.attrib:
        if len(type_plot) == 4:
            raise TelemacException(\
                'Do not know what dimension your plotted data will '
                'be in: ' + xml_child.attrib["xref"])
        elif len(type_plot) == 6:  # defaults for each type
            if type_plot[4:6] == '1d':
                xml_child.set("type", type_plot[4:6] + ':history')
            if type_plot[4:6] == '2d':
                xml_child.set("type", type_plot[4:6] + ':p-section')
            if type_plot[4:6] == '3d':
                xml_child.set("type", type_plot[4:6] + ':i-surface')
        else:
            raise TelemacException(\
                'the type of your plot should be in: '
                '["plot1d","plot2d","plot3d","plotpv"] for ' +
                xml_child.attrib["xref"])
    else:
        if len(type_plot) == 4:
            if len(xml_child.attrib["type"].split(':')) == 2:
                type_plot += xml_child.attrib["type"].split(':')[0]
            else:
                raise TelemacException(\
                    'do not know what dimension your plotted data '
                    'will be in: ' + xml_child.attrib["xref"])
        elif len(type_plot) == 6:
            if len(xml_child.attrib["type"].split(':')) == 2:
                if type_plot[4:6] != xml_child.attrib["type"].split(':')[0]:
                    raise TelemacException(\
                        'inconsistency in the dimension of your plotted'
                        'data in: ' + xml_child.attrib["xref"])
            else:
                xml_child.attrib["type"] = type_plot[4:6] + ":" + \
                                                  xml_child.attrib["type"]
        else:
            raise TelemacException(\
              'The type of your plot should be in: '
              '["plot1d","plot2d","plot3d","plotpv"] for ' +
              xml_child.attrib["xref"])
    plot.active['type'] = type_plot
    ploting = xml_child
    # ~~ Step 1. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~
    plot.add_draw(ploting, rank)
    report.update({'type': 'plot',
                   'xref': plot.active['xref'],
                   'updt': False,
                   'fail': False,
                   'warn': False,
                   'rank': plot.active['rank'],
                   'value': 0,
                   'title': 'This was ignored'})
    # ~~> Temper with rank but still gather intelligence
    _ = plot.dids[type_plot][plot.active['xref']]['rank']
    # ~~ Step 2. Cumul layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for layer in ploting.findall("layer"):
        index, namex = plot.add_sub_task(layer)
        target = plot.tasks[namex][index]["target"]
        # ~~> round up targets and their configurations looking in exes and
        # does
        xref = target
        src = ''
        if len(target.split(':')) > 1:
            xref, src = target.split(':')
        if xref in cast.dids['cast']:
            if src == '':
                src = plot.tasks[namex][index]['vars'].split(';')[0]\
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
                     'Could not find reference to draw the cast: ' +
                     xref + ':' + src)
            else:
                plot.target_sub_task(layers, index, namex)
        elif src != '':
            if xref in save.dids:
                layers = {}
                one_found = False
                for cfgname in xml_config:
                    one_found = True
                    if cfgname not in save.dids[xref]:
                        continue
                    findlayer = find_targets(save.dids[xref][cfgname], src)
                    if findlayer != []:
                        layers.update({cfgname: findlayer})
                if one_found and layers == {}:
                    raise TelemacException(\
                      'Could not find reference to draw the extract: '
                      + xref + ':' + src)
                else:
                    plot.target_sub_task(layers, index, namex)
            elif xref in do.dids:
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
                         'Could not find reference to draw the action: '\
                         + xref + ':' + src)
                else:
                    plot.target_sub_task(layers, index, namex)
            else:
                raise TelemacException(\
                  'Could not find reference to draw the action: '
                  + xref)
        else:
            if plot.tasks[namex][index]["where"] != '':
                if path.exists(path.join(\
                        plot.tasks[namex][index]["where"], target)):
                    findlayer = {}
                    tmp = path.splitext(path.basename(target))[1].lower()[1:]
                    for cfgname in xml_config:
                        findlayer.update({cfgname:[[path.join(\
                                plot.tasks[namex][index]["where"],\
                                target)], '', tmp]})
                    plot.target_sub_task(findlayer, index, namex)
                else:
                    raise TelemacException(\
                      'Could not find reference to extract the action: '
                      + target + ' where ' +
                      plot.tasks[namex][index]["where"])
            elif path.exists(path.join(plot.path, target)):
                findlayer = {}
                tmp = path.splitext(path.basename(target))[1].lower()[1:]
                for cfgname in xml_config:
                    findlayer.update({cfgname:\
                        [[path.join(plot.path, target)], '', tmp]})
                plot.target_sub_task(findlayer, index, namex)
            else:
                raise TelemacException(\
                   'Could not find reference to extract the action: '+\
                   target)
        # ~~> round up decos, replacing the name by the associated deco dico
        # > at the action level
        if type(plot.tasks['deco']) == type(''):
            if plot.tasks['deco'] != '':
                decos = plot.tasks['deco'].split(';')
                plot.tasks['deco'] = {}
                for deco in decos:
                    if deco in dc.dids['deco']:
                        plot.tasks['deco'].update(dc.dids['deco'][deco])
                    else:
                        raise TelemacException(\
                          'could not find reference to deco tag <' +
                          deco + '> for figure "' + plot.tasks['xref'] +
                          '"')
            elif plot.tasks['xref'] in dc.dids['deco']:
                plot.tasks['deco'] = dc.dids['deco'][plot.tasks['xref']]
            else:
                if type_plot[4:6] == '1d':
                    plot.tasks['deco'] = {}  # {'look':[deco_default_1d]}
                if type_plot[4:6] == '2d':
                    plot.tasks['deco'] = deco_default_2d
                if type_plot[4:6] == '3d':
                    if not MAYAVI_AVAIL:
                        return None, None
                    plot.tasks['deco'] = deco_default_3d
        # > at the layer level
        if type(plot.tasks[namex][index]['deco']) == type(''):
            if plot.tasks[namex][index]['deco'] != '':
                decos = plot.tasks[namex][index]['deco'].split(';')
                plot.tasks[namex][index]['deco'] = {}
                for deco in decos:
                    if deco in dc.dids['deco']:
                        for name in dc.dids['deco'][deco]:
                            if name in ['look', 'data']:
                                plot.tasks[namex][index]['deco']\
                                     .update(dc.dids['deco'][deco][name][0])
                    else:
                        raise TelemacException(\
                          'Could not find reference to layer deco tag: '
                          + deco + ' for figure ' + plot.tasks['xref'])
            else:
                if type_plot[4:6] == '1d':
                    # {'look':[deco_default_1d]}
                    plot.tasks[namex][index]['deco'] = {}
                if type_plot[4:6] == '2d':
                    plot.tasks[namex][index]['deco'] = deco_default_2d
                if type_plot[4:6] == '3d':
                    if not MAYAVI_AVAIL:
                        return None, None
                    plot.tasks[namex][index]['deco'] = deco_default_3d
    plot.update(plot.tasks)
    if xcpt != []:
        raise TelemacException(\
            'Looking at targets in xml_file: ' + xml_file+
            str(xcpt))
    # ~~ Matrix distribution by plot types ~~~~~~~~~~~~~~~~~~~~~~~~~~
    xref = plot.tasks['xref']
    draw = plot.dids[type_plot][xref]
    if not "layers" in draw:
        return None, None
    one_found = False
    for layer in draw["layers"]:
        if layer['file_name'] != {}:
            one_found = True
    if not one_found:
        return None, None
    print('     +> reference: ' + xref + ' of type ' + type_plot)
    # now done with strings as arrays proved to be too challenging
    xlayers = ''
    for layer in draw["layers"]:
        if layer['config'] == 'together':
            xys = []
            for x in xlayers.split('|'):
                xys.append((x + ';' + ':'.join(layer['file_name'].keys()))\
                    .strip(';'))
            xlayers = '|'.join(xys)
        elif layer['config'] == 'distinct':
            xys = []
            for key in layer['file_name']:
                for x in xlayers.split('|'):
                    xys.append((x + ';' + key).strip(';'))
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
    if xlayers == '':
        # TODO: See if needs to be uncommented
       #xcpt.append({\
       #    'name':'run_xml',
       #    'msg':'could not find reference to draw the action: '+target})
        return None, None
    nb_file = 0
    alayers = xlayers.split('|')
    for cfglist in alayers:
        # ~~> Figure name
        if len(alayers) == 1:
            figure_name = '.'.join([xref.replace(' ', '_'), draw['saveas']])
        else:
            nb_file += 1
            figure_name = '.'.join(\
                    [xref.replace(' ', '_'), str(nb_file), draw['saveas']])
        print('         ~> saved as: ' + figure_name)
        figure_name = path.join(path.dirname(xml_file), figure_name)
        # ~~> Create Figure
        if type_plot[4:6] == "1d":
            figure = Figure1D(caster, draw)
        elif type_plot[4:6] == "2d":
            figure = Figure2D(caster, draw)
        elif type_plot[4:6] == "3d":
            if not MAYAVI_AVAIL:
                return None, None
            figure = Figure3D(caster, draw)
        display = False
        for layer, cfgs in zip(draw["layers"], cfglist.split(';')):
            for cfg in cfgs.split(':'):
                display = display or xml_config[cfg]['options'].display
                for fle in layer['file_name'][cfg][0]:
                    figure.add(layer['file_name'][cfg][2],\
                               {'file': fle,
                                'deco': layer["deco"],
                                'xref': xref,
                                'vars': layer["vars"],
                                'extract': layer["extract"],
                                'sample': layer["sample"],
                                'type': layer['type'],
                                'time': layer["time"]})
        if display:
            figure.show()
        else:
            figure.save(figure_name)
        updated = True
    if updated:
        report.update({'updt': updated, 'title': 'My work is done'})
    if xcpt != []:
        raise TelemacException(\
            'Looking at plotting in xml_file: ' + xml_file+
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
