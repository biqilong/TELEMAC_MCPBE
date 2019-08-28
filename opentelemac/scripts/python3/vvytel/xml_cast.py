# -*- coding: utf-8 -*-
"""
Created on Thu Jun 14 09:38:59 2018

@author: dalledonne
"""
from os import path
try:
    # Available in python 3.5
    from math import gcd
except ImportError:
    from fractions import gcd
from argparse import Namespace
from utils.exceptions import TelemacException
from vvytel.telemac_xml_tools import find_targets
####
# Import below not used but must be kept for XML calls
####
from vvytel.math_xml_tools import mapdiff, checkval
from vvytel.telemac_xml_tools import copy_cas
import numpy as np
    # _____________________________________________________//        \\
    # ______________________________________________________>> CAST <<
    #                                                      \\        //
#if xml_child.tag[0:4] == "cast" and not run_only:
def xml_cast(bypass, cast, caster, dc, do, rank, \
             reports, save, space, xcpt, xml_child, xml_config, xml_file):
    report = {'type': 'cast'}
    report.update(dc.filtrkeys)
    type_cast = xml_child.tag
    cast.active['type'] = type_cast
    casting = xml_child
    # ~~ Step 1. Common check for keys ~~~~~~~~~~~~~~~~~~~~~~~~
    cast.add_cast(casting, rank)
    report.update({\
            'xref': cast.tasks['xref'],
            'updt': False,
            'rank': cast.tasks['rank']})
    # ~~ Step 2. Python code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if len(casting.findall("python")) > 1:
        raise TelemacException(\
            'You can only have one python key in cast referenced: '+\
            cast.tasks["xref"])
    if len(casting.findall("python")) > 0:
        code = casting.findall("python")[0]
        cast.add_python_task(code)
    # ~~ Step 3. Return statement ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if len(casting.findall("return")) > 1:
        raise TelemacException(\
            'you can only have one return key in cast referenced: '+\
            cast.tasks["xref"])
    if len(casting.findall("return")) > 0:
        code = casting.findall("return")[0]
        cast.add_return_task(code)
    # ~~ Step 2. Variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for vari in casting.getchildren():
        if vari.tag in ['python', 'return']:
            continue
        if vari.tag in cast.tasks:
            raise TelemacException(\
                 'The variable {} is cast more than once in {}'\
                 .format(vari.tag, cast.tasks["xref"]))
        index, namex = cast.add_variable_task(vari, vari.tag)
        target = cast.tasks[namex][index]["target"]
        # ~~> just casting, no specific targets
        if target == '':
            cast.tasks[namex][index].update({\
                 'file_name': {},
                 'target': cast.tasks["xref"]})
            continue
        # ~~> round up targets and their configurations looking in exes and
        # does
        xref = target
        src = ''
        if len(target.split(':')) > 1:
            xref, src = target.split(':')
        xdid = ''
        for did in cast.dids:
            if xref in cast.dids[did].keys():
                xdid = did
        if xdid != '':
            if src not in caster.obdata.keys():
                raise TelemacException(\
                     'Did not already cast variable ' + src + \
                     ' wihtin ' + xref)
            layers = {}
            for cfgname in xml_config:
                # TODO: maybe you do not need xref
                layers.update({cfgname: [[xref + ':' + src], '', '']})
            if layers != {}:
                cast.target_sub_task(layers, index, namex)
            continue  # try finding the next variable
        for did in save.dids:
            if xref in save.dids[did].keys():
                xdid = did
        # saved files can be reopened as target to do something else
        if xdid != '':
            # TODO: 'saveas' may not be correct
            target = xref + '.' + save.dids[xdid][xref]['saveas']
            if save.dids[xdid][xref]["where"] != '':
                if path.exists(path.join(\
                        save.dids[xdid][xref]["where"], target)):
                    findlayer = {}
                    tmp = path.splitext(path.basename(target))[1].lower()[1:]
                    for cfgname in xml_config:
                        findlayer.update({cfgname:\
                            [[path.join(save.dids[xdid][xref]["where"],\
                                        target)], '', tmp]})
                    cast.target_sub_task(findlayer, index, namex)
                else:
                    raise TelemacException(\
                       'Could not find reference to extract the cast: '+\
                       target + ' where '+\
                       cast.tasks[namex][index]["where"])
            elif path.exists(path.join(save.path, target)):
                findlayer = {}
                tmp = path.splitext(path.basename(target))[1].lower()[1:]
                for cfgname in xml_config:
                    findlayer.update({\
                            cfgname: [[path.join(save.path, target)], '', tmp]})
                cast.target_sub_task(findlayer, index, namex)
            continue  # try finding the next variable
        if xref in do.dids:
            layers = {}
            one_found = False
            for cfgname in xml_config:
                if cfgname not in do.dids[xref]:
                    continue
                one_found = True
                findlayer = find_targets(do.dids[xref][cfgname], src)
                if findlayer != []:
                    layers.update({cfgname: findlayer})
            if one_found and layers == {}:
                raise TelemacException(\
                     'Could not find reference to cast the '\
                     'action: {}:{}'.format(xref, src))
            else:
                cast.target_sub_task(layers, index, namex)
            continue  # try finding the next variable
        if src == '':
            if cast.tasks["where"] != '':
                if path.exists(path.join(cast.tasks["where"], target)):
                    findlayer = {}
                    tmp = path.splitext(path.basename(target))[1].lower()[1:]
                    for cfgname in xml_config:
                        findlayer.update({cfgname:\
                            [[path.join(cast.tasks["where"], target)],\
                              '', tmp]})
                    cast.target_sub_task(findlayer, index, namex)
                else:
                    raise TelemacException(\
                      'could not find reference to extract the cast: '+\
                      target + ' where ' + cast.tasks["where"])
            elif path.exists(path.join(cast.path, target)):
                findlayer = {}
                tmp = path.splitext(path.basename(target))[1].lower()[1:]
                for cfgname in xml_config:
                    findlayer.update({cfgname:\
                        [[path.join(cast.path, target)], '', tmp]})
                cast.target_sub_task(findlayer, index, namex)
            else:
                raise TelemacException(\
                    'Could not find reference to cast: ' + target)
    cast.update(cast.tasks)
    if xcpt != []:
        raise TelemacException(\
             'Looking at casting in xml_file: ' + xml_file+
             str(xcpt))
    # ~~ Matrix distribution by casting types ~~~~~~~~~~~~~~~~~~~~
    # /!\ you can only have one of them per variable
    xref = cast.tasks['xref']
    task = cast.dids[type_cast][xref]
    print('     +> reference: ' + xref + ' of type ' + type_cast)
    # now done with strings as arrays proved to be too challenging
    xvars = ''
    for var in task["vars"]:
        if task['config'] == 'together':
            raise TelemacException(\
                'Could not have more than one configuration at '
                'a time for casting: ' + var['xref'])
        elif task['config'] == 'distinct':
            raise TelemacException(\
                'Could not have more than one configuration at a '
                'time for casting: ' + var['xref'])
        elif task['config'] == 'oneofall':
            xys = []
            if var['file_name'] != {}:
                cfg = list(var['file_name'])[0]
            else:
                cfg = '-'
            for x in xvars.split('|'):
                xys.append((x + ';' + cfg).strip(';'))
            xvars = '|'.join(xys)
        else:
            xys = []
            if var['file_name'] != {}:
                if task['config'] in var['file_name']:
                    for x in xvars.split('|'):
                        xys.append((x + ';' + task['config']).strip(';'))
            else:
                for x in xvars.split('|'):
                    xys.append((x + ';' + '-').strip(';'))
            xvars = '|'.join(xys)
    # ~~> First, loading the user python (this is not executing anything
    # yet ...)
    if 'python' in cast.tasks:
        exec(cast.tasks['python'])
    # ~~> Second, casting and extracting data
    avars = xvars.split('|')
    for cfglist in avars:
        # ~~> Cast all variables
        for var, cfgs in zip(task["vars"], cfglist.split(';')):
            for cfg in cfgs.split(':'):
                if var['file_name'] != {}:
                    # you just need one for a cast
                    cfg = list(var['file_name'])[0]
                # more practical than a dict {'support':None, 'values':None}
                space[var['xref']] = Namespace()
                if var['file_name'] != {} and var['file_name'][cfg][0] != []:
                    file_name = var['file_name'][cfg]
                    var.update({'file': file_name[0][0]})
                    # Casting file
                    added = caster.add(file_name[2], var)
                    if not added:
                        print('            > will ignore this for now. {}'\
                              .format(var['xref']))
                        space[var['xref']] = file_name[0][0]
                    else:
                        space[var['xref']] = caster.get(file_name[2], var)
                else:
                    local_space = space.copy()
                    local_space.update(locals())
                    res = eval(var["vars"], globals(), local_space)
                    # The returned value should be a tuple
                    if type(res) != type(()):
                        raise TelemacException(\
                            'The result of the function ' +
                            var["vars"] + ' for ' + cast.tasks["xref"] +
                            ' should be a tuple 4 arguments: '
                            'time,names,support,values')
                    # The return value should be of dimension 4
                    if len(res) != 4:
                        raise TelemacException(\
                            'The result of the function ' + var["vars"]+
                            ' for ' + cast.tasks["xref"] +
                            ' should have 4 arguments: '
                            'time,names,support,values')
                    time, names, support, values = res
                    caster.set(var['xref'], Namespace(**{\
                            'support': support,
                            'values': values,
                            'names': names,
                            'time': time}))
                    space[var['xref']].support = support
                    space[var['xref']].values = values
                    space[var['xref']].names = names
                    space[var['xref']].time = time
    # ~~> Last but not least, validating
    report.update({'updt': True})
    if 'return' not in cast.tasks:
        report.update({'fail': False,
                       'warn': False,
                       'value': 0,
                       'title': 'My Work is done'})
    else:
        for var in cast.tasks['return']:
            if var in ['fail', 'warn', 'value']:
                res = eval(cast.tasks['return'][var], globals(), space)
                report.update({var: res})
                print('          - cast: ' + var + ' = ' + repr(res) + \
                        ' ( expression: ' + cast.tasks['return'][var] + ' )')
                # if epsilon was reached then we crash
                if var in ['fail'] and res:
                    # only one item here
                    raise TelemacException(\
                        'Epsilon was reached for the test case with "'
                        + cast.tasks['return'][var] + '"')
            else:
                report.update({var: cast.tasks['return'][var]})
        report.update({'title': cast.tasks['return']['fail']})
    if xcpt != []:
        raise TelemacException(\
            'Looking at casting in xml_file: ' + xml_file+
            str(xcpt))
    one_found = False
    for i in range(len(reports)):
        # /!\ You are sure there is one xref (?)
        if reports[i]['xref'] == report['xref']:
            reports[i] = report
            one_found = True
    if not one_found:
        reports.append(report)
    return reports, xcpt, space
