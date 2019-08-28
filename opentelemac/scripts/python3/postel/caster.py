r"""@author TELEMAC-MASCARET Consortium
    @brief
"""
from __future__ import print_function
import numpy as np
from data_manip.extraction.parser_csv import CSV
from postel.parser_output import OutputFileData
from pretel.meshes import xys_locate_mesh
from postel.mycast import get_kdtree, get_mpl_tri, split_quad_to_triangle
from postel.mycast import what_vars_slf, what_time_slf, what_sample
from postel.cast_telemac_file import CastTelemacFile
from optparse import Values
from data_manip.extraction.parser_selafin import get_value_polyline
from utils.exceptions import TelemacException

class Caster():

    def __init__(self, caster={'object':{}, 'obdata':{}}):
        self.object = caster['object']  # refered to by file names
        self.obdata = caster['obdata']  # refered to data extracted in layers

    def add(self, typl, what):
        # ~~> cast already prepared becasue .add is always called
        if typl == '':
            return True
        # ~~> bundling of the layers becasue .add is always called
        if what["xref"] not in self.obdata.keys():
            obdata = Values()
            obdata.type = ''
            obdata.unit = []
            obdata.support = []
            obdata.function = []
            obdata.values = []
            self.obdata.update({what["xref"]:obdata})
        # ~~> file references
        if what['file'] in self.object.keys():
            return True
        # ~~> unexplored teritory
        if 'sortie' in typl.lower():
            self.object.update({what['file']:OutputFileData(what['file'])})
        elif 'csv' in typl.lower() or 'prn' in typl.lower():
            self.object.update({what['file']:CSV(what['file'])})
        elif 'tif' in typl.lower() or 'jpg' in typl.lower()  \
          or 'gif' in typl.lower() or 'png' in typl.lower()   \
          or 'bmp' in typl.lower():
            self.object.update({what['file']:'image'})
        # ~~> SELAFIN file
        elif 'SELAFIN' in typl.upper() or \
             'WACLEO' in typl.upper() or \
             'slf' in typl.lower() or \
             'med' in typl.lower() or \
             'spe' in typl.lower():
            slf = CastTelemacFile(what['file'])
            slf.set_kd_tree()
            # Only doing triangulation if we have triangles...
            ndp = slf.get_mesh_npoin_per_element()
            if ndp == 3:
                slf.set_mpl_tri()
            self.object.update({what['file']:slf})
        else:
            return False

        return True

    def set(self, vref, cast):
        if vref in self.obdata.keys():
            raise TelemacException(\
                '... cast reference already used: {}'.format(vref))
        self.obdata.update({vref:cast})

    def get(self, typl, what):
        if typl == '':
            if what['file'] in self.obdata.keys():
                return self.obdata[what['file']]
            raise TelemacException(\
                    '... I did not cast '
                    'the following reference: {}'.format(what['file']))
        if what['file'] not in self.object:
            raise TelemacException(\
                    '... the cast does not include reference to '
                    'your file : {}'.format(what['file']))
        if what['type'][0:2].lower() == '1d':
            return self.get_1d(typl, what)
        elif what['type'][0:2].lower() == '2d':
            return self.get_2d(typl, what)
        elif what['type'][0:2].lower() == '3d':
            return self.get_3d(typl, what)
        else:
            raise TelemacException(\
                    '... do not know how to extract '
                    'from this key: {}'.format(what['type'][0:2]))

    def get_1d(self, typl, what):
        obj = self.object[what['file']]
        if 'sortie' in typl.lower():
            # what['vars']: list of pairs variables:support delimited by ';'
            time, values = obj.get_value_history_sortie(what["vars"])
            self.obdata[what["xref"]] =\
                      Values({'type':what['type'],
                              'unit':time[0],
                              'support':time[1],
                              'function':values[0][0:-1],
                              'values':values[0][-1]})
        elif 'csv' in typl.lower() or 'prn' in typl.lower():
            # what['vars']: list of pairs variables:support delimited by ';'
            dist, values = obj.get_columns(what["vars"])
            self.obdata[what["xref"]] = \
                      Values({'type':what['type'],
                              'unit':dist[0],
                              'support':dist[1],
                              'function':values[0][0:-1],
                              'values':values[0][-1]})
        # ~~> SELAFIN file
        elif 'SELAFIN' in typl.upper() or \
             'WACLEO' in typl.upper() or \
             'slf' in typl.lower() or \
             'med' in typl.lower() or \
             'spe' in typl.lower():
            # ~~> 1D time history from 2D or 3D results
            if what['type'].split(':')[1] == 'history':
                time, values = obj.cast_history_at_points(what["vars"],
                                                          what["time"],
                                                          what["extract"])
                self.obdata[what["xref"]] = \
                          Values({'type':what['type'],
                                  'unit':time[0],
                                  'support':time[1],
                                  'function':values[0][0:-1],
                                  'values':values[0][-1]})
            # ~~> 1D spatial profiles from 2D or 3D results
            elif what['type'].split(':')[1] == 'v-section':
                dist, values = obj.cast_profile_at_polyline(what["vars"],
                                                            what["time"],
                                                            what["extract"])
                self.obdata[what["xref"]] = \
                          Values({'type':what['type'],
                                  'unit':dist[0],
                                  'support':dist[1],
                                  'function':values[0][0:-1],
                                  'values':values[0][-1]})
                # values[0][-1] is of the shape:
                #  [ [ [ [x0,x1,.]p0,[x0,x1,.]p1,. ]t0, \
                #                    [ [x0,x1,.]p0,[x0,x1,.]p1,. ]t1,. ]v0,
                #    [ [ [x0,x1,.]p0,[x0,x1,.]p1,. ]t0, \
                #                    [ [x0,x1,.]p0,[x0,x1,.]p1,. ]t1,. ]v1,
                #    ... ]
            else:
                raise TelemacException(\
                        '... do not know how to draw '
                        'this SELAFIN type: {}'.format(what['type']))
        else:
            raise TelemacException(\
                    '... do not know how to extract '
                    'from this format: {}'.format(what['type']))
        return self.obdata[what["xref"]]

    def get_2d(self, typl, what):

        obj = self.object[what['file']]

        # /!\ WACLEO: Temporary fix because TOMAWAC's IOs names are not yet
        # standard TELEMAC
        if 'WACLEO' in typl.upper() or \
           'SELAFIN' in typl.upper() or \
           'slf' in typl.lower() or\
           'spe' in typl.lower() or\
           'med' in typl.lower():

            if what['type'].split(':')[1] == 'v-section':

                # ~~> Extract data
                ikle4, mesh_x, mesh_z, support3d = obj.cast_vmesh_at_polyline(\
                    what["time"], what["extract"])
                # split each quad into triangles
                ikle3 = split_quad_to_triangle(ikle4)
                myvars, vtypes = what_vars_slf(what['vars'], obj.varnames)
                time = what_time_slf(what['time'], obj.tags['times'])
                tree = get_kdtree(mesh_x, mesh_z, ikle3)
                tria = get_mpl_tri(mesh_x, mesh_z, ikle3)[0]
                data = get_value_polyline(obj, time, support3d,myvars)

                # ~~> Possible sampling of the data
                if what["sample"] != '':
                    supmesh_x = mesh_x
                    supmesh_z = mesh_z
                    mesh_x = []
                    mesh_z = []
                    ikle4 = []
                    support2d = []
                    grids, _ = what_sample(what["sample"],
                                           [(min(supmesh_x), min(supmesh_z)),
                                            (max(supmesh_x), max(supmesh_z))])
                    for dimx, dimy, x, y in grids:
                        for xyi in np.dstack((x, y))[0]:
                            support2d.append(xys_locate_mesh(\
                                    xyi, ikle3,
                                    supmesh_x, supmesh_z,
                                    tree, tria))
                        ikle4.extend(([i+j*dimx, i+1+j*dimx, i+1+(j+1)*dimx,
                                       i+(j+1)*dimx] \
                                             for j in range(dimy-1) \
                                           for i in range(dimx-1)))
                        mesh_x.extend(x)
                        mesh_z.extend(y)
                    ikle4 = np.asarray(ikle4)
                    ikle3 = split_quad_to_triangle(ikle4)
                    mesh_x = np.asarray(mesh_x)
                    mesh_z = np.asarray(mesh_z)

                # ~~> Loop on variables
                for _, vtype in zip(myvars, vtypes):

                    varsors = []
                    for ivar in range(len(myvars[0])):
                        varsors.append(np.ravel(data[ivar][0].T))

                    # ~~> Re-sampling
                    if what["sample"] != '':
                        data = np.zeros((len(myvars[0]), len(support2d)),
                                        dtype=np.float64)
                        for ivar in range(len(myvars[0])):
                            for ipt in range(len(support2d)):
                                l_n, b_n = support2d[ipt]
                                data[ivar][ipt] = 0.0
                                # /!\ node could be outside domain
                                for inod in range(len(b_n)):
                                    if l_n[inod] >= 0:
                                        data[ivar][ipt] += \
                                             b_n[inod]*varsors[ivar][l_n[inod]]
                        varsors = data

                    # ~~> Draw/Dump (multiple options possible)
                    if "wire" in vtype or "grid" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle4],
                                                               mesh_z[ikle4])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                    if "mesh" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle3],
                                                               mesh_z[ikle3])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                    if "map" in vtype or "label" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'map',
                                          'support':(mesh_x, mesh_z, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    if "arrow" in vtype or "vector" in vtype or \
                       "angle" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'vector',
                                          'support':(mesh_x, mesh_z, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]

            elif what['type'].split(':')[1] == 'p-section':

                # ~~> Extract data
                ikle3, mesh_x, mesh_y, _ = \
                        obj.cast_hmesh_at_levels(what['extract'])
                if obj.ndp2 == 4:
                    ikle3 = split_quad_to_triangle(ikle3)
                ikle4 = ikle3
                myvars, vtypes = what_vars_slf(what['vars'], obj.varnames)
                tree = obj.tree
                tria = obj.neighbours

                # ~~> Possible re-sampling
                support2d = []
                if what["sample"] != '':
                    supmesh_x = mesh_x
                    supmesh_y = mesh_y
                    mesh_x = []
                    mesh_y = []
                    ikle4 = []
                    support2d = []
                    grids, _ = what_sample(what["sample"],
                                           [(min(supmesh_x), min(supmesh_y)),
                                            (max(supmesh_x), max(supmesh_y))])
                    for dimx, dimy, x, y in grids:
                        for xyi in np.dstack((x, y))[0]:
                            support2d.append(xys_locate_mesh(\
                                    xyi, ikle3,
                                    supmesh_x, supmesh_y,
                                    tree, tria))
                        ikle4.extend(([i+j*dimx, i+1+j*dimx, i+1+(j+1)*dimx,
                                       i+(j+1)*dimx] \
                                          for j in range(dimy-1) \
                                         for i in range(dimx-1)))
                        mesh_x.extend(x)
                        mesh_y.extend(y)
                    ikle4 = np.asarray(ikle4)
                    ikle3 = split_quad_to_triangle(ikle4)
                    mesh_x = np.asarray(mesh_x)
                    mesh_y = np.asarray(mesh_y)

                # ~~> Loop on variables
                for var in what["vars"].split(';'):
                    vvar, vtype = var.split(':')

                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Extract variable data for only one time frame and one
                    # plane
                    varsors = obj.cast_hvalue_at_levels(vvar, what['time'],
                                                        what['extract'])
                    # ~~> Re-sampling
                    if support2d != []:
                        data = np.zeros((len(myvars[0]), len(support2d)),
                                        dtype=np.float64)
                        for ivar in range(len(myvars[0])):
                            for ipt in range(len(support2d)):
                                l_n, b_n = support2d[ipt]
                                data[ivar][ipt] = 0.0
                                # /!\ node could be outside domain
                                for inod in range(len(b_n)):
                                    if l_n[inod] >= 0:
                                        data[ivar][ipt] += \
                                             b_n[inod]*varsors[ivar][l_n[inod]]
                        varsors = data
                    # ~~> Draw/Dump (multiple options possible)
                    if "wire" in vtype or "grid" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle4],
                                                               mesh_y[ikle4])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                    if "mesh" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle3],
                                                               mesh_y[ikle3])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                    if "map" in vtype or "label" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'map',
                                          'support':(mesh_x, mesh_y, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    elif "arrow" in vtype or "vector" in vtype or \
                         "angle" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'vector',
                                          'support':(mesh_x, mesh_y, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    else:
                        print('... do not know how to draw this SELAFIN type: '\
                              + vtype)

            elif what['type'].split(':')[1] == '':

                # ~~> Extract data
                ikle3 = obj.ikle3
                mesh_x = obj.meshx
                mesh_y = obj.meshy
                if obj.ndp2 == 4:
                    ikle3 = split_quad_to_triangle(ikle3)
                ikle4 = ikle3
                myvars, vtypes = what_vars_slf(what['vars'], obj.varnames)
                tree = obj.tree
                tria = obj.neighbours

                # ~~> Possible re-sampling
                support2d = []
                if what["sample"] != '':
                    supmesh_x = mesh_x
                    supmesh_y = mesh_y
                    mesh_x = []
                    mesh_y = []
                    ikle4 = []
                    support2d = []
                    grids, _ = what_sample(what["sample"],
                                           [(min(supmesh_x), min(supmesh_y)),
                                            (max(supmesh_x), max(supmesh_y))])
                    for dimx, dimy, x, y in grids:
                        for xyi in np.dstack((x, y))[0]:
                            support2d.append(xys_locate_mesh(\
                                    xyi, ikle3,
                                    supmesh_x, supmesh_y,
                                    tree, tria))
                        ikle4.extend(([i+j*dimx, i+1+j*dimx, i+1+(j+1)*dimx,
                                       i+(j+1)*dimx] \
                                        for j in range(dimy-1) \
                                       for i in range(dimx-1)))
                        mesh_x.extend(x)
                        mesh_y.extend(y)
                    ikle4 = np.asarray(ikle4)
                    ikle3 = split_quad_to_triangle(ikle4)
                    mesh_x = np.asarray(mesh_x)
                    mesh_y = np.asarray(mesh_y)

                # ~~> Loop on variables
                var_names = []
                for var in what["vars"].split(';'):
                    vvar, vtype = var.split(':')
                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Extract variable data for only one plane
                    varsors = obj.cast_values(vvar, what['time'])
                    # Get value of the timestep
                    time = obj.tags['times'][what_time_slf(what['time'], obj.tags['times'])]
                    if vvar == '':
                        var_names = obj.varnames
                    else:
                        var_names.append(vvar)
                    # ~~> Re-sampling
                    if support2d != []:
                        data = np.zeros(varsors.shape, dtype=np.float64)
                        for itime in range(len(varsors)):
                            for ivar in range(len(myvars[0])):
                                for ipt in range(len(support2d)):
                                    l_n, b_n = support2d[ipt]
                                    # /!\ node could be outside domain
                                    for inod in range(len(b_n)):
                                        if l_n[inod] >= 0:
                                            data[itime][ivar][ipt] += \
                                               b_n[inod]*\
                                               varsors[itime][ivar][l_n[inod]]
                        varsors = data
                    # ~~> Draw/Dump (multiple options possible)
                    if "wire" in vtype or "grid" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle4],
                                                               mesh_y[ikle4])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                        # return np.dstack((mesh_x[ikle4],mesh_y[ikle4]))
                    if "mesh" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle3],
                                                               mesh_y[ikle3])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                    if "map" in vtype or "label" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'map',
                                          'support':(mesh_x, mesh_y, ikle3),
                                          'function':'none',
                                          'values':varsors,
                                          'names':myvars[1],
                                          'time':time})
                        return self.obdata[what["xref"]]
                    elif "arrow" in vtype or "vector" in vtype or \
                         "angle" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'vector',
                                          'support':(mesh_x, mesh_y, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    else:
                        print('... do not know how to draw this SELAFIN type: '\
                              + vtype)


            # ~~> unkonwn
            else:
                print('... do not know how to do this type of extraction: ' + \
                        what['type'].split(':')[1])

        # ~~> unkonwn
        else:
            print('... do not know how to extract from this format3: ' + typl)

    def get_3d(self, typl, what):
        obj = self.object[what['file']]

        if 'SELAFIN' in typl.upper() or \
           'WACLEO' in typl.upper() or \
           'slf' in typl.lower() or \
           'spe' in typl.lower() or \
           'med' in typl.lower():

            # TODO: range of plans and resample within a 2d and a 3d box.
            if what['type'].split(':')[1] == 'i-surface':

                # ~~> Extract data
                ikle2 = obj.ikle2
                ikle3 = obj.ikle3
                mesh_x = np.tile(obj.meshx, obj.nplan)
                mesh_y = np.tile(obj.meshy, obj.nplan)
                ikle4 = ikle3
                mesh_z = obj.cast_vmesh_at_levels(what["time"],
                                                  what["extract"])[3].ravel()
                tree = obj.tree
                tria = obj.neighbours

                # ~~> Possible re-sampling
                if what["sample"] != '':
                    support2d = []
                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Spatial 2D
                    supmesh_x = mesh_x[0:obj.npoin2]
                    supmesh_y = mesh_y[0:obj.npoin2]
                    mesh_x = []
                    mesh_y = []
                    ikle4 = []
                    support2d = []
                    grids, plans = what_sample(\
                            what["sample"],
                            [(min(supmesh_x), min(supmesh_y)),
                             (max(supmesh_x), max(supmesh_y)),
                             [0, obj.nplan]])
                    if len(plans) < 2:
                        raise TelemacException(\
                           '... you have to have more than one plan in {}'
                           ' for the smapling of a 3d volume'\
                           .format(what["sample"]))

                    for dimx, dimy, x, y in grids:
                        for xyi in np.dstack((x, y))[0]:
                            support2d.append(xys_locate_mesh(\
                                    xyi, ikle2,
                                    supmesh_x, supmesh_y,
                                    tree, tria))
                        ikle4.extend((\
                            [i+j*dimx+k*dimx*dimy,
                             i+1+j*dimx+k*dimx*dimy,
                             i+1+(j+1)*dimx+k*dimx*dimy,
                             i+(j+1)*dimx+k*dimx*dimy,
                             i+j*dimx+(k+1)*dimx*dimy,
                             i+1+j*dimx+(k+1)*dimx*dimy,
                             i+1+(j+1)*dimx+(k+1)*dimx*dimy,
                             i+(j+1)*dimx+(k+1)*dimx*dimy] \
                                for k in range(len(plans)-1) \
                                    for j in range(dimy-1) \
                                        for i in range(dimx-1)))
                        mesh_x.extend((x for k in range(len(plans))))
                        mesh_y.extend((y for k in range(len(plans))))
                    ikle4 = np.asarray(ikle4)
                    mesh_x = np.asarray(mesh_x)
                    mesh_y = np.asarray(mesh_y)
                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Vertical component
                    data = np.zeros((len(plans), len(support2d)),
                                    dtype=np.float64)
                    for ipt in range(len(support2d)):
                        l_n, b_n = support2d[ipt]
                        # /!\ node could be outside domain
                        for inod in range(len(b_n)):
                            for iplan in range(len(plans)):
                                if l_n[inod] >= 0:
                                    data[iplan][ipt] += \
                                            b_n[inod]*mesh_z[iplan][l_n[inod]]
                    mesh_z = data.ravel()

                # ~~> Loop on variables
                for var in what["vars"].split(';'):
                    vvar, vtype = var.split(':')

                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Extract variable data for only one time frame and one
                    # plane
                    varsors = obj.cast_values(vvar, what['time'])
                    # ~~> Draw/Dump (multiple options possible)
                    if "wire" in vtype or "grid" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle4],
                                                               mesh_y[ikle4],
                                                               mesh_z[ikle4])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                    if "mesh" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle4],
                                                               mesh_y[ikle4],
                                                               mesh_z[ikle4])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                    if "map" in vtype or "label" in vtype or "contour" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'map',
                                          'support':(mesh_x, mesh_y,
                                                     mesh_z, ikle4),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    elif "arrow" in vtype or "vector" in vtype or \
                          "angle" in vtype or "streamline" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'vector',
                                          'support':(mesh_x, mesh_y,
                                                     mesh_z, ikle4),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    else:
                        print('... do not know how to draw this SELAFIN type: '\
                              + vtype)

            elif what['type'].split(':')[1] == '3d-view':

                # ~~> Extract data
                ikle4, mesh_x, mesh_y, mesh_z, support3d = \
                          obj.cast_vmesh_at_polyline_plane(what["time"],
                                                           what["extract"])

                # split each quad into triangles
                ikle3 = split_quad_to_triangle(ikle4)
                myvars, vtypes = what_vars_slf(what['vars'], obj.varnames)
                time = what_time_slf(what['time'], obj.tags['times'])
                tree = get_kdtree(mesh_x, mesh_z, ikle3)
                tria = get_mpl_tri(mesh_x, mesh_z, ikle3)[0]
                data = get_value_polyline_slf(obj.file, obj.tags, time,
                                              support3d,
                                              obj.nvar, obj.npoin3,
                                              obj.nplan, myvars)

                # ~~> Possible sampling of the data
                # No resampling yet

                # ~~> Loop on variables
                for _, vtype in zip(myvars, vtypes):

                    varsors = []
                    for ivar in range(len(myvars[0])):
                        varsors.append(np.ravel(data[ivar][0].T))

                    # ~~> Re-sampling
                    # No resampling yet

                    # ~~> Draw/Dump (multiple options possible)
                    if "map" in vtype or "label" in vtype:
                        self.obdata[what["xref"]] =\
                                  Values({'type':what['type'],
                                          'unit':'map',
                                          'support':(mesh_x, mesh_y,
                                                     mesh_z, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    if "arrow" in vtype or "vector" in vtype or \
                       "angle" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'vector',
                                          'support':(mesh_x, mesh_y,
                                                     mesh_z, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]

            elif what['type'].split(':')[1] == 'p-section':

                # ~~> Extract data
                ikle3, mesh_x, mesh_y, _ = \
                        obj.cast_hmesh_at_levels(what['extract'])
                if obj.ndp2 == 4:
                    ikle3 = split_quad_to_triangle(ikle3)
                ikle4 = ikle3
                myvars, vtypes = what_vars_slf(what['vars'], obj.varnames)
                tree = obj.tree
                tria = obj.neighbours

                mesh_z = obj.cast_vmesh_at_levels(what["time"],
                                                  what["extract"])[3].ravel()

                # ~~> Possible re-sampling
                support2d = []
                if what["sample"] != '':
                    supmesh_x = mesh_x
                    supmesh_y = mesh_y
                    mesh_x = []
                    mesh_y = []
                    ikle4 = []
                    grids, _ = what_sample(what["sample"],
                                           [(min(supmesh_x), min(supmesh_y)),
                                            (max(supmesh_x), max(supmesh_y))])
                    for dimx, dimy, x, y in grids:
                        for xyi in np.dstack((x, y))[0]:
                            support2d.append(xys_locate_mesh(xyi, ikle3,\
                                supmesh_x, supmesh_y, tree, tria))
                        ikle4.extend(([i+j*dimx, i+1+j*dimx,
                                       i+1+(j+1)*dimx, i+(j+1)*dimx] \
                                       for j in range(dimy-1) \
                                       for i in range(dimx-1)))
                        mesh_x.extend(x)
                        mesh_y.extend(y)
                    ikle4 = np.asarray(ikle4)
                    ikle3 = split_quad_to_triangle(ikle4)
                    mesh_x = np.asarray(mesh_x)
                    mesh_y = np.asarray(mesh_y)

                # ~~> Loop on variables
                for var in what["vars"].split(';'):
                    vvar, vtype = var.split(':')

                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Extract variable data for only one time frame and one
                    # plane
                    varsors = obj.cast_hvalue_at_levels(vvar, what['time'],
                                                        what['extract'])
                    # ~~> Re-sampling
                    if support2d != []:
                        data = np.zeros((len(myvars[0]), len(support2d)),
                                        dtype=np.float64)
                        for ivar in range(len(myvars[0])):
                            for ipt in range(len(support2d)):
                                l_n, b_n = support2d[ipt]
                                data[ivar][ipt] = 0.0
                                # /!\ node could be outside domain
                                for inod in range(len(b_n)):
                                    if l_n[inod] >= 0:
                                        data[ivar][ipt] += \
                                              b_n[inod]*varsors[ivar][l_n[inod]]
                        varsors = data
                        supmesh_z = mesh_z
                        mesh_z = np.zeros(len(support2d), dtype=np.float64)
                        for ipt in range(len(support2d)):
                            l_n, b_n = support2d[ipt]
                            mesh_z[ipt] = 0.0
                            # /!\ node could be outside domain
                            for inod in range(len(b_n)):
                                if l_n[inod] >= 0:
                                    mesh_z[ipt] += \
                                            b_n[inod]*supmesh_z[l_n[inod]]
                    # ~~> Draw/Dump (multiple options possible)
                    if "map" in vtype or "label" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'map',
                                          'support':(mesh_x, mesh_y,
                                                     mesh_z, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    elif "arrow" in vtype or "vector" in vtype or \
                         "angle" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'vector',
                                          'support':(mesh_x, mesh_y,
                                                     mesh_z, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    else:
                        print('... do not know how to draw this SELAFIN type: '\
                              + vtype)

            elif what['type'].split(':')[1] == 'v-section':

                # ~~> Extract data
                ikle4, mesh_x, mesh_y, mesh_z, support3d = \
                          obj.cast_vmesh_at_polyline_plane(what["time"],
                                                           what["extract"])

                # split each quad into triangles
                ikle3 = split_quad_to_triangle(ikle4)
                myvars, vtypes = what_vars_slf(what['vars'], obj.varnames)
                time = what_time_slf(what['time'], obj.tags['times'])
                tree = get_kdtree(mesh_x, mesh_z, ikle3)
                tria = get_mpl_tri(mesh_x, mesh_z, ikle3)[0]
                data = get_value_polyline_slf(obj.file, obj.tags, time,\
                        support3d, obj.nvar, obj.npoin3, obj.nplan, myvars)

                # ~~> Possible sampling of the data
                # No resampling yet

                # ~~> Loop on variables
                for _, vtype in zip(myvars, vtypes):

                    varsors = []
                    for ivar in range(len(myvars[0])):
                        varsors.append(np.ravel(data[ivar][0].T))

                    # ~~> Re-sampling
                    # No resampling yet

                    # ~~> Draw/Dump (multiple options possible)
                    if "map" in vtype or "label" in vtype:
                        self.obdata[what["xref"]] =\
                                  Values({'type':what['type'],
                                          'unit':'map',
                                          'support':(mesh_x, mesh_y,
                                                     mesh_z, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    if "arrow" in vtype or "vector" in vtype or \
                       "angle" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'vector',
                                          'support':(mesh_x, mesh_y,
                                                     mesh_z, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]

            elif what['type'].split(':')[1] == '':

                # ~~> Extract data
                ikle3 = obj.ikle3
                mesh_x = obj.meshx
                mesh_y = obj.meshy
                if obj.ndp2 == 4:
                    ikle3 = split_quad_to_triangle(ikle3)
                ikle4 = ikle3
                myvars, vtypes = what_vars_slf(what['vars'], obj.varnames)
                tree = obj.tree
                tria = obj.neighbours

                # ~~> Possible re-sampling
                support2d = []

                # ~~> Loop on variables
                for var in what["vars"].split(';'):
                    vvar, vtype = var.split(':')

                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Extract variable data for only one plane
                    varsors = obj.cast_values(vvar, what['time'])
                    # ~~> Re-sampling
                    if support2d != []:
                        data = np.zeros(varsors.shape, dtype=np.float64)
                        for itime in range(len(varsors)):
                            for ivar in range(len(myvars[0])):
                                for ipt in range(len(support2d)):
                                    l_n, b_n = support2d[ipt]
                                    # /!\ node could be outside domain
                                    for inod in range(len(b_n)):
                                        if l_n[inod] >= 0:
                                            data[itime][ivar][ipt] += \
                                                b_n[inod]*\
                                                varsors[itime][ivar][l_n[inod]]
                        varsors = data
                    # ~~> Draw/Dump (multiple options possible)
                    if "wire" in vtype or "grid" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle4],
                                                               mesh_y[ikle4])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                    if "mesh" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'wire',
                                          'support':np.dstack((mesh_x[ikle3],
                                                               mesh_y[ikle3])),
                                          'function':'none',
                                          'values':[]})
                        return self.obdata[what["xref"]]
                        # return np.dstack((mesh_x[ikle3],mesh_y[ikle3]))
                    if "map" in vtype or "label" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'map',
                                          'support':(mesh_x, mesh_y, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    elif "arrow" in vtype or "vector" in vtype or \
                         "angle" in vtype:
                        self.obdata[what["xref"]] = \
                                  Values({'type':what['type'],
                                          'unit':'vector',
                                          'support':(mesh_x, mesh_y, ikle3),
                                          'function':'none',
                                          'values':varsors})
                        return self.obdata[what["xref"]]
                    else:
                        print('... do not know how to draw this SELAFIN type: '\
                              + vtype)

            # ~~> unknown
            else:
                print('... do not know how to do this type of extraction: ' + \
                        what['type'].split(':')[1])

        # ~~> unknown
        else:
            print('... do not know how to extract from this format4: ' + typl)

    def __del__(self):
        """ Delete all objects """
        for obj in list(self.object.keys()):
            del self.object[obj]
