r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief Class to cast SERAFIN format
"""
from __future__ import print_function
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import numpy as np
# ~~> dependencies towards other pytel/modules
from utils.parser_strings import parse_array_frame, parse_array_point
from utils.exceptions import TelemacException
from data_manip.formats.selafin import Selafin
from data_manip.extraction.parser_selafin import get_value_history_slf, \
                                  get_value_polyline_slf, \
                                  subset_variables_slf, \
                                  get_value_polyplan_slf
from pretel.meshes import xys_locate_mesh, slice_mesh
from postel.mycast import what_time_slf

class CastSelafin(Selafin):

    def cast_history_at_points(self, what_vars, what_time, what_points):

        # ~~> Extract data
        # what_vars: list of pairs variables:support delimited by ';'
        # (/!\ support is ignored)
        vrs = subset_variables_slf(what_vars, self.varnames)
        # what_time: list of frames or (times,) delimited by ';'
        itime = parse_array_frame(what_time, len(self.tags['cores']))
        # what_points: could be list delimited by ';', of:
        #    + points (x;y),
        #    + 2D points (x;y) bundled within (x;y)#n or
        #      (x;y)@d#n, delimited by ';'
        #      where n is a plan number and d is depth from that plane
        #      (or the surface by default)
        support2d = []
        zps = []
        p_ap = parse_array_point(what_points, self.nplan)
        for xyi, zpi in p_ap:
            if isinstance(xyi, tuple):
                support2d.append(xys_locate_mesh(xyi, self.ikle2, self.meshx,
                                                 self.meshy, self.tree,
                                                 self.neighbours))
            else:
                support2d.append(xyi)
            zps.append(zpi)
        support3d = list(zip(support2d, zps))
        # - support2d[i][0] is either the node or the triplet of nodes for each
        #   element including (x,y)
        # - support2d[i][1] is the plan or depth definition
        data = get_value_history_slf(self.file, self.tags, itime, support3d,
                                     self.nvar, self.npoin3, self.nplan, vrs)

        # ~~> Draw/Dump data
        return ('Time (s)', self.tags['times'][itime]), \
            [('', [n.replace(' ', '_').replace(',', ';') for n in vrs[1]], \
            [(str(n[0])+':'+str(m)).replace(' ', '').replace(',', ';') \
            for n in p_ap for m in n[1]], \
            data)]

    def cast_profile_at_polyline(self, what_vars, what_time, what_points):

        # ~~> Extract data
        # what['vars']: list of pairs variables:support2d delimited by ';'
        vrs = subset_variables_slf(what_vars, self.varnames)
        # what['time']: list of frames or (times,) delimited by ';'
        itime = parse_array_frame(what_time, len(self.tags['cores']))
        # what['extract']: could be list delimited by ';', of:
        #    + points (x;y),
        #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by
        #      ';'
        #      where n is a plan number and d is depth from that plane (or the
        #      surface by default)
        xyo = []
        zpo = []
        for xyi, zpi in parse_array_point(what_points, self.nplan):
            if type(xyi) == type(()):
                xyo.append(xyi)
            else:
                xyo.append((self.meshx[xyi], self.meshy[xyi]))
            for plan in zpi: # /!\ common definition of plans
                if plan not in zpo:
                    zpo.append(plan) # /!\ only allowing plans for now
        xys, support2d = slice_mesh(xyo, self.ikle2, self.meshx, self.meshy,
                                    self.tree)
        # - support2d[i][0] is either the douplets of nodes for each edges
        #   crossing with the polyline
        # - support2d[i][1] is the plan or depth definition
        # common vertical definition to all points
        support3d = [(s2d, zpo) for s2d in support2d]
        data = get_value_polyline_slf(self.file, self.tags, itime, support3d,
                                      self.nvar, self.npoin3, self.nplan, vrs)
        # Distance d-axis
        distot = 0.0
        dist = [distot]
        for x_y in range(len(xys)-1):
            distot += np.sqrt(np.power(xys[x_y+1][0]-xys[x_y][0], 2) +\
                              np.power(xys[x_y+1][1]-xys[x_y][1], 2))
            dist.append(distot)
        # ~~> Draw/Dump data
        return ('Distance (m)', dist), [('v-section', vrs[1],
                                         self.tags['times'][itime], zpo, data)]

    def cast_vmesh_at_polyline(self, what_time, what_points):

        # what_points: could be list delimited by ';', of:
        #    + points (x;y),
        #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by
        #      ';'
        #      where n is a plan number and d is depth from that plane (or the
        #      surface by default)
        xyo = []
        zpo = []
        for xyi, zpi in parse_array_point(what_points, self.nplan):
            if xyi == []:
                raise TelemacException(\
                    '... I could not find anything to extract in "{}" '
                    'as support for '
                    'the cross section.'.format(what_points["extract"].strip()))
            if type(xyi) == type(()):
                xyo.append(xyi)
            else:
                xyo.append((self.meshx[xyi], self.meshy[xyi]))
            for plan in zpi:   # /!\ common deinition of plans
                # /!\ only allowing plans for now
                if plan not in zpo:
                    zpo.append(plan)

        # ~~> Extract horizontal cross meshx
        xys, support2d = slice_mesh(xyo, self.ikle2, self.meshx, self.meshy,
                                    self.tree)
        support3d = []
        for s2d in support2d:
            # common vertical definition to all points
            support3d.append((s2d, zpo))
        # Distance d-axis
        distot = 0.0
        dist = [distot]
        for x_y in range(len(xys)-1):
            distot += np.sqrt(np.power(xys[x_y+1][0]-xys[x_y][0], 2) + \
                np.power(xys[x_y+1][1]-xys[x_y][1], 2))
            dist.append(distot)
        meshx = np.repeat(dist, len(zpo))

        # ~~>  Extract meshz for more than one time frame
        varz = subset_variables_slf('z', self.varnames)
        itime = what_time_slf(what_time, self.tags['cores'])
        meshz = np.ravel(get_value_polyline_slf(\
                     self.file, self.tags, itime, support3d,
                     self.nvar, self.npoin3, self.nplan,
                     varz)[0][0].T)

        # ~~>  Connect with ikle, keeping quads
        ikle = []
        for j in range(len(dist)-1):
            for i in range(len(zpo)-1):
                ikle.append([i+j*len(zpo), i+(j+1)*len(zpo), i+1+(j+1)*len(zpo),
                             i+1+j*len(zpo)])
        ikle = np.array(ikle)

        return ikle, meshx, meshz, support3d

    def cast_vmesh_at_polyline_plane(self, what_time, what_points):

        # what_points: could be list delimited by ';', of:
        #    + points (x;y),
        #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by
        #      ';'
        #      where n is a plan number and d is depth from that plane (or the
        #      surface by default)
        xyo = []
        zpo = []
        for xyi, zpi in parse_array_point(what_points, self.nplan):
            if xyi == []:
                raise TelemacException(\
                     '... I could not find anything to extract in "{}" as'
                     'support for the cross section.'\
                     .format(what_points["extract"].strip()))
            if type(xyi) == type(()):
                xyo.append(xyi)
            else:
                xyo.append((self.meshx[xyi], self.meshy[xyi]))
            for plan in zpi:             # /!\ common deinition of plans
                if plan not in zpo:
                    zpo.append(plan)     # /!\ only allowing plans for now

        # ~~> Extract horizontal cross meshx
        xys, support2d = slice_mesh(xyo, self.ikle2, self.meshx, self.meshy,
                                    self.tree)
        support3d = []
        for s2d in support2d:
            # common vertical definition to all points
            support3d.append((s2d, zpo))

        # Distance d-axis
        distot = 0.0
        dist = [distot]
        for x_y in range(len(xys)-1):
            distot += np.sqrt(np.power(xys[x_y+1][0]-xys[x_y][0], 2) + \
                np.power(xys[x_y+1][1]-xys[x_y][1], 2))
            dist.append(distot)

        newx = []
        newy = []
        for x_y in range(len(xys)):
            newx.append(xys[x_y][0])
            newy.append(xys[x_y][1])

        meshx = np.repeat(newx, len(zpo))
        meshy = np.repeat(newy, len(zpo))

        # ~~>  Extract meshz for more than one time frame
        varz = subset_variables_slf('z', self.varnames)
        itime = what_time_slf(what_time, self.tags['cores'])
        meshz = np.ravel(get_value_polyline_slf(\
                    self.file, self.tags, itime, support3d,
                    self.nvar, self.npoin3, self.nplan,
                    varz)[0][0].T)

        # ~~>  Connect with ikle, keeping quads
        ikle = []
        for j in range(len(dist)-1):
            for i in range(len(zpo)-1):
                ikle.append([i+j*len(zpo), i+(j+1)*len(zpo), i+1+(j+1)*len(zpo),
                             i+1+j*len(zpo)])
        ikle = np.array(ikle)

        return ikle, meshx, meshy, meshz, support3d

    def cast_hmesh_at_levels(self, what_points):

        # what_points: could be list delimited by ';', of:
        #    + empty spatial location [],
        #    + bundled within []#n or []@d#n, delimited by ';'
        #      where n is a plan number and d is depth from that plane (or the
        #      surface by default)
        zpo = []
        for _, zpi in parse_array_point(what_points, self.nplan):
            for plan in zpi:  # /!\ common definition of plans
                # /!\ only allowing plans for now
                if plan not in zpo:
                    zpo.append(plan)

        return self.ikle2, self.meshx, self.meshy, zpo

    def cast_vmesh_at_levels(self, what_time, what_points):

        itime = what_time_slf(what_time, self.tags['cores'])
        zpo = self.cast_hmesh_at_levels(what_points)[3]
        # what_vars: is set here for Z
        vrs = []
        for vname in self.varnames:
            if 'ELEVATION' in vname:
                vrs = subset_variables_slf('ELEVATION', self.varnames)
            if 'COTE Z' in vname:
                vrs = subset_variables_slf('COTE Z', self.varnames)
            if 'WATER DEPTH' in vname:
                vrs = subset_variables_slf('WATER DEPTH', self.varnames)
            if 'HAUTEUR D\'EAU' in vname:
                vrs = subset_variables_slf('HAUTEUR D\'EAU', self.varnames)
            if 'FREE SURFACE' in vname:
                vrs = subset_variables_slf('FREE SURFACE', self.varnames)
            if 'SURFACE LIBRE' in vname:
                vrs = subset_variables_slf('SURFACE LIBRE', self.varnames)
        if vrs == []:
            raise TelemacException(\
               '... Could not find [\'ELEVATION\'] or [\'COTE Z\'] in {} \n   '
               '+> Your file may not be a 3D file (?)'\
               .format(repr(self.varnames)))
        return self.ikle3, self.meshx, self.meshy,\
                 get_value_polyplan_slf(self.file, self.tags, itime, zpo,
                                        self.nvar, self.npoin3, self.nplan,
                                        vrs)[0][0]

    def cast_hvalue_at_levels(self, what_vars, what_time, what_points):

        itime = what_time_slf(what_time, self.tags['cores'])
        # what_vars: list of pairs variables:support2d delimited by ';'
        vrs = subset_variables_slf(what_vars, self.varnames)
        # what_points: could be list delimited by ';', of:
        #    + points (x;y),
        #    + 2D points (x;y) bundled within (x;y)#n or (x;y)@d#n, delimited by
        #    ';'
        #      where n is a plan number and d is depth from that plane (or the
        #      surface by default)
        xyo = []
        zpo = []
        for xyi, zpi in parse_array_point(what_points, self.nplan):
            if xyi == [] or type(xyi) == type(()):
                xyo.append(xyi)
            else:
                xyo.append((self.meshx[xyi], self.meshy[xyi]))
            for plan in zpi:             # /!\ common deinition of plans
                if plan not in zpo:
                    zpo.append(plan)     # /!\ only allowing plans for now
        if len(zpo) != 1:
            print('... the vertical definition should only have one plan in '\
                'this case. It is: '+repr(what_points)+\
                '. I will assume you wish to plot the higher plane.')
        # could be more than one variables including v,
        # but only one time frame itime and one plan
        data = get_value_polyplan_slf(self.file, self.tags, itime, zpo,
                                      self.nvar, self.npoin3, self.nplan, vrs)
        varsors = []
        for ivar in range(len(data)):
            varsors.append(data[ivar][0][0]) # TODO: give me more time

        return varsors

    def cast_values(self, what_vars, what_time):

        _, fsize = self.file['float']
        # /!\ For the moment, only one frame at a time
        times = what_time_slf(what_time, self.tags['cores'])
        # what_vars: list of pairs variables:support2d delimited by ';'
        vars_indexes, _ = subset_variables_slf(what_vars, self.varnames)
        if fsize == 4:
            varsors = np.zeros((len(times), len(vars_indexes), self.npoin3),
                               dtype=np.float32)
        else:
            varsors = np.zeros((len(times), len(vars_indexes), self.npoin3),
                               dtype=np.float64)
        for itime in range(len(times)):
            varsors[itime] = self.get_variables_at(times[itime], vars_indexes)
        return varsors
