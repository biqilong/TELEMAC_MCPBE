r"""@author TELEMAC-MASCARET Consortium
    @brief
"""
from __future__ import print_function
from postel.caster import Caster
from postel.mycast import what_vars_slf
from data_manip.formats.selafin import Selafin
import numpy as np
from utils.exceptions import TelemacException

class Dumper2D(Caster):
    """
    @brief
    """
    def __init__(self, caster, dump):
        Caster.__init__(self, {'object': caster.object,
                               'obdata': caster.obdata})
        self.obtype = dump['saveas']  # the type of file, 'slf' most probably
        self.oudata = None  # the loaded Selafin object itself, most probably

    def add(self, typl, what):
        Caster.add(self, typl, what)

        # ~~> output from for 2D file
        if self.obtype == 'slf':
            # self.obdump.add(self.object[what['file']])
            cast = self.get(typl, what)
            support = cast.support
            values = cast.values
            if len(support) != 3:
                raise TelemacException(\
                        '... not enough information to save as 2d variable')
            obj = self.object[what['file']]
            # ~~ Selafin header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if not self.oudata:
                self.oudata = Selafin('')
                # create the out header
                # TODO: pass it on from what and deco
                self.oudata.title = ''
                self.oudata.nbv1 = 0
                self.oudata.varnames = []
                self.oudata.varunits = []
                self.oudata.iparam = obj.iparam
                self.oudata.iparam[6] = 1  # 3d being forced to 2d
                self.oudata.ndp2 = len(support[2][0])
                if np.all([obj.ikle2, support[2]]):
                    self.oudata.ikle2 = support[3]
                    self.oudata.ipob2 = np.zeros(len(support[0]), dtype=np.int)
                    self.oudata.meshx = support[0]
                    self.oudata.meshy = support[1]
                else:
                    self.oudata.ikle2 = obj.ikle2
                    self.oudata.ipob2 = obj.ipob2  # ipobo missing from support
                    self.oudata.meshx = obj.meshx
                    self.oudata.meshy = obj.meshy
                self.oudata.nelem2 = len(self.oudata.ikle2)
                self.oudata.npoin2 = len(self.oudata.meshx)
                self.oudata.nelem3 = self.oudata.nelem2
                self.oudata.npoin3 = self.oudata.npoin2
                self.oudata.ndp3 = self.oudata.ndp2
                self.oudata.nplan = 1
            vrs, _ = what_vars_slf(what['vars'], obj.varnames)
            self.oudata.nbv1 = self.oudata.nbv1 + len(vrs[0])
            self.oudata.nbv2 = 0
            self.oudata.nvar = self.oudata.nbv1 + self.oudata.nbv2
            self.oudata.cldnames = []
            self.oudata.cldunits = []
            self.oudata.varindex = range(self.oudata.nvar)
            for ivar, ival in zip(vrs[0], range(len(vrs[0]))):
                self.oudata.varnames.append(obj.varnames[ivar])
                self.oudata.varunits.append(obj.varunits[ivar])
                self.obdata.update({obj.varnames[ivar]: [values[ival]]})
            if max(self.oudata.iparam[9], obj.iparam[9]) > 0:
                if self.oudata.datetime != obj.datetime:
                    self.oudata.iparam[9] = 0
            if self.oudata.nelem2 != obj.nelem2 or \
                self.oudata.npoin2 != obj.npoin2:
                raise TelemacException(\
                        '... mismatch between the 2D sizes of layers of a '
                        'same save2d object ')
            self.oudata.ikle3 = self.oudata.ikle2
            self.oudata.ipob3 = self.oudata.ipob2

        # ~~> unkonwn
        else:
            raise TelemacException(\
                    '... do not know how to write to this format: '
                    '{}'.format(self.obtype))

    def save(self, file_name):
        """
        @brief Save into a file
        @param file_name: file to save the dump into
        """
        # gather common information for the final header
        if self.obtype == 'slf':
            self.oudata.fole = {}
            self.oudata.fole.update({'name':file_name})
            # "<" means little-endian, ">" means big-endian
            self.oudata.fole.update({'endian':">"})
            self.oudata.fole.update({'float':('f', 4)})
            self.oudata.fole.update({'hook': open(file_name, 'wb')})
            self.oudata.append_header_slf()
            # TODO: recover track of time
            self.oudata.append_core_time_slf(0.0)
            for ivar in self.oudata.varnames:
                self.oudata.append_core_vars_slf(self.obdata[ivar])
            self.oudata.fole['hook'].close()

        # ~~> unkonwn
        else:
            raise TelemacException(\
                    '... do not know how to write to this format: '
                    '{}'.format(self.obtype))
