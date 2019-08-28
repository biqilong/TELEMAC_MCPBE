r"""@author TELEMAC-MASCARET Consortium

    @brief
"""
from __future__ import print_function
from postel.caster import Caster
from data_manip.extraction.parser_csv import CSV
from utils.exceptions import TelemacException

class Dumper1D(Caster):
    """
    @brief Dump one dimension information
    @param Caster:
    """
    def __init__(self, caster, dump):
        Caster.__init__(self,
                        {'object': caster.object,
                         'obdata': caster.obdata
                        })
        self.obtype = dump['saveas']
        self.oudata = None

    def add(self, typl, what):
        """
        @brief
        @param typl:
        @param what:
        """
        Caster.add(self, typl, what)

        # ~~> only csv is recognised fr now
        if self.obtype != 'csv':
            raise TelemacException(\
                    '... do not know how to write to '
                    'this format: {}'.format(self.obtype))


        # ~~> initialisation
        if not self.oudata:
            self.oudata = CSV()

        # ~~> write-up
        cast = self.get(typl, what)

        # ~~> 1D time history from 2D or 3D results
        if what['type'].split(':')[1] == 'history' or 'sortie' in typl.lower():
            self.oudata.add_columns((cast.unit, cast.support),
                                    (cast.function, cast.values))

        # ~~> 1D vertical cross section from 2D or 3D results
        elif what['type'].split(':')[1] == 'v-section' or 'csv' in typl.lower():
            try:  # if instance unit exist
                self.oudata.add_columns((cast.unit, cast.support),
                                        (cast.function, cast.values))
            except Exception:
                dim = len(cast.values.shape)
                fct = ['v-section']
                if dim > 1:
                    fct.append(['VAR' + str(ivar) \
                                     for ivar in range(cast.values.shape[0])])
                if dim > 2:
                    fct.append(
                        [str(itim) for itim in range(cast.values.shape[1])]
                    )
                if dim > 3:
                    fct.append(
                        [str(ilay) for ilay in range(cast.values.shape[2])]
                    )
                self.oudata.add_columns(('unit', cast.support),
                                        (fct, cast.values))

        # ~~> unkonwn
        else:
            raise TelemacException(\
                    '... do not know how to extract '
                    'from this format: {}'.format(typl))

    def save(self, file_name):
        """
        @brief
        @param file_name:
        """
        self.oudata.put_file_content(file_name)
