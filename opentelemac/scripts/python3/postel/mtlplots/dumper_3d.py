r"""@author TELEMAC-MASCARET Consortium

      @brief
"""
from __future__ import print_function
from data_manip.formats.selafin import Selafin
from postel.caster import Caster
from utils.exceptions import TelemacException

class Dumper3D(Caster):
    """
    @brief Dump three dimension information
    @param Caster:
    """
    def __init__(self, caster, dump):
        Caster.__init__(self, {'object':caster.object, 'obdata':caster.obdata})
        self.obtype = dump['saveas'] # the type of file, 'slf' most probably
        self.oudata = None # the loaded SELAFIN object itself, most probably

    def add(self, typl, what):
        """
        @brief
        @param typl:
        @param what:
        """
        Caster.add(self, typl, what)

        # ~~> output from for 3D file
        if self.obtype == 'slf':
            if not self.oudata:
                # TODO: Missing file name in SELAFIN
                self.oudata = Selafin('')
        # ~~> unkonwn
        else:
            raise TelemacException(\
                    '... do not know how to write to this format: '
                    '{}'.format(self.obtype))

    def save(self, file_name):
        """
        @brief
        @param file_name:
        """
        self.oudata.put_content(file_name)
