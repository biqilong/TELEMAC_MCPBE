r"""@author TELEMAC-MASCARET Consortium
    @brief
"""
from data_manip.data_manip.formats.selafins import Selafins
class DumpSelafin(Selafins):
    """
    @brief
    """

    def add(self, slf):
        if self.slf is None:
            self.slf = slf
        self.slfs.append(slf)
        self.suite = self.is_suite()  # True if there is only one slf
        self.merge = self.is_merge()  # True if there is only one slf
