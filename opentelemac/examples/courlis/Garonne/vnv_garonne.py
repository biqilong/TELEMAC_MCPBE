
"""
Validation script for Garonne
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of Garonne
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['mascaret', 'courlis']

    def _pre(self):
        """
        Defining the studies
        """

        # Garonne Canal transcritical kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'hydro_Torr.xcas')

    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """
