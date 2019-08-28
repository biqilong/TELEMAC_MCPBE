
"""
Validation script for cons_mixte-t2d
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 4
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # concentration mixte T2D+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_cons_mixte-t2d.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_cons_mixte-t2d.slf',
                            eps=[1.e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_cons_mixte-t2d.slf',
                            eps=[1.e-7])


    def _post(self):
        """
        Post-treatment processes
        """
