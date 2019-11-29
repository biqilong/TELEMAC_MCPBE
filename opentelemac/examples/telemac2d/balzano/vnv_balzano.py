
"""
Validation script for balzano
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
        self.rank = 2
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # balzano scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_balzano.cas')


        # balzano parallel mode
        cas = TelemacCas('t2d_balzano.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_balzano_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_balzano.slf',
                            eps=[2., 2., 1.1, 0.1, 1.e-8, 4., 2., 1.e-8])

    def _post(self):
        """
        Post-treatment processes
        """
