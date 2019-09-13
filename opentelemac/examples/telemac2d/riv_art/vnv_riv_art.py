
"""
Validation script for riv_art
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

        # riv_art scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_riv_art.cas')


        # riv_art parallel mode
        cas = TelemacCas('t2d_riv_art.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_riv_art_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_riv_art.slf',
                            eps=[1.E-7, 1.E-8, 1.E-7, 3.E-8, 1.E-8, 1.E-8, 1.E-8, 1.E-10, 1.E-10, 1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_riv_art.slf',
                            eps=[2.E-2, 9.E-3, 2.E-4, 2.E-4, 1.E-8, 1.E-8, 1.E-8, 1.E-10, 1.E-10, 1.E-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[2.E-2, 9.E-3, 2.E-4, 2.E-4, 1.E-8, 2.E-8, 2.E-8, 1.E-10, 1.E-10, 1.E-10])


    def _post(self):
        """
        Post-treatment processes
        """

