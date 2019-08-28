
"""
Validation script for gouttedo
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from postel.plot_actions import *
from postel.plot_vnv import *

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of Thompson
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """
        
        # WC2003 T2D+GAI scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_WC2003_BOMC4_5CL.cas')


        # WC2003 T2D+GAI parallel mode
        cas = TelemacCas('t2d_WC2003_BOMC4_5CL.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_WC2003_BOMC4_5CL_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """
        
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_WC2003.slf',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_WC2003.slf',
                            eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_WC2003.slf',
                            eps=[1.e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_WC2003.slf',
                            eps=[1.e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-6])


    def _post(self):
        """
        Post-treatment processes
        """
        