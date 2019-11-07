
"""
Validation script for shoal
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
        self.rank = 3
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # shoal scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_shoal.cas')


        # shoal parallel mode
        cas = TelemacCas('t2d_shoal.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_shoal_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_shoal.slf',
                            eps=[2.E-1,2.E-1,4.E-3,3.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_shoal.slf',
                            eps=[2.E-1,2.E-1,4.E-3,3.E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[2.E-3,4.E-3,8.E-4,1.E-4])


    def _post(self):
        """
        Post-treatment processes
        """

