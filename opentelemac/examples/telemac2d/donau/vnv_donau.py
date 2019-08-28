
"""
Validation script for donau
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

        # donau scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_donau.cas')


        # donau parallel mode
        cas = TelemacCas('t2d_donau.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_donau_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_donau.slf',
                            eps=[2.E-5, 3.E-5, 2.E-5, 4.E-5, 1.E-8, 3.E-5, 2.E-5, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_donau.slf',
                            eps=[2.E-5, 2.E-5, 2.E-5, 4.E-5, 1.E-8, 3.E-5, 2.E-5, 1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[3.E-5, 4.E-5, 3.E-5, 4.E-5, 1.E-8, 6.E-5, 4.E-5, 1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """

