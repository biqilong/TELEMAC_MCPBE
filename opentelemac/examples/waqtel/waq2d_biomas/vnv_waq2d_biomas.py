
"""
Validation script for waq2d_biomas
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
        self.rank = 1
        self.tags = ['telemac2d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # water quality- dissolved biomas process
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_waq_biomas.cas')


        # water quality- dissolved biomas process
        cas = TelemacCas('t2d_waq_biomas.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_waq_biomas_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_waq_biomas.slf',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_waq_biomas.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-15])


    def _post(self):
        """
        Post-treatment processes
        """
