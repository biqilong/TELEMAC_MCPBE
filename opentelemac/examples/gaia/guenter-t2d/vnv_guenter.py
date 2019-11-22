
"""
Validation script for guenter
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
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # Evolution of a bump in 2D T2D+GAI scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_guenter.cas')


        # Evolution of a bump in 2D T2D+GAI parallel mode
        cas = TelemacCas('t2d_guenter.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_guenter_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:GAIRES',
                            'gai_ref_guenter.slf',
                            eps=[1.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:GAIRES',
                            'gai_ref_guenter.slf',
                            eps=[1.2E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:GAIRES',
                            'vnv_par:GAIRES',
                            eps=[1.1E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_guenter.slf',
                            eps=[1.3E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_guenter.slf',
                            eps=[1.4E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[1.4E-3])


    def _post(self):
        """
        Post-treatment processes
        """

