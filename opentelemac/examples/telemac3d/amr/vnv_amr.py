
"""
Validation script for amr
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # amr scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_amr.cas')


        # amr parallel mode
        cas = TelemacCas('t3d_amr.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_amr_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_amr.slf',
                            eps=[1.8, 0.2, 0.02, 0.5, 0.04])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_amr.slf',
                            eps=[2.3, 0.2, 0.02, 0.6, 0.03])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[2.3, 0.2, 0.02, 0.6, 0.05])


    def _post(self):
        """
        Post-treatment processes
        """

