
"""
Validation script for waq3d_aed2
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
        self.rank = 0
        self.tags = ['telemac3d', 'waqtel', 'aed']

    def _pre(self):
        """
        Defining the studies
        """

        # water quality in 3D - based on AED2
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_waq3d_aed2.cas')


        # water quality in 3D parallel - based on AED2
        cas = TelemacCas('t3d_waq3d_aed2.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_waq3d_aed2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_waq3d_aed2.slf',
                            eps=[1.e-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_waq3d_aed2.slf',
                            eps=[1e-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1e-15])


    def _post(self):
        """
        Post-treatment processes
        """

