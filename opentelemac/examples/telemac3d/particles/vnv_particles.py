
"""
Validation script for particles
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # particles scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_particles.cas')


        # particles parallel mode
        cas = TelemacCas('t3d_particles.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_particles_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_particles.slf',
                            eps=[1.E-4, 2.E-2, 1.E-2, 1.E-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_particles.slf',
                            eps=[1.E-4, 2.E-2, 1.E-2, 1.E-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-4, 2.E-2, 1.E-2, 1.E-2])


    def _post(self):
        """
        Post-treatment processes
        """
