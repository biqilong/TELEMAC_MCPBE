
"""
Validation script for bottom_bc
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

        # Bottom inlet scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_bottom_inlet.cas')


        # Bottom inlet parallel mode
        cas = TelemacCas('t3d_bottom_inlet.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_bottom_inlet_par.cas',
                       cas=cas)

        del cas


        # Bottom source scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_bottom_source.cas')


        # Bottom source parallel mode
        cas = TelemacCas('t3d_bottom_source.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_bottom_source_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_bottom_inlet.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_bottom_inlet.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_bottom_source.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_bottom_source.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """

