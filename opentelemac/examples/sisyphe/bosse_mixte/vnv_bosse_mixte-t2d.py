
"""
Validation script for bosse_mixte-t2d
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
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # bosse mixte T2D+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_bosse_mixte-t2d.cas')


        # bosse mixte T2D+SIS parallel mode
        cas = TelemacCas('t2d_bosse_mixte-t2d.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_bosse_mixte-t2d_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_bosse_mixte-t2d.slf',
                            eps=[1e-9, 1.e-9, 1.e-9, 1.e-9, 1.e-9, 1.e-9, 1.e-7, 1.e-9, 1.e-9, 1.e-9, 1.e-9, 1.e-9, 1.e-9, 1.e-9, 1.e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_bosse_mixte-t2d.slf',
                            eps=[1.e-6, 1.e-5, 1.e-6, 1.e-6, 1.e-8, 1.e-9, 1.e-5, 1.e-9, 1.e-8, 1.e-7, 1.e-7, 1.e-8, 1.e-9, 1.e-9, 1.e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[1.e-6, 1.e-5, 1.e-6, 1.e-6, 1.e-8, 1.e-9, 1.e-5, 1.e-9, 1.e-8, 1.e-7, 1.e-7, 1.e-8, 1.e-9, 1.e-9, 1.e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_bosse_mixte-t2d.slf',
                            eps=[1.e-9, 1.e-5, 1.e-9, 1.e-9, 1.e-9, 1.e-7, 1.e-7, 1.e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_bosse_mixte-t2d.slf',
                            eps=[1.e-6, 1.e-4, 1.e-6, 1.e-6, 1.e-8, 1.e-6, 1.e-6, 1.e-7])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-6, 1.e-5, 1.e-6, 1.e-6, 1.e-8, 1.e-6, 1.e-6, 1.e-7])


    def _post(self):
        """
        Post-treatment processes
        """

