
"""
Validation script for kd09
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
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # kd09 telemac2d-sisyphe coupled scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_kd09_mak.cas')


        # kd09 telemac2d-sisyphe coupled scalar mode
        cas = TelemacCas('t2d_kd09_mak.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_kd09_mak_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_kd09.slf',
                            eps=[3.E-2, 3.E-3, 7.E-4, 3.E-3, 3.E-3, 5.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_kd09.slf',
                            eps=[3.E-2, 3.E-3, 7.E-4, 3.E-3, 3.E-3, 5.E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[3.E-2, 3.E-3, 7.E-4, 3.E-3, 3.E-3, 5.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_kd09.slf',
                            eps=[3.E-2, 3.E-3, 7.E-4, 4.E-3, 3.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_kd09.slf',
                            eps=[3.E-2, 3.E-3, 7.E-4, 4.E-3, 3.E-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[3.E-2, 3.E-3, 7.E-4, 4.E-3, 3.E-3])


    def _post(self):
        """
        Post-treatment processes
        """

