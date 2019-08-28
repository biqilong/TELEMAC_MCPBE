
"""
Validation script for wind_txy
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # wind varying in t and x - scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_wind_txy.cas')


        # wind varying in t and x - parallel mode
        cas = TelemacCas('t2d_wind_txy.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_wind_txy_par.cas',
                       cas=cas)

        del cas


        # binary wind varying in t and x - scalar mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_wind_txy_bin.cas')


        # binary wind varying in t and x - parallel mode
        cas = TelemacCas('t2d_wind_txy_bin.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_wind_txy_bin_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_wind_txy.slf',
                            eps=[1.e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_wind_txy.slf',
                            eps=[1.e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_wind_txy.slf',
                            eps=[1.e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_wind_txy.slf',
                            eps=[1.e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1.e-5])


    def _post(self):
        """
        Post-treatment processes
        """

