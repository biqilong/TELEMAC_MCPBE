
"""
Validation script for pluie
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

        # pluie scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_pluie.cas')


        # pluie parallel mode
        cas = TelemacCas('t2d_pluie.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_pluie_par.cas',
                       cas=cas)
        del cas

        # rain with runoff model scalar mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_pluie_cn.cas')


        # rain with runoff model parallel mode
        cas = TelemacCas('t2d_pluie_cn.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_pluie_cn_par.cas',
                       cas=cas)
        del cas

        # rain with hyetopgraph model scalar mode
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_pluie_cn_geo_hyetograph.cas')


        # rain with hyetopgraph model parallel mode
        cas = TelemacCas('t2d_pluie_cn_geo_hyetograph.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_pluie_cn_geo_hyetograph_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_rain_uniform.slf',
                            eps=[1.E-15])


    def _post(self):
        """
        Post-treatment processes
        """

