
"""
Validation script for hippodrome
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
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # hippodrome scalar mode T2D+GAI
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_1NCOb.cas')


        # hippodrome parallel mode T2D+GAI
        cas = TelemacCas('t2d_1NCOb.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_1NCOb_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T2D+GAI
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_1NCOb_vf.cas')


        # hippodrome parallel mode T2D+GAI
        cas = TelemacCas('t2d_1NCOb_vf.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_1NCOb_vf_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T2D+GAI
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_4NCOb.cas')


        # hippodrome parallel mode T2D+GAI
        cas = TelemacCas('t2d_4NCOb.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_4NCOb_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T2D+GAI
        self.add_study('vnv_7',
                       'telemac2d',
                       't2d_4NCOb_vf.cas')


        # hippodrome parallel mode T2D+GAI
        cas = TelemacCas('t2d_4NCOb_vf.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_8',
                       'telemac2d',
                       't2d_4NCOb_vf_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T2D+GAI
        self.add_study('vnv_9',
                       'telemac2d',
                       't2d_4NCOb_strat_vf.cas')


        # hippodrome parallel mode T2D+GAI
        cas = TelemacCas('t2d_4NCOb_strat_vf.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_10',
                       'telemac2d',
                       't2d_4NCOb_strat_vf_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T2D+GAI
        self.add_study('vnv_13',
                       'telemac2d',
                       't2d_1COs.cas')


        # hippodrome parallel mode T2D+GAI
        cas = TelemacCas('t2d_1COs.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_14',
                       'telemac2d',
                       't2d_1COs_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_1NCOb.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_1NCOb.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_1NCOb.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_1NCOb.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:GAIRES',
                            'gai_ref_1NCOb_vf.slf',
                            eps=[1.e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:GAIRES',
                            'gai_ref_1NCOb_vf.slf',
                            eps=[1.e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:GAIRES',
                            'vnv_4:GAIRES',
                            eps=[1.e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_1NCOb_vf.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T2DRES',
                            'f2d_1NCOb_vf.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:GAIRES',
                            'gai_ref_4NCOb.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:GAIRES',
                            'gai_ref_4NCOb.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:GAIRES',
                            'vnv_6:GAIRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T2DRES',
                            'f2d_4NCOb.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T2DRES',
                            'f2d_4NCOb.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:GAIRES',
                            'gai_ref_4NCOb_vf.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:GAIRES',
                            'gai_ref_4NCOb_vf.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_7:GAIRES',
                            'vnv_8:GAIRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:T2DRES',
                            'f2d_4NCOb_vf.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:T2DRES',
                            'f2d_4NCOb_vf.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_7:T2DRES',
                            'vnv_8:T2DRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_9:GAIRES',
                            'gai_ref_4NCOb_strat_vf.slf',
                            eps=[2.e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_10:GAIRES',
                            'gai_ref_4NCOb_strat_vf.slf',
                            eps=[1.e-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_9:GAIRES',
                            'vnv_10:GAIRES',
                            eps=[1.e-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_9:T2DRES',
                            'f2d_4NCOb_strat_vf.slf',
                            eps=[2.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_10:T2DRES',
                            'f2d_4NCOb_strat_vf.slf',
                            eps=[1.e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_9:T2DRES',
                            'vnv_10:T2DRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_13:GAIRES',
                            'gai_ref_1COs.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_14:GAIRES',
                            'gai_ref_1COs.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_13:GAIRES',
                            'vnv_14:GAIRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_13:T2DRES',
                            'f2d_1COs.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_14:T2DRES',
                            'f2d_1COs.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_13:T2DRES',
                            'vnv_14:T2DRES',
                            eps=[1.e-3])


    def _post(self):
        """
        Post-treatment processes
        """

