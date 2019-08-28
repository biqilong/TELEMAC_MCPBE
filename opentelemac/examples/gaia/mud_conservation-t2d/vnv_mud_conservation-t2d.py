
"""
Validation script for mud_conservation-t2d
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
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # Mud conservation T2D+GAI scalar mode
        self.add_study('vnv_mud_cons_scalar',
                       'telemac2d',
                       't2d_mud_cons.cas')


        # Mud conservation T2D+GAI scalar mode
        cas = TelemacCas('t2d_mud_cons.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_mud_cons_para',
                       'telemac2d',
                       't2d_mud_cons_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_scalar:GAIRES',
                            'gai_ref_mud_cons-t2d.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_para:GAIRES',
                            'gai_ref_mud_cons-t2d.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_para:GAIRES',
                            'vnv_mud_cons_scalar:GAIRES',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_scalar:T2DRES',
                            'f2d_mud_cons-t2d.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_para:T2DRES',
                            'f2d_mud_cons-t2d.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_para:T2DRES',
                            'vnv_mud_cons_scalar:T2DRES',
                            eps=[1e-3])


    def _post(self):
        """
        Post-treatment processes
        """

