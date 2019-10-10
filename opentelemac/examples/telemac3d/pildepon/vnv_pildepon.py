
"""
Validation script for pildepon
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # pildepon scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_piledepon.cas')


        # pildepon parallel mode
        cas = TelemacCas('t3d_piledepon.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_piledepon_par.cas',
                       cas=cas)

        del cas


        # pildepon scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_piledepon-nonhydro.cas')


        # pildepon parallel mode
        cas = TelemacCas('t3d_piledepon-nonhydro.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_piledepon-nonhydro_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_piledepon.slf',
                            eps=[1.E-1, 2.E0, 1.5E0, 1.E0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_piledepon.slf',
                            eps=[1.E-1, 2.E0, 1.5E0, 1.E0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-1, 2.E0, 1.5E0, 1.E0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_piledepon-nonhydro.slf',
                            eps=[0.04, 1.5, 1., 0.4, 0.2, 0.3, 0.1, 0.2, 0.2, 0.1, 0.1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_piledepon-nonhydro.slf',
                            eps=[0.2, 1.7, 1.1, 0.4, 0.3, 0.6, 0.3, 0.3, 0.4, 0.3, 0.3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[0.2, 1.7, 1.1, 0.4, 0.3, 0.6, 0.3, 0.3, 0.4, 0.3, 0.3])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_t3dgeo = self.get_study_file('vnv_1:T3DGEO')
        res_vnv_1_t3dgeo = TelemacFile(vnv_1_t3dgeo)

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        res_vnv_1_t3dgeo.close()
