
"""
Validation script for delwaq
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # delwaq scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_delwaq.cas')


        # delwaq parallel mode
        cas = TelemacCas('t2d_delwaq.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_delwaq_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_delwaq.slf',
                            eps=[0.1, 0.07, 0.007, 0.007, 1.E-9, 0.04])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_delwaq.slf',
                            eps=[0.25, 0.12, 0.012, 0.012, 1.E-9, 0.08])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[0.25, 0.12, 0.012, 0.012, 1.E-9, 0.08])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(10, 8),
                   fig_name='img/Mesh')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   fig_size=(10, 6),
                   fig_name='img/Bathy')

        # Plotting VELOCITY at 1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=1,
                   filled_contours=True,
                   fig_size=(10, 6),
                   fig_name='img/Velocity_map_t1')

        # Plotting VELOCITY at 15
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=15,
                   filled_contours=True,
                   fig_size=(10, 6),
                   fig_name='img/Velocity_map_t15')

        # Plotting VECTORS at 15
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=15,
                   plot_mesh=True,
                   cbar_priority='vector',
                   colored_vectors=True,
                   vectors_scale=35,
                   grid_resolution=[30, 30],
                   fig_size=(10, 6),
                   fig_name='img/Velocity_arrows')

        # Closing files
        del res_vnv_1_t2dgeo
        del res_vnv_1_t2dres
