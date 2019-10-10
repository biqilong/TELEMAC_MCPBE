
"""
Validation script for wesel
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

        # wesel scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_wesel.cas')


        # wesel scalar mode
        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_wesel_pos.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_wesel.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_wesel_pos.slf',
                            eps=[])


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
        vnv_2_t2dres = self.get_study_file('vnv_2:T2DRES')
        res_vnv_2_t2dres = TelemacFile(vnv_2_t2dres)

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
                   fig_size=(10, 8),
                   fig_name='img/Bathy')


        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurface_tf')


        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/Velocity_tf')


        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_2_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurface_tf_pos')


        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_2_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/Velocity_tf_pos')

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
        res_vnv_2_t2dres.close()
