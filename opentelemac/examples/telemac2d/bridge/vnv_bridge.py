
"""
Validation script for bridge
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
        self.rank = 2
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # bridge scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_bridge.cas')


        # bridge parallel mode
        cas = TelemacCas('t2d_bridge.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_bridge_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_bridge.slf',
                            eps=[1.e-7, 1.e-7, 1.e-7, 1.e-7, 2.e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_bridge.slf',
                            eps=[1.e-7, 1.e-7, 1.e-7, 1.e-7, 2.e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-7, 1.e-7, 1.e-7, 1.e-7, 2.e-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        # Plotting FREE SURFACE in vertical slice
        vnv_plot1d_polylines('FREE SURFACE',
                             res_vnv_1_t2dres,
                             'Elevation',
                             fig_size=(12, 5),
                             poly=[[0., 125.], [1000., 125.]],
                             poly_number=[500],
                             record=[0, 1],
                             fig_name='img/FreeSurface_Section',
                             plot_bottom=True)

        # Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(10, 5),
                   fig_name='img/Mesh')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   fig_size=(10, 5),
                   fig_name='img/Bathy',
                   filled_contours=True)

        # Plotting FREE SURFACE at 0
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=0,
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface_t0',
                   filled_contours=True)

        # Plotting FREE SURFACE at 1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=1,
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface_t1',
                   filled_contours=True)

        # Plotting VELOCITY V at 1
        vnv_plot2d('VELOCITY V',
                   res_vnv_1_t2dres,
                   record=1,
                   fig_size=(10, 5),
                   fig_name='img/VelocityV_t1',
                   filled_contours=True)