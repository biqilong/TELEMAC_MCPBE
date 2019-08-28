
"""
Validation script for friction
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
        self.rank = 0
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # friction scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_friction.cas')


        # friction parallel mode
        cas = TelemacCas('art_friction.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_friction_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_friction.slf',
                            eps=[1.e-7, 5.e-7, 1.e-7, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-7, 1.e-7, 1.e-8, 1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_friction.slf',
                            eps=[1.e-7, 5.e-7, 1.e-7, 1.e-8, 1.e-8, 0.001, 1.e-4, 0.2, 1.e-5, 1.e-5, 1.e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 0.001, 1.e-4, 0.2, 1.e-5, 1.e-5, 1.e-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_timeseries_on_polyline
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[0, 0.6], [19.9, 0.6]],
                fig_size=(10, 7),
                fig_name='img/Section')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   fig_size=(12, 4),
                   fig_name='img/Mesh')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 4),
                   fig_name='img/Wave_height')


        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_artres,
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 4),
                   fig_name='img/Free_Surface')

        # Closing files
        del res_vnv_1_artres
        del res_vnv_1_artgeo