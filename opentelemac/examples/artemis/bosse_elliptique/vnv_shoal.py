
"""
Validation script for shoal
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
        self.rank = 1
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # elliptic shoal scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_shoal.cas')


        # elliptic shoal parallel mode
        cas = TelemacCas('art_shoal.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_shoal_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_shoal.slf',
                            eps=[1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_shoal.slf',
                            eps=[2.e-4, 10., 1.e-4, 1.e-8, 1.e-8, 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[2.e-4, 10., 1.e-4, 1.e-8, 1.e-8, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_timeseries_on_polyline
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[10, 18], [20, 18]],
                fig_size=(10, 7),
                fig_name='img/Section1')

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[10, 17], [20, 17]],
                fig_size=(10, 7),
                fig_name='img/Section2')

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[10, 16], [20, 16]],
                fig_size=(10, 7),
                fig_name='img/Section3')

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[10, 15], [0, 15]],
                fig_size=(10, 7),
                fig_name='img/Section4')

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[10, 14], [20, 14]],
                fig_size=(10, 7),
                fig_name='img/Section5')

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[14.75, 8], [14.75, 20]],
                fig_size=(10, 7),
                fig_name='img/Section6')

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[15.75, 8], [15.75, 20]],
                fig_size=(10, 7),
                fig_name='img/Section7')

        # Plotting WAVE HEIGHT over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_artres,
                'WAVE HEIGHT',
                poly=[[16.75, 8], [16.75, 20]],
                fig_size=(10, 7),
                fig_name='img/Section8')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   fig_size=(10, 10),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 10),
                   fig_name='img/Bathy')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 10),
                   fig_name='img/WaveHeight')

        # Closing files
        del res_vnv_1_artgeo
        del res_vnv_1_artres
