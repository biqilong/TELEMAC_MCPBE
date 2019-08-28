
"""
Validation script for chwac1
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
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # chwac1 scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_chwac.cas')


        # chwac1 scalar mode
        cas = TelemacCas('art_chwac.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_chwac_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_chwac.12689.slf',
                            eps=[360])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_chwac.12689.slf',
                            eps=[360])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[360])


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
                poly=[[-6, 0.4], [14, 0.4]],
                fig_size=(10, 7),
                fig_name='img/Section')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   fig_size=(10, 3),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 3),
                   fig_name='img/Bathy')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 3),
                   fig_name='img/WaveHeight')

        # Closing files
        del res_vnv_1_artgeo
        del res_vnv_1_artres
