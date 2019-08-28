
"""
Validation script for flam
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

        # flam scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_flamanville.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_flamanville.9112.slf',
                            eps=[])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_mesh2d, plot_var
        # Getting files
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)

        #Plotting mesh
        plot_mesh2d(res_vnv_1_artgeo,
                    fig_size=(12, 8),
                    fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        plot_var(res_vnv_1_artres,
                 'BOTTOM',
                 record=0,
                 fig_size=(12, 8),
                 fig_name='img/Bathy')


        # Plotting WAVE HEIGHT at 0
        plot_var(res_vnv_1_artres,
                 'WAVE HEIGHT',
                 record=0,
                 fig_size=(12, 8),
                 fig_name='img/WaveHeight')


        # Plotting QB at 0
        plot_var(res_vnv_1_artres,
                 'QB',
                 record=0,
                 fig_size=(12, 8),
                 fig_name='img/Breaking')
