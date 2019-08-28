
"""
Validation script for kochin_2
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

        # AUTOMATIC TETAP CALCULATION FOR RADIAL WAVE POTENTIAL
        self.add_study('vnv_1',
                       'artemis',
                       'art_kochin_2.cas')


        # AUTOMATIC TETAP CALCULATION FOR RADIAL WAVE POTENTIAL
        cas = TelemacCas('art_kochin_2.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'artemis',
                       'art_kochin_2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_kochin_2.slf',
                            eps=[1.e-5, 1.e-4, 1.e-8, 1.e-8, 1.e-8, 1.e-4, 1.e-4, 0.001, 1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_kochin_2.slf',
                            eps=[1.e-5, 1.e-4, 1.e-8, 1.e-8, 1.e-8, 1.e-4, 1.e-4, 4.0, 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-5, 1.e-4, 1.e-8, 1.e-8, 1.e-8, 1.e-4, 1.e-4, 4.0, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_mesh2d, plot_var
        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)

        #Plotting mesh
        plot_mesh2d(res_vnv_1_artgeo,
                    fig_size=(8, 8),
                    fig_name='img/Mesh')


        # Plotting WAVE HEIGHT at 0
        plot_var(res_vnv_1_artres,
                 'WAVE HEIGHT',
                 record=0,
                 fig_size=(8, 8),
                 fig_name='img/WaveHeight')


        # Plotting REAL POTENTIAL at 0
        plot_var(res_vnv_1_artres,
                 'REAL POTENTIAL',
                 record=0,
                 fig_size=(8, 8),
                 fig_name='img/RealPotential')


        # Plotting IMAG POTENTIAL at 0
        plot_var(res_vnv_1_artres,
                 'IMAG POTENTIAL',
                 record=0,
                 fig_size=(8, 8),
                 fig_name='img/ImagPotential')

