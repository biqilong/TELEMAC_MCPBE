
"""
Validation script for gouttedo
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from postel.plot_actions import *
from postel.plot_vnv import *

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of Thompson
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # shoal scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_angularspred.cas')


        # shoal parallel mode
        cas = TelemacCas('tom_angularspred.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_angularspred_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison between sequential reference
        self.check_epsilons('fom_ang.slf',
                            'vnv_2:WACRES',
                            eps=[1e-8])

        # Comparison between sequential and spectre
        self.check_epsilons('fom_ang.spe',
                            'vnv_2:WACLEO',
                            eps=[1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-8])


    def _post(self):
        """
        Post-treatment processes
        """
                # Getting files
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo)
        vnv_1_wacleo = self.get_study_file('vnv_1:WACLEO')
        spe = TelemacFile(vnv_1_wacleo)
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_wacgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   fig_size=(12, 7),
                   fig_name='img/hm0')


        points = spe.get_list_spectrum_points()

        record = -1

        # Plotting F01 PT2D000101 at -1
        fig, axe = plt.subplots(figsize=(7, 7))

        # Adding plot of spectrum for each point
        for point in points:
            # Getting list of frequencies and spectrum value
            freq, spectrum = spe.get_spectrum(point, record)
            # Plotting it
            plot1d(axe, freq, spectrum,
                   plot_label='point {:06d}'.format(point),
                   x_label='Frequencies',
                   y_label='Spectrum')

        axe.legend()

        fig_name="img/spectrum"
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
