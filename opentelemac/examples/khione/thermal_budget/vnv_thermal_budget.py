
"""
Validation script for thermal_budget
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
        self.tags = ['telemac2d', 'waqtel', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # thermal budget under icy conditions (serial)
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_thermic.cas')


        # thermal budget under icy conditions (parallel)
        cas = TelemacCas('t2d_thermic.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_thermic_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_thermic.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_thermic.slf',
                            eps=[])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d
        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        import numpy as np
        # Getting files
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)

        res_vnv_1 = TelemacFile(self.get_study_file('vnv_1:T2DRES'))

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(7, 7),
                   fig_name='img/mesh')

        exp = np.loadtxt('data/ref_crissp2d.prn')

        values = res_vnv_1.get_timeseries_on_nodes([95], 'TEMPERATURE')

        fig, ax = plt.subplots(1, 1, figsize=(12, 3))

        plot1d(ax, exp[:, 0], exp[:, 1], color='r', marker='o',
               plot_label='CRISSP2D', lw=2)

        plot1d(ax, res_vnv_1.times, values[0, :], color='g',
               plot_label='KHIONE', lw=2)

        ax.set_xlabel('Time [s]')
        ax.set_ylabel('Temperature [oC]')
        ax.legend()

        print(" "*8+"~> Plotting img/temperature")
        plt.savefig('img/temperature')

        # Closing files
        del res_vnv_1_t2dgeo
        del res_vnv_1
