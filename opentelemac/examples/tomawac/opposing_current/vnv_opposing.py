
"""
Class for validation of opposing
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
        self.tags = ['tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # opposing scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_opposing.cas')


        # opposing scalar mode
        cas = TelemacCas('tom_opposing.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_opposing_par.cas',
                       cas=cas)

        del cas


        # opposing opt1 parallel mode
        self.add_study('vnv_3',
                       'tomawac',
                       'tom_opposing_opt1.cas')


        # opposing opt1 parallel mode
        cas = TelemacCas('tom_opposing_opt1.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'tomawac',
                       'tom_opposing_opt1_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_opposing_cur.slf',
                            eps=[1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_opposing_cur.slf',
                            eps=[1e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:WACRES',
                            'fom_opposing_cur_opt1.slf',
                            eps=[1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:WACRES',
                            'fom_opposing_cur_opt1.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        from postel.plot1d import plot1d
        import matplotlib.pyplot as plt
                # Getting files
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo)

        res1 = TelemacFile(self.get_study_file('vnv_1:WACRES'))
        res2 = TelemacFile(self.get_study_file('vnv_3:WACRES'))

        poly = [[0, 1], [8, 1]]

        _, abs_curv, values1 = \
           res1.get_timeseries_on_polyline('WAVE HEIGHT HM0', poly)

        _, _, values2 = \
           res2.get_timeseries_on_polyline('WAVE HEIGHT HM0', poly)

        fig, ax = plt.subplots(figsize=(12, 7))

        plot1d(ax, abs_curv, values1[:, -1],
               plot_label='Limit Method',
               color='Aqua')
        plot1d(ax, abs_curv, values2[:, -1],
               plot_label='WB Method',
               color='red')

        ax.legend()

        fig_name = 'img/section1d'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)

        # Plotting BOTTOM over polyline
        vnv_plot1d_polylines(\
                'BOTTOM',
                res_vnv_1_wacgeo,
                poly=[[0, 1], [8, 1]],
                record=0,
                fig_size=(12, 7),
                fig_name='img/section1d2')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_wacgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        # Closing files
        res_vnv_1_wacgeo.close()
        res1.close()
        res2.close()
