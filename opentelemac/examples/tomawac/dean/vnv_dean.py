
"""
Validation script for dean
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

        # dean scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_dean.cas')


        # dean parallel mode
        cas = TelemacCas('tom_dean.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_dean_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_dean.slf',
                            eps=[0.0002, 0.0005, 0.0001, 0.0001, 1e-4, 1e-4, 0.003, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_dean.slf',
                            eps=[0.0002, 0.0005, 0.0001, 0.0001, 1e-4, 1e-4, 0.003, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_timeseries_on_polyline
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)

        # Plotting WAVE HEIGHT HM0 over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_wacres,
                'WAVE HEIGHT HM0',
                poly=[[0, 6], [300, 6]],
                records=[-1],
                fig_size=(12, 7),
                fig_name='img/section1d')

        # Plotting BOTTOM at -1
        vnv_plot2d('BOTTOM',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   cmap_name='YlGnBu',
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        # Plotting PRIVATE 1 at -1
        vnv_plot2d('PRIVATE 1',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   cmap_name='Greens',
                   fig_size=(12, 7),
                   fig_name='img/kmoyen')

        # Closing files
        del res_vnv_1_wacres
