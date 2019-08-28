
"""
Validation script for deferl_bj78
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
        self.tags = ['tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # deferl_bj78 scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_bj15.cas')


        # deferl_bj78 parallel mode
        cas = TelemacCas('tom_bj15.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_bj15_par.cas',
                       cas=cas)

        del cas


        # deferl3 scalar mode
        self.add_study('vnv_1bis',
                       'tomawac',
                       'tom_deferl2.cas')


        # deferl3 scalar mode
        self.add_study('vnv_3',
                       'tomawac',
                       'tom_deferl3.cas')


        # deferl4 scalar mode
        self.add_study('vnv_4',
                       'tomawac',
                       'tom_deferl4.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_bj15.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1bis:WACRES',
                            'fom_deferl2.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_bj15.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:WACRES',
                            'fom_deferl3.slf',
                            eps=[1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:WACRES',
                            'fom_deferl4.slf',
                            eps=[1e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_timeseries_on_polyline
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_4_wacres = self.get_study_file('vnv_4:WACRES')
        res_vnv_4_wacres = TelemacFile(vnv_4_wacres)
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)

        # Plotting FOND over polyline over records 0
        plot_timeseries_on_polyline(\
                res_vnv_1_wacgeo,
                'FOND',
                poly=[[0, 6], [25, 6]],
                records=[-1],
                fig_size=(12, 7),
                fig_name='img/section1d')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_wacgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/results')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/results3')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_4_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/results4')


        # Plotting BETA at -1
        vnv_plot2d('BETA',
                   res_vnv_4_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/beta4')

        # Closing files
        del res_vnv_1_wacres
        del res_vnv_4_wacres
        del res_vnv_1_wacgeo
        del res_vnv_3_wacres