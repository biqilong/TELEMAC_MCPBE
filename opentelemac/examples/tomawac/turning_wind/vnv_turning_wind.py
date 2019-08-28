
"""
Validation script for turning_wind
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
        self.tags = ['tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # turning_wind scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_turning_wind.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_TW.slf',
                            eps=[1e-9, 361, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACLEO',
                            'spe_ref.slf',
                            eps=[1e-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_history, vnv_plot2d
                # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo)

        #Plotting WAVE HEIGHT HM0 on [10, 10] over records range(0, ntimestep)
        vnv_plot1d_history(\
                'WAVE HEIGHT HM0',
                res_vnv_1_wacres,
                'WAVE HEIGHT HM0',
                points=[[10, 10]],
                points_labels=['point (10, 10)'],
                fig_size=(12, 7),
                fig_name='img/timeseries')


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

        # Closing files
        del res_vnv_1_wacres
        del res_vnv_1_wacgeo
