
"""
Validation script for cavity
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # cavity scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_cavity.cas')


        # cavity parallel mode
        cas = TelemacCas('t2d_cavity.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_cavity_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_cavity.slf',
                            eps=[1e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_cavity.slf',
                            eps=[1e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-2])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_timeseries_on_polyline
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        # Plotting BOTTOM over polyline over records range(0, -1)
        plot_timeseries_on_polyline(\
                res_vnv_1_t2dres,
                'BOTTOM',
                poly=[[15, 0], [15, 6]],
                records=range(0, res_vnv_1_t2dres.ntimestep),
                fig_size=(12, 5),
                fig_name='img/BottomProfile')

        # Plotting WATER DEPTH at -1
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(20, 7),
                   fig_name='img/Water_depth')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(20, 7),
                   fig_name='img/Velocity')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   fig_size=(6, 5),
                   xlim=[11, 17],
                   ylim=[3, 6],
                   record=0,
                   filled_contours=True,
                   fig_name='img/VelocityROIt0',
                   streamlines=True)

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   fig_size=(6, 5),
                   xlim=[11, 17],
                   ylim=[3, 6],
                   record=-1,
                   filled_contours=True,
                   fig_name='img/VelocityROItf',
                   streamlines=True)

        # Closing files
        del res_vnv_1_t2dres
