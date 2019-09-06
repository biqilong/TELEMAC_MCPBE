
"""
Validation script for rouse-t3d
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
        self.tags = ['telemac3d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # rouse-t3d scalar mode T3D+GAI
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_rouse-t3d.cas')


        # rouse-t3d parallel mode T3D+GAI
        cas = TelemacCas('t3d_rouse-t3d.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 2)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_rouse-t3d_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_rouse-t3d.slf',
                            eps=[1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_rouse-t3d.slf',
                            eps=[1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_rouse-t3d.slf',
                            eps=[1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_rouse-t3d.slf',
                            eps=[1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d, plot_vertical_slice
        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)


        points = [2000., 0.]

        timeseries_z = res_vnv_1_t3dres.get_data_timeseries_on_vertical_segment(\
                points, 'ELEVATION Z')
        timeseries_vel = res_vnv_1_t3dres.get_data_timeseries_on_vertical_segment(\
                points, 'VELOCITY U')

        fig, ax = plt.subplots(figsize=(12, 7))

        # Plotting vertical section
        plot1d(ax, timeseries_vel[-1, :], timeseries_z[-1, :],
               x_label='Velocity U [m/s]',
               y_label='Elevation Z [m]',
               plot_label=\
         'Velocity profile on the vertical at {} at the last time step'\
         .format(points))

        ax.legend()

        fig_name = 'img/velocityU_profile'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.clf()

        # Plotting vertical section
        timeseries_conc = res_vnv_1_t3dres.get_data_timeseries_on_vertical_segment(\
                points, 'COH SEDIMENT1')

        fig, ax = plt.subplots(figsize=(12, 7))

        # Plotting 3d points
        plot1d(ax, timeseries_conc[-1, :], timeseries_z[-1, :],
               x_label='Suspended concentration [g/L]',
               y_label='Elevation Z [m]',
               plot_label=\
         'Velocity profile on the vertical at {} at the last time step'\
         .format(points))

        ax.legend()

        fig_name = 'img/suspconc_profile'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.clf()

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        # Plotting COH SEDIMENT1 at -1
        vnv_plot2d('COH SEDIMENT1',
                   res_vnv_1_t3dhyd,
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/sediment_plan_view')


        # TODO: change cmap
        # Plotting vertical split
        plot_vertical_slice(res_vnv_1_t3dres,
                            'COH SEDIMENT1',
                            poly=[[-2500, 0], [2500, 0]],
                            record=-1,
                            fig_size=(12, 7),
                            fig_name='img/sediment_section')

        # TODO: change cmap
        # Plotting vertical split
        plot_vertical_slice(res_vnv_1_t3dres,
                            'VELOCITY U',
                            poly=[[-2500, 0], [2500, 0]],
                            record=-1,
                            fig_size=(12, 7),
                            fig_name='img/velocityU_section')
        # Closing files
        del res_vnv_1_t3dres
        del res_vnv_1_t3dhyd
