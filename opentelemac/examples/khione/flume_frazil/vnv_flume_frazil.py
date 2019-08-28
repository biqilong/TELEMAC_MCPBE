
"""
Validation script for flume_frazil
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

        # flow down a flat flume (initial steady state)
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_frazil_ini.cas')


        # thermal model with fazil ice growth (serial)
        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_frazil.cas')


        # thermal model with fazil ice growth (parallel)
        cas = TelemacCas('t2d_frazil.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_frazil_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_longflume.slf',
                            eps=[1.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_longflume.slf',
                            eps=[1.E-1])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_2:T2DRES',
                            'vnv_3:T2DRES',
                            eps=[1.E-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d, plot2d_scalar_filled_contour
        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt

        geo = TelemacFile(self.get_study_file('vnv_1:T2DGEO'))
        res_vnv_1 = TelemacFile(self.get_study_file('vnv_1:T2DRES'))
        res_vnv_2 = TelemacFile(self.get_study_file('vnv_2:T2DRES'))
        res_vnv_3 = TelemacFile(self.get_study_file('vnv_3:T2DRES'))

        #Plotting mesh
        vnv_plot2d('',
                   geo,
                   plot_mesh=True,
                   fig_size=(18, 2),
                   fig_name='img/mesh',
                   x_label='Length [m]',
                   y_label='Width [m]')

        # Plotting bottom
        vnv_plot2d('BOTTOM',
                   geo,
                   record=0,
                   filled_contours=True,
                   fig_size=(18, 2),
                   fig_name='img/bottom',
                   x_label='Length [m]',
                   y_label='Width [m]',
                   cbar_label='Bed level [m]')

        # Plotting frazil map
        vnv_plot2d('FRAZIL (CONC)',
                   res_vnv_2,
                   record=-1,
                   filled_contours=True,
                   fig_size=(18, 2),
                   fig_name='img/frazil-map',
                   x_label='Length [m]',
                   y_label='Width [m]',
                   cbar_label='Frazil concentration [si]')

        # Plotting difference between scal and parallel run
        fig, ax = plt.subplots(1, 1, figsize=(18, 2))

        val_scal = res_vnv_2.get_data_value('FRAZIL (CONC)', -1)
        val_par = res_vnv_3.get_data_value('FRAZIL (CONC)', -1)
        diff = val_scal - val_par

        plot2d_scalar_filled_contour(fig, ax, res_vnv_2.tri, diff,
                                     data_name='Frazil concentration [si]')

        print(" "*8+"~> Plotting img/frazil-dif")
        plt.savefig('img/frazil-dif')
        plt.clf()


        # Plotting profile
        fig, ax = plt.subplots(1, 1, figsize=(12, 2))

        poly = [[0., 75.], [10000., 75.]]
        poly_number = geo.discretize_polyline(poly)

        _, abs_curv, bottoms = \
                geo.get_timeseries_on_polyline(poly, 'BOTTOM', poly_number)

        _, abs_curv2, surfaces = \
                res_vnv_1.get_timeseries_on_polyline(poly, 'FREE SURFACE',
                                                     poly_number)

        plot1d(ax, abs_curv, bottoms[:, 0], color='b',
               plot_label='bottom')
        plot1d(ax, abs_curv2, surfaces[:, -1], color='r',
               plot_label='surface')

        ax.legend()

        print(" "*8+"~> Plotting img/l-section")
        plt.savefig('img/l-section')
        plt.clf()

        # Plotting frazil profile
        fig, ax = plt.subplots(1, 1, figsize=(12, 2))

        poly = [[0., 75.], [10000., 75.]]
        poly_number = geo.discretize_polyline(poly)

        _, abs_curv, temps = \
             res_vnv_2.get_timeseries_on_polyline(poly, 'TEMPERATURE',
                                                  poly_number)

        _, abs_curv2, frazils = \
             res_vnv_2.get_timeseries_on_polyline(poly, 'FRAZIL (CONC)',
                                                  poly_number)

        plot1d(ax, abs_curv, temps[:, 0], color='b',
               plot_label='temperature')
        plot1d(ax, abs_curv2, frazils[:, 0], color='k',
               plot_label='frazil t=0')
        plot1d(ax, abs_curv2, frazils[:, -1], color='r',
               plot_label='frazil t=-1')

        ax.legend()

        print(" "*8+"~> Plotting img/frazil-profile")
        plt.savefig('img/frazil-profile')
        plt.clf()
