
"""
Validation script for flume_trapeze
"""
import datetime
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from postel.deco_vnv import decoVNV, decoVNV_1d
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from data_manip.computation.datetimes import compute_datetimes

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['telemac2d', 'waqtel', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # flow down a flat flume (initial steady state)
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_trapeze_ini.cas')

        # thermal model with fazil ice growth (serial)
        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_trapeze.cas')

        # thermal model with fazil ice growth (parallel)
        cas = TelemacCas('t2d_trapeze.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_trapeze_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_trapeze.slf',
                            eps=[1.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_trapeze.slf',
                            eps=[1.E-3])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_2:T2DRES',
                            'vnv_3:T2DRES',
                            eps=[1.])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d, plot2d_scalar_filled_contour
        from postel.plot_vnv import vnv_plot1d, vnv_plot2d,\
            vnv_plot1d_history, vnv_plot1d_polylines

        geo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1 = TelemacFile(self.get_study_file('vnv_1:T2DRES'))
        res_vnv_2 = TelemacFile(self.get_study_file('vnv_2:T2DRES'))
        res_vnv_3 = TelemacFile(self.get_study_file('vnv_3:T2DRES'))

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        #Plotting mesh
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            plot_mesh=True,
            annotate_bnd=True,
            filled_contours=False,
            fig_size=(14, 2.5),
            fig_name='img/mesh',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/bottom',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        #======================================================================
        # FIRST OBSERVATION:
        #
        # Plotting temperature map
        vnv_plot2d(\
            'TEMPERATURE',
            res_vnv_2,
            record=-1,
            filled_contours=True,
            mask_tidal_flats=True,
            tidal_flats_threshold=0.01,
            plot_mesh=True,
            plot_only_dry_mesh=True,
            fig_size=(14, 2.5),
            fig_name='img/T-2d_scalarmap',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$T$ $(^\circ C)$')

        # Plotting frazil map
        vnv_plot2d(\
            'FRAZIL',
            res_vnv_2,
            record=-1,
            filled_contours=True,
            mask_tidal_flats=True,
            tidal_flats_threshold=0.01,
            plot_mesh=True,
            plot_only_dry_mesh=True,
            fig_size=(14, 2.5),
            fig_name='img/Cf-2d_scalarmap',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$C_f$ (volume fraction)')

        #----------------------------------------------------------------------
        # Plotting difference between scal and parallel run
        fig, ax = plt.subplots(1, 1, figsize=(14, 2.5))

        val_scal = res_vnv_2.get_data_value('FRAZIL', -1)
        val_par = res_vnv_3.get_data_value('FRAZIL', -1)
        diff = val_scal - val_par

        plot2d_scalar_filled_contour(fig, ax, res_vnv_2.tri, diff,
                                     data_name='$C_f$ (volume fraction)')

        print(" "*8+"~> Plotting img/Cf-dif")
        plt.savefig('img/Cf-dif')
        plt.clf()

        #----------------------------------------------------------------------
        # Plot timeseries on points:
        xpos = [100.*i for i in range(1, 4, 1)]

        for x in xpos:
            plot1d_history_TCf(\
                res=res_vnv_2,
                points=[[x, 18.]],
                xlim=[datetime.datetime(2016, 12, 2, 0, 0, 0),
                      datetime.datetime(2016, 12, 2, 4, 0, 0)],
                fig_name='img/timeseries_TCf_at_xy={}_75'.format(int(x)))

        #----------------------------------------------------------------------
        # Plotting profile
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_vnv_2,
            legend_labels='free surface',
            record=-1, 
            poly=[[0., 18.], [400., 18.]],
            poly_number=[50],
            fig_size=(10, 3),
            y_label='$z$ $(m)$',
            x_label='$x$ $(m)$',
            fig_name='img/profile_elevation',
            plot_bottom=True)

        tf = res_vnv_2.times[-1]
        times = [tf/2., tf]

        vnv_plot1d_polylines(\
            'TEMPERATURE',
            res_vnv_2,
            legend_labels='',
            time=times,
            poly=[[0., 18.], [400., 18.]],
            poly_number=[150],
            fig_size=(10, 4),
            y_label='$T$ $(\degree C)$',
            x_label='$x$ $(m)$',
            fig_name='img/profile_T',
            plot_bottom=False)

        vnv_plot1d_polylines(\
            'FRAZIL',
            res_vnv_2,
            legend_labels='',
            time=times,
            poly=[[0., 18.], [400., 18.]],
            poly_number=[150],
            fig_size=(10, 4),
            y_label='$C_f$ (volume fraction)',
            x_label='$x$ $(m)$',
            fig_name='img/profile_Cf',
            plot_bottom=False)

        #======================================================================
        # SENSIBILITY TO PHYSICAL AND NUMERICAL PARAMETERS:
        #
        #TODO


        # Closing files
        del geo
        del res_vnv_1
        del res_vnv_2
        del res_vnv_3

def plot1d_history_TCf(res, points, xlim=None, fig_name=''):
    """
    Plot 1d timeseries of temperature and frazil
    """
    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    fig, ax = plt.subplots(1, 1, figsize=(10, 4))
    axb = ax.twinx()
    # get data values
    T = res.get_timeseries_on_points('TEMPERATURE', points)
    C = res.get_timeseries_on_points('FRAZIL', points)
    # get datetimes
    datetimes = compute_datetimes(res.times,
                                  initial_date=res.datetime)
    # plot both T and Cf
    ax.plot(datetimes, T[0, :], label='Temperature', color='#002d74')
    axb.grid()
    axb.plot(datetimes, C[0, :], label='Frazil', color='#e85113')
    # set labels
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%H"))
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    ax.set_xlabel('t (h)')
    ax.set_ylabel('$T$ $(\degree C)$')
    axb.set_ylabel('$C_f$ $(volume fraction)$')
    ax.legend(loc=3)
    axb.legend(loc=4)
    # save figure:
    print(" "*8+"~> Plotting {}".format(fig_name))
    fig.savefig(fig_name)

