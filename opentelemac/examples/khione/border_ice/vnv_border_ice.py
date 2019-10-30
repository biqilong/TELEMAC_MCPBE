
"""
Validation script for border_ice
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
        self.rank = 2
        self.tags = ['telemac2d', 'waqtel', 'khione']

    def _pre(self):
        """
        Defining the studies
        """
        # serial
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_border-ice.cas')

        # parallel
        cas = TelemacCas('t2d_border-ice.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_border-ice_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_border-ice.slf',
                            eps=[1.E-3])
        self.check_epsilons('vnv_seq:ICERES',
                            'fce_border-ice.slf',
                            eps=[1.E-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_border-ice.slf',
                            eps=[1.])
        self.check_epsilons('vnv_par:ICERES',
                            'fce_border-ice.slf',
                            eps=[1000.])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[1.])
        self.check_epsilons('vnv_seq:ICERES',
                            'vnv_par:ICERES',
                            eps=[1000.])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d, plot2d_scalar_filled_contour
        from postel.plot_vnv import vnv_plot1d, vnv_plot2d,\
            vnv_plot1d_history, vnv_plot1d_polylines

        geo, _ = self.get_study_res('vnv_seq:T2DGEO', load_bnd=True)
        #res_ini = TelemacFile(self.get_study_file('vnv_ini:T2DRES'))
        res_seq = TelemacFile(self.get_study_file('vnv_seq:T2DRES'))
        res_par = TelemacFile(self.get_study_file('vnv_par:T2DRES'))
        res_ice_seq = TelemacFile(self.get_study_file('vnv_seq:ICERES'))
        res_ice_par = TelemacFile(self.get_study_file('vnv_par:ICERES'))

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
            fig_size=(14, 3.2),
            fig_name='img/mesh',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            filled_contours=True,
            fig_size=(15, 3.),
            fig_name='img/bottom',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        #======================================================================
        # FIRST OBSERVATION:
        #
        fig_size=(15, 3)
        times = [0., 3600., 7200.]
        hours = [0, 1, 2]

        for idx, time in enumerate(times):
            # Plotting velocity
            vnv_plot2d(\
                'VELOCITY',
                res_seq,
                time=time,
                filled_contours=True,
                vectors=True,
                vectors_scale=15, vectors_normalize=False,
                grid_resolution=[100, 20],
                fig_size=fig_size,
                fig_name='img/U-2d_scalarmap_{}h'.format(hours[idx]),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$|U|$ $(m/s)$')

            # Plotting temperature
            vnv_plot2d(\
                'TEMPERATURE',
                res_seq,
                time=time,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/T-2d_scalarmap_{}h'.format(hours[idx]),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$T$ $(^\circ C)$')

            # Plotting frazil
            vnv_plot2d(\
                'FRAZIL',
                res_seq,
                var_factor=1000.0,
                time=time,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/Cf-2d_scalarmap_{}h'.format(hours[idx]),
                cmap_name='Blues_r',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$10^3$ $C_f$ (volume fraction)')

            # Plotting ice cover thickness
            vnv_plot2d(\
                'TOTAL ICE THICK.',
                res_ice_seq,
                time=time,
                var_factor=1000.0,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/Hg-2d_scalarmap_{}h'.format(hours[idx]),
                cmap_name='coolwarm',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$h_i$ $(mm)$')

            # Plotting ice cover type
            vnv_plot2d(\
                'CHARACTERISTICS ',
                res_ice_seq,
                time=time,
                filled_contours=True,
                fig_size=fig_size,
                fig_name='img/Ice-2d_scalarmap_{}h'.format(hours[idx]),
                cmap_name='coolwarm',
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='Ice type')

        #======================================================================
        # SENSIBILITY TO PHYSICAL AND NUMERICAL PARAMETERS:
        #
        #TODO

        # Closing files
        del geo
        #del res_ini
        del res_seq
        del res_par
        del res_ice_seq
        del res_ice_par

