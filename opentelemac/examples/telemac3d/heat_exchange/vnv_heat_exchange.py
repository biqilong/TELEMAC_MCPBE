
"""
Validation script for heat_exchange
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
        self.tags = ['telemac3d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # heat exchange scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_heat_exchange.cas')


        # heat exchange parallel mode
        cas = TelemacCas('t3d_heat_exchange.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_heat_exchange_par.cas',
                       cas=cas)

        del cas


        # heat exchange rain wind scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_heat_exchange_rain_wind.cas')


        # heat exchange rain wind parallel mode
        cas = TelemacCas('t3d_heat_exchange_rain_wind.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_heat_exchange_rain_wind_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_heat_exchange.slf',
                            eps=[1.E-4, 0.03, 0.03, 0.004, 0.4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_heat_exchange.slf',
                            eps=[2.E-4, 0.03, 0.03, 0.004, 0.3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-4, 0.03, 0.03, 0.004, 0.4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_heat_exchange_rain_wind.slf',
                            eps=[2.E-3, 1.E-4, 1.E-4, 1.E-5, 5.E-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_heat_exchange_rain_wind.slf',
                            eps=[2.E-3, 2.E-4, 1.E-4, 1.E-5, 5.E-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[2.E-3, 2.E-4, 1.E-4, 1.E-5, 6.E-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)

        node = res.get_closest_node([0, 0], plane=res.nplan-1)

        #Plotting Z on [0, 0] over records range(0, res.ntimestep)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res,
                'ELEVATION Z',
                nodes=[node],
                fig_size=(12, 7),
                fig_name='img/timeseries')

        #Plotting Z on [0, 0] over records range(0, res.ntimestep)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res,
                'ELEVATION Z',
                nodes=[node -i*res.npoin2 for i in range(res.nplan)],
                nodes_labels=['plane {}'.format(i) for i in range(res.nplan)],
                fig_size=(12, 7),
                fig_name='img/timeseries_all_layers')


        #Plotting mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        #Plotting mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[-100, 0], [100, 0]],
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh_section')

        # Plotting VELOCITY U at -1
        vnv_plot2d('VELOCITY U',
                   res,
                   record=-1,
                   filled_contours=True,
                   cmap_anme='viridis',
                   fig_size=(12, 7),
                   fig_name='img/velocity')

        res.close()
