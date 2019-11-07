
"""
Validation script for mersey
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # mersey scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_mersey.cas')


        # mersey parallel mode
        cas = TelemacCas('t2d_mersey.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_mersey_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_mersey.slf',
                            eps=[2e-1, 2e-1, 2e-1, 2e-1, 1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_mersey.slf',
                            eps=[2e-1, 3e-1, 2e-1, 2e-1, 1e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-1, 4e-1, 4e-2, 4e-2, 1e-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
        # Getting files
#        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
#        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo, load_bnd=True)
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
#                   annotate_liq_bnd=True,
                   fig_size=(7, 7),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   fig_size=(7, 7),
                   fig_name='img/Bathy')


        # Plotting WATER DEPTH at 22350
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   time=22350,
                   fig_title='WATER DEPTH at t= 22350s',
                   plot_mesh=True,
                   filled_contours=True,
                   vmin=0.05,
                   vmax=24,
                   nv=11,
                   fig_size=(7, 7),
                   fig_name='img/Water_depth22')

        # Plotting WATER DEPTH at t= 44700s
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   time=44700,
                   fig_title='WATER DEPTH at t= 44700s',
                   plot_mesh=True,
                   filled_contours=True,
                   vmin=0.05,
                   vmax=24,
                   nv=11,
                   fig_size=(7, 7),
                   fig_name='img/Water_depth44')

        # Plotting VELOCITY at -1
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   time=11200,
                   plot_mesh=True,
                   cbar_priority='vector',
                   colored_vectors=True,
                   grid_resolution=[50, 50],
                   fig_size=(7, 7),
                   fig_name='img/Velocity_arrows')

        # Plotting at points
        vnv_plot1d_history(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                '',
                points_labels=['1','2','3','4'],
                points=[[318021,400340],[331444,395025],[337922,383132],[348256,380434]],
                fig_size=(7, 5),
                fig_name='img/timeserie',
                fig_title='FREE SURFACE',
                xlim=[0., 45000],
                ylim=[0, 10]
        )

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()

