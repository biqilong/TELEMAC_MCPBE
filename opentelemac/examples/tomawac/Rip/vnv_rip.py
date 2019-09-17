
"""
Validation script for rip
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
        self.tags = ['telemac3d', 'tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # RIP T3D+TOM scalar mode 3D coupling
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_rip.cas')


        # RIP T3D+TOM parallel mode 3D coupling
        cas = TelemacCas('t3d_rip.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_rip_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # T3D Comparison with a reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_rip.slf',
                            eps=[0.002,0.03,0.01,0.002,5e-6,0.004,0.0003,0.0002])

        # T3D Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[0.005,0.05,0.022,0.003,5e-6,0.008,0.0007,0.0005])

        # WAC Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_rip.slf',
                            eps=[0.006,3,1e-11,0.003,0.02,0.02,2e-5,1e-5,0.1,0.6])

        # WAC Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_4:WACRES',
                            eps=[0.006,4,1e-11,0.005,0.022,0.02,4e-5,2e-5,0.1,0.8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_vertical_slice
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        res_rip = TelemacFile('rip.slf')

        # Plotting BOTTOM at -1
        vnv_plot2d('BOTTOM',
                   res_rip,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/bottom')


        # Plotting vertical split
        plot_vertical_slice(res_vnv_1_t3dres,
                            'VELOCITY U',
                            poly=[[0, 9], [14.6, 9]],
                            record=-1,
                            fig_size=(12, 7),
                            fig_name='img/Ucoupver1')

        # Plotting vertical split
        plot_vertical_slice(res_vnv_1_t3dres,
                            'VELOCITY U',
                            poly=[[11.8, 0], [11.8, 18]],
                            record=-1,
                            fig_size=(12, 7),
                            fig_name='img/Ucoupver2')

        # Plotting vertical split
        plot_vertical_slice(res_vnv_1_t3dres,
                            'VELOCITY U',
                            poly=[[0, 13.6], [14.6, 13.6]],
                            record=-1,
                            fig_size=(12, 7),
                            fig_name='img/Ucoupver3')

        # Plotting vertical split
        plot_vertical_slice(res_vnv_1_t3dres,
                            'USTOKES',
                            poly=[[0, 13.6], [14.6, 13.6]],
                            record=-1,
                            fig_size=(12, 7),
                            fig_name='img/Ustokes')
        # Closing files
        del res_vnv_1_t3dres
        del res_rip
