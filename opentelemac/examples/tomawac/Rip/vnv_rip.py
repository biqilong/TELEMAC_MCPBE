
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
                            eps=[1e-10])

        # T3D Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1e-10])

        # WAC Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_rip.slf',
                            eps=[1e-11])

        # WAC Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-11])


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
