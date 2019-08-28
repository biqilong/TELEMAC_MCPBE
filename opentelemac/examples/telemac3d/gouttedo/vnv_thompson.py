
"""
Validation script for thompson
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
        self.rank = 4
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # thompson scalar mode
        self.add_study('vnv_th_1',
                       'telemac3d',
                       't3d_thompson.cas')


        # thompson parallel mode
        cas = TelemacCas('t3d_thompson.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_th_2',
                       'telemac3d',
                       't3d_thompson_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_th_1:T3DRES',
                            'f3d_thompson.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_th_2:T3DRES',
                            'f3d_thompson.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_th_1:T3DRES',
                            'vnv_th_2:T3DRES',
                            eps=[1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_3d_scalar_map
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_th_1_t3dres = self.get_study_file('vnv_th_1:T3DRES')
        res_vnv_th_1_t3dres = TelemacFile(vnv_th_1_t3dres)
        vnv_th_1_t3dhyd = self.get_study_file('vnv_th_1:T3DHYD')
        res_vnv_th_1_t3dhyd = TelemacFile(vnv_th_1_t3dhyd)

        # Plotting Horizontal mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_th_1_t3dres,
                   plot_mesh=True,
                   fig_size=(10, 15),
                   fig_name='img/Mesh_th')
        # Plotting Vercial Mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_th_1_t3dres,
                   poly=[[0, 10], [20, 10]],
                   record=0,
                   plot_mesh=True,
                   fig_size=(10, 5),
                   fig_name='img/MeshV_th')

        # Plotting 3d scalar map for WATER DEPTH at 0
        plot_3d_scalar_map(res_vnv_th_1_t3dhyd,
                           'WATER DEPTH',
                           record=0,
                           fig_size=(10, 15),
                           fig_name='img/water_depth_th_0')


        # Plotting 3d scalar map for WATER DEPTH at 2
        plot_3d_scalar_map(res_vnv_th_1_t3dhyd,
                           'WATER DEPTH',
                           record=2,
                           fig_size=(10, 15),
                           fig_name='img/water_depth_th_08')


        # Plotting 3d scalar map for WATER DEPTH at 4
        plot_3d_scalar_map(res_vnv_th_1_t3dhyd,
                           'WATER DEPTH',
                           record=4,
                           fig_size=(10, 15),
                           fig_name='img/water_depth_th_16')


        # Plotting 3d scalar map for WATER DEPTH at 6
        plot_3d_scalar_map(res_vnv_th_1_t3dhyd,
                           'WATER DEPTH',
                           record=6,
                           fig_size=(10, 15),
                           fig_name='img/water_depth_th_24')


        # Plotting 3d scalar map for WATER DEPTH at 8
        plot_3d_scalar_map(res_vnv_th_1_t3dhyd,
                           'WATER DEPTH',
                           record=8,
                           fig_size=(10, 15),
                           fig_name='img/water_depth_th_32')


        # Plotting 3d scalar map for WATER DEPTH at 10
        plot_3d_scalar_map(res_vnv_th_1_t3dhyd,
                           'WATER DEPTH',
                           record=10,
                           fig_size=(10, 15),
                           fig_name='img/water_depth_th_40')