
"""
Validation script for stratification
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # stratification scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_stratification.cas')


        # stratification parallel mode
        cas = TelemacCas('t3d_stratification.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_stratification_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_stratification.slf',
                            eps=[1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_stratification.slf',
                            eps=[1.E-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-10])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot2d_scalar_filled_contour,\
                triangulation_from_data
        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        import numpy as np
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)

        # Plotting horizontal split
        vnv_plot2d('ELEVATION Z',
                   res,
                   plane=0,
                   record=0,
                   filled_contours=True,
                   fig_size=(10, 15),
                   fig_name='img/bathy')

        # TODO: not the same plots of MeshV, Sal_ini and Sal_f
        # Plotting horizontal mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   plane=0,
                   record=0,
                   plot_mesh=True,
                   fig_size=(10, 15),
                   fig_name='img/Mesh')

        # Plotting horizontal mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[0, 50], [2000, 50]],
                   record=0,
                   plot_mesh=True,
                   fig_size=(10, 15),
                   fig_name='img/meshV')

        # Plotting vertical split
        vnv_plot2d('SALINITY',
                   res,
                   poly=[[0, 50], [2000, 50]],
                   record=0,
                   filled_contours=True,
                   fig_size=(10, 15),
                   fig_name='img/Sal_ini')

        # Plotting vertical split
        vnv_plot2d('SALINITY',
                   res,
                   poly=[[0, 50], [2000, 50]],
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 15),
                   fig_name='img/Sal_f')

        # Plotting vertical split

        fig, ax = plt.subplots(2, 1)

        poly = [[0, 50], [2000, 50]]

        poly_number = res.discretize_polyline(poly)

        _, abs_curv, poly_z0 = res.get_data_values_on_vertical_plan(\
                                  poly, 'ELEVATION Z', poly_number, 0)
        _, abs_curv, poly_z1 = res.get_data_values_on_vertical_plan(\
                                  poly, 'ELEVATION Z', poly_number, -1)

        _, _, data0 = res.get_data_values_on_vertical_plan(\
                           poly, 'NUZ FOR VELOCITY', poly_number, 0)

        _, _, data1 = res.get_data_values_on_vertical_plan(\
                           poly, 'NUZ FOR VELOCITY', poly_number, -1)
        mesh0 = triangulation_from_data(abs_curv, poly_z0)
        mesh1 = triangulation_from_data(abs_curv, poly_z1)

        plot2d_scalar_filled_contour(fig, ax[0], mesh0, data0.flatten(),
                                     data_name='nuz for velocity',
                                     levels=np.linspace(0, 0.021, 11))

        ax[0].set_title('Initial state')

        plot2d_scalar_filled_contour(fig, ax[1], mesh1, data1.flatten(),
                                     data_name='nuz for velocity',
                                     levels=np.linspace(0, 0.021, 11))

        ax[1].set_title('Final state')


        fig_name = 'img/turb'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.clf()
