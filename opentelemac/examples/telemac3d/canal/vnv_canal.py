
"""
Validation script for canal
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
        self.tags = ['telemac3d', 'telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # canal scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_canal.cas')


        # canal parallel mode
        cas = TelemacCas('t3d_canal.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_canal_par.cas',
                       cas=cas)

        del cas


        # canal scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_canal-nonhydro.cas')


        # canal parallel mode
        cas = TelemacCas('t3d_canal-nonhydro.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_canal-nonhydro_par.cas',
                       cas=cas)

        del cas


        # canal scalar mode
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_canal.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_canal.slf',
                            eps=[1.E-11])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_canal.slf',
                            eps=[1.E-11])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-11])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_canal_nonhydro.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_canal_nonhydro.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        from postel.plot1d import plot1d
        import matplotlib.pyplot as plt
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        res_vnv_3_t3dres = TelemacFile(vnv_3_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)
        vnv_5_t2dres = self.get_study_file('vnv_5:T2DRES')
        res_vnv_5_t2dres = TelemacFile(vnv_5_t2dres)

        fig, ax = plt.subplots(1, 1, figsize=(12, 9))

        poly = [[0, 50], [500, 50]]

        poly_number = res_vnv_5_t2dres.discretize_polyline(poly)

        # Telemac 2d data
        _, abs_curv, data2d = res_vnv_5_t2dres.get_timeseries_on_polyline(\
                poly, 'FREE SURFACE', poly_number)

        plot1d(ax, abs_curv, data2d[:, -1],
               plot_label='TELEMAC-2D')

        # Telemac 3d Non hydro static data
        _, abs_curv, poly_z = res_vnv_3_t3dres.get_data_values_on_vertical_plan(\
                poly, 'ELEVATION Z', poly_number, -1)

        plot1d(ax, abs_curv, poly_z[:, -1],
               plot_label='TELEMAC-3D Non-Hydrostatic')

        # Telemac 3d Non hydro static data
        _, abs_curv, poly_z = res_vnv_1_t3dres.get_data_values_on_vertical_plan(\
                poly, 'ELEVATION Z', poly_number, -1)

        plot1d(ax, abs_curv, poly_z[:, -1],
               plot_label='TELEMAC-3D Hydrostatic')

        ax.legend()

        fig_name = "img/free_surface"
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.clf()


        # Plotting horizontal split
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   plane=0,
                   record=0,
                   filled_contours=True,
                   fig_size=(10, 3),
                   fig_name='img/veloH')

        # Plotting horizontal split
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=0,
                   filled_contours=True,
                   fig_size=(10, 3),
                   fig_name='img/freeSurfac')

        #Plotting Horizontal Mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dhyd,
                   plot_mesh=True,
                   fig_size=(10, 15),
                   fig_name='img/Mesh')

        #Plotting Vertical Mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   record=0,
                   poly=[[0, 50], [500, 50]],
                   fig_size=(10, 3),
                   fig_name='img/MeshV')
