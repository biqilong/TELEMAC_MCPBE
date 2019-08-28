
"""
Validation script for vent
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # vent scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_vent.cas')


        # vent parallel mode
        cas = TelemacCas('t3d_vent.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_vent_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_vent.slf',
                            eps=[1.E-6, 1.E-7, 1.E-7, 1.E-8, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_vent.slf',
                            eps=[1.E-6, 1.E-7, 1.E-7, 1.E-8, 1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-6, 1.E-7, 1.E-8, 1.E-8, 1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_vertical_slice, \
                triangulation_from_data, plot_timeseries_on_polyline
        from postel.plot2d import plot2d_vectors
        import matplotlib.pyplot as plt
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)

        # Plotting WATER DEPTH over polyline over records res_vnv_1_t3dhyd.ntimestep
        plot_timeseries_on_polyline(\
                res_vnv_1_t3dhyd,
                'WATER DEPTH',
                poly=[[0, 50], [500, 50]],
                fig_size=(12, 7),
                fig_name='img/freeSurface')

        # Plotting vertical split
        plot_vertical_slice(res,
                            'VELOCITY V',
                            poly=[[0, 50], [500, 50]],
                            record=-1,
                            fig_size=(10, 15),
                            fig_name='img/fieldVelo')

        # Ploting vertical slice as vectors
        # TODO: Do a better plot

        # points defining the polyline
        poly_points = [[0., 50.], [500., 50.]]

        # number of points per segment of the polyline
        poly_number = res.discretize_polyline(poly_points)

        # slice at initial time step (-1) of the elevation variable
        _, abs_curv, values_poly_z =\
               res.get_data_values_on_vertical_plan(\
                 poly_points, 'ELEVATION Z', poly_number, -1)

        # slice at initial time step (-1) of the velocity u variable
        _, _, vel_u =\
               res.get_data_values_on_vertical_plan(\
                 poly_points, 'VELOCITY U', poly_number, -1)
        # slice at initial time step (-1) of the velocity v variable
        _, _, vel_v =\
               res.get_data_values_on_vertical_plan(\
                 poly_points, 'VELOCITY V', poly_number, -1)
        # creation of a mesh from the elevation value and curvilinear coordinate of the polyline
        mesh = triangulation_from_data(abs_curv, values_poly_z)

        fig, ax = plt.subplots(1, 1, figsize=(10, 15))

        plot2d_vectors(fig, ax, mesh, vel_u.flatten(), vel_v.flatten(),
                       grid_resolution=[100, 20],
                       data_name='velocity',
                       color='k',
                       vmin=(200, -30),
                       vmax=(300, 0))

        fig_name = 'img/vecVelo'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.clf()

        # Closing files
        del res
        del res_vnv_1_t3dhyd
