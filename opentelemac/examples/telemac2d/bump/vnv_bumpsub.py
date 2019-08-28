
"""
Validation script for bumpsub
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # bumpsub scalar mode
        self.add_study('vnv_bumpsub_scal',
                       'telemac2d',
                       't2d_bumpsub.cas')

        # bumpsub parallel mode
        cas = TelemacCas('t2d_bumpsub.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_bumpsub_paral',
                       'telemac2d',
                       't2d_bumpsub_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bumpsub_scal:T2DRES',
                            'f2d_bumpsub.slf',
                            eps=[1000])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_bumpsub_paral:T2DRES',
                            'f2d_bumpsub.slf',
                            eps=[1000])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_bumpsub_scal:T2DRES',
                            'vnv_bumpsub_paral:T2DRES',
                            eps=[1000])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d

        # Getting files
        geom, _ = self.get_study_res('vnv_bumpsub_scal:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('vnv_bumpsub_scal:T2DRES')

        # Plot bathy:
        vnv_plot1d_polylines(\
            'BOTTOM',
            res,
            '',
            fig_size=(8, 2),
            record=0,
            fig_name='img/bumpsub_bathy',
            plot_bottom=True)

        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/bumpsub_mesh0',
            annotate_bnd=False,
            plot_mesh=True)

        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/bumpsub_mesh',
            annotate_bnd=True,
            plot_mesh=True)

        # Plot free surface:
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res,
            'Elevation',
            fig_size=(5, 4),
            record=-1,
            ref_name='EXACT ELEVATION',
            fig_name='img/bumpsub_free_surface',
            ylim=[-0.2, 1.9],
            plot_bottom=True)

        # Plot froud number:
        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res,
            'Froude number',
            fig_size=(5, 4),
            record=-1,
            fig_name='img/bumpsub_froude_number',
            y_label='Fr',
            plot_bottom=False)

        # Plot velocity:
        vnv_plot2d(\
            'VELOCITY',
            res,
            record=-1,
            fig_size=(10, 2),
            fig_name='img/bumpsub_velocity_vector',
            cbar_name='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            vectors=True, vectors_scale=30,
            grid_resolution=[10, 10])

        # Closing files
        del geom
        del res
