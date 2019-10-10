
"""
Validation script for Rouse
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

        # Rouse parallel mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_rouse.cas')


        # Rouse parallel mode
        cas = TelemacCas('t3d_rouse.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_rouse_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_rouse.slf',
                            eps=[1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_rouse.slf',
                            eps=[1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)

        #Plotting Z on [250, 50] over records range(0, res_vnv_1_t3dres.ntimestep)
        nplan = res_vnv_1_t3dres.nplan
        node = res_vnv_1_t3dres.get_closest_node([250, 50], plane=0)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res_vnv_1_t3dres,
                'ELEVATION Z',
                nodes=[node +i*res_vnv_1_t3dres.npoin2 for i in range(nplan)],
                nodes_labels=["plane {}".format(i) for i in range(nplan)],
                fig_size=(12, 7),
                fig_name='img/timeseries')

        #TODO: Add offset to axes we cannont see the first and last plane (ax.set_aspect ?)
        node1 = res_vnv_1_t3dres.get_closest_node([250, 50], plane=nplan-1)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res_vnv_1_t3dres,
                'ELEVATION Z',
                nodes=[node, node1],
                nodes_labels=['first plane', 'last plane'],
                fig_size=(12, 7),
                fig_name='img/timeseries_p1')


        #Plotting mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        # Plotting vertical split
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t3dres,
                   poly=[[450, 0], [450, 100]],
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/tracer')

        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t3dres,
                   poly=[[450, 0], [450, 100]],
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/velocityU_section')

        # Plotting vertical split
        vnv_plot2d('VELOCITY V',
                   res_vnv_1_t3dres,
                   poly=[[450, 0], [450, 100]],
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/velocityV_section')

        # Plotting TRACER 1 at -1
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t3dres,
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/tracer_tf')

        res_vnv_1_t3dres.close()
