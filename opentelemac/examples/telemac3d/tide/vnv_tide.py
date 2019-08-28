
"""
Validation script for tide
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

        # tide scalar mode type
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_tide-jmj_type.cas')


        # tide parallel mode type
        cas = TelemacCas('t3d_tide-jmj_type.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_tide-jmj_type_par.cas',
                       cas=cas)

        del cas


        # tide scalar mode real gen
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_tide-jmj_real_gen.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_tide-jmj_type.slf',
                            eps=[1.E0, 1.5E0, 5.E0, 1.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_tide-jmj_type.slf',
                            eps=[1.E0, 1.5E0, 5.E0, 1.E-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E0, 1.5E0, 5.E0, 1.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_tide-jmj_real_gen.slf',
                            eps=[1.E-1, 4.E-1, 3.E-1, 1.E-1])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)

        node = res.get_closest_node([195000, 150000], plane=res.nplan-1)

        vnv_plot1d_history(\
                'ELEVATION Z',
                res,
                'ELEVATION Z',
                nodes=[node],
                fig_size=(12, 7),
                fig_name='img/timeseries')


        #Plotting Z on a node
        vnv_plot1d_history(\
                'ELEVATION Z',
                res,
                'ELEVATION Z',
                nodes=[node -i*res.npoin2 for i in range(res.nplan-1)],
                fig_size=(12, 7),
                fig_name='img/timeseries_all_layers')


        #Plotting mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        # Plotting mesh section
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[185500, 150000], [200500, 150000]],
                   record=-1,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh_section')


        # Plotting Z at -1
        vnv_plot2d('ELEVATION Z',
                   res,
                   record=-1,
                   plane=res.nplan-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/z_map')


        # Plotting VELOCITY U at -5
        vnv_plot2d('VELOCITY U',
                   res,
                   plane=res.nplan-1,
                   record=res.ntimestep-5,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/velocity')
