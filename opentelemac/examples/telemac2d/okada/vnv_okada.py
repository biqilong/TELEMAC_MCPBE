
"""
Validation script for okada
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

        # okada scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_okada.cas')


        # okada parallel mode
        cas = TelemacCas('t2d_okada.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_okada_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_okada.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_okada.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-7, 1.E-7, 2.E-6, 1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
                # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)

        # Plotting FREE SURFACE over polyline over records [0, 1, 2, 3]
        vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                poly=[[-1600000, -1400000], [-1000000, -2000000]],
                record=[0, 1, 2, 3],
                fig_size=(12, 5),
                fig_name='img/FS_section')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(10, 8),
                   fig_name='img/Mesh')


        # Plotting FREE SURFACE at 0
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=0,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurfacet0')


        # Plotting FREE SURFACE at 3
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=3,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurfacet3')


        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurfacetf')

        # Closing files
        res_vnv_1_t2dres.close()
        res_vnv_1_t2dgeo.close()
