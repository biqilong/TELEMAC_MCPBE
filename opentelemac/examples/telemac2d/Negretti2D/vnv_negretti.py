
"""
Validation script for negretti
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

        # negretti scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_negretti.cas')


        # negretti parallel mode
        cas = TelemacCas('t2d_negretti.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_negretti_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_negretti.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_negretti.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_history, vnv_plot1d_polylines, \
                vnv_plot2d
        # Getting files
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        #Plotting FREE SURFACE on 703, [0, 1] over records range(0, ntimestep)
        vnv_plot1d_history(\
            'FREE SURFACE',
            res_vnv_1_t2dres,
            'FREE SURFACE',
            points=[[0, 1]],
            nodes=[703],
            fig_size=(12, 3),
            fig_name='img/figure1')

        #Plotting FREE SURFACE in slice plane
        vnv_plot1d_polylines(\
            'FREE SURFACE', res_vnv_1_t2dres, 'elevation',
            fig_size=(12, 3),
            record=-1,
            poly=[[0, 0.93], [21, 1.07]],
            fig_name='img/figure2',
            plot_bottom=True)

        # Plotting VELOCITY at -1
        vnv_plot2d(\
            'VELOCITY',
            res_vnv_1_t2dres,
            record=-1,
            filled_contours=True,
            vectors=True, vectors_scale=50,
            fig_size=(13, 5),
            fig_name='img/figure6')

        #Plotting mesh
        vnv_plot2d(\
            '',
            res_vnv_1_t2dgeo,
            plot_mesh=True,
            fig_size=(12, 5),
            fig_name='img/figure7')

        # Plotting FREE SURFACE at -1
        vnv_plot2d(\
            'FREE SURFACE',
            res_vnv_1_t2dres,
            record=-1,
            filled_contours=True,
            fig_size=(13, 5),
            fig_name='img/figure8')

        # Plotting VELOCITY at -1
        vnv_plot2d(\
            'VELOCITY',
            res_vnv_1_t2dres,
            record=-1,
            filled_contours=True,
            streamlines=True, streamlines_density=1,
            grid_resolution=[20, 50],
            fig_size=(13, 5),
            fig_name='img/figure9')

        # Closing files
        del res_vnv_1_t2dres
        del res_vnv_1_t2dgeo
