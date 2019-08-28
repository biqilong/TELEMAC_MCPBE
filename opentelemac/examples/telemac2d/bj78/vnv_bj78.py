
"""
Validation script for bj78
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

        # bj78 scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_bj78.cas')


        # bj78 parallel mode
        cas = TelemacCas('t2d_bj78.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_bj78_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_bj78.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_bj78.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history,\
                 vnv_plot1d_polylines
        # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)

        #Plotting FREE SURFACE on [10, 0.4] over records 0:5
        vnv_plot1d_history(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                legend_labels='FREE SURFACE',
                points=[[10, 0.4]],
                fig_size=(12, 5),
                fig_name='img/FreeSurfaceTimeSeries')

        # Plotting FREE SURFACE over polyline over records 0:5
        vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                poly=[[-5, 0.4], [10, 0.4]],
                record=list(range(0, res_vnv_1_t2dres.ntimestep, 5)),
                fig_size=(12, 5),
                fig_name='img/FreeSurfaceSection')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(20, 5),
                   fig_name='img/Mesh')

        # Closing files
        del res_vnv_1_t2dres
        del res_vnv_1_t2dgeo
