
"""
Validation script for NonLinearWave
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
        self.rank = 3
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # NonLinearWave scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_nonlinearwave.cas')


        # NonLinearWave parallel mode
        cas = TelemacCas('t3d_nonlinearwave.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_nonlinearwave_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_nonlinearwave.slf',
                            eps=[1.E-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_nonlinearwave.slf',
                            eps=[1.E-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-9])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)

        #Plotting mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')
