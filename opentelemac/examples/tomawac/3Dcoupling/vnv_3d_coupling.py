
"""
Validation script for 3Dcoupling
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
        self.tags = ['telemac3d', 'sisyphe', 'tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # littoral3D T3D+TOM scalar mode 2D coupling
        self.add_study('vnv_1',
                       'telemac3d',
                       'T3D_littoral.cas')


        # littoral3D T3D+TOM parallel mode 2D coupling
        cas = TelemacCas('T3D_littoral.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       'T3D_littoral_par.cas',
                       cas=cas)

        del cas


        # littoral3D T3D+TOM scalar mode 3D coupling
        self.add_study('vnv_3',
                       'telemac3d',
                       'T3D_3Dcoupling.cas')


        # littoral3D T3D+TOM parallel mode 3D coupling
        cas = TelemacCas('T3D_3Dcoupling.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       'T3D_3Dcoupling_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'ref_T3D_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'ref_T3D_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'ref_tom_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'ref_tom_littoral.slf',
                            eps=[1e-8, 361, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # NEW COUPLING Comparison with a reference for T3D.
        self.check_epsilons('vnv_3:T3DRES',
                            'ref_T3D3_littoralcoup.slf',
                            eps=[1e-8])

        # NEW COUPLING Comparison with a reference for WAC
        self.check_epsilons('vnv_3:WACRES',
                            'ref_tom_couplittoral.slf',
                            eps=[1e-8])

        # NEW COUPLING Comparison with a reference for T3DHYD
        self.check_epsilons('vnv_3:T3DHYD',
                            'ref_T3D2_littoralcoup.slf',
                            eps=[1e-8])

        # NEW COUPLING Comparison between sequential and parallel for T3D.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1e-8])

        # NEW COUPLING Comparison between sequential and parallel for WAC.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-8])

        # NEW COUPLING Comparison between sequential and parallel for WAC.
        self.check_epsilons('vnv_3:T3DHYD',
                            'vnv_4:T3DHYD',
                            eps=[1e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dgeo = self.get_study_file('vnv_1:T3DGEO')
        res_vnv_1_t3dgeo = TelemacFile(vnv_1_t3dgeo)
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        res_vnv_3_t3dres = TelemacFile(vnv_3_t3dres)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   fig_size=(9, 4),
                   fig_name='img/fond')


        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t3dres,
                   poly=[[500, 0], [500, 200]],
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultscoupVert')

        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t3dres,
                   poly=[[0, 100], [1000, 100]],
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultscoupVert2')

        # Plotting horizontal split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t3dres,
                   plane=0,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultshori')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM')


        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_3_t3dres,
                   poly=[[500, 0], [500, 200]],
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultscoupVert3')

        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_3_t3dres,
                   poly=[[0, 100], [1000, 100]],
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultscoupVert23')

        # Plotting horizontal split
        vnv_plot2d('VELOCITY U',
                   res_vnv_3_t3dres,
                   plane=0,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultshori3')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   record=-1,
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM3')

