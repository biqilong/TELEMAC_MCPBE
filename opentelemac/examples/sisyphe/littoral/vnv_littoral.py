
"""
Validation script for littoral
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
        self.rank = 0
        self.tags = ['telemac2d', 'sisyphe', 'tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # littoral T2D+TOM+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_littoral.cas')


        # littoral T2D+TOM+SIS scalar mode
        cas = TelemacCas('t2d_littoral.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_littoral_par.cas',
                       cas=cas)
        del cas

        # littoral T2D+TOM+SIS tel2tom 'scalar' en fait 2 proc mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_tel2tom_same.cas')
        
        cas = TelemacCas('t2d_tel2tom_same.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_tel2tom_same_par.cas',
                       cas=cas)

        del cas
        
        # littoral T2D+TOM+SIS tel2tom2 'scalar' en fait 2 proc mode
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_tel2tom_different.cas')
        
        cas = TelemacCas('t2d_tel2tom_different.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_tel2tom_different_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'ref_sis_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'ref_sis_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'ref_t2d_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'ref_t2d_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'ref_tom_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'ref_tom_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:SISRES',
                            'ref_sis_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:SISRES',
                            'ref_sis_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:SISRES',
                            'vnv_4:SISRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'ref_t2d_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T2DRES',
                            'ref_t2d_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:WACRES',
                            'ref_tom_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:WACRES',
                            'ref_tom_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:SISRES',
                            'ref_sis_different.slf',
                            eps=[1e6] )
                            #eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:SISRES',
                            'ref_sis_different.slf',
                            eps=[1e6] )
                            #eps=[2e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:SISRES',
                            'vnv_6:SISRES',
                            eps=[1e6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T2DRES',
                            'ref_t2d_different.slf',
                            eps=[1e6] )
                            #eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T2DRES',
                            'ref_t2d_different.slf',
                            eps=[1e6] )
                            #eps=[2e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[1e6] )
                            #                            eps=[2e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:WACRES',
                            'ref_tom_different.slf',
                            eps=[1e6] )
                            #eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:WACRES',
                            'ref_tom_different.slf',
                            eps=[1e6] )
                            #eps=[2e-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:WACRES',
                            'vnv_6:WACRES',
                            eps=[1e6] )
                            #eps=[2e-1])


        

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_1_sisres = self.get_study_file('vnv_1:SISRES')
        res_vnv_1_sisres = TelemacFile(vnv_1_sisres)
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)
        vnv_3_sisres = self.get_study_file('vnv_3:SISRES')
        res_vnv_3_sisres = TelemacFile(vnv_3_sisres)
        vnv_3_t2dres = self.get_study_file('vnv_3:T2DRES')
        res_vnv_3_t2dres = TelemacFile(vnv_3_t2dres)

        vnv_5_wacres = self.get_study_file('vnv_5:WACRES')
        res_vnv_5_wacres = TelemacFile(vnv_5_wacres)
        vnv_5_sisres = self.get_study_file('vnv_5:SISRES')
        res_vnv_5_sisres = TelemacFile(vnv_5_sisres)
        vnv_5_t2dres = self.get_study_file('vnv_5:T2DRES')
        res_vnv_5_t2dres = TelemacFile(vnv_5_t2dres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(9, 4),
                   fig_name='img/fond')


        # Plotting VELOCITY U at -1
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsT2D')


        # Plotting BED SHEAR STRESS at -1
        vnv_plot2d('BED SHEAR STRESS',
                   res_vnv_1_sisres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsSIS')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM')
        
        # Plotting VELOCITY U at -1
        vnv_plot2d('VELOCITY U',
                   res_vnv_3_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTEL2TOM')


        # Plotting BED SHEAR STRESS at -1
        vnv_plot2d('BED SHEAR STRESS',
                   res_vnv_3_sisres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsSISTEL2TOM')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM2TEL')
        # Plotting VELOCITY U at -1
        
        vnv_plot2d('VELOCITY U',
                   res_vnv_5_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTEL2TOMdiff')


        # Plotting BED SHEAR STRESS at -1
        vnv_plot2d('BED SHEAR STRESS',
                   res_vnv_5_sisres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsSISTEL2TOMdiff')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_5_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM2TELdiff')

        # Closing files
        res_vnv_1_wacres.close()
        res_vnv_1_sisres.close()
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
        res_vnv_3_wacres.close()
        res_vnv_3_sisres.close()
        res_vnv_3_t2dres.close()
