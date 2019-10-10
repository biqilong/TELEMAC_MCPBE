
"""
Validation script for init
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # init_1 scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_init.cas')


        # init_1 parallel mode
        cas = TelemacCas('t2d_init.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_init_par.cas',
                       cas=cas)

        del cas


        # init no water scalar mode
        self.add_study('vnv_no_water_seq',
                       'telemac2d',
                       't2d_init-no-water.cas')


        # init no water parallel mode
        cas = TelemacCas('t2d_init-no-water.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_no_water_par',
                       'telemac2d',
                       't2d_init-no-water_par.cas',
                       cas=cas)

        del cas


        # init_cin scalar mode
        self.add_study('vnv_cin-seq',
                       'telemac2d',
                       't2d_init_cin.cas')


        # init_cin parallel mode
        cas = TelemacCas('t2d_init_cin.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 8)

        self.add_study('vnv_cin-par',
                       'telemac2d',
                       't2d_init_cin_par.cas',
                       cas=cas)

        del cas


        # init_cin scalar mode
        self.add_study('vnv_cin-src-seq',
                       'telemac2d',
                       't2d_init_cin_source_by_reg.cas')


        # init_cin parallel mode
        cas = TelemacCas('t2d_init_cin_source_by_reg.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 8)

        self.add_study('vnv_cin-src-par',
                       'telemac2d',
                       't2d_init_cin_source_by_reg_par.cas',
                       cas=cas)

        del cas


        # init restart scalar mode
        self.add_study('vnv_restart-seq',
                       'telemac2d',
                       't2d_init-restart.cas')


        # init restart parallel mode
        cas = TelemacCas('t2d_init-restart.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_restart-par',
                       'telemac2d',
                       't2d_init-restart_par.cas',
                       cas=cas)

        del cas


        # init restart scalar mode with source file
        self.add_study('vnv_restart-reg-seq',
                       'telemac2d',
                       't2d_init-restart_source_by_reg.cas')


        # init restart parallel mode with source file
        cas = TelemacCas('t2d_init-restart_source_by_reg.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_restart-reg-par',
                       'telemac2d',
                       't2d_init-restart_source_by_reg_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_init.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_init.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_no_water_seq:T2DRES',
                            'f2d_init-no-water.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_no_water_par:T2DRES',
                            'f2d_init-no-water.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_no_water_seq:T2DRES',
                            'vnv_no_water_par:T2DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_cin-seq:T2DRES',
                            'f2d_init_cin.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_cin-par:T2DRES',
                            'f2d_init_cin.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_cin-seq:T2DRES',
                            'vnv_cin-par:T2DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_cin-src-seq:T2DRES',
                            'vnv_cin-seq:T2DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_cin-src-par:T2DRES',
                            'vnv_cin-par:T2DRES',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_cin-src-seq:T2DRES',
                            'vnv_cin-src-par:T2DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_restart-seq:T2DRES',
                            'f2d_init-restart.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_restart-par:T2DRES',
                            'f2d_init-restart.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_restart-seq:T2DRES',
                            'vnv_restart-par:T2DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_restart-reg-seq:T2DRES',
                            'vnv_restart-seq:T2DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_restart-reg-par:T2DRES',
                            'vnv_restart-par:T2DRES',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_restart-reg-par:T2DRES',
                            'vnv_restart-reg-seq:T2DRES',
                            eps=[])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_cin_seq_t2dgeo = self.get_study_file('vnv_cin-seq:T2DGEO')
        res_vnv_cin_seq_t2dgeo = TelemacFile(vnv_cin_seq_t2dgeo)
        vnv_cin_seq_t2dres = self.get_study_file('vnv_cin-seq:T2DRES')
        res_vnv_cin_seq_t2dres = TelemacFile(vnv_cin_seq_t2dres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_cin_seq_t2dgeo,
                   plot_mesh=True,
                   fig_size=(10, 5),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_cin_seq_t2dres,
                   record=0,
                   filled_contours=True,
                   fig_size=(10, 5),
                   fig_name='img/Bathy')


        # Plotting FREE SURFACE at 1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_cin_seq_t2dres,
                   record=1,
                   filled_contours=True,
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface_t1')


        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_cin_seq_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 5),
                   fig_name='img/FreeSurface_tf')


        # Plotting VISCOSITY at -1
        vnv_plot2d('VISCOSITY',
                   res_vnv_cin_seq_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 5),
                   fig_name='img/Turb_Visco_tf')

        # Closing files
        res_vnv_cin_seq_t2dgeo.close()
        res_vnv_cin_seq_t2dres.close()
