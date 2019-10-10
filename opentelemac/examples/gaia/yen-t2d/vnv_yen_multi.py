
"""
Validation script for yen_multi
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
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # yen-exp_multi T2D+GAI scalar mode
        self.add_study('vnv_multi1',
                       'telemac2d',
                       't2d_yen-exp_multi1.cas')


        # yen-exp_multi T2D+GAI scalar mode
        for bed_form in [2, 3, 4, 5, 6, 7, 10, 30, 9]:
            t2d_steering_file = 't2d_yen-exp_multi{}.cas'.format(bed_form)
            gai_steering_file = 'gaia_yen-exp_multi{}.cas'.format(bed_form)

            t2d_cas = TelemacCas('t2d_yen-exp_multi1.cas', get_dico('telemac2d'))
            t2d_cas.set('GAIA STEERING FILE', gai_steering_file)

            gai_cas = TelemacCas('gaia_yen-exp_multi1.cas', get_dico('gaia'))
            gai_cas.set('BED-LOAD TRANSPORT FORMULA FOR ALL SANDS', bed_form)
            if bed_form == 9:
                gai_cas.set('LAYERS NON COHESIVE BED POROSITY',
                            [0.37500, 0.375, 0.375, 0.375])
                gai_cas.set('ACTIVE LAYER THICKNESS', 0.01)
                gai_cas.set('NUMBER OF LAYERS FOR INITIAL STRATIFICATION', 4)
            gai_cas.write(gai_steering_file)

            self.add_study('vnv_multi{}'.format(bed_form),
                           'telemac2d',
                           t2d_steering_file,
                           cas=t2d_cas)
            del t2d_cas
            del gai_cas


        for hid_form in [1, 2, 4]:
            t2d_steering_file = 't2d_yen-exp_multi1_hid{}.cas'.format(hid_form)
            gai_steering_file = 'gaia_yen-exp_multi1_hid{}.cas'.format(hid_form)

            t2d_cas = TelemacCas('t2d_yen-exp_multi1.cas', get_dico('telemac2d'))
            t2d_cas.set('GAIA STEERING FILE', gai_steering_file)

            gai_cas = TelemacCas('gaia_yen-exp_multi1.cas', get_dico('gaia'))
            gai_cas.set('HIDING FACTOR FORMULA', hid_form)
            gai_cas.write(gai_steering_file)

            self.add_study('vnv_multi1_hid{}'.format(hid_form),
                           'telemac2d',
                           t2d_steering_file,
                           cas=t2d_cas)
            del t2d_cas
            del gai_cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1:GAIRES',
                            'gai_ref_multi1.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1:T2DRES',
                            'f2d_multi1.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi2:GAIRES',
                            'gai_ref_multi2.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi2:T2DRES',
                            'f2d_multi2.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi3:GAIRES',
                            'gai_ref_multi3.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi3:T2DRES',
                            'f2d_multi3.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi4:GAIRES',
                            'gai_ref_multi4.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi4:T2DRES',
                            'f2d_multi4.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi5:GAIRES',
                            'gai_ref_multi5.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi5:T2DRES',
                            'f2d_multi5.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi6:GAIRES',
                            'gai_ref_multi6.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi6:T2DRES',
                            'f2d_multi6.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi7:GAIRES',
                            'gai_ref_multi7.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi7:T2DRES',
                            'f2d_multi7.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi10:GAIRES',
                            'gai_ref_multi10.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi10:T2DRES',
                            'f2d_multi10.slf',
                            eps=[1e-3])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi30:GAIRES',
                            'gai_ref_multi30.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi30:T2DRES',
                            'f2d_multi30.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi9:GAIRES',
                            'gai_ref_multi9.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi9:T2DRES',
                            'f2d_multi9.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid1:GAIRES',
                            'gai_ref_multi1_hid1.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid1:T2DRES',
                            'f2d_multi1_hid1.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid2:GAIRES',
                            'gai_ref_multi1_hid2.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid2:T2DRES',
                            'f2d_multi1_hid2.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid4:GAIRES',
                            'gai_ref_multi1_hid4.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_multi1_hid4:T2DRES',
                            'f2d_multi1_hid4.slf',
                            eps=[1e-3])


    def _post(self):
        """
        Post-treatment processes
        """
        import matplotlib.pyplot as plt
        import numpy as np

        def yen(file_name, in_title):
            """
            Custom plot

            @param file_name (str) Name of the result file
            @parma in_title (str) Title of the fig and part of name of file
            """
            # factor is used for scaling (evolution/h0_factor = evolutionCut)
            # See the paper of Yen et al. 1995
            import matplotlib.tri as tri

            slf = TelemacFile(file_name)
            mesh = np.array(slf.ikle3)
            triang = tri.Triangulation(slf.meshx, slf.meshy, mesh)

            evolution = slf.get_data_value('CUMUL BED EVOL', -1)
            evolution_interpolator = \
                    tri.LinearTriInterpolator(triang, evolution)

            # Read the reference file
            h0_factor = 0.0544

            #load profile 90
            data_profile_90 = np.genfromtxt("data/yen_90profilexy-koor.dat",
                                            names=["x", "y", "z"])
            profile_ref_90x = data_profile_90["x"]
            profile_ref_90y = data_profile_90["y"]
            profile_ref_90z = data_profile_90["z"]
            evolution_cut_90 = \
              evolution_interpolator.__call__(profile_ref_90x,
                                              profile_ref_90y)/h0_factor

            distance_profile_90 = profile_ref_90y - 46.1371

            #load profile 180
            data_profile_180 = np.genfromtxt("data/yen_180profilexy-koor.dat",
                                             names=["x", "y", "z"])
            profile_ref_180x = data_profile_180["x"]
            profile_ref_180y = data_profile_180["y"]
            profile_ref_180z = data_profile_180["z"]
            evolution_cut_180 = \
               evolution_interpolator.__call__(profile_ref_180x,
                                               profile_ref_180y)/h0_factor

            distance_profile_180 = profile_ref_180x - 104.682

            ###################################################################
            #
            # Plot with matplotlib
            #
            ###################################################################

            #plot 90
            plt.plot(distance_profile_90, profile_ref_90z, "o-",
                     color="darkorange", label="Reference")
            plt.plot(distance_profile_90, evolution_cut_90, "<--",
                     color="green", label="Simulation")
            plt.xlabel("Distance [m]")
            plt.ylabel("Evolution [m]")
            plt.grid()
            plt.xlim([0.0, 1.0])
            plt.legend()
            title = in_title +"_90"
            plt.title(title)
            print(" "*8+"~> Plotting "+title)
            plt.savefig('img/' + title + ".pdf")
            plt.savefig('img/' + title + ".png")
            plt.clf()

            #plot 180
            plt.plot(distance_profile_180, profile_ref_180z, "o-",
                     color="darkorange", label="Reference")
            plt.plot(distance_profile_180, evolution_cut_180, "<--",
                     color="green", label="Simulation")
            plt.xlabel("Distance [m]")
            plt.ylabel("Evolution [m]")
            plt.grid()
            plt.xlim([0.0, 1.0])
            plt.legend()
            title = in_title + "_180"
            plt.title(title)
            print(" "*8+"~> Plotting "+title)
            plt.savefig('img/'+title + ".pdf")
            plt.savefig('img/'+title + ".png")
            plt.clf()

            slf.close()

        def yen_contour(file_name, in_title):
            """
            Custom contour

            @param file_name (str) Name of the result file
            @parma in_title (str) Title of the fig and part of name of file
            """

            import matplotlib.image as image

            slf = TelemacFile(file_name)
            mesh = np.array(slf.ikle3)

            evolution = slf.get_data_value('CUMUL BED EVOL', -1)

            #scale evolution
            h0_factor = 0.0544
            evolution = evolution/h0_factor

            fig = plt.figure()
            ax = fig.add_subplot(111)
            ax.axis('off')

            levels = np.asarray([-0.5, -0.25, 0, 0.25, 0.5])
            plt.tricontourf(slf.meshx, slf.meshy, mesh, evolution,
                            levels, extend='both', alpha=0.6)

            cbar = plt.colorbar()
            cbar.set_label('EVOLUTION')
            #plt.legend()

            data_xmin = 96.6819 -0.12
            data_xmax = 105.6819 + 0.4
            data_ymin = 42.6371 -1
            data_ymax = 42.6371 + 1.0
            plt.xlim([data_xmin, data_xmax])
            plt.ylim([data_ymin, data_ymax])

            title = in_title + "_EvolutionR05"
            img = image.imread('data/PaperDataRun5.png')

            xmin = data_xmin -0.2
            xmax = data_xmax -0.055

            ymin = data_ymin -3.3
            ymax = data_ymax + 3.83

            plt.axes().set_aspect('equal', 'datalim')
            ax.imshow(img, aspect=1, extent=[xmin, xmax, ymin, ymax],
                      zorder=+1)
            plt.tight_layout()
            print(" "*8+"~> Plotting img/"+title)
            plt.savefig('img/' + title + ".pdf", dpi=300)
            plt.savefig('img/' + title + ".png", dpi=300)
            plt.clf()

            slf.close()

        for bed_form in [1, 2, 3, 4, 5, 6, 7, 10, 30, 9]:
            res_file = self.get_study_file('vnv_multi{}:GAIRES'.format(bed_form))
            title = 'gaia_yen-exp_multi{}'.format(bed_form)
            yen(res_file, title)
            yen_contour(res_file, title)

        for hid_form in [1, 2, 4]:
            res_file = self.get_study_file('vnv_multi1_hid{}:GAIRES'.format(hid_form))
            title = 'gaia_yen-exp_multi1_hid{}'.format(hid_form)
            yen(res_file, title)
            yen_contour(res_file, title)


