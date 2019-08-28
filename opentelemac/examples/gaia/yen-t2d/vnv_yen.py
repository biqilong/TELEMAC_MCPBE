
"""
Validation script for yen
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
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # yen-exp T2D+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_yen-exp.cas')


        # yen-exp T2D+SIS parallel mode
        cas = TelemacCas('t2d_yen-exp.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_yen-exp_par.cas',
                       cas=cas)

        del cas


        # yen-exp_multi T2D+GAI scalar mode
        self.add_study('vnv_1_vf',
                       'telemac2d',
                       't2d_yen-exp_vf.cas')


        # yen-exp_multi T2D+GAI scalar mode
        cas = TelemacCas('t2d_yen-exp_vf.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2_vf',
                       'telemac2d',
                       't2d_yen-exp_vf_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_yen-exp.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_yen-exp.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_yen-exp.slf',
                            eps=[1.e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_yen-exp.slf',
                            eps=[1.e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1_vf:GAIRES',
                            'gai_ref_yen-exp_vf.slf',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2_vf:GAIRES',
                            'gai_ref_yen-exp_vf.slf',
                            eps=[1e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1_vf:GAIRES',
                            'vnv_2_vf:GAIRES',
                            eps=[1e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1_vf:T2DRES',
                            'f2d_yen-exp_vf.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2_vf:T2DRES',
                            'f2d_yen-exp_vf.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1_vf:T2DRES',
                            'vnv_2_vf:T2DRES',
                            eps=[1e-4])


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

            del slf

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

        yen(self.get_study_file('vnv_1:GAIRES'), 'gaia_yen-exp')
        yen_contour(self.get_study_file('vnv_1:GAIRES'), 'gaia_yen-exp')

        yen(self.get_study_file('vnv_1_vf:GAIRES'), 'gaia_yen-exp_vf')
        yen_contour(self.get_study_file('vnv_1_vf:GAIRES'), 'gaia_yen-exp_vf')
