
"""
Validation script for yen-multigrain
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
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # yen-exp_multi T2D+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_yen-exp_multi.cas')


        # yen-exp_multi T2D+SIS parallel mode
        cas = TelemacCas('t2d_yen-exp_multi.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_yen-exp_multi_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_yen-exp_multi.slf',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_yen-exp_multi.slf',
                            eps=[1.e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[1.e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_yen-exp_multi.slf',
                            eps=[1.e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_yen-exp_multi.slf',
                            eps=[1.e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.e-4])


    def _post(self):
        """
        Post-treatment processes
        """
        import matplotlib.tri as tri
        import matplotlib.image as image
        import matplotlib.pyplot as plt
        import numpy as np
        #TODO: Redo with postel ?
        #TODO: Remove yen*.py
        ########################################################################
        #
        # factor is used for scaling (evolution/h0_factor = evolutionCut)
        # See the paper of Yen et al. 1995
        ########################################################################

        slf = TelemacFile(self.get_study_file('vnv_1:SISRES'))
        mesh = np.array(slf.ikle3)
        meshx = slf.meshx
        meshy = slf.meshy
        triang = slf.tri

        evolution = slf.get_data_value('EVOLUTION', -1)
        evolution_interpolator = tri.LinearTriInterpolator(triang, evolution)

        ########################################################################
        #
        # Read the reference file
        #
        ########################################################################
        h0_factor = 0.0544

        #load profile 90
        data_profile_90 = np.genfromtxt("yen_90profilexy-koor.dat",
                                        names=["x", "y", "z"])
        profile_ref_90x = data_profile_90["x"]
        profile_ref_90y = data_profile_90["y"]
        profile_ref_90z = data_profile_90["z"]
        evolution_cut_90 = \
           evolution_interpolator.__call__(profile_ref_90x, profile_ref_90y)\
           /h0_factor

        distance_profile_90 = profile_ref_90y - 46.1371

        #load profile 180
        data_profile_180 = np.genfromtxt("yen_180profilexy-koor.dat",
                                         names=["x", "y", "z"])
        profile_ref_180x = data_profile_180["x"]
        profile_ref_180y = data_profile_180["y"]
        profile_ref_180z = data_profile_180["z"]
        evolution_cut_180 = \
           evolution_interpolator.__call__(profile_ref_180x, profile_ref_180y)\
           /h0_factor

        distance_profile_180 = profile_ref_180x - 104.682


        ########################################################################
        #
        # Plot with matplotlib
        #
        ########################################################################

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
        title = "CrossSections_90"
        fig_name = "img/"+title
        plt.title(title)
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name + ".pdf")
        plt.savefig(fig_name + ".png")
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
        title = "CrossSections_180"
        fig_name = "img/"+title
        plt.title(title)
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name + ".pdf")
        plt.savefig(fig_name + ".png")
        plt.clf()

        ########################################################################
        #
        # factor is used for scaling (evolution/h0_factor = evolutionCut)
        # See the paper of Yen et al. 1995
        ########################################################################

        #scale evolution
        h0_factor = 0.0544
        evolution = evolution/h0_factor

        fig = plt.figure()
        ax = fig.add_subplot(111)
        ax.axis('off')

        levels = np.asarray([-0.5, -0.25, 0, 0.25, 0.5])
        plt.tricontourf(meshx, meshy, mesh, evolution,
                        levels, extend='both', alpha=0.6)

        cbar = plt.colorbar()
        cbar.set_label('EVOLUTION')
        #plt.legend()

        data_xmin = 96.6819 -0.12
        data_xmax = 105.6819 + 0.4
        data_ymin = 42.6371 +1
        data_ymax = 42.6371 + 1.01
        plt.xlim([data_xmin, data_xmax])
        plt.ylim([data_ymin, data_ymax])


        fig_name = "img/EvolutionR05"
        img = image.imread('data/PaperDataRun5.png')

        xmin = data_xmin -0.2
        xmax = data_xmax -0.055

        ymin = data_ymin -3.3
        ymax = data_ymax + 3.83

        plt.axes().set_aspect('equal', 'datalim')
        ax.imshow(img, aspect=1, extent=[xmin, xmax, ymin, ymax], zorder=+1)
        plt.tight_layout()
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name + ".pdf", dpi=300)
        plt.savefig(fig_name + ".png", dpi=300)
        plt.clf()
