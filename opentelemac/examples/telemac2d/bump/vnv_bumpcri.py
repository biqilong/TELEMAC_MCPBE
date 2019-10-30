
"""
Validation script for bumpcri
"""
import numpy as np
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
#from analytic_sol import BumpAnalyticSol #TODO: fix import

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['telemac2d']

        # Boundary conditions:
        self.q_in = 0.3
        self.h_out = 0.5 # not used when flow is critical 

        # Analytic solution:
        self.sol = BumpAnalyticSol(\
            flow='cri', Q=self.q_in, hl=self.h_out, length=20.,\
            bottom_function='exponential', N=801)
        self.sol()

    def set_bumpcri_values(self, cas):
        cas.set('BOUNDARY CONDITIONS FILE', "geo_bump_tor.cli", convert=True)
        cas.set('PRESCRIBED FLOWRATES', [0., self.q_in])
        cas.remove('PRESCRIBED ELEVATIONS')

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # CHAR run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [1, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        self.add_study('char_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # CHAR parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('char_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # N run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [4, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 1)
        self.add_study('n_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # N parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('n_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # PSI run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [5, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 1)
        self.add_study('psi_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # PSI parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('psi_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # PSI LIPS run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [5, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 4)
        self.add_study('lips_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # PSI LIPS parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('lips_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # NERD run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [14, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        cas.set('TREATMENT OF NEGATIVE DEPTHS', 2)
        cas.set('TIDAL FLATS', True)
        cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
        cas.set('MASS-LUMPING ON H', 1.)
        cas.set('SUPG OPTION', [2, 0])
        self.add_study('nerd_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # NERD parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('nerd_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # ERIA run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [15, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        cas.set('TREATMENT OF NEGATIVE DEPTHS', 3)
        cas.set('TIDAL FLATS', True)
        cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
        cas.set('MASS-LUMPING ON H', 1.)
        cas.set('SUPG OPTION', [2, 0])
        self.add_study('eria_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # ERIA parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('eria_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # KIN1 run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 1)
        self.add_study('kin1_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # KIN1 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # KIN2 run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 2)
        self.add_study('kin2_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # KIN2 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # HLLC run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 5)
        self.add_study('hllc_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # HLLC parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # Copy of analytic solution in studies for initialization
        for name, study in self.studies.items():
            self.sol.savetxt(path=self.get_vnv_working_dir(name))

    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('char_seq:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('char_par:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('char_seq:T2DRES', 'char_seq:T2DRES', eps=[1.])

        self.check_epsilons('n_seq:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('n_par:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('n_seq:T2DRES', 'n_seq:T2DRES', eps=[1.])

        self.check_epsilons('psi_seq:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('psi_par:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('psi_seq:T2DRES', 'psi_seq:T2DRES', eps=[1.])

        self.check_epsilons('lips_seq:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('lips_par:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('lips_seq:T2DRES', 'lips_seq:T2DRES', eps=[1.])

        self.check_epsilons('kin1_seq:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('kin1_par:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_seq:T2DRES', eps=[1.])

        self.check_epsilons('kin2_seq:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('kin2_par:T2DRES', 'f2d_bumpcri.slf', eps=[1.])
        self.check_epsilons('kin2_seq:T2DRES', 'kin2_seq:T2DRES', eps=[1.])

        self.check_epsilons('hllc_seq:T2DRES', 'f2d_bumpcri.slf', eps=[1e-8])
        self.check_epsilons('hllc_par:T2DRES', 'f2d_bumpcri.slf', eps=[1e-4])
        self.check_epsilons('hllc_seq:T2DRES', 'hllc_seq:T2DRES', eps=[1e-4])

    def _post(self):
        """
        Post-treatment processes
        """
        import numpy as np
        from scipy.interpolate import interp1d
        from os import path
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d, \
                vnv_plotbar, vnv_plotbar_cpu_times
        from vvytel.vnv_tools import compute_norm, compute_diff
        from data_manip.computation.volume import compute_fv_cell_area

        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        geom, _ = self.get_study_res('hllc_seq:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('hllc_seq:T2DRES')

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # Plot bathy:
        vnv_plot1d_polylines(\
            'BOTTOM',
            res,
            '',
            fig_size=(8, 2),
            record=0,
            fig_name='img/bumpcri_bathy',
            plot_bottom=True)

        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/bumpcri_mesh0',
            annotate_bnd=False,
            plot_mesh=True)

        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/bumpcri_mesh',
            annotate_bnd=True,
            plot_mesh=True)

        #======================================================================
        # FIRST OBSERVATION RESULTS - 1D PLOTS
        #
        # Plot free surface:
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res,
            'Elevation',
            fig_size=(5, 4),
            record=0,
            ref_data=np.c_[self.sol.x, self.sol.E],
            ref_label='Analytic',
            fig_name='img/bumpcri_free_surface_0',
            ylim=[0., 0.6],
            plot_bottom=True)

        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res,
            'Elevation',
            fig_size=(5, 4),
            record=-1,
            ref_data=np.c_[self.sol.x, self.sol.E],
            ref_label='Analytic',
            fig_name='img/bumpcri_free_surface',
            ylim=[0., 0.6],
            plot_bottom=True)

        # Plot froud number:
        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res,
            'Froude number',
            fig_size=(5, 4),
            record=0,
            ref_data=np.c_[self.sol.x, self.sol.F],
            ref_label='Analytic',
            fig_name='img/bumpcri_froude_number_0',
            y_label='Fr',
            plot_bottom=False)

        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res,
            'Froude number',
            fig_size=(5, 4),
            record=-1,
            ref_data=np.c_[self.sol.x, self.sol.F],
            ref_label='Analytic',
            fig_name='img/bumpcri_froude_number',
            y_label='Fr',
            plot_bottom=False)

        #======================================================================
        # FIRST OBSERVATION RESULTS - 2D PLOTS
        #
        # Plot depth:
        vnv_plot2d(\
            'FREE SURFACE',
            res,
            record=-1,
            fig_size=(10, 2),
            fig_name='img/bumpcri_elevation_2d',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True)

        # Plot velocity:
        vnv_plot2d(\
            'VELOCITY',
            res,
            record=-1,
            fig_size=(10, 2),
            fig_name='img/bumpcri_velocity_2d_vector',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            vectors=True, vectors_scale=30,
            grid_resolution=[10, 10])

        # Closing files
        del geom
        del res

        #======================================================================
        # COMPARISON OF NUMERICAL SCHEMES:
        #
        #----------------------------------------------------------------------
        # Computation time:
        vnv_plotbar_cpu_times(\
            self.action_time,
            fig_size=(7, 2.5),
            fig_name='img/bumpcri_cpu_times')

        #----------------------------------------------------------------------
        # Accuracy of free surface (1D slice):
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list,
            res_labels,
            record=-1,
            fig_size=(6, 5),
            ref_data=np.c_[self.sol.x, self.sol.E],
            ref_label='Analytic',
            fig_name='img/bumpcri_elevation_1dslice_comparison_tf',
            markers=True,
            markevery=15,
            plot_bottom=False)

        # Accuracy of velocity (1D slice):
        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res_list,
            res_labels,
            record=-1,
            ref_data=np.c_[self.sol.x, self.sol.F],
            ref_label='Analytic',
            fig_size=(6, 5),
            fig_name='img/bumpcri_froud_1dslice_comparison_tf',
            markers=True,
            markevery=15,
            y_label='Fr',
            plot_bottom=False)

        #----------------------------------------------------------------------
        # Error at t=tf (computed from mass matrix file):
        #
        # Compute errors at final time for each case:
        errLinf_H = [] # error Linf on H at tf
        errLinf_U = [] # error Linf on U at tf
        errL1_H = [] # error L1 on H at tf
        errL1_U = [] # error L1 on U at tf
        errL2_H = [] # error L2 on H at tf
        errL2_U = [] # error L2 on U at tf

        idx = 0
        for name, study in self.studies.items():
            if 'seq' in name:
                # Mass matrix at final time
                massm_file = path.join(
                    self.get_vnv_working_dir(name), 'mass_matrix_tf.txt')
                massm = np.genfromtxt(massm_file)

                # Projection of analytic sol on mesh
                npoint = res_list[idx].npoin2
                H_interp = interp1d(self.sol.x, self.sol.H, kind='linear')
                U_interp = interp1d(self.sol.x, self.sol.U, kind='linear')
                H_ref = np.zeros((npoint))
                U_ref = np.zeros((npoint))
                for i in range(npoint):
                    xi = res_list[idx].tri.x[i]
                    H_ref[i] = H_interp(xi)
                    U_ref[i] = U_interp(xi)

                # Compute diff
                H = res_list[idx].get_data_value('WATER DEPTH', -1)
                U = res_list[idx].get_data_value('VELOCITY U', -1)
                H_diff = compute_diff(H, H_ref, relative=False)
                U_diff = compute_diff(U, U_ref, relative=False)

                # Compute Linf errors:
                errLinf_H.append(compute_norm(H_diff, norm='linf', mass=massm))
                errLinf_U.append(compute_norm(U_diff, norm='linf', mass=massm))
                # Compute L1 errors:
                errL1_H.append(compute_norm(H_diff, norm='l1', mass=massm))
                errL1_U.append(compute_norm(U_diff, norm='l1', mass=massm))
                # Compute L2 errors:
                errL2_H.append(compute_norm(H_diff, norm='l2', mass=massm))
                errL2_U.append(compute_norm(U_diff, norm='l2', mass=massm))

                idx += 1

        errors_H_tf = [errLinf_H, errL1_H, errL2_H]
        errors_U_tf = [errLinf_U, errL1_U, errL2_U]

        # Bar plots of errors at final time
        vnv_plotbar(\
            errors_H_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on H at $t=t_f$',
            y_scale='log',
            fig_name="img/bumpcri_errors_H_tf",
            annotate=True)

        vnv_plotbar(\
            errors_U_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on U at $t=t_f$',
            y_scale='log',
            fig_name="img/bumpcri_errors_U_tf",
            annotate=True)

        #======================================================================
        # Delete results
        for res in res_list:
            del res

#******************************************************************************
class Bottom():
    """
    Bottom elevation class
    """
    def __init__(self, bottom_function, x, xb=10.):
        self.function = bottom_function

        if isinstance(x, list) or isinstance(x, np.ndarray):
            self.zb = np.empty(len(x))
            for i in range(len(x)):
                self.zb[i] = self.compute_bottom(x[i])
        else:
            self.zb = self.compute_bottom(x)

    def compute_bottom(self, x):
        if self.function == 'parabolic':
            return self.parabolic(x)
        elif self.function == 'exponential':
            return self.exponential(x)
        else:
            raise ValueError("Unknown bottom_function")

    def parabolic(self, x, xb=10.):
        if 8. < x < 12.:
            return 0.2 - 0.05*(x - xb)**2
        else:
            return 0.

    def exponential(self, x, xb=10.):
        return 0.25*np.exp(-0.5*(x - xb)**2)


class BumpAnalyticSol():
    """
    Bumps 1d stationary analytical solution

    """
    def __init__(\
            self, flow='sub', Q=8.85, hl=1.8, length=20., width=2.,
            xb=10., bottom_function='parabolic', N=8001):

        # Physical properties
        self.G = 9.81
        self.flow = flow
        self.q0 = Q/width
        self.hc = (self.q0**2/self.G)**(1./3.)
        self.hl = hl

        # 1D mesh
        self.N = N
        self.x = np.linspace(0., length, N)

        # Bottom
        bottom = Bottom(bottom_function, self.x, xb=10.)
        self.zb = bottom.zb
        self.zm = max(self.zb)

        # Solution
        self.H = np.empty((N))
        self.E = np.empty((N))
        self.U = np.empty((N))
        self.V = np.empty((N))
        self.F = np.empty((N))

    def __call__(self):
        if self.flow == 'sub':
            self.compute_subcritical_solution()
        elif self.flow == 'cri':
            self.compute_critical_solution()
        elif self.flow == 'trans':
            self.compute_transcritical_solution()

        for i in range(self.N):
            self.E[i] = self.zb[i] + self.H[i]
            self.U[i] = self.q0 / self.H[i]
            self.V[i] = 0.0
            self.F[i] = abs(self.U[i])/np.sqrt(self.G*self.H[i])

    def compute_subcritical_solution(self):
        coeff = np.zeros((4), dtype='d')

        for i in range(self.N):
            polyd3_coef(coeff, self.zb[i], self.q0, self.hl, 0.)
            hsub = max(np.roots(coeff).real)
            # Subcritical flow for all i:
            self.H[i] = hsub

    def compute_critical_solution(self):
        coeff = np.zeros((4), dtype='d')

        for i in range(self.N):
            polyd3_coef(coeff, self.zb[i], self.q0, self.hc, self.zm)
            hroots = np.sort(np.roots(coeff).real)
            hsub = hroots[2]
            hsup = hroots[1]
            j = np.argmax(self.zb)
            if self.x[i] < self.x[j]:
                # Subcritical flow:
                self.H[i] = hsub
            elif self.x[i] > self.x[j]:
                # Supercritical flow:
                self.H[i] = hsup
            else:
                # Transition:
                self.H[i] = self.hc

    def compute_transcritical_solution(self):
        coeff = np.zeros((4), dtype='d')
        i = self.N - 1
        supercritical = False
        hjump = False
        h1d = np.empty(self.N, dtype='d')

        while i >= 0:
            while supercritical == False:
                j = np.argmax(self.zb)
                if self.x[i] > self.x[j]:
                    polyd3_coef(coeff, self.zb[i], self.q0, self.hl, 0.)
                    hroots = np.sort(np.roots(coeff).real)
                    h1d[i] = hroots[2]
                    if h1d[i] < self.hc:
                        supercritical = True
                i = i - 1
                if i < 0: break

            if supercritical == True:
                ic = np.argmax(self.zb)
                while hjump == False:
                    polyd3_coef(coeff, self.zb[i], self.q0, self.hc, self.zm)
                    hroots = np.sort(np.roots(coeff).real)
                    h1d[ic] = hroots[1]
                    if ic > i:
                        state_L = 2*self.q0**2/h1d[ic] + self.G*h1d[ic]**2
                        state_R = 2*self.q0**2/h1d[ic+1] + self.G*h1d[ic+1]**2
                        if state_L < state_R:
                            hjump = True
                            x_shock = self.x[ic]
                            i = -1
                    ic += 1

        if not hjump:
            raise ValueError("No jump, check flow type")

        for i in range(self.N):
            if self.x[i] > x_shock:
                polyd3_coef(coeff, self.zb[i], self.q0, self.hl, 0.)
                hsub = max(np.roots(coeff).real)
                # Subcritical flow:
                self.H[i] = hsub
            if self.x[i] <= x_shock:
                polyd3_coef(coeff, self.zb[i], self.q0, self.hc, self.zm)
                hroots = np.sort(np.roots(coeff).real)
                hsub = hroots[2]
                hsup = hroots[1]
                j = np.argmax(self.zb)
                if self.x[i] < self.x[j]:
                    # Subcritical flow:
                    self.H[i] = hsub
                elif self.x[i] > self.x[j]:
                    # Supercritical flow:
                    self.H[i] = hsup
                else:
                    # Transition:
                    self.H[i] = self.hc

    def savetxt(self, path=''):
        np.savetxt(path + '/ANALYTIC_SOL.txt',\
            np.c_[self.x, self.H, self.U, self.E, self.F])

    def cleantxt(self, path=''):
        os.system("rm {}".format(path + '/ANALYTIC_SOL.txt'))

def polyd3_coef(coef, z, q, h, zm):
    G = 9.81
    coef[0] = 1.0
    coef[1] = z - q**2/(2.*G*h**2) - h - zm
    coef[2] = 0.0
    coef[3] = q**2/(2.*G)
    return 0
