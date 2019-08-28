# -*- coding: utf-8 -*-
"""
    Python wrapper to the Fortran APIs of Telemac-Mascaret

    Author(s): Fabrice Zaoui, Yoann Audouin, Cedric Goeury, Renaud Barate

    Copyright EDF 2016
"""
import sys
import logging
import shutil
#
from compilation.compil_tools import get_api_ld_flags, get_api_incs_flags,\
                       compile_princi_lib
from telapy.tools.polygon import is_in_polygon
from telapy.tools.decode_range import decode_range
from ctypes import cdll
import os
from os import path
from execution.telemac_cas import TelemacCas
from utils.exceptions import TelemacException
from importlib import reload
from argparse import Namespace
from config import update_config, CFGS
import numpy as np


def get_file_format(key, cas):
    """
    Get the format of the file of keyword key

    @param key Name of the keyword
    @param cas Steering file structure
    @param dico Dictionary structure
    @param frgb Language used
    """

    i = 0
    # Loop on all the keywords in the cas file
    for k in cas.values:
        # The keyword we are searching for contains both the keyword 'keyword'
        # and the word FORMAT (same word in french and english)
        if key in k and ('FORMAT ' in k or ' FORMAT' in k):
            return cas.values[k]
        else:
            i = i + 1
    # By default if there is no format keyword the file is SERAFIN

    return 'SERAFIN'


class ApiModule(object):
    """The Generic Python class for TELEMAC-MASCARET APIs"""
    _api = None
    logger = logging.getLogger(__name__)
    _error = 0

    def __init__(self, name, casfile,
                 user_fortran,
                 dicofile,
                 lang, stdout,
                 comm, recompile,
                 code=None, log_lvl='INFO'):
        """
        Constructor for apiModule

        @param name Name of the code (t2d, sis, ...)
        @param casFile Name of the steering file
        @param user_fortran Name of the user Fortran
        @param dicofile Path to the dictionary
        @param lang Language for ouput (1: French, 2:English)
        @param stdout Where to put the listing
        @param comm MPI communicator
        @param recompile If true recompiling the API
        @param code For coupling
        """

        self.name = name

        self.nbnodes = 0
        self.bottom = None
        self.nelem = 0
        self.tri = None
        self.coordx = None
        self.coordy = None
        self.parallel_run = False

        if log_lvl == 'INFO':
            i_log = logging.INFO
        elif log_lvl == 'DEBUG':
            i_log = logging.DEBUG
        else:
            i_log = logging.CRITICAL
        logging.basicConfig(level=i_log)
        # User Fortran MUST be loaded before apit2d importation
        self.rank = 0
        if comm is not None:
            self.rank = comm.Get_rank()
            self.parallel_run = comm.Get_size() > 1
        if recompile:
            # Compiling API with user_fortran
            if user_fortran is not None:
                # Get configuration information
                options = Namespace()
                options.root_dir = ''
                options.config_name = ''
                options.config_file = ''
                update_config(options)
                cfg = CFGS.configs[CFGS.cfgname]
                # compile user fortran
                if self.rank == 0:
                    self.logger.debug('%d: starting compilation', self.rank)
                    incs_flags = get_api_incs_flags()
                    ld_flags = get_api_ld_flags(False)
                    compile_princi_lib(user_fortran,
                                       incs_flags, ld_flags)

                # Waiting for proc 0 to finish recompiling API
                if comm is not None:
                    comm.barrier()
                    # Load user fortran
                user_fortran_lib_path = \
                    path.join(os.getcwd(),
                              'libuser_fortran'+cfg['sfx_lib'])
                cdll.LoadLibrary(user_fortran_lib_path)

        # Load api
        try:
            if ApiModule._api is None:
                import _api
            else:
                reload(ApiModule._api)
        except Exception as execpt:
            if sys.platform.startswith('linux'):
                ext = 'so'
            elif sys.platform.startswith('darwin'):
                ext = 'dylib'
            elif sys.platform.startswith('win'):
                ext = 'dll'
            else:
                raise TelemacException('Error: unsupported Operating System!')
            raise TelemacException(\
                    'Error: unable to load the dynamic library '
                    + '_api.' + ext
                    + '\nYou can check the environment variable:'
                    + ' PYTHONPATH'
                    + '\n'+str(execpt))
        ApiModule._api = sys.modules['_api']
        self.api_inter = ApiModule._api.api_interface

        # Making links to all the functions
        self.run_set_config = getattr(self.api_inter,
                                      "run_set_config_"+self.name)
        self.run_read_case = getattr(self.api_inter, "run_read_case_"
                                     + self.name)
        self.run_allocation = getattr(self.api_inter,
                                      "run_allocation_"+self.name)
        self.run_init = getattr(self.api_inter, "run_init_"+self.name)
        self.run_timestep = getattr(self.api_inter, "run_timestep_"+self.name)
        self.run_finalize = getattr(self.api_inter, "run_finalize_"+self.name)
        self.api_get_integer = getattr(self.api_inter, "get_integer_"
                                       + self.name)
        self.api_set_integer = getattr(self.api_inter, "set_integer_"
                                       + self.name)
        self.api_get_integer_array = \
            getattr(self.api_inter, "get_integer_array_" + self.name)
        self.api_set_integer_array = \
            getattr(self.api_inter, "set_integer_array_" + self.name)
        self.api_get_double = getattr(self.api_inter, "get_double_"+self.name)
        self.api_set_double = getattr(self.api_inter, "set_double_"+self.name)
        self.api_get_double_array = \
                getattr(self.api_inter, "get_double_array_"+self.name)
        self.api_set_double_array = \
                getattr(self.api_inter, "set_double_array_"+self.name)
        self.api_get_string = getattr(self.api_inter, "get_string_"+self.name)
        self.api_set_string = getattr(self.api_inter, "set_string_"+self.name)
        self.api_get_boolean = getattr(self.api_inter, "get_boolean_"
                                       + self.name)
        self.api_set_boolean = getattr(self.api_inter, "set_boolean_"
                                       + self.name)
        self.get_var_type = getattr(self.api_inter, "get_var_type_"+self.name)
        self.get_var_size = getattr(self.api_inter, "get_var_size_"+self.name)
        self.mod_handle_var = getattr(ApiModule._api,
                                      "api_handle_var_"+self.name)

        self.api_handle_error = ApiModule._api.api_handle_error

        self.lang = lang
        self.stdout = stdout
        self.casfile = casfile
        self.dicofile = dicofile
        if comm is not None:
            self.fcomm = comm.py2f()
            self.ncsize = comm.Get_size()
            self.rank = comm.Get_rank()
        else:
            self.fcomm = 0
            self.ncsize = 0
            self.rank = 0
        self.comm = comm
        self.code = code
        self._initstate = 0

        # Parsing steering file
        self.cas = TelemacCas(self.casfile, self.dicofile)

        # run_set_config
        self.logger.debug('%d: starting run_set_config', self.rank)
        self.my_id, self._error = self.run_set_config(self.stdout,
                                                      self.lang, self.fcomm)
        self.logger.debug('%d: ending run_set_config', self.rank)
        # Running partitionning step if in parallel
        if self.ncsize > 1:
            if self.rank == 0:
                self.partitionning_step()
            self.comm.Barrier()


    @property
    def error(self):
        """Error property
        """
        return self._error

    @error.setter
    def error(self, value):
        """Detect errors

        Overwright attribute setter to detect API errors.
        If :attr:`error` is not set null, an error is raised and the programme
        is terminated.

        :param int value: value to assign
        """
        if value != 0:
            self.logger.error("API error:\n%s", self.get_error_message)
            raise SystemExit
        self._error = 0

    def partitionning_step(self):
        """
           Partition function
           It will run partel for the files that needs to be split
           and copy the others

        """

        # Get the name of the boundary conditions file
        cli_file = self.cas.get('BOUNDARY CONDITIONS FILE')

        # Check if we have some of the optionnals file for

        # Getting sections file if there is one
        sec_file = self.cas.get('SECTIONS INPUT FILE', '')

        # Getting zones file if there is one
        zone_file = self.cas.get('ZONES FILE', '')

        # Getting weirs data file if there is one and we have weirs type == 2
        weirs_file = self.cas.get('WEIRS DATA FILE', '')
        if weirs_file != '':
            type_s = self.cas.get('TYPE OF WEIRS')
            if type_s != 2:
                weirs_file = ' '

        # Partionning the geometry file first
        geo_file = self.cas.get('GEOMETRY FILE')
        geo_fmt = get_file_format('GEOMETRY FILE', self.cas)
        self.logger.debug('%d: starting partel for %s', self.rank, geo_file)
        self._error = self.api_inter.run_partel(\
                         self.my_id, geo_file, cli_file,
                         self.ncsize, 1, geo_fmt,
                         sec_file, zone_file, weirs_file)
        self.logger.debug('%d: starting partel for %s', self.rank, geo_file)
        # Loop on all input files
        for key in self.cas.in_files:
            ffile = self.cas.values[key]
            if ffile != '':
                submit = self.cas.in_files[key].split(';')
                if key == 'GEOMETRY FILE':
                    continue
                elif submit[5][0:7] == 'SELAFIN':
                    file_fmt = get_file_format(key, self.cas)
                    self.logger.debug('%d: starting parres for %s',
                                      self.rank, ffile)
                    self._error = self.api_inter.run_parres(\
                            self.my_id, geo_file, ffile,
                            self.ncsize, geo_fmt, file_fmt)
                    self.logger.debug('%d: endding parres for %s',
                                      self.rank, ffile)
                elif submit[5][0:7] == 'PARAL' or \
                        (key == 'WEIRS DATA FILE' and type_s != 2):
                    for i in range(self.ncsize):
                        shutil.copyfile(ffile,
                                        ffile + '{0:05d}-{1:05d}'
                                        .format(self.ncsize-1, i))

    def concatenation_step(self):
        """
           Concatenate function
           will run gretel for the file where it is necessary
        """

        # Get name of the geometry file
        geo_file = self.cas.get('GEOMETRY FILE')
        geo_fmt = get_file_format('GEOMETRY FILE', self.cas)

        # Get the name of the boundary conditions file
        cli_file = self.cas.get('BOUNDARY CONDITIONS FILE')

        # Get name of the geometry file
        nplan = self.cas.get('NUMBER OF HORIZONTAL LEVELS', default=0)

        # Loop on all output files
        for key in self.cas.out_files:
            ffile = self.cas.values[key]
            if ffile != '':
                submit = self.cas.out_files[key].split(';')
                if submit[5][0:7] == 'SELAFIN':
                    file_fmt = get_file_format(key, self.cas)
                    self.logger.debug('%d: starting gretel for %s',
                                      self.rank, ffile)
                    part_file = ffile+'{0:05d}-{1:05d}'.format(self.ncsize-1, 0)
                    if not path.exists(part_file):
                        self.logger.info(\
                            "File {} does not seems to exist not"\
                            "running gretel\n If you changed the name of the"\
                            "file from the one in the steering file you need"\
                            "to run gretel manually")
                        continue
                    self._error = self.api_inter.run_gretel(\
                            self.my_id, geo_file, geo_fmt,
                            cli_file, ffile, file_fmt,
                            self.ncsize, nplan)
                    self.logger.debug('%d: endding gretel for %s',
                                      self.rank, ffile)
                if submit[5][0:6] == 'DELWAQ':
                    print("Delwaq is not handled yet merging"
                          "will have to be done by hand.")

    def set_case(self, init=True):
        """
           Read the steering file and run allocation

           @param init (boolean) If true calling p_init
        """
        # run_read_case
        self.logger.debug('%d: beginning run_read_case', self.rank)
        if self.name == "sis":
            self._error = self.run_read_case(self.my_id, self.code,
                                             self.casfile, self.dicofile,
                                             init)
        else:
            self._error = self.run_read_case(self.my_id, self.casfile,
                                             self.dicofile, init)
        self.logger.debug('%d: ending run_read_case', self.rank)
        self._initstate = 1

        return self._error

    def init_state_default(self):
        """
        Initialize the state of the model Telemac 2D with the values of
        disharges and water levels as indicated by the steering file
        """
        if self._initstate == 0:
            raise TelemacException(\
                    'Error: the object is not a Telemac 2D instance')
        else:
            # run_allocation
            self.logger.debug('%d: beginning run_allocation', self.rank)
            self._error = self.run_allocation(self.my_id)
            self.logger.debug('%d: ending run_allocation', self.rank)
            self.logger.debug('%d: beginning run_init', self.rank)
            self._error = self.run_init(self.my_id)
            self.logger.debug('%d: ending run_init', self.rank)
            if self._error == 0:
                self._initstate = 2

    def run_one_time_step(self):
        """
        Run one time step
        """
        if self._initstate != 2:
            raise TelemacException('Error: the initial conditions are not set\n\
                       Use init_state_default first')
        else:
            self._error = self.run_timestep(self.my_id)

    def run_all_time_steps(self):
        """
        Run all the time steps

        @retuns the number of computed time steps
        """

        ntimesteps = self.get("MODEL.NTIMESTEPS")
        self.logger.debug('%d: Number of timestep %d', self.rank, ntimesteps)
        # Check if we are in finite volume
        equation = ''
        try:
            equation = self.get("MODEL.EQUATION")
            if 'VF' in equation or 'FV' in equation:
                ntimesteps = 1
        except Exception:
            # If the keyword does not exist then the module does
            # not have finite volumes
            pass

        for itime in range(ntimesteps):
            self.logger.debug('%d: beginning run_timestep %d', self.rank,
                              itime)
            self._error = self.run_one_time_step()
            self.logger.debug('%d: ending run_timestep %d', self.rank, itime)

        return ntimesteps

    def get_mesh(self):
        """
        Get the 2D mesh of triangular cells

        @returns X, Y coordinates and connectivity
        """
        self.nbnodes = self.get('MODEL.NPOIN')
        self.coordx = self.get_array('MODEL.X')
        self.coordy = self.get_array('MODEL.Y')
        self.nelem = self.get('MODEL.NELEM')
        self.tri = self.get_array('MODEL.IKLE').T - 1

        return self.coordx, self.coordy, self.tri

    def get_node(self, xval, yval):
        """
        Get the nearest node number for the coordinates (xval, yval).

        @param xval X coordinate.
        @param yval Y coordinate.

        @returns An integer value from 0 to (nbnode-1).
        """
        #@todo replace using new toolbox
        from scipy.spatial.distance import cdist
        pt_val = np.array([[xval, yval]])
        if self.coordx is None:
            _, _, _ = self.get_mesh()
        xy_array = np.array([self.coordx, self.coordy]).transpose()
        return np.argmin(cdist(xy_array, pt_val))

    def get_elem(self, xval, yval):
        """
        Get the triangle where the point (xval, yval) is

        @param xval X coordinate
        @param yval Y coordinate

        @return integer value from 0 to (nbtriangle-1)
                 (-1 if no triangle found)
        """
        #@todo replace using new toolbox
        if self.coordx is None:
            _, _, _ = self.get_mesh()
        xy_array = np.array([self.coordx, self.coordy]).transpose()
        pt_val = np.array([xval, yval])
        dimtri = self.tri.shape
        triangle = -1
        for i in range(dimtri[0]):
            pt1 = xy_array[self.tri[i, 0]]
            pt2 = xy_array[self.tri[i, 1]]
            pt3 = xy_array[self.tri[i, 2]]
            vec0 = pt3 - pt1
            vec1 = pt2 - pt1
            vec2 = pt_val - pt1
            dot00 = np.dot(vec0, vec0)
            dot01 = np.dot(vec0, vec1)
            dot02 = np.dot(vec0, vec2)
            dot11 = np.dot(vec1, vec1)
            dot12 = np.dot(vec1, vec2)
            invden = 1. / (dot00 * dot11 - dot01 * dot01)
            dist_u = (dot11 * dot02 - dot01 * dot12) * invden
            dist_v = (dot00 * dot12 - dot01 * dot02) * invden
            if (dist_u >= 0) & (dist_v >= 0) & (dist_u + dist_v < 1):
                triangle = i
                break
        return triangle

    def show_mesh(self, show=True, visu2d=True):
        """
        Show the 2D mesh with topography

        @param show Display the graph (Default True)
        @param visu2d 2d display (Default True)

        @retuns the figure object
        """
        #@todo replace using new plots
        import matplotlib.pyplot as plt
        import matplotlib.cm as cm
        from mpl_toolkits.mplot3d import Axes3D
        if self.coordx is None:
            self.get_mesh()
        if self.bottom is None:
            self.bottom = self.get_array('MODEL.BOTTOMELEVATION')
        fig = plt.figure()
        if visu2d:
            plt.tripcolor(self.coordx, self.coordy, self.tri, self.bottom,
                          shading='flat', edgecolor='w', cmap=cm.terrain)
            plt.colorbar()
        else:
            axe = Axes3D(fig)
            axe.plot_trisurf(self.coordx, self.coordy, self.tri, self.bottom,
                             cmap=cm.terrain, linewidth=0.1)
        plt.title('2D mesh (%d triangles, %d nodes) \
                   with the bottom elevation (m)' % (self.nelem, self.nbnodes))
        plt.xlabel('X-coordinate (m)')
        plt.ylabel('Y-coordinate (m)')
        if show:
            plt.show()
        return fig

    def list_variables(self):
        """
        List the names and the meaning of available variables and parameters

        @retuns two lists of strings (name and meaning)
        """
        nb_var = getattr(self.mod_handle_var, "nb_var_"+self.name)
        var_len = getattr(self.mod_handle_var, self.name+"_var_len")
        info_len = getattr(self.mod_handle_var, self.name+"_info_len")

        vnames = []
        vinfo = []
        varnames = []
        varinfo = []
        # Reordering string array for variable names
        tmp = getattr(self.mod_handle_var, "vname_"+self.name)
        for j in range(var_len):
            for i in range(nb_var):
                varnames.append(tmp[i][j])
        # Reordering string array for variable info
        tmp = getattr(self.mod_handle_var, "vinfo_"+self.name)
        for j in range(info_len):
            for i in range(nb_var):
                varinfo.append(tmp[i][j])
        # Extracting name and info into a list
        for i in range(nb_var):
            vnames.append(b''.join(varnames[i*var_len:(i+1)*var_len])\
                             .strip().decode('utf-8'))
            vinfo.append(b''.join(varinfo[i*info_len:(i+1)*info_len])\
                            .strip().decode('utf-8'))
        return vnames, vinfo

    def get(self, varname, i=-1, j=-1, k=-1, global_num=True):
        """
        Get the value of a variable of Telemac 2D

        @param varname Name of the variable
        @param i index on first dimension
        @param j index on second dimension
        @param k index on third dimension
        @param global_num Are the index on local/global numbering

        @retuns variable value
        """
        value = None
        vartype, _, ndim, _, _, _, _, _, self._error = \
            self.get_var_type(varname.encode('utf-8'))
        dim1, dim2, dim3, self._error = \
                self.get_var_size(self.my_id, varname.encode('utf-8'))
        # If we have a string the first dimension is the size of the string
        if b"STRING" in vartype:
            ndim -= 1
            dim0 = dim1
            dim1 = dim2
            dim2 = dim3

        # Checking that index are within bound
        # Only doing that for local numbering and sequential runs
        # TODO: See how to hand that so that it works for both
        if not global_num or not self.parallel_run:
            if ndim >= 1:
                if not 0 <= i < dim1:
                    raise TelemacException(\
                            "i=%i is not within [0,%i]" % (i, dim1))

            if ndim >= 2:
                if not 0 <= j < dim2:
                    raise TelemacException(\
                            "j=%i is not within [0,%i]" % (j, dim2))

            if ndim == 3:
                if not 0 <= k < dim3:
                    raise TelemacException(\
                            "k=%i is not within [0,%i]" % (k, dim3))

        # Getting value depending on type
        if b"DOUBLE" in vartype:
            value, self._error = \
                 self.api_get_double(self.my_id, varname, global_num,
                                     i+1, j+1, k+1)
        elif b"INTEGER" in vartype:
            value, self._error = self.api_get_integer(self.my_id, varname,
                                                      i+1, j+1, k+1)
        elif b"STRING" in vartype:
            tmp_value, self._error = self.api_get_string(self.my_id, varname,
                                                         dim0, i+1, j+1)
            value = tmp_value.tostring().strip()
        elif b"BOOLEAN" in vartype:
            value, self._error = self.api_get_boolean(self.my_id, varname,
                                                      i+1, j+1, k+1)
        else:
            raise TelemacException(\
                    "Unknown data type %s for %s" % (vartype, varname))

        return value

    def set(self, varname, value, i=-1, j=-1, k=-1, global_num=True):
        """
        Get the value of a variable of Telemac 2D

        @param varname Name of the variable
        @param i index on first dimension
        @param j index on second dimension
        @param k index on third dimension
        @param global_num Are the index on local/global numbering

        @retuns variable value
        """
        vartype, readonly, ndim, _, _, _, _, _, self._error = \
            self.get_var_type(varname)

        dim1, dim2, dim3, self._error = self.get_var_size(self.my_id, varname)

        # In case of a string bypassing check on first dimension (it is the
        # size of the string)
        # If we have a string the first dimension is the size of the string
        if b"STRING" in vartype:
            ndim -= 1
            dim0 = dim1
            dim1 = dim2
            dim2 = dim3

        # Check readonly value
        if readonly:
            raise TelemacException(\
                    "Variable %s is readonly" % varname)

        # Checking that index are within bound
        # Only doing that for local numbering and sequential runs
        # TODO: See how to hand that so that it works for both
        if not global_num or not self.parallel_run:
            if ndim >= 1:
                if not 0 <= i < dim1:
                    raise TelemacException(\
                            "i=%i is not within [0,%i]" % (i, dim1))

            if ndim >= 2:
                if not 0 <= j < dim2:
                    raise TelemacException(\
                            "j=%i is not within [0,%i]" % (j, dim2))

            if ndim == 3:
                if not 0 <= k < dim3:
                    raise TelemacException(\
                            "k=%i is not within [0,%i]" % (k, dim3))

        # Getting value depending on type
        if b"DOUBLE" in vartype:
            self._error = \
               self.api_set_double(self.my_id, varname, value, global_num,
                                   i+1, j+1, k+1)
        elif b"INTEGER" in vartype:
            self._error = self.api_set_integer(self.my_id, varname, value,
                                               i+1, j+1, k+1)
        elif b"STRING" in vartype:
            # Filling value with spaces to reach dim1
            tmp_str = value + ' '*(dim0 - len(value))
            self._error = self.api_set_string(self.my_id, varname, tmp_str,
                                              i+1, j+1)
        elif b"BOOLEAN" in vartype:
            self._error = self.api_set_boolean(self.my_id, varname, value,
                                               i+1, j+1, k+1)
        else:
            raise TelemacException(\
                    "Unknown data type %s for %s" % (vartype, varname))

    def get_array(self, varname, block_index=0):
        """
        Retrieves all the values from a variable into a numpy array

        @param Name of the variable

        @returns A numpy array containing the values
        """
        var_type, _, ndim, _, _, _, _, _, self._error = \
                self.get_var_type(varname)
        dim1, dim2, dim3, self._error = self.get_var_size(self.my_id, varname)
        if not(b"DOUBLE" in var_type or b"INTEGER" in var_type):
            raise TelemacException(\
                    "get_array only works for integer and double"+\
                    "arrays not for {}".format(var_type))

        if ndim == 1:
            # Initialising array
            if b"DOUBLE" in var_type:
                res = np.zeros((dim1), dtype=np.float64)
                self.api_get_double_array(self.my_id, varname, res, dim1)
            else:
                res = np.zeros((dim1), dtype=np.int32)
                self.api_get_integer_array(self.my_id, varname, res, dim1)
        elif ndim == 2:
            if b"DOUBLE_BLOCK" in var_type:
                res = np.zeros(dim2, dtype=np.float64)
                self.api_get_double_array(self.my_id, varname, res, dim2,
                                          block_index=block_index+1)
            elif b"DOUBLE" in var_type:
                res = np.zeros((dim1*dim2), dtype=np.float64)
                self.api_get_double_array(self.my_id, varname, res, dim1*dim2)
            else:
                res = np.zeros((dim1*dim2), dtype=np.int32)
                self.api_get_integer_array(self.my_id, varname, res, dim1*dim2)
            res = res.reshape((dim1, dim2))
        elif ndim == 3:
            if b"DOUBLE" in var_type:
                res = np.zeros((dim1*dim2*dim3), dtype=np.float64)
                self.api_get_double_array(self.my_id, varname, res,
                                          dim1*dim2*dim3)
            else:
                res = np.zeros((dim1*dim2*dim3), dtype=np.int32)
                self.api_get_integer_array(self.my_id, varname, res,
                                           dim1*dim2*dim3)
            res = res.reshape((dim1, dim2, dim3))
        else:
            raise TelemacException(\
                    "Getting array of a 0d variable!!\n\
                    Use basic get instead")

        return res

    def set_array(self, varname, values):
        """
        Retrieves all the values from a variable into a numpy array

        @param varname Name of the variable
        @param values Value for each index of the array
        """
        var_type, _, ndim, _, _, _, _, _, self._error = \
                self.get_var_type(varname)
        dim1, dim2, dim3, self._error = self.get_var_size(self.my_id, varname)
        if not(b"DOUBLE" in var_type or b"INTEGER" in var_type):
            raise TelemacException(\
                    "get_array only works for integer and double"+\
                    "arrays not for {}".format(var_type))

        if ndim == 1:
            # Checking shape
            if values.shape != (dim1,):
                raise TelemacException(\
                        "Error in shape of values is %s should be %s"
                        % (str(values.shape), str((dim1,))))
            if b'DOUBLE' in var_type:
                self.api_set_double_array(self.my_id, varname, values, dim1)
            else:
                self.api_set_integer_array(self.my_id, varname, values, dim1)
        elif ndim == 2:
            # Checking shape
            if values.shape != (dim1, dim2):
                raise TelemacException(\
                        "Error in shape of values is %s should be %s"
                        % (str(values.shape), str((dim1, dim2))))
            tmp = values.reshape(dim1*dim2)
            if b'DOUBLE' in var_type:
                self.api_set_double_array(self.my_id, varname, tmp, dim1*dim2)
            else:
                self.api_set_integer_array(self.my_id, varname, tmp, dim1*dim2)
        elif ndim == 3:
            # Checking shape
            if values.shape != (dim1, dim2, dim3):
                raise TelemacException(\
                        "Error in shape of values is %s should be %s"
                        % (str(values.shape), str((dim1, dim2, dim3))))
            tmp = values.reshape(dim1*dim2*dim3)
            if b'DOUBLE' in var_type:
                self.api_set_double_array(self.my_id, varname, tmp,
                                          dim1*dim2*dim3)
            else:
                self.api_set_integer_array(self.my_id, varname, tmp,
                                           dim1*dim2*dim3)
        else:
            raise TelemacException(\
                    "Setting array of a 0d variable!!\n\
                    Use basic set instead")

    def get_on_polygon(self, varname, poly):
        """
        Retrieves values for point within the polygon poly
        Warning this works only on array that are of size NPOIN

        @param varname Name of the variable
        @param poly List of tuple containing the x and y
                    on the points of the polygon

        @retuns A numpy array containing all the values
        """
        if self.coordx is None:
            _, _, _ = self.get_mesh()

        points_in_poly = []

        # Detect the points that are within the polygon
        for i, pt_x, pt_y in zip(range(self.nbnodes),
                                 self.coordx,
                                 self.coordy):
            if is_in_polygon(pt_x, pt_y, poly):
                points_in_poly.append(i)

        if points_in_poly == []:
            raise TelemacException("No points are within the polygon")
        # Build the numpy array
        res = np.full((len(points_in_poly)),
                      self.get(varname, i=points_in_poly[0]))
        # Looping on all the points that are within the polygon
        values = self.get_array(varname)
        for i, point in enumerate(points_in_poly):
            res[i] = values[point]

        return res

    def set_on_polygon(self, varname, value, poly):
        """
        Set varname to value on all points that are within the polygon poly
        Warning this works only on array that are of size NPOIN

        @param varname Name of the variable
        @param value The value to set
        @param poly List of tuple containing the x and y
                    on the points of the polygon
        """
        if self.coordx is None:
            _, _, _ = self.get_mesh()

        for i, pt_x, pt_y in zip(range(self.nbnodes),
                                 self.coordx,
                                 self.coordy):
            if is_in_polygon(pt_x, pt_y, poly):
                self.set(varname, value, i=i,
                         global_num=not self.parallel_run)

    def get_on_range(self, varname, irange, jrange="", krange=""):
        """
        Retrieves the values of the variable on the range given as argument

        @param varname Name of the variable
        @param irange Range for index i (first dimension)
        @param jrange Range for index j (second dimension)
        @param krange Range for index k (third dimension)

        @retruns A numpy array containing the values
        """
        _, _, ndim, _, _, _, _, _, self._error = self.get_var_type(varname)

        if ndim == 1:
            # Checking range
            if irange == "":
                raise TelemacException(\
                        "Missing range for first dimension")
            # Decoding ranges
            my_irange = decode_range(irange)

            # Initialising array
            res = np.full((len(my_irange)), self.get(varname, i=0))

            # Looping on all indexes
            for i, val_i in enumerate(my_irange):
                res[i] = self.get(varname, i=val_i)
        elif ndim == 2:
            # Checking range
            if irange == "":
                raise TelemacException("Missing range for first dimension")
            if jrange == "":
                raise TelemacException("Missing range for second dimension")
            # Decoding ranges
            my_irange = decode_range(irange)
            my_jrange = decode_range(jrange)

            # Initialising array
            res = np.full((len(my_irange), len(my_jrange)),
                          self.get(varname))

            # Looping on all indexes
            for i, val_i in enumerate(my_irange):
                for j, val_j in enumerate(my_jrange):
                    res[i, j] = self.get(varname, i=val_i, j=val_j)
        elif ndim == 3:
            # Checking range
            if irange == "":
                raise TelemacException("Missing range for first dimension")
            if jrange == "":
                raise TelemacException("Missing range for second dimension")
            if krange == "":
                raise TelemacException("Missing range for third dimension")
            # Decoding ranges
            my_irange = decode_range(irange)
            my_jrange = decode_range(jrange)
            my_krange = decode_range(krange)

            # Initialising array
            res = np.full((len(my_irange), len(my_jrange), len(my_krange)),
                          self.get(varname))

            # Looping on all indexes
            for i, val_i in enumerate(my_irange):
                for j, val_j in enumerate(my_jrange):
                    for k, val_k in enumerate(my_krange):
                        res[i, j, k] = self.get(varname, i=val_i,
                                                j=val_j, k=val_k)
        else:
            raise TelemacException(\
                    "Getting range of a 0d variable!!\n\
                    Use basic set instead")

        return res

    def set_on_range(self, varname, value, irange, jrange="", krange=""):
        """
        Retrieves the values of the variable on the range given as argument

        @param varname Name of the variable
        @param value  Value to apply on the ranges
        @param irange Range for index i (first dimension)
        @param jrange Range for index j (second dimension)
        @param krange Range for index k (third dimension)
        """
        _, _, ndim, _, _, _, _, _, self._error = self.get_var_type(varname)

        if ndim == 1:
            # Checking range
            if irange == "":
                raise TelemacException("Missing range for first dimension")
            # Decoding ranges
            my_irange = decode_range(irange)

            # Looping on all indexes
            for val_i in my_irange:
                self.set(varname, value, i=val_i)

        elif ndim == 2:
            # Checking range
            if irange == "":
                raise TelemacException("Missing range for first dimension")
            if jrange == "":
                raise TelemacException("Missing range for second dimension")
            # Decoding ranges
            my_irange = decode_range(irange)
            my_jrange = decode_range(jrange)

            # Looping on all indexes
            for val_i in my_irange:
                for val_j in my_jrange:
                    self.set(varname, value, i=val_i, j=val_j)

        elif ndim == 3:
            # Checking range
            if irange == "":
                raise TelemacException("Missing range for first dimension")
            if jrange == "":
                raise TelemacException("Missing range for second dimension")
            if krange == "":
                raise TelemacException("Missing range for third dimension")
            # Decoding ranges
            my_irange = decode_range(irange)
            my_jrange = decode_range(jrange)
            my_krange = decode_range(krange)

            # Looping on all indexes
            for val_i in my_irange:
                for val_j in my_jrange:
                    for val_k in my_krange:
                        self.set(varname, value, i=val_i,
                                 j=val_j, k=val_k)
        else:
            raise TelemacException("Setting range of a 0d variable!!\n\
                             Use basic set instead")

    def get_error_message(self):
        """
        Get the error message from the Fortran sources of Telemac 2D

        @retuns character string of the error message
        """
        return self.api_handle_error.err_mess.tostring().strip()

    def finalize(self):
        """
        Delete the Telemac 2D instance

        @retuns error code
        """
        self.logger.debug('%d: beginning run_finalize', self.rank)
        self._error = self.run_finalize(self.my_id)
        self.logger.debug('%d: ending run_finalize', self.rank)

        # Running merging step if in parallel
        if self.ncsize > 1:
            if self.rank == 0:
                self.concatenation_step()
            self.comm.Barrier()

    def generate_var_info(self):
        """
        Returns a dictionary containg specific informations for each variable

        @returns the dictionary
        """

        var_info = {}

        vnames, vinfo = self.list_variables()

        for varname, varinfo in zip(vnames, vinfo):
            vartype, _, _, _, _, _, get_pos, set_pos, self._error = \
                    self.get_var_type(varname)
            var_info[varname.rstrip()] = {'get_pos': get_pos,
                                          'set_pos': set_pos,
                                          'info': varinfo.rstrip(),
                                          'type': vartype.rstrip()}

        return var_info

    def dump_var_info(self):
        """
        Print Missing information for each variable
        """
        var_info = {}

        vnames, vinfo = self.list_variables()

        for varname, _ in zip(vnames, vinfo):
            print("For Variable "+varname.strip())
            vartype, _, ndim, _, _, _, get_pos, set_pos, self._error = \
                self.get_var_type(varname)
            dim1, dim2, dim3, self._error = \
                self.get_var_size(self.my_id, varname)

            if get_pos == -1:
                print(" - Missing get position")
            if set_pos == -1:
                print(" - Missing set position")
            if vartype == '':
                print(" - Missing vartype")
            if ndim != 0:
                if ndim >= 1:
                    if dim1 == 0:
                        print(" - Missing dim1")
                if ndim >= 2:
                    if dim2 == 0:
                        print(" - Missing dim2")
                if ndim >= 3:
                    if dim3 == 0:
                        print(" - Missing dim3")

        return var_info