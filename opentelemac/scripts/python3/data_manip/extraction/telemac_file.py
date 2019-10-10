"""
Contains the class TelemacFile
"""
from utils.exceptions import TelemacException
from telapy.api.hermes import HermesFile
from os import path
import math
import numpy as np
from scipy.spatial import cKDTree
import matplotlib.tri as mtri
from data_manip.extraction.linspace import \
        linspace_poly, curvilinear_abscissa

class TelemacFile(HermesFile):
    """
    Class to extract data from a TelemacFile
    """

    def __init__(self, file_name, bnd_file=None, log_lvl='INFO'):
        """
        Initialisation of a file reader

        @param file_name (str) Name of the mesh file
        @param bnd_file (str) Name of the boundary file (default None)
        @parma log_lvl (str) Level of log information
        """

        # Identifying format from file extension if not given
        _, ext = path.splitext(file_name)
        if ext == '.med':
            fformat = 'MED'
        else:
            fformat = 'SERAFIN'
        HermesFile.__init__(self, file_name,
                            boundary_file=bnd_file,
                            fformat=fformat,
                            log_lvl=log_lvl)

        self._title = None
        self._nvar = None
        self._varnames = None
        self._varunits = None
        self._datetime = None
        self._nelem3 = None
        self._npoin3 = None
        self._ndp3 = None
        self._nplan = None
        self._nelem2 = None
        self._npoin2 = None
        self._ndp2 = None
        self._meshx = None
        self._meshy = None
        self._ikle3 = None
        self._ikle2 = None
        self._ipob3 = None
        self._ipob2 = None
        self._ntimestep = None
        self._tri = None
        self._times = None
        self.tree = None
        self.neighbours = None
        self.edges = None

    @property
    def title(self):
        """
        Returns title value
        """
        if self._title is None:
            self._title = self.get_mesh_title()

        return self._title

    @title.setter
    def title(self, title):
        """ Setting title value """
        self._title = title

    @property
    def nvar(self):
        """
        Returns nvar value
        """
        if self._nvar is None:
            self._nvar = self.get_data_nvar()

        return self._nvar

    @nvar.setter
    def nvar(self, value):
        """ Setting nvar value """
        self._nvar = value

    @property
    def varnames(self):
        """
        Returns varnames value
        """
        if self._varnames is None:
            tmp_varnames, tmp_varunits = self.get_data_var_list()
            # Removing spaces at the end of the names/units
            self._varnames = [var.strip() for var in tmp_varnames]
            self._varunits = [var.strip() for var in tmp_varunits]

        return self._varnames

    @varnames.setter
    def varnames(self, value):
        """ Setting varnames value """
        self._varnames = value

    @property
    def varunits(self):
        """
        Returns varnames value
        """
        if self._varunits is None:
            self._varnames, self._varunits = self.get_data_var_list()

        return self._varunits

    @varunits.setter
    def varunits(self, value):
        """ Setting varunits value """
        self._varunits = value

    @property
    def datetime(self):
        """
        Returns datetime value
        """
        if self._datetime is None:
            self._datetime = self.get_mesh_date()

        return self._datetime

    @datetime.setter
    def datetime(self, value):
        """ Setting datetime value """
        self._datetime = value

    @property
    def nelem3(self):
        """
        Returns number of 3d elements
        """
        if self._nelem3 is None:
            self._nelem3 = self.get_mesh_nelem()

        return self._nelem3

    @property
    def npoin3(self):
        """
        Returns number of 3d points
        """
        if self._npoin3 is None:
            self._npoin3 = self.get_mesh_npoin()

        return self._npoin3

    @property
    def ndp3(self):
        """
        Returns number of points in a 3d element
        """
        if self._ndp3 is None:
            self._ndp3 = self.get_mesh_npoin_per_element()

        return self._ndp3

    @property
    def nplan(self):
        """
        Returns the number of horizontal planes
        """
        if self._nplan is None:
            self._nplan = max(1, self.get_mesh_nplan())

        return self._nplan

    @property
    def nelem2(self):
        """
        Returns the number of 2d elements
        """
        if self._nelem2 is None:
            if self.nplan > 1:
                self._nelem2 = self.nelem3//(self._nplan-1)
            else:
                self._nelem2 = self.nelem3

        return self._nelem2

    @property
    def npoin2(self):
        """
        Returns the number of 2d points
        """
        if self._npoin2 is None:
            if self.nplan > 1:
                self._npoin2 = self.npoin3//self.nplan
            else:
                self._npoin2 = self.npoin3

        return self._npoin2

    @property
    def ndp2(self):
        """
        Returns the number of points per 2d element
        """
        if self._ndp2 is None:
            if self.nplan > 1:
                self._ndp2 = self.ndp3 // 2
            else:
                self._ndp2 = self.ndp3

        return self._ndp2

    @property
    def ikle3(self):
        """
        Returns the connectivity table for 3d elements
        """
        if self._ikle3 is None:
            self._ikle3 = self.get_mesh_connectivity()

        return self._ikle3

    @property
    def ikle2(self):
        """
        Returns the connectivity table for 2d elements
        """
        if self._ikle2 is None:
            if self.nplan > 1:
                self._ikle2 = np.compress(np.repeat([True, False], self.ndp2),
                                          self.ikle3[0:self.nelem2], axis=1)
            else:
                self._ikle2 = self.ikle3

        return self._ikle2

    @property
    def ipob3(self):
        """
        Returns the ipobo array for 3d points
        """
        if self._ipob3 is None:
            self._ipob3 = self.get_bnd_ipobo()

        return self._ipob3

    @property
    def ipob2(self):
        """
        Returns the ipobo array for 2d points
        """
        if self._ipob2 is None:
            if self.nplan > 1:
                self._ipob2 = self.ipob3[0:self._npoin2]
            else:
                self._ipob2 = self.ipob3

        return self._ipob2

    @property
    def meshx(self):
        """
        Returns x coordinates value
        """
        if self._meshx is None:
            self._meshx = self.get_mesh_coord(1)

        return self._meshx

    @property
    def meshy(self):
        """
        Returns y coordinates value
        """
        if self._meshy is None:
            self._meshy = self.get_mesh_coord(2)

        return self._meshy

    @property
    def ntimestep(self):
        """
        Returns the number of records in the file
        """
        if self._ntimestep is None:
            self._ntimestep = self.get_data_ntimestep()

        return self._ntimestep

    @property
    def tri(self):
        """
        Returns matplotlib triangulation of the 2d elements
        """
        if self._tri is None:
            self._tri = mtri.Triangulation(self.meshx[:self.npoin2],
                                           self.meshy[:self.npoin2],
                                           self.ikle2)

        return self._tri


    @property
    def times(self):
        """
        Returns a list of the times in the file
        """
        if self._times is None:
            self._times = np.zeros((self.ntimestep), dtype=np.float64)
            for record in range(self.ntimestep):
                self._times[record] = self.get_data_time(record)

        return self._times

    def set_kd_tree(self, reset=False):
        """
        Builds a KDTree (improves search of neighbours)

        @param reset (boolean) Force reset of tree
        """
        if reset or self.tree is None:
            isoxy = np.column_stack((np.sum(self.meshx[self.ikle2],
                                            axis=1)/3.0,
                                     np.sum(self.meshy[self.ikle2],
                                            axis=1)/3.0))
            self.tree = cKDTree(isoxy)

    def set_mpl_tri(self, reset=False):
        """
        Build neighbours from matplotlib

        @param reset (boolean) Force computing neighbours
        """
        if reset or self.neighbours is None or self.edges is None:
            #from matplotlib.tri import Triangulation
            mpltri = self.tri.get_cpp_triangulation()
            self.neighbours = mpltri.get_neighbors()
            self.edges = mpltri.get_edges()

    #############################################
    #
    # Tools
    #
    #############################################

    def get_closest_record(self, time):
        """
        Get the record closest to a given time

        @param time (float) Time for which we seek the record

        @returns (int) The record
        """
        dist = 10000000
        record = -1
        # TODO: optimise a bit
        for i, itime in enumerate(self.times):
            if (abs(time-itime)) < dist:
                dist = abs(time-itime)
                record = i

        return record

    def get_closest_node(self, point, plane=None):
        """
        Return the closest node to a given point If the mesh is 3d it will
        search for record 0

        @param point (np.array) coordinates of the point
        @param plane (int) (Only for a 3d mesh) If given will look for the
        closest node on the given plane point should be [x, y]

        @returns (int) Number of the node
        """
        node = -1
        best_dist = 1.e100

        if len(point) == 3:
            # Seaching in 3d mesh
            meshz = self.get_data_value('ELEVATION Z', 0)
            for i in range(self.npoin3):
                dist = (self.meshx[i]- point[0])**2 + \
                       (self.meshy[i]- point[1])**2 + \
                       (meshz[i]- point[2])**2

                if dist < best_dist:
                    best_dist = dist
                    node = i

        elif len(point) == 2:
            if plane is None:
                # Searching in a 2d mesh
                for i in range(self.npoin2):
                    dist = (self.meshx[i]- point[0])**2 + \
                           (self.meshy[i]- point[1])**2

                    if dist < best_dist:
                        best_dist = dist
                        node = i
            else:
                # Searching in a given plane for the closest node
                for i in range(plane*self.npoin2, (plane+1)*self.npoin2):
                    dist = (self.meshx[i]- point[0])**2 + \
                           (self.meshy[i]- point[1])**2

                    if dist < best_dist:
                        best_dist = dist
                        node = i

        else:
            raise TelemacException(\
                "Point should be 2d or 3d: {}".format(point))

        return node

    def discretize_polyline(self, polyline):
        """
        Will return the number of point for each polyline segment taking the
        minimum mesh resolution as step

        @param polyline (list) List of points of the polyline

        @returns (list) List of discretisation for each segment
        """
        discret = []
        # ~~> Calculate the minimum mesh resolution
        dxy = math.sqrt(min(np.square(np.sum(np.fabs(\
                              self.meshx[self.ikle2]-\
                              self.meshx[np.roll(self.ikle2, 1)]),
                                             axis=1)/3.0) + \
                             np.square(np.sum(np.fabs(\
                              self.meshy[self.ikle2]-\
                              self.meshy[np.roll(self.ikle2, 1)]),
                                              axis=1)/3.0)))
        for i in range(len(polyline)-1):
            dio = math.sqrt(sum(np.square(np.array(polyline[i])-np.array(polyline[i+1]))))
            discret.append(int(dio/dxy))

        return discret

    #############################################
    #
    # Computing boundary information
    #
    #############################################
    def get_bnd_info(self):
        """
        Get boundary condition type of nodes
        """
        nbor = self.get_bnd_numbering()
        liubor, lihbor, livbor, _, _, _, _, \
            litbor, _, _, _, _ = self.get_bnd_value()

        return (nbor, liubor, lihbor, livbor, litbor)

    def get_liq_bnd_info(self):
        """
        Returns info on the liquid boundaries

        @returns
        """
        try:
            import _api as api
        except ImportError as xcpt:
            raise TelemacException(\
                "Could not load the telemac api.\n"\
                "They are mandatory for this function\n"+str(xcpt))

        ikles = self.ikle2.reshape(self.nelem2*3) + 1
        ndim = self.get_mesh_dimension()
        identify_liq_bnd = api.api_interface.identify_liq_bnd
        nbor, liubor, lihbor, _, _ = self.get_bnd_info()
        # Switching to fortran numbering
        nbor += 1

        coords = np.zeros((2, self.npoin2), dtype=np.float)
        coords[0, :] = self.meshx
        coords[1, :] = self.meshy
        coord = coords.reshape(self.npoin2*2)

        # Not using nelbor, ifabor, kp1bor (for now)
        _, _, _, numliq = identify_liq_bnd(\
                ikles, ndim, liubor, lihbor, nbor,
                coord)

        nbor -= 1

        return nbor, numliq

    #############################################
    #
    # data extractions functions
    #
    #############################################
    #
    # Extractrion for a given record
    #
    def get_data_on_points(self, varname, record, points):
        """
        Extract values on points in telemac result file (2D or 3D)
        for the given variable for one record

        @param varname (string) Name of variable for which to extract data
        @param record (int) Number of desired record to extract
        @param points list of numpy.array containing points of extraction

        @returns (numpy.array)
        """
        res = float('nan')*np.ones((len(points)), dtype=np.float64)
        if len(np.shape(np.array(points))) != 2:
            raise TelemacException('Warning problem with the list of '\
                                   'extraction points')
        # dimension of the computation result
        dim = np.shape(np.array(points))[1]
        if dim == 2:
            res = self._get_data_on_2d_points(varname, record, points)
        elif dim == 3:
            res = self._get_data_on_3d_points(varname, record, points)
        else:
            raise TelemacException('Warning problem with the dimension of '\
                                   'extraction points')
        return res

    def _get_data_on_2d_points(self, varname, record, points):
        """
        Extract values on points in telemac-2d result file
        for the given variable for one record

        @param varname (string) Name of variable for which to extract data
        @param record (int) Number of the desired record to extract
        @param points (list) list of points to extract

        @returns (numpy.array)
        """
        if self.get_mesh_dimension() != 2:
            raise TelemacException("Action possible only on 2d mesh")

        res = np.zeros((len(points)), dtype=np.float64)
        values = self.get_data_value(varname, record)
        if len(values) > self.npoin2:
            raise TelemacException('Warning the dimension of the result '\
                                   'file is greater than 2')
        data_interp = mtri.LinearTriInterpolator(self.tri, values)
        for i, point in enumerate(points):
            res[i] = data_interp(point[0], point[1])
        return res

    def _get_data_on_3d_points(self, varname, record, points):
        """
        Extract values on points in telemac-3d result file
        for the given variable for one record

        @param points list of numpy.array containing points of
               extraction (x,y,z)
        @param varname (string) Name of variable for which to extract data
        @param record (int) Number of desired record to extract

        @returns (numpy.array)
        """
        if self.get_mesh_dimension() != 3:
            raise TelemacException("Action possible only on 3d mesh")

        res = float('nan')*np.ones((len(points)), dtype=np.float64)
        for i, point in enumerate(points):
            elev = self.get_data_on_vertical_segment(\
                    'ELEVATION Z', record, point[:-1])
            values = self.get_data_on_vertical_segment(\
                    varname, record, point[:-1])
            for plan in range(self.nplan-1):
                if elev[plan] <= point[-1] and point[-1] <= elev[plan+1]:
                    shz = (point[-1]-elev[plan])/max((elev[plan+1]\
                                                      -elev[plan]), 1.e-6)
                    res[i] = (1.0-shz)*values[plan]+shz*values[plan+1]
        return res

    def get_data_on_polyline(self, varname, record, polyline_points,
                             discretized_number=None):
        """
        Extract values of points over time for the given variable
        for record

        @param varname (string) Name of variable for which to extract data
        @param record (int) Number of the desired record to extract
        @param polyline_points (list) List of points defining the polyline
        @param discretized_number (list) list of number of discretized points
        on each polyline segment if None given will use self.discretize_polyline

        @returns (numpy.array, numpy.array, numpy.array)

        """
        if self.get_mesh_dimension() != 2:
            raise TelemacException("Action possible only on 2d mesh")

        if len(np.shape(np.array(polyline_points))) != 2:
            raise TelemacException('Warning problem with the list of '\
                                   'extraction points')

        if discretized_number is None:
            discretized_number = self.discretize_polyline(polyline_points)

        # dimension of the computation result
        dim = np.shape(np.array(polyline_points))[1]
        if dim == 2:
            polygone_discretized_points = linspace_poly(polyline_points,\
                                                    discretized_number)
            values_polylines = self.get_data_on_points(\
                                  varname,
                                  record,
                                  polygone_discretized_points)
            abs_curv = curvilinear_abscissa(polygone_discretized_points)
        else:
            raise TelemacException('Warning the extraction on a polyline'\
                                   ' is valid only in 2d')
        return polygone_discretized_points, abs_curv, values_polylines

    def get_data_on_horizontal_plane(self, varname, record, plane_number):
        """
        Extract values of one plane in telemac-3d result file
        for the given variable

        @param varname (string) Name of variable for which to extract data
        @param record (int) Number of desired record
        @param plane_number (int) Number of desired plane

        @returns (numpy.array)

        """
        if self.get_mesh_dimension() != 3:
            raise TelemacException("Action possible only on 3d mesh")

        values = self.get_data_value(varname, record)
        if plane_number < 0 or plane_number >= self.nplan:
            raise TelemacException(\
                    'Wrong plane number {} should be in [0, {}]'\
                    .format(plane_number, self.nplan-1))
        start = plane_number*self.npoin2
        end = (plane_number+1)*self.npoin2
        extracted_values = values[start:end]

        return extracted_values

    def get_data_on_horizontal_slice(\
            self, varname, record, zslices, nplanref=None):
        """
        Extract values of plan in telemac-3d result file for the given variable

        @param zslice (numpy.array) Elevation of the slice
        @param record (int) Number of desired record
        @param nplanref (int) Number of reference plane
        @param varname (string) Name of variable for which to extract data

        @returns (numpy.array)
        """
        if self.get_mesh_dimension() != 3:
            raise TelemacException("Action possible only on 3d mesh")

        if isinstance(zslices, list) or isinstance(zslices, np.ndarray):
            if zslices.ndim > 1:
                raise TelemacException('Warning the slice coordinate'\
                                      'must be 1d')
            res = np.zeros(((self.npoin2), len(zslices)), dtype=np.float64)
            zslices_list = zslices
        elif isinstance(zslices, int):
            res = np.zeros(((self.npoin2), zslices), dtype=np.float64)
            zslices_list = [zslices]
        else:
            raise TelemacException('Unknown zslices type')

        zref = np.zeros((self.npoin2), dtype=np.float64)

        if 'ELEVATION Z' in self.varnames:
            if nplanref is not None:
                zref = self.get_data_on_horizontal_plane(\
                                'ELEVATION Z', record, nplanref)
            values_elevation = self.get_data_value('ELEVATION Z', record)
            values_elevation = values_elevation.reshape(self.nplan, self.npoin2)
            values_var = self.get_data_value(varname, record)
            values_var = values_var.reshape(self.nplan, self.npoin2)

            for izs, zslice in enumerate(zslices_list):
                zslice = zref + zslice
                for j in range(self.npoin2):
                    res[j, izs] = float('nan')
                    for i in range(self.nplan-1):
                        if values_elevation[i, j] <= zslice[j] and \
                           zslice[j] <= values_elevation[i+1, j]:
                            shz = (zslice[i]-values_elevation[i, j])/\
                                  max((values_elevation[i+1, j] \
                                       - values_elevation[i, j]), 1.0e-6)
                            res[j, izs] = (1.0-shz)*values_var[i, j]+shz*\
                                         values_var[i+1, j]
                            break
        else:
            raise TelemacException('Warning the dimension of the result '\
                                   'file is not 3 ELEVATION Z is missing')

        if isinstance(zslices, list) or isinstance(zslices, np.ndarray):
            return res
        elif isinstance(zslices, int):
            return res[:, 0]

    def get_data_on_vertical_plane(self, varname, record, polyline_points,
                                   discretized_number=None):
        """
        Extract values of plan in telemac-3d result file for the given variable

        @param varname (string) Name of variable for which to extract data
        @param record (int) Number of desired record
        @param polyline_points (list) List of points defining the polyline
        @param discretized_number (list) List of number of discretized points
        on each polyline segment

        @returns (numpy.array)

        """
        if self.get_mesh_dimension() != 3:
            raise TelemacException("Action possible only on 3d mesh")

        if len(np.shape(np.array(polyline_points))) != 2:
            raise TelemacException('Warning problem with the list of '\
                                   'extraction points')

        if discretized_number is None:
            discretized_number = self.discretize_polyline(polyline_points)

        dim = np.shape(np.array(polyline_points))[1]
        if dim == 2:
            nplan = self.nplan
            polygone_discretized_points = linspace_poly(\
                                        polyline_points, discretized_number)
            npoly = len(polygone_discretized_points)
            values_polylines = np.zeros((npoly, nplan), dtype=np.float64)
            abs_curv = curvilinear_abscissa(polygone_discretized_points)
            for plan in range(self.nplan):
                values = self.get_data_on_horizontal_plane(\
                                                varname, record, plan)
                data_interp = mtri.LinearTriInterpolator(self.tri, values)
                for i, point in enumerate(polygone_discretized_points):
                    values_polylines[i, plan] = data_interp(point[0], point[1])
        else:
            raise TelemacException('Warning the extraction on a polyline'\
                                   ' of 2d points')
        return polygone_discretized_points, abs_curv, values_polylines

    def get_data_on_vertical_segment(self, var_name, record, point):
        """
        Extract values for each plane of a 2d points in telemac-3d result file
        for the given variable

        @param point (numpy.array) Point of extraction
        @param varname (string) Name of variable for which to extract data
        @param record (int) Number of desired record

        @returns (numpy.array)

        """
        if self.get_mesh_dimension() != 3:
            raise TelemacException("Action possible only on 3d mesh")

        if len(point) != 2:
            raise TelemacException('Warning the extraction point '\
                                   'must be 2d')
        nplan = self.nplan
        res = np.zeros(nplan)
        for plan in range(self.nplan):
            values = self.get_data_on_horizontal_plane(\
                               var_name, record, plan)
            data_interp = mtri.LinearTriInterpolator(self.tri, values)
            res[plan] = data_interp(point[0], point[1])
        return res


    #
    # Extractrion of a timeserie (extraction of data over all records)
    #
    def get_timeseries_on_nodes(self, varname, nodes):
        """
        Extract values of nodes over time for the given variable

        @param varname (string) Name of variable for which to extract data
        @param nodes (list) list of nodes to extract

        @returns (numpy.array) shape (len(nodes), self.ntimestep)

        """
        res = np.zeros((len(nodes), self.ntimestep), dtype=np.float64)
        for record in range(self.ntimestep):
            values = self.get_data_value(varname, record)
            res[range(len(nodes)), record] = values[nodes]

        return res

    def get_timeseries_on_points(self, varname, points):
        """
        Extract values of points over time for the given variable

        @param varname (string) Name of variable for which to extract data
        @param points (list) List of points to extract

        @returns (numpy.array)

        """
        res = np.zeros((len(points), self.ntimestep), dtype=np.float64)
        for record in range(self.ntimestep):
            res[:, record] = self.get_data_on_points(varname, record, points)
        return res


    def get_timeseries_on_polyline(self, varname, polyline_points,
                                   discretized_number=None):
        """
        Extract values of points over time for the given variable

        @param varname (string) Name of variable for which to extract data
        @param polyline_points (list) List of points defining the polyline
        @param discretized_number (list) List of number of discretized points
        on each polyline segment

        @returns (numpy.array, numpy.array, numpy.array) polygone discretised
        polygone_discretized_points, abs_curv, values_polylines

        """
        if self.get_mesh_dimension() != 2:
            raise TelemacException("Action possible only on 2d mesh")

        if len(np.shape(np.array(polyline_points))) != 2:
            raise TelemacException('Warning problem with the list of '\
                                   'extraction points')

        if discretized_number is None:
            discretized_number = self.discretize_polyline(polyline_points)

        # dimension of the computation result
        dim = np.shape(np.array(polyline_points))[1]
        if dim == 2:
            polygone_discretized_points = linspace_poly(polyline_points,\
                                                    discretized_number)
            values_polylines = self.get_timeseries_on_points(\
                                 varname, polygone_discretized_points)
            abs_curv = curvilinear_abscissa(polygone_discretized_points)
        else:
            raise TelemacException('Warning the extraction on a polyline'\
                                   ' is valid only in 2d')
        return polygone_discretized_points, abs_curv, values_polylines

    def get_timeseries_on_vertical_segment(self, varname, point):
        """
        Extract values of plan in telemac-3d result file for the given variable

        @param varname (string) Name of variable for which to extract data
        @param point (numpy.array) Point of extraction

        @returns (numpy.array)

        """
        if self.get_mesh_dimension() != 3:
            raise TelemacException("Action possible only on 3d mesh")

        nplan = self.nplan
        res = np.zeros((self.ntimestep, nplan), dtype=np.float64)
        for record in range(self.ntimestep):
            res[record, :] = self.get_data_on_vertical_segment(\
                                    varname, record, point)
        return res

    #############################################
    #
    # Spectrum specific functions
    #
    #############################################
    def is_a_spectrum_file(self):
        """
        Checking if the file is a spectrum file
        Criteria:
        - Quadrangles
        - All variables start with FPTS
        """
        import re

        is_spectrum = self.ndp3 == 4
        regex = re.compile(r'F[0-9]{2} PT2D[0-9]{6}')
        is_spectrum = is_spectrum and \
                      all([regex.match(var) is not None for var in self.varnames])

        return is_spectrum

    def get_spectrum_freq(self):
        """
        Compute the list of frequencies
        This only works if the file is a tomawac spectrum file

        @returns (numpy.array, numpy.array) List of frequencies, List of
        frequencie steps
        """
        if not self.is_a_spectrum_file():
            raise TelemacException("This file does not seem to be a spectrum file")

        nfreq = 0
        eps = 1e-6
        f_1 = 10e10
        f_2 = 10e10
        raisf = 0.
        for x, y in zip(self.meshx, self.meshy):
            if abs(x) <= eps and y >= 0.:
                nfreq += 1
                f_temp = y
                if f_temp < f_1:
                    f_2 = f_1
                    f_1 = f_temp
                elif f_temp < f_2:
                    f_2 = f_temp

        raisf = f_2/f_1

        freqs = [f_1 * raisf**i for i in range(nfreq)]

        dfreqs = np.zeros(nfreq, dtype=np.float64)

        auxi = (raisf - 1.)/2.
        dfreqs[0] = auxi*freqs[0]
        for i in range(1, nfreq-1):
            dfreqs[i] = auxi*(freqs[i] + freqs[i-1])

        dfreqs[-1] = auxi*freqs[-2]

        return np.array(freqs), dfreqs

    def get_list_spectrum_points(self):
        """
        Returns the list of spectrum points in the file
        """
        if not self.is_a_spectrum_file():
            raise TelemacException("This file does not seem to be a spectrum file")

        points = []
        for var in self.varnames:
            number = var.split()[-1][5:].lstrip('0')
            points.append(int(number))

        return points

    def get_spectrum_varname(self, point):
        """
        Return the variable associated to the spectrum point 'point'

        @param point (int) Point number

        @returns (string) Name of the variable
        """
        if not self.is_a_spectrum_file():
            raise TelemacException("This file does not seem to be a spectrum file")

        spectrum_var = None
        # Getting the variable for point point
        for var in self.varnames:
            if "{:06d}".format(point) in var:
                spectrum_var = var
                break

        if spectrum_var is None:
            raise TelemacException("Could not find point {} in your variables:\
            \n{}".format(point, self.varnames))

        return spectrum_var

    def get_angular_dispersion(self, point, record, radian=False):
        """
        Return value of the angular dispersion

        @param point (int) number of the point for which we extract the spectrum
        @param record (int) Time record for which to extract
        @param radian (boolean) If true theta is built in radian otherwise in
        degree

        @returns (numpy.array, numpy.array) The frequencie list, The angular
        dispersion values
        """
        if not self.is_a_spectrum_file():
            raise TelemacException("This file does not seem to be a spectrum file")

        spectrum_var = self.get_spectrum_varname(point)

        # Getting list of frequencies
        freqs, dfreqs = self.get_spectrum_freq()

        nfreq = len(freqs)
        ntheta = self.npoin2//nfreq

        # Reshaping to match nfreq*ntheta
        data = self.get_data_value(spectrum_var, record)\
                   .reshape((nfreq, ntheta))

        ang_disp = np.zeros(ntheta, dtype=np.float64)
        # Integration over frequencies
        for itheta in range(ntheta):
            for ifreq in range(nfreq):
                ang_disp[itheta] += data[ifreq, itheta]*dfreqs[ifreq]

        # Defining if we are in radian or degree
        if radian:
            val = 2*np.pi
        else:
            val = 360.

        # Building angles array
        theta = [i*val/ntheta for i in range(ntheta)]

        return theta, ang_disp

    def get_spectrum(self, point, record):
        """
        Return value of spectrum for a given point and record

        @param point (int) number of the point for which we extract the spectrum
        @param record (int) Time record for which to extract

        @returns (numpy.array, numpy.array) The frequencie list, The spectrum
        values
        """
        if not self.is_a_spectrum_file():
            raise TelemacException("This file does not seem to be a spectrum file")

        spectrum_var = self.get_spectrum_varname(point)

        # Getting list of frequencies
        freqs, _ = self.get_spectrum_freq()

        nfreq = len(freqs)
        ntheta = self.npoin2//nfreq

        # Reshaping to match nfreq*ntheta
        data = self.get_data_value(spectrum_var, record)\
                   .reshape((nfreq, ntheta))

        # Integration over angles
        spectrum = np.sum(data, axis=1) * 2*np.pi/ntheta

        return freqs, spectrum
