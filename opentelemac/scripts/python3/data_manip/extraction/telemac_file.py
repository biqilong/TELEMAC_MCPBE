"""
Contains the class TelemacFile
"""
from utils.exceptions import TelemacException
from telapy.api.hermes import HermesFile
from data_manip.computation.triangulation import triangulation_from_data
from os import path
import math
import numpy as np
from scipy.spatial import cKDTree
import scipy.spatial.distance as sc
import matplotlib.tri as mtri

def linspace_seg(point1, point2, length, end=False):
    """
    Discretize segment into length points

    @param point1 (numpy array) first point of the segment
    @param point2 (numpy array) second point of the segment
    @param length (int) number of discretized points on the segment
    @param end (optional logical) include or not the last point

    @returns (numpy.array)
    """
    v_1 = np.linspace(point1[0], point2[0], length, endpoint=end)
    v_2 = np.linspace(point1[1], point2[1], length, endpoint=end)
    line = np.zeros(shape=[length, 2])
    line[:, 0] = v_1
    line[:, 1] = v_2
    return line

def linspace_poly(poly_points, poly_number):
    """
    Discretize polyline

    @param poly_points (list of numpy array) list of polyline points
    @param poly_number (list of int) number of discretized points for each
    polyline segments

    @returns (numpy.array)
    """
    list_seg = []
    for i in range(len(poly_points)-1):
        lin = linspace_seg(poly_points[i], poly_points[i+1], poly_number[i],\
                           end=False)
        list_seg.append(lin)
    length = sum(poly_number)+1
    poly = np.zeros(shape=[length, 2])
    poly[0:poly_number[0], :] = list_seg[0]
    deb = 0
    for i in range(0, len(poly_number)-1):
        deb = deb + poly_number[i]
        poly[deb:(poly_number[i+1]+deb), :] = list_seg[i+1][:]
    poly[-1, :] = poly_points[-1]
    return poly

def curvilinear_abscissa(coord_poly):
    """
    Compute curvilinear abscissa of a polyline

    @param coord_poly (list) coordinates of polyline (list of 2-uple)

    @return curv_absc (list) list of curvilinear abscissa (list of float)
    """
    curv_absc = np.zeros(len(coord_poly))

    for i in range(len(coord_poly)-1):
        coord_point_1 = coord_poly[i, :]
        coord_point_2 = coord_poly[i+1, :]
        curv_absc[i+1] = curv_absc[i] + sc.euclidean(coord_point_1,
                                                     coord_point_2)

    return curv_absc

class TelemacFile(HermesFile):
    """
    Class to extract data from a TelemacFile
    """

    def __init__(self, file_name, bnd_file=None):
        """
        Initialisation of a file reader

        @param file_name Name of the mesh file
        @param bnd_file Name of the boundary file (default None)
        """

        # Identifying format from file extension if not given
        _, ext = path.splitext(file_name)
        if ext == '.med':
            fformat = 'MED'
        else:
            fformat = 'SERAFIN'
        HermesFile.__init__(self, file_name, boundary_file=bnd_file, fformat=fformat)

        self._title = None
        self._nbv1 = None
        self._nbv2 = 0
        self._nvar = None
        self._varnames = None
        self._varunits = None
        self._varnames = None
        self._varunits = None
        self._cdlnames = None
        self._cdlunits = None
        self._iparam = None
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
        self._varindex = None
        self._ikle3 = None
        self._ikle2 = None
        self._ipob3 = None
        self._ipob2 = None
        self._tags = None
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
    def nbv1(self):
        """
        Returns nbv1 value
        """
        if self._nbv1 is None:
            self._nbv1 = self.get_data_nvar()

        return self._nbv1

    @nbv1.setter
    def nbv1(self, nbv1):
        """ Setting nbv1 value """
        self._nbv1 = nbv1

    @property
    def nvar(self):
        """
        Returns nvar value
        """
        if self._nvar is None:
            self._nvar = self.nbv1

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
    def iparam(self):
        """
        Returns datetime value
        """
        if self._iparam is None:
            self._iparam = [0]*10
            self._iparam[0] = 1
            self._iparam[6] = self.nplan
            self._iparam[7] = self.get_bnd_npoin()
            self._iparam[8] = self.get_mesh_nptir()
            self._iparam[9] = 0 if self.datetime is None else 1

        return self._iparam

    @property
    def nelem3(self):
        """
        Returns datetime value
        """
        if self._nelem3 is None:
            self._nelem3 = self.get_mesh_nelem()

        return self._nelem3

    @property
    def npoin3(self):
        """
        Returns datetime value
        """
        if self._npoin3 is None:
            self._npoin3 = self.get_mesh_npoin()

        return self._npoin3

    @property
    def ndp3(self):
        """
        Returns datetime value
        """
        if self._ndp3 is None:
            self._ndp3 = self.get_mesh_npoin_per_element()

        return self._ndp3

    @property
    def nplan(self):
        """
        Returns datetime value
        """
        if self._nplan is None:
            self._nplan = max(1, self.get_mesh_nplan())

        return self._nplan

    @property
    def nelem2(self):
        """
        Returns datetime value
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
        Returns datetime value
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
        Returns datetime value
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
        Returns datetime value
        """
        if self._ikle3 is None:
            self._ikle3 = self.get_mesh_connectivity()

        return self._ikle3

    @property
    def ikle2(self):
        """
        Returns ikle2 value
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
        Returns datetime value
        """
        if self._ipob3 is None:
            self._ipob3 = self.get_bnd_ipobo()

        return self._ipob3

    @property
    def ipob2(self):
        """
        Returns datetime value
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
        Returns datetime value
        """
        if self._ntimestep is None:
            self._ntimestep = self.get_data_ntimestep()

        return self._ntimestep

    @property
    def varindex(self):
        """
        Returns y coordinates value
        """
        if self._varindex is None:
            self._varindex = range(self._nvar)

        return self._varindex

    @property
    def tri(self):
        """
        Returns matplotlib triangulation
        """
        if self._tri is None:
            self._tri = mtri.Triangulation(self.meshx[:self.npoin2],
                                           self.meshy[:self.npoin2],
                                           self.ikle2)

        return self._tri

    @property
    def tags(self):
        """
        Returns dictonary containing time step info
        """
        if self._tags is None:
            self._tags = {'cores':[], 'times':[]}
            for itime in range(self.ntimestep):
                self._tags['cores'].append(None)
                self._tags['times'].append(self.get_data_time(itime))

            self._tags['times'] = np.asarray(self._tags['times'])

        return self._tags

    def get_time_record(self, time):
        """
        Get the record closest to time

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

    @property
    def times(self):
        """
        Returns a list of the times in the file
        """
        if self._times is None:
            self._times = np.zeros((self.ntimestep), dtype=np.float64)
            for i in range(self.ntimestep):
                self._times[i] = self.get_data_time(i)

        return self._times

    def set_kd_tree(self, reset=False):
        """
        Builds a KDTree (impoves search of neighbours)

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
                "They are mandatory for this function\n"+print(xcpt))

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

    def get_variables_at(self, frame, vars_indexes):
        """
        Get values for a given time step and a list of variables

        @param frame (int) Time step to extract
        @param vars_indexes (list) List of variable names

        @return (np.array) array containing the values for each variable
        """
        res = []
        for ivar in vars_indexes:
            var_name = self.varnames[ivar]
            res.append(self.get_data_value(var_name, frame))

        return np.asarray(res)

    def get_values(self, time):
        """
        Get values for all variables for a give time step
        If alter_values were set it applies modification
        """
        res = self.get_variables_at(time, self.varindex)

        return res

    def get_series(self, nodes, vars_indexes=None, showbar=True):
        """
        Return the value for a list of nodes on variables given in vars_indexes
        for each time step

        @param nodes (list) list of nodes for which to extract data
        @param vars_indexes (list) List of variable indexes to extract data for
        @param showbar (boolean) If True display a showbar for the progress

        """
        if vars_indexes is None:
            vars_indexes = self.varindex
        res = np.zeros((len(vars_indexes), len(nodes),
                        len(self.ntimestep)),
                       dtype=np.float64)
        for itime in range(self.ntimestep):
            for ivar_res, ivar in enumerate(self.varindex):
                var_name = self.varnames[ivar]
                elev = self.get_data_value(var_name, itime)
                for inode_res, inode in enumerate(nodes):
                    res[ivar_res, inode_res, itime] = elev[inode]

        return res

    def get_timeseries_on_nodes(self, nodes, varname):
        """
        Extract values of nodes over time for the given variable

        @param nodes (list) list of nodes to extract
        @param varname (string) Name of variable for which to extract data

        @returns (numpy.array)

        """
        res = np.zeros((len(nodes), self.ntimestep), dtype=np.float64)
        for time in range(self.ntimestep):
            values = self.get_data_value(varname, time)
            res[range(len(nodes)), time] = values[nodes]

        return res

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

    def get_values_on_points(self, points, varname, timestep):
        """
        Extract values on points in telemac result file (2D or 3D)
        for the given variable for one time step

        @param points list of numpy.array containing points of extraction
        @param varname (string) Name of variable for which to extract data
        @param timestep (int) Number of desired time step to extract

        @returns (numpy.array)
        """
        res = float('nan')*np.ones((len(points)), dtype=np.float64)
        if len(np.shape(np.array(points))) != 2:
            raise TelemacException('Warning problem with the list of '\
                                   'extraction points')
        # dimension of the computation result
        dim = np.shape(np.array(points))[1]
        if dim == 2:
            res = self.get_values_on_2d_points(points, varname, timestep)
        elif dim == 3:
            res = self.get_values_on_3d_points(points, varname, timestep)
        else:
            raise TelemacException('Warning problem with the dimension of '\
                                   'extraction points')
        return res

    def get_values_on_2d_points(self, points, varname, timestep):
        """
        Extract values on points in telemac-2d result file
        for the given variable for one time step

        @param points (list) list of points to extract
        @param varname (string) Name of variable for which to extract data
        @param timestep (int) Number of the desired time step to extract

        @returns (numpy.array)
        """
        res = np.zeros((len(points)), dtype=np.float64)
        values = self.get_data_value(varname, timestep)
        if len(values) > self.npoin2:
            raise TelemacException('Warning the dimension of the result '\
                                   'file is greater than 2')
        data_interp = mtri.LinearTriInterpolator(self.tri, values)
        for i, point in enumerate(points):
            res[i] = data_interp(point[0], point[1])
        return res

    def get_values_on_3d_points(self, points, varname, timestep):
        """
        Extract values on points in telemac-3d result file
        for the given variable for one time step

        @param points list of numpy.array containing points of
               extraction (x,y,z)
        @param varname (string) Name of variable for which to extract data
        @param timestep (int) Number of desired time step to extract

        @returns (numpy.array)
        """
        res = float('nan')*np.ones((len(points)), dtype=np.float64)
        for i, point in enumerate(points):
            elev = self.get_data_value_on_vertical_segment(point[:-1],\
                                        'ELEVATION Z', timestep)
            values = self.get_data_value_on_vertical_segment(point[:-1],\
                                        varname, timestep)
            for plan in range(self.nplan-1):
                if elev[plan] <= point[-1] and point[-1] <= elev[plan+1]:
                    shz = (point[-1]-elev[plan])/max((elev[plan+1]\
                                                      -elev[plan]), 1.e-6)
                    res[i] = (1.0-shz)*values[plan]+shz*values[plan+1]
        return res

    def get_timeseries_on_points(self, points, varname):
        """
        Extract values of points over time for the given variable

        @param points (list) List of points to extract
        @param varname (string) Name of variable for which to extract data

        @returns (numpy.array)

        """
        res = np.zeros((len(points), self.ntimestep), dtype=np.float64)
        for time in range(self.ntimestep):
            res[:, time] = self.get_values_on_points(points, varname, time)
        return res

    def discretize_polyline(self, polyline):
        """
        Will return the number of point for each polyline segment taking the
        minimum mesh resolution as step

        @param polyline (list) List of points of the polyline
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

    def get_values_on_polyline(self, polyline_points, varname,
                               discretized_number, timestep):
        """
        Extract values of points over time for the given variable
        for one time step

        @param polyline_points (list) List of points defining the polyline
        @param varname (string) Name of variable for which to extract data
        @param discretized_number (list) list of number of discretized points
        on each polyline segment
        @param timestep (int) Number of the desired time step to extract

        @returns (numpy.array,numpy.array)

        """
        if len(np.shape(np.array(polyline_points))) != 2:
            raise TelemacException('Warning problem with the list of '\
                                   'extraction points')
        # dimension of the computation result
        dim = np.shape(np.array(polyline_points))[1]
        if dim == 2:
            polygone_discretized_points = linspace_poly(polyline_points,\
                                                    discretized_number)
            values_polylines = self.get_values_on_points(\
                                  polygone_discretized_points,
                                  varname,
                                  timestep)
            abs_curv = curvilinear_abscissa(polygone_discretized_points)
        else:
            raise TelemacException('Warning the extraction on a polyline'\
                                   ' is valid only in 2d')
        return polygone_discretized_points, abs_curv, values_polylines

    def get_timeseries_on_polyline(self, polyline_points, varname,
                                   discretized_number):
        """
        Extract values of points over time for the given variable

        @param polyline_points (list) List of points defining the polyline
        @param varname (string) Name of variable for which to extract data
        @param discretized_number (list) List of number of discretized points
        on each polyline segment

        @returns (numpy.array, numpy.array, numpy.array) polygone discretised
        polygone_discretized_points, abs_curv, values_polylines

        """
        if len(np.shape(np.array(polyline_points))) != 2:
            raise TelemacException('Warning problem with the list of '\
                                   'extraction points')
        # dimension of the computation result
        dim = np.shape(np.array(polyline_points))[1]
        if dim == 2:
            polygone_discretized_points = linspace_poly(polyline_points,\
                                                    discretized_number)
            values_polylines = self.get_timeseries_on_points(\
                                 polygone_discretized_points, varname)
            abs_curv = curvilinear_abscissa(polygone_discretized_points)
        else:
            raise TelemacException('Warning the extraction on a polyline'\
                                   ' is valid only in 2d')
        return polygone_discretized_points, abs_curv, values_polylines

    def get_data_values_on_horizontal_plan(self, plane_number,
                                           timestep, varname):
        """
        Extract values of one plane in telemac-3d result file
        for the given variable

        @param plane_number (int) Number of desired plane
        @param timestep (int) Number of desired timestep
        @param varname (string) Name of variable for which to extract data

        @returns (numpy.array)

        """
        values = self.get_data_value(varname, timestep)
        if self.npoin2 == self.npoin3:
            raise TelemacException('Warning the dimension of the result '\
                                   'file is not 3')
        if plane_number < 0 or plane_number >= self.nplan:
            raise TelemacException(\
                    'Wrong plane number {} should be in [0, {}]'\
                    .format(plane_number, self.nplan-1))
        start = plane_number*self.npoin2
        end = (plane_number+1)*self.npoin2
        extracted_values = values[start:end]

        return extracted_values

    def get_data_values_on_vertical_plan(self, polyline_points, varname,
                                         discretized_number, timestep):
        """
        Extract values of plan in telemac-3d result file for the given variable

        @param polyline_points (list) List of points defining the polyline
        @param varname (string) Name of variable for which to extract data
        @param discretized_number (list) List of number of discretized points
        on each polyline segment
        @param timestep (int) Number of desired timestep

        @returns (numpy.array)

        """
        if len(np.shape(np.array(polyline_points))) != 2:
            raise TelemacException('Warning problem with the list of '\
                                   'extraction points')
        dim = np.shape(np.array(polyline_points))[1]
        if dim == 2:
            nplan = self.nplan
            polygone_discretized_points = linspace_poly(\
                                        polyline_points, discretized_number)
            npoly = len(polygone_discretized_points)
            values_polylines = np.zeros((npoly, nplan), dtype=np.float64)
            abs_curv = curvilinear_abscissa(polygone_discretized_points)
            for plan in range(self.nplan):
                values = self.get_data_values_on_horizontal_plan(\
                                                plan, timestep, varname)
                data_interp = mtri.LinearTriInterpolator(self.tri, values)
                for i, point in enumerate(polygone_discretized_points):
                    values_polylines[i, plan] = data_interp(point[0], point[1])
        else:
            raise TelemacException('Warning the extraction on a polyline'\
                                   ' is valid only in 2d')
        return polygone_discretized_points, abs_curv, values_polylines

    def get_data_value_on_vertical_segment(self, point, varname, timestep):
        """
        Extract values of plan in telemac-3d result file for the given variable

        @param point (numpy.array) Point of extraction
        @param varname (string) Name of variable for which to extract data
        @param timestep (int) Number of desired timestep

        @returns (numpy.array)

        """
        if len(point) != 2:
            raise TelemacException('Warning the extraction point '\
                                   'must be 2d')
        nplan = self.nplan
        res = np.zeros(nplan)
        for plan in range(self.nplan):
            values = self.get_data_values_on_horizontal_plan(\
                               plan, timestep, varname)
            data_interp = mtri.LinearTriInterpolator(self.tri, values)
            res[plan] = data_interp(point[0], point[1])
        return res

    def get_data_timeseries_on_vertical_segment(self, point, varname):
        """
        Extract values of plan in telemac-3d result file for the given variable

        @param point (numpy.array) Point of extraction
        @param varname (string) Name of variable for which to extract data

        @returns (numpy.array)

        """
        nplan = self.nplan
        res = np.zeros((self.ntimestep, nplan), dtype=np.float64)
        for time in range(self.ntimestep):
            res[time, :] = self.get_data_value_on_vertical_segment(\
                                    point, varname, time)
        return res

    def get_data_value_on_horizontal_slice(\
            self, zslices, timestep, varname, nplanref=None):
        """
        Extract values of plan in telemac-3d result file for the given variable

        @param zslice (numpy.array) Elevation of the slice
        @param timestep (int) Number of desired timestep
        @param nplanref (int) Number of reference plane
        @param varname (string) Name of variable for which to extract data

        @returns (numpy.array)
        """
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
                zref = self.get_data_values_on_horizontal_plan(\
                                nplanref, timestep, 'ELEVATION Z')
            values_elevation = self.get_data_value('ELEVATION Z', timestep)
            values_elevation = values_elevation.reshape(self.nplan, self.npoin2)
            values_var = self.get_data_value(varname, timestep)
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

    def get_data(self, var_name, record,
                 zslice=None, poly=None, poly_number=None, plane=0):
        """
        Wrapper for get_data_value, get_data_values_on_horizontal_plan
        get_data_values_on_horizontal_slice, and get_data_values_on_vertical_plan.

        Select the right function depending on zslice, poly and plane number.
        """
        ndim = self.get_mesh_dimension()

        if zslice is not None and poly is None:
            # Get data on horizontal slice plane
            scalar = self.get_data_value_on_horizontal_slice(zslice, record, var_name)
            return self.tri, scalar

        elif poly is not None and zslice is None:
            # Get data on vertical slice plane (from polyline)
            namez = self.varnames[0]
            if poly_number is None:
                poly_number = self.discretize_polyline(poly)
            _, abs_curv, values_z = \
                self.get_data_values_on_vertical_plan(poly, namez, \
                                                     poly_number, record)
            _, _, scalar = \
                self.get_data_values_on_vertical_plan(poly, var_name, \
                                                     poly_number, record)
            mesh = triangulation_from_data(abs_curv, values_z)
            return mesh, scalar.flatten()

        elif poly is None and zslice is None:
            if ndim == 3:
                # Get data on plane (3D)
                scalar = self.get_data_values_on_horizontal_plan(plane, record, var_name)

            elif ndim == 2:
                # Get data on mesh (2D)
                scalar = self.get_data_value(var_name, record)
            return self.tri, scalar

        else:
            raise TelemacException("Cannot extract from both horizontal and " + \
                                   "vertical slice planes")

    def get_spectrum_freq(self):
        """
        Compute the list of frequencies
        This only works if the file is a tomawac spectrum file

        @returns (numpy.array, numpy.array) List of frequencies, List of
        frequencie steps
        """
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
