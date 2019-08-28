r"""@author TELEMAC-MASCARET Consortium

    @brief
"""
from __future__ import print_function
from utils.parser_strings import parse_array_frame, parse_array_grid
from utils.exceptions import TelemacException
from data_manip.extraction.parser_selafin import subset_variables_slf
import numpy as np

def get_kdtree(meshx, meshy, ikle):
    """
    @brief Create a KDTree from mesh
    @param meshx: X coordinates
    @param meshy: Y coordinates
    @param ikle: Mesh connectivity table
    @return:
    """
    from scipy.spatial import cKDTree
    isoxy = np.column_stack((np.sum(meshx[ikle], axis=1)/3.0,
                             np.sum(meshy[ikle], axis=1)/3.0))
    return cKDTree(isoxy)

def get_mpl_tri(meshx, meshy, ikle):
    """
    @brief Return neighbours and boundary segment of the mesh
    @param meshx: X coordinates
    @param meshy: Y coordinates
    @param ikle: Mesh connectivity table
    @return:
    """
    from matplotlib.tri import Triangulation
    mpltri = Triangulation(meshx, meshy, ikle).get_cpp_triangulation()
    return mpltri.get_neighbors(), mpltri.get_edges()

def what_time_slf(instr, ctimes):
    """
    @brief
    @param instr: list of frames or (times,) delimited by ';'
    @param ctimes: list of time
    @return:
    """
    return parse_array_frame(instr, len(ctimes))

def what_vars_slf(instr, vnames):
    """
    @brief
    @param instr: list of pairs "variable:support" delimited by ';'
    @param vnames: list of variables names from the SELAFIN file
    @return:
    """
    variables = []
    vtypes = []
    for var in instr.split(';'):
        tmp, vtype = var.split(':')
        variables.append(tmp)
        vtypes.append(vtype)
    return subset_variables_slf(';'.join(variables), vnames), vtypes

def what_sample(inspl, box):
    grids = []
    plans = []
    for grid in parse_array_grid(inspl, box):
        if grid[0][0] == grid[1][0]:
            raise TelemacException(\
                  '... same min(x)='+str(grid[0][0])+' and max(x)='+\
                  str(grid[1][0])+' values in: '+repr(inspl)+\
                  '. I cannot create a box.')
        if grid[0][1] == grid[1][1]:
            raise TelemacException(\
                  '... same min(y)='+str(grid[0][1])+' and max(y)='+\
                  str(grid[1][1])+' values in: '+repr(inspl)+\
                  '. I cannot create a box.')
        if len(grid[0]) > 2:
            if grid[0][2] == grid[1][2]:
                raise TelemacException(\
                      '... same min(z)='+str(grid[0][2])+' and max(z)='+\
                      str(grid[1][2])+' values in: '+repr(inspl)+\
                      '. I cannot create a box.')
            m_x, m_y, m_z = np.meshgrid(np.linspace(grid[0][0],
                                                    grid[1][0],
                                                    grid[-1][0]+1),
                                        np.linspace(grid[0][1],
                                                    grid[1][1],
                                                    grid[-1][1]+1),
                                        np.linspace(grid[0][2],
                                                    grid[1][2],
                                                    grid[-1][2]+1))
            grids.append((len(m_x[0][0]),
                          len(m_x[0]),
                          len(m_x),
                          np.concatenate(m_x),
                          np.concatenate(m_y),
                          np.concatenate(m_z)))
        else:
            m_x, m_y = np.meshgrid(np.linspace(grid[0][0],
                                               grid[1][0],
                                               grid[-1][0]+1),
                                   np.linspace(grid[0][1],
                                               grid[1][1],
                                               grid[-1][1]+1))
            grids.append((len(m_x[0]),
                          len(m_x),
                          np.concatenate(m_x),
                          np.concatenate(m_y)))
            if len(grid[-1]) == 3:
                plans = grid[2]
    return grids, plans

def set_quad(grids):
    meshx = []
    meshy = []
    ikle = []
    for dimx, dimy, x, y in grids:
        ikle.extend(([i+j*dimx, i+1+j*dimx, i+1+(j+1)*dimx, i+(j+1)*dimx] \
                     for j in range(dimy-1) for i in range(dimx-1)))
        meshx.extend(x)
        meshy.extend(y)
    return np.array(ikle), np.array(meshx), np.array(meshy)

def split_quad_to_triangle(ikle):
    """
    @brief split each quad into triangles
    @param grids:
    @return:
    """
    return np.delete(np.concatenate((ikle, np.roll(ikle, 2, axis=1))),
                     np.s_[3::], axis=1)
