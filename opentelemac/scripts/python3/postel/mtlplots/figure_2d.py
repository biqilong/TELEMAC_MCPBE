r"""@author TELEMAC-MASCARET Consortium
    @brief
"""
from __future__ import print_function
import sys
from copy import deepcopy
from os import path, stat, mkdir
from postel.caster import Caster
from postel.mtlplots.title import Title
from postel.mtlplots.axes import Axes
from postel.mtlplots.myplot2d import map_deco_default, DECO_DEFAULT, \
                                     draw_geo, draw_image
from postel.mtlplots.myplot2d import draw_mesh_2d_elements, deco, \
                                     draw_3d_surface
from postel.mtlplots.myplot2d import draw_coloured_tri_maps, \
                                     draw_coloured_tri_vects
from postel.mtlplots.myplot2d import draw_labeled_tri_contours
from utils.exceptions import TelemacException
import numpy as np
import matplotlib as mpl
# Warning the line belows active 3d projection
# Do not remove it even if it is not used
from mpl_toolkits.mplot3d import Axes3D

try:
    HIDE_DEFAULT = not sys.stderr.isatty()
except AttributeError:
    HIDE_DEFAULT = True  # output does not support isatty()
if HIDE_DEFAULT:
    mpl.use('Agg')  # Use of Agg must be done before importing matplotlib.pyplot
import matplotlib.pyplot as plt

class Figure2D(Caster):
    """
    @brief Class for 2D figure
    """
    def __init__(self, caster, plot):
        Caster.__init__(self, {'object': caster.object,
                               'obdata': caster.obdata})

        myplt = deepcopy(plot)

        # ~~~ special keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for key in ['size', 'roi']:
            if key in myplt.keys():
                myplt['deco'].update({key: plot[key]})
                del myplt[key]

        # ~~~ special case for set ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if 'set' in myplt.keys():
            if 'look' in myplt['deco'].keys():
                for l in myplt['deco']['look']:
                    if 'set' in l:
                        if myplt['set'] not in l['set'].split(';'):
                            l['set'] = myplt['set'] + ';' + l['set']
                    else:
                        l.update({'set': myplt['set']})
            else:
                myplt['deco'].update({'look': [{'set': myplt['set']}]})
            del myplt['set']

        # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.dpar = {}
        self.mpar, self.upar = map_deco_default(myplt['deco'], DECO_DEFAULT)
        mpl.rcParams.update(self.mpar)

        # ~~> type of plot
        if self.upar['type'] != '':
            if self.upar['type'][1] == 'line' or self.upar['type'][1] == 'rose':
                self.upar['adjustable'] = 'datalim'
            else:
                if 'aspect' not in self.upar.keys():
                    self.upar['aspect'] = 'auto'
        else:
            if 'aspect' not in self.upar.keys():
                self.upar['aspect'] = 'auto'

        # Create the variable for the title
        titre = Title(self.upar, DECO_DEFAULT)
        # create the variable for the axes
        axe = Axes(self.upar)

        # Check if there is a 3d projection
        deco_axe = {}
        if 'projection' in self.upar:
            deco_axe['projection'] = self.upar['projection']
            self.upar.pop('projection', None)

        # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # TODO: do this when you draw
        fig = plt.figure(figsize=self.mpar['figure.figsize'])

        # ~~> add_subplot, or axes definition
        nrows = 1
        ncols = 1
        if 'nrows' in self.upar:
            nrows = int(self.upar['nrows'])
            self.upar.pop('nrows', None)
        if 'ncols' in self.upar:
            ncols = int(self.upar['ncols'])
            self.upar.pop('ncols', None)
        if 'index' in self.upar:
            self.upar.pop('index', None)
        self.axes = []
        for n in range(nrows):
            for col in range(ncols):
                self.axes.append(fig.add_subplot(\
                        nrows, ncols, (ncols * n + 1) + col,
                        **deco_axe))
            # Add the title in the graph
            titre.set_title(self.axes[-1])
            # change the label of the axe
            axe.set_param(self.axes[-1])

        # fig.add_subplot(111) returns an Axes instance, where we can plot and
        # this is also the reason why we call the variable referring to that
        # instance axes.  This is a common way to add an Axes to a Figure, but
        # add_subplot() does a bit more: it adds a subplot. So far we have only
        # seen a Figure with one Axes instance, so only one area where we can
        # draw, but Matplotlib allows more than one

        # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        self.x_ratio = None
        if 'x_ratio' in self.upar and self.upar['x_ratio'] is not None:
            self.x_ratio = float(self.upar['x_ratio'])
        self.y_ratio = None
        if 'y_ratio' in self.upar and self.upar['y_ratio'] is not None:
            self.y_ratio = float(self.upar['y_ratio'])
        # ~~> region of interes and default margins
        self.dpar.update({"roi": [[],
                                  [axe.param_lim['axe.emarge']['val'],
                                   axe.param_lim['axe.nmarge']['val'],
                                   axe.param_lim['axe.wmarge']['val'],
                                   axe.param_lim['axe.smarge']['val'],
                                  ]
                                 ]
                         }
                        )

        self.plt = plt
        self.fig = fig

    def add(self, typl, what):
        """
        Add a curve in a graph
        @param typl:
        @param what:
        @return:
        """
        Caster.add(self, typl, what)

        if len(what['vars'].split(';')) > 1:
            raise TelemacException(\
                    '... do not know support multiple variables anymore: '
                    '{}'.format(what['vars']))


        if 'tif' in typl.lower():
            vtype = what['vars'].split(':')[1]
            if "geo" in vtype:
                draw_geo(self.plt, what['file'])
            if "image" in vtype:
                draw_image(self.plt, what['deco'], what['file'])

        elif 'jpg' in typl.lower() or 'gif' in typl.lower() \
                or 'png' in typl.lower() or 'bmp' in typl.lower():
            vtype = what['vars'].split(':')[1]
            if "image" in vtype:
                draw_image(self.plt, what['deco'], what['file'])

        elif what['type'].split(':')[1] == 'v-section':

            cast = self.get(typl, what)
            elements = cast.support
            varsors = cast.values
            vtype = what['vars'].split(':')[1]

            if self.x_ratio is not None:
                if isinstance(elements, tuple):
                    elements = list(elements)
                    elements[0] *= self.x_ratio
                    elements = tuple(elements)
                else:
                    elements.T[0] *= self.x_ratio
            if self.y_ratio is not None:
                if isinstance(elements, tuple):
                    elements = list(elements)
                    elements[1] *= self.y_ratio
                    elements = tuple(elements)
                else:
                    elements.T[1] *= self.y_ratio

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
                # ~~> Draw/Dump (works with triangles and quads)
                draw_mesh_2d_elements(\
                        self.plt, self.axes, self.fig, what['deco'], elements)
                x_0 = np.ravel(elements.T[0])
                y_0 = np.ravel(elements.T[1])
                if self.dpar["roi"][0] == []:
                    self.dpar["roi"][0] = [\
                            np.min(x_0), np.min(y_0), np.max(x_0), np.max(y_0)]
                else:
                    self.dpar["roi"][0] = [\
                            min(self.dpar["roi"][0][0], np.min(x_0)),
                            min(self.dpar["roi"][0][1], np.min(y_0)),
                            max(self.dpar["roi"][0][2], np.max(x_0)),
                            max(self.dpar["roi"][0][3], np.max(y_0))]
                    # TODO: colour the mesh according to cast.values

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            else:
                meshx, meshz, ikle3 = elements
                # ~~> Multi-variables calculations
                if len(varsors) > 1:
                    if "map" in vtype:
                        varsors = [np.sqrt(np.sum(np.power(np.dstack(\
                                  varsors[0:3:2])[0], 2), axis=1))]
                # ~~> Draw/Dump (multiple options possible)
                if "map" in vtype:
                    draw_coloured_tri_maps(\
                            self.plt, self.axes, self.fig, what['deco'],
                            (meshx, meshz, ikle3, varsors[0]))
                if "label" in vtype:
                    draw_labeled_tri_contours(self.plt, what['deco'],
                                              (meshx, meshz, ikle3, varsors[0]))
                if "arrow" in vtype or "vector" in vtype:
                    draw_coloured_tri_vects(\
                            self.plt, self.axes, self.fig, what['deco'],
                            (meshx, meshz, varsors, False, ikle3))
                if "angle" in vtype:
                    draw_coloured_tri_vects(self.plt, self.axes, self.fig,
                                            what['deco'],
                                            (meshx, meshz, varsors, True,
                                             ikle3))
                x_0 = np.ravel(meshx[ikle3])
                y_0 = np.ravel(meshz[ikle3])
                if self.dpar["roi"][0] == []:
                    self.dpar["roi"][0] = [np.min(x_0), np.min(y_0),
                                           np.max(x_0), np.max(y_0)]
                else:
                    self.dpar["roi"][0] = [\
                            min(self.dpar["roi"][0][0], np.min(x_0)),
                            min(self.dpar["roi"][0][1], np.min(y_0)),
                            max(self.dpar["roi"][0][2], np.max(x_0)),
                            max(self.dpar["roi"][0][3], np.max(y_0))]


        elif what['type'].split(':')[1] == 'p-section':

            cast = self.get(typl, what)
            elements = cast.support
            varsors = cast.values
            vtype = what['vars'].split(':')[1]

            if self.x_ratio is not None:
                if isinstance(elements, tuple):
                    elements = list(elements)
                    elements[0] *= self.x_ratio
                    elements = tuple(elements)
                else:
                    elements.T[0] *= self.x_ratio
            if self.y_ratio is not None:
                if isinstance(elements, tuple):
                    elements = list(elements)
                    elements[1] *= self.y_ratio
                    elements = tuple(elements)
                else:
                    elements.T[1] *= self.y_ratio

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # ~~> Draw triangles and quads
            if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
                # ~~> Draw/Dump (works with triangles and quads)
                draw_mesh_2d_elements(\
                        self.plt, self.axes, self.fig, what['deco'], elements)
                x_0 = np.ravel(elements.T[0])
                y_0 = np.ravel(elements.T[1])
                if self.dpar["roi"][0] == []:
                    self.dpar["roi"][0] = [np.min(x_0), np.min(y_0),
                                           np.max(x_0), np.max(y_0)]
                else:
                    self.dpar["roi"][0] = [\
                            min(self.dpar["roi"][0][0], np.min(x_0)),
                            min(self.dpar["roi"][0][1], np.min(y_0)),
                            max(self.dpar["roi"][0][2], np.max(x_0)),
                            max(self.dpar["roi"][0][3], np.max(y_0))]
                    # TODO: colour the mesh according to cast.values

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # ~~> Extract variable data for only one time frame and one plane
            else:
                meshx, meshy, ikle3 = elements
                # ~~> Multi-variables calculations
                if len(varsors) > 1:
                    if "map" in vtype:
                        varsors = [np.sqrt(np.sum(np.power(np.dstack(\
                                                varsors[0:2])[0], 2), axis=1))]
                # ~~> Draw/Dump (multiple options possible)
                if "map3d" in vtype:
                    # Converting to float32 needed by draw_3d_surface
                    tmp = varsors[0].astype(np.float32)
                    draw_3d_surface(self.plt, self.axes, self.fig, what['deco'],
                                    (meshx, meshy, ikle3, tmp))
                elif "map" in vtype:
                    draw_coloured_tri_maps(self.plt, self.axes, self.fig,
                                           what['deco'],
                                           (meshx, meshy, ikle3, varsors[0]))
                elif "label" in vtype:
                    draw_labeled_tri_contours(self.plt, what['deco'],
                                              (meshx, meshy, ikle3, varsors[0]))
                elif "arrow" in vtype or "vector" in vtype:
                    draw_coloured_tri_vects(self.plt, self.axes, self.fig,
                                            what['deco'],
                                            (meshx, meshy, varsors, False,
                                             ikle3))
                elif "angle" in vtype:
                    draw_coloured_tri_vects(self.plt, self.axes, self.fig,
                                            what['deco'],
                                            (meshx, meshy, varsors, True,
                                             ikle3))
                else:
                    raise TelemacException(\
                      '... do not know how to draw this SELAFIN type: ' + vtype)
                x_0 = np.ravel(meshx[ikle3])
                y_0 = np.ravel(meshy[ikle3])
                if self.dpar["roi"][0] == []:
                    self.dpar["roi"][0] = [np.min(x_0), np.min(y_0),
                                           np.max(x_0), np.max(y_0)]
                else:
                    self.dpar["roi"][0] = [\
                            min(self.dpar["roi"][0][0], np.min(x_0)),
                            min(self.dpar["roi"][0][1], np.min(y_0)),
                            max(self.dpar["roi"][0][2], np.max(x_0)),
                            max(self.dpar["roi"][0][3], np.max(y_0))]

        # ~~> unknown
        else:
            raise TelemacException(\
                    '... do not know how to do this type of extraction: ' + \
                    what['type'].split(':')[1])

    def show(self):
        """
        @brief Show the figure
        """
        deco(self.plt, self.upar, self.dpar)
        self.plt.show()
        self.plt.close()

    def save(self, file_name):
        """
        @brief Save the figure into a file
        @param file_name: file to save the figure into
        """
        deco(self.plt, self.upar, self.dpar)
        try:
            stat(path.split(file_name)[0])
        except OSError:
            mkdir(path.split(file_name)[0])

        # Before saving the image, check if the user want to apply a ratio for
        # the axes.
        for axe in self.axes:
            if self.x_ratio is not None:
                locs = axe.get_xticks()
                label = [u'{}'.format(l / self.x_ratio) for l in locs]
                axe.set_xticklabels(label)
            if self.y_ratio is not None:
                locs = axe.get_yticks()
                label = [u'{}'.format(l / self.y_ratio) for l in locs]
                axe.set_yticklabels(label)

        self.plt.savefig(file_name, bbox_inches='tight')
        self.plt.close()

    # _____               ______________________________________________
    # ____/ Mesh Toolbox /_____________________________________________/
    #
    # drawMesh* applies to mesh of triangles / quads
    #   - by default this is drawn as a uni-colour wireframe
    #   - see also drawCoulouredMesh*
    # Note: triplot could be used but would not be valid for quads

    def draw_coloured_mesh_lines(self, edges, deco_user):
        pass
