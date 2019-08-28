r"""@author TELEMAC-MASCARET Consortium

    @brief
"""
from __future__ import print_function
from copy import deepcopy
from os import path, stat, mkdir
from postel.caster import Caster
from postel.mtlplots.title import Title
from postel.mtlplots.axes import Axes
from postel.mtlplots.myplot1d import map_deco_default, DECO_DEFAULT
from postel.mtlplots.myplot1d import draw_history_lines, deco
from postel.mtlplots.myplot1d import draw_polyline_lines
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from  matplotlib.lines import Line2D as matplotlib_lines_Line2D
from utils.exceptions import TelemacException

class Figure1D(Caster):
    """
    @brief Plot 1D matplotlib figure
    @param Caster:
    """
    def __init__(self, caster, plot):
        Caster.__init__(self,
                        {'object': caster.object,
                         'obdata': caster.obdata})

        myplt = deepcopy(plot)

        # ~~~ special keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for key in ['size', 'roi']:
            if key in myplt.keys():
                myplt['deco'].update({key: plot[key]})
                del myplt[key]

        # ~~~ special case for set ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
        self.axe = Axes(self.upar)

        # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # TODO: do this when you draw
        fig = plt.figure(figsize=self.mpar['figure.figsize'])

        deco_axe = {}
        # ~~> add_subplot, or ax definition
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
                self.axes.append(fig.add_subplot(nrows, ncols,
                                                 (ncols * n + 1) + col,
                                                 **deco_axe))
            # Add the title in the graph
            titre.set_title(self.axes[-1])
            # change the label of the axe
            self.axe.set_param(self.axes[-1], plegend=False)

        self.lines = []

        # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.x_ratio = None
        if 'x_ratio' in self.upar and self.upar['x_ratio'] is not None:
            self.x_ratio = float(self.upar['x_ratio'])
        self.y_ratio = None
        if 'y_ratio' in self.upar and self.upar['y_ratio'] is not None:
            self.y_ratio = float(self.upar['y_ratio'])
        # ~~> region of interes and default margins
        self.dpar.update({"roi": [[],
                                  [self.axe.param_lim['axe.emarge']['val'],
                                   self.axe.param_lim['axe.nmarge']['val'],
                                   self.axe.param_lim['axe.wmarge']['val'],
                                   self.axe.param_lim['axe.smarge']['val'],
                                  ]
                                 ]
                         }
                        )

        self.plt = plt
        self.fig = fig

    def add(self, typl, what):
        """
        Add a curve in a figure
        @param typl (string): type of data (SELAFIN, etc.)
        @param what (dico): details of the plot (file name,
                     type of plot, deco, extract zone,
                     variable to plot, etc.)
        """
        Caster.add(self, typl, what)

        if len(what['vars'].split(';')) > 1:
            raise TelemacException(\
                    '... do not know support multiple variables anymore: ' + \
                    what['vars'])

        # ~~> 1D time history from 2D or 3D results
        if what['type'].split(':')[1] == 'history' or 'sortie' in typl.lower():
            cast = self.get(typl, what)
            elements = cast.support
            varsors = cast.values
            varnames = cast.function

            valout = draw_history_lines(self.plt, self.axes, what['deco'],
                                        elements, varnames, varsors)
            x_0, y_0, line_tempo = valout
            if isinstance(line_tempo, list):
                for elem in line_tempo:
                    if isinstance(elem, matplotlib_lines_Line2D):
                        self.lines.append(elem)
            else:
                self.lines.append(line_tempo)

            if self.dpar["roi"][0] == []:
                self.dpar["roi"][0] = [np.min(x_0), np.min(y_0),
                                       np.max(x_0), np.max(y_0)]
            else:
                self.dpar["roi"][0] = [min(self.dpar["roi"][0][0], np.min(x_0)),
                                       min(self.dpar["roi"][0][1], np.min(y_0)),
                                       max(self.dpar["roi"][0][2], np.max(x_0)),
                                       max(self.dpar["roi"][0][3], np.max(y_0))]

        # ~~> 1D time history from 2D or 3D results
        elif what['type'].split(':')[1] == 'v-section' or 'csv' in typl.lower():
            cast = self.get(typl, what)
            try:  # if instance unit exist
                valout = draw_polyline_lines(self.plt, self.axes,
                                             what['deco'],
                                             (cast.unit, cast.support),
                                             (cast.function, cast.values))
                x_0, y_0, line_tempo = valout
                if isinstance(line_tempo, list):
                    for elem in line_tempo:
                        if isinstance(elem, matplotlib_lines_Line2D):
                            self.lines.append(elem)
                else:
                    self.lines.append(line_tempo)
                if self.dpar["roi"][0] == []:
                    self.dpar["roi"][0] = [np.min(x_0), np.min(y_0),
                                           np.max(x_0), np.max(y_0)]
                else:
                    self.dpar["roi"][0] = [min(self.dpar["roi"][0][0],
                                               np.min(x_0)),
                                           min(self.dpar["roi"][0][1],
                                               np.min(y_0)),
                                           max(self.dpar["roi"][0][2],
                                               np.max(x_0)),
                                           max(self.dpar["roi"][0][3],
                                               np.max(y_0))]
            except Exception:
                dim = len(cast.values.shape)
                fct = ['v-section']
                if dim > 1:
                    fct.append(['VAR' + str(ivar) \
                                    for ivar in range(cast.values.shape[0])])
                if dim > 2:
                    fct.append(
                        [str(itim) for itim in range(cast.values.shape[1])])
                if dim > 3:
                    fct.append(
                        [str(ilay) for ilay in range(cast.values.shape[2])])
                valout = draw_polyline_lines(self.plt, self.axes,
                                             what['deco'],
                                             ('unit', cast.support),
                                             (fct, cast.values))
                x_0, y_0, line_tempo = valout
                if isinstance(line_tempo, list):
                    for elem in line_tempo:
                        if isinstance(elem, matplotlib_lines_Line2D):
                            self.lines.append(elem)
                else:
                    self.lines.append(line_tempo)
                if self.dpar["roi"][0] == []:
                    self.dpar["roi"][0] = [np.min(x_0), np.min(y_0),
                                           np.max(x_0), np.max(y_0)]
                else:
                    self.dpar["roi"][0] = [min(self.dpar["roi"][0][0],
                                               np.min(x_0)),
                                           min(self.dpar["roi"][0][1],
                                               np.min(y_0)),
                                           max(self.dpar["roi"][0][2],
                                               np.max(x_0)),
                                           max(self.dpar["roi"][0][3],
                                               np.max(y_0))]

        # ~~> unkonwn
        else:
            raise TelemacException(\
                    '... do not know how to draw to this format: '
                    '{}'.format(typl))

        if self.lines is not None:
            self.axe.param_legend['legend']['val'] = self.lines
        self.axe.set_param(self.axes[0])

    def show(self):
        """
        Show the figure on the window
        @return:
        """
        deco(self.plt, self.upar, self.dpar)
        self.plt.show()
        self.plt.close()

    def save(self, file_name):
        """
        Export a graphic in a image
        @param file_name:
        @return:
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
