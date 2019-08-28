"""@author David H. Roscoe and Sebastien E. Bourban
"""
"""@note ... this work is based on a collaborative effort between
  .________.                                                          ,--.
  |        |                                                      .  (  (
  |,-.    /   HR Wallingford                EDF - LNHE           / \_ \_/ .--.
  /   \  /    Howbery Park,                 6, quai Watier       \   )   /_   )
   ,.  `'     Wallingford, Oxfordshire      78401 Cedex           `-'_  __ `--
  /  \   /    OX10 8BA, United Kingdom      Chatou, France        __/ \ \ `.
 /    `-'|    www.hrwallingford.com         innovation.edf.com   |    )  )  )
!________!                                                        `--'   `--
"""
"""@history 30/08/2011 -- Sebastien E. Bourban
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path, stat, mkdir
from copy import deepcopy
import numpy as np
# ~~> matplotlib and pyplot
import matplotlib as mpl
from  matplotlib.lines import Line2D as matplotlib_lines_Line2D

try:
   hide_default = not sys.stderr.isatty()
except AttributeError:
   hide_default = True  # output does not support isatty()
if hide_default:
   mpl.use('Agg')  # Use of Agg must be done before importing matplotlib.pyplot
import matplotlib.pyplot as plt
# ~~> dependencies towards other mtlplots
# ~~> dependencies towards other pytel/modules
from samplers.mycast import Caster
from parsers.parserCSV import CSV
from parsers.parserStrings import parseArrayFrame, parseArrayPaires
from param_Axe import Title, ColorBar, Quiver, Axes

# _____               ______________________________________________
# ____/ Default DECO /_____________________________________________/
#

decoDefault = {
   "size": '(10;10)', "aspect": '1', "dpi": '', "ratio2d": '', "title": '', "roi": '', "type": '', "set": '',
   ### LINES
   # See http://matplotlib.org/api/artist_api.html#module-matplotlib.lines for more
   # information on line properties.
   'lines.linewidth': 1.0,  # line width in points
   'lines.linestyle': '-',  # solid line
   'lines.color': 'blue',  # has no affect on plot(); see axes.color_cycle
   'lines.marker': None,  # the default marker
   'lines.markeredgewidth': 0.5,  # the line width around the marker symbol
   'lines.markersize': 6,  # markersize, in points
   'lines.dash_joinstyle': 'miter',  # miter|round|bevel
   'lines.dash_capstyle': 'butt',  # butt|round|projecting
   'lines.solid_joinstyle': 'miter',  # miter|round|bevel
   'lines.solid_capstyle': 'projecting',  # butt|round|projecting
   'lines.antialiased': True,  # render lines in antialised (no jaggies)
   ### AXES
   # default face and edge color, default tick sizes,
   # default fontsizes for ticklabels, and so on.  See
   # http://matplotlib.org/api/axes_api.html#module-matplotlib.axes
   # 'axes.hold'               : True,           # whether to clear the axes by default on
   'axes.facecolor': 'white',  # axes background color
   'axes.edgecolor': 'black',  # axes edge color
   'axes.linewidth': 1.0,  # edge linewidth
   'axes.grid': False,  # display grid or not
   'axes.titlesize': 16,  # fontsize of the axes title
   'axes.labelsize': 14,  # fontsize of the x any y labels
   'axes.labelweight': 'normal',  # weight of the x and y labels
   'axes.labelcolor': 'black',
   'axes.axisbelow': False,  # whether axis gridlines and ticks are below
   # the axes elements (lines, text, etc)
   'axes.formatter.limits': [-7, 7],  # use scientific notation if log10
   # of the axis range is smaller than the
   # first or larger than the second
   'axes.formatter.use_locale': False,  # When True, format tick labels
   # according to the user's locale.
   # For example, use ',' as a decimal
   # separator in the fr_FR locale.
   'axes.formatter.use_mathtext': False,  # When True, use mathtext for scientific
   # notation.
   'axes.unicode_minus': True,  # use unicode for the minus symbol
   # rather than hyphen.  See
   # http://en.wikipedia.org/wiki/Plus_and_minus_signs#Character_codes
   'axes.color_cycle': ['b', 'g', 'r', 'c', 'm', 'y', 'k'],
   # color cycle for plot lines
   # as list of string colorspecs:
   # single letter, long name, or
   # web-style hex
   'axes.xmargin': 0,  # x margin.  See `axes.Axes.margins`
   'axes.ymargin': 0,  # y margin See `axes.Axes.margins`
   'polaraxes.grid': True,  # display grid on polar axes
   'axes3d.grid': True,  # display grid on 3d axes
   ### TICKS
   # see http://matplotlib.org/api/axis_api.html#matplotlib.axis.Tick
   'xtick.major.size': 4,  # major tick size in points
   'xtick.minor.size': 2,  # minor tick size in points
   'xtick.major.width': 0.5,  # major tick width in points
   'xtick.minor.width': 0.5,  # minor tick width in points
   'xtick.major.pad': 4,  # distance to major tick label in points
   'xtick.minor.pad': 4,  # distance to the minor tick label in points
   'xtick.color': 'k',  # color of the tick labels
   'xtick.labelsize': 12,  # fontsize of the tick labels
   'xtick.direction': 'in',  # direction: in, out, or inout
   'ytick.major.size': 4,  # major tick size in points
   'ytick.minor.size': 2,  # minor tick size in points
   'ytick.major.width': 0.5,  # major tick width in points
   'ytick.minor.width': 0.5,  # minor tick width in points
   'ytick.major.pad': 4,  # distance to major tick label in points
   'ytick.minor.pad': 4,  # distance to the minor tick label in points
   'ytick.color': 'k',  # color of the tick labels
   'ytick.labelsize': 12,  # fontsize of the tick labels
   'ytick.direction': 'in',  # direction: in, out, or inout
   ### GRIDS
   'grid.color': 'black',  # grid color
   'grid.linestyle': ':',  # dotted
   'grid.linewidth': 0.5,  # in points
   'grid.alpha': 1.0,  # transparency, between 0.0 and 1.0
   ### LEGEND
   'legend.fancybox': False,  # if True, use a rounded box for the
   # legend, else a rectangle
   'legend.isaxes': True,
   'legend.numpoints': 2,  # the number of points in the legend line
   'legend.fontsize': 14,
   'legend.borderpad': 0.5,  # border whitespace in fontsize units
   'legend.markerscale': 1.0,  # the relative size of legend markers vs. original
   # the following dimensions are in axes coords
   'legend.labelspacing': 0.5,  # the vertical space between the legend entries in fraction of fontsize
   'legend.handlelength': 2.,  # the length of the legend lines in fraction of fontsize
   'legend.handleheight': 0.7,  # the height of the legend handle in fraction of fontsize
   'legend.handletextpad': 0.8,  # the space between the legend line and legend text in fraction of fontsize
   'legend.borderaxespad': 0.5,  # the border between the axes and legend edge in fraction of fontsize
   'legend.columnspacing': 2.,  # the border between the axes and legend edge in fraction of fontsize
   'legend.shadow': False,
   'legend.frameon': True,  # whether or not to draw a frame around legend
   'legend.scatterpoints': 3  # number of scatter points
}


# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def mapDecoDefault(decoUser, default):
   # ~~~ melting the pot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mpar = {}  # mpar, contains the matplotlib parameters (defaults)
   upar = deepcopy(default)  # upar, contains the user parameters (from the XML)
   for name in decoUser:
      if name not in ['look', 'data']:
         upar.update({name: decoUser[name]})
   for name in decoUser:
      if name in ['look', 'data']:
         upar.update(decoUser[name][0])

   # ~~~ special conversions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Replaces the relevat mpar by the upar values
   for key in upar.keys():  # /!\ the .keys() is necessary
      if key in mpl.rcParams.keys():
         if type(mpl.rcParams[key]) == type(upar[key]):
            mpar[key] = deepcopy(upar[key])
            del upar[key]
         else:
            if isinstance(mpl.rcParams[key], list):
               if isinstance(mpl.rcParams[key][0], int) or isinstance(mpl.rcParams[key][0], float):
                  mpar[key] = parseArrayFrame(upar[key])
                  del upar[key]
               elif isinstance(mpl.rcParams[key][0], str) or isinstance(mpl.rcParams[key][0], unicode):
                  print(upar[key].strip('[]'))
                  mpar[key] = [s.strip() for s in upar[key].strip('[]').replace(';', ',').split(',')]
                  del upar[key]
               else:
                  err = '... I did not know {} for key: {}. Could be an acceptable type'.format(
                     str(type(mpl.rcParams[key])))
                  print(err)
                  sys.exit(1)
            elif isinstance(mpl.rcParams[key], bool):
               mpar[key] = (upar[key].lower() == 'true')
               del upar[key]
            elif isinstance(mpl.rcParams[key], int):
               mpar[key] = int(upar[key])
               del upar[key]
            elif isinstance(mpl.rcParams[key], float):
               mpar[key] = float(upar[key])
               del upar[key]
            elif isinstance(mpl.rcParams[key], str):
               mpar[key] = upar[key]
               del upar[key]
            elif mpl.rcParams[key] is None:
               mpar[key] = upar[key]
               del upar[key]
            elif isinstance(mpl.rcParams[key][0], unicode):
               mpar[key] = upar[key]
               del upar[key]
            else:
               err = '... I did not know {} for key: {}. Could be an acceptable type'.format(
                  str(type(mpl.rcParams[key])))
               print(err)
               sys.exit(1)
      elif key == "dpi":
         if upar[key] != '':
            mpar.update({'savefig.dpi': int(upar[key])})
            mpar.update({'figure.dpi': int(upar[key])})
         del upar[key]
      elif key == "size":
         if upar[key] != '':
            mpar.update({'figure.figsize': parseArrayPaires(upar[key])[0]})
         del upar[key]
      elif key == "roi":
         if upar[key] != '':
            pairs = parseArrayPaires(upar[key])
            if len(pairs) == 2:
               pairs.extend([(0., 0.), (0., 0.)])
            elif len(pairs) != 4:
               print('... could not interprete roi (' + upar[key] + '): ' + pairs)
               sys.exit(1)
            upar[key] = pairs

   return mpar, upar


# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def drawHistoryLines(myplt, ax, decoUser, x, ys):
   axes = myplt.axis()
   line_tempo = []
   label_tempo = []
   # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xname, x0 = x
   ynames, y0 = ys
   dim = len(ynames) - 1
   # ynames[0] is an extra meta data
   if dim == 1:
      n0 = ynames[1]
      for i0 in range(len(n0)):  # each variables
         ax.set_ylabel(n0[i0])
         l = ax.plot(x0, y0[i0], **decoUser)
         if type(l) is list:
            line_tempo.append(l[0])
         else:
            line_tempo.append(l)
         if 'label' in decoUser and decoUser['label'] is not None:
            label_tempo.append(decoUser['label'])
   elif dim == 2:
      # ~~> This is called for 1d:history, for instance
      n0, n1 = ynames[1:]
      for i1 in range(len(n1)):  # each location
         for i0 in range(len(n0)):  # each variables
            # myplt.ylabel(str(n1[])) you could label each curve in time and plan
            l = ax.plot(x0, y0[i0][i1], **decoUser)
            if type(l) is list:
               line_tempo.append(l[0])
            else:
               line_tempo.append(l)
            if 'label' in decoUser and decoUser['label'] is not None:
               label_tempo.append(decoUser['label'])
   elif dim == 3:
      # ~~> This is called for 1d:v-section, for instance
      n0, n1, n2 = ynames[1:]
      for i2 in range(len(n2)):  # each plan
         for i1 in range(len(n1)):  # each time
            for i0 in range(len(n0)):  # each variables
               # myplt.ylabel(str(n1[])) you could label each curve in time and plan
               l = ax.plot(x0, y0[i0][i1][i2], **decoUser)
               if type(l) is list:
                  line_tempo.append(l[0])
               else:
                  line_tempo.append(l)
               if 'label' in decoUser and decoUser['label'] is not None:
                  label_tempo.append(decoUser['label'])
   elif dim == 4:
      n0, n1, n2, n3 = ynames[1:]
      for i3 in range(len(n3)):
         for i2 in range(len(n2)):
            for i1 in range(len(n1)):
               for i0 in range(len(n0)):
                  # myplt.ylabel(str(n1[]))
                  l = ax.plot(x0, y0[i0][i1][i2][i3], **decoUser)
                  if type(l) is list:
                     line_tempo.append(l[0])
                  else:
                     line_tempo.append(l)
                  if 'label' in decoUser and decoUser['label'] is not None:
                     label_tempo.append(decoUser['label'])

   return x0, y0, line_tempo, label_tempo


def drawPolylineLines(myplt, ax, decoUser, x, ys):
   line_tempo = []
   label_tempo = []
   # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xname, x0 = x
   ynames, y0 = ys
   dim = len(ynames) - 1
   time_in_label = False
   label = None
   if 'label' in decoUser and decoUser['label'] is not None:
      label = decoUser['label']
   if 'time_in_label' in decoUser and decoUser['time_in_label'] is not None and decoUser['time_in_label']:
      time_in_label = True
      decoUser.pop('time_in_label', None)
      if label is not None:
         label_ref = label
   fill_min = None
   if 'fill_min' in decoUser and decoUser['fill_min'] is not None:
      fill_min = float(decoUser['fill_min'])
      decoUser.pop('fill_min', None)

   # ynames[0] is an extra meta data
   if dim == 1:
      n0 = ynames[1]
      for i0 in range(len(n0)):  # each variables
         myplt.ylabel(n0[i0])
         if label is not None:
            label_tempo.append(label)
         if 'fill_between' in decoUser and decoUser['fill_between'] is not None:
            decoUser.pop('fill_between', None)
            if fill_min is not None:
               ax.fill_between(x0, y0[i0], fill_min, **decoUser)
            else:
               ax.fill_between(x0, y0[i0], min(y0[i0]), **decoUser)
         else:
            ax.plot(x0, y0[i0], **decoUser)
   elif dim == 2:
      n0, n1 = ynames[1:]
      for i1 in range(len(n1)):  # each time
         for i0 in range(len(n0)):  # each variables
            # myplt.ylabel(n0[i])
            if label is not None:
               if time_in_label:
                  label = label_ref.format(n1[i1])
               label_tempo.append(label)
               decoUser['label'] = label
            if 'fill_between' in decoUser and decoUser['fill_between'] is not None and decoUser['fill_between']:
               decoUser.pop('fill_between', None)
               if fill_min is not None:
                  l = ax.fill_between(x0, y0[i0][i1], fill_min, **decoUser)
               else:
                  l = ax.fill_between(x0, y0[i0][i1], min(y0[i0][i1]), **decoUser)
            else:
               l = ax.plot(x0, y0[i0][i1], **decoUser)
            if type(l) is list:
               line_tempo.append(l[0])
            else:
               line_tempo.append(l)
               # myplot.plot[i].updte()
   elif dim == 3:
      # ~~> This is called for 1d:v-section, for instance
      n0, n1, n2 = ynames[1:]
      for i2 in range(len(n2)):  # each plan
         for i1 in range(len(n1)):  # each time
            for i0 in range(len(n0)):  # each variables
               # myplt.ylabel(str(n1[])) you could label each curve in time and plan
               if label is not None:
                  if time_in_label:
                     label = label_ref.format(n1[i1])
                  label_tempo.append(label)
                  decoUser['label'] = label
               if 'fill_between' in decoUser and decoUser['fill_between'] is not None and decoUser['fill_between']:
                  decoUser.pop('fill_between', None)
                  if fill_min is not None:
                     l = ax.fill_between(x0, y0[i0][i1][i2], fill_min, **decoUser)
                  else:
                     l = ax.fill_between(x0, y0[i0][i1][i2], min(y0[i0][i1][i2]), **decoUser)
               else:
                  l = ax.plot(x0, y0[i0][i1][i2], **decoUser)
               if type(l) is list:
                  line_tempo.append(l[0])
               else:
                  line_tempo.append(l)
   elif dim == 4:
      n0, n1, n2, n3 = ynames[1:]
      for i3 in range(len(n3)):
         for i2 in range(len(n2)):
            for i1 in range(len(n1)):
               for i0 in range(len(n0)):
                  # myplt.ylabel(str(n1[]))
                  if label is not None:
                     if time_in_label:
                        label = label_ref.format(n1[i1])
                     label_tempo.append(label)
                     decoUser['label'] = label
                  if 'fill_between' in decoUser and decoUser['fill_between'] is not None and decoUser['fill_between']:
                     decoUser.pop('fill_between', None)
                     if fill_min is not None:
                        l = ax.fill_between(x0, y0[i0][i1][i2][i3], fill_min, **decoUser)
                     else:
                        l = ax.fill_between(x0, y0[i0][i1][i2][i3], min(y0[i0][i1][i2][i3]), **decoUser)
                  else:
                     l = ax.plot(x0, y0[i0][i1][i2][i3], **decoUser)
                  if type(l) is list:
                     line_tempo.append(l[0])
                  else:
                     line_tempo.append(l)
                  if 'label' in decoUser and decoUser['label'] is not None:
                     label_tempo.append(decoUser['label'])

   return x0, y0, line_tempo, label_tempo


# _____                      _______________________________________
# ____/ Primary Method:Deco /______________________________________/
#

def deco(myplt, upar, dpar):
   # ~~ General keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if upar['set'] != '':
      for set in upar['set'].split(';'):
         if set[-1] != ')': set += '()'
         eval("myplt." + set)

   # ~~ Axis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if upar["roi"] != '':
      (xmin, ymin), (xmax, ymax), (emar, nmar), (wmar, smar) = upar["roi"]
      xgap = xmax - xmin
      xmin -= wmar * xgap
      xmax += emar * xgap
      ygap = ymax - ymin
      ymin -= smar * ygap
      ymax += nmar * ygap
      myplt.axis([xmin, xmax, ymin, ymax])
   elif dpar["roi"] != '':
      roi, mar = dpar["roi"]
      xmin, ymin, xmax, ymax = roi
      emar, nmar, wmar, smar = mar
      xgap = xmax - xmin
      xmin -= wmar * xgap
      xmax += emar * xgap
      ygap = ymax - ymin
      ymin -= smar * ygap
      ymax += nmar * ygap
      myplt.axis([xmin, xmax, ymin, ymax])

   return


# _____                         ____________________________________
# ____/ Primary Classes:Dumper /___________________________________/
#

class Dumper1D(Caster):
   def __init__(self, caster, dump):
      Caster.__init__(self, {'object': caster.object, 'obdata': caster.obdata})
      self.obtype = dump['saveas']
      self.oudata = None

   def add(self, typl, what):
      Caster.add(self, typl, what)

      # ~~> only csv is recognised fr now
      if self.obtype != 'csv':  # TODO: raise exception
         print('... do not know how to write to this format: ' + self.obtype)
         sys.exit(1)

      # ~~> initialisation
      if not self.oudata: self.oudata = CSV()

      # ~~> write-up
      cast = self.get(typl, what)

      # ~~> 1D time history from 2D or 3D results
      if what['type'].split(':')[1] == 'history' or 'sortie' in typl.lower():
         # try: # if instance unit exist
         self.oudata.addColumns((cast.unit, cast.support), (cast.function, cast.values))
         # except:
         #   self.oudata.addColumns(( 'unit',cast.support ),( ('history','function'),cast.values ))

      # ~~> 1D vertical cross section from 2D or 3D results
      elif what['type'].split(':')[1] == 'v-section' or 'csv' in typl.lower():
         # cast = self.get(typl,what)
         try:  # if instance unit exist
            self.oudata.addColumns((cast.unit, cast.support), (cast.function, cast.values))
         except:
            dim = len(cast.values.shape)
            fct = ['v-section']
            if dim > 1: fct.append(['VAR' + str(ivar) for ivar in range(cast.values.shape[0])])
            if dim > 2: fct.append([str(itim) for itim in range(cast.values.shape[1])])
            if dim > 3: fct.append([str(ilay) for ilay in range(cast.values.shape[2])])
            self.oudata.addColumns(('unit', cast.support), (fct, cast.values))

      # ~~> unkonwn
      else:  # TODO: raise exception
         print('... do not know how to extract from this format: ' + typl)
         sys.exit(1)

   def save(self, fileName):
      self.oudata.putFileContent(fileName)


# _____                          ___________________________________
# ____/ Primary Classes: Figure /__________________________________/
#
class Figure1D(Caster):
   def __init__(self, caster, plot):
      Caster.__init__(self, {'object': caster.object, 'obdata': caster.obdata})

      # ~~~ special keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for key in ['size', 'roi']:
         if key in plot.keys():
            plot['deco'].update({key: plot[key]})
            del plot[key]

      # ~~~ special case for set ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if 'set' in plot.keys():
         if 'look' in plot['deco'].keys():
            for l in plot['deco']['look']:
               if 'set' in l:
                  if plot['set'] not in l['set'].split(';'):
                     l['set'] = plot['set'] + ';' + l['set']
               else:
                  l.update({'set': plot['set']})
         else:
            plot['deco'].update({'look': [{'set': plot['set']}]})
         del plot['set']

      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self.dpar = {}
      self.mpar, self.upar = mapDecoDefault(plot['deco'], decoDefault)
      mpl.rcParams.update(self.mpar)
      # ~~> by default, there is no grid in 1D
      # self.mpar.update({ 'grid.alpha':1.0 })

      # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      fig = plt.figure()
      # fig.add_subplot(111) returns an Axes instance, where we can plot and this is
      #   also the reason why we call the variable referring to that instance ax.
      #   This is a common way to add an Axes to a Figure, but add_subplot() does a
      #   bit more: it adds a subplot. So far we have only seen a Figure with one Axes
      #   instance, so only one area where we can draw, but Matplotlib allows more than one

      # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # mpl.rcdefaults() C. COULET put in comment otherwise it cancel the update in line 415

      # ~~> type of plot
      if self.upar['type'] != '':
         if self.upar['type'][1] == 'line' or self.upar['type'][1] == 'rose':
            self.upar['adjustable'] = 'datalim'
         else:
            if 'aspect' not in self.upar.keys():
               self.upar['aspect'] = 'equal'
      else:
         if 'aspect' not in self.upar.keys():
            self.upar['aspect'] = 'equal'

      # Create the cariable for the title
      titre = Title(self.upar, decoDefault)
      # create the variable for the axes
      self.param_axe = Axes(self.upar)

      # ~~> add_subplot, or ax definition
      nrows = 1
      ncols = 1
      index = 1
      if 'nrows' in self.upar:
         nrows = int(self.upar['nrows'])
         self.upar.pop('nrows', None)
      if 'ncols' in self.upar:
         ncols = int(self.upar['ncols'])
         self.upar.pop('ncols', None)
      if 'index' in self.upar:
         index = index(self.upar['index'])
         self.upar.pop('index', None)
      self.ax = []
      for n in range(nrows):
         self.ax.append(fig.add_subplot(nrows, ncols, n + 1))
         # Add the title in the graph
         titre.set_title(self.ax[-1])
         # change the label of the axe
         self.param_axe.set_param(self.ax[-1], plegend=False)
      # ~~> default region of interes and default margins
      self.dpar.update({"roi": [[], [0.02, 0.02, 0.02, 0.02]]})

      self.lines = []
      self.label = []

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

      if len(what['vars'].split(';')) > 1:  # TODO: raise exception
         print('... do not know support multiple variables anymore: ' + what['vars'])
         sys.exit(1)

      # ~~> 1D time history from 2D or 3D results
      if what['type'].split(':')[1] == 'history' or 'sortie' in typl.lower():
         cast = self.get(typl, what)
         # try: # if instance unit exist
         valout = drawHistoryLines(self.plt, self.ax[0], what['deco'], (cast.unit, cast.support), (cast.function, cast.values))
         x0, y0, line_tempo, label_tempo = valout
         self.label.extend(label_tempo)
         if isinstance(line_tempo, list):
            for elem in line_tempo:
               if isinstance(elem, matplotlib_lines_Line2D):
                  self.lines.append(elem)
         else:
            self.lines.append(line_tempo)
         if self.dpar["roi"][0] == []:
            self.dpar["roi"][0] = [np.min(x0), np.min(y0), np.max(x0), np.max(y0)]
         else:
            self.dpar["roi"][0] = [min(self.dpar["roi"][0][0], np.min(x0)), min(self.dpar["roi"][0][1], np.min(y0)),
                                   max(self.dpar["roi"][0][2], np.max(x0)), max(self.dpar["roi"][0][3], np.max(y0))]
            # except:
            #   x0,y0 = drawHistoryLines(self.plt,( 'unit',cast.support ),( ('history','function'),cast.values ))
            #   deco(self.plt,self.upar,x0,y0)

      # ~~> 1D time history from 2D or 3D results
      elif what['type'].split(':')[1] == 'v-section' or 'csv' in typl.lower():
         cast = self.get(typl, what)
         try:  # if instance unit exist
            valout = drawPolylineLines(self.plt, self.ax[0], what['deco'],
                                       (cast.unit, cast.support),
                                       (cast.function, cast.values))
            x0, y0, line_tempo, label_tempo = valout
            self.label.extend(label_tempo)
            if isinstance(line_tempo, list):
               for elem in line_tempo:
                  if isinstance(elem, matplotlib_lines_Line2D):
                     self.lines.append(elem)
            else:
               self.lines.append(line_tempo)
            if self.dpar["roi"][0] == []:
               self.dpar["roi"][0] = [np.min(x0), np.min(y0), np.max(x0), np.max(y0)]
            else:
               self.dpar["roi"][0] = [min(self.dpar["roi"][0][0], np.min(x0)),
                                      min(self.dpar["roi"][0][1], np.min(y0)),
                                      max(self.dpar["roi"][0][2], np.max(x0)),
                                      max(self.dpar["roi"][0][3], np.max(y0))]
         except:
            dim = len(cast.values.shape)
            fct = ['v-section']
            if dim > 1:
               fct.append(['VAR' + str(ivar) for ivar in range(cast.values.shape[0])])
            if dim > 2:
               fct.append([str(itim) for itim in range(cast.values.shape[1])])
            if dim > 3:
               fct.append([str(ilay) for ilay in range(cast.values.shape[2])])
            valout = drawPolylineLines(self.plt, self.ax[0], what['deco'],
                                       ('unit', cast.support),
                                       (fct, cast.values))
            x0, y0, line_tempo, label_tempo = valout
            self.label.extend(label_tempo)
            if isinstance(line_tempo, list):
               for elem in line_tempo:
                  if isinstance(elem, matplotlib_lines_Line2D):
                     self.lines.append(elem)
            else:
               self.lines.append(line_tempo)
            if self.dpar["roi"][0] == []:
               self.dpar["roi"][0] = [np.min(x0), np.min(y0), np.max(x0), np.max(y0)]
            else:
               self.dpar["roi"][0] = [min(self.dpar["roi"][0][0], np.min(x0)),
                                      min(self.dpar["roi"][0][1], np.min(y0)),
                                      max(self.dpar["roi"][0][2], np.max(x0)),
                                      max(self.dpar["roi"][0][3], np.max(y0))]

      # ~~> unkonwn
      else:  # TODO: raise exception
         print('... do not know how to draw from this format: ' + typl)
         sys.exit(1)

   def show(self):
      """
      Show the figure on the window
      @return:
      """
      deco(self.plt, self.upar, self.dpar)
      self.plt.show()
      self.plt.close()

   def save(self, fileName):
      """
      Export a graphic in a image
      @param fileName:
      @return:
      """

      deco(self.plt, self.upar, self.dpar)
      try:
         stat(path.split(fileName)[0])
      except:
         mkdir(path.split(fileName)[0])

      if self.label is not None:
         self.param_axe.paramLegend['legend.label']['val'] = self.label
      if self.lines is not None:
         self.param_axe.paramLegend['legend']['val'] = self.lines
      res = self.param_axe.set_param(self.ax[0])

      dico_extra = {}
      if self.param_axe.paramLegend['legend']['val'] is not None and self.param_axe.paramLegend['legend']['val']:
         dico_extra['bbox_extra_artists'] = (res['legend'],)
         dico_extra['bbox_inches'] = 'tight'

      self.plt.savefig(fileName, **dico_extra)
      self.plt.close()


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__ = "David H. Roscoe; Sebastien E. Bourban"
__date__ = "$19-Jul-2011 08:51:29$"

if __name__ == "__main__":
   sys.exit(0)
