r"""@author TELEMAC-MASCARET Consortium

    @brief
"""
from __future__ import print_function
import sys
from copy import deepcopy
from utils.parser_strings import parse_array_frame, parse_array_paires
from utils.exceptions import TelemacException
from postel.mtlplots.title import Title
from postel.mtlplots.axes import Axes
import matplotlib as mpl
try:
    HIDE_DEFAULT = not sys.stderr.isatty()
except AttributeError:
    HIDE_DEFAULT = True  # output does not support isatty()
if HIDE_DEFAULT:
    mpl.use('Agg')  # Use of Agg must be done before importing matplotlib.pyplot


DECO_DEFAULT = {\
    "size": '(10;10)',
    "aspect": 'auto',
    "dpi": '',
    "ratio2d": '',
    "title": '',
    "roi": '',
    "type": '',
    "set": '',
    ### LINES
    # See http://matplotlib.org/api/artist_api.html#module-matplotlib.lines
    # for more information on line properties.
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
    # whether to clear the axes by default on
    # 'axes.hold'               : True,
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
    'axes.formatter.use_mathtext': False,  # When True, use mathtext for
    # scientific notation.
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
    'legend.markerscale': 1.0, # the relative size of legend markers vs original
    # the following dimensions are in axes coords
    # the vertical space between the legend entries in fraction of fontsize
    'legend.labelspacing': 0.5,
    # the length of the legend lines in fraction of fontsize
    'legend.handlelength': 2.,
    # the height of the legend handle in fraction of fontsize
    'legend.handleheight': 0.7,
    # the space between the legend line and legend text in fraction of fontsize
    'legend.handletextpad': 0.8,
    # the border between the axes and legend edge in fraction of fontsize
    'legend.borderaxespad': 0.5,
    # the border between the axes and legend edge in fraction of fontsize
    'legend.columnspacing': 2.,
    'legend.shadow': False,
    'legend.frameon': True,  # whether or not to draw a frame around legend
    'legend.scatterpoints': 3  # number of scatter points
               }

def map_deco_default(deco_user, default):
    """
        @brief Return configuration for matplot
    """
    # ~~~ melting the pot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mpar = {}  # mpar, contains the matplotlib parameters (defaults)
    upar = deepcopy(default) # upar, contains the user parameters (from the XML)
    for name in deco_user:
        if name not in ['look', 'data']:
            upar.update({name: deco_user[name]})
    for name in deco_user:
        if name in ['look', 'data']:
            upar.update(deco_user[name][0])

    # ~~~ special conversions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Replaces the relevat mpar by the upar values
    for key in list(upar):  # /!\ the .keys() is necessary
        if key in mpl.rcParams.keys():
            if type(mpl.rcParams[key]) == type(upar[key]):
                mpar[key] = deepcopy(upar[key])
                del upar[key]
            else:
                if isinstance(mpl.rcParams[key], list):
                    if isinstance(mpl.rcParams[key][0], int) or \
                        isinstance(mpl.rcParams[key][0], float):
                        mpar[key] = parse_array_frame(upar[key])
                        del upar[key]
                    elif isinstance(mpl.rcParams[key][0], str) or \
                          isinstance(mpl.rcParams[key][0], unicode):
                        print(upar[key].strip('[]'))
                        values = upar[key].strip('[]')\
                                          .replace(';', ',').split(',')
                        mpar[key] = [s.strip() for s in values]
                        del upar[key]
                    else:
                        err = '... I did not know {} for key: {}. '\
                                'Could be an acceptable type'.format(\
                            str(type(mpl.rcParams[key])), key)
                        raise TelemacException(err)
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
                    err = '... I did not know {} for key: {}. '\
                            'Could be an acceptable type'.format(\
                        str(type(mpl.rcParams[key])), key)
                    raise TelemacException(err)
        elif key == "dpi":
            if upar[key] != '':
                mpar.update({'savefig.dpi': int(upar[key])})
                mpar.update({'figure.dpi': int(upar[key])})
            del upar[key]
        elif key == "size":
            if upar[key] != '':
                mpar.update({'figure.figsize': parse_array_paires(upar[key])[0]})
            del upar[key]
        elif key == "roi":
            if upar[key] != '':
                pairs = parse_array_paires(upar[key])
                if len(pairs) == 2:
                    pairs.extend([(0., 0.), (0., 0.)])
                elif len(pairs) != 4:
                    raise TelemacException(\
                            'Could not interprete roi ({}): '
                            '{}'.format(upar[key], pairs))

                upar[key] = pairs
    return mpar, upar

def draw_history_lines(myplt, axes, deco_user, x_0, ynames, y_0):

    # Create the cariable for the title
    titre = Title(deco_user, DECO_DEFAULT)
    # create the variable for the axes
    axe = Axes(deco_user)
    # Position of the graph in the subplot
    index = 0
    if 'index' in deco_user and deco_user['index'] is not None:
        index = int(deco_user['index']) - 1
        deco_user.pop('index', None)

    line_tempo = []
    # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dim = len(ynames) - 1
    # ynames[0] is an extra meta data
    if dim == 1:
        n_0 = ynames[1]
        for i_0 in range(len(n_0)):  # each variables
            axes[index].set_ylabel(n_0[i_0])
            l = axes[index].plot(x_0, y_0[i_0], **deco_user)
            if isinstance(l, list):
                line_tempo.append(l[0])
            else:
                line_tempo.append(l)
    elif dim == 2:
        # ~~> This is called for 1d:history, for instance
        n_0, n_1 = ynames[1:]
        for i_1 in range(len(n_1)):  # each location
            for i_0 in range(len(n_0)):  # each variables
                # myplt.ylabel(str(n_1[])) you could label each curve in
                # time and plan
                l = axes[index].plot(x_0, y_0[i_0][i_1], **deco_user)
                if isinstance(l, list):
                    line_tempo.append(l[0])
                else:
                    line_tempo.append(l)
    elif dim == 3:
        # ~~> This is called for 1d:v-section, for instance
        n_0, n_1, n_2 = ynames[1:]
        for i_2 in range(len(n_2)):  # each plan
            for i_1 in range(len(n_1)):  # each time
                for i_0 in range(len(n_0)):  # each variables
                    # myplt.ylabel(str(n_1[])) you could label each curve
                    # in time and plan
                    l = axes[index].plot(x_0, y_0[i_0][i_1][i_2], **deco_user)
                    if isinstance(l, list):
                        line_tempo.append(l[0])
                    else:
                        line_tempo.append(l)
    elif dim == 4:
        n_0, n_1, n_2, n_3 = ynames[1:]
        for i_3 in range(len(n_3)):
            for i_2 in range(len(n_2)):
                for i_1 in range(len(n_1)):
                    for i_0 in range(len(n_0)):
                        # myplt.ylabel(str(n_1[]))
                        l = axes[index].plot(x_0, y_0[i_0][i_1][i_2][i_3],
                                             **deco_user)
                        if type(l) is list:
                            line_tempo.append(l[0])
                        else:
                            line_tempo.append(l)

    # Add the title in the graph
    titre.set_title(axes[index])
    # change the label of the axe
    axe.set_param(axes[index])
    return x_0, y_0, line_tempo


def draw_polyline_lines(myplt, axes, deco_user, x, y_s):

    # Create the cariable for the title
    titre = Title(deco_user, DECO_DEFAULT)
    # create the variable for the axes
    axe = Axes(deco_user)

    # Position of the graph in the subplot
    index = 0
    if 'index' in deco_user and deco_user['index'] is not None:
        index = int(deco_user['index']) - 1
        deco_user.pop('index', None)

    line_tempo = []
    # ~~ Data draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    _, x_0 = x
    ynames, y_0 = y_s
    dim = len(ynames) - 1
    time_in_label = False
    label = None
    if 'label' in deco_user and deco_user['label'] is not None:
        label = deco_user['label']
    if 'time_in_label' in deco_user and deco_user['time_in_label'] is not None \
        and deco_user['time_in_label']:
        time_in_label = True
        deco_user.pop('time_in_label', None)
        if label is not None:
            label_ref = label
    fill_min = None
    if 'fill_min' in deco_user and deco_user['fill_min'] is not None:
        fill_min = float(deco_user['fill_min'])
        deco_user.pop('fill_min', None)

    # ynames[0] is an extra meta data
    if dim == 1:
        n_0 = ynames[1]
        for i_0 in range(len(n_0)):  # each variables
            myplt.ylabel(n_0[i_0])
            if 'fill_between' in deco_user\
               and deco_user['fill_between'] is not None:
                deco_user.pop('fill_between', None)
                if fill_min is not None:
                    axes[index].fill_between(x_0, y_0[i_0], fill_min,
                                             **deco_user)
                else:
                    axes[index].fill_between(x_0, y_0[i_0], min(y_0[i_0]),
                                             **deco_user)
            else:
                axes[index].plot(x_0, y_0[i_0], **deco_user)
    elif dim == 2:
        n_0, n_1 = ynames[1:]
        for i_1 in range(len(n_1)):  # each time
            for i_0 in range(len(n_0)):  # each variables
                # myplt.ylabel(n_0[i])
                if label is not None:
                    if time_in_label:
                        label = label_ref.format(n_1[i_1])
                if 'fill_between' in deco_user and \
                    deco_user['fill_between'] is not None and \
                    deco_user['fill_between']:

                    deco_user.pop('fill_between', None)
                    if fill_min is not None:
                        l = axes[index].fill_between(x_0, y_0[i_0][i_1],
                                                     fill_min, **deco_user)
                    else:
                        l = axes[index].fill_between(\
                                x_0, y_0[i_0][i_1], min(y_0[i_0][i_1]),
                                **deco_user)
                else:
                    l = axes[index].plot(x_0, y_0[i_0][i_1], **deco_user)
                if type(l) is list:
                    line_tempo.append(l[0])
                else:
                    line_tempo.append(l)
                    # myplot.plot[i].updte()
    elif dim == 3:
        # ~~> This is called for 1d:v-section, for instance
        n_0, n_1, n_2 = ynames[1:]
        for i_2 in range(len(n_2)):  # each plan
            for i_1 in range(len(n_1)):  # each time
                for i_0 in range(len(n_0)):  # each variables
                    # myplt.ylabel(str(n_1[])) you could label each
                    # curve in time and plan
                    if label is not None:
                        if time_in_label:
                            label = label_ref.format(n_1[i_1])
                    if 'fill_between' in deco_user and \
                        deco_user['fill_between'] is not None and \
                        deco_user['fill_between']:

                        deco_user.pop('fill_between', None)
                        if fill_min is not None:
                            l = axes[index].fill_between(\
                                    x_0, y_0[i_0][i_1][i_2], fill_min,
                                    **deco_user)
                        else:
                            l = axes[index].fill_between(\
                                    x_0, y_0[i_0][i_1][i_2],
                                    min(y_0[i_0][i_1][i_2]),
                                    **deco_user)
                    else:
                        l = axes[index].plot(x_0, y_0[i_0][i_1][i_2],
                                             **deco_user)
                    if type(l) is list:
                        line_tempo.append(l[0])
                    else:
                        line_tempo.append(l)
    elif dim == 4:
        n_0, n_1, n_2, n_3 = ynames[1:]
        for i_3 in range(len(n_3)):
            for i_2 in range(len(n_2)):
                for i_1 in range(len(n_1)):
                    for i_0 in range(len(n_0)):
                        # myplt.ylabel(str(n_1[]))
                        if label is not None:
                            if time_in_label:
                                label = label_ref.format(n_1[i_1])
                        if 'fill_between' in deco_user and \
                            deco_user['fill_between'] is not None and \
                            deco_user['fill_between']:

                            deco_user.pop('fill_between', None)
                            if fill_min is not None:
                                l = axes[index].fill_between(\
                                        x_0, y_0[i_0][i_1][i_2][i_3],
                                        fill_min, **deco_user)
                            else:
                                l = axes[index].fill_between(\
                                        x_0, y_0[i_0][i_1][i_2][i_3],
                                        min(y_0[i_0][i_1][i_2][i_3]),
                                        **deco_user)
                        else:
                            l = axes[index].plot(x_0, y_0[i_0][i_1][i_2][i_3],
                                                 **deco_user)
                        if type(l) is list:
                            line_tempo.append(l[0])
                        else:
                            line_tempo.append(l)

    # Add the title in the graph
    titre.set_title(axes[index])
    # change the label of the axe
    axe.set_param(axes[index])
    return x_0, y_0, line_tempo

def deco(myplt, upar, dpar):
    # ~~ Ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # crax.axis('equal')         # sets both axis scale to be equal
    # crax = myplt.gca()
    # crax.set_aspect('equal')

    # ~~ General keys ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if upar['set'] != '':
        for iset in upar['set'].split(';'):
            if iset[-1] != ')':
                iset += '()'
            eval("myplt." + iset)

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
