r"""@author TELEMAC-MASCARET Consortium
    @brief
"""
from __future__ import print_function
from copy import deepcopy
# ~~> dependencies towards other mtlplots
from postel.mtlplots.get_colour_map import get_colour_map, HRWD
from postel.mtlplots.quiver import Quiver
from postel.mtlplots.title import Title
from postel.mtlplots.axes import Axes
from postel.mtlplots.color_bar import ColorBar
from utils.parser_strings import parse_array_frame, parse_array_paires
from utils.exceptions import TelemacException
import numpy as np
import matplotlib as mpl
import matplotlib.tri as tri
import matplotlib.collections as collections

# _____               ______________________________________________
# ____/ Default DECO /_____________________________________________/
#

DECO_DEFAULT = {\
    "size": '(10;10)',
    "aspect": 'auto',
    "dpi": '',
    "ratio2d": '',
    "title": '',
    "roi": '',
    "type": '',
    "set": '',
    "background": '(1.0,0.90196,0.6)',
    ### LINES
    # See http://matplotlib.org/api/artist_api.html#module-matplotlib.lines for
    # more information on line properties.
    'lines.linewidth': 0.5,  # line width in points
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
    'axes.labelsize': 12,  # fontsize of the x any y labels
    'axes.labelweight': 10,  # weight of the x and y labels
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
    # When True, use mathtext for scientific
    'axes.formatter.use_mathtext': False,
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
    # the relative size of legend markers vs. original
    'legend.markerscale': 1.0,
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


# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def map_deco_default(deco_user, default):
    """
    @brief Configure with user parameter or the default
    @param deco_user: user parameter
    @param default : default parameter
    """
    # ~~~ melting the pot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mpar = {}  # mpar, contains the matplotlib parameters (defaults)
    # upar, contains the user parameters (from the XML)
    upar = deepcopy(default)
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
                        values = upar[key].strip('[]').replace(';', ',')\
                                                      .split(',')
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
                mpar.update({'figure.figsize': \
                             parse_array_paires(upar[key])[0]})
            del upar[key]
        elif key == "roi":
            if upar[key] != '':
                pairs = parse_array_paires(upar[key])
                if len(pairs) == 2:
                    pairs.extend([(0., 0.), (0., 0.)])
                elif len(pairs) != 4:
                    raise TelemacException(\
                            '... could not interprete roi ({}): '
                            '{}'.format(upar[key], pairs))
                upar[key] = pairs

    return mpar, upar


# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def draw_geo(myplt, fname):
    """
    @brief
    @param myplt: plot variable of matplotlib
    @param fname: filename of the image to open
    """
    import gdal
    import Image
    image1 = Image.open(fname)
    dataset = gdal.Open(fname)
    cols = dataset.RasterXSize
    rows = dataset.RasterYSize
    geotransform = dataset.GetGeoTransform()
    x_1 = geotransform[0]
    y_2 = geotransform[3]
    d_x = geotransform[1]
    d_y = geotransform[5]
    x_2 = x_1 + d_x * cols
    y_1 = y_2 + d_y * rows
    myplt.imshow(image1.transpose(1), extent=[x_1, x_2, y_1, y_2])

    return


def draw_image(myplt, deco_user, fname):
    """

    @param myplt: plot variable of matplotlib
    @param deco_user: all user parameter for the optionnal keyword of the plot
    @param fname: filename of the image to open
    """
    import Image
    image1 = Image.open(fname)

    if 'extent' in deco_user:
        extent = eval(deco_user['extent'])
        myplt.imshow(image1.transpose(1), extent=extent)
    else:
        myplt.imshow(image1.transpose(1))

    return


def draw_mesh_2d_elements(myplt, a_x, fig, deco_user, elements):
    """
    @param myplt: plot variable of matplotlib
    @param deco_user: all user parameter for the optionnal keyword of the plot
    @param elements: all elements of the mesh
    """

    # Create the cariable for the title
    titre = Title(deco_user, DECO_DEFAULT)
    # create the variable for the a_x
    axe = Axes(deco_user)

    # Position of the graph in the subplot
    index = 0
    if 'index' in deco_user and deco_user['index'] is not None:
        index = int(deco_user['index']) - 1
        deco_user.pop('index', None)

    # *2DElements: draw individual elements polygons  (triangle or quads)
    # ~~> Focus on current subplot / a_x instance
    # crax = myplt.gca()
    # ~~>  Collections
    #   colection = collections.PolyCollection(
    #      elements, cmap=cm.jet, antialiaseds=0, # norm=myplt.Normalize()
    #      edgecolors = 'k', linewidth=1, facecolors = 'none')

    colection = collections.PolyCollection(elements, facecolors='none',
                                           linewidth=0.1)

    # each triangle colour dependent on its value from its verticies
    # colection.set_array(val)
    # ~~> Plot data
    # ex: fig = myplt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is in
    # inches
    a_x[index].add_collection(colection)  # adds, or plots our collection

    # Add the title in the graph
    titre.set_title(a_x[index])
    # change the label of the axe
    axe.set_param(a_x[index])

    return


def draw_coloured_tri_maps(myplt, a_x, fig, deco_user, t1):
    """
    This function adds a filled contour of a triangular mesh.
    It's possible to add a colorbar by adding the keyword "colorbar=True" in the
    xml file.

    @param myplt: plot variable of matplotlib
    @param deco_user: all user parameter for the optionnal keyword of the plot
    @param x: x coordinate of the mesh
    @param y: y coordinate of the mesh
    @param ikle: connectivity of the mesh
    @param z: value for x and y
    @return:
    """
    (x, y, ikle, z) = t1
    # Position of the graph in the subplot
    index = 0
    if 'index' in deco_user and deco_user['index'] is not None:
        index = int(deco_user['index']) - 1
        deco_user.pop('index', None)

    # ~~> Colour maps
    if 'cmap' in deco_user and deco_user['cmap'].split('.')[-1] == 'xml':
        cmap = mpl.colors.LinearSegmentedColormap('user',\
            get_colour_map(deco_user['cmap']))
        deco_user['cmap'] = cmap

    # ~~> Colour maps
    for key in deco_user:
        try:
            deco_user[key] = eval(deco_user[key])
        except Exception:
            pass

    # Title
    titre = Title(deco_user, DECO_DEFAULT)
    # color bar
    colorbar = ColorBar(deco_user)
    # add paramater for the a_x
    axe = Axes(deco_user)

    if 'levels' in deco_user:
        deco_user['levels'] = sorted(deco_user['levels'])

    triang = tri.Triangulation(x, y, triangles=ikle)
    c_s = a_x[index].tricontourf(triang, z, **deco_user)

    # Add the title in the graph
    titre.set_title(a_x[index])

    ## Add the colorbar in the axe
    colorbar.set_colorbar(a_x, fig, myplt, c_s)

    # add the label parameter in the axe
    axe.set_param(a_x[index])

    return


def draw_labeled_tri_contours(myplt, deco_user, t1):
    """
    @brief Convert negative dashed default contour lines by solids
            matplotlib.rcParams['contour.negative_linestyle'] = 'solid'
    @param myplt: plot variable of matplotlib
    @param deco_user: all user parameter for the optionnal keyword of the plot
    @param x: x coordinate of the mesh
    @param y: y coordinate of the mesh
    @param ikle: connectivity of the mesh
    @param z: value for x and y
    """
    (x, y, ikle, z) = t1
    # contour levels based on z values
    zmin = np.min(z)
    zmax = np.max(z)

    for key in deco_user:
        try:
            deco_user[key] = eval(deco_user[key])
        except Exception:
            pass

    if zmin < zmax:
        c_s = myplt.tricontour(x, y, ikle, z, **deco_user)
        myplt.clabel(c_s, **deco_user)

    return

def draw_coloured_tri_vects(myplt, a_x, fig, deco_user, t1):
    """
    This function add vectors on the figure. All parameter known by matplotlib
    can be used It's also possible to create a regular grid and put vector on it
    instead of the mesh node.  For this add in the xml file the variable
    "grid_delta" and give (deltaX; deltaY) Add quiverkey by using the key word
    "key=yes" in the xml and after use the keyword "key_x", "key_y",
    "key_length", "key_label", "labelpos" and "coordinates" to place the
    quiverkey
    @param myplt: plot variable of matplotlib
    @param deco_user: all user parameter for the optionnal keyword of the plot
    @param x: x coordinate of the mesh
    @param y: y coordinate of the mesh
    @param u_v: array with dimension (2, nb val x, nb val y)
    @param normalised: logical if vector are normalised or not
    @param ikle: connectivity of the mesh
    @return:
    """

    (x, y, u_v, normalised, ikle) = t1
    # Position of the graph in the subplot
    index = 0
    if 'index' in deco_user and deco_user['index'] is not None:
        index = int(deco_user['index']) - 1
        deco_user.pop('index', None)

    # ~~> Colour maps
    if 'cmap' in deco_user and deco_user['cmap'].split('.')[-1] == 'xml':
        cmap = mpl.colors.LinearSegmentedColormap(\
                'user', get_colour_map(deco_user['cmap']))
        deco_user['cmap'] = cmap

    # ~~> Plot data
    for key in deco_user:
        if (key == 'units') or (key == 'angles') or (key == 'scale_units'):
            pass
        else:
            try:
                deco_user[key] = eval(deco_user[key])
            except Exception:
                pass

    # Create the cariable for the title
    titre = Title(deco_user, DECO_DEFAULT)
    # Create the variable for the colorbar
    colorbar = ColorBar(deco_user)
    # create the variable for the quiver
    quiver = Quiver(deco_user)

    grid_delta_x = None
    grid_delta_y = None

    for key in deco_user:
        if key == 'grid_delta':
            liste = deco_user[key].replace("(", '').replace(')', '').split(';')
            grid_delta_x = float(liste[0])
            grid_delta_y = float(liste[1])

    #z = np.sqrt(np.sum(np.power(np.dstack(u_v[0:2])[0], 2), axis=1))

    # Create grid to place quiver on it
    if grid_delta_y is not None:
        triangul = tri.Triangulation(x, y, ikle)
        xmin = np.amin(x)
        ymin = np.amin(y)
        xmax = np.amax(x) + grid_delta_x
        ymax = np.amax(y) + grid_delta_y
        (x, y) = np.mgrid[xmin:xmax:grid_delta_x, ymin:ymax:grid_delta_y]
        interp_u = tri.LinearTriInterpolator(triangul, u_v[0])
        interp_v = tri.LinearTriInterpolator(triangul, u_v[1])
        u_v[0] = np.array(interp_u(x, y))
        u_v[1] = np.array(interp_v(x, y))

    if not normalised:
        u = u_v[0]
        v = u_v[1]
    else:
        norm = (u_v[0] ** 2. + u_v[1] ** 2.) ** .5
        u = u_v[0] / norm
        v = u_v[1] / norm
    c_s = quiver.set_quiver(a_x[index], x, y, u, v)

    # Add the title in the graph
    titre.set_title(a_x[index])
    ## Add the colorbar in the axe
    colorbar.set_colorbar(a_x, fig, myplt, c_s)

    return


def draw_3d_surface(myplt, a_x, fig, deco_user, t1):
    """
    We can use this function to draw a 3D surface of a plane.
    @param myplt: plot variable of matplotlib
    @param deco_user: all user parameter for the optionnal keyword of the plot
    @return:
    """
    (x, y, ikle, z) = t1
    # Position of the graph in the subplot
    index = 0
    if 'index' in deco_user and deco_user['index'] is not None:
        index = int(deco_user['index']) - 1
        deco_user.pop('index', None)

    # ~~> Colour maps
    if 'cmap' in deco_user and deco_user['cmap'].split('.')[-1] == 'xml':
        cmap = mpl.colors.LinearSegmentedColormap(\
                'user', get_colour_map(deco_user['cmap']))
        deco_user['cmap'] = cmap

    # ~~> Colour maps
    for key in deco_user:
        try:
            deco_user[key] = eval(deco_user[key])
        except Exception:
            pass

    # Create the cariable for the title
    titre = Title(deco_user, DECO_DEFAULT)
    # Create the variable for the colorbar
    colorbar = ColorBar(deco_user)
    # create the variable for the a_x
    axe = Axes(deco_user)

    if 'levels' in deco_user.keys():
        deco_user['vmin'] = deco_user['levels'][0]
        deco_user['vmax'] = deco_user['levels'][-1]
        deco_user.pop('levels', None)

    if 'zlim' in deco_user.keys():
        a_x[index].set_zlim(deco_user['zlim'])
        deco_user.pop('zlim', None)

    project = None
    if 'project' in deco_user.keys():
        project = deco_user['project']
        deco_user.pop('project', None)

    triang = tri.Triangulation(x, y, triangles=ikle)
    c_s = a_x[index].plot_trisurf(x, y, z, triangles=triang.triangles,
                                  linewidth=0, **deco_user)
    if project is not None:
        if isinstance(project, list):
            for proj in project:
                if proj == 'z':
                    zmin, _ = a_x[index].get_zlim()
                a_x[index].tricontourf(\
                        x, y, z, triangles=triang.triangles, zdir=proj,
                        offset=-zmin, cmap=deco_user['cmap'])
        else:
            if project == 'z':
                zmin, _ = a_x[index].get_zlim()
            a_x[index].tricontourf(x, y, z, triangles=triang.triangles,
                                   zdir=project, offset=-zmin,
                                   cmap=deco_user['cmap'])

    # Add the title in the graph
    titre.set_title(a_x[index])
    ## Add the colorbar in the axe
    colorbar.set_colorbar(a_x, fig, myplt, c_s)
    # change the label of the axe
    axe.set_param(a_x[index])

    return
# _____                      _______________________________________
# ____/ Primary Method:Deco /______________________________________/
#

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
# _____               ______________________________________________
# ____/ Grid Toolbox /_____________________________________________/
#
#   draw_grid* applies to regular i,j-grids
#      - by default this is drawn as a uni-colour wireframe
#      - see also drawCoulouredGrid*
#   *Cells: draw individual cell squares

#   draw_grid* applies to regular i,j-grids
#      - by default this is drawn as a uni-colour wireframe
#      - see also draw_grid_contours, drawColouredGridContours,
#        drawLabeledGridContours, etc.
#   *Contours: draw iso-value contours

def draw_grid_contours(plot, deco_user, t1):

    (x, y, z) = t1

    # ~~> Focus on current subplot / a_x instance
    # crax = plot.gca()
    # TODO: Function not working missing variables
    # The one below are added from other routines
    # To be tested
    levels = deco_user['levels']
    zorder = deco_user['zorder']

    # ~~ Split contours in major/minor classes ~~~~~~~~~~~~~~~~~~~~~~

    # ~~> Draw major contours
    cs1 = plot.contour(x, y, z, levels[::2],
                       colors=deco_user['contour.major.color'], hold='on')
    for coll in cs1.collections:
        coll.set_linestyle(deco_user['contour.major.style'])
        coll.set_zorder(zorder)

    # ~~> Draw minor contours
    cs2 = plot.contour(x, y, z, levels[1::2],
                       colors=deco_user['contour.minor.color'], hold='on')
    for coll in cs2.collections:
        coll.set_linestyle(deco_user['contour.minor.style'])
        coll.set_zorder(zorder)

        # label every 4th level
    inline_label_size = int(HRWD['inline.label.size'])
    _ = plot.clabel(cs2, cs2.levels[1::2],
                    inline=1,
                    fmt='%' + HRWD['inline.label.fmt'],
                    fontsize=inline_label_size)

    return


# _____               ______________________________________________
# ____/ Mesh Toolbox /_____________________________________________/
#
#   Contour plot of a Z based on a triangular mesh a with labels.

def draw_mesh_lines(myplt, edges):
    """
    @brief
    """
    #  *Lines: draw individual edges
    # TODO: Find a way to do colours properly -- and complete
    #       drawColouredMeshLines
    # ~~> Focus on current subplot / a_x instance
    crax = myplt.gca()
    # ~~>  Collections
    colection = collections.LineCollection(\
        edges, antialiaseds=0,  # cmap=cm.jet ,norm=self.plt.Normalize(),
        linewidth=1)  # colors = 'k',
    # colection.set_zorder(deco['zorder'])
    # each element colour dependent on its value from its verticies
    # colection.set_array(val)
    # ~~> Plot data
    # ex: fig = self.plt.figure(1,figsize=(3.0,4.0),dpi=100), where figsize is
    # in inches
    # adds, or plots our collection
    crax.add_collection(colection, autolim=True)

    return


def draw_coloured_quad_maps(plt, deco_user, t0, t1):

    (nelem, npoin, _, _) = t0
    (x, y, _, z) = t1

    # ~~> Focus on current subplot / a_x instance
    crax = plt.gca()
    # ~~> Plot data

    n_x = npoin - nelem
    n_y = int(npoin / n_x)
    mesh = np.column_stack((x, y))
    msh = collections.QuadMesh(n_x - 1, n_y - 1, mesh, True, shading='gouraud')
    ###!!! I have tried cax and msh, and various combos
    # toolbar.show()
    # canvas.draw()
    msh.set_array(z)
    crax.add_collection(msh)

    xmin = min(deco_user['roi'][0][0], deco_user['roi'][1][0])
    xmax = max(deco_user['roi'][0][0], deco_user['roi'][1][0])
    ymin = min(deco_user['roi'][0][1], deco_user['roi'][1][1])
    ymax = max(deco_user['roi'][0][1], deco_user['roi'][1][1])
    crax.set_xlim(xmin - 0.5, xmax + 0.5)  # sets x axis limits, default 0-1
    crax.set_ylim(ymin - 0.5, ymax + 0.5)  # sets y axis limits, default 0-1
    crax.axis('auto')  # sets both axis scale to be auto


    return
