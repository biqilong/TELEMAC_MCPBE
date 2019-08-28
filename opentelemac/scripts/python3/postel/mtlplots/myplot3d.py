r"""@author TELEMAC-MASCARET Consortium

    @brief
"""
from __future__ import print_function
from copy import deepcopy
# ~~> dependencies towards other mtlplots
from utils.parser_strings import parse_array_paires
from utils.exceptions import TelemacException
import numpy as np
import mayavi
from mayavi import mlab
from tvtk.api import tvtk

# ~~> mayavi
try:
    from tvtk.api import tvtk
    MAYAVI_AVAIL = True
except ImportError:
    MAYAVI_AVAIL = False

# _____                        _____________________________________
# ____/ Primary Mayavi Object /____________________________________/
#
def type_unstructured_grid(xyz, ikle):

    cell_ug = tvtk.CellArray()
    cell_ug.set_cells(len(ikle), np.insert(ikle, 0, 6, axis=1).ravel())
    type_ug = tvtk.UnstructuredGrid(points=xyz)
    wedge_type = 13
    cell_type = np.ones(len(ikle))*wedge_type
    offset = 7*np.arange(len(ikle))
    type_ug.set_cells(cell_type, offset, cell_ug)

    return type_ug

# _____                                _____________________________
# ____/ Primary Mayavi Object (Plane) /____________________________/
#

def type_unstructured_grid_plane(xyz, ikle):

    cell_ug = tvtk.CellArray()
    cell_ug.set_cells(len(ikle), np.insert(ikle, 0, 3, axis=1).ravel())
    type_ug = tvtk.UnstructuredGrid(points=xyz)
    type_ug.set_cells(5*np.ones(len(ikle)), 7*np.arange(len(ikle)), cell_ug)

    return type_ug

# _____               ______________________________________________
# ____/ Default DECO /_____________________________________________/
#

DECO_DEFAULT = {\
    "size":'(10;10)',
    "aspect": 'auto',
    "dpi":'',
    "ratio2d": '',
    "title": '',
    "roi": '',
    "type":'',
    "set": '',
    "background": '(1.0,0.90196,0.6)',
    "azimuth":'',
    "elevation":'',
    "distance":'',
    "focalx":'',
    "focaly":'',
    "focalz":'',
    "interactive":'',
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
    for key in deco_user:
        if key in ['look', 'data']:
            upar.update(deco_user[key][0])
        elif key == "dpi":
            if upar[key] != '':
                mpar.update({'savefig.dpi': int(upar[key])})
                mpar.update({'figure.dpi': int(upar[key])})
            del upar[key]
        elif key == "size":
            if upar[key] != '':
                mpar.update(\
                        {'figure.figsize': parse_array_paires(upar[key])[0]})
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
# ____/ Primary Method:Deco /______________________________________/
#

def deco(myplt, upar, dpar):
    """
    @brief
    @param myplt: plot variable of matplotlib
    @param upar:
    @param dpar:
    """
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

    return

# _____                      _______________________________________
# ____/ Primary Method:Draw /______________________________________/
#

def draw_coloured_tri_maps(myplt, fig, deco_user, vtype, data):
    """
    This function adds a filled contour of a triangular mesh.
    It's possible to add a colorbar by adding the keyword "colorbar=True" in the
    xml file.

    @param myplt: plot variable of matplotlib
    @param deco_user: all user parameter for the optionnal keyword of the plot
    @param data (tuple): (x, y, z, ikle, v) where:
                         x: x coordinate of the mesh
                         y: y coordinate of the mesh
                         ikle: connectivity of the mesh
                         z: value for x and y
                         v: data value
    """

    print("I am starting the plot")
    x, y, z, ikle, _ = data
    opacity = 0.5
    if 'opacity' in deco_user:
        if deco_user['opacity'] != '':
            opacity = float(deco_user['opacity'])

    # ~~> Unstructured 3D Mesh
    # points as the shape of [ [x1,y1,z1],...[xn,yn,zn] ]
    points = np.dstack((x, y, z))[0]
    struct = type_unstructured_grid(points, ikle)
    struct.point_data.scalars = z.ravel()
    struct.point_data.scalars.name = 'z'

    fig1 = mlab.figure('z', bgcolor=(1, 1, 1))
    mayavi.engine.current_scene.scene.off_screen_rendering = True
    mlab.options.offscreen = True
    surf = myplt.pipeline.surface(struct, opacity=opacity)
    fig1.scene.isometric_view()
    mlab.savefig('figure3d.png')

    return

def draw_coloured_tri_vects(myplt, fig, deco_user, vtype,
                            data):
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
    @param data (tuple): (x, y, z, ikle, uvw, normalised) where:
                         x: x coordinate of the mesh
                         y: y coordinate of the mesh
                         uvw: array with dimension (2, nb val x, nb val y)
                         normalised: logical if vector are normalised or not
                         ikle: connectivity of the mesh
    """
    x, y, z, ikle, uvw, _ = data
    # ~~> Unstructured 3D Mesh
    # points as the shape of [ [x1,y1,z1],...[xn,yn,zn] ]
    points = np.dstack((x, y, z))[0]

    struct = type_unstructured_grid(points, ikle)
    struct.point_data.vectors = uvw[0].T
    struct.point_data.vectors.name = 'velocity'

    vel = myplt.pipeline.vectors(struct)

    if 'z_scale' in deco_user:
        if deco_user['z_scale'] != '':
            z_scale = float(deco_user['z_scale'])
            vel.actor.actor.scale = np.array([1.0, 1.0, z_scale])

    colours = vel.parent

    if 'colour_range' in deco_user:
        if deco_user['colour_range'] != '':
            colour_range = parse_array_paires(deco_user['colour_range'])[0][:]
            colours.scalar_lut_manager.data_range = colour_range

    if 'number_colours' in deco_user:
        if deco_user['number_colours'] != '':
            number_colours = int(deco_user['number_colours'])
            colours.scalar_lut_manager.number_of_colors = number_colours
            colours.scalar_lut_manager.number_of_labels = number_colours + 1

    _ = myplt.outline(color=(0, 0, 0))

    if "streamline" in vtype:

        vel.glyph.visible = False
        vel.actor.actor.visibility = False
        vel.visible = False

        norm = myplt.pipeline.extract_vector_norm(struct)
        stream = myplt.pipeline.streamline(norm)

        stream.seed.widget = stream.seed.widget_list[2]
        stream.seed.widget.enabled = False

        if 'z_scale' in deco_user:
            if deco_user['z_scale'] != '':
                z_scale = float(deco_user['z_scale'])
                stream.actor.actor.scale = np.array([1.0, 1.0, z_scale])

        colours = stream.parent

        if 'colour_range' in deco_user:
            if deco_user['colour_range'] != '':
                colour_range = parse_array_paires(deco_user['colour_range'])[0][:]
                colours.scalar_lut_manager.data_range = colour_range

        if 'number_colours' in deco_user:
            if deco_user['number_colours'] != '':
                number_colours = int(deco_user['number_colours'])
                colours.scalar_lut_manager.number_of_colors = number_colours
                colours.scalar_lut_manager.number_of_labels = number_colours + 1

    for key in deco_user:
        try:
            exec(str(key)+' = '+str(deco_user[key]))
        except Exception:
            pass

    return

def draw_coloured_tri_maps_plane(myplt, deco_user, vtype,
                                 data):

    x, y, z, ikle, v = data
    opacity = 0.5
    if 'opacity' in deco_user:
        if deco_user['opacity'] != '':
            opacity = float(deco_user['opacity'])

    # ~~> Unstructured 3D Mesh
    # points as the shape of [ [x1,y1,z1],...[xn,yn,zn] ]

    points = np.dstack((x, y, z))[0]

    struct = type_unstructured_grid_plane(points, ikle)
    struct.point_data.scalars = v.ravel()
    struct.point_data.scalars.name = 'z'

    surf = myplt.pipeline.surface(struct, opacity=opacity)
    surf.actor.mapper.interpolate_scalars_before_mapping = True
    surf.actor.property.lighting = False

    if 'z_scale' in deco_user:
        if deco_user['z_scale'] != '':
            z_scale = float(deco_user['z_scale'])
            surf.actor.actor.scale = np.array([1.0, 1.0, z_scale])

    colours = surf.parent

    if 'colour_range' in deco_user:
        if deco_user['colour_range'] != '':
            colour_range = parse_array_paires(deco_user['colour_range'])[0][:]
            colours.scalar_lut_manager.data_range = colour_range

    if 'number_colours' in deco_user:
        if deco_user['number_colours'] != '':
            number_colours = int(deco_user['number_colours'])
            colours.scalar_lut_manager.number_of_colors = number_colours
            colours.scalar_lut_manager.number_of_labels = number_colours + 1

    for key in deco_user:
        try:
            exec(str(key)+' = '+str(deco_user[key]))
        except Exception:
            pass

    return
