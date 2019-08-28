r"""@author TELEMAC-MASCARET Consortium
    @brief
"""
from __future__ import print_function
from os import path, stat, mkdir
from postel.caster import Caster
from postel.mtlplots.myplot3d import map_deco_default, DECO_DEFAULT,\
                                     draw_coloured_tri_maps,\
                                     draw_coloured_tri_vects

try:
    from mayavi import mlab
    MAYAVI_AVAIL = True
except ImportError:
    MAYAVI_AVAIL = False

#@mlab.show
def nothing():
    return

class Figure3D(Caster):
    """
    @brief Plot 3D matplotlib figure
    @param Caster:
    """
    def __init__(self, caster, plot):
        Caster.__init__(self, {'object': caster.object,
                               'obdata': caster.obdata})
        # ~~~ figure decoration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.mpar, self.upar = map_deco_default(plot['deco'], DECO_DEFAULT)
        # ~~~ create figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        fig = mlab.figure(bgcolor=(1, 1, 1))

        mlab.options.offscreen = True
        self.plt = mlab
        self.fig = fig

    def add(self, typl, what):
        """
        Add a curve in a graph
        @param typl:
        @param what:
        @return:
        """
        Caster.add(self, typl, what)
        if 'SELAFIN' in typl.upper() or \
            'slf' in typl.lower():

            if what['type'].split(':')[1] == '3d-view':

                cast = self.get(typl, what)
                elements = cast.support
                varsors = cast.values

                # ~~> Loop on variables
                for vtype in what['vars'].split(';'):
                    vtype = vtype.split(':')[1]

                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Draw triangles and quads
                    if "mesh" in vtype or "wire" in vtype or "grid" in vtype:
                        pass

                    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # ~~> Extract variable data for only one time frame and one
                    # plane
                    else:
                        meshx, meshy, meshz, ikle3 = elements

                        # ~~> Draw/Dump (multiple options possible)
                        if "map" in vtype or "contour" in vtype:
                            draw_coloured_tri_maps(\
                                    self.plt, self.fig, what['deco'],
                                    vtype, (meshx, meshy, meshz,
                                            ikle3, varsors[0]))

                        elif "label" in vtype:
                            pass

                        elif "arrow" in vtype or "vector" in vtype or \
                              "streamline" in vtype:
                            draw_coloured_tri_vects(self.plt, self.fig,
                                                    what['deco'], vtype,
                                                    (meshx, meshy, meshz,
                                                     ikle3, varsors, False))

                        elif "angle" in vtype:
                            draw_coloured_tri_vects(\
                                    self.plt, self.fig, what['deco'],
                                    vtype, (meshx, meshy, varsors, True))
                        else:
                            print('... do not know how to draw this SELAFIN'
                                  'type: '+ vtype)

            else:
                print('... do not know how to do this type of extraction: ' + \
                        what['type'].split(':')[1])


    def show(self):
        """
        Show a graphic
        @param file_name:
        @return:
        """
        self.fig.scene.isometric_view()
#        deco(self.plt, self.upar, self.dpar)
        self.plt.show()
        self.plt.close()

    def save(self, file_name):
        """
        Export a graphic in a image
        @param file_name:
        @return:
        """
#        deco(self.plt, self.upar, self.dpar)

        default_view = mlab.view()
        azimuth = default_view[0]
        elevation = default_view[1]
        distance = default_view[2]
        focalpoint = default_view[3]

        if self.upar['azimuth'] != '':
            azimuth = float(self.upar['azimuth'])
        if self.upar['elevation'] != '':
            elevation = float(self.upar['elevation'])
        if self.upar['distance'] != '':
            distance = float(self.upar['distance'])
        if self.upar['focalx'] != '' and self.upar['focaly'] != '' and \
            self.upar['focalz'] != '':
            focalpoint = (float(self.upar['focalx']),
                          float(self.upar['focaly']),
                          float(self.upar['focalz']))

        mlab.view(azimuth=azimuth,
                  elevation=elevation,
                  distance=distance,
                  focalpoint=focalpoint)

        try:
            stat(path.split(file_name)[0])
        except Exception:
            mkdir(path.split(file_name)[0])

        self.plt.savefig(file_name)
        self.plt.close()
