r"""@author TELEMAC-MASCARET Consortium

     @brief 2D plot in a 3d lyaout
"""
from postel.mtlplots.param_axe import set_param_unique
from postel.mtlplots.param_axe import get_param_unique

class ColorBar(object):
    """
    @brief Class where we store all necessary parameter of the ColorBar
    """
    def __init__(self, deco_user):

        self.cbar = None
        self.color_bar = False
        self.common_cbar = False
        self.param_title = {\
        'colorbar_title': {'name': 'set_title', 'val': None},
        'colorbar_title_fontsize': {'name': 'fontsize', 'val': None},
        'colorbar_title_x': {'name': 'x', 'val': None},
        'colorbar_title_y': {'name': 'y', 'val': None},
                           }
        self.param_cb = {'colorbar': {'name': 'colorbar', 'val': None},
                         'orientation': {'name': 'orientation', 'val': None},
                         'fraction': {'name': 'fraction', 'val': None},
                         'pad': {'name': 'pad', 'val': None},
                         'shrink': {'name': 'shrink', 'val': None},
                         'colorbar_aspect': {'name': 'aspect', 'val': None},
                         'anchor': {'name': 'anchor', 'val': None},
                         'panchor': {'name': 'panchor', 'val': None},
                         'format': {'name': 'format', 'val': None},
                        }
        self.get_param(deco_user)

    def get_param(self, deco_user):
        """
        Get all parameter for the ColorBar
        @param deco_user:
        @return:
        """
        get_param_unique(deco_user, self.param_cb)
        get_param_unique(deco_user, self.param_title)
        if 'common_colorbar' in deco_user.keys() and \
            deco_user['common_colorbar'] is not None:

            self.common_cbar = deco_user['common_colorbar']
            deco_user.pop('common_colorbar', None)

    def set_colorbar(self, axe, fig, plt, c_s):

        if self.param_cb['colorbar']['val'] is not None and \
            self.param_cb['colorbar']['val']:

            self.param_cb['colorbar']['val'] = c_s
            if self.common_cbar:
                self.param_cb["ax"] = {'name': 'ax', 'val': axe}
            self.cbar = set_param_unique(fig, self.param_cb, 'colorbar', 'all')
            if self.param_title['colorbar_title']['val'] is not None:
                set_param_unique(self.cbar.ax, self.param_title,
                                 'colorbar_title', 'all')
