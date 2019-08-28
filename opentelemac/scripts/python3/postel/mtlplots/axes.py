r"""@author TELEMAC-MASCARET Consortium

     @brief
"""
from postel.mtlplots.param_axe import set_param_unique
from postel.mtlplots.param_axe import get_param_unique
from postel.mtlplots.param_axe import put_in_list

class Axes(object):
    """
    Class where we store all the parameter corresponding to an Axe
    """
    def __init__(self, deco_user):

        self.param_font = {'fontsize_x': {'name': 'fontsize', 'val': None},
                           'fontsize_y': {'name': 'fontsize', 'val': None},
                           'fontsize_z': {'name': 'fontsize', 'val': None},
                           'label_x': {'name': 'set_xlabel', 'val': None},
                           'label_y': {'name': 'set_ylabel', 'val': None},
                           'label_z': {'name': 'set_zlabel', 'val': None},
                           'font_x': {'name': 'font', 'val': None},
                           'font_y': {'name': 'font', 'val': None},
                           'font_z': {'name': 'font', 'val': None},
                          }
        self.param_tick = {\
                 'fontsize_xticks': {'name': 'set_fontsize', 'val': None},
                 'fontsize_yticks': {'name': 'set_fontsize', 'val': None},
                 'fontsize_zticks': {'name': 'set_fontsize', 'val': None},
                          }
        self.param_type = {'aspect': {'name': 'set_aspect', 'val': None},
                           'adjustable': {'name': 'adjustable', 'val': None},
                          }
        self.param_lim = {'lim_x': {'name': 'set_xlim', 'val': None},
                          'lim_y': {'name': 'set_ylim', 'val': None},
                          'lim_z': {'name': 'set_zlim', 'val': None},
                          'axe.nmarge': {'name': 'north_marge', 'val': 0.02},
                          'axe.smarge': {'name': 'south_marge', 'val': 0.02},
                          'axe.emarge': {'name': 'est_marge', 'val': 0.02},
                          'axe.wmarge': {'name': 'west_marge', 'val': 0.02},
                         }

        self.param_legend = {'legend': {'name': 'legend', 'val': None},
                             'legend.labels': {'name': 'label', 'val': None},
                             'legend.position': {'name': 'loc', 'val': None},
                             'legend.borderaxespad': {'name': 'borderaxespad',
                                                      'val': None},
                             'legend.ncol': {'name': 'ncol', 'val': None},
                            }

        self.param_annote = {\
              'annotation': {'name': 'annotate', 'val': None},
              'annotation.xy': {'name': 'xy', 'val': None},
              'annotation.xycoords': {'name': 'xycoords', 'val': 'data'},
              'annotation.textcoords': {'name': 'textcoords', 'val': None},
              'annotation.xytext': {'name': 'xytext', 'val': None},
              'annotation.arrowprops': {'name': 'arrowprops',\
                                  'val': dict(facecolor='black', width=1.)},
              'point.annotate': {'name': 'plot', 'val': None},
              'point.annotate.marker': {'name': 'marker', 'val': 'o'},
                            }
        self.get_param(deco_user)

    def get_param(self, deco_user):
        """
        Get all the parameter for the Axe
        @param deco_user:
        @return:
        """
        get_param_unique(deco_user, self.param_font)
        get_param_unique(deco_user, self.param_tick)
        get_param_unique(deco_user, self.param_type)
        get_param_unique(deco_user, self.param_lim)
        get_param_unique(deco_user, self.param_legend)
        get_param_unique(deco_user, self.param_annote)
        if self.param_annote['point.annotate']['val'] is not None and \
            self.param_annote['point.annotate']['val']:

            self.param_annote['point.annotate']['val'] = \
                    self.param_annote['annotation.xy']['val']
            if isinstance(self.param_annote['point.annotate']['val'], list):
                self.param_annote['point.annotate.y'] = {}
                self.param_annote['point.annotate.y']['name'] = 'y'
                self.param_annote['point.annotate.y']['val'] = []
                for idx, (x, y) in \
                          enumerate(self.param_annote['point.annotate']['val']):
                    #TODO: be careful before it was point.annotate in the next
                    # line
                    self.param_annote['point.annotate.y']['val'][idx] = x
                    self.param_annote['point.annotate.y']['val'].append(y)
        if isinstance(self.param_annote['annotation.xy']['val'], list):
            nb_val = len(self.param_annote['annotation.xy']['val'])

            res = put_in_list(self.param_annote['annotation.arrowprops'],
                              nb_val)
            self.param_annote['annotation.arrowprops']['val'] = res

            res = put_in_list(self.param_annote['annotation.xycoords'], nb_val)
            self.param_annote['annotation.xycoords']['val'] = res

            res = put_in_list(self.param_annote['annotation.textcoords'],
                              nb_val)
            self.param_annote['annotation.textcoords']['val'] = res

            res = put_in_list(self.param_annote['point.annotate.marker'],
                              nb_val)
            self.param_annote['point.annotate.marker']['val'] = res

    def set_param(self, axes, pfont=True, paspect=True, plegend=True, plim=True,
                  pticks=True, pannotate=True):
        """
        Apply all the parameter. But we can choose to not update a specific
        parameter with the boolean parameter
        @param axes:
        @param pfont:
        @param paspect:
        @param plegend:
        @param plim:
        @param pticks:
        @return:
        """
        legend = None
        if pfont:
            set_param_unique(axes, self.param_font, 'label_x', '_x')
            set_param_unique(axes, self.param_font, 'label_y', '_y')
            set_param_unique(axes, self.param_font, 'label_z', '_z')

        if paspect:
            set_param_unique(axes, self.param_type, 'aspect', 'adjustable')

        if plegend:
            legend = set_param_unique(axes, self.param_legend,
                                      'legend', 'legend.')

        if plim:
            set_param_unique(axes, self.param_lim, 'lim_x', 'dumb')
            set_param_unique(axes, self.param_lim, 'lim_y', 'dumb')
            set_param_unique(axes, self.param_lim, 'lim_z', 'dumb')

        if pticks:
            for tick in axes.xaxis.get_major_ticks():
                set_param_unique(tick.label, self.param_tick, 'fontsize_xticks',
                                 'dumb')
            for tick in axes.yaxis.get_major_ticks():
                set_param_unique(tick.label, self.param_tick, 'fontsize_yticks',
                                 'dumb')
            try:
                for tick in axes.zaxis.get_major_ticks():
                    set_param_unique(\
                            tick.label, self.param_tick, 'fontsize_zticks',
                            'dumb')
            except Exception:
                pass

        if pannotate:
            if isinstance(self.param_annote['annotation.xy']['val'], list):
                for i, _ in \
                        enumerate(self.param_annote['annotation.xy']['val']):
                    set_param_unique(axes, self.param_annote, 'annotation',
                                     'annotation.', index=i)
                    if self.param_annote['point.annotate']['val'] is not None:
                        set_param_unique(\
                                axes, self.param_annote, 'point.annotate',
                                'point.annotate.', index=i)
            else:
                set_param_unique(axes, self.param_annote,
                                 'annotation', 'annotation.')
                if self.param_annote['point.annotate']['val'] is not None:
                    set_param_unique(axes, self.param_annote, 'point.annotate',
                                     'point.annotate.')


        return {'legend': legend}
