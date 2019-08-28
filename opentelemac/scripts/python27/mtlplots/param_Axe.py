# -*- coding: utf8 -*-


def get_param_unique(decoUser, param):
   """
   get common param from the dictionnary "decoUser" and copy it in the dictionnary "param"
   @param decoUser: parameter read from the xml file given in the "deco"
   @param param: specific param
   @return:
   """
   for key, item in param.items():
      if key in decoUser and decoUser[key] is not None:
         try:
            param[key]['val'] = eval(decoUser[key])
         except:
            param[key]['val'] = decoUser[key]
         decoUser.pop(key, None)

def set_param_unique(ax, param, label, elem, index=None):
   """
   apply to the object "ax" the method stored in param[label]['name'] and apply the corresponding parameter in label and elem
   @param ax:
   @param param:
   @param label:
   @param elem:
   @return:
   """
   deco = {}
   res = None
   if param[label]['val'] is not None:
      for key, item in param.items():
         if elem == 'all':
            if key is not label and item['val'] is not None:
               if index is not None:
                  deco[item['name']] = item['val'][index]
               else:
                  deco[item['name']] = item['val']
         else:
            if elem in key and key is not label and item['val'] is not None:
               if index is not None:
                  deco[item['name']] = item['val'][index]
               else:
                  deco[item['name']] = item['val']
      if index is not None:
         print param[label]['name'], param[label]['val'][index], deco
         res = getattr(ax, param[label]['name'])(param[label]['val'][index], **deco)
      else:
         res = getattr(ax, param[label]['name'])(param[label]['val'], **deco)

   return res

def put_in_list(param, nb_val):
   """
   If param['val'] is a single value, then it will duplicate the value into a list
   @param param:
   @param nb_val:
   @return:
   """
   res = param['val']
   if not isinstance(param['val'], list):
      val = param['val']
      res = []
      for i in range(nb_val):
         res.append(val)
   return res

class Title:
   """
   Class where we store all necessary parameter of the Title
   """
   def __init__(self, decoUser, decoDefault):

      self.title = None
      self.paramTitle = {'graph_title': {'name': 'set_title', 'val': None},
                         'fontsize_title': {'name': 'fontsize', 'val': decoDefault['axes.titlesize']},
                         'loc_title': {'name': 'loc', 'val': None},
                         'x_title': {'name': 'x', 'val': None},
                         'y_title': {'name': 'y', 'val': None},
                         }
      self.get_param(decoUser)

   def get_param(self, decoUser):
      """
        Get all parameter from the decoUser
        @return:
        """
      get_param_unique(decoUser, self.paramTitle)

   def set_title(self, ax):
      """
        Add the title in the axe
        @param ax:
        @return:
        """
      set_param_unique(ax, self.paramTitle, 'graph_title', '_title')


class ColorBar:
   """
   Class where we store all necessary parameter of the ColorBar
   """
   def __init__(self, decoUser, decoDefault):
      """

      @param decoUser:
      @param decoDefault:
      @return:
      """
      self.cbar = None
      self.colorBar = False
      self.common_cbar = False
      self.paramTitle = {'colorbar_title': {'name': 'set_title', 'val': None},
                         'colorbar_title_fontsize': {'name': 'fontsize', 'val': None},
                         'colorbar_title_x': {'name': 'x', 'val': None},
                         'colorbar_title_y': {'name': 'y', 'val': None},
                         }
      self.paramCb = {'colorbar': {'name': 'colorbar', 'val': None},
                      'orientation': {'name': 'orientation', 'val': None},
                      'fraction': {'name': 'fraction', 'val': None},
                      'pad': {'name': 'pad', 'val': None},
                      'shrink': {'name': 'shrink', 'val': None},
                      'colorbar_aspect': {'name': 'aspect', 'val': None},
                      'anchor': {'name': 'anchor', 'val': None},
                      'panchor': {'name': 'panchor', 'val': None},
                      'format': {'name': 'format', 'val': None},
                      }
      self.get_param(decoUser)

   def get_param(self, decoUser):
      """
      Get all parameter for the ColorBar
      @param decoUser:
      @return:
      """
      get_param_unique(decoUser, self.paramCb)
      get_param_unique(decoUser, self.paramTitle)
      if 'common_colorbar' in decoUser.keys() and decoUser['common_colorbar'] is not None:
         self.common_cbar = decoUser['common_colorbar']
         decoUser.pop('common_colorbar', None)

   def set_colorbar(self, ax, fig, plt, cs):
      """
        Add a colorbar for the axe
        @param ax:
        @param fig:
        @param plt:
        @param cs:
        @return:
        """
      if self.paramCb['colorbar']['val'] is not None and self.paramCb['colorbar']['val']:
         self.paramCb['colorbar']['val'] = cs
         if self.common_cbar:
            self.paramCb["ax"] = {'name': 'ax', 'val': ax}
         self.cbar = set_param_unique(fig, self.paramCb, 'colorbar', 'all')
         if self.paramTitle['colorbar_title']['val'] is not None:
            set_param_unique(self.cbar.ax, self.paramTitle, 'colorbar_title', 'all')


class Quiver:
   """
   Class where we store all the parameter for quiver
   """
   vector_dict = ['units', 'angles', 'scale', 'scale_units', 'width',
                  'headwidth', 'headlength', 'headaxislength', 'minshaft', 'minlength',
                  'pivot', 'color', 'cmap']
   key_dict = ['key_x', 'key_y', 'key_length', 'key_label', 'labelpos',
               'coordinates', 'key_color']

   def __init__(self, decoUser):
      self.paramQuiver = {}
      self.paramKey = {}
      self.val_Key = {'X': 0.8, 'Y': 0.05, 'U': 1.0, 'label': ''}
      self.key = False
      self.get_param(decoUser)

   def get_param(self, decoUser):
      """
      Get all parameter for the quiver
      @param decoUser:
      @return:
      """
      if 'key' in decoUser.keys():
         try:
            decoUser['key'] = eval(decoUser['key'])
         except:
            None
         if decoUser['key']:
            self.key = True
      for key in decoUser:
         if (key in Quiver.vector_dict):
            if key == ('scale' or 'width' or
                          'headwidth' or 'headlength' or
                          'headaxislength' or 'minshaft' or 'minlength' or
                          'linewidths'):
               self.paramQuiver[key] = float(decoUser[key])
            else:
               self.paramQuiver[key] = decoUser[key]
         elif (key in Quiver.key_dict):
            if key == 'key_x':
               self.val_Key['X'] = float(decoUser[key])
            elif key == 'key_y':
               self.val_Key['Y'] = float(decoUser[key])
            elif key == 'key_length':
               self.val_Key['U'] = float(decoUser[key])
            elif key == 'key_label':
               self.val_Key['label'] = decoUser[key]
            elif key == 'key_color':
               self.paramKey['color'] = decoUser[key]
            else:
               try:
                  self.paramKey[key] = eval(decoUser[key])
               except:
                  self.paramKey[key] = decoUser[key]

   def set_quiver(self, ax, x, y, u, v):
      """
      Add quiver in the figure
      @param ax:
      @param x:
      @param y:
      @param u:
      @param v:
      @return:
      """
      cs = ax.quiver(x, y, u, v, **self.paramQuiver)
      if self.key:
         ax.quiverkey(cs, self.val_Key['X'], self.val_Key['Y'], self.val_Key['U'], self.val_Key['label'],
                      **self.paramKey)


class Axes:
   """
   Class where we store all the parameter corresponding to an Axe
   """
   def __init__(self, decoUser):

      self.paramFont = {'fontsize_x': {'name': 'fontsize', 'val': None},
                        'fontsize_y': {'name': 'fontsize', 'val': None},
                        'fontsize_z': {'name': 'fontsize', 'val': None},
                        'label_x': {'name': 'set_xlabel', 'val': None},
                        'label_y': {'name': 'set_ylabel', 'val': None},
                        'label_z': {'name': 'set_zlabel', 'val': None},
                        'font_x': {'name': 'font', 'val': None},
                        'font_y': {'name': 'font', 'val': None},
                        'font_z': {'name': 'font', 'val': None},
                        }
      self.paramTick = {'fontsize_xticks': {'name': 'set_fontsize', 'val': None},
                        'fontsize_yticks': {'name': 'set_fontsize', 'val': None},
                        'fontsize_zticks': {'name': 'set_fontsize', 'val': None},
                        }
      self.paramType = {'aspect': {'name': 'set_aspect', 'val': None},
                        'adjustable': {'name': 'adjustable', 'val': None},
                        }
      self.paramLim = {'lim_x': {'name': 'set_xlim', 'val': None},
                       'lim_y': {'name': 'set_ylim', 'val': None},
                       'lim_z': {'name': 'set_zlim', 'val': None},
                       'axe.nmarge': {'name': 'north_marge', 'val': 0.02},
                       'axe.smarge': {'name': 'south_marge', 'val': 0.02},
                       'axe.emarge': {'name': 'est_marge', 'val': 0.02},
                       'axe.wmarge': {'name': 'west_marge', 'val': 0.02},
                       }

      self.paramLegend = {'legend': {'name': 'legend', 'val': None},
                          'legend.label': {'name': 'labels', 'val': None},
                          'legend.position': {'name': 'loc', 'val': None},
                          'legend.borderaxespad': {'name': 'borderaxespad', 'val': None},
                          'legend.ncol': {'name': 'ncol', 'val': None},
                          }

      self.paramAnnote = {'annotation': {'name': 'annotate', 'val': None},
                          'annotation.xy': {'name': 'xy', 'val': None},
                          'annotation.xycoords': {'name': 'xycoords', 'val': 'data'},
                          'annotation.textcoords': {'name': 'textcoords', 'val': None},
                          'annotation.xytext': {'name': 'xytext', 'val': None},
                          'annotation.arrowprops': {'name': 'arrowprops', 'val': dict(facecolor='black', width=1.)},
                          'point.annotate': {'name': 'plot', 'val': None},
                          'point.annotate.marker': {'name': 'marker', 'val': 'o'},
                          }
      self.get_param(decoUser)

   def get_param(self, decoUser):
      """
      Get all the parameter for the Axe
      @param decoUser:
      @return:
      """
      get_param_unique(decoUser, self.paramFont)
      get_param_unique(decoUser, self.paramTick)
      get_param_unique(decoUser, self.paramType)
      get_param_unique(decoUser, self.paramLim)
      get_param_unique(decoUser, self.paramLegend)
      get_param_unique(decoUser, self.paramAnnote)
      if self.paramAnnote['point.annotate']['val'] is not None and self.paramAnnote['point.annotate']['val']:
         self.paramAnnote['point.annotate']['val'] = self.paramAnnote['annotation.xy']['val']
         if isinstance(self.paramAnnote['point.annotate']['val'], list):
            self.paramAnnote['point.annotate.y'] = {}
            self.paramAnnote['point.annotate.y']['name'] = 'y'
            self.paramAnnote['point.annotate.y']['val'] = []
            for idx, (x, y) in enumerate(self.paramAnnote['point.annotate']['val']):
               self.paramAnnote['point.annotate']['val'][idx] = x
               self.paramAnnote['point.annotate.y']['val'].append(y)
      if isinstance(self.paramAnnote['annotation.xy']['val'], list):
         nb_val = len(self.paramAnnote['annotation.xy']['val'])

         res = put_in_list(self.paramAnnote['annotation.arrowprops'], nb_val)
         self.paramAnnote['annotation.arrowprops']['val'] = res

         res = put_in_list(self.paramAnnote['annotation.xycoords'], nb_val)
         self.paramAnnote['annotation.xycoords']['val'] = res

         res = put_in_list(self.paramAnnote['annotation.textcoords'], nb_val)
         self.paramAnnote['annotation.textcoords']['val'] = res

         res = put_in_list(self.paramAnnote['point.annotate.marker'], nb_val)
         self.paramAnnote['point.annotate.marker']['val'] = res



   def set_param(self, ax, pfont=True, paspect=True, plegend=True, plim=True, pticks=True,
                 pannotate=True):
      """
      Apply all the parameter. But we can choose to not update a specific parameter with the boolean parameter
      @param ax:
      @param pfont:
      @param paspect:
      @param plegend:
      @param plim:
      @param pticks:
      @return:
      """
      legend = None
      if pfont:
         set_param_unique(ax, self.paramFont, 'label_x', '_x')
         set_param_unique(ax, self.paramFont, 'label_y', '_y')
         set_param_unique(ax, self.paramFont, 'label_z', '_z')

      if paspect:
         set_param_unique(ax, self.paramType, 'aspect', 'adjustable')

      if plegend:
         legend = set_param_unique(ax, self.paramLegend, 'legend', 'legend.')

      if plim:
         set_param_unique(ax, self.paramLim, 'lim_x', 'dumb')
         set_param_unique(ax, self.paramLim, 'lim_y', 'dumb')
         set_param_unique(ax, self.paramLim, 'lim_z', 'dumb')

      if pticks:
         for tick in ax.xaxis.get_major_ticks():
            set_param_unique(tick.label, self.paramTick, 'fontsize_xticks', 'dumb')
         for tick in ax.yaxis.get_major_ticks():
            set_param_unique(tick.label, self.paramTick, 'fontsize_yticks', 'dumb')
         try:
            for tick in ax.zaxis.get_major_ticks():
               set_param_unique(tick.label, self.paramTick, 'fontsize_zticks', 'dumb')
         except:
            None

      if pannotate:
         if isinstance(self.paramAnnote['annotation.xy']['val'], list):
            for i, elem in enumerate(self.paramAnnote['annotation.xy']['val']):
               set_param_unique(ax, self.paramAnnote, 'annotation', 'annotation.', index=i)
               if self.paramAnnote['point.annotate']['val'] is not None:
                  set_param_unique(ax, self.paramAnnote, 'point.annotate', 'point.annotate.', index=i)
         else:
            set_param_unique(ax, self.paramAnnote, 'annotation', 'annotation.')
            if self.paramAnnote['point.annotate']['val'] is not None:
               set_param_unique(ax, self.paramAnnote, 'point.annotate', 'point.annotate.')


      return {'legend': legend}

if __name__ == '__main__':
   None
