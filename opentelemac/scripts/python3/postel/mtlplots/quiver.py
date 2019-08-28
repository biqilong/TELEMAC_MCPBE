r"""@author TELEMAC-MASCARET Consortium

     @brief
"""

class Quiver(object):
    """
    @brief Class where we store all the parameter for quiver
    """
    vector_dict = ['units', 'angles', 'scale', 'scale_units', 'width',
                   'headwidth', 'headlength', 'headaxislength', 'minshaft',
                   'minlength', 'pivot', 'color', 'cmap']
    key_dict = ['key_x', 'key_y', 'key_length', 'key_label', 'labelpos',
                'coordinates', 'key_color']

    def __init__(self, deco_user):
        self.param_quiver = {}
        self.param_key = {}
        self.val_key = {'X': 0.8, 'Y': 0.05, 'U': 1.0, 'label': ''}
        self.key = False
        self.get_param(deco_user)

    def get_param(self, deco_user):
        """
        Get all parameter for the quiver
        @param deco_user:
        @return:
        """
        if 'key' in deco_user.keys():
            try:
                deco_user['key'] = eval(deco_user['key'])
            except Exception:
                pass
            if deco_user['key']:
                self.key = True
        for key in deco_user:
            if key in Quiver.vector_dict:
                if key == ('scale' or 'width' or \
                           'headwidth' or 'headlength' or
                           'headaxislength' or 'minshaft' or 'minlength' or
                           'linewidths'):
                    self.param_quiver[key] = float(deco_user[key])
                else:
                    self.param_quiver[key] = deco_user[key]
            elif key in Quiver.key_dict:
                if key == 'key_x':
                    self.val_key['X'] = float(deco_user[key])
                elif key == 'key_y':
                    self.val_key['Y'] = float(deco_user[key])
                elif key == 'key_length':
                    self.val_key['U'] = float(deco_user[key])
                elif key == 'key_label':
                    self.val_key['label'] = deco_user[key]
                elif key == 'key_color':
                    self.param_key['color'] = deco_user[key]
                else:
                    try:
                        self.param_key[key] = eval(deco_user[key])
                    except Exception:
                        self.param_key[key] = deco_user[key]

    def set_quiver(self, axe, x, y, u, v):
        c_s = axe.quiver(x, y, u, v, **self.param_quiver)
        if self.key:
            axe.quiverkey(c_s, self.val_key['X'], self.val_key['Y'],
                          self.val_key['U'], self.val_key['label'],
                          **self.param_key)
