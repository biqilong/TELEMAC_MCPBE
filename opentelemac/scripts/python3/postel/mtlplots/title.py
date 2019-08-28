r"""@author TELEMAC-MASCARET Consortium

     @brief
"""
from postel.mtlplots.param_axe import set_param_unique
from postel.mtlplots.param_axe import get_param_unique

class Title(object):
    """
    @brief Class where we store all necessary parameter of the Title
    """
    def __init__(self, deco_user, decoDefault):

        self.title = None
        self.param_title = {\
             'graph_title': {'name': 'set_title', 'val': None},
             'fontsize_title': {'name': 'fontsize',
                                'val': decoDefault['axes.titlesize']},
             'loc_title': {'name': 'loc', 'val': None},
             'x_title': {'name': 'x', 'val': None},
             'y_title': {'name': 'y', 'val': None},
                           }
        self.get_param(deco_user)

    def get_param(self, deco_user):
        """
          Get all parameter from the deco_user
          @return:
          """
        get_param_unique(deco_user, self.param_title)

    def set_title(self, axe):
        set_param_unique(axe, self.param_title, 'graph_title', '_title')
