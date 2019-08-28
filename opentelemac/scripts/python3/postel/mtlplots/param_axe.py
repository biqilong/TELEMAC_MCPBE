r"""@author TELEMAC-MASCARET Consortium

     @brief 2D plot in a 3d lyaout
"""

def get_param_unique(deco_user, param):
    """
    @brief get common param from the dictionnary "deco_user" and copy it in the
    dictionnary "param"
    @param deco_user: parameter read from the xml file given in the "deco"
    @param param: specific param
    @return:
    """
    for key, _ in param.items():
        if key in deco_user and deco_user[key] is not None:
            try:
                param[key]['val'] = eval(deco_user[key])
            except Exception:
                param[key]['val'] = deco_user[key]
            deco_user.pop(key, None)

def set_param_unique(axe, param, label, elem, index=None):
    """
    apply to the object "axe" the method stored in param[label]['name'] and
    apply the corresponding parameter in label and elem
    @param axe:
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
        # param is a structure defined in axe.py
        # here the result is the value of the given parameter's name
        if index is not None:
            res = getattr(axe, param[label]['name'])(param[label]['val'][index],
                                                     **deco)
        else:
            if label == "legend":
                res = getattr(axe, param[label]['name'])(handles=param[label]['val'],
                                                         **deco)
            else:
                res = getattr(axe, param[label]['name'])(param[label]['val'],
                                                         **deco)
    return res

def put_in_list(param, nb_val):
    """
    If param['val'] is a single value, then it will duplicate the value into a
    list
    @param param:
    @param nb_val:
    @return:
    """
    res = param['val']
    if not isinstance(param['val'], list):
        val = param['val']
        res = []
        for _ in range(nb_val):
            res.append(val)
    return res

