r"""@author TELEMAC-MASCARET Consortium

      @brief 2D plot in a 3d lyaout
"""
#!/usr/bin/env python
from __future__ import print_function
from os import path
from utils.exceptions import TelemacException

DECO_FIGURE = {'data':{'title':'openTELEMAC'},
               'look':{'savefig.dpi':100, 'savefig.format':"png",
                       'grid': True, 'crax.xlim': 0.02,
                       'crax.ylim': 0.02}
              }
DECO_LAYER = {'data':{},
              'look':{'1d-line-colours':[],
                      '1d-line-symbols':[],
                      'contour.levels' : '12',
                      'contour.major.style' : 'solid',
                      'contour.minor.style' : 'dashed',
                      'contour.major.color' : 'black',
                      'contour.minor.color' : 'gray'}
             }
HRWD = {'plot_config_version' : 'R1r13',
        'hrw_logo' : 'hr_wallingford.png',
        'x_margin' : '0.02',
        'y_margin' : '0.02',
        'margins.x' : '0.02',
        'margins.y' : '0.02',
        'client_logo' : 'client_company.png',
        'plot.boundary.padding' : '0.05',
        'contour.nlevels' : '20',
        'cmap.preset' : 'jet',
        'cmap.user.min_max' : 'False',
        'cmap.user.min' : '138',
        'cmap.user.max' : '150',
        'contour_line_color ' : 'black',
        'contour_line_color2' : 'gray',
        'inline_label_fmt' : '1.1f',
        'inline_label_size' : '9',
        'colorbar.label.fmt' : '1.3f',
        'plot_xlabel' : 'x-axis (m)',
        'plot_ylabel' : 'y-axis (m)',
        'scatter.marker.type' : 'o',
        'scatter.marker.size' : '100',
        'scatter.marker.alpha' : '0.5',
        'scatter.marker.outlines' : 'False',
        'mesh_line_color_rgba' : '0.0,0.0,0.6,1.0',
        'quiver_line_color_rgba' : '0.0,0.0,0.1,1.0',
        'filled_triangle_alpha' : '0.99',
        'trimesh_contour_inline_labels' : 'True',
        'filled_curvilinear_alpha' : '1.0',
        'use_image_bound_box' : 'False',
        'image_retint' : 'False',
        'image_retint_cmap' : 'gray',
        'trimesh_overlap_zo' : 'True',
        'trimesh_labels_density' : '1188',
        'vector_regrid_density' : '80',
        'outline_grid_dimension' : '10',
        'anno.footer.height' : '1.0',
        'anno.footer.width' : '1.0',
        'anno.footer.placement' : 'bottom',
        'anno.title.font.size' : '18',
        'anno.subtitle.font.size' : '11',
        'anno.footer.font.size' : '11',
        'anno.num_logos' : '0',
        'anno.num_labels' : '1',
        'legend.location' : 'best',
        'quiver.legend.location' : 'lower right',
        'anno.label1.value' : 'My Plot',
        'anno.label1.name' : 'Plot Title ',
        'anno.label1.font.size' : '18 ',
        'anno.label1.enabled' : 'True',
        'anno.label1.font.family' : 'helvetica',
        'anno.label1.rotation' : '0',
        'anno.label1.rect' : \
                '0.000000,0.900000,1.000000,0.100000,center,bottom',
        'anno.label1.font.style' : 'normal',
        'anno.label1.option' : 'headlineString ',
        'anno.label1.font.weight' : 'bold',
        'anno.label1.placement' : 'plot'
       }

def get_colour_map(file_name):

    red = []
    green = []
    blue = []
    if path.exists(file_name):
        f = open(file_name, 'r')
    else:
        raise TelemacException(\
                "... Could not access/read expected colour "
                "map file content: {} " .format(file_name))
    try:
        import xml.etree.ElementTree as XML
        xml_tree = XML.parse(f)
        xml_root = xml_tree.getroot()
        f.close()
    except:
        f.close()
        raise TelemacException(\
                "... Could not access/read expected colour "
                "map file content: {} " .format(file_name))

    for entry in xml_root.findall("Point"):
        red.append((float(entry.attrib["x"]),
                    float(entry.attrib["r"]),
                    float(entry.attrib["r"])))
        blue.append((float(entry.attrib["x"]),
                     float(entry.attrib["b"]),
                     float(entry.attrib["b"])))
        green.append((float(entry.attrib["x"]),
                      float(entry.attrib["g"]),
                      float(entry.attrib["g"])))

    return {'blue': blue, 'red': red, 'green': green}
