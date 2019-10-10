
"""
Validation script for refinement
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['stbtel']

    def _pre(self):
        """
        Defining the studies
        """

        # malpa scalar mode
        self.add_study('vnv_global',
                       'stbtel',
                       'stb_global_refine.cas')


        # malpa scalar mode
        self.add_study('vnv_local',
                       'stbtel',
                       'stb_local_refine.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_global_stbgeo = self.get_study_file('vnv_global:STBGEO')
        res_vnv_global_stbgeo = TelemacFile(vnv_global_stbgeo)
        vnv_local_stbres = self.get_study_file('vnv_local:STBRES')
        res_vnv_local_stbres = TelemacFile(vnv_local_stbres)
        vnv_global_stbres = self.get_study_file('vnv_global:STBRES')
        res_vnv_global_stbres = TelemacFile(vnv_global_stbres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_global_stbgeo,
                   plot_mesh=True,
                   fig_size=(20, 5),
                   fig_name='img/inimesh')


        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_global_stbres,
                   plot_mesh=True,
                   fig_size=(20, 5),
                   fig_name='img/globaly_refined_mesh')


        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_local_stbres,
                   plot_mesh=True,
                   fig_size=(20, 5),
                   fig_name='img/locally_refined_mesh')

        # Closing files
        res_vnv_global_stbgeo.close()
        res_vnv_local_stbres.close()
        res_vnv_global_stbres.close()
