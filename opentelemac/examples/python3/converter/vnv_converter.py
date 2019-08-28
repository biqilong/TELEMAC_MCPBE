
"""
Validation script for converter
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['python3', 'med']

    def _pre(self):
        """
        Defining the studies
        """

        # Convertion from Serafin to Med geometry + boundary
        self.add_command('vnv_srf2med_geo',
                         'converter.py srf2med mesh.slf -b mesh.cli mesh2.slf.med')


        # Convertion from Serafin to Med 2d result file only
        self.add_command('vnv_srf2med_r2d',
                         'converter.py srf2med r2d.slf r2d.slf.med')


        # Convertion from Serafin to Med 2d result file only
        self.add_command('vnv_srf2med_r3d',
                         'converter.py srf2med r3d.slf r3d.slf.med')


        # Convertion from Serafin to VTK 2d result file only
        self.add_command('vnv_srf2vtk_r2d',
                         'converter.py srf2vtk r2d.slf r2d.vtk')


        # Convertion from Serafin to VTK 2d result file only
        self.add_command('vnv_srf2vtk_r3d',
                         'converter.py srf2vtk r3d.slf r3d.vtk')


        # Convertion from MED to Serafin geometry + boundary
        self.add_command('vnv_med2srf_geo',
                         'converter.py med2srf mesh.med -b mesh.bnd mesh.med.slf')


        # Convertion from MED to Serafin 2d result file only
        self.add_command('vnv_med2srf_r2d',
                         'converter.py med2srf r2d.med r2d.med.slf')


        # Convertion from MED to Serafin 2d result file only
        self.add_command('vnv_med2srf_r3d',
                         'converter.py med2srf r3d.med r3d.med.slf')


        # Refining a mesh file splitting each triangle in 4
        self.add_command('vnv_refinement',
                         'converter.py refine mesh.slf -b mesh.cli meshx4.slf')


        # Converting kenue polygon file into a Serafin file
        self.add_command('kenue2shp-1',
                         'converter.py kenue2shp polygon.i2s')


        # Converting kenue polygon file into a Serafin file
        self.add_command('kenue2shp-2',
                         'converter.py kenue2shp polylines.i2s')

    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

