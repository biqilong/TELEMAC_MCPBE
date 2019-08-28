
"""
Validation script for api
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
        self.tags = ['python3']

    def _pre(self):
        """
        Defining the studies
        """

        # Telemac2d api run of gouttedo
        self.add_command('vnv_t2d',
                         'python3 gouttedo.py')


        # Telemac2d api run of gouttedo in parallel
        self.add_command('vnv_t2d-par',
                         'mpiexec -n 4 python3 gouttedo.py')


        # Tomawac api run of dean
        self.add_command('vnv_wac',
                         'python3 dean.py')


        # Tomawac api run of dean in parallel
        self.add_command('vnv_wac-par',
                         'mpiexec -n 4 python3 dean.py')


        # Telemac3d api run of gouttedo3d
        self.add_command('vnv_t3d',
                         'python3 gouttedo3d.py')


        # Telemac3d api run of gouttedo3d in parallel
        self.add_command('vnv_t3d-par',
                         'mpiexec -n 4 python3 gouttedo3d.py')


        # Artemis api run of bj78
        self.add_command('vnv_art',
                         'python3 bj78.py')


        # Telemac2d-sisyphe api run of conservation
        self.add_command('vnv_t2d-sis',
                         'python3 conservation.py')


        # Mascaret api run of Test1
        self.add_command('vnv_mascaret',
                         'python3 test1_mascaret.py')


        # Mascaret api run of Test_Tracer
        self.add_command('vnv_mascaret_tracer',
                         'python3 test_mascaret_tracer.py')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

