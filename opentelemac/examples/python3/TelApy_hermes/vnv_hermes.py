
"""
Validation script for hermes
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

        # Telemac2d api run of gouttedo
        self.add_command('vnv_read_write',
                         'python3 read_write.py')


        # Telemac2d api run of gouttedo
        self.add_command('vnv_read_write_format',
                         'python3 read_write_format.py')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """
