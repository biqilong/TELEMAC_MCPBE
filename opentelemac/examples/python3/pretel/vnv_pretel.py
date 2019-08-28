
"""
Validation script for pretel
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

        # Scanning double precision big-endian file
        self.add_command('vnv_1_scan_big_double',
                         'run_selafin.py scan r2d_bowl_vf_gb_DoublePrecision_BigEndian.slf --core')


        # Scanning double precision little endian file
        self.add_command('vnv_2_scan_little_double',
                         'run_selafin.py scan r2d_bowl_vf_gb_DoublePrecision_LittleEndian.slf --core')


        # Scanning single precision big endian file
        self.add_command('vnv_3_scan_big_single',
                         'run_selafin.py scan r2d_bowl_vf_gb_SinglePrecision_BigEndian.slf --core')


        # Scanning single precision little endian file
        self.add_command('vnv_4_scan_little_single',
                         'run_selafin.py scan r2d_bowl_vf_gb_SinglePrecision_LittleEndian.slf --core')


        # Scanning of a spectral file (Tomawac output)
        self.add_command('vnv_5_spec',
                         'run_selafin.py spec opposing_cur.spe')


        # Remove one time step each 2
        self.add_command('vnv_6_chop',
                         'run_selafin.py chop r2d_gouttedo.slf r2d_gouttedo_chop.slf -f 1 -s -1 -d 2')


        # Keeping only last time step in file
        self.add_command('7_alter-1',
                         'run_selafin.py alter r2d_gouttedo.slf r2d_gouttedo_last_time.slf -f -1 -s -1 -d 1 ')


        # Switch endianess
        self.add_command('7_alter-2',
                         'run_selafin.py alter r2d_bowl_vf_gb_SinglePrecision_LittleEndian.slf r2d_bowl_BigEndian.slf --endian')


        # Increase water depth by 10m
        self.add_command('7_alter-3',
                         'run_selafin.py alter r2d_gouttedo.slf r2d_gouttedo_10m.slf --Z?="WATER DEPTH" --Z+? 10')


        # Merge of an output from 0.0s to 1.8s with one from 2.0s to 4.0s
        self.add_command('vnv_8_merge',
                         'run_selafin.py merge r2d_gouttedo_part1.slf r2d_gouttedo_part2.slf r2d_gouttedo_full.slf')


        # Splitting in 4 a mesh
        self.add_command('vnv_9_subdivide',
                         'run_selafin.py subdivide r2d_gouttedo.slf r2d_gouttedo_x4.slf ')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

