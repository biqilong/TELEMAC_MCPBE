from data_manip.extraction.telemac_file import TelemacFile
import numpy as np


def read_write_add_var(file_name):
    """
    """

    print("Adding new variable in {}".format(file_name))
    res = TelemacFile(file_name, access='rw')

    # Loading data from file
    res.read(res)

    # Adding new variable
    res._varnames.append('stuff')
    res._varunits.append('M')
    res._nvar += 1

    bathy = np.ones((res.ntimestep, 1, res.npoin3), dtype=np.float64)

    res._values = np.append(res._values, bathy, axis=1)

    # Writting class data
    res.write()

    res.close()

def read_write_add_time(file_name):
    """
    """

    print("Adding new time step in {}".format(file_name))
    res = TelemacFile(file_name, access='rw')

    res.read(res)

    res._times = np.append(res._times, [666.0], axis=0)
    res._ntimestep += 1

    bathy = np.ones((1, res._nvar, res.npoin3), dtype=np.float64)

    res._values = np.append(res._values, bathy, axis=0)

    res.write()

    res.close()

def read_write(file_name1, file_name2):

    print("Reading content and writting it in another file:\n{} -> {}".format(file_name1, file_name2))
    res1 = TelemacFile(file_name1, access='r')
    res2 = TelemacFile(file_name2, access='w')

    res2.read(res1)

    res2.write()

    res1.close()
    res2.close()

def read_write_bnd(file_name1, bnd_name1, file_name2, bnd_name2):

    print("Reading content and writting it in another file:\n{} -> {}".format(file_name1, file_name2))
    res1 = TelemacFile(file_name1, bnd_file=bnd_name1, access='r')
    res2 = TelemacFile(file_name2, bnd_file=bnd_name2, access='w')

    res2.read(res1)

    res2.write()

    res1.close()
    res2.close()


def main():
    import shutil
    shutil.copy('geo_triangular_shelf.slf', 'geo_triangular_shelf1.slf')
    shutil.copy('geo_triangular_shelf.cli', 'geo_triangular_shelf1.cli')

    shutil.copy('geo_gouttedo.slf', 'geo_gouttedo1.slf')
    shutil.copy('geo_gouttedo.cli', 'geo_gouttedo1.cli')
    shutil.copy('geo_gouttedo.med', 'geo_gouttedo1.med')
    shutil.copy('geo_gouttedo.clm', 'geo_gouttedo1.clm')

    read_write('geo_gouttedo1.slf', 'geo_gouttedo2.slf')
    read_write('geo_gouttedo1.med', 'geo_gouttedo2.med')

    read_write_bnd('geo_triangular_shelf1.slf', 'geo_triangular_shelf1.cli',
                   'geo_triangular_shelf3.slf', 'geo_triangular_shelf3.cli')
    read_write_bnd('geo_triangular_shelf1.slf', 'geo_triangular_shelf1.cli',
                   'geo_triangular_shelf3.med', 'geo_triangular_shelf3.clm')
    read_write_bnd('geo_gouttedo1.slf', 'geo_gouttedo1.cli',
                   'geo_gouttedo3.slf', 'geo_gouttedo3.cli')
    read_write_bnd('geo_gouttedo1.med', 'geo_gouttedo1.clm',
                   'geo_gouttedo3.med', 'geo_gouttedo3.clm')

    read_write_add_var('geo_gouttedo1.med')
    read_write_add_var('geo_gouttedo1.slf')

    read_write_add_time('geo_gouttedo1.slf')
    read_write_add_time('geo_gouttedo1.med')

if __name__ == "__main__":
    main()
