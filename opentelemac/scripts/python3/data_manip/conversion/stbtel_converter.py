#!/usr/bin/env python3
r"""@author TELEMAC-MASCARET Consortium

    @brief Run a converions of mesh files using stbtel
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import os
import subprocess as sp

try:
    from MEDLoader import MEDFileUMesh, DataArrayInt
    ML_AVAIL = True
except ImportError:
    ML_AVAIL = False

from utils.exceptions import TelemacException

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__ = "Yoann Audouin"
__date__ = "$21-Sep-2012 16:51:09$"

CAS_CANVAS = \
    """
    /
    / CONVERSION OF MESH FILE USING STBTEL
    /
    CONVERTER = YES
    DEBUG = {debug}
    /
    / INPUT FILE INFORMATION
    /
    INPUT FILE FORMAT : '{input_format}'
    INPUT FILE : '{input_file}'
    BOUNDARY CONDITION IN SERAFIN FORMAT : {srf_bnd}
    {in_add_file}
    /
    / OUTPUT FILE INFORMATION
    /
    OUTPUT FILE FORMAT : '{output_format}'
    OUTPUT FILE : '{output_file}'
    {out_add_file}
    """

def add_bnd(input_file):
    """
        Add the boundary element and the groups in a med file generated by the
        converter in Stbtel
        @param input_file Name of the file in which to add the boundaries
    """

    # Open the file
    mesh_file_2d = MEDFileUMesh(input_file, "MESH")

    # Change coordiantes names TODO: Change that in the converter
    coords = mesh_file_2d.getCoords()
    coords.setInfoOnComponents(["X [m]", "Y [m]"])

    # Extract 2D mesh
    mesh_2d = mesh_file_2d.getMeshAtLevel(0)
    mesh_0d = mesh_file_2d.getMeshAtLevel(-2)

    # Compute boundary elements
    mesh_1d = mesh_2d.computeSkin()

    # Order cells for med format
    mesh_1d.sortCellsInMEDFileFrmt()

    n_segment = mesh_1d.getNumberOfCells()
    # Create groups
    grps = []
    for group_name in mesh_file_2d.getGroupsNames():
        cells = []
        pt_in_group = mesh_file_2d.getGroupArr(-2, group_name)
        node_in_group = [mesh_0d.getNodeIdsOfCell(pt[0])[0] \
                         for pt in pt_in_group]
        for cell in range(n_segment):
            cell1, cell2 = mesh_1d.getNodeIdsOfCell(cell)
            if group_name != 'CONLIM_2222':
                if cell1 in node_in_group and cell2 in node_in_group:
                    cells.append(cell)
            else:
                if cell1 in node_in_group or cell2 in node_in_group:
                    cells.append(cell)
        grp = DataArrayInt(cells)
        grp.setName(group_name)
        grps.append(grp)

    # Add Boundary elements to the file
    mesh_file_2d.setMeshAtLevel(-1, mesh_1d)

    # Add the groups to the file
    mesh_file_2d.setGroupsAtLevel(-1, grps)

    # Write the new information
    mesh_file_2d.write(input_file, 0)


def build_cas(extens, input_format, input_file,
              output_format, output_file,
              srf_bnd, debug,
              bnd_file, log_file):
    """
    Build the steering file for stbtel

    @param extens Parrallel extension 00000-00000
    @param input_format Format of the input file
    @param input_file Path of the input file
    @param output_format Format of the output file
    @param output_file Path of the output file

    @return the steering file
    """
    # Building canvas for steering file
    ldebug = 'YES' if debug else 'NO'
    lsrf_bnd = 'YES' if srf_bnd else 'NO'
    # Additional files
    ## input files
    in_add_file = ''
    if bnd_file:
        in_add_file += "BOUNDARY FILE : '%s' \n" % (bnd_file + extens)
    if log_file:
        in_add_file += "LOG FILE : '%s' \n" % (log_file + extens)
    ## Output files
    out_add_file = ''
    if bnd_file:
        if output_format in ["SERAFIN", "CGNS", "SERAFIND"]:
            out_add_file += "OUTPUT BOUNDARY FILE : '%s'\n" % \
                            (output_file[:-3] + 'cli' + extens)
        if output_format == "MED":
            out_add_file += "OUTPUT BOUNDARY FILE : '%s'\n" % \
                            (output_file[:-3] + 'bnd' + extens)

    return CAS_CANVAS.format(
        debug=ldebug,
        input_format=input_format,
        input_file=input_file + extens,
        srf_bnd=lsrf_bnd,
        in_add_file=in_add_file,
        output_format=output_format,
        output_file=output_file + extens,
        out_add_file=out_add_file)

def run_converter(action, input_file, output_file, root_dir, bnd_file,
                  log_file, ndomains, srf_bnd, debug):
    """
    Run a conversion using stbtel

    @param action (string) Name of the conversion to do
    @param input_file (string) Name of the input file
    @param output_file (string) Name of the output_file
    @param root_dir (string) Path to the root of Telemac
    @param bnd_file (string) Boundary file
    @param log_file (string) log file (for unv format only)
    @param ndomains (integer) Number of parallel files
    @param srf_bnd (boolean) If True read boundaries from the boundary file
                             instead it is read from the input file
    @param debug (boolean) If True stbtel will write debug information
    """
    if action == 'srf2med':
        input_format = 'SERAFIN'
        output_format = 'MED'
    elif action == 'srf2vtk':
        input_format = 'SERAFIN'
        output_format = 'VTK'
    elif action == 'med2srfd':
        input_format = 'MED'
        output_format = 'SERAFIND'
    elif action == 'med2srf':
        input_format = 'MED'
        output_format = 'SERAFIN'
    else:
        raise TelemacException("unknow conversion {}".format(action))

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # ~~~~ Identifying input and output informations ~~~~~~~~~~~~~~~~~~~

    # Loop on the number of domains
    for idom in range(ndomains):
        # build the extension added to each file of the distributed mesh by
        # partel
        if ndomains == 1:
            # Nothing if we are dealling with a non-distributed mesh
            extens = ''
        else:
            # the string of format 00000-00000
            extens = str(ndomains - 1).zfill(5) + '-' + str(idom).zfill(5)

        # Treatment in case we are doing a refinement
        cas = build_cas(extens, input_format, input_file,
                        output_format, output_file, srf_bnd, debug,
                        bnd_file, log_file)
        # Writting the steering file
        cas_name = 'stb' + extens + ".cas"
        with open(cas_name, "w") as fobj:
            fobj.write(cas)
        # Running stbtel
        path_stbtel = "stbtel.py"
        if root_dir is not None:
            path_stbtel = os.path.join(root_dir, "scripts",
                                       "python3", path_stbtel)
        stbtel_args = [path_stbtel, cas_name]
        if root_dir is not None:
            stbtel_args += ["-r", root_dir]
        print("Calling: " + " ".join(stbtel_args))
        code = sp.call(stbtel_args)

        if code != 0:
            sys.exit(code)
        else:
            # Remove the case file
            os.remove(cas_name)
        # In case of a conversion srf to med adding bnd elements
        # and we have boundaries
        if input_format == 'SERAFIN' and output_format == 'MED' \
                and bnd_file:
            if ML_AVAIL:
                add_bnd(output_file)
                print("-> Adding boundary elements")
            else:
                print("Warning: \n"
                      "MEDLoader unavailable boundary elements will " +
                      "not be added when converting to MED")
                print("Install MEDLoader or run inside a salome shell")


def stbtel_converter_parser(subparser, name, help_msg):
    """
    Adding argument to parser for stbtel converter

    @param subparser (ArgumentParser) the parser to update
    @param name (string) Name of the action
    @param help_msg (string) Help message for that action

    @return (ArgumentParser) the updated parser
    """

    parser = subparser.add_parser(name,\
            help=help_msg)
    parser.add_argument(
        "input_file", default="",
        help="name of the input file also defines the input format")
    # output name option
    parser.add_argument(
        dest="output_file", default="",
        help="name of the output file also defines the output format")
    # the boundary file option
    parser.add_argument(
        "-b", "--boundary-file",
        dest="bnd_file", default="",
        help="name of the boundary file")
    # the log file option
    parser.add_argument(
        "-l", "--log-file",
        dest="log_file", default="",
        help="name of the log file")
    # option for converting distributed mesh
    parser.add_argument(
        "-n", "--ndomains",
        type=int, dest="ndomains", default=1,
        help="number of sub-domains of the distributed mesh")
    # Option to tell stbtel to read the boundary conidtion from the boundary
    # file
    parser.add_argument(
        "--srf-bnd", action="store_true",
        dest="srf_bnd", default=False,
        help="tell stbtel to read the boundary condition from the "
             "boundary file")
    # the debug mode option
    parser.add_argument(
        "--debug", action="store_true",
        dest="debug", default=False,
        help="Enable debug mode which displays more informations "
             "during run time")
    # root directory
    parser.add_argument(
        "-r", "--root-dir",
        dest="root_dir", default=None,
        help="specify the root, default is taken from config file")

    return subparser
