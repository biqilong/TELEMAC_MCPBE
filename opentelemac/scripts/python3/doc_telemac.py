#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium

   @brief Scripts to compile the telemac-mascaret documentation
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import chdir, remove, walk, path, linesep, sep, mkdir
from argparse import ArgumentParser, RawDescriptionHelpFormatter
# ~~> dependencies towards other pytel/modules
from utils.messages import Messages
from utils.files import get_file_content
from utils.exceptions import TelemacException
from config import add_config_argument, update_config, CFGS


# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#
__author__ = "Yoann Audouin"
__date__ = "$21-Sep-2012 16:51:09$"

def clean_doc(doc_dir, fullclean):
    """
    @brief Remove latex temporary files

    @param doc_dir Directory containing the main tex file
    @param fullclean If Yes will remove the pdf file as well
    """
    _, _, files = next(walk(doc_dir))
    for fle in files:
        if fle.endswith((".bbl", ".blg", ".aux", ".out", ".toc", ".log",
                         ".nlo", "~", "idx", "ptc")):
            remove(fle)
        if fullclean and fle.endswith(".pdf"):
            remove(fle)


def compiletex(texfile, version):
    """
    @brief Full procedure for compiling a LaTeX file
             .i.e pdflatex,bibtex,pdflatex,pdflatex
    @param texfile Name of the main LaTex file
    @param version Version of the code/documentation
    """

    # First compilation
    mes = Messages(size=10)
    tail, code = mes.run_cmd("pdflatex --jobname=%s_%s %s.tex" %
                             (texfile, version, texfile), False)

    if code != 0:
        raise TelemacException('Latex compilation failed\n {}'.format(tail))

    # Bibtex compilation
    tail, code = mes.run_cmd("bibtex %s.aux" %
                             (texfile+'_'+version), False)

    # Forcing bibtex to pass
    code = 0

    if code != 0:
        raise TelemacException('Bibtex compilation failed\n {}'.format(tail))

    # Second compilation
    tail, code = mes.run_cmd("pdflatex --jobname=%s_%s %s.tex" %
                             (texfile, version, texfile), False)

    if code != 0:
        raise TelemacException('Latex compilation failed\n{}'.format(tail))

    # Third compilation
    tail, code = mes.run_cmd("pdflatex --jobname=%s_%s %s.tex" %
                             (texfile, version, texfile), False)

    if code != 0:
        raise TelemacException('Latex compilation failed\n{}'.format(tail))

#
def create_case_list_file(doc_dir, val_dir, cfg_val, cleanup):
    """
    @brief Creates the CASELIST.tex which includes
          all the test cases tex file

    @param doc_dir Path to directry containing the main LaTeX file
    @param cfg_val list of path for the examples
    @param cleanup If yes clean up the temporay files instead
                   of creating the CASELIST.Tex file
    @return the list of cases that where missing the .tex file
    """
    case_list_file = path.join(doc_dir, 'latex', 'CASELIST.tex')
    # Creating latex folder if not there
    # often the case with git (empty folder are not created)
    if not path.exists(path.join(doc_dir, 'latex')):
        mkdir(path.join(doc_dir, 'latex'))
    skipped_cases = []
    if cleanup:
        if path.exists(case_list_file):
            remove(case_list_file)
    else:
        # Remove the file if it is already there
        if path.exists(case_list_file):
            remove(case_list_file)
        with open(case_list_file, 'w') as fobj:
            # Loop on all test cases
            for case in sorted(cfg_val):
                if not path.exists(path.join(val_dir, case, 'doc',
                                             case+".tex")):
                    skipped_cases.append(case)
                else:
                    txt = linesep + r'\subincludefrom{' + \
                        val_dir.replace(sep, '/') +\
                        '/' + case + '/' + 'doc' +\
                        '/' + '}{' + case + '}' + \
                        linesep + r'\clearpage' + linesep
                    fobj.write(txt)
    return skipped_cases


def generate_ref_from_dict(exe_path, dictionary, latex_file, lng, cleanup):
    """
    @brief Generate the Latex file for the
            reference manual from the dictionary

    @param exe_path Path to homere_damocles executable
    @param dictionary Path to the dictionary to read
    @param latex_file Name of the outpu latex file that will
                         contain the reference manual
    @param lng Language for the reference manual
                 1: French
                 2: English
    """
    # Building input parameter file
    latex_dir = path.dirname(latex_file)
    if not path.exists(latex_dir):
        mkdir(latex_dir)
    param_file = path.join(latex_dir, 'gen_ref.par')
    log_file = path.join(latex_dir, 'gen_ref.log')
    # Cleanup
    if cleanup:
        if path.exists(param_file):
            remove(param_file)
        if path.exists(log_file):
            remove(log_file)
        if path.exists(latex_file):
            remove(latex_file)
    else:
        # Creating parameter file for damocles
        with open(param_file, 'w') as f:
            f.write('LATEX'+'\n')
            f.write(dictionary+'\n')
            f.write(latex_file+'\n')
            f.write(lng+'\n')
        # Removing LaTeX file if already there
        if path.exists(latex_file):
            remove(latex_file)
        # Run Fortran program
        mes = Messages(size=10)
        cmd = "%s < %s > %s" % (exe_path, param_file, log_file)
        print(cmd)
        _, code = mes.run_cmd(cmd, False)
        if code != 0:
            raise TelemacException(\
                    'Could not generated data from dictionary '
                    + '\n\nHere is the log:\n'
                    + '\n'.join(get_file_content(log_file))
                                  )


def compile_doc(doc_dir, doc_name, version, cleanup, fullcleanup):
    """
    @brief Compile the telemac-mascaret documentation

    @param doc_dir Directory containing the main LaTeX file
    @param doc_name Name of the main LaTeX file
    @param version Version of the code/documentation
    @param cleanup If yes remove temporary files
    @param fullcleanup If yes does cleanup + remove pdf
    """
    chdir(doc_dir)
    if cleanup or fullcleanup:
        clean_doc(doc_dir, fullcleanup)
        print('   - Cleaned up folder '+doc_dir+'\n')
    else:
        # removing pdflatex temporary files
        clean_doc(doc_dir, False)
        # compiling the texfile
        compiletex(doc_name, version)

def generate_notebook_pdf(doc_dir, notebook_dir):
    """
    Generate an html layout of the notebooks using ipython nbconvert
    Than coying back the file into doc_dir

    @param doc_dir (string) Path to the folder that will contain the html
                            version of the docuemntation
    @param notebook_dir (string) Path to the notebooks
    """
    # Creating doc folder if necessary
    if not path.exists(doc_dir):
        mkdir(doc_dir)

    # Running convertion in notebook folder
    # Gathering all html files
    for root, _, files in walk(notebook_dir):
        for ffile in files:
            if ffile.endswith("ipynb"):
                # Skipping notebook tmp folders
                if ".ipynb_checkpoint" in root:
                    continue
                notebook = path.join(root, ffile)
                cmd = "ipython3 nbconvert --to pdf --output-dir={} {}"\
                       .format(doc_dir, notebook)
                print("   ~> Converting "+ffile)
                # Running convertion
                mes = Messages(size=10)
                tail, code = mes.run_cmd(cmd, bypass=False)

                if code != 0:
                    raise TelemacException('nbconvert failed\n {}'.format(tail))

def main():
    """
    Main program for the compilation of the documentation of
    the telemac-mascaret system
    """
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nLoading Options and Configurations\n'+'~'*72+'\n')
    parser = ArgumentParser(
        formatter_class=RawDescriptionHelpFormatter,
        description=('''\n
By Default all the documentation are generated\n
use the options --validation/reference/user/release/theory to compile only one
        '''))
    parser = add_config_argument(parser)
    parser.add_argument(
        "-m", "--modules",
        dest="modules", default='',
        help="specify the list modules, default is taken from config file")
    parser.add_argument(
        "-M", "--misc",
        dest="misc", default='',
        help="specify the list of misc documentation to compile, "
             "default is all of them")
    parser.add_argument(
        "--validation", action="store_true",
        dest="validation", default=False,
        help="Will generate the validation documentation")
    parser.add_argument(
        "--case-list",
        dest="case_list", default='',
        help="List of cas to include in the validation documentation"
             "separated by ',' (default all of them)")
    parser.add_argument(
        "--reference", action="store_true",
        dest="reference", default=False,
        help="Will generate the reference documentation")
    parser.add_argument(
        "--user", action="store_true",
        dest="user", default=False,
        help="Will generate the user documentation")
    parser.add_argument(
        "--release", action="store_true",
        dest="release_note", default=False,
        help="Will generate the release note")
    parser.add_argument(
        "--theory", action="store_true",
        dest="theory_guide", default=False,
        help="Will generate the theory guide")
    parser.add_argument(
        "--notebook", action="store_true",
        dest="notebook", default=False,
        help="Will generate the notebook into an html version")
    parser.add_argument(
        "--clean", action="store_true",
        dest="cleanup", default=False,
        help="Will remove all temporary file "
             "generated by pdflatex")
    parser.add_argument(
        "--fullclean", action="store_true",
        dest="fullcleanup", default=False,
        help="Same as clean but removes the pdf as well")

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    options = parser.parse_args()
    update_config(options)


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Compile the valiation documentation
    doall = not (options.validation or options.user or options.reference
                 or options.release_note or options.theory_guide)
    cfg = CFGS.configs[CFGS.cfgname]
    # still in lower case
    root = CFGS.get_root()
    # Get what i to be compiled
    # By default everything if something is defined compiling only that
    if options.modules != '':
        module_list = options.modules.split(',')
    else:
        # all modules
        module_list = ['artemis', 'stbtel', 'sisyphe',
                       'telemac2d', 'telemac3d', 'tomawac', 'waqtel',
                       'telapy', 'mascaret', 'gaia', 'nestor']
    if options.misc != '':
        misc_list = options.misc.split(',')
        module_list = []
    else:
        # all docs
        misc_list = ['developer_guide', 'software_quality_plan',
                     'TelemacDocTemplate', 'git_guide']
        # If a module was specified or a specific documentation for modules
        # not compiling Misc documentation
        if options.modules != '' or not doall:
            misc_list = []

    CFGS.compute_vnv_info()

    # Get version in config if it exist use trunk otherwise
    version = cfg.get('version', 'trunk')

    # Initialise output message
    output_mess = '\n\n'
    # Look on all the modules for the documentation
    for code_name in module_list:
        print('\nCompilation of the documentation for ' + code_name
              + '\n'+'~'*72)
        # list of what to do for the module
        todo = []
        if options.validation or doall:
            if code_name not in ['telapy', 'mascaret']:
                # Building Validation LaTeX file
                doc_dir = path.join(root, 'documentation',
                                    code_name, 'validation')
                chdir(doc_dir)
                if options.case_list != '':
                    list_of_case = options.case_list.split(',')
                else:
                    list_of_case = list(cfg['VALIDATION'][code_name].keys())
                    list_of_case.remove('path')
                skiped_case = \
                    create_case_list_file(
                        doc_dir,
                        cfg['VALIDATION'][code_name]['path'],
                        list_of_case,
                        options.cleanup or options.fullcleanup)
                for case in skiped_case:
                    output_mess += r'   - /!\ Missing LaTeX file for ' + \
                                   case+'\n'
                todo.append('validation')
        if options.reference or doall:
            if code_name not in ['telapy', 'mascaret', 'nestor']:
                # Path to the dictionary
                dictionary = path.join(root, 'sources', code_name,
                                       code_name+'.dico')
                # Path to latex File
                latex_file = path.join(root, 'documentation',
                                       code_name, 'reference',
                                       'latex', 'Corpus.tex')
                # English only for now
                lng = '2'
                # Path to bin directory
                exe_path = path.join(\
                        root, 'builds', CFGS.cfgname,
                        'bin', 'damocles'+cfg['SYSTEM']['sfx_exe'])
                generate_ref_from_dict(\
                        exe_path, dictionary, latex_file, lng,
                        options.cleanup or options.fullcleanup)
                todo.append('reference')
        if options.user or doall:
            if code_name not in ['mascaret']:
                # Normal Compilation of a LaTeX file
                todo.append('user')
        if options.theory_guide or doall:
            # theory guide only available for telemac3d
            if code_name in ['telemac3d', 'mascaret', 'waqtel']:
                todo.append('theory_guide')
        for doc_type in todo:
            doc_dir = path.join(root, 'documentation',
                                code_name, doc_type)
            chdir(doc_dir)
            # Check if the file exist
            if path.exists(path.join(doc_dir,
                                     code_name + "_" + doc_type + ".tex")):
                compile_doc(doc_dir, code_name+'_'+doc_type,
                            version,
                            options.cleanup, options.fullcleanup)
            else:
                raise TelemacException(\
                        "   - Error for {} {}, {}.tex "
                        "not found ".format(code_name,
                                            path.basename(doc_dir),
                                            code_name+"_"+doc_type))
            if not (options.cleanup or options.fullcleanup):
                output_mess += '   - Created %s_%s_%s.pdf\n' % \
                              (code_name, doc_type, version)
    # List of the other documentation
    for doc in misc_list:
        print('\nCompilation of the documentation for ' + doc
              + '\n'+'~'*72)
        doc_dir = path.join(root, 'documentation',
                            'Misc', doc)

        if doc == 'notebook':
            notebook_dir = path.join(root, 'examples', 'notebook')
            generate_notebook_pdf(doc_dir, notebook_dir)
        else:

            chdir(doc_dir)
            if path.exists(path.join(doc_dir, doc + ".tex")):
                compile_doc(doc_dir, doc,
                            version,
                            options.cleanup, options.fullcleanup)
            else:
                raise TelemacException(\
                        "   - Error in {}, {}.tex "
                        "not found ".format(path.basename(doc_dir), doc))
        if not (options.cleanup or options.fullcleanup):
            output_mess += '   - Created %s_%s.pdf\n' % \
                          (doc, version)

    print(output_mess)
    print('\n\n'+'~'*72)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nMy work is done\n\n')

    sys.exit(0)


if __name__ == "__main__":
    main()
