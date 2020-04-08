"""
Class for Telemac steering file manipulation
"""

import re
from os import path
import shutil

from execution.tools import KEY_COMMENT, EMPTY_LINE, KEY_EQUALS, VAL_EQUALS, \
                            KEY_NONE, convert_to_type, check_type, format72
from execution.telemac_dico import TelemacDico, DICOS
from utils.exceptions import TelemacException
from utils.files import is_newer


SPECIAL = ['VARIABLES FOR GRAPHIC PRINTOUTS',
           'VARIABLES FOR 3D GRAPHIC PRINTOUTS',
           'VARIABLES FOR 2D GRAPHIC PRINTOUTS',
           'VARIABLES TO BE PRINTED',
           'COUPLING WITH',
          ]

def get_dico(module):
    """
    Returns path of the dictionary for a given module

    @param module (str) name of a telemac-mascaret module

    @returns the path
    """
    from config import CFGS
    if CFGS is None:
        raise TelemacException(\
                "This function only works if a configuration is set")
    return path.join(CFGS.get_root(), 'sources', module, module+'.dico')

class TelemacCas():
    """
    Class to hanlde a Telemac-mascaret steering file
    """

    def __init__(self, file_name, dico_file, access='r', check_files=True):
        """
        Init of the class

        @param file_name (string) Name of the steering file
        @param dico_file (string) Name of the dictionary to use
        @param access (string) r (read) or w (write)
        @param check_files (bool) If true checking that input files exist
        """
        self.file_name = file_name
        #TODO: Add identification of the module
        self.values = {}
        self.lang = ''
        self.in_files = {}
        self.out_files = {}
        self.access = access
        self.check_files = check_files
        # Check if the dictionary has already been parsed
        if dico_file not in DICOS:
            DICOS[dico_file] = TelemacDico(dico_file)
        self.dico = DICOS[dico_file]

        if 'w' not in self.access:
            # Reading data from file
            self._parse_cas()
            # Getting language info
            self._identify_lang()
            # Checking that all the keyword are from the dictionary
            self._check_content()
            # Convert content according to its type
            self._convert_values()
            # Checking that values are in CHOIX/CHOIX1
            self._check_choix()
            # Identify input and ouput files
            self._set_io_files()

    def _parse_cas(self):
        """
        Parse the steering file and identify (key, value)
        And updates self.values according to the pairs identified
        """
        lines = []
        # ~~ clean ending empty lines
        with open(self.file_name, 'r') as f:
            for line in f.readlines():
                # Remove trailing spaces (left and right) and \n
                line = line.strip(' ').rstrip('\n')
                # not adding empty lines
                if line != '':
                    # Skipping &key (&ETA, &FIN...)
                    if line[0] == '&':
                        continue
                    lines.append(line)

        # ~~ clean comments
        core = []
        for line in lines:
            line = line.replace('"""', "'''")\
                       .replace('"', "'")\
                       .replace("''", '"')
            proc = re.match(KEY_COMMENT, line+'/')
            line = proc.group('before').strip() + ' '
            proc = re.match(EMPTY_LINE, line)
            if not proc:
                core.append(line)

        # Creates a one line of the cleaned up steering
        cas_stream = (' '.join(core))
        # ~~ Matching keword -> values
        while cas_stream != '':
            # ~~ Matching keyword
            proc = re.match(KEY_EQUALS, cas_stream)
            if not proc:
                raise TelemacException(\
                   ' Error while parsing steering file {} '
                   'incorrect line:\n{}'\
                   .format(self.file_name, cas_stream[:100]))
            keyword = proc.group('key').strip()
            cas_stream = proc.group('after')    # still hold the separator
            # ~~ Matching value
            proc = re.match(VAL_EQUALS, cas_stream)
            if not proc:
                raise TelemacException('No value to keyword '+keyword)
            val = []
            # The value can be on multiple lines
            while proc:
                if proc.group('val') == '"':
                    val.append('')
                else:
                    val.append(proc.group('val').replace("'", ''))
                cas_stream = proc.group('after')    # still hold the separator
                proc = re.match(VAL_EQUALS, cas_stream)
            # Updating the value with the last one read
            self.values[keyword] = val

    def _identify_lang(self):
        """
        Identifying the language of the steering file
        """
        # Identifying language
        # Looping on all the keywords of the steering file
        # And looking for the first keyword that is in only french/english
        for key in self.values:
            # Skipping keyword that are the same in french and english
            if key in self.dico.fr2gb and key in self.dico.gb2fr:
                continue
            if key in self.dico.fr2gb:
                self.lang = 'fr'
            else:
                self.lang = 'en'
            break

    def _check_content(self):
        """
        Checks that all the keywords are from the dictionary
        """
        if self.lang == 'en':
            key_list = self.dico.gb2fr
        else:
            key_list = self.dico.fr2gb

        for key in self.values:
            if key not in key_list:
                raise TelemacException(\
                    "Unknown keyword {} in steering file {}"\
                    .format(key, self.file_name))

    def _check_choix(self):
        """
        Check if the keyword value is in the list of choix
        """
        for key, value in self.values.items():
            # If empty value doing nothing
            if value in ['', []]:
                continue
            # Check if we have a keyword with choices
            choix = 'CHOIX1' if self.lang == 'en' else 'CHOIX'
            if choix in self.dico.data[key]:
                list_choix = self.dico.data[key][choix]
                # Special treatment for grapchi outputs like keywords
                if key in SPECIAL:
                    if key == 'COUPLING WITH':
                        list_val = value.split(";")
                    else:
                        list_val = value.split(",")
                    for val in list_val:
                        tmp_val = str(val.strip(' 0123456789*'))
                        new_val = 'k' + tmp_val + 'i'
                        new_val2 = 'k' + tmp_val
                        new_val3 = tmp_val + 'i'
                        # Handling case of Tracer lists such as T1, T* ...
                        # Special case for gaia where you have stuff like kSi
                        # and kES where k and i are number or *
                        if not(str(val).strip(' ') in list_choix or \
                           str(val).rstrip('123456789*') in list_choix or \
                           new_val in list_choix or \
                           new_val2 in list_choix or \
                           new_val3 in list_choix):
                            raise TelemacException(\
                       "In {}: \n".format(self.file_name)+
                       "The value for {} ({})is not among the choices: \n{}"\
                       .format(key, val, list_choix))
                elif isinstance(value, list):
                    for val in value:
                        if str(val).strip(' ') not in list_choix:
                            raise TelemacException(\
                         "In {}: \n".format(self.file_name)+
                         "The value for {} ({})is not among the choices: \n{}"\
                         .format(key, val, list_choix))
                else:
                    if str(value).strip(' ') not in list_choix:
                        raise TelemacException(\
                         "In {}: \n".format(self.file_name)+
                         "The value for {} ({})is not among the choices: \n{}"\
                         .format(key, value, list_choix))

    def _convert_values(self):
        """
        Convert string value to its Python type and replace key by english key
        """


        # Updating values dict
        # Converting value and translating key if necessary
        # Using a copy of keys as the loop removes some elements
        list_keys = list(self.values.keys())
        for keyword in list_keys:
            value = self.values[keyword]
            if self.lang == 'en':
                gb_keyword = keyword
            else:
                gb_keyword = self.dico.fr2gb[keyword]

            key_info = self.dico.data[gb_keyword]

            # Convert according to type of the keyword
            if self.lang == 'fr':
                # All keys in values are to be in english
                del self.values[keyword]

            self.values[gb_keyword] = convert_to_type(key_info['TYPE'], value)

    def _set_io_files(self):
        """
        Detect input and ouput files from the steergin file data
        Checks that input files exists as well
        """
        for key in self.values:
            # Identify a file keyword (they have a SUBMIT key)
            key_data = self.dico.data[key]
            if 'SUBMIT' in key_data:
                # input file
                if 'LIT' in key_data['SUBMIT']:
                    self.in_files[key] = key_data['SUBMIT']
                    val = self.values[key].strip("'")
                    if self.check_files:
                        if not path.exists(val):
                            raise TelemacException(\
                                "In {} missing file for {}:\n {}"\
                                .format(self.file_name, key, val))
                # output file
                if 'ECR' in key_data['SUBMIT']:
                    self.out_files[key] = key_data['SUBMIT']

    def write(self, cas_file):
        """
        Write content of class in ascii for into a file
        """
        # TODO: fancier write using rubrique
        # Name of current rubriques
        rubs = ['', '', '']
        # Numerotation of current rubrique
        irubs = [0, 0, 0]

        with open(cas_file, 'w') as f:
            # Following dictionary order (for nicer output)
            for key in self.dico.data:
                if key in self.values:
                    # keyword adaptaion to language
                    if self.lang == 'fr':
                        real_key = self.dico.gb2fr[key]
                        rubrique = 'RUBRIQUE'
                    else:
                        real_key = key
                        rubrique = 'RUBRIQUE1'

                    val = self.values[key]
                    # Printing rubrique information
                    string = ''
                    for irub in range(3):
                        # Empty rubrique displaying keyword
                        rub = self.dico.data[key][rubrique][irub]
                        if rub == '':
                            break
                        if rub != rubs[irub]:
                            irubs[irub] += 1
                            for i in range(irub+1, 3):
                                irubs[i] = 0
                            if irub == 0:
                                string += "/"*72+"\n"
                            string += \
                               "/// {num}-{rub}\n"\
                               .format(sep="/"*(72),
                                       num=".".join([str(i) for i in irubs[:(irub+1)]]),
                                       rub=rub.lower())
                            rubs[irub] = rub
                    f.write(string)
                    if isinstance(val, list):
                        s_val = [repr(item) for item in val]
                        string = "{} = {}\n".format(real_key, ";".join(s_val))
                        if len(string) < 73:
                            f.write(string)
                        else:
                            string = "{} = \n{}\n".format(real_key,
                                                          ";\n".join(s_val))
                            f.write(string)
                    else:
                        string = "{} = {}\n".format(real_key, repr(val))
                        if len(string) < 73:
                            f.write(string)
                        else:
                            string = "{} =\n{}\n".format(real_key,
                                                         format72(repr(val)))
                            f.write(string)

    def write_fr_gb(self, output_dir=''):
        """
        Write an english and french version of the steering file

        @param output_dir (string) If given translated steering files are
        written into it otherwise they are written next to the steering file
        """
        with open(self.file_name, 'r') as f:
            cas_lines = f.readlines()

        core = []
        for cas_line in cas_lines:
            # ~~> scan through to remove all comments
            cas_line = cas_line.replace('"""', "'''").replace('"', "'")
            proc = re.match(KEY_COMMENT, cas_line+'/')
            head = proc.group('before').strip()
            core.append(head)
        cas_stream = ' '.join(core)

        fr_lines = []
        gb_lines = []
        for cas_line in cas_lines:

            # ~~> split comments
            cas_line = cas_line.replace('"""', "'''").replace('"', "'")
            proc = re.match(KEY_COMMENT, cas_line)
            head = proc.group('before').strip()
            # /!\ is not translated
            tail = proc.group('after').strip()
            # ~~ special keys starting with '&'
            proc = re.match(KEY_NONE, head+' ')
            if proc:
                head = ''
                tail = cas_line.strip()
            frline = head
            gbline = head

            # ~~> this is a must for multiple keywords on one line
            cas_stream = cas_line
            while cas_stream != '':
                proc = re.match(KEY_EQUALS, cas_stream)
                if not proc:
                    raise TelemacException(\
                        'Unhandled error\n    '\
                        'around there :'+cas_stream[:100])
                keyword = proc.group('key').strip()
                if keyword not in head:
                    break  # move on to next line
                # If just a value skipping
                if '=' not in head and ':' not in head:
                    break # move on to next line
                # If only part of a string value skipping
                if (head.count("'") == 1 and ("L'" not in keyword and \
                                              "D'" not in keyword)) or \
                    head.count('"') == 1:
                    break

                # ~~> translate the keyword
                head = head.replace(keyword, '', 1)
                if keyword.upper() in self.dico.gb2fr:
                    frline = frline.replace(\
                            keyword,
                            self.dico.gb2fr[keyword],
                            1)
                if keyword.upper() in self.dico.fr2gb:
                    gbline = gbline.replace(\
                            keyword,
                            self.dico.fr2gb[keyword],
                            1)

                # ~~> look for less obvious keywords
                cas_stream = proc.group('after')    # still hold the separator
                proc = re.match(VAL_EQUALS, cas_stream)
                if not proc:
                    raise Exception('No value to keyword: '+keyword)
                while proc:
                    cas_stream = proc.group('after')
                    proc = re.match(VAL_EQUALS, cas_stream)

            # final append
            fr_lines.append(frline + tail)
            gb_lines.append(gbline + tail)

        # ~~ print FR and GB versions of the CAS file
        if output_dir != '':
            file_name = path.join(output_dir,
                                  path.basename(self.file_name))
        else:
            file_name = self.file_name
        base, ext = path.splitext(file_name)
        fr_name = base+"_fr"+ext
        gb_name = base+"_en"+ext
        with open(fr_name, 'w') as f:
            f.write('\n'.join(fr_lines))
        with open(gb_name, 'w') as f:
            f.write('\n'.join(gb_lines))

    def get(self, key, default=None):
        """
        Return value for a given keyword.
        key can be in any language.

        @param key (string) Name of the keyword (french or english)
        @param default (list/int/float/boolean/string) if not None, Value
        returned if key not in case
        """
        # Get english version of the keyword
        if key not in self.dico.fr2gb and key not in self.dico.gb2fr:
            if default is not None:
                return default
            raise TelemacException(\
                    "keyword: {} not in dictionary".format(key))
        # Getting english keyword
        gb_key = self.dico.fr2gb.get(key, key)

        # Not the same default in french and english
        if self.lang == 'fr':
            defaut_name = "DEFAUT"
        else:
            defaut_name = "DEFAUT1"

        key_data = self.dico.data[gb_key]

        # default value
        dico_default = key_data.get(defaut_name, None)

        # returning value from steergin if there is one default otherwise
        return self.values.get(gb_key, dico_default)

    def set(self, key, val, convert=False):
        """
        Set value of key in steering file

        @param key (string) Name of the keyword
        @param val (list/int/boolean/float) Value to set the keyword to
        @param convert (boolean) If true val in string and need to be converted
        """
        # Get english version of the keyword
        if key not in self.dico.fr2gb and key not in self.dico.gb2fr:
            raise TelemacException(\
                    "keyword: {} not in dictionary".format(key))
        gb_key = self.dico.fr2gb.get(key, key)

        # Checking value type
        key_data = self.dico.data[gb_key]
        var_type = key_data['TYPE']

        # Converting if ask for
        if convert:
            new_val = convert_to_type(var_type, val)
        else:
            new_val = val

        if not check_type(var_type, new_val):
            raise TelemacException(\
                    "Value to set on {} is not of type {} but of type {}"\
                    .format(key, var_type, type(new_val)))

        self.values[gb_key] = new_val

        # If the value changed is a file update in_files and out_files
        if 'SUBMIT' in key_data:
            if 'LIT' in key_data['SUBMIT']:
                self.in_files[gb_key] = key_data['SUBMIT']
                val = self.values[gb_key].strip("'")
                if not path.exists(val):
                    raise TelemacException(\
                        "In {} missing file for {}:\n {}"\
                        .format(self.file_name, key, val))
            # output file
            if 'ECR' in key_data['SUBMIT']:
                self.out_files[gb_key] = key_data['SUBMIT']

    def remove(self, key):
        """
        Remove a keyword from the steering file if it is in otherwise does
        nothing

        @param key (str) keyword to remove
        """
        if key in self.values:

            del self.values[key]

            if key in self.in_files:
                del self.in_files[key]

            if key in self.out_files:
                del self.out_files[key]

    def get_file_from_submit(self, submit):
        """
        Returns the value of the keyword associated to the temporary file name
        (for example T2DGEO, T3DHYD, T3DRES...)
        If the file asked for is a outfile file and a PARAL
        Returning list of values instead

        @param submit (str) Name in the temporary folder

        @returns (str) value of the key word
        """
        file_key = None
        # Search in in_files and out_files for the submit
        for key, sub in self.out_files.items():
            if submit in sub:
                file_key = key
                file_sub = sub
                break

        # Was not found in the out files looking in the input files
        if file_key is None:
            for key, sub in self.in_files.items():
                if submit in sub:
                    file_key = key
                    file_sub = sub
                    break

        if file_key is None:
            raise TelemacException(\
                    "Could not find {} in files for {}"\
                    .format(submit, self.file_name))

        # In the the file a PARAL (i.e it will have one for each processor)
        ncsize = self.get('PARALLEL PROCESSORS')
        if 'PARAL' in file_sub and file_key in self.out_files \
            and ncsize > 1:
            root, ext = path.splitext(self.get(file_key))
            file_names = []
            for ipid in range(ncsize):
                ffile = "{}{:05d}-{:05d}{}".format(root, ncsize-1, ipid, ext)
                file_names.append(ffile)
            return file_names

        return self.get(file_key)

    def copy_cas_files(self, dir_path, verbose=False, copy_cas_file=True):
        """
        Will copy all input files and the steering file into a directory

        @param dir_path (str) Directory in which to copy the files
        @param verbose (bool) If true makes a print for each copy
        @param copy_cas_file (boole) If True also copies the steering file
        """
        if not path.exists(dir_path):
            raise TelemacException(self, "Copy dir does not exists:\n"+dir_path)

        # Copying input_files
        for key in self.in_files:
            ffile = self.get(key)
            src = path.join(path.dirname(self.file_name), ffile)
            dest = path.join(dir_path, ffile)
            # TODO: Issue with a folder as it is always copied because the
            # check is going to be on the folder itself and not its content
            if is_newer(dest, src):
                if path.isdir(src):
                    if path.exists(dest):
                        shutil.rmtree(dest)
                    shutil.copytree(src, dest)
                else:
                    shutil.copy2(src, dest)
                if verbose:
                    print("Copying {} -> {}".format(ffile, dir_path))

        if copy_cas_file:
            # Copying steering file
            src = self.file_name
            dest = path.join(dir_path, path.basename(self.file_name))
            if is_newer(dest, src):
                shutil.copy2(src, dest)
                if verbose:
                    print("Copying {} -> {}".format(src, dir_path))

    def __str__(self):
        """ str function """
        string = "~~ " + self.file_name + "\n"
        string = "   Language: " + self.lang + "\n"
        for key in sorted(self.values.keys()):
            string += "{} = {}\n".format(key, self.values[key])
        string += "input files:\n"
        for key in self.in_files:
            string += "  " + self.values[key].strip("'") + "\n"
        string += "output files:\n"
        for key in self.out_files:
            string += "  " + self.values[key].strip("'") + "\n"

        return string
