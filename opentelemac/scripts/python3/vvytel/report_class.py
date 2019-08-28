r"""@author TELEMAC-MASCARET Consortium

    @note ... this work is based on a collaborative effort between
                 TELEMAC-MASCARET consortium members
    @brief Report class for validation
"""
from __future__ import print_function
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import time
from os import path, walk, remove
from copy import deepcopy
# ~~> dependencies towards other pytel/modules
from utils.files import move_file2file, put_file_content

# _____                        _____________________________________
# ____/ Primary Class: Report /____________________________________/
#

class Report(object):
    """
     @brief The principal object in Report is self.reports.
     It has the following structure:
        { "repname" :
          { "xmlfile1":{},"xmlfile2":{},... }
        }
     where each "xmlfile" dict has the following keys:
        [ '', '', '' ]
     and where the various "root" are roots to a particular branch of
     the system.

    """
    # Constant definition
    comment = '#'
    delimiter = ','
    # Arbitrary associations between report headers and
    # the return of the XML keys
    heads = [\
        'XML Name',
        'Author',
        'Contact',
        'Total Duration (s)',
        'Action Name',
        'XML Path',
        'Action Type',
        'Action Failed',
        'Action Warned',
        'Action Rank',
        'Action Updated',
        'Action Result',
        'Action Meta-Data']
    hkeys = [\
        'file',
        'author',
        'contact',
        'duration',
        'xref',
        'path',
        'type',
        'fail',
        'warn',
        'rank',
        'updt',
        'value',
        'title']


    def __init__(self, repname):
        """
          @brief Initialisation
          @param  repname (string) a user defined name, which will distinguish
                         one report from another
        """
        self.reports = {}
        # > file_fields makes up the name of the report file,
        # stored at the root of the system
        self.file_fields = ['[version]',
                            repname,
                            time.strftime("%Y-%m-%d-%Hh%Mmin%Ss",
                                          time.localtime(time.time()))]


    def add(self, repname, root, version):
        """
          @brief Adds a new report to the list of reports.
          @param repname (string) report user defined middle name, which
                                  will distinguish one report from another
          @param root (string) the root of a particular system branch
          @param version (string) end name, could be svn version number
        """
        if repname == '':
            return {}

        # ~~> Current file name
        self.file_fields[0] = version
        self.file_fields[1] = repname
        file_name = path.join(root, '_'.join(self.file_fields) + '.csv')
        if not repname in self.reports:
            self.reports.update({repname: {'head':[], 'core':{}, 'file':[]}})
        if file_name not in self.reports[repname]['file']:
            self.reports[repname]['file'].append(file_name)
        # ~~> Possible existing file name
        dirpath, _, files = next(walk(root))
        for fle in files:
            l_name, l_ext = path.splitext(fle)
            if l_ext != '.csv':
                continue
            l_name = l_name.split('_')
            if len(l_name) < 3:
                continue
            if l_name[0] == version and repname in l_name[1]:
                if path.join(dirpath, fle) != file_name:
                    print('      +> Moving existing: ' + l_name[2] + ' to ' + \
                            path.basename(file_name))
                    try:
                        self.reports[repname].update(\
                            {'head':self.load_head(path.join(dirpath, fle))})
                        self.reports[repname].update(\
                            {'core':self.load_core(path.join(dirpath, fle))})
                        move_file2file(path.join(dirpath, fle), file_name)
                    except Exception:
                        print('      ... I could not make sense of your '\
                              'previous report: ' + fle + ' so I deleted it ')
                        remove(path.join(dirpath, fle))

        return self.reports[repname]['core']

    def load_head(self, file_name):
        """
          @brief Loads header of a report.
          @param file_name (string) report file name
          @returns current header
        """
        l_list = []
        fle = open(file_name, 'r')
        for line in fle:
            if line[0] == self.comment:
                l_list.append(line.strip())
            else:
                break
        fle.close()
        return l_list

    def load_core(self, file_name):
        """
          @brief Loads core of a report.
          @param file_name (string) report file name
          @returns current core
        """
        l_casesl = {}
        headrow = []
        corerow = ''
        # ~~> Opening
        fle = open(file_name, 'r')
        # ~~> Parsing head row
        for line in fle:
            if line[0] == self.comment:
                continue
            if headrow == []:
                headrow = line.split(self.delimiter)
            else:
                corerow = line.replace('"', '').split(self.delimiter)
                case_name = corerow[self.hkeys.index('file')]
                if case_name not in l_casesl:
                    # name of the xml file
                    l_casesl.update({case_name: {'casts': []}})
                l_casesl[case_name]['file'] = \
                        path.join(corerow[self.hkeys.index('path')], case_name)
                l_casesl[case_name]['duration'] = \
                        float(corerow[self.hkeys.index('duration')])
                cast = {}
                cast.update({'type': corerow[self.hkeys.index('type')]})
                if corerow[self.hkeys.index('fail')] != '':
                    cast.update(\
                    {'fail':
                     ('true' in corerow[self.hkeys.index('fail')].lower())})
                if corerow[self.hkeys.index('warn')] != '':
                    cast.update(\
                    {'warn':
                     ('true' in corerow[self.hkeys.index('warn')].lower())})
                if corerow[self.hkeys.index('updt')] != '':
                    cast.update(\
                    {'updt':
                     ('true' in corerow[self.hkeys.index('updt')].lower())})
                cast.update({'value': corerow[self.hkeys.index('value')]})
                cast.update({'rank': corerow[self.hkeys.index('rank')]})
                cast.update({'title': corerow[self.hkeys.index('title')]})
                cast.update({'xref': corerow[self.hkeys.index('xref')]})
                l_casesl[case_name]['casts'].append(cast)
        # ~~> closure
        fle.close()
        return l_casesl

# /!\ TODO:(JCP) Update with CSV call functionalities (?)
    def dump(self):
        """
          @brief Dump validation reports in CSV.
        """
        if self.reports == {}:
            return
        contents = {}
        # ~~> Copy the default header in all files
        for rep_name in self.reports:
            for file_name in self.reports[rep_name]['file']:
                if file_name not in contents:
                    content = deepcopy(self.reports[rep_name]['head'])
                    content.append(self.delimiter.join(self.heads))
                    contents.update({file_name: content})
        # ~~> Line template for those absenties
        emptyline = []
        for _ in self.heads:
            emptyline.append('')
        # ~~> Copy the core passed/failed vallues
        # repname  (could be "Validation-Summary")
        for rep_name in sorted(self.reports):
            # filename (root dependant)
            for file_name in sorted(self.reports[rep_name]['file']):
                # case name (could bumpflu.xml)
                for case_name in sorted(self.reports[rep_name]['core']):
                    for cast in \
                        self.reports[rep_name]['core'][case_name]['casts']:
                        line = deepcopy(emptyline)
                        for key in cast:
                            if key == "graph_title":
                                line[self.hkeys.index("title")] = \
                                        '"' + str(cast[key]) + '"'
                            else:
                                line[self.hkeys.index(key)] = \
                                        '"' + str(cast[key]) + '"'
                        report = self.reports[rep_name]['core'][case_name]
                        line[self.hkeys.index('file')] = case_name
                        line[self.hkeys.index('path')] = \
                                path.dirname(report['file'])
                        line[self.hkeys.index('duration')] = \
                                str(report['duration'])
                        contents[file_name].append(self.delimiter.join(line))
        for file_name in contents:
            put_file_content(file_name, contents[file_name])


