#!/usr/bin/env python
# _____           __________________________________________________
# ____/ Python 3 /_________________________________________________/
#
# necessrary for un-ended prints
from __future__ import print_function

"""@author Sebastien E. Bourban, Noemie Durand and Alain Weisgerber
"""
"""@note ... this work is based on a collaborative effort between
  .________.                                                          ,--.
  |        |                                                      .  (  (
  |,-.    /   HR Wallingford                EDF - LNHE           / \_ \_/ .--.
  /   \  /    Howbery Park,                 6, quai Watier       \   )   /_   )
   ,.  `'     Wallingford, Oxfordshire      78401 Cedex           `-'_  __ `--
  /  \   /    OX10 8BA, United Kingdom      Chatou, France        __/ \ \ `.
 /    `-'|    www.hrwallingford.com         innovation.edf.com   |    )  )  )
!________!                                                        `--'   `--
"""
"""@history 10/03/2011 -- Chris Cawthorn
      Amended to enable listing file in addition to output to stdout.
      Use of multi-threads.
"""
"""@history 04/04/2011 -- Sebastien E. Bourban
         Correction for POSTEL3D Use of key 'MULTI' for output file
         recollection.
"""
"""@history 05/04/2011 -- Sebastien E. Bourban
         Correction, adding an empty line at the end of all ASCII files
         (bug reported with CONFIG).
"""
"""@history 05/04/2011 -- Sebastien E. Bourban
         Amended to support reccursively coupled CAS Files, using
         "COUPLAGE AVEC".
"""
"""@history 28/04/2011 -- Sebastien E. Bourban
         Now supports SYSTELCFG as a directory (old Perl version, to which
         systel.cfg is added) or as a file.
"""
"""@history 30/04/2011 -- Sebastien E. Bourban
         Upgrade made to config parsing to include the option to reset
         the version and the root from the command line option:
         -v <version>, reset the version read in the config file with this
         -r <root>, reset the root path read in the config file with this
"""
"""@history 05/07/2011 -- Sebastien E. Bourban
         Python interpreter added for linux calls. This is a temporary
         solution as "/usr/bin/env" is not strickly portable cross
         operating systems
"""
"""@history 10/10/2011 -- Jan-Philip Gehrcke
         Correction made to the management of sortie files. (search JPG)
"""
"""@history 10/02/2012 -- Sebastien E. Bourban
         Addition of the fixed directory option, which is particulalry useful
         for parallel simulations.
"""
"""@history 20/02/2012 -- Sebastien E. Bourban
         Allowing PARTEL to run in parallel, having received the PARTEL
         source code from Charles (STFC-DL).
"""
"""@history 28/02/2012 -- Sebastien E. Bourban
         Allowing the python version of PARTEL_PARA to run in parallel, finding
         that PARTEL in PARALLEL did not solve our partitioning problem.
"""
"""@history 07/03/2012 -- Sebastien E. Bourban
         Allowing a launch of the main executable / script to run on an HPC queue
         Example given with BSUB to run on encore.ocf.co.uk
"""
"""@history 04/04/2012 -- Sebastien E. Bourban
         Three new options are now available toruncode:
         --split: only does the copying of files (and the split when in parallel)
         -x  : only does the compilation of the executable
         --run  : only does the running (by copying the CAS and the PRINCI
            again, for good measure)
         --merge: only does the re-collection and copy back of files,
            which is most useful when the simulation is put on an HPC queue.
"""
"""@history 12/04/2012 -- Sebastien E. Bourban
         Removed the dependency of the compilation of the PRINCI at run time
         from the configuration files (update or clean statements).
"""
"""@history 12/05/2012 -- Fabien Decung & Sebastien E. Bourban
         Modified checkConsistency so the behaviour is as follows :
          - if parallel not in module keys => ncsize should be 0 or stop
          - else ncsize should be 1 or more
         Also, forces the re-writing of the CAS file in the temporary directory
         so keywords can now be modified before running the CAS
"""
"""@history 18/06/2012 -- Sebastien E. Bourban & Fabien Decung
         Calls to sys.exit() and os.system() have been progressively captured
         into a try/except statement to better manage errors.
         This, however, assumes that all errors are anticipated.
"""
"""@history 29/08/2012 -- Sebastien E. Bourban
         Additonal option --nctile setting the number of cores per node.
         In the case of HPC use, the variable <ncsize> is replaced by ncsize,
            and now two other variables are available: <nctile> and <ncnode>.
         ncsize must be ncnode x nctile.
"""
"""@history 04/12/2012 -- Juliette Parisi and Sebastien E. Bourban
   Simplifying call to parseConfigFile, which now takes two arguments
      options.configFile, and options.configName and return one or more
      valid configurations in an array. Testing for validity is now done
      within config.py
"""
"""@history 15/04/2014 -- Sebastien E. Bourban
   FORTRAN file can now be refered to as directories, in which case, all files
      within it will be bundled together as one FORTRAn file.
"""
"""@history 23/09/2014 -- Sebastien E. Bourban and Yoann Audoin
   The content of the log files from GRETEL and PARTEL are now reported
   in the error report.
"""
"""@history 25/12/2014 -- Sebastien E. Bourban
   'version' is not mandatroy anymore.
   It has been removed from having to be in the configuration file.
"""
"""@history 15/09/2016 -- Sebastien E. Bourban
   Allowing for Fortran directive to be present in the user file (extension .F)
"""
"""@history 22/09/2016 -- Christophe Coulet (Artelia)
   Adding the management of parallel treatment of Weirs file according to Type
   Type = 1 (Original version) - duplicate
   Type = 2 (RIG version) - argument passed to partel
"""
"""@history 03/05/2018 -- Judicael Grasset (Daresbury Lab & EDF)
    Add code for concatenation of partel output
"""
"""@brief
         runcode is the execution launcher for all TELEMAC modules
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import re
import sys
import shutil
import threading
from argparse import ArgumentParser,RawDescriptionHelpFormatter
from time import localtime, strftime
from subprocess import *
from os import path,walk,mkdir,chdir,remove,sep,environ,listdir,getcwd
# ~~> dependencies towards other modules
from config import parseConfigFile,parseConfig_RunningTELEMAC
# ~~> dependencies towards other pytel/modules
from utils.files import checkSymLink,symlinkFile,getFileContent,putFileContent,removeDirectories,createDirectories,isNewer,zipsortie,copyFiles,copyFile
from utils.messages import MESSAGES,filterMessage,banner
from parsers.parserKeywords import scanCAS,readCAS,scanDICO, getCASLang,getKeyWord,setKeyValue,getIOFilesSubmit
from parsers.parserSortie import getLatestSortieFiles
from mascaret import main as main_mascaret

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

# _____                  ___________________________________________
# ____/ General Toolbox /__________________________________________/
#

def checkParaTilling(onctile,oncnode,oncsize,ncruns,ncsize):
   """
   @brief
      Check the consistency between number of core / processors and domains.
      The logic is as follows:
    > First, nctile is the one parameter we cannot modify, unless
         ncnode and ncsize are provided.
    > If ncruns > 1, then ncsize and nctile will not be adjusted,
      but:
       - if ncnode is given by the user, there will be no
         adjustment, even if the resource allocated ncnode * nctile
         might be too much or too few.
       - if ncnode is not provided, the ncnode will be adjusted to
         best fit ncsize * ncruns with a constant nctile.
    > If ncruns = 1, then normal adjustment will be done,
      with:
       - if ncnode is given by the user then ...
          + if ncsize is given by the user, there will be a
            re-adjustment of nctile to accomodate
          + if nctile is given by the user, there will be a
            re-adjustment of ncsize to accomodate

   @param onctile: is the number of core utilised per node
   @param oncnode: is the number of pysical processors.
   @param oncsize:
   @param ncruns: allows a number of CAS files to be placed in the same
                  queue and run in parallel as a single batch.
   @param ncsize: is the value of set in the CAS file, i.e. the total
                  number of geometrical sub-domains.
   @note: ncsize = 0 is also supported.
   """
   # ~~ Default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ncnode = 1
   if oncnode != '': ncnode = max( 1,int(oncnode) )
   if oncsize != '': ncsize = int(oncsize)

   # ~~ Special case of nctile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   nctile = 1
   if onctile != '': nctile = max( 1,int(onctile) )
   elif ncnode > 1:
      if ncsize > 1: nctile = int( ncsize / ncnode )
      elif ncruns > 1: nctile = int( ncruns / ncnode )

   # ~~ Special case of batching ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if oncnode == '':
      # ~~> temporary measure before doing each run in parallel of one another
      ncnode = int( max( 1,ncsize ) / nctile )
      if ncnode * nctile < max( 1,ncsize ): ncnode = ncnode + 1
      # ~~> valid for runs in parallel of one another
      #ncnode = int( max( 1,ncsize ) * ncruns / nctile )
      #if ncnode * nctile < max( 1,ncsize ) * ncruns: ncnode = ncnode + 1

   if ncruns == 1:
   # ~~ Standard cases ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # If the command line options.nctile and options.ncnode are fixed
      if onctile != '' and oncnode != '':
         ncsize = ncnode * nctile
   # If options.ncsize is set, it will have priority over the others
      elif oncsize != '':
         # ncnode is an integer of nctile and ncsize is re-ajusted
         if onctile != '':
            ncnode = int( max( 1,ncsize ) / nctile )
            while ncnode * nctile < max( 1,ncsize ): ncnode = ncnode + 1
         # nctile is an integer of ncnode and ncsize is re-ajusted
         if oncnode != '':
            nctile = int( max( 1,ncsize ) / ncnode )
            while ncnode * nctile < max( 1,ncsize ): nctile = nctile + 1
         # local processor with 1 node and many cores
         if onctile == '' and oncnode == '':
            ncnode = 1
            nctile = max( 1,ncsize )

   return nctile,ncnode,ncsize

def getGretelCmd(pbin,cfg):
   """
   @brief Returns the command to execute GRETEL and its arguments

   @param pbin: Path to to partel executable from root
   @param cfg:  Configuration file information

   @return execmd: the text string that will be executed on the system
   """
   # ~~> Temporary variable to keep pbin unchanged
   PARDir = pbin
   if cfg['PARTEL'] != {}:
      if 'PATH' in cfg['PARTEL']:
         PARDir = cfg['PARTEL']['PATH'].replace('<root>',cfg['root']).replace('<config>',pbin)
   # ~~> GRETEL Executable
   execmd = path.join(PARDir,'gretel'+cfg['SYSTEM']['sfx_exe'])

   return execmd


def getPartelCmd(pbin,cfg,CASFile):
   """
   @brief Returns the command to execute PARTEL and its arguments

   @param pbin Path to to partel executable from root
   @param cfg Configuration file information
   @param CASFile Steering file information

   @return execmd: the text string that will be executed on the system
   """
   # ~~> Temporary variable to keep pbin unchanged
   PARDir = pbin
   if cfg['PARTEL'] != {}:
       if 'PATH' in cfg['PARTEL']:
         PARDir = cfg['PARTEL']['PATH'].replace('<root>',cfg['root']).replace('<config>',pbin)
   # ~~> Default call to PARTEL
   execmd = path.join(pbin+sep+'partel'+cfg['SYSTEM']['sfx_exe']+' < PARTEL.PAR >> <partel.log>')
   # ~~> User defined call to PARTEL
   if cfg['PARTEL'] != {}:
      if 'EXEC' in cfg['PARTEL']:
         execmd = cfg['PARTEL']['EXEC']
   # ~~> Replacement of known keys
   # <mpi_cmdexec> and <exename> should be known by now
   if cfg['MPI'] != {}:
      execmd = execmd.replace('<mpi_cmdexec>',CASFile['mpi']).replace('<exename>','')
   # <root> and <config> are part of the arguments
   execmd = execmd.replace('<root>',cfg['root']).replace('<config>',PARDir)

   return execmd


def processCAS(casFiles,dico,frgb):

   # ~~ Aquire CAS Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cases = []; langs = []
   for casFile in casFiles:

      #/!\ to do: possible use of os.path.relpath() and comparison with os.getcwd()
      casFile = path.realpath(casFile)
      if not path.exists(casFile):
         raise Exception([{'name':'runCAS','msg':'inexistent CAS file: '+casFile+ \
            '\n    +> or you may have forgotten an option key in your command line'}])

      # ~~> extract keywords
      cas = readCAS(scanCAS(getFileContent(casFile)),dico,frgb)

      # ~~> extract language and set extra keywords
      lang = getCASLang(cas,frgb)
      if lang == 1:
         cas = setKeyValue('FICHIER DES PARAMETRES',cas,frgb,repr(path.basename(casFile)))
         cas = setKeyValue('DICTIONNAIRE',cas,frgb,repr(path.normpath(frgb['DICO'])))
      if lang == 2:
         cas = setKeyValue('STEERING FILE',cas,frgb,repr(path.basename(casFile)))
         cas = setKeyValue('DICTIONARY',cas,frgb,repr(path.normpath(frgb['DICO'])))

      # ~~> Store the CAS File
      cases.append( cas ); langs.append( lang )

   # ~~ Batching commonalities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   lang = langs[0]
   ncsize = getNCSIZE(cases[0],dico,frgb)
   for cf,c,l in zip(casFiles,cases,langs):
      if ncsize != getNCSIZE(c,dico,frgb):
         raise Exception([{'name':'processCAS','msg':'batched CAS files should have same NCSIZE '+str(ncsize)+' != '+path.basename(cf)}])
      if lang != l:
         raise Exception([{'name':'processCAS','msg':'batched CAS files should have same LANGUAGE '+str(lang)+' != '+path.basename(cf)}])

   # ~~ For Information ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if lang == 1: print( '    +> simulation en Francais' )
   if lang == 2: print( '    +> running in English' )

   return cases,lang,ncsize


def processTMP(casFile):
   """
   @brief
      Format the current date and time into a string to make up the end part
         of the name of the temporary directory (when running a simulation)
   @param casFile: name of the CAS file, making up the front part of the
         name of the temporary directory
   @return TMPDir: The complete name of the temporary directory.
   """
   # ~~ TMP Directory ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   TMPDir = casFile + '_' + strftime("%Y-%m-%d-%Hh%Mmin%Ss", localtime())
   return TMPDir


def processLIT(cas,iFiles,TMPDir,ncsize,update,dico,frgb,use_link):

   # ~~> exception report
   #  xcpt will accumulate all input troubles before reporting a more
   #    comprehensive list of possible errors
   xcpt = []
   #
   section_name = ' '
   zone_name    = ' '
   weir_name    = ' '
   #
   # ~~> loop over all (key,value) pairs of the CAS file
   #   and process those that are related to input file names
   #
   #   /!\ the FORTRAN file and its associated exe file are not included here
   #
   for k,v in zip(*cas[1]):
      if k in iFiles:
         if iFiles[k].split(';')[5][0:7] == 'FORTRAN': continue
         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         # ~~> checking represence of input files
         cref = v[0].strip("'\"")
         #  cref: is a file name, read as the value of a CAS keyword k
         #   > if the requested input does not exist, append to the exceptions and
         #    carry on checking the other inputs.
         if not path.isfile(cref):
            xcpt.append({'name':'processLIT','msg':'input file does not exist ( '+path.basename(cref)+' ) for key '+k})
            continue
         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         # ~~> checking validity of default files
         crun = path.join(TMPDir,iFiles[k].split(';')[1])
         #  crun: is the default input name as set in the temporary directory
         #   > if crun does not exists then cref is copied as crun again
         #   > if cref is newer than crun then cref is copied as crun again
         if path.exists(crun) and not update:
            if not isNewer(crun,cref) == 1:
               # ~~> further check are necessary depending on file type
               if iFiles[k].split(';')[5][0:7] == 'SELAFIN' or iFiles[k].split(';')[5][0:5] == 'PARAL':
                  # ~~> check if all files are there
                  #   > while cref is one file, crun could have been split already
                  #    into multiple parallel files
                  found = True
                  for npsize in range(ncsize):
                     pll = crun+'{0:05d}-{1:05d}'.format(ncsize-1,npsize)
                     if not path.isfile(pll): found = False
                     elif not isNewer(pll,cref) == 1: found = False
                  if found:
                     #   > no partioning is required and no re-copying either
                     iFiles[k] = iFiles[k].replace('SELAFIN','DONE').replace('PARAL','DONE')
                     continue
               elif iFiles[k].split(';')[5][0:3] == 'CAS':
                  #   > force the copying of the CAS file for some reason
                  print( '    re-copying: '+ crun )
                  putFileContent(crun,cas[0])
                  # /!\ this may not be compatible with the 72-character rule of DAMOCLES
                  #newcas = [line[1:] if line[0] == ' ' else line for line in cas[0]]
                  #putFileContent(crun,newcas)
                  continue
               else:
                  #   > you have passed all checks - you can ignore that file
                  print( '      ignoring: '+ path.basename(cref)+' '+crun )
                  continue
         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         # ~~> files are otherwise copied (or linked)
         if use_link:
            print( '       linking: '+ path.basename(cref)+' '+crun )
            symlinkFile(path.join(getcwd(),cref), crun)
         else:
            print( '       copying: '+ path.basename(cref)+' '+crun )
            shutil.copyfile(path.join(getcwd(),cref), crun)
         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         # ~~> last special treatment for some of the ASCII files
         if iFiles[k].split(';')[3] == 'ASC':
            #   > extra requirement for PARTEL
            if iFiles[k].split(';')[5][0:7] == 'SECTION':
               section_name = path.basename(crun)
            if iFiles[k].split(';')[5][0:5] == 'ZONES':
               zone_name = path.basename(crun)
            if iFiles[k].split(';')[5][0:5] == 'WEIRS':
               value,defaut = getKeyWord('TYPE DES SEUILS',cas,dico,frgb)
               if value != []: type_s = value[0]
               else: type_s = defaut[0]
               if type_s == 2: weir_name = path.basename(crun)
               # /!\ will be copied regardeless ?
               # else: iFiles[k] = iFiles[k].replace('WEIRS','PARAL')

   if xcpt != []: raise Exception(xcpt) # raise full report
   return section_name,zone_name,weir_name

def processECR(cas,oFiles,CASDir,TMPDir,sortiefile,ncsize,bypass):

   xcpt = []                            # try all files for full report
   # ~~ copy output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k,v in zip(*cas[1]):
      if  k in oFiles:
         if oFiles[k].split(';')[5] == 'MULTI':   # POSTEL3D
            npsize = 1
            while 1:                              # HORIZONTAL SECTION FILES
               cref = path.join(CASDir,v[0].strip("'\"")+'_{0:03d}'.format(npsize))
               if path.isfile(cref):
                  bs,es = path.splitext(cref)
                  i = 0
                  while 1:   # this would be an infinite loop only if you have an inifite number of files
                     i = i + 1
                     if not path.isfile(bs+'_old'+str(i)+es): break
                  shutil.move(cref,bs+'_old'+str(i)+es)
               crun = oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)
               if not path.isfile(crun): break
               shutil.move(crun,cref) #shutil.copy2(crun,cref)
               print( '      moving: '+ path.basename(cref) )
               npsize = npsize + 1
            npsize = 1
            while 1:                              # VERTICAL SECTION FILES
               nptime = 1
               if not path.isfile(oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime)): break
               while 1:
                  cref = path.join(CASDir,v[0].strip("'\"")+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime))
                  if path.isfile(cref):
                     bs,es = path.splitext(cref)
                     i = 0
                     while 1:   # this would be an infinite loop only if you have an inifite number of files
                        i = i + 1
                        if not path.isfile(bs+'_old'+str(i)+es): break
                     shutil.move(cref,bs+'_old'+str(i)+es)
                  crun = oFiles[k].split(';')[1]+'_{0:03d}'.format(npsize)+'-{0:03d}'.format(nptime)
                  if not path.isfile(crun): break
                  shutil.move(crun,cref) #shutil.copy2(crun,cref)
                  print( '      moving: '+ path.basename(cref) )
                  nptime = nptime + 1
               npsize = npsize + 1
         elif oFiles[k].split(';')[5] == 'PARAL' and ncsize > 1: # MAIN MODULE
            npsize = 0
            cb,ce = path.splitext(v[0].strip("'\""))
            while 1:
               cref = path.join(CASDir,cb+'{0:05d}-{1:05d}'.format(ncsize-1,npsize)+ce)
               if path.isfile(cref):
                  bs,es = path.splitext(cref)
                  i = 0
                  while 1:   # this would be an infinite loop only if you have an inifite number of files
                     i = i + 1
                     if not path.isfile(bs+'_old'+str(i)+es): break
                  shutil.move(cref,bs+'_old'+str(i)+es)
               crun = oFiles[k].split(';')[1]+'{0:05d}-{1:05d}'.format(ncsize-1,npsize)
               if not path.isfile(crun): break
               shutil.move(crun,cref) #shutil.copy2(crun,cref)
               print( '      moving: '+ path.basename(cref) )
               npsize = npsize + 1
         elif oFiles[k].split(';')[5] == 'MULTI2':
            for crun in listdir('.') :
               if crun.count(oFiles[k].split(';')[1]) == 1:
                  cref = path.join(CASDir,crun.lower().replace(oFiles[k].split(';')[1].lower(),
                         v[0].strip("'\"").split('.')[0])) + '.' + v[0].strip("'\"").split('.')[1]
                  if path.isfile(cref):
                     bs,es = path.splitext(cref)
                     i = 0
                     while 1:   # this would be an infinite loop only if you have an inifite number of files
                        i = i + 1
                        if not path.isfile(bs+'_old'+str(i)+es): break
                     shutil.move(cref,bs+'_old'+str(i)+es)
                  shutil.move(crun,cref)
                  print( '      moving: '+ path.basename(cref) )
         else:
            cref = path.join(CASDir,v[0].strip("'\""))
            if path.isfile(cref):
               bs,es = path.splitext(cref)
               i = 0
               while 1:   # this would be an infinite loop only if you have an inifite number of files
                  i = i + 1
                  if not path.isfile(bs+'_old'+str(i)+es): break
               shutil.move(cref,bs+'_old'+str(i)+es)
            crun = oFiles[k].split(';')[1]
            if not path.isfile(crun):
               xcpt.append({'name':'processECR','msg':'did not create outfile: '+path.basename(cref)+' ('+crun+')'})
               continue
            shutil.move(crun,cref)
            print( '      moving: '+ path.basename(cref) )

   # ~~~ copy the sortie file(s) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   sortiefiles = []
   if sortiefile != None:    # sortiefile.rstrip() != '':
      crun = path.basename(sortiefile)
      cref = path.join(CASDir,sortiefile)
      if not path.isfile(crun):
         xcpt.append({'name':'processECR','msg':'did not create listing file: '+path.basename(cref)+' ('+crun+')'})
         raise Exception(xcpt) # raise full report
      shutil.copy(crun,cref)
      print( '     copying: '+ path.basename(cref) )
      sortiefiles.append(cref)

      # ~~~> If in parallel, also copy the slave log files called PEnnnnn_xxxxx.log
      #   for slave x of n but for the last one called the sortie file
      if ncsize > 1:
         for i in range(ncsize-1):
            slavefile = 'PE{0:05d}-{1:05d}.LOG'.format(ncsize-1,i+1)
            bs,es = path.splitext(sortiefile) # (path.basename(sortiefile))
            slogfile  = bs+'_p'+'{0:05d}'.format(i+1)+es
            crun = slavefile
            cref = path.join(CASDir,slogfile)
            if not path.isfile(crun):
               xcpt.append({'name':'processECR','msg':'could not find the listing file: '+crun})
               raise Exception(xcpt) # raise full report
            shutil.copy(crun,cref)
            print( '     copying: '+ path.basename(cref) )
            sortiefiles.append(cref)
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpt != []: raise Exception(xcpt) # raise full report
   return sortiefiles

def processARTNIM(cas,CASDir,dico,frgb):

   from runSELAFIN import transfSELAFIN
   xcpt = []                            # try all files for full report
   # ~~> Output to the prost process
   value,defaut = getKeyWord('FICHIER DE SURFACE LIBRE',cas,dico,frgb)
   if value == []: return ''
   print( '    +> '+value[0].replace("'","") )

   fileNameWfs = path.join(CASDir,value[0].replace("'",""))
   if path.isfile(fileNameWfs):
      bs,es = path.splitext(fileNameWfs)
      i = 0
      while 1:   # this would be an infinite loop only if you have an inifite number of files
         i = i + 1
         if not path.isfile(bs+'_old'+str(i)+es): break
      shutil.move(fileNameWfs,bs+'_old'+str(i)+es)

   # ~~> Input to the prost process
   value,defaut = getKeyWord('FICHIER DES PHASES ET AMPLITUDES',cas,dico,frgb)
   if value != []: fileNameAmp = path.join(CASDir,value[0].replace("'",""))
   if not path.isfile(fileNameAmp):
      xcpt.append({'name':'processARTNIM','msg':'could not find the file of amplitudes and phases.'})
      raise Exception(xcpt) # raise full report

   # ~~> Parameters
   tfrom = 2006.07
   value,defaut = getKeyWord('PREMIER TEMPS DANS LE FICHIER DE SURFACE LIBRE',cas,dico,frgb)
   if value != []: tfrom = float(value[0])
   tstep = 0.34
   value,defaut = getKeyWord('PAS DE TEMPS',cas,dico,frgb)
   if value != []: tstep = float(value[0])
   tstop = 2108.75
   value,defaut = getKeyWord('NOMBRE DE PAS DE TEMPS',cas,dico,frgb)
   if value != []: tstop = tfrom + int(value[0])*tstep

   # ~~> Processing the new file
   slf = transfSELAFIN( fileNameAmp, times = (tfrom,tstep,tstop) )
   slf.calcFreeSurfaceFromARTEMIS()
   slf.putContent(fileNameWfs)

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #if xcpt != []: raise Exception(xcpt) # raise full report
   return fileNameWfs

def processCONFIG(lang):

   # ~~ create CONFIG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('CONFIG',[repr(lang),'6',''])
   return True

def getNCSIZE(cas,dico,frgb):

   # ~~ check keyword ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   value,defaut = getKeyWord('PROCESSEURS PARALLELES',cas,dico,frgb)
   ncsize = 0
   if value != []: ncsize = value[0]
   elif defaut != []: ncsize = int(defaut[0])

   return ncsize

def getMPICommand(cfgMPI):
   # ~~> Executable
   mpiCmd = cfgMPI['EXEC']
   # ~~> host file
   hostfile = ''
   if 'HOSTFILE' in cfgMPI: hostfile = cfgMPI['HOSTFILE']
   mpiCmd = mpiCmd.replace('<hostfile>',hostfile)
   # ~~> stdin file
   infile = ''
   if 'INFILE' in cfgMPI: infile = cfgMPI['INFILE']
   mpiCmd = mpiCmd.replace('<mpi_infile>',infile)

   return mpiCmd

def getHPCCommand(cfgHPC):
   # ~~> Executable
   if 'EXCODE' in cfgHPC: hpcCmd = cfgHPC['EXCODE']
   elif 'PYCODE' in cfgHPC: hpcCmd = cfgHPC['PYCODE']
   # ~~> script
   if 'STDIN' in cfgHPC:
      hpc_stdin = cfgHPC['STDIN'][0]
      hpcCmd = hpcCmd.replace('<hpc_stdin>',hpc_stdin)

   return hpcCmd

def getHPCDepend(cfgHPC):
   # ~~> Executable
   if 'DEPEND' in cfgHPC: return cfgHPC['DEPEND']
   else: return ''

def processExecutable(cas,pbin,plib,pobj,system,dico,frgb,trace,bypass):
   """
   @brief Process the excecutable including:
      - checking the presence and the validity of the current executable
      - if necessary, copying the FORTRAN files in the temporary directory
      - if necessary, compiling the FORTRAN into the executable
      The background of this function sits where the CAS file is

   @param cas:  Everything that touches the CAS file.
   @param pbin: Location of the default executables
   @param plib: location of the associated libs (and cmdx files)
   @param pobj: location of the associated objs (and cmdo files)
   @param update: whether to force the source copying / re-compilation
   @param dico:
   @param frgb:

   @return the name of the executable whether copied or compiled and a
      logical whether updated (True) or not (i.e. ignored)

   @note even in case of coupling, the principal executable remains
   @note possible fortran files may or may not be associated with the
      principal code and may be files or directories
   @note If the executable exist, and that the user fortran files have not
      changed and that the system has not been recompiled, then the executable
      remains valid
   @note The name of the executable is taken to be based on the name defined
      by the user, i.e. the name of the PRINCI whether it is a file or a
      directory.
   @note
      > cas['wir'] is where the local executable will be (the temp directory)
      > cas['wrt'] is whether to force the update or not
      > cas['dir'] is where the CAS file is from
   @note
      - exeFile: The name of the default executable as well as the system
        preference for that file extension
      - oriFile: The user define name of the executable, based on the name of
        the FORTRAN FILE whether a file or a directory.
      - useName,objName,f90Name,objCmd,exeCmd
   """
   # ~~ exception error, if any
   mes = MESSAGES(size=10)
   # ~~ saving current location
   curdir = getcwd()
   # ~~ principal code name
   codeName = cas['code']
   # ~~ default executable (no user defined fortran file(s)
   exeFile = path.join(pbin,codeName+system['sfx_exe'])
   if not path.exists(exeFile): raise Exception([{'name':'processExecutable','msg': \
         '\nNot able to find your default executable: ' + exeFile + '\n' + \
         '\n ... you have to compile this module at least: '+codeName}])

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> list possible user fortran files, including coupling
   #
   oriFort = []
   value,defaut = getKeyWord('FICHIER FORTRAN',cas['cas'],dico,frgb)
   if value != []:
      oriFile = path.join(curdir,value[0].strip("'\""))
      if path.exists(oriFile):
         if path.isfile(oriFile): oriFort.append( oriFile )
         else:
            for f in listdir(oriFile):
               if path.isfile(path.join(oriFile,f)):
                  if f[0] == '.' or f[-1] == '~':
                      continue
                  oriFort.append( path.join(oriFile,f) )
      else: raise Exception([{'name':'processExecutable','msg': \
            '\nYou named a user fortran source in your CAS file but I cannot find it: ' + oriFile + '\n' + \
            '\n ... you may wish to remove it.'}])
   for cplage in cas['with']:
      vplage,defaut = getKeyWord('FICHIER FORTRAN',cas['with'][cplage]['cas'],dico,frgb)
      if vplage != []:
         oriFile = path.join(curdir,vplage[0].strip("'\""))
         if path.exists(oriFile):
            if path.isfile(oriFile): oriFort.append( oriFile )
            else:
               for f in listdir(oriFile):
                  if path.isfile(path.join(oriFile,f)):
                     if f[0] == '.' or f[-1] == '~':
                         continue
                     oriFort.append( path.join(oriFile,f) )

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> check relevance of the executable (assumed True to start with)
   #
   relevant = True
   #
   # ~~ case of user fortran
   if oriFort != []:
      # wirFort working sub-directory, locally contain all user fortran files
      wirFort = path.join(cas['wir'],'user_fortran')
      # useFile is the name of the local executable regardeless
      useFile = 'out_'+path.basename(path.splitext(oriFile)[0])+system['sfx_exe']
      exeFort = path.join(cas['wir'],useFile)
      # ~~ check if compiltion is required
      if path.exists(exeFort):
         # ~~ is the default executable exeFile newer than the local executable ?
         if isNewer(exeFort,exeFile) == 1: relevant = False
         # ~~ is(are) the source file(s) newer than the local executable ?
         for oriFile in oriFort:
            if isNewer(exeFort,oriFile) == 1: relevant = False
         # ~~ is(are) the local source file(s) newer than the source file(s) ?
         for oriFile in oriFort:
            if isNewer(path.join(wirFort,path.basename(oriFile)),oriFile) == 1: relevant = False
         # ~~ is(are) the original source file(s) accounted for amongst the local source file(s) ?
         for oriFile in oriFort:
            found  = False
            for wirFile in listdir(wirFort):
               if path.basename(oriFile) == path.basename(wirFile): found = True
            if not found: relevant = False
         # ~~ is(are) the local source file(s) still relevant compared to the original source file(s) ?
         for wirFile in listdir(wirFort):
            if path.splitext(wirFile)[1] != system['sfx_obj'] and path.splitext(wirFile)[1] != system['sfx_mod']:
               found  = False
               for oriFile in oriFort:
                  if path.basename(oriFile) == path.basename(wirFile): found = True
            if not found: relevant = False
      else: relevant = False
   #
   # ~~ without user fortran
   else:
   # ~~ default executable
      useFile = 'out_' + path.basename(exeFile)
      exeFort = path.join(cas['wir'],useFile)
      # ~~ check if compiltion is required
      if path.exists(exeFort):
         # ~~ is the default executable exeFile newer than the local executable ?
         if isNewer(exeFort,exeFile) == 1: relevant = False
      else: relevant = False

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> if relevant, no need to re-compile
   if relevant:
      print( '      ignoring: '+exeFort )
      return exeFort,False

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> otherwise, copy the default executable
   if oriFort == []:
      if path.exists(exeFort):
         remove(exeFort)
         print( '    re-copying: '+path.basename(exeFile)+' '+exeFort )
      else:
         print( '       copying: '+path.basename(exeFile)+' '+exeFort )
      shutil.copy2(exeFile,exeFort)

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> otherwise, remove user files
   else:
      print( '  > requires re-compilation' )
      # removing source files
      if path.exists(wirFort): removeDirectories(wirFort)
      # removing the executable file
      if path.exists(exeFort): remove(exeFort)
      if path.exists(path.basename(exeFile)): remove(path.basename(exeFile))

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> ... and copying fortran file(s)
      print( '      creating: '+ wirFort )
      createDirectories(wirFort)
      for oriFile in oriFort:
         print( '       copying: '+ path.basename(oriFile)+' '+wirFort )
         if path.isfile(oriFile): copyFile(oriFile,wirFort)
         else: copyFiles(oriFile,wirFort)

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ~~> ... and compiling fortran file(s)
      # ~~ default command line for compilation of obects
      cmdoFile = path.join(pobj,codeName+'.cmdo')
      if not path.exists(cmdoFile): raise Exception([{'name':'processExecutable','msg': \
         '\nNot able to find your OBJECT command line: ' + cmdoFile + '\n' + \
         '\n ... you have to compile this module at least: '+codeName}])
      objCmd = getFileContent(cmdoFile)[0]
      # ~~ make the keys portable (no full path)
      for k in trace: objCmd = objCmd.replace('['+k+']',path.normpath(trace[k]))
      # ~~ into local compilation
      chdir(wirFort)
      print( '  > compiling objs' )
      # ~~ compilation one file at a time
      objs = []
      # Reordonning user fortran to compile module first
      # Are considered modules files beginning the letter m
      user_files = []
      tmp = []
      for f90 in sorted(listdir(wirFort)):
         if path.isfile(f90):
            # /!\ TODO avoid using hard-coded explicit assumptions
            #          use getPrincipalWrapNames(f90)[0], or better, scan the local user tree
            if f90.lower()[0] == "m":
               user_files.append(f90)
            else:
               tmp.append(f90)
      user_files.extend(tmp)
      # Looping on ordered fortran files
      for f90 in user_files:
         print( '       compiling: '+f90, end='' )
         try:
            tail,code = mes.runCmd(objCmd.replace('<f95name>',f90),bypass)
         except Exception as e:
            raise Exception([filterMessage({'name':'processExecutable','msg':'something went wrong for no reason in executing:\n\n'+ \
               objCmd.replace('<f95name>',f90)+'\n\nPlease verify your compiler installation.'},e,bypass)])
         if code != 0: raise Exception([{'name':'processExecutable','msg':'could not compile your FORTRAN (runcode='+str(code)+').\n      '+tail}])
         print( ' ... completed' )
         objs.append(path.splitext(f90)[0]+system['sfx_obj'])
      # ~~ default command line for linkage into an executable
      cmdxFile = path.join(plib,codeName+'.cmdx')
      if not path.exists(cmdxFile): raise Exception([{'name':'processExecutable','msg': \
         '\nNot able to find your EXECUTE command line: ' + cmdxFile + '\n' + \
         '\n ... you have to compile this module at least: '+codeName}])
      exeCmd = getFileContent(cmdxFile)[0]
      # ~~ make the keys portable (no full path)
      for k in trace: exeCmd = exeCmd.replace('['+k+']',path.normpath(trace[k]))
      exeCmd = exeCmd.replace('<objs>',' '.join(objs)).replace('<exename>','"'+exeFort+'"')
      try:
         tail,code = mes.runCmd(exeCmd,bypass)
      except Exception as e:
         raise Exception([filterMessage({'name':'processExecutable','msg':'something went wrong for no reason. Please verify your external library installation.'},e,bypass)])
      if code != 0: raise Exception([{'name':'processExecutable','msg':'could not link your executable (runcode='+str(code)+').\n      '+tail}])
      print( '       created: '+path.basename(exeFort) )
      shutil.copy2(exeFort,curdir)

      # ~~ out of local compilation
      chdir(curdir)

   return exeFort,True

def getCONLIM(cas,iFiles):

   # ~~ look for CONLIM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CONLIM = ''
   for k in cas[1][0]:
      if k in iFiles:
         if iFiles[k].split(';')[5] == 'CONLIM': CONLIM = iFiles[k].split(';')[1]
   return CONLIM

def getGLOGEO(cas,iFiles):

   # ~~ look for GLOBAL GEO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   GLOGEO = ''
   FMTGEO = ''
   GLOBND = ''
   for k in cas[1][0]:
      if k in iFiles:
         if iFiles[k].split(';')[5][-4:] == 'GEOM':
            GLOGEO = iFiles[k].split(';')[1]
            FMTGEO = getFileFormat(cas,k)
         if iFiles[k].split(';')[5] == 'CONLIM':
            GLOBND = iFiles[k].split(';')[1]

   return GLOGEO,FMTGEO,GLOBND

def getFileFormat(cas,keyword):
   """
   Search in a cas object for the format key word
   associated with the keyword in argument
   """
   i = 0
   # Loop on all the keywords in the cas file
   for k in cas[1][0]:
      # The keyword we are searching for contains both the keyword 'keyword'
      # and the word FORMAT (same word in french and english)
      if keyword in k and ('FORMAT ' in k or ' FORMAT' in k):
         return cas[1][1][i][0].strip("'\"")
      else:
         i = i + 1
   # By default if there is no format keyword the file is SERAFIN
   return 'SERAFIN'

def runPartition(partel,geom,fmtgeom,conlim,ncsize,bypass,section_name,zone_name,weir_name,iPart,concat):

   if ncsize < 2: return True
   # ~~ split GEO, CONLIM, SECTIONS, ZONES and WEIR file ~~~~~~~~~~~~~~~~~
   print( '\n... partitioning base files (geo, conlim, sections, zones and weirs)' )
   try:
      runPARTEL(partel,geom,fmtgeom,conlim,ncsize,bypass,
                section_name,zone_name,weir_name,geom,fmtgeom,iPart,concat)
   except Exception as e:
      raise Exception([filterMessage({'name':'runPartition'},e,bypass)])

   return True

def copyPartition(partel,cas,geom,fmtgeom,conlim,iFiles,ncsize,bypass,section_name,zone_name,weir_name,use_link,iPart,concat):

   if ncsize < 2: return True
   # ~~ split input files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print( '\n... splitting / copying other input files' )
   for k in cas[1][0]:
      if k in iFiles:
         crun = iFiles[k].split(';')[1]
         if iFiles[k].split(';')[5][-4:] == 'GEOM': continue
         elif iFiles[k].split(';')[5][0:7] == 'SELAFIN':
            print( '  partitioning: '+ path.basename(crun) )
            try:
               # Get the real name of the file to identify the format
               # A better solution would be to find the definition of the format in the cas file
               fileFormat = getFileFormat(cas,k)
               runPARTEL(partel,crun,fileFormat,conlim,ncsize,bypass,
                         section_name,zone_name,weir_name,geom,fmtgeom,iPart,concat)
            except Exception as e:
               raise Exception([filterMessage({'name':'copyPartition'},e,bypass)])
         elif (iFiles[k].split(';')[5][0:5] == 'PARAL' and not iFiles[k].split(';')[4][0:3] == 'LIT') or iFiles[k].split(';')[5][0:5] == 'WEIRS' and weir_name == ' ':
            if use_link:
               print( '  duplilinking: '+ path.basename(crun) )
               for n in range(ncsize):
                  symlinkFile(crun,
                              crun+('00000'+str(ncsize-1))[-5:]+'-'+('00000'+str(n))[-5:])
            else:
               print( '   duplicating: '+ path.basename(crun) )
               for n in range(ncsize):
                  shutil.copy2(crun,
                               crun+('00000'+str(ncsize-1))[-5:]+'-'+('00000'+str(n))[-5:])

   return True

def runPARTEL(partel,file,fileFormat,conlim,ncsize,bypass,section_name,zone_name,weir_name,geom,fmtgeom,iPart,concat):
   # TODO: You should check if the file exist and should be updated (or not)
   putFileContent('PARTEL.PAR',
                  [file,fileFormat,conlim,str(ncsize),str(iPart),
                   section_name,zone_name,weir_name,geom,fmtgeom,concat,''])
   parCmd = partel.replace('<partel.log>','partel_'+file+'.log').split(';')
   mes = MESSAGES(size=10)
   for p in parCmd:
      try:
         print( '    +> '+p )
         tail,code = mes.runCmd(p,bypass)
      except Exception as e:
         raise Exception([filterMessage(
               {'name':'runPARTEL',
                'msg':'something went wrong, I am not sure why. Here is the log:\n'+
                      '\n'.join(getFileContent('partel_'+file+'.log'))
               },e,bypass)])
      if code != 0:
         raise Exception([
               {'name':'runPARTEL',
                'msg':'Could not split your file '+file\
                      +' (runcode='+str(code)+') with the error as follows:'\
                      +'\n      '+tail\
                      +'\n      You may have forgotten to compile PARTEL '\
                      +'with the appropriate compiler directive'\
                      +'\n        (add -DHAVE_MPI to your cmd_obj in your configuration file).'\
                      +'\n\nHere is the log:\n'+'\n'.join(getFileContent('partel_'+file+'.log'))
               }])
   return

# ~~~ CCW: amended runCode to include optional listing file        ~~~
# ~~~      print_twice echos the listing output to the sortie file ~~~
def print_twice(pipe,ofile,last_line):

   # Utility subroutine to print listing data both to stdout
   # and to the listing file, accessed via the ofile handle
   lastlineempty = False      # JPG addition here as opposed to argument
   last_dat = ''
   for line in iter(pipe.readline,''):
      dat = line.rstrip()
      # This IF statement just avoid printing a lot of blank lines
      # at the end of the run, before Python realises that the process
      # has stopped.
      if (dat == ''):
         if not lastlineempty:
            print( dat )                # Print to screen
            if ofile != None:
               ofile.write(dat+'\n')    # Write to sortiefile (if requested)
            lastlineempty = True        # Set to avoid printing multiple consecutive newlines
      else:
         lastlineempty = False
         print( dat )                   # Print to screen
         if ofile != None:
            ofile.write(dat+'\n')       # Write to sortiefile (if requested)
         last_dat = dat

   last_line.append(last_dat)

def runCode(exe,sortiefile):
   ofile = None
   lasterr = []
   lastout = []
   # If sortiefile is required, open it
   if sortiefile != None: ofile = open(sortiefile,"w")
   # Start process with command 'exe', and direct standard output and
   # standard error into PIPE (part of the Popen object called proc)
   proc = Popen(exe,bufsize=1024,stdout=PIPE,stderr=PIPE,shell=True)
   # Define a thread, t1, t2 that will execute the subroutine 'print_twice', with
   # the args given.
   t1 = threading.Thread(target=print_twice,args=(proc.stdout,ofile,lastout))
   t2 = threading.Thread(target=print_twice,args=(proc.stderr,ofile,lasterr))
   # Start the print_twice thread. This continues until the stdout buffer is empty
   # (usually when the Telemac code has terminated)
   t1.start()
   t2.start()
   # Wait for t1, t2 to terminate before continuing
   t1.join()
   t2.join()
   # Close the sortiefile, if used
   if ofile: ofile.close()
   # Wait to make sure that the Telemac code really has terminated
   # Note: this is probably unnecessary, but done to make sure that
   #       a zero return code is returned, indicating successful completion.
   proc.wait()
   if proc.returncode == 0: return True
   raise Exception({'name':'runCode','msg':'Fail to run\n'+exe+
      '\n'+'~'*18+'\n'+str(lasterr[0])+'\n'+'~'*18})
   return False

def runRecollection(gretel,cas,glogeo,fmtgeo,globnd,oFiles,ncsize,bypass):

   if ncsize < 2: return True
   # ~~ aggregate output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   for k in cas[1][0]:
      if  k in oFiles:
         crun = oFiles[k].split(';')[1]
         tpe = oFiles[k].split(';')[5]
         if tpe[0:7] == 'SELAFIN':
            print( '    collecting: '+ path.basename(crun) )
            try:
               fileFormat = getFileFormat(cas,k)
               # We need nplan for gretel in med format
               idx = -1
               if "NUMBER OF HORIZONTAL LEVELS" in cas[1][0]:
                  idx = cas[1][0].index("NUMBER OF HORIZONTAL LEVELS")
               if "NOMBRE DE PLANS HORIZONTAUX" in cas[1][0]:
                  idx = cas[1][0].index("NOMBRE DE PLANS HORIZONTAUX")
               nplan = 0 if idx == -1 else cas[1][1][idx][0]
               runGRETEL(gretel,crun,fileFormat,glogeo,fmtgeo,globnd,ncsize,nplan,bypass)
            except Exception as e:
               raise Exception([filterMessage({'name':'runRecollection'},e,bypass)])
         if tpe[0:6] == 'DELWAQ':
            print( '    collecting: '+ path.basename(crun) )
            try:
               runGREDEL(gretel,crun,glogeo,tpe[6:],ncsize,bypass)
            except Exception as e:
               raise Exception([filterMessage({'name':'runRecollection'},e,bypass)])
   return True

def runGRETEL(gretel,file,fileFormat,geom,geoFormat,bnd,ncsize,nplan,bypass):

   # ~~ Run GRETEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   gretel_par = 'gretel_'+file+'.par'
   gretel_log = 'gretel_'+file+'.log'
   putFileContent(gretel_par,
                  [geom,geoFormat,bnd,file,fileFormat,str(ncsize),str(nplan)])
   mes = MESSAGES(size=10)
   cmd = '%s < %s >> %s'%(gretel, gretel_par, gretel_log)
   try:
      tail,code = mes.runCmd(cmd,bypass)
   except Exception as e:
      raise Exception(
             [filterMessage(
               {'name':'runGRETEL',
                'msg':'something went wrong, I am not sure why. Here is the log:\n'\
                      +'\n'.join(getFileContent(gretel_log))
               },e,bypass)])
   if code != 0:
      raise Exception([
            {'name':'runGRETEL',
             'msg':'Could not split your file '+file\
                   +' (runcode='+str(code)+') with the error as follows:'\
                   +'\n      '+tail+'\n\nHere is the log:\n'\
                   +'\n'.join(getFileContent(gretel_log))
            }])
   return

def runGREDEL(gredel,file,geom,type,ncsize,bypass):

   # ~~ Change GRETEL into GREDEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   pg = path.dirname(gredel)
   bg,eg = path.splitext(path.basename(gredel))
   gredel = path.join(pg,'gredel' + type.lower() + '_autop' + eg)
   # ~~ Run GREDEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   putFileContent('gretel_'+file+'.par',[geom,file,str(ncsize)])
   mes = MESSAGES(size=10)
   cmd = gredel+' < gretel_'+file+'.par >> gretel_'+file+'.log'
   try:
      tail,code = mes.runCmd(cmd,bypass)
   except Exception as e:
      raise Exception([filterMessage({'name':'runGREDEL','msg':'something went wrong, I am not sure why. Please verify your compilation or the python script itself.'},e,bypass)])
   if code != 0: raise Exception([{'name':'runGREDEL','msg':'Could not split your file (runcode='+str(code)+').\n     '+file+'\n      '+tail}])
   return

"""
   runCAS now takes in an array of casFiles, and if possible,
      run these in parallel of one another and as one job on a queue
      where the mpi_exec command do the parallelisation
   Notes:
      - casdir is where the CAS files are.
      - The hpccmd is unique. The mpicmd is not (unfortunately).
"""
def runCAS(cfgName,cfg,codeName,casNames,options):

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~~~ Read the main DICO File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   dicoFile = path.join(cfg['MODULES'][codeName]['path'],codeName+'.dico')
   if not path.exists(dicoFile): raise Exception([{'name':'runCAS','msg':'could not find the DICO file: '+dicoFile}])
   print( '\n... reading the main module dictionary' )
   frgb,dico = scanDICO(dicoFile)
   iFS,oFS = getIOFilesSubmit(frgb,dico)
   #> MODFiles avoids duplication of dico parsing
   MODFiles = { codeName:{ 'frgb':frgb,'iFS':iFS,'oFS':oFS,'dico':dico } }
   # ~~> structural assumptions
   pbin = cfg['root']+sep+'builds'+sep+cfgName+sep+'bin'
   pobj = cfg['MODULES'][codeName]['path'].replace(cfg['root']+sep+'sources',cfg['root']+sep+'builds'+sep+cfgName+sep+'obj')
   plib = cfg['root']+sep+'builds'+sep+cfgName+sep+'lib'

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~~~ Acquiring all CAS file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.split, to copy the correct CAS files and get the LIT files
   #    - if options.run, to get to the FORTRAN FILE for compilation
   #    - if options.compileonly, same as options.run
   #    - if options.merge, to get the ECR files and associated ncsize
   print( '\n... processing the main CAS file(s)' )
   try:
      cases,lang,ncsize = processCAS(casNames,MODFiles[codeName]['dico'],MODFiles[codeName]['frgb'])
   except Exception as e:
      raise Exception([filterMessage({'name':'runCAS'},e,options.bypass)])
   CASFiles = {}
   # ~~ Find a common root to all CAS files ~~~~~~~~~~~~~~~~~~~~~
   # /!\ in case of multiple CAS files, all CAS files have to leave at the same address
   casdir = ''
   for cas,casName in zip(cases,casNames):
      name = path.basename(casName)
      if casdir == '': casdir = path.dirname(path.realpath(casName))
      elif casdir != path.dirname(path.realpath(casName)):
         raise Exception([{'name':'runCAS','msg':'Location of more than one CAS file is not common to all:' \
            '    +> you should have all your CAS files within the same directory'}])
      CASFiles.update({ name:{ 'code':codeName, 'cas':cas, 'dir':casdir } })
   ncruns = len(CASFiles)

   # /!\ options.mpi is True only if you are in your second call, within the HPC queue; you have already used up your HPC credits
   if options.mpi and 'HPC' in cfg: cfg['HPC'] = {}
   # /!\ hpcpass is True only if you are in your first call and you intend to do a second call with the same configuration
   hpcpass = False
   if cfg['HPC'] != {}: hpcpass = ( 'PYCODE' in cfg['HPC'] )

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.split, to know where to copy the LIT files into
   #    - if options.run, to run
   #    - if options.compileonly, same as options.run
   #    - if options.merge, to know where to copy the ECR file from
   # Outputs ...
   #    > the full name of the sortie file without extension, in CASFiles[name]['sortie']
   #    > wdir same as above but would be reset with -w option
   #    > CASFiles[name]['wir'] is where the local executable will be
   #    > CASFiles[name]['wrt'] is whether to force the update or not
   #    > CASFiles[name]['dir'] is where the CAS file is from
   print( '\n... handling temporary directories' )
   for name in CASFiles:
      # ~~> default temporary directory name
      TMPDir = processTMP(CASFiles[name]['dir']+sep+name)    #/!\ includes date/time in the name
      wdir = TMPDir
      CASFiles[name].update({ 'wir':wdir, 'wrt':False, 'sortie':TMPDir })
      # ~~> user defined directory name
      if options.wDir != '':
         if ncruns == 1: wdir = path.join(CASFiles[name]['dir'],options.wDir)
         else: wdir = path.join(CASFiles[name]['dir'],path.basename(options.wDir)+'_'+name)
         CASFiles[name]['wir'] = wdir
      # ~~> dealing with the temporary directory
      if not path.exists(wdir):
         mkdir(wdir)
         CASFiles[name]['wrt'] = True

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Read the included CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.split, to include coupling code files in LIT
   #    - if options.run, to run in coupled mode ?? may not need to know that ??
   #    - if options.compileonly, to aggregate FORTRAN FILEs
   #    - if options.merge, to include coupling code files in ECR
   print( '\n... checking coupling between codes' )
   for name in CASFiles:
      cplages,defaut = getKeyWord('COUPLING WITH',CASFiles[name]['cas'],MODFiles[CASFiles[name]['code']]['dico'],MODFiles[CASFiles[name]['code']]['frgb'])
      CASFiles[name].update({ 'with':{} })

      #/!\ having done the loop this way it will not check for DELWAQ
      for cplage in cplages:
         for mod in cfg['MODULES']:
            if mod in cplage.lower():

               # ~~~~ Extract the CAS File name ~~~~~~~~~~~~~~~~~~~~~~~
               casNamePlage,defaut = getKeyWord(mod.upper()+' STEERING FILE',CASFiles[name]['cas'],MODFiles[CASFiles[name]['code']]['dico'],MODFiles[CASFiles[name]['code']]['frgb'])
               if casNamePlage == []: casNamePlage = defaut[0]
               else: casNamePlage = casNamePlage[0].strip("'\"")
               casNamePlage = path.join(CASFiles[name]['dir'],casNamePlage)
               if not path.isfile(casNamePlage): raise Exception([{'name':'runCAS','msg':'missing coupling CAS file for '+mod+': '+casNamePlage}])

               # ~~~~ Read the DICO File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               if mod not in MODFiles:
                  dicoFilePlage = path.join(cfg['MODULES'][mod]['path'],mod+'.dico')
                  if not path.exists(dicoFilePlage): raise Exception([{'name':'getDICO','msg':'could not find the DICO file: '+dicoFilePlage}])
                  frgbPlage,dicoPlage = scanDICO(dicoFilePlage)
                  iFSPlage,oFSPlage = getIOFilesSubmit(frgbPlage,dicoPlage)
                  MODFiles.update({ mod:{ 'frgb':frgbPlage,'iFS':iFSPlage,'oFS':oFSPlage,'dico':dicoPlage } })

               # ~~ Read the coupled CAS File ~~~~~~~~~~~~~~~~~~~~~~~~~
               casPlage,l,n = processCAS([casNamePlage],MODFiles[mod]['dico'],MODFiles[mod]['frgb'])
               CASFiles[name]['with'].update({ casNamePlage:{ 'code':mod, 'cas':casPlage[0] } })

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Common behaviours for all CAS files ~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.split, to copy the correct CAS files once ncsize updated
   #    - if options.run, to re-copy the correct CAS file
   #    - if options.merge, to get ncsize but no need to copy correct CAS file anymore
   # Outputs ...
   #    > nctile, ncnode, ncsize
   #    > lang, casdir
   print( '\n... checking parallelisation' )
   nctile,ncnode,ncsize = checkParaTilling(options.nctile,options.ncnode,options.ncsize,ncruns,ncsize)
   if cfg['MPI'] != {}: ncsize = max( 1, ncsize )
   elif ncsize > 1:
      raise Exception([{'name':'runCAS','msg':'parallel inconsistency: ' \
         '\n    +> you may be using an inappropriate configuration: '+cfgName+ \
         '\n    +> or may be wishing for scalar mode while setting to '+str(ncsize)+' processors'}])
   if cfg['MPI'] == {}: ncsize = 0      #TODO: check if this is still useful
   # ~~ Forces keyword if parallel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # /!\ in case of multiple CAS files, you have to have the same ncsize
   if lang == 1:
      proc_parall='PROCESSEURS PARALLELES'
   elif lang == 2:
      proc_parall='PARALLEL PROCESSORS'
   # Adding the number of processors in the cas file
   for name in CASFiles:
      CASFiles[name]['cas'] = \
         setKeyValue(proc_parall,CASFiles[name]['cas'],MODFiles[CASFiles[name]['code']]['frgb'],ncsize)
      # Adding in coupled cas file as well
      for modcasname in CASFiles[name]['with']:
         setKeyValue(proc_parall,CASFiles[name]['with'][modcasname]['cas'],
                     MODFiles[CASFiles[name]['with'][modcasname]['code']]['frgb'],
                     ncsize)

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling all input files (PART I) ~~~~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.split, obvisouly this is PART I of the main file pre-processing
   #    - if options.compileonly, you also need to copy the FORTRAN FILE
   # This does not include the processing of the Fortran file(s) and associated
   #  compilation
   #
   section_name = ' '
   zone_name = ' '
   weir_name = ' '
   if not options.merge and not options.run and not hpcpass:
      print( '\n... first pass at updating all input files' )
      for name in CASFiles:
         # >>> Placing yourself where the CAS File is
         chdir(CASFiles[name]['dir'])
         # >>> Copy INPUT files into wdir
         try:
            # /!\ recompilation is not necessary and sometimes unwanted
            # Removing princi folder if it exists
            #if path.exists(path.join(CASFiles[name]['wir'],'user_fortran')):
            #   for file in listdir(path.join(CASFiles[name]['wir'],'user_fortran')):
            #      remove(file)
            section_name,zone_name,weir_name = processLIT(CASFiles[name]['cas'],
                       MODFiles[CASFiles[name]['code']]['iFS'],
                       CASFiles[name]['wir'],ncsize,CASFiles[name]['wrt'],
                       MODFiles[CASFiles[name]['code']]['dico'],
                       MODFiles[CASFiles[name]['code']]['frgb'],options.use_link)
            # Adding section name to CAS file information as the coupled module
            # might have sections and zones as well
            CASFiles[name]['section'] = section_name
            CASFiles[name]['zone'] = zone_name
            CASFiles[name]['weir'] = weir_name
         except Exception as e:
            raise Exception([filterMessage({'name':'runCAS'},e,options.bypass)])  # only one item here
         for cplage in CASFiles[name]['with']:
            try:
               section_name,zone_name,weir_name = processLIT(CASFiles[name]['with'][cplage]['cas'],
                          MODFiles[CASFiles[name]['with'][cplage]['code']]['iFS'],
                          CASFiles[name]['wir'],ncsize,CASFiles[name]['wrt'],
                          MODFiles[CASFiles[name]['code']]['dico'],
                          MODFiles[CASFiles[name]['code']]['frgb'],options.use_link)
               CASFiles[name]['with'][cplage]['section'] = section_name
               CASFiles[name]['with'][cplage]['zone'] = zone_name
               CASFiles[name]['with'][cplage]['weir'] = weir_name
            except Exception as e:
               raise Exception([filterMessage({'name':'runCAS'},e,options.bypass)])  # only one item here
         # >>> Placing yourself into the wdir
         chdir(CASFiles[name]['wir'])
         # >>> Creating LNG file
         processCONFIG(lang)


   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling the executables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.run, obvisouly this is the main executable to run
   #    - if options.compileonly, obvisouly this is the main executable to create
   #    - if you are in your first pass of two of the HPC configuration, as you may need to update the name in the STDIN script
   if not options.merge and not options.split and not hpcpass:
      print( '\n... checking the executable' )
      for name in CASFiles:
         # >>> Placing yourself where the CAS is
         chdir(CASFiles[name]['dir'])
         try:
            CASFiles[name]['exe'],_ = processExecutable(CASFiles[name],pbin,plib,pobj,cfg['SYSTEM'],
                     MODFiles[CASFiles[name]['code']]['dico'],
                     MODFiles[CASFiles[name]['code']]['frgb'],cfg['TRACE'],options.bypass)
         except Exception as e:
            raise Exception([filterMessage({'name':'runCAS','msg':'failed to compile'},e,options.bypass)])  # only one item here
         CASFiles[name]['run'] = CASFiles[name]['exe']

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling the MPI command ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.split, for PARTEL that could be used in parallel
   #    - if options.run, obvisouly this is the main executable to run
   # Note: the mpicmd is unique and run once
   if not options.merge and not hpcpass:
      if cfg['MPI'] != {}:
         print( '\n... modifying run command to MPI instruction' )
         # ~~> MPI host file provided through the command line
         if options.hosts != '':
            if 'HOSTS' in cfg['MPI']: cfg['MPI']['HOSTS'] = options.hosts.replace(':',' ')
            else: cfg['MPI'].update( {'HOSTS':options.hosts.replace(':',' ')} )
         # ~~> MPI host file ( may be re-written by the HPC INFILE script )
         hostfile = cfg['MPI']['HOSTFILE']
         hosts = []; n = 0
         while n < ncsize:
            for i in cfg['MPI']['HOSTS'].split():
               hosts.append(i); n += 1
               if n == ncsize: break
         # ~~> MPI Command line and options ( except <exename> )
         mpicmd = getMPICommand(cfg['MPI']).replace('<root>',cfg['root']) # /!\ cfg['MPI'] is also modified
         # mpi_exec supports: -n <ncsize> -wdir <wdir> <exename>
         mpicmd = mpicmd.replace('<ncsize>',str(ncsize))
         for name in CASFiles:
            # >>> Parallel execution configuration
            chdir(CASFiles[name]['wir'])
            mpi = mpicmd
            # ~~> filling in the blanks
            mpi = mpi.replace('<wdir>',CASFiles[name]['wir'])
            CASFiles[name]['mpi'] = mpi
            if not options.split:
               CASFiles[name]['run'] = mpi.replace('<exename>',CASFiles[name]['exe']) #path.basename(CASFiles[name]['exe']))
            # ~~> no file handling necessary if hpcpass
            if hpcpass: continue
            # ~~> Creating the HOST file
            putFileContent(hostfile,hosts)
            # ~~> Creating the PARA file
            putFileContent('PARAL',[str(ncsize),str(len(CASFiles[name]['wir']+sep)),CASFiles[name]['wir']+sep,''])

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Getting out if compile only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if options.compileonly and not hpcpass:
      print( '\n\n'+'~'*72+'\n' )
      print( '... Your simulation is ready for launch and you can now :\n' )
      print( '    +> re-run without option -x (--compileonly) or with option --run\n' )
      if cfg['MPI'] == {}:
         print( '    +> or run the following command within each local subdirectory:' )
         for name in CASFiles:
            print( '       -> in <'+CASFiles[name]['wir'].replace(CASFiles[name]['dir'],'.')+sep+'>'+ \
               ' run with EXE:\n                 '+path.basename(CASFiles[name]['exe']) )
      else:
         print( '    +> or run with MPI: ' )
         for name in CASFiles: print( '                 '+CASFiles[name]['run'] )
      return []

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling the PARTEL command and partitioning ~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.split, to execute PARTEL if ncsize > 0
   #    - options.compileonly is out already
   if not options.merge and not options.run and ncsize > 0 and not hpcpass:
      print( '\n... modifying run command to PARTEL instruction' )
      for name in CASFiles:
         chdir(CASFiles[name]['wir'])
         # ~~> Path
         parcmd = getPartelCmd(pbin,cfg,CASFiles[name]).replace('<root>',cfg['root'])
         # >>> Add running command
         CASFiles[name].update({ 'par':parcmd })

         # ~~> Run PARTEL for the base files (GEO,CONLIM,SECTIONS,ZONES,WEIRS)
         CONLIM = getCONLIM(CASFiles[name]['cas'],
                            MODFiles[CASFiles[name]['code']]['iFS'])    # Global CONLIM file
         GLOGEO,FMTGEO,GLOBND = getGLOGEO(CASFiles[name]['cas'],
                                   MODFiles[CASFiles[name]['code']]['iFS'])    # Global GEO file
         section_name = CASFiles[name]['section']
         zone_name = CASFiles[name]['zone']
         weir_name = CASFiles[name]['weir']
         # Identify the partitioner to use for Partel
         part2int = {"'METIS'":1,"'SCOTCH'":2,"'PARMETIS'":3,"'PTSCOTCH'":4}
         idx = -1
         if "PARTITIONING TOOL" in CASFiles[name]['cas'][1][0]:
            idx = CASFiles[name]['cas'][1][0].index("PARTITIONING TOOL")
         if "PARTITIONNEUR" in CASFiles[name]['cas'][1][0]:
            idx = CASFiles[name]['cas'][1][0].index("PARTITIONNEUR")
         # If the key in not in the steering file we use metis
         iPart = 1 if idx == -1 else part2int[CASFiles[name]['cas'][1][1][idx][0]]
         #Are we gonna concatenate the output of partel or not ?
         concat, concat_default = getKeyWord('CONCATENATE PARTEL OUTPUT',cas,dico,frgb)
         if(concat==[]):
             if(concat_default!=[]):
                 concat=concat_default[0]
             else:
                 concat='NO'
         else:
             if(concat[0]=='TRUE'):
                 concat='YES'
             else:
                 concat='NO'

         try:
            runPartition(parcmd,GLOGEO,FMTGEO,CONLIM,ncsize,
                         options.bypass,section_name,zone_name,weir_name,iPart,concat)
         except Exception as e:
            raise Exception([filterMessage({'name':'runCAS','msg':'Could not partition the base files for the following CAS file: '+name},e,options.bypass)])
         # ~~> Copy partition for the other input files
         copyPartition(parcmd,CASFiles[name]['cas'],GLOGEO,FMTGEO,CONLIM,\
                       MODFiles[CASFiles[name]['code']]['iFS'],\
                       ncsize,False,section_name,zone_name,weir_name,options.use_link,iPart,concat)

         for cplage in CASFiles[name]['with']:
            CONLIM = getCONLIM(CASFiles[name]['with'][cplage]['cas'],MODFiles[CASFiles[name]['with'][cplage]['code']]['iFS'])
            GLOGEO, FMTGEO, GLOBND = getGLOGEO(CASFiles[name]['with'][cplage]['cas'],MODFiles[CASFiles[name]['with'][cplage]['code']]['iFS'])
            section_name = CASFiles[name]['with'][cplage]['section']
            zone_name = CASFiles[name]['with'][cplage]['zone']
            weir_name = CASFiles[name]['with'][cplage]['weir']
            try:
               runPartition(parcmd,GLOGEO,FMTGEO,CONLIM,ncsize,options.bypass,section_name,zone_name,weir_name,iPart,concat)
            except Exception as e:
               raise Exception([filterMessage({'name':'runCAS','msg':'Could not partition the base files for the CAS file couled with: '+name},e,options.bypass)])
            # ~~> Copy partition for the other input files
            copyPartition(parcmd,CASFiles[name]['with'][cplage]['cas'],GLOGEO,FMTGEO,CONLIM,\
                          MODFiles[CASFiles[name]['with'][cplage]['code']]['iFS'],\
                          ncsize,False,section_name,zone_name,weir_name,options.use_link,iPart,concat)

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Getting out if split only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if options.split and not hpcpass:
      print( '\n\n'+'~'*72+'\n' )
      print( '... Your simulation is almost ready for launch. You need to compile your executable with the option -x (--compileonly)\n' )
      return []          # Split only: do not proceed any further

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling sortie file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Outputs ...
   #    > CASFiles.values()[0]['sortie']
   print( '\n... handling sortie file(s)' )
   if not options.sortieFile:
      for name in CASFiles: CASFiles[name]['sortie'] = None
   else:
      if options.merge:
         # try re-using existing/latest sortie file with same root name
         for name in CASFiles: CASFiles[name]['sortie'] = path.basename(getLatestSortieFiles(path.join(CASFiles[name]['wir'],path.basename(name)))[0])
      else:
         # define the filename (basename) of the sortie file
         for name in CASFiles: CASFiles[name]['sortie'] = path.basename(CASFiles[name]['sortie'])+'.sortie'

   if not options.merge:
      print( '\n\nRunning your simulation(s) :\n'+'~'*72+'\n' )
   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Running the Executable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - options.split is out already
   #    - options.compileonly is out already
   #    - if options.run, obvisouly this is the main run of the executable
   # Inputs ...
   #    - runcmd if options.hpc
   #    - CASFiles[name]['run'] and CASFiles[name]['sortie'] otherwise
      if cfg['HPC'] == {}:
         for name in CASFiles:  # /!\ This should be done in parallel when multiple CASFiles
            chdir(CASFiles[name]['wir'])
            print( '\n\n'+CASFiles[name]['run']+'\n\n' )
            # ~~> added banner - TODO: add RELEASE to the dico
            #value,defaut = getKeyWord('RELEASE',CASFiles[name]['cas'],MODFiles[CASFiles[name]['code']]['dico'],MODFiles[CASFiles[name]['code']]['frgb'])
            #if defaut != []: print( '\n'.join(banner(CASFiles[name]['code']+' - '+defaut[0].lower())) )
            # ~~> here you go run
            if not runCode(CASFiles[name]['run'],CASFiles[name]['sortie']):
               raise Exception([filterMessage({'name':'runCAS','msg':'Did not seem to catch that error...'})])

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling the HPC before running ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # You need to do this if ...
   #    - if options.run, obvisouly this is the main executable to run
   # Inputs ...
   #    - ncsize, nctilem ncnode, wdir, casdir, options, codeName
   #    - cfg['HPC']['STDIN'] and cfg['MPI']['HOSTS']
   #    - CASFiles.values()[0]['sortie'] and CASFiles.values()[0]['exe']
   #    - CASFiles[name]['run']
   # Outputs ...
   #    > runcmd and putFileContent(stdinfile,)
      elif 'STDIN' not in cfg['HPC']:
         raise Exception([{'name':'runCAS','msg':'\nI would need the key hpc_stdin in you configuration so I can launch your simulation on the HPC queue.'}])
      else:
         jobID = ''
         for name in CASFiles:  # /!\ This is being done in parallel when multiple CASFiles
            #if not hpcpass:
            chdir(CASFiles[name]['wir'])
            print( '\n... modifying run command to HPC instruction' )
            # ~~> HPC Command line launching runcode
            hpccmd = getHPCCommand(cfg['HPC']).replace('<root>',cfg['root'])
            if not hpcpass: hpccmd = hpccmd.replace('<wdir>',CASFiles[name]['wir'])
            else: hpccmd = hpccmd.replace('<wdir>',CASFiles[name]['dir'])
            # ~~> HPC dependency between jobs
            hpcjob = getHPCDepend(cfg['HPC'])
            if hpcjob != '' and jobID != '' and hpcpass: hpccmd = hpccmd + ' ' + hpcjob.replace('<jobid>',jobID)
            # ~~> HPC queueing script
            stdinfile = cfg['HPC']['STDIN'][0]   # only one key for now
            stdin = cfg['HPC']['STDIN'][1]
            if cfg['MPI'] != {}: stdin = stdin.replace('<hosts>',cfg['MPI']['HOSTS'])
            stdin = stdin.replace('<root>',cfg['root'])
            stdin = stdin.replace('<configName>',cfgName)
            stdin = stdin.replace('<ncsize>',str(ncsize))
            stdin = stdin.replace('<nctile>',str(nctile))
            stdin = stdin.replace('<ncnode>',str(ncnode))
            stdin = stdin.replace('<email>',options.email)
            if ncruns == 1: stdin = stdin.replace('<jobname>',options.jobname)
            else: stdin = stdin.replace('<jobname>',name)
            stdin = stdin.replace('<time>',strftime("%Y-%m-%d-%Hh%Mmin%Ss", localtime()))
            stdin = stdin.replace('<queue>',options.hpc_queue)
            stdin = stdin.replace('<walltime>',options.walltime)
            stdin = stdin.replace('<codename>',codeName)
            stdin = stdin.replace('\n ','\n')
            if not hpcpass: stdin = stdin.replace('<wdir>',CASFiles[name]['wir'])      # /!\ HPC_STDIN in the TMP directory
            else: stdin = stdin.replace('<wdir>',CASFiles[name]['dir'])
            sortie = 'hpc-job.sortie'
            if options.sortieFile: sortie = CASFiles[name]['sortie']
            stdin = stdin.replace('<sortiefile>',sortie)
            # ~~> Recreate the <mpi_exec> (option --hpc)
            if not hpcpass:
               if 'exe' in CASFiles[name]: stdin = stdin.replace('<exename>',path.basename(CASFiles[name]['exe']))
               else: stdin = stdin.replace('<exename>',CASFiles[name]['run'])
               stdin = stdin.replace('<mpi_cmdexec>',CASFiles[name]['run'])   # /!\ serial mode
            # ~~> Recreate the runcode.py command
            else:
               stdin = stdin.replace('<exename>',name)
               runcmd = 'runcode.py ' + codeName + ' --mpi '
               if options.configName != '': runcmd = runcmd + ' -c ' + options.configName
               if options.configFile != '': runcmd = runcmd + ' -f ' + options.configFile
               if options.rootDir != '': runcmd = runcmd + ' -r ' + options.rootDir
               runcmd = runcmd + ' -s '
               if options.tmpdirectory: runcmd = runcmd + ' -t '
               runcmd = runcmd + ' -w ' + CASFiles[name]['wir']
               runcmd = runcmd + ' --nctile ' + str(nctile)
               runcmd = runcmd + ' --ncnode ' + str(ncnode)
               runcmd = runcmd + ' --ncsize ' + str(ncsize)
               if options.split: runcmd = runcmd + ' --split '
               if options.compileonly: runcmd = runcmd + ' -x '
               if options.merge: runcmd = runcmd + ' --merge '
               if options.run: runcmd = runcmd + ' --run '
               runcmd = runcmd + ' ' + name
               stdin = stdin.replace('<py_runcode>',runcmd)
            # ~~> Write to HPC_STDIN
            #if not hpcpass:
            chdir(CASFiles[name]['wir'])
            putFileContent(stdinfile,stdin.split('\n'))

            # ~~> added banner
            value,defaut = getKeyWord('RELEASE',CASFiles[name]['cas'],MODFiles[CASFiles[name]['code']]['dico'],MODFiles[CASFiles[name]['code']]['frgb'])
            print( '\n'.join(banner(CASFiles[name]['code'])))
            # ~~> here you go run
            if not runCode(hpccmd,sortie):
               raise Exception([filterMessage({'name':'runCAS','msg':'Did not seem to catch that error...'})])
            jobID = getFileContent(sortie)[0].strip()
            print( '... Your simulation ('+name+') has been launched through the queue.\n' )
            if hpcpass:
               print( '   +> You need to wait for completion before checking on results.\n' )
            else:
               print( '   +> You need to wait for completion before re-collecting files using the option --merge\n' )
         return []

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Getting out if run only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #    - options.split and options.compileonly are out already
   if options.run:
      print( '\n\n'+'~'*72+'\n' )
      print( '... Your simulation has been completed but you need to re-collect files using the option --merge\n' )
      return []          # Run only: do not proceed any further

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling the recollection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #    - options.split and options.compileonly and options run are out already
   if ncsize > 0:
      print( '\n\n'+'~'*72+'\n' )
      print( '... merging separated result files\n' )
      # ~~> Path
      execmd = getGretelCmd(pbin,cfg).replace('<root>',cfg['root'])
      # ~~> Run GRETEL
      for name in CASFiles:
         print( '    +> '+name )
         chdir(CASFiles[name]['wir'])
         # Global GEO file
         GLOGEO,FMTGEO,GLOBND = getGLOGEO(CASFiles[name]['cas'],MODFiles[CASFiles[name]['code']]['iFS'])
         runRecollection(execmd,CASFiles[name]['cas'],GLOGEO,FMTGEO,GLOBND,
                         MODFiles[CASFiles[name]['code']]['oFS'],
                         ncsize,options.bypass)
         for cplage in CASFiles[name]['with']:
            GLOGEO,FMTGEO,GLOBND = getGLOGEO(CASFiles[name]['with'][cplage]['cas'],
                                      MODFiles[CASFiles[name]['with'][cplage]['code']]['iFS'])
            runRecollection(execmd,CASFiles[name]['with'][cplage]['cas'],GLOGEO,FMTGEO,GLOBND,
                            MODFiles[CASFiles[name]['with'][cplage]['code']]['oFS'],
                            ncsize,options.bypass)

   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   # ~~ Handling all output files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print( '\n\n'+'~'*72+'\n' )
   print( '... handling result files\n' )
   sortiefiles = []
   for name in CASFiles:
      print( '    +> '+name )
      chdir(CASFiles[name]['wir'])
      # ~~> copying all primary result files
      files = processECR(CASFiles[name]['cas'],MODFiles[CASFiles[name]['code']]['oFS'],
                         CASFiles[name]['dir'],CASFiles[name]['wir'],
                         CASFiles[name]['sortie'],ncsize,options.bypass)
      if options.sortieFile: sortiefiles.extend(files)
      # ~~> copying all coupled result files
      for cplage in CASFiles[name]['with']:
         files = processECR(CASFiles[name]['with'][cplage]['cas'],
                            MODFiles[CASFiles[name]['with'][cplage]['code']]['oFS'],
                            CASFiles[name]['dir'],CASFiles[name]['wir'],None,
                            ncsize,options.bypass)
         if options.sortieFile: sortiefiles.extend(files)
      # ~~> zipping sortie files if necessary
      if not options.nozip and ncsize > 1 and options.sortieFile:
         zipsortie(sortiefiles[0])
      # ~~> post-processing the ARTEMIS animation file if necessary
      files = processARTNIM(CASFiles[name]['cas'],CASFiles[name]['dir'],
                            MODFiles[CASFiles[name]['code']]['dico'],
                            MODFiles[CASFiles[name]['code']]['frgb'])

   # ~~ Handling Directories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if options.tmpdirectory:
      chdir(casdir)
      for name in CASFiles:
         try:
            removeDirectories(CASFiles[name]['wir'])
         except Exception as e:
            print( '\n\nWarning: Your operating system does not allow me to remove a directory\n\n' )

   return sortiefiles

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban; Noemie Durand"
__date__ ="$19-Jul-2010 08:51:29$"

def main(module=None):
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print( '\n\nLoading Options and Configurations\n'+72*'~'+'\n' )
   if module == None:
      parser = ArgumentParser(\
         formatter_class=RawDescriptionHelpFormatter,
         description=('''\n
runcode is the execution launcher for all TELEMAC modules.\n
where module can be:\n
   mascaret    the 1D hydrodyanmic / tracer / water quality solver
   telemac2d   the 2D hydrodyanmic / tracer / water quality solver
   telemac3d   the 3D hydrodynamic / tracer / water quality solver
   artemis     the phase resolving wave solver
   tomawac     the 3rd generation wave transformation solver
   sisyphe     the sediment transport and geomorphogical solver
   stbtel      a pre-processor for the modules
   postel3d    a post-processor for telemac3d
         '''),
         usage=' (--help for help)\n---------\n      =>  %(prog)s module [options] casfile(s)\n---------',
         epilog=('''\nexamples:\n---------
1:    => runcode.py telemac2d -s t2d.cas
---------'''))
      parser.add_argument( "module", default=None, \
         choices=['telemac2d','telemac3d','mascaret','artemis','tomawac','stbtel','postel3d','sisyphe','partel','estel3d'] )
   else:
      parser = ArgumentParser(\
         formatter_class=RawDescriptionHelpFormatter,
         description=('''\n
%(prog)s is one of the execution launcher for the TELEMAC system.
         '''),
         usage=' (--help for help)\n---------\n      =>  %(prog)s [options] casfile(s)\n---------',
         epilog=('''\nexamples:\n---------
1:    => %(prog)s -s t2d.cas
---------'''))
      parser.set_defaults(module=module)
   # ~~> Environment
   parser.add_argument(\
      "-c", "--configname",dest="configName",default='',
      help="specify configuration name, default is randomly found in the configuration file" )
   parser.add_argument(\
      "-f", "--configfile",dest="configFile",default='',
      help="specify configuration file, default is systel.cfg" )
   parser.add_argument(\
      "-r", "--rootdir",dest="rootDir",default='',
      help="specify the root, default is taken from config file" )
   parser.add_argument(\
      "-s", "--sortiefile",action="store_true",dest="sortieFile",default=False,
      help="specify whether there is a sortie file, default is no" )
   parser.add_argument(\
      "-t", "--tmpdirectory",action="store_false",dest="tmpdirectory",default=True,
      help="specify whether the temporary directory is removed, default is yes" )
   parser.add_argument(\
      "-x", "--compileonly",action="store_true",dest="compileonly",default=False,
      help="specify whether to only create an executable but not run, default is no" )
   parser.add_argument(\
      "-w", "--workdirectory",dest="wDir",default='',
      help="specify whether to re-run within a defined subdirectory" )
   parser.add_argument(\
      "--nozip",action="store_true",dest="nozip",default=False,
      help="specify whether to zip the extra sortie file if simulation in parallel" )
   # ~~> HPC / parallel
   if module is None:
      parser.add_argument(\
         "--jobname",dest="jobname",default=path.basename(sys.argv[0]),
         help="specify a jobname for HPC queue tracking" )
   else:
      parser.add_argument(\
         "--jobname",dest="jobname",default=module,
         help="specify a jobname for HPC queue tracking" )
   parser.add_argument(\
      "--queue",dest="hpc_queue",default='',
      help="specify a queue for HPC queue tracking" )
   parser.add_argument(\
      "--walltime",dest="walltime",default='01:00:00',
      help="specify a walltime for HPC queue tracking" )
   parser.add_argument(\
      "--email",dest="email",default='s.bourban@hrwallingford.com',
      help="specify an e-mail adress to warn when HPC job is finished" )
   parser.add_argument(\
      "--hosts",dest="hosts",default='',
      help="specify the list of hosts available for parallel mode, ';' delimited" )
   parser.add_argument(\
      "--ncsize",dest="ncsize",default='',
      help="the number of processors forced in parallel mode" )
   parser.add_argument(\
      "--nctile",dest="nctile",default='0',
      help="the number of core per node. ncsize/nctile is the number of compute nodes" )
   parser.add_argument(\
      "--ncnode",dest="ncnode",default='',
      help="the number of of nodes. ncsize = ncnode*nctile is the total number of compute nodes" )
   parser.add_argument(\
      "--sequential",action="store_true",dest="sequential",default=False,
      help="if present, imposes that multiple CAS files are launched one after the other" )
   parser.add_argument(\
      "--mpi",action="store_true",dest="mpi",default=False,
      help="make sure the mpi command is executed, ignoring any hpc command" )
   parser.add_argument(\
      "--split",action="store_true",dest="split",default=False,
      help="will only do the trace (and the split in parallel) if option there" )
   parser.add_argument(\
      "--merge",action="store_true",dest="merge",default=False,
      help="will only do the output copying (and recollection in parallel) if option there" )
   parser.add_argument(\
      "--run",action="store_true",dest="run",default=False,
      help="will only run the simulation if option there" )
   # ~~> Other
   parser.add_argument(\
      "--use-link",action="store_true",dest="use_link",default=False,
      help="Will use link instead of copy in the temporary folder (Unix system only)" )
   parser.add_argument( "args", metavar='cas file(s)',nargs="+" )

   options = parser.parse_args()
   # If module is mascaret calling mascaret's main
   if options.module == 'mascaret':
       main_mascaret(True)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # The path to the root relates to the script launched, which implies
   # that the user environment knows which to run
   # (this script is stored under .../scripts/python27/)
   #PWD = path.dirname(path.dirname(path.dirname(sys.argv[0])))
   PWD = path.dirname(path.dirname( path.dirname(__file__)) )
   # if the appropriate command line option is used, then reset rootDir
   if options.rootDir != '': PWD = path.abspath(options.rootDir)

   # user configuration name
   USETELCFG = ''
   if 'USETELCFG' in environ: USETELCFG = environ['USETELCFG']
   if options.configName == '': options.configName = USETELCFG
   # user configuration file
   SYSTELCFG = path.join(PWD,'configs')
   if 'SYSTELCFG' in environ: SYSTELCFG = environ['SYSTELCFG']
   if options.configFile != '': SYSTELCFG = options.configFile
   if path.isdir(SYSTELCFG): SYSTELCFG = path.join(SYSTELCFG,'systel.cfg')
   options.configFile = SYSTELCFG

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ banners ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mes = MESSAGES()  # runcode takes its version number from the CAS file
   svnrev = ''
   svnurl = ''
   svnban = 'unknown revision'
   try:
      key_equals = re.compile(r'(?P<key>[^:]*)(?P<after>.*)',re.I)
      tail,code = mes.runCmd('svn info '+PWD,True)
      for line in tail.split('\n'):
         proc = re.match(key_equals,line)
         if proc:
            if proc.group('key').strip() == 'Revision': svnrev = proc.group('after')[1:].strip()
            if proc.group('key').strip() == 'URL': svnurl = proc.group('after')[1:].strip()
   except:
      pass
   if svnrev+svnurl == '':
      print( '\n'.join(banner('unknown revision')) )
   else:
      if svnurl != '': print( '\n'.join(banner(svnurl.split('/')[-1])) )
      if svnrev != '': print( '\n'.join(banner('rev. #'+svnrev)) )

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for one configuration only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if not path.isfile(options.configFile):
      print( '\nNot able to get to the configuration file: ' + options.configFile + '\n' )
      dircfg = path.abspath(path.dirname(options.configFile))
      if path.isdir(dircfg) :
         print( ' ... in directory: ' + dircfg + '\n ... use instead: ' )
         _, _, filenames = walk(dircfg).next()
         for fle in filenames :
            head,tail = path.splitext(fle)
            if tail == '.cfg' :
               print( '    +> '+fle )
      sys.exit(1)
   #if len(options.args) < 2:
   #   print( '\nThe name of the module to run and one CAS file at least are required\n' )
   #   parser.print_help()
   #   sys.exit(1)
   # Checking if symlink is available
   if (options.use_link and not checkSymLink(options.use_link)):
      print( '\nThe symlink option is only available on Linux systems. Remove the option and try again' )
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads command line arguments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   codeName = options.module
   casFiles = options.args

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Works for only one configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cfgs = parseConfigFile(options.configFile,options.configName)
   cfgname = cfgs.iterkeys().next()

   # still in lower case
   if 'root' not in cfgs[cfgname]: cfgs[cfgname]['root'] = PWD
   if options.rootDir != '': cfgs[cfgname]['root'] = PWD
   # recognised keys in the config
   if options.ncsize == '' and 'ncsize' in cfgs[cfgname]:
      options.ncsize = cfgs[cfgname]['ncsize']
   if options.nctile == '' and 'nctile' in cfgs[cfgname]:
      options.nctile = cfgs[cfgname]['nctile']
   if options.ncnode == '' and 'ncnode' in cfgs[cfgname]:
      options.ncnode = cfgs[cfgname]['ncnode']

   # bypass errors and carries on
   options.bypass = False
   if options.split or options.merge or options.run:
      if options.wDir == '':
         print( '\nPlease use option -w (--workdirectory) with either of the options --split, --run or --merge\n' )
         sys.exit(1)
   if (options.split and options.merge) \
      or (options.split and options.run) \
      or (options.split and options.compileonly) \
      or (options.merge and options.run) \
      or (options.merge and options.compileonly) \
      or (options.run and options.compileonly):
      print( '\nOnly one of the options --split, --run, --merge or --compileonly (-x) can be used at a time' )
      sys.exit(1)
   # parsing for proper naming
   cfg = parseConfig_RunningTELEMAC(cfgs[cfgname])

   print( '\n\nRunning your CAS file for:\n'+'~'*72+'\n' )
   print( '    +> configuration: ' +  cfgname )
   if 'brief' in cfgs[cfgname]:
      print( '\n    +> '+'\n    |  '.join(cfgs[cfgname]['brief'].split('\n')) + '\n' )
   print( '    +> root:          ' +  cfgs[cfgname]['root'] )
   if options.wDir != '':
      print( '    +> directory      ' +  options.wDir )
      options.tmpdirectory = False
   print( '\n\n'+'~'*72+'\n' )

# >>> Check wether the config has been compiled for the runcode
   if options.compileonly:
      cfg['REBUILD'] = 1
   if codeName not in cfg['MODULES']:
      print( '\nThe code requested is not installed on this system : ' + codeName + '\n' )
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   xcpts = MESSAGES()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Run the Code from the CAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   try:
      runCAS(cfgname,cfg,codeName,casFiles,options)
   except Exception as e:
      xcpts.addMessages(filterMessage({'name':'_____________\nruncode::main:\n'},e,options.bypass))

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reporting errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if xcpts.notEmpty():
      print( '\n\nHummm ... I could not complete my work.\n'+'~'*72 \
         + xcpts.exceptMessages() )
      sys.exit(1)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   else:
      print( '\n\nMy work is done\n\n' )
      sys.exit(0)

if __name__ == "__main__":
   main(None)