#!/usr/bin/env python
"""@author Juliette C.E. Parisi and Sebastien E. Bourban
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
"""@brief
      Beware the the variables number / names are set in the code below.
      Creates a binary liquid boundary file from a global model (SLF form)
        by interpolating on a given GEO model domain. The time series in
        the BND file are extracted only at liquid nodes as defined in the
        CONLIM file.
"""
"""@history 02/12/2013 -- Juliette C.E. Parisi
      Created draft script to write the binary liquid boundary file
         on the basis of the HYCOM global model results
"""
"""@history 11/11/2014 -- Sebastien E. Bourban
      Heavy modifictaions to make it generic and in order to
         correctly fill-in the IPOBO and the IKLE in both 2D and 3D
         based on fancy numpy programing.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
from struct import unpack
import sys
from os import path
import numpy as np
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
from utils.progressbar import ProgressBar
from utils.files import putFileContent
from converters import convertUTM as utm


# ~~> matplotlib and pyplot
import matplotlib as mpl
#try: hide_default = not sys.stderr.isatty()
#except AttributeError: hide_default = True # output does not support isatty()
#mpl.use('Agg') # Use of Agg must be done before importing matplotlib.pyplot
import matplotlib.pyplot as plt
import matplotlib.cm as cm                                  # used for colour maps
#from matplotlib.colors import LinearSegmentedColormap
import matplotlib.collections as collections                # used for collections

#npts = 200
#x = np.random.uniform(-2,2,npts)
#y = np.random.uniform(-2,2,npts)
#z = x * np.exp( -x**2 -y**2 )
#plt.subplot(111)
#plt.plot(x,y,'ko',ms=3)
#plt.xlim(-2,2)
#plt.ylim(-2,2)
#plt.show()
#sys.exit()

def drawColouredQuadMaps(npoin,nelem,x,y,z):

   fig = plt.subplot(111)
   
   # ~~> Focus on current subplot / axes instance
   crax = plt.gca()
   # ~~> Plot data
   colourmap = cm.jet
   #im = plt.imshow( z, cmap=cm.jet, interpolation="nearest" )
   #plt.colorbar( im )
   #if 'cmapPlot' in geometry:
   #   colourmap = LinearSegmentedColormap('User', getColourMap(geometry['cmapPlot']))
   #zmin = np.min(z); zmax = np.max(z)

   nx = npoin-nelem
   ny = int(npoin/nx)
   #plt.plot(y,x,'ko',ms=3)
   #mesh = np.column_stack((x,y))
   msh = collections.QuadMesh(nx-1,ny-1,np.column_stack((x,y)),True,shading='gouraud')
   
   #msh.set_array(np.zeros(self.x_cells*self.y_cells))
   #msh.set_array(np.array(self.FD.GetTimestepData(0)))
   #msh.set_clim(0.0, 1.0)
   #axis.axis([0, self.x_max, 0, self.y_top])
   #plt.colorbar(self.cax)
   ###!!! I have tried cax and msh, and various combos
   #toolbar.show()
   #canvas.draw()
   msh.set_array(z.ravel())
   crax.add_collection(msh)

   #cs = plt.tricontour(x,y,ikle, z, linewidths=0.5, colors='k')
   #ex: colors='k' or colors=('r', 'g', 'b', (1,1,0), '#afeeee', '1')
   #plt.tricontourf(x,y,ikle, z, cmap=colourmap)
   # adds numbers along the iso-contours
   #plt.clabel(cs,fontsize=9,inline=1)
   xmin = min(x)
   xmax = max(x)
   ymin = min(y)
   ymax = max(y)
   crax.set_xlim(xmin-0.05,xmax+0.05)   # sets x axis limits, default 0-1
   crax.set_ylim(ymin-0.05,ymax+0.05)   # sets y axis limits, default 0-1
   crax.axis('equal')         # sets both axis scale to be equal


   #mp.set_title('%s\n2D mesh with %d elements, timestep %d, Variable - %s' %(d['NAME'],d['NELEM3'],t,d['VARNAMES'][v]))     # sets up title
   #if 'cmapPlot' in geometry: fig.colorbar(colection)     # sets up colourbar
   #if 'cmapPlot' in geometry: fig.colorbar(colormap)     # sets up colourbar
   plt.show()

   return

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#
__author__="Juliette C.E. Parisi"
__date__ ="$02-Dec-2013 15:09:48$"

if __name__ == "__main__":

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Dependencies towards other modules ~~~~~~~~~~~~~~~~~~~~~~~~~~
   from argparse import ArgumentParser,RawDescriptionHelpFormatter
   from parsers.parserSELAFIN import CONLIM,SELAFIN,subsetVariablesSLF
   from samplers.meshes import xysLocateMesh

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print( '\n\nInterpreting command line options\n'+'~'*72+'\n' )
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
A script to map spectral outter model results, stored as SELAFIN files, onto the
   spatially and time varying boundary of a spatially contained SELAFIN file
   of your choosing (your MESH).
      '''),
      usage=' (--help for help)\n---------\n       =>  %(prog)s  open-bound.cli open-bound.slf in-outer-geo.slf in-outer-spec.slf out-bound.slf \n---------')
   parser.add_argument(\
      "--ll2utm",dest="ll2utm",default=None,
      help="assume outer file is in lat-long and open-bound file in UTM" )
   parser.add_argument( "args",default='',nargs=5 )
   options = parser.parse_args()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ cli+slf new mesh ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   cliFile = options.args[0]
   if not path.exists(cliFile):
      print( '... the provided cliFile does not seem to exist: '+cliFile+'\n\n' )
      sys.exit(1)
   geoFile = options.args[1]
   if not path.exists(geoFile):
      print( '... the provided geoFile does not seem to exist: '+geoFile+'\n\n' )
      sys.exit(1)

   # Read the new CLI file to get boundary node numbers
   print( '   +> getting hold of the CONLIM file and of its liquid boundaries' )
   cli = CONLIM(cliFile)
   # Keeping only open boundary nodes
   BOR = np.extract( cli.BOR['lih'] != 2, cli.BOR['n'] )

   # Find corresponding (x,y) in corresponding new mesh
   print( '   +> getting hold of the GEO file and of its bathymetry' )
   geo = SELAFIN(geoFile)
   #bat = geo.getVariablesAt( 0,subsetVariablesSLF("BOTTOM: ",geo.VARNAMES)[0] )[0]
   if options.ll2utm != None:
      zone = int(options.ll2utm)
      x,y = utm.toLatLong(geo.MESHX[BOR-1],geo.MESHY[BOR-1],zone)
   else:
      x = geo.MESHX[BOR-1]
      y = geo.MESHY[BOR-1]
   xys = np.vstack( (x,y) ).T
      
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ slf+spe existing res ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   slfFile = options.args[2]
   if not path.exists(slfFile):
      print( '... the provided slfFile does not seem to exist: '+slfFile+'\n\n' )
      sys.exit(1)
   slf = SELAFIN(slfFile)
   slf.setKDTree()
   slf.setMPLTri()
   speFile = options.args[3]
   if not path.exists(speFile):
      print( '... the provided slfFile does not seem to exist: '+speFile+'\n\n' )
      sys.exit(1)
   spe = SELAFIN(speFile)

   print( '   +> support extraction' )
   # Extract triangles and weigths in 2D
   support2d = []
   ibar = 0; pbar = ProgressBar(maxval=len(xys)).start()
   for xyi in xys:
      support2d.append(xysLocateMesh(xyi,slf.IKLE2,slf.MESHX,slf.MESHY,slf.tree,slf.neighbours))
      ibar+=1
      pbar.update(ibar)
   pbar.finish()
   
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ writes BND header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   bndFile = options.args[4]
   bnd = SELAFIN('')
   bnd.fole = {}
   bnd.fole.update({ 'hook': open(bndFile,'wb') })
   bnd.fole.update({ 'name': bndFile})
   bnd.fole.update({ 'endian': ">" })     # big endian
   bnd.fole.update({ 'float': ('f',4) })  # single precision

   # Meta data and variable names
   bnd.TITLE = spe.TITLE
   # spectrum for new locations / nodes
   for i in range(len(BOR)):
      bnd.VARNAMES.append(('F'+('00'+str(i))[-2:]+' PT2D'+('000000'+str(BOR[i]))[-6:]+'                ')[:16])
      bnd.VARUNITS.append('UI              ')
   bnd.NBV1 = len(bnd.VARNAMES)
   bnd.NVAR = bnd.NBV1
   bnd.VARINDEX = range(bnd.NVAR)

   # sizes and mesh connectivity / spectrum
   bnd.NPLAN = spe.NPLAN
   bnd.NDP2 = spe.NDP2
   bnd.NDP3 = bnd.NDP2
   bnd.NPOIN2 = spe.NPOIN2
   bnd.NPOIN3 = spe.NPOIN3
   bnd.IPARAM = spe.IPARAM
   bnd.IPOB2 = spe.IPOB2
   bnd.IKLE2 = spe.IKLE2
   # Last few numbers
   bnd.NELEM2 = len(bnd.IKLE2)
   bnd.NELEM3 = bnd.NELEM2
   bnd.IPOB3 = bnd.IPOB2
   bnd.IKLE3 = bnd.IKLE2
   # Mesh coordinates
   bnd.MESHX = spe.MESHX
   bnd.MESHY = spe.MESHY

   print( '   +> writing header' )
   # Write header
   bnd.appendHeaderSLF()

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ writes BND core ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   print( '   +> setting variables' )
   # TIME and DATE extraction
   bnd.DATETIME = spe.DATETIME
   bnd.tags['times'] = spe.tags['times']
   
   # pointer initialisation
   f = spe.file['hook']
   endian = spe.file['endian']
   ftype,fsize = spe.file['float']

   # Identofy variables (required for support2d geo-locations)
   specloc = []
   for n,a in support2d: specloc.extend(n)
   varsIndexes = np.unique(specloc)
   if fsize == 4:
      z = np.zeros((len(varsIndexes),spe.NPOIN2),dtype=np.float32)
      d = np.zeros(spe.NPOIN2,dtype=np.float32)
   else:
      z = np.zeros((len(varsIndexes),spe.NPOIN2),dtype=np.float64)
      d = np.zeros(spe.NPOIN2,dtype=np.float64)
   
   # Read / Write data, one time step at a time to support large files
   print( '   +> reading / writing variables' )
   pbar = ProgressBar(maxval=len(spe.tags['times'])).start()
   for t in range(len(spe.tags['times'])):
      f.seek(spe.tags['cores'][t])            # [t] is the frame to be extracted
      f.seek(4+fsize+4,1)                     # the file pointer is initialised      
      bnd.appendCoreTimeSLF( t )
      
      # Extract relevant spectrum, where
      #  varsIndexes only contains the relevant nodes
      #  jvar varies from 0 to len(varsIndexes)
      jvar = 0
      for ivar in range(spe.NVAR):
         f.seek(4,1)                          # the file pointer advances through all records to keep on track
         if ivar in varsIndexes:
            z[jvar,:] = unpack(endian+str(spe.NPOIN2)+ftype,f.read(fsize*spe.NPOIN2))
            jvar += 1
         else:
            f.seek(fsize*spe.NPOIN2,1)            # the file pointer advances through all records to keep on track
         f.seek(4,1)

      # linear interpolation
      ivar = 0
      for bn,ln in support2d:
         d[:] = 0.
         for inod in range(len(bn)):
            jvar = np.where(varsIndexes == bn[inod])[0][0]
            d += ln[inod]*z[jvar,:]
         bnd.appendCoreVarsSLF( [d] )
         ivar += 1
      
      pbar.update(t)
   pbar.finish()

   # Close bndFile
   bnd.fole['hook'].close()

   print( '   +> writing out the file with coordinate to impose' )
   dat = [ str(len(BOR)) + ' 0' ]
   for i in np.sort(BOR):
      dat.append( str(i) + ' ' + repr(geo.MESHX[i-1]) + ' ' + repr(geo.MESHY[i-1]) + ' 0.0' )
   putFileContent(bndFile+'.dat',dat)
   
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print( '\n\nMy work is done\n\n' )

   sys.exit(0)
