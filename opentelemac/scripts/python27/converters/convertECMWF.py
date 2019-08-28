"""@author Sebastien E. Bourban
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
   Tools for handling conversions to-from ECMWF server files
"""
"""@details
   Contains server read functions to convert to SELAFIN file
"""
"""@history 12/12/2014 -- Sebastien E. Bourban
   Complete write-up of the script to produce 2D SELAFIN files.
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import path,listdir
from datetime import datetime,timedelta
import time
import math
import numpy as np
from scipy.io import netcdf
import urllib
import urllib2
import httplib
import traceback
from argparse import ArgumentParser,RawDescriptionHelpFormatter
# ~~> dependencies towards other pytel scripts
sys.path.append( path.join( path.dirname(sys.argv[0]), '..' ) )
# ~~> dependencies towards other modules
from parsers.parserSELAFIN import SELAFIN
from utils.progressbar import ProgressBar

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

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#
# When using the following user ID to access the ECMWF computing
#    facilities it is with the understanding that you acknowledge
#    the terms and conditions detailed at:
#    http://www.ecmwf.int/en/forecasts/software-and-tools
#
# username: s.bourban@hrwallingford.com
# password: Lxtc14
#
config = {
    "url"   : "https://api.ecmwf.int/v1",
    "key"   : "70f6a4499dddb7d17545f9bd3cf5ef3f",
    "email" : "s.bourban@hrwallingford.com",
    "password"   : "Lxtc14",
}
# ECMWF Re-Analysis, Keys to retrieve requests
#
#  'dataset' (those if general license), "interim" being the default:
#     era15  - ECMWF Global Reanalysis Data - ERA-15 (Jan 1979 - Dec 1993)
#     era20c  -	Reanalysis of the 20th-century using surface observations only (Jan 1900 - Dec 2010)
#     era20cmv0  -	ERA-20CM: Ensemble of climate model integrations (Experimental version)
#     era40  -	ECMWF Global Reanalysis Data - ERA-40 (Sep 1957 - Aug 2002)
#     eraclim  -	ERA-20CM: Ensemble of climate model integrations
#     icoads  -	ICOADS v2.5.1 with interpolated 20CR feedback
#     interim  -	ECMWF Global Reanalysis Data - ERA Interim (Jan 1979 - present)
#     ispd  -	ISPD v2.2
#     yotc  -	YOTC (Year of Tropical Convection)
#  'step', "6" being the default:
#     "24/to/120/by/24", ...
#  'number'  : "all",
#  'levtype' : "sl",
#  'date'    : "20071001",
#  'time'    : "00",
#  'origin'  : "all",
#  'type'    : "pf",
#  'param'   : "tp",
#  'area'    : "70/-130/30/-60",
#  'grid'    : "2/2",
#  'target'  : "data.grib" # wil not be used anymore ... since directly into SELAFIN
#

# _____                  ___________________________________________
# ____/ API Classes /__________________________________________/
#
# (C) Copyright 2012-2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.
#

try:
   import json
except:
   import simplejson as json

try:
   import pygrib
   importgrib = True
except:
   importgrib = False

class RetryError(Exception):
   def __init__(self, code, text):
      self.code = code
      self.text = text
   def __str__(self): return "%d %s" % (self.code, self.text)

class APIException(Exception):
   def __init__(self, value):
      self.value = value
   def __str__(self): return repr(self.value)

def robust(func):

   def wrapped(*args,**kwargs):
      tries = 0
      while True:
         try:
            return func(*args,**kwargs)
         except urllib2.HTTPError as e:
            print( "WARNING: httplib2.HTTPError received %s" % (str(e)) )
            if e.code < 500: raise
            tries += 1
            if tries > 10: raise
            time.sleep(60)
         except httplib.BadStatusLine as e:
            print( "WARNING: httplib.BadStatusLine received %s" % (str(e)) )
            tries += 1
            if tries > 10: raise
            time.sleep(60)
         except urllib2.URLError as e:
            print( "WARNING: httplib2.URLError received %s %s" % ( str(e.errno), str(e) ) )
            tries += 1
            if tries > 10: raise
            time.sleep(60)
         except APIException:
            raise
         except RetryError, e:
            print( "WARNING: HTTP received %s" & (str(e.code)) )
            print( e.text )
            tries += 1
            if tries > 10: raise
            time.sleep(60)
         except:
            print( "Unexpected error: %s" & (str( sys.exc_info()[0] )) )
            print( traceback.format_exc() )
            raise

   return wrapped

SAY = True
class Ignore303(urllib2.HTTPRedirectHandler):

   def redirect_request(self, req, fp, code, msg, headers, newurl):
      if code in [301, 302]:
         # We want the posts to work even if we are redirected
         if code == 301:
            global SAY, URL
            if SAY:
               o = req.get_full_url()
               n = newurl
               while o != URL and len(o) and len(n) and o[-1] == n[-1]:
                  o = o[0:-1]
                  n = n[0:-1]
               print()
               print( "*** ECMWF API has moved" )
               print( "***   OLD: %s" % (o) )
               print( "***   NEW: %s" % (n) )
               print( "*** Please update your ~/.ecmwfapirc file" )
               print()
               SAY = False
         data = None
         if req.has_data(): data = req.get_data()
         return urllib2.Request(newurl, data=data, headers = req.headers,
                                origin_req_host=req.get_origin_req_host(), unverifiable=True)
      return None

   def http_error_303(self, req, fp, code, msg, headers):
      infourl = urllib.addinfourl(fp, headers, req.get_full_url())
      infourl.status = code
      infourl.code = code
      return infourl

class Connection(object):

   def __init__(self, email = None, key = None, verbose = False, quiet = False):
      self.email    = email
      self.key      = key
      self.retry    = 5
      self.location = None
      self.done     = False
      self.value    = True
      self.offset   = 0
      self.verbose  = verbose
      self.quiet    = quiet
      self.status   = None

   @robust
   def call(self, url, payload = None, method = "GET"):

      if self.verbose: print( method +' '+ url )

      headers = { "Accept" : "application/json", "From" : self.email, "X-ECMWF-KEY" : self.key }

      opener = urllib2.build_opener(Ignore303)

      data = None
      if payload is not None:
         data = json.dumps(payload)
         data.encode('utf-8')
         headers["Content-Type"] = "application/json";

      url = "%s?offset=%d&limit=500" % (url, self.offset)
      req = urllib2.Request(url=url, data=data, headers=headers)
      if method:
         req.get_method = lambda: method

      error = False
      try:
         try:
            res  = opener.open(req)
         except urllib2.HTTPError as e:
            # It seems that some version of urllib2 are buggy
            if e.code <= 299: res = e
            else: raise
      except urllib2.HTTPError as e:
         print( repr(e) )
         error = True
         res   = e
         # 502: Proxy Error
         # 503: Service Temporarily Unavailable
         if e.code >= 500: raise RetryError(e.code, e.read())

      self.retry    = int(res.headers.get("Retry-After", self.retry))
      code          = res.code
      if code in [201, 202]: self.location = res.headers.get("Location",    self.location)

      if self.verbose:
         print( "Code " + code )
         print( "Content-Type " + res.headers.get("Content-Type") )
         print( "Content-Length " + res.headers.get("Content-Length") )
         print( "Location " + res.headers.get("Location") )

      body = res.read()
      res.close()

      if code in [204]:
         self.last = None
         return None
      else:
         try:
            self.last  =  json.loads(body)
         except Exception as e:
            self.last = { "error" : "%s: %s" % (e, body) }
            error = True

      if self.verbose: print( repr(json.dumps(self.last,indent=4)) )

      self.status = self.last.get("status", self.status)

      if self.verbose: print( "Status " + self.status )

      if "messages" in self.last:
         for n in self.last["messages"]:
            if not self.quiet: print( n )
            self.offset += 1

      if code == 200 and self.status == "complete":
         self.value = self.last
         self.done  = True
         if isinstance(self.value, dict) and "result" in self.value: self.value = self.value["result"]

      if code in [303]:
         self.value = self.last
         self.done  = True

      if "error" in self.last: raise APIException("ecmwf.API error 1: %s" % (self.last["error"],) )

      if error: raise APIException("ecmwf.API error 2: %s" % (res, ) )

      return self.last

   def submit(self, url, payload): self.call(url, payload, "POST")

   def POST(self, url, payload): return self.call(url, payload, "POST")

   def GET(self, url): return self.call(url, None, "GET")

   def wait(self):
      if self.verbose: print( "Sleeping %s second(s)" % (str(self.retry))  )
      time.sleep(self.retry)
      self.call(self.location, None, "GET")

   def ready(self): return self.done

   def result(self): return self.value

   def cleanup(self):
      try:
         if self.location: self.call(self.location, None, "DELETE")
      except:
         pass

# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#

class ECMWF():

   def __init__(self,dates,request):

      # ~> Initialisation
      self.moddates = dates
      self.request = request
      # ~~> inheritence
      self.slf2d = SELAFIN('')
      self.slf2d.TITLE = ''
      self.slf2d.fole = {}
      

   def connectToECMWF(self,dataset):

      status = ''
      # ~> Establish connection
      self.connection = Connection(config['email'], config['key'], quiet = True, verbose = False)
      # ~> Verify connection
      user = self.connection.call("%s/%s" % (config['url'], "who-am-i"))
      print( '   ~> access through username: %s\n' % (user["full_name"] or "user '%s'" % user["uid"],) )
      # ~> Request dataset
      self.connection.submit("%s/%s/requests" % (config['url'], dataset), self.request)
      status = self.connection.status
      print( '   ~> request has been ' + status )
      # ~> Wait for remote processing
      while not self.connection.ready():
         if status != self.connection.status:
            status = self.connection.status
            print( '   ~> request remains ' + status + '...' )
         self.connection.wait()
      # ~> Request completed
      print( '   ~> request is now ' + self.connection.status )
      self.connection.cleanup()

   def downloadECMWF(self):

      result = self.connection.result()
      fileName = self.request.get("target")

      # ~> tries connecting 3 times before stopping
      tries = 0
      while True:

         # ~> downloading file by blocks
         http = urllib2.urlopen(result["href"])
         f = open(fileName,"wb")
         ibar = 0; pbar = ProgressBar(maxval=result["size"]).start()
         while True:
            chunk = http.read(1024*1024)
            if not chunk: break
            f.write(chunk)
            ibar += len(chunk)
            pbar.update(ibar)
         f.flush()
         f.close()
         pbar.finish()
         # ~> have I got everything ?
         if ibar == result["size"]: break
         if tries == 3:
            print( "    ... exhausted the number of download trials.\nYou may wish to attempt this again later." )
            sys.exit()
         print( "    ... trying to download the data once more ..." )
         tries += 1

   def openECMWF(self):
      
      self.ecmwfdata = netcdf.netcdf_file(self.request.get("target"), 'r')

      # ~~~~ Time records ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ATs = self.ecmwfdata.variables['time'][:]-70*365.25*24
      AT0 = datetime.fromtimestamp(ATs[0]*3600.)
      self.slf2d.DATETIME = [ d for d in AT0.timetuple()[0:6] ]
      self.slf2d.tags = { 'times': 3600 * ( ATs-ATs[0] ) } # time record in hours
      
   def closeECMWF(self): self.slf2d.fole['hook'].close()

   def setGeometry(self):

      # ~~> 2D grid
      print '   +> set the mesh and connectivity'
      x = self.ecmwfdata.variables['longitude'][:]
      self.NX1D = len(x)
      y = self.ecmwfdata.variables['latitude'][:]
      self.NY1D = len(y)
      # ~~> TODO: a more complicated check should be done here with a mix of entries
      x1,y1,x2,y2 = self.request['area'].split('/')
      if float(x1) < 0.: x1 = float(x1) + 360.0
      if float(x2) < 0.: x2 = float(x2) + 360.0
      self.MASKX = np.logical_and( float(x1)<=x[0],x[0]<=float(x2) )
      if not np.any(self.MASKX):
         print( '... your spatial range seems out of bound:\n       you asked for [ '+x1+' - '+x2+' ], while x is:\n       '+repr(x)+'\n\n' )
         sys.exit(1)
      self.MASKY = np.logical_and( float(y1)<=y.T[0],y.T[0]<=float(y2) )
      if not np.any(self.MASKY):
         print( '... your spatial range seems out of bound:\n       you asked for [ '+y1+' - '+y2+' ], while y is:\n       '+repr(y)+'\n\n' )
         sys.exit(1)
         
      self.slf2d.MESHX = np.tile(x,self.NY1D).reshape(self.NY1D,self.NX1D).T.ravel()
      self.slf2d.MESHY = np.tile(y,self.NX1D)
      
      self.slf2d.NPLAN = 1
      self.slf2d.NDP2 = 3
      self.slf2d.NDP3 = self.slf2d.NDP2
      self.slf2d.NPOIN2 = self.NX1D * self.NY1D
      self.slf2d.NPOIN3 = self.slf2d.NPOIN2
      self.slf2d.NELEM2 = 2*( self.NX1D-1 )*( self.NY1D-1 )
      self.slf2d.NELEM3 = self.slf2d.NELEM2
      # ~~> lat,lon correction
      for i in range(self.slf2d.NPOIN2):
         if( self.slf2d.MESHX[i] > 180 ): self.slf2d.MESHX[i] = self.slf2d.MESHX[i] - 360.0
      #for i in range(2172,self.NY1D):
      #   self.slf2d.MESHY[i] = 47.0 + ( i-2172 )/18.0

      # ~~> Connectivity
      ielem = 0; pbar = ProgressBar(maxval=self.slf2d.NELEM3).start()
      self.slf2d.IKLE3 = np.zeros((self.slf2d.NELEM3,self.slf2d.NDP3),dtype=np.int)
      for i in range(1,self.NX1D):
         for j in range(1,self.NY1D):
            ipoin = (i-1)*self.NY1D + j - 1
            # ~~> first triangle
            self.slf2d.IKLE3[ielem][0] = ipoin
            self.slf2d.IKLE3[ielem][1] = ipoin + self.NY1D
            self.slf2d.IKLE3[ielem][2] = ipoin + 1
            ielem = ielem + 1
            pbar.update(ielem)
            # ~~> second triangle
            self.slf2d.IKLE3[ielem][0] = ipoin + self.NY1D
            self.slf2d.IKLE3[ielem][1] = ipoin + self.NY1D + 1
            self.slf2d.IKLE3[ielem][2] = ipoin + 1
            ielem = ielem + 1
            pbar.update(ielem)
      pbar.finish()

      # ~~> Boundaries
      pbar = ProgressBar(maxval=self.NX1D+self.NY1D).start()
      self.slf2d.IPOB3 = np.zeros(self.slf2d.NPOIN3,dtype=np.int)
      # ~~> along the x-axis (lon)
      for i in range(self.NX1D):
         ipoin = i*self.NY1D
         self.slf2d.IPOB3[ipoin] = i + 1
         ipoin = i*self.NY1D -1
         self.slf2d.IPOB3[ipoin] = 2*self.NX1D+(self.NY1D-2) - i
         pbar.update(i)
      # ~~> along the y-axis (alt)
      for i in range(1,self.NY1D):
         ipoin = i
         self.slf2d.IPOB3[ipoin] = 2*self.NX1D + 2*(self.NY1D-2) -i + 1
         ipoin = self.NY1D*(self.NX1D-1) + i
         self.slf2d.IPOB3[ipoin] = self.NX1D + i
         pbar.update(i+self.NX1D)
      pbar.finish()

      # ~~> Boundary points             NPLAN,              NPTFR,   DATE
      self.slf2d.IPARAM = [ 0,0,0,0,0,0,    0,  2*self.NX1D+2*(self.NY1D-2),0,    1 ]


   def putGeometry(self,fileName):
            
      print '   +> writing up the geometry file'

      self.slf2d.fole = {}
      self.slf2d.fole.update({ 'hook': open(fileName,'wb') })
      self.slf2d.fole.update({ 'name': fileName })
      self.slf2d.fole.update({ 'endian': ">" })     # big endian
      self.slf2d.fole.update({ 'float': ('f',4) })  # single precision

      self.slf2d.VARNAMES = ['RANGE          ']
      self.slf2d.VARUNITS = ['UI             ']
      self.slf2d.NBV1 = len(self.slf2d.VARNAMES)
      self.slf2d.NVAR = self.slf2d.NBV1
      self.slf2d.VARINDEX = range(self.slf2d.NVAR)
         
      print( '       - Write SELAFIN geometry' )
      self.slf2d.appendHeaderSLF()
      
      print( '       - Write SELAFIN core' )
      varof = self.ecmwfdata.variables['d2fd'].add_offset
      varsf = self.ecmwfdata.variables['d2fd'].scale_factor
      var2d = np.zeros((self.NX1D,self.NY1D),dtype=np.float)
      ibar = 0
      pbar = ProgressBar(maxval=len(self.slf2d.tags['times'])).start()
      for t in range(len(self.slf2d.tags['times'])):
         self.slf2d.appendCoreTimeSLF(t)
         z = 10 ** ( varsf * np.swapaxes(np.swapaxes(self.ecmwfdata.variables['d2fd'][t],1,3),0,2) + varof )
         for j in range( self.NY1D ):
            for i in range( self.NX1D ):
               var2d[i,j] = max(z[j][i].ravel())
         self.slf2d.appendCoreVarsSLF([var2d.ravel()])
         ibar += 1
         pbar.update(ibar)
      pbar.finish()

      self.slf2d.fole['hook'].close()


   def setSpectral(self):
      
      print '   +> reseting the header of the spectral file'

      print '      - read the spectra definition'
      self.numberOfFrequencies = len( self.ecmwfdata.variables['frequency'][:] )
      self.numberOfDirections = len( self.ecmwfdata.variables['direction'][:] )
      self.freq = [ 0.035, 0.038, 0.042, 0.046, 0.051, 0.056, 0.061, 0.067, 0.074, 0.081, 
                    0.09,  0.098, 0.108, 0.119, 0.131, 0.144, 0.159, 0.174, 0.192, 0.211,
                    0.232, 0.255, 0.281, 0.309, 0.34,  0.374, 0.411, 0.453, 0.498, 0.548 ]
      self.dirc = 7.5 + 15.*np.arange(self.numberOfDirections) - 7.5    #  /!? only for TOMAWC to work
      
      # ~~> sizes (spectral numbers)
      self.slf2d.NPLAN = 1
      self.slf2d.NDP2 = 4
      self.slf2d.NDP3 = self.slf2d.NDP2
      self.slf2d.NPOIN2 = self.numberOfDirections * self.numberOfFrequencies
      self.slf2d.NPOIN3 = self.slf2d.NPOIN2
      self.slf2d.NELEM2 = self.numberOfDirections * ( self.numberOfFrequencies-1 )
      self.slf2d.NELEM3 = self.slf2d.NELEM2
      self.slf2d.NPTFR  = 2*self.numberOfDirections
      self.slf2d.IPARAM = [ 0,0,0,0,0,0,    0,   2*self.numberOfDirections,0,1 ]

      # ~~> 2D grid (spectral grid) - TODO: use numpy here !
      self.slf2d.MESHX = np.zeros(self.slf2d.NPOIN2,dtype=np.float)
      self.slf2d.MESHY = np.zeros(self.slf2d.NPOIN2,dtype=np.float)
      print '      - set the mesh'
      ipoin = 0; pbar = ProgressBar(maxval=self.slf2d.NPOIN2).start()
      for jf in range(self.numberOfFrequencies):
         for ii in range(self.numberOfDirections):
            self.slf2d.MESHX[ii+self.numberOfDirections*jf] = self.freq[jf]*math.sin(math.pi*self.dirc[ii]/180.)
            self.slf2d.MESHY[ii+self.numberOfDirections*jf] = self.freq[jf]*math.cos(math.pi*self.dirc[ii]/180.)
            ipoin += 1
            pbar.update(ipoin)
      pbar.finish()

      # ~~> Connectivity - TODO: use numpy here !
      print '      - set the connectivity'
      ielem = 0; pbar = ProgressBar(maxval=self.slf2d.NELEM3).start()
      self.slf2d.IKLE3 = np.zeros((self.slf2d.NELEM3,self.slf2d.NDP3),dtype=np.int)
      for jf in range(self.numberOfFrequencies-1):
         for ii in range(self.numberOfDirections):
            self.slf2d.IKLE3[ielem][0] = (ii+1) % self.numberOfDirections + jf*self.numberOfDirections
            ielem += 1
      for ielem in range(self.slf2d.NELEM3):
         self.slf2d.IKLE3[ielem][1] = ielem
         self.slf2d.IKLE3[ielem][2] = ielem + self.numberOfDirections
         self.slf2d.IKLE3[ielem][3] = self.slf2d.IKLE3[ielem][0] + self.numberOfDirections
         pbar.update(ielem)
      pbar.finish()

      # ~~> Boundaries - TODO: use numpy here !
      self.slf2d.IPOB3 = np.zeros(self.slf2d.NPOIN3,dtype=np.int)
      # ~~> along the ?-axis 
      for ii in range(self.numberOfDirections):
         self.slf2d.IPOB3[ii] = ii
      for ii in range(self.numberOfDirections,2*self.numberOfDirections):
         self.slf2d.IPOB3[ii] = self.numberOfDirections + self.numberOfDirections * self.numberOfFrequencies - ii


   def appendHeaderECMWF(self):

      self.slf2d.VARNAMES = []
      self.slf2d.VARUNITS = []      
      # ~~> variables
      self.slf2d.TITLE = ''
      if self.typ == 'wave':
         self.slf2d.VARNAMES = ['WAVE HEIGHT     ', \
            'WAVE PERIOD     ','WAVE DIRECTION  ']
         self.slf2d.VARUNITS = ['M               ', \
            'S               ','DEGREES         ']
      elif self.typ == 'spec':
         for i in range( self.NX1D * self.NY1D ):
            self.slf2d.VARNAMES.append(('F PT '+str(i+1)+'                ')[:16])
            self.slf2d.VARUNITS.append('UI              ')
         print '    - from ',self.slf2d.VARNAMES[0],' to ',self.slf2d.VARNAMES[-1]
      else:
         self.slf2d.VARNAMES = ['SURFACE PRESSURE', \
            'WIND VELOCITY U ','WIND VELOCITY V ', \
            'AIR TEMPERATURE ']
         self.slf2d.VARUNITS = ['UI              ', \
            'M/S             ','M/S             ', \
            'DEGREES         ']
      self.slf2d.NBV1 = len(self.slf2d.VARNAMES)
      self.slf2d.NVAR = self.slf2d.NBV1
      self.slf2d.VARINDEX = range(self.slf2d.NVAR)

      self.slf2d.appendHeaderSLF()

   def appendCoreTimeECMWF(self,t): self.slf2d.appendCoreTimeSLF(t)

   def appendCoreVarsECMWF(self,itime):
      # Note: this is how you get to the attributes ...
      # ecmwfdata.variables['sp'].ncattrs()
      # in particular ...
      # ecmwfdata.variables['sp'].units
      # ecmwfdata.variables['sp'].missing_value

      if self.typ == 'wave':
         # ~~> WAVE HEIGHT == 'swh'
         var2d = np.swapaxes( self.ecmwfdata.variables['swh'][itime][:], 0,1).ravel()
         varof = self.ecmwfdata.variables['swh'].add_offset
         varsf = self.ecmwfdata.variables['swh'].scale_factor
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> SIGNIFICANT WAVE PERIOD == 'mwp'
         var2d = np.swapaxes( self.ecmwfdata.variables['mwp'][itime][:], 0,1).ravel()
         varof = self.ecmwfdata.variables['mwp'].add_offset
         varsf = self.ecmwfdata.variables['mwp'].scale_factor
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> MEAN WAVE DIRECTION == 'mwd'
         var2d = np.swapaxes( self.ecmwfdata.variables['mwd'][itime][:], 0,1).ravel()
         varof = self.ecmwfdata.variables['mwd'].add_offset
         varsf = self.ecmwfdata.variables['mwd'].scale_factor
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

      elif self.typ == 'spec':
         varof = self.ecmwfdata.variables['d2fd'].add_offset
         varsf = self.ecmwfdata.variables['d2fd'].scale_factor
         z = 10 ** ( varsf * np.swapaxes(np.swapaxes(self.ecmwfdata.variables['d2fd'][itime],1,3),0,2) + varof )
         for j in range( self.NY1D ):
            for i in range( self.NX1D ):
               self.slf2d.appendCoreVarsSLF([z[j][i].ravel()])

      else:
         # ~~> SURFACE PRESSURE == 'sp'
         var2d = np.swapaxes( self.ecmwfdata.variables['sp'][itime][:], 0,1).ravel()
         varof = self.ecmwfdata.variables['sp'].add_offset
         varsf = self.ecmwfdata.variables['sp'].scale_factor
         #print( self.ecmwfdata.variables['sp'].units )
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> WIND VELOCITY U == 'u10'
         var2d = np.swapaxes( self.ecmwfdata.variables['u10'][itime][:], 0,1).ravel()
         varof = self.ecmwfdata.variables['u10'].add_offset
         varsf = self.ecmwfdata.variables['u10'].scale_factor
         #print( self.ecmwfdata.variables['u10'].units )
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> WIND VELOCITY V == 'v10'
         var2d = np.swapaxes( self.ecmwfdata.variables['v10'][itime][:], 0,1).ravel()
         varof = self.ecmwfdata.variables['v10'].add_offset
         varsf = self.ecmwfdata.variables['v10'].scale_factor
         #print( self.ecmwfdata.variables['v10'].units )
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof])

         # ~~> AIR TEMPERATURE == 't2m'
         var2d = np.swapaxes( self.ecmwfdata.variables['t2m'][itime][:], 0,1).ravel()
         varof = self.ecmwfdata.variables['t2m'].add_offset
         varsf = self.ecmwfdata.variables['t2m'].scale_factor
         self.slf2d.appendCoreVarsSLF([varsf*var2d+varof-273.15])  # Kelvin to Celsius


   def putContent(self,fileName,stream,showbar=True):

      # ~~> netcdf reader
      self.typ = stream
      
      if self.typ == 'spec':
         # ~~> new SELAFIN writer
         self.slf2d.fole = {}
         self.slf2d.fole.update({ 'hook': open(fileName,'wb') })
         self.slf2d.fole.update({ 'name': fileName })
         self.slf2d.fole.update({ 'endian': ">" })     # big endian
         self.slf2d.fole.update({ 'float': ('f',4) })  # single precision

         print( '     +> Write SELAFIN header' )
         self.appendHeaderECMWF()

         print( '     +> Write SELAFIN core' )
         ibar = 0
         if showbar: pbar = ProgressBar(maxval=len(self.slf2d.tags['times'])).start()
         for t in range(len(self.slf2d.tags['times'])):
            self.appendCoreTimeECMWF(t)
            self.appendCoreVarsECMWF(ibar)
            ibar += 1
            if showbar: pbar.update(ibar)
         self.slf2d.fole['hook'].close()
         if showbar: pbar.finish()
      
      else:
         # ~~> inheritence
         #self.slf2d = SELAFIN('')     # surface
         #self.slf2d.DATETIME = self.moddates[0]

         # ~~> new SELAFIN writer
         self.slf2d.fole = {}
         self.slf2d.fole.update({ 'hook': open(fileName,'wb') })
         self.slf2d.fole.update({ 'name': fileName })
         self.slf2d.fole.update({ 'endian': ">" })     # big endian
         self.slf2d.fole.update({ 'float': ('f',4) })  # single precision

         print( '     +> Write SELAFIN header' )
         self.appendHeaderECMWF()

         print( '     +> Write SELAFIN core' )
         ibar = 0
         if showbar: pbar = ProgressBar(maxval=len(self.slf2d.tags['times'])).start()
         for t in range(len(self.slf2d.tags['times'])):
            self.appendCoreTimeECMWF(t)
            self.appendCoreVarsECMWF(ibar)
            ibar += 1
            if showbar: pbar.update(ibar)
         self.slf2d.fole['hook'].close()
         if showbar: pbar.finish()


# _____                  ___________________________________________
# ____/ Primary Classes /__________________________________________/
#

class GRIB():

   def __init__(self,dataset,request,stream):

      self.request = request
      # ~~> inheritence
      self.slf2d = SELAFIN('')
      self.slf2d.TITLE = ''
      self.slf2d.fole = {}
      self.dataset = []
      self.variables = []

      print '   +> identifying relevant files, by variables'
      # ~~> filter requested variables
      self.variables = []
      foundDataset = []
      ibar = 0; pbar = ProgressBar(maxval=len(dataset)).start()
      for d in dataset:
         grbs = pygrib.open(d)
         for grb in grbs:
            if str(grb.indicatorOfParameter) in request['param'].split('/'):
               if d not in foundDataset: foundDataset.append(d)
               if grb.indicatorOfParameter not in self.variables: self.variables.append(grb.indicatorOfParameter)
               else: break
         grbs.close()
         ibar += 1
         pbar.update(ibar)
      pbar.finish()
      if self.variables == []:
         print( '... could not find the requested valiables.\n\n' )
         sys.exit(1)

      print '   +> sorting out timeline'
      # ~~>  checking consistency of origin of date and time
      for d in foundDataset:
         grbs = pygrib.open(d)
         for grb in grbs:
            AT0 = pygrib.julian_to_datetime(grb.julianDay)
            break
         break
      print '      - start date and time',AT0
      self.slf2d.DATETIME = [ d for d in AT0.timetuple()[0:6] ]
      # ~~>  recording times from origin of date and time
      ATs = []
      DTs = []
      ibar = 0; pbar = ProgressBar(maxval=len(foundDataset)).start()
      for d in foundDataset:
         grbs = pygrib.open(d)
         for grb in grbs:
            c = str(grb.validityDate)
            ATs.append( datetime( int(c[:4]),int(c[4:6]),int(c[6:]) ) + timedelta( seconds=int(grb.validityTime*36) ) )
            DTs.append( (ATs[-1]-AT0).total_seconds())
            break
         ibar += 1
         pbar.update(ibar)
      pbar.finish()
      print '      - finish date and time',ATs[-1]
      # ~~>  checking if the list is sorted
      if not all( ATs[i] < ATs[i+1] for i in range(len(ATs)-1) ):
         print( '... your dataset is not sorted. Here is the time profile in seconds:\n'+repr(DTs)+'\n\n' )
         sys.exit(1)
      # ~~> filter requested times
      udates = [ datetime(*[int(a) for a in d.split('-')]) for d in request['date'].split('/to/') ]
      self.slf2d.tags = { 'times': [] }
      udates[1] = udates[1]+timedelta(hours=24.)
      for i in range(len(ATs)):
         if udates[0] <= ATs[i] and ATs[i] <= udates[1]:
            self.slf2d.tags['times'].append(DTs[i])
            self.dataset.append(foundDataset[i])
      print '   +> actual timeline'
      print '      - start date and time  ',AT0+timedelta(seconds=self.slf2d.tags['times'][0])
      print '      - finish date and time ',AT0+timedelta(seconds=self.slf2d.tags['times'][-1])
      
      # ~> Other initialisations
      self.typ = stream

      # ~~> spatial sizes
      print '   +> checking out sizes'
      grbs = pygrib.open(self.dataset[0])
      for grb in grbs:
         self.missingValue = grb.missingValue
         self.scaleValuesBy = grb.scaleValuesBy  # TODO: use it
         self.offset = grb.offset                # TODO: use it
         #self.NX1D = grb.Nj
         #self.NY1D = grb.Ni
         break
      grbs.close()


   def openGRIB(self,fileName):
      
      self.slf2d.fole.update({ 'hook': open(fileName,'wb') })
      self.slf2d.fole.update({ 'name': fileName })
      self.slf2d.fole.update({ 'endian': ">" })     # big endian
      self.slf2d.fole.update({ 'float': ('f',4) })  # single precision

   def closeGRIB(self): self.slf2d.fole['hook'].close()

   def setGeometry(self):

      # ~~> header
      self.byrowdown = False

      # ~~> 2D grid
      print '   +> set the mesh and connectivity'
      x1,y1,x2,y2 = self.request['area'].split('/')
      grbs = pygrib.open(self.dataset[0])
      for grb in grbs:
         y,x = grb.latlons()
         self.MASKX = np.logical_and( float(x1)<=x[0],x[0]<=float(x2) )
         lx = x[0][self.MASKX]
         if not np.any(self.MASKX):
            self.MASKX = np.logical_and( float(x1)<=x[0]-360.,x[0]-360.<=float(x2) )
            lx = x[0][self.MASKX]-360.
         if not np.any(self.MASKX):
            print( '... your spatial range seems out of bound:\n       you asked for [ '+x1+' - '+x2+' ], while x is:\n       '+repr(x)+'\n\n' )
            sys.exit(1)
         self.NX1D = len(lx)
         self.MASKY = np.logical_and( float(y1)<=y.T[0],y.T[0]<=float(y2) )
         ly = y.T[0][self.MASKY]
         if not np.any(self.MASKY):
            print( '... your spatial range seems out of bound:\n       you asked for [ '+y1+' - '+y2+' ], while y is:\n       '+repr(y)+'\n\n' )
            sys.exit(1)
         self.NY1D = len(ly)
         if self.byrowdown:
            self.slf2d.MESHX = np.ravel(np.tile(lx,self.NY1D).reshape(self.NY1D,self.NX1D))
            self.slf2d.MESHY = np.ravel(np.tile(ly,self.NX1D).reshape(self.NX1D,self.NY1D).T)
         else:
            self.slf2d.MESHX = np.ravel(np.tile(lx,self.NY1D).reshape(self.NY1D,self.NX1D).T)
            self.slf2d.MESHY = np.ravel(np.tile(ly,self.NX1D).reshape(self.NX1D,self.NY1D))
         break
      grbs.close()

      self.slf2d.NPLAN = 1
      self.slf2d.NDP2 = 3
      self.slf2d.NDP3 = self.slf2d.NDP2
      self.slf2d.NPOIN2 = self.NX1D * self.NY1D
      self.slf2d.NPOIN3 = self.slf2d.NPOIN2
      self.slf2d.NELEM2 = 2*( self.NX1D-1 )*( self.NY1D-1 )
      self.slf2d.NELEM3 = self.slf2d.NELEM2

      # ~~> Connectivity - numbered by rows
      ielem = 0; pbar = ProgressBar(maxval=self.slf2d.NELEM3).start()
      self.slf2d.IKLE3 = np.zeros((self.slf2d.NELEM3,self.slf2d.NDP3),dtype=np.int)
      if self.byrowdown:
         for j in range(1,self.NY1D):
            for i in range(1,self.NX1D):
               ipoin = (j-1)*self.NX1D + i - 1
               # ~~> first triangle
               self.slf2d.IKLE3[ielem][0] = ipoin
               self.slf2d.IKLE3[ielem][1] = ipoin + self.NX1D
               self.slf2d.IKLE3[ielem][2] = ipoin + 1
               ielem = ielem + 1
               pbar.update(ielem)
               # ~~> second triangle
               self.slf2d.IKLE3[ielem][0] = ipoin + self.NX1D
               self.slf2d.IKLE3[ielem][1] = ipoin + self.NX1D + 1
               self.slf2d.IKLE3[ielem][2] = ipoin + 1
               ielem = ielem + 1
               pbar.update(ielem)
      else:
         for j in range(1,self.NY1D):
            for i in range(1,self.NX1D):
               ipoin = j - 1 + (i- 1)*self.NY1D
               # ~~> first triangle
               self.slf2d.IKLE3[ielem][0] = ipoin
               self.slf2d.IKLE3[ielem][1] = ipoin + 1
               self.slf2d.IKLE3[ielem][2] = ipoin + self.NY1D
               ielem = ielem + 1
               pbar.update(ielem)
               # ~~> second triangle
               self.slf2d.IKLE3[ielem][0] = ipoin + self.NY1D
               self.slf2d.IKLE3[ielem][1] = ipoin + 1
               self.slf2d.IKLE3[ielem][2] = ipoin + self.NY1D + 1
               ielem = ielem + 1
               pbar.update(ielem)
      pbar.finish()

      # ~~> Boundaries
      self.slf2d.IPOB3 = np.zeros(self.slf2d.NPOIN3,dtype=np.int)

      if self.byrowdown:
         # ~~> around the box
         for i in range(self.NX1D):
            ipoin = i
            self.slf2d.IPOB3[i] = ipoin
         for i in range(self.NX1D):
            ipoin = self.NX1D + self.NY1D - 2 + i
            self.slf2d.IPOB3[self.NX1D*self.NY1D-i-1] = ipoin
         for j in range(1,self.NY1D-1):
            ipoin = j + self.NX1D - 1
            self.slf2d.IPOB3[(j+1)*self.NX1D-1] = ipoin
         for j in range(1,self.NY1D-1):
            ipoin = self.NY1D + 2 * self.NX1D + j - 3
            self.slf2d.IPOB3[self.NX1D*self.NY1D-j*self.NX1D-self.NX1D] = ipoin
      else:
         # ~~> around the box
         for j in range(self.NY1D):
            ipoin = j
            self.slf2d.IPOB3[j] = ipoin
         for j in range(self.NY1D):
            ipoin = self.NY1D + self.NX1D - 2 + j
            self.slf2d.IPOB3[self.NY1D*self.NX1D-j-1] = ipoin
         for i in range(1,self.NX1D-1):
            ipoin = i + self.NY1D - 1
            self.slf2d.IPOB3[(i+1)*self.NY1D-1] = ipoin
         for i in range(1,self.NX1D-1):
            ipoin = self.NX1D + 2 * self.NY1D + i - 3
            self.slf2d.IPOB3[self.NY1D*self.NX1D-i*self.NY1D-self.NY1D] = ipoin

      # ~~> Boundary points             NPLAN,              NPTFR,   DATE
      self.slf2d.IPARAM = [ 0,0,0,0,0,0,    0,  2*self.NX1D+2*(self.NY1D-2),0,    1 ]

   def putGeometry(self,fileName):
            
      print '   +> writing up the geometry file'

      self.slf2d.fole = {}
      self.slf2d.fole.update({ 'hook': open(fileName,'wb') })
      self.slf2d.fole.update({ 'name': fileName })
      self.slf2d.fole.update({ 'endian': ">" })     # big endian
      self.slf2d.fole.update({ 'float': ('f',4) })  # single precision

      self.slf2d.VARNAMES = ['RANGE          ']
      self.slf2d.VARUNITS = ['UI             ']
      self.slf2d.NBV1 = len(self.slf2d.VARNAMES)
      self.slf2d.NVAR = self.slf2d.NBV1
      self.slf2d.VARINDEX = range(self.slf2d.NVAR)
         
      print( '       - Write SELAFIN header' )
      self.slf2d.appendHeaderSLF()
      
      # ~~> A few more number and a spectral template for input/output
      grbs = pygrib.open(self.dataset[0])
      for grb in grbs:
         numberOfDirections = grb.numberOfDirections
         numberOfFrequencies = grb.numberOfFrequencies
         break
      grbs.close()
      spec = np.zeros((numberOfDirections,numberOfFrequencies,self.NX1D,self.NY1D),dtype=np.float)
      var = np.zeros((self.NX1D,self.NY1D),dtype=np.float)

      print( '       - Write SELAFIN core' )
      ibar = 0; pbar = ProgressBar(maxval=len(self.slf2d.tags['times'])).start()
      for itime in range(len(self.slf2d.tags['times'])):
      
         self.slf2d.appendCoreTimeSLF(self.slf2d.tags['times'][itime])
         grbs = pygrib.open(self.dataset[itime])
         for grb in grbs:
            ii = 0
            data = grb.values.data
            data[np.where( np.absolute(data) <= 0.001 )] = np.nan
            data[np.where( data == self.missingValue )] = np.nan
            data = 10. ** data
            data[np.isnan( data )] = 0.
            for iy in range(len(self.MASKY)):
               if self.MASKY[iy]:
                  spec[grb.directionNumber-1,grb.frequencyNumber-1,:,ii] = data[iy][self.MASKX]
                  ii += 1
         grbs.close()

         ibar += 1
         pbar.update(ibar)
         for ix in range(self.NX1D):
            for iy in range(self.NY1D):
               var[ix,iy] = max(spec[:,:,ix,iy].ravel())-min(spec[:,:,ix,iy].ravel())
         self.slf2d.appendCoreVarsSLF([var.ravel()])
         
      pbar.finish()
      self.slf2d.fole['hook'].close()


   def setSpectral(self):
      
      print '   +> reseting the header of the spectral file'

      print '      - read the spectra definition'
      grbs = pygrib.open(self.dataset[0])
      for grb in grbs:
         self.numberOfDirections = grb.numberOfDirections
         self.numberOfFrequencies = grb.numberOfFrequencies
         self.freq = np.asarray( grb.scaledFrequencies, dtype=np.float ) / grb.frequencyScalingFactor
         self.dirc = np.asarray( grb.scaledDirections, dtype=np.float ) / grb.directionScalingFactor - 7.5    #  /!? only so that TOMAWAC works
         break
      grbs.close()

      # ~~> sizes (spectral numbers)
      self.slf2d.NPLAN = 1
      self.slf2d.NDP2 = 4
      self.slf2d.NDP3 = self.slf2d.NDP2
      self.slf2d.NPOIN2 = self.numberOfDirections * self.numberOfFrequencies
      self.slf2d.NPOIN3 = self.slf2d.NPOIN2
      self.slf2d.NELEM2 = self.numberOfDirections * ( self.numberOfFrequencies-1 )
      self.slf2d.NELEM3 = self.slf2d.NELEM2
      self.slf2d.NPTFR  = 2*self.numberOfDirections
      self.slf2d.IPARAM = [ 0,0,0,0,0,0,    0,   2*self.numberOfDirections,0,1 ]

      # ~~> 2D grid (spectral grid) - TODO: use numpy here !
      self.slf2d.MESHX = np.zeros(self.slf2d.NPOIN2,dtype=np.float)
      self.slf2d.MESHY = np.zeros(self.slf2d.NPOIN2,dtype=np.float)
      print '      - set the mesh'
      ipoin = 0; pbar = ProgressBar(maxval=self.slf2d.NPOIN2).start()
      for jf in range(self.numberOfFrequencies):
         for ii in range(self.numberOfDirections):
            self.slf2d.MESHX[ii+self.numberOfDirections*jf] = self.freq[jf]*math.sin(math.pi*self.dirc[ii]/180.)
            self.slf2d.MESHY[ii+self.numberOfDirections*jf] = self.freq[jf]*math.cos(math.pi*self.dirc[ii]/180.)
            ipoin += 1
            pbar.update(ipoin)
      pbar.finish()

      # ~~> Connectivity - TODO: use numpy here !
      print '      - set the connectivity'
      ielem = 0; pbar = ProgressBar(maxval=self.slf2d.NELEM3).start()
      self.slf2d.IKLE3 = np.zeros((self.slf2d.NELEM3,self.slf2d.NDP3),dtype=np.int)
      for jf in range(self.numberOfFrequencies-1):
         for ii in range(self.numberOfDirections):
            self.slf2d.IKLE3[ielem][0] = (ii+1) % self.numberOfDirections + jf*self.numberOfDirections
            ielem += 1
      for ielem in range(self.slf2d.NELEM3):
         self.slf2d.IKLE3[ielem][1] = ielem
         self.slf2d.IKLE3[ielem][2] = ielem + self.numberOfDirections
         self.slf2d.IKLE3[ielem][3] = self.slf2d.IKLE3[ielem][0] + self.numberOfDirections
         pbar.update(ielem)
      pbar.finish()

      # ~~> Boundaries - TODO: use numpy here !
      pbar = ProgressBar(maxval=self.NX1D+self.NY1D).start()
      self.slf2d.IPOB3 = np.zeros(self.slf2d.NPOIN3,dtype=np.int)
      # ~~> along the ?-axis 
      for ii in range(self.numberOfDirections):
         self.slf2d.IPOB3[ii] = ii
      for ii in range(self.numberOfDirections,2*self.numberOfDirections):
         self.slf2d.IPOB3[ii] = self.numberOfDirections + self.numberOfDirections * self.numberOfFrequencies - ii
      pbar.finish()


   def appendHeaderGRIB(self):
      
      self.slf2d.VARNAMES = []
      self.slf2d.VARUNITS = []
      if self.typ == 'wave':
         # TODO: codes for waves
         print '... waves, not coded yet'
         sys.exit(1)
         self.slf2d.VARNAMES = ['WAVE HEIGHT     ', \
            'WAVE PERIOD     ','WAVE DIRECTION  ']
         self.slf2d.VARUNITS = ['M               ', \
            'S               ','DEGREES         ']
         for var in self.slf2d.VARNAMES: print '    - ',var
      elif self.typ == 'oper':
         for i in self.variables:
            if 151 == i:
               self.slf2d.VARNAMES.append('SURFACE PRESSURE')
               self.slf2d.VARUNITS.append('UI              ')
            if 165 == i:
               self.slf2d.VARNAMES.append('WIND VELOCITY U ')
               self.slf2d.VARUNITS.append('M/S             ')
            if 166 == i:
               self.slf2d.VARNAMES.append('WIND VELOCITY V ')
               self.slf2d.VARUNITS.append('M/S             ')
            if 167 == i:
               self.slf2d.VARNAMES.append('AIR TEMPERATURE ')
               self.slf2d.VARUNITS.append('DEGREES         ')
         for var in self.slf2d.VARNAMES: print '    - ',var
      elif self.typ == 'spec':
         if 251 in self.variables:
            #self.slf2d.VARNAMES.append('WAVE SPECTRUM   ')
            #self.slf2d.VARUNITS.append('UI              ')
            for i in range( self.NX1D * self.NY1D ):
               self.slf2d.VARNAMES.append(('F PT '+str(i+1)+'                ')[:16])
               self.slf2d.VARUNITS.append('UI              ')
         print '    - from ',self.slf2d.VARNAMES[0],' to ',self.slf2d.VARNAMES[-1]
      if self.slf2d.VARNAMES == []:
         print( '... could not match requested valiable with type of file.\n\n' )
         sys.exit(1)
      self.slf2d.NBV1 = len(self.slf2d.VARNAMES)
      self.slf2d.NVAR = self.slf2d.NBV1
      self.slf2d.VARINDEX = range(self.slf2d.NVAR)

      self.slf2d.appendHeaderSLF()
      
   def appendCoreTimeGRIB(self,itime):
      
      self.slf2d.appendCoreTimeSLF(self.slf2d.tags['times'][itime])
   
   def appendCoreVarsGRIB(self,itime):
      
      if self.typ == 'wave':
         pass
         # ~~> WAVE HEIGHT == 'swh'
         # ~~> SIGNIFICANT WAVE PERIOD == 'mwp'
         # ~~> MEAN WAVE DIRECTION == 'mwd'

      elif self.typ == 'oper':
         var2d = np.zeros((self.slf2d.NVAR,self.slf2d.NPOIN2),dtype=np.float)
         grbs = pygrib.open(self.dataset[itime])
         for grb in grbs:
            if grb.indicatorOfParameter in self.variables:
               jvar = self.variables.index(grb.indicatorOfParameter)
               var2d[jvar,:] = np.ravel(grb.values.T)
         grbs.close()
         for jvar in range(self.slf2d.NVAR):
            self.slf2d.appendCoreVarsSLF([var2d[jvar,:]])

      elif self.typ == 'spec':

         spec = np.zeros((self.numberOfDirections,self.numberOfFrequencies,self.NX1D,self.NY1D),dtype=np.float)
         grbs = pygrib.open(self.dataset[itime])
         ibar = 0; pbar = ProgressBar(maxval=self.numberOfDirections*self.numberOfFrequencies).start()
         for grb in grbs:
            ii = 0
            data = grb.values.data
            data[np.where( np.absolute(data) <= 0.001 )] = np.nan
            data[np.where( data == self.missingValue )] = np.nan
            data = 10. ** data
            data[np.isnan( data )] = 0.
            for iy in range(len(self.MASKY)):
               if self.MASKY[iy]:
                  spec[grb.directionNumber-1,grb.frequencyNumber-1,:,ii] = data[iy][self.MASKX]
                  ii += 1
            ibar += 1
            pbar.update(ibar)
         pbar.finish()
         grbs.close()
         
         #drawColouredQuadMaps(self.slf2d.NPOIN2,self.slf2d.NELEM2,self.slf2d.MESHX,self.slf2d.MESHY,np.ravel(a))
         for ix in range(self.NX1D):
            for iy in range(self.NY1D):
               #print len(np.ravel(spec[:,:,ix,iy])),np.ravel(spec[:,:,ix,iy])
               self.slf2d.appendCoreVarsSLF([np.ravel(spec[:,:,ix,iy].T)])

   def putContent(self,fileName,showbar=True):

      grb2slf.openGRIB(rootName)
      
      print( '     +> Write SELAFIN header' )
      grb2slf.appendHeaderGRIB()

      print( '     +> Write SELAFIN core' )
      #if showbar: pbar = ProgressBar(maxval=len(self.dataset)).start()
      for itime in range(len(self.dataset)):
         AT = ( datetime( *self.slf2d.DATETIME ) + timedelta( seconds=int(self.slf2d.tags['times'][itime]) ) ).timetuple()[0:6]
         print( "        - "+ str(AT[2])+"-"+str(AT[1])+"-"+str(AT[0])+" "+ str(AT[3])+":"+str(AT[4])+":"+str(AT[5]) )
         self.appendCoreTimeGRIB(itime)
         self.appendCoreVarsGRIB(itime)
         #if showbar: pbar.update(itime)
      #if showbar: pbar.finish()

      grb2slf.closeGRIB()

# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

__author__="Sebastien E. Bourban"
__date__ ="$12-Dec-2014 08:51:29$"

if __name__ == "__main__":
   debug = False

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print( '\n\nInterpreting command line options\n'+72*'~'+'\n' )
   parser = ArgumentParser(\
      formatter_class=RawDescriptionHelpFormatter,
      description=('''\n
Either download netCDF files from 'http://api.ecmwf.int/' or load up locally
   stored GRIB files and convert them into a SELAFIN file.
      '''),
      usage=' (--help for help)\n---------\n      =>  %(prog)s [options] rootname\n---------',
      epilog=('''\nexamples:\n---------
1:    => convertECMWF.py --from "2011-02-15" --stop "2011-02-20" --bl "(34,140)" --tr "(41,147)" --dataset interm --stream wave ecmwf-erai-atm-5d.slf
2:    => convertECMWF.py --from "2011-02-15" --stop "2011-02-20" --bl "(34,140)" --tr "(41,147)" --dataset interm --stream oper ecmwf-erai-wav-5d.slf
3:    => convertECMWF.py --from "2011-02-15" --stop "2011-02-20" --bl "(34,140)" --tr "(41,147)" --dataset era5 --stream wave ecmwf-era5-wav-5d.slf
3:    => convertECMWF.py --from "2011-02-15" --stop "2011-02-20" --bl "(34,140)" --tr "(41,147)" --dataset ./grib --stream spec ecmwf-grib-spec-5d.slf
---------'''))
   parser.add_argument(\
      "rootName",default='',
      help="specify the root name of the resulting SELAFIN file." )
   parser.add_argument(\
      "-f", "--from",dest="tfrom",default=None, #required=True,
      help='specify the first date included. Example: "1972-13-07"' )
   parser.add_argument(\
      "-s", "--stop",dest="tstop",default=None, #required=True,
      help='specify the last date included "1980-12-31"' )
   parser.add_argument(\
      "--bl",dest="blcorner",default=None, #required=True,
      help='specify the bottom left corner. Example: "(25,-117)"' )
   parser.add_argument(\
      "--tr",dest="trcorner",default=None, #required=True,
      help='specify the top right corner. Example: "(27,-110)"' )
   parser.add_argument(\
      "--dataset",dest="dataset",default='interim',
      help="type of dataset requested either 'interim', 'era5' or the name of a grib or netcdf file or dirctory contaiing grib or netcdf files, depending on periods and resolution, etc.")
   parser.add_argument(\
      "--stream",dest="stream",default='oper',
      choices=set(("oper", "wave", "spec")),
      help="type of stream requested either 'oper' (atmospheric) or 'wave' (waves), etc.")
   options = parser.parse_args()

   # Arbitrary 6-day period
   period = [[],[]]
   if options.tfrom != None:
      for i in options.tfrom.split('-'): period[0].append(int(i))
   else:
      print( '... could not find your from date. Please use --from option (- delimited, no spaces).\n\n' )
      sys.exit(1)
   if options.tstop != None:
      for i in options.tstop.split('-'): period[1].append(int(i))
   else:
      print( '... could not find your stop date. Please use --stop option (- delimited, no spaces).\n\n' )
      sys.exit(1)

   # arbitrary box (small pieve of the atlantic side of Mexico)
   modelbox = [[],[]]
   if options.blcorner != None:
      for i in options.blcorner.replace('(','').replace(')','').split(','): modelbox[0].append(float(i))
   else:
      print( '... could not find your bounding box bottom left corner. Please use --bl option (, delimited, no spaces).\n\n' )
      sys.exit(1)
   if options.trcorner != None:
      for i in options.trcorner.replace('(','').replace(')','').split(','): modelbox[1].append(float(i))
   else:
      print( '... could not find your bounding box top right corner. Please use --tr option (, delimited, no spaces).\n\n' )
      sys.exit(1)

   # rootName
   #if len(args) != 1:
   #   print( '... only one file name is necessary to capture the processed stream.\n\n' )
   #   sys.exit(1)
   rootName = options.rootName #args[-1]
   head,tail = path.splitext(rootName)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Making a request ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if options.dataset == 'interim':
      download = True
      req = {
         'dataset' : "interim",
         'step'    : "0",
         'number'  : "all",
         'levtype' : "sfc",      # surface or single level
         'date'    : "2011-02-15/to/2011-06-15",
         'time'    : "00/06/12/18",
         # 'origin'  : "all",
         'type'    : "an",       # analysis
         'param'   : "134.128/165.128/166.128/167.128", # because these are ambigous: "ap/u10/v10/t2m"
         'area'    : "41/140/34/147",
         'grid'    : "0.125/0.125",
         'target'  : head+'.nc',
         'format'  : 'netcdf',
         'class'   : "ei",
         'stream'  : options.stream
      }
      if options.stream == 'wave': req['param'] = "swh/mwp/mwd"
      # only supported file type
      fileType = 'netcdf'
   elif options.dataset == 'era5':
      download = True
      req = {
         'dataset' : "era5",
         'step'    : "0",
         'number'  : "all",
         'levtype' : "sfc",      # surface or single level
         'date'    : "2011-02-15/to/2011-06-15",
         'time'    : "00/01/02/03/04/05/06/07/08/09/10/11/12/13/14/15/16/17/18/19/20/21/22/23",
         # 'origin'  : "all",
         'type'    : "an",       # analysis
         'param'   : "134.128/165.128/166.128/167.128", # because these are ambigous: "ap/u10/v10/t2m"
         'area'    : "41/140/34/147",
         'grid'    : "0.125/0.125",
         'target'  : head+'.nc',
         'format'  : 'netcdf',
         'class'   : "ea",
         'stream'  : options.stream
      }
      if options.stream == 'wave': req['param'] = "swh/mwp/mwd"
      if options.stream == 'spec':
         req['param'] = "2dfd"
         req['stream'] = 'wave'
      # only supported file type
      fileType = 'netcdf'
   else:
      download = False
      # ~~> GRIB files within a directory
      if path.isdir(options.dataset):
         # only supported file type
         fileType = 'grib'
         options.dataset = [ path.join(options.dataset,d) for d in listdir(options.dataset) ]
      # ~~> actual file
      elif path.exists(options.dataset):
         if path.splitext(options.dataset)[1] =='.nc': fileType = 'netcdf'
         else: fileType = 'grib'
         options.dataset = [ options.dataset ]
      else:
         print( '... could not find your local files.\n\n' )
         sys.exit(1)
      # ~~> In case of GRIB files
      if fileType == 'grib':
         if not importgrib:
            print( '... could not import pygrib on this platform.\n\n' )
            sys.exit(1)
      # ~~> In case of GRIB files
      req = {
         'date'    : "2011-02-15/to/2011-06-15",
         'time'    : "00/01/02/03/04/05/06/07/08/09/10/11/12/13/14/15/16/17/18/19/20/21/22/23",
         'param'   : "151/165/166", # because these are ambigous: "ap/u10/v10"
         'area'    : "41/140/34/147",
         'grid'    : "0.125/0.125",
         'target'  : options.dataset[0]
      }
      if options.stream == 'spec': req['param'] = "251"


   req['date'] = options.tfrom + '/to/' + options.tstop
   req['area'] = options.blcorner.replace('(','').replace(')','').replace(',','/') + '/' + options.trcorner.replace('(','').replace(')','').replace(',','/')

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Loading up the GRIB file~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
   if fileType == 'grib':
      
      if download:
         print( '... I am not programmed to download grib files directly.\n\n' )
         sys.exit(1)
      print( '\n\n'+72*'~'+'\n' )
      print( '\nLoading essentials from the GRIB\n' )
      grb2slf = GRIB(options.dataset,req,options.stream)
      
      grb2slf.setGeometry()

      if options.stream == 'spec':
         print( '\n\n'+72*'~'+'\n' )
         print( '\nSpecial case for spectral file\n' )
         grb2slf.putGeometry('geo_'+head+'.slf')
         grb2slf.setSpectral()

      print( '\n\n'+72*'~'+'\n' )
      print( '\nConverting grib file(s) into SELAFIN\n' )
      grb2slf.putContent(rootName)
   
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Downloading the NetCDF file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Unfortunately, I did not manage to access the NetCDF file remotely
   elif fileType == 'netcdf':

      ecmwf2slf = ECMWF(period,req)
      if download:
         print( '\n\n'+72*'~'+'\n' )
         print( '\nMaking an ECMWF request\n' )
         ecmwf2slf.connectToECMWF("datasets/%s" % (req['dataset']))

         print( '\n\n'+72*'~'+'\n' )
         print( '\nHaving to download the ECMWF file first\n' )
         ecmwf2slf.downloadECMWF()
         print( "   ~> download completed." )

      ecmwf2slf.openECMWF()
      ecmwf2slf.setGeometry()
      
      if options.stream == 'spec':
         print( '\n\n'+72*'~'+'\n' )
         print( '\nSpecial case for spectral file\n' )
         ecmwf2slf.putGeometry('geo_'+head+'.slf')
         ecmwf2slf.setSpectral()

      print( '\n\n'+72*'~'+'\n' )
      print( '\nConverting netcdf file into SELAFIN\n' )
      ecmwf2slf.putContent(rootName,options.stream)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   print( '\n\nMy work is done\n\n' )

   sys.exit(0)


#print grb.keys()
#print grb.count,grb.countTotal,grb.section0Length,grb.section1Length,grb.totalLength
#print grb.numberOfDirections,grb.numberOfFrequencies
#print grb.numberOfCodedValues,grb.numberOfValues,grb.Nj,grb.Ni
#sys.exit()
#print grb.name,grb.dataDate,grb.dataTime,grb.indicatorOfParameter,len(grb.latitudes),len(grb.longitudes)
#print grb.shortName
#print grb.numberOfDataPoints
#print grb.numberOfValues
#y = grb.latitudes
#x = grb.longitudes
#x,y = grb.latlons()
#print x.shape,x
#print y.shape,y

"""

"""