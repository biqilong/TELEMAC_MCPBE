#!/usr/bin/env python
from struct import unpack,pack
import sys
import numpy as np
#import openturns as op
from os import path
import math

INTSIZE = 4
REALSIZE = 4
TITLESIZE = 80
VARSIZE = 32
ENDIAN = ">"
CARTYPE = "s"
INTTYPE = "l"
REALTYPE = "f"

class SrfObj:
   def __init__(self,fname):
      self.fname = fname
      self.title = ""
      self.nvar = 0
      self.varName = []
      self.nplan = 0
      self.nptfr = 0
      self.nptir = 0
      self.date = None
      self.nelem = 0
      self.npoin = 0
      self.ndp = 0
      self.ikle = None
      self.ipobo = None
      self.x = None
      self.y = None
      self.ntimestep = 0
      self.times = None
      self.values = None
      with open(fname) as f:
         # Title
         f.read(4)
         self.title = unpack(ENDIAN+str(TITLESIZE)+CARTYPE,f.read(TITLESIZE))[0]
         f.read(4)
         # Number of variable
         f.read(4)
         self.nvar,_ = unpack(ENDIAN+'2'+INTTYPE,f.read(INTSIZE*2))
         f.read(4)
         # Variable names
         for i in range(self.nvar):
            f.read(4)
            self.varName.append(unpack(ENDIAN+str(VARSIZE)+CARTYPE,f.read(VARSIZE))[0])
            f.read(4)
         # 10 integers
         f.read(4)
         _,_,_,_,_,_,self.nplan,self.nptfr,self.nptir,self.has_date = \
              unpack(ENDIAN+'10'+INTTYPE,f.read(INTSIZE*10))
         f.read(4)
         # Reads the date if there is one
         if(self.has_date != 0):
            f.read(4)
            self.date = unpack(ENDIAN+'6'+INTTYPE,f.read(INTSIZE*6))
            f.read(4)
         # Read nelem,npoin,ndp,1
         f.read(4)
         self.nelem,self.npoin,self.ndp,_ = unpack(ENDIAN+'4'+INTTYPE,f.read(INTSIZE*4))
         # Compute the number of time steps
         self.compute_ntimestep()

   def __del__(self):
      self.fname = ""
      self.title = ""
      self.nvar = 0
      del self.varName
      self.nplan = 0
      self.nptfr = 0
      self.nptir = 0
      del self.date
      self.nelem = 0
      self.npoin = 0
      self.ndp = 0
      del self.ikle
      del self.ipobo
      del self.x
      del self.y
      self.ntimestep = 0
      del self.times
      del self.values

   def compute_ntimestep(self):
      """
         Compute the number of timestep using the file size
      """
      # Get the size of the file
      fsize = path.getsize(self.fname)
      # Define the position in the file of the data
      pos_data = 4+80+4\
               + 4+2*INTSIZE+4\
               + (4+32+4) * self.nvar\
               + 4+10*INTSIZE+4\
               + 4+4*INTSIZE+4\
               + 4+self.nelem*self.ndp*INTSIZE+4\
               + 4+self.npoin*INTSIZE+4\
               + 4+self.npoin*REALSIZE+4\
               + 4+self.npoin*REALSIZE+4
      # Add offset due to date if necessary
      if(self.has_date != 0):
         pos_data += 4+6*INTSIZE+4
      # Size of the data from a whoel timestep
      size_data_set = 4+REALSIZE+4 + (4+self.npoin*REALSIZE+4)*self.nvar
      # Compute the number of timestep
      self.ntimestep = (fsize - pos_data)/size_data_set

   def read(self):
      """
         Read a serafin file
      """
      with open(self.fname,"rb") as f:
         # Title
         f.read(4)
         self.title = unpack(ENDIAN+str(TITLESIZE)+CARTYPE,f.read(TITLESIZE))[0]
         f.read(4)
         # Number of variable
         f.read(4)
         self.nvar,_ = unpack(ENDIAN+'2'+INTTYPE,f.read(INTSIZE*2))
         f.read(4)
         # Variable names
         for i in range(self.nvar):
            f.read(4)
            self.varName.append(unpack(ENDIAN+str(VARSIZE)+CARTYPE,f.read(VARSIZE))[0])
            f.read(4)
         # 10 integers
         f.read(4)
         _,_,_,_,_,_,self.nplan,self.nptfr,self.nptir,self.has_date = \
              unpack(ENDIAN+'10'+INTTYPE,f.read(INTSIZE*10))
         f.read(4)
         # Reads the date if there is one
         if(self.has_date != 0):
            f.read(4)
            self.date = unpack(ENDIAN+'6'+INTTYPE,f.read(INTSIZE*6))
            f.read(4)
         # Read nelem,npoin,ndp,1
         f.read(4)
         self.nelem,self.npoin,self.ndp,_ = unpack(ENDIAN+'4'+INTTYPE,f.read(INTSIZE*4))
         f.read(4)
         # Read ikle
         self.ikle = np.zeros(self.nelem*self.ndp)
         f.read(4)
         self.ikle = np.asarray(unpack(ENDIAN+str(self.nelem*self.ndp)+INTTYPE,
                                       f.read(INTSIZE*self.nelem*self.ndp)))
         f.read(4)
         # Read ipobo/knolg
         self.ipobo = np.zeros(self.npoin)
         f.read(4)
         self.ipobo = np.asarray(unpack(ENDIAN+str(self.npoin)+INTTYPE,
                                        f.read(INTSIZE*self.npoin)))
         f.read(4)
         # Read x and y
         self.x = np.zeros(self.npoin)
         self.y = np.zeros(self.npoin)
         f.read(4)
         self.x = np.asarray(unpack(ENDIAN+str(self.npoin)+REALTYPE,
                                    f.read(REALSIZE*self.npoin)))
         f.read(4)
         f.read(4)
         self.y = np.asarray(unpack(ENDIAN+str(self.npoin)+REALTYPE,
                                    f.read(REALSIZE*self.npoin)))
         f.read(4)
         # Data informations
         self.compute_ntimestep()

   def read_values(self):
      """
         Read all the values from the file and file self.values
      """
      # Define the position in the file of the data
      pos_data = 4+80+4\
               + 4+2*INTSIZE+4\
               + (4+32+4)*self.nvar\
               + 4+10*INTSIZE+4\
               + 4+4*INTSIZE+4\
               + 4+self.nelem*self.ndp*INTSIZE+4\
               + 4+self.npoin*INTSIZE+4\
               + 4+self.npoin*REALSIZE+4\
               + 4+self.npoin*REALSIZE+4
      # Add date id necessary
      if(self.has_date != 0):
         pos_data += 4+6*INTSIZE+4
      self.values = np.zeros((self.ntimestep,self.nvar,self.npoin))
      self.times = np.zeros((self.ntimestep))
      with open(self.fname,'rb') as f:
         f.seek(pos_data)
         for i in range(self.ntimestep):
            f.read(4)
            self.times[i] = unpack(ENDIAN+REALTYPE,f.read(REALSIZE))[0]
            f.read(4)
            for j in range(self.nvar):
               f.read(4)
               self.values[i][j] = np.asarray(unpack(ENDIAN+str(self.npoin)+REALTYPE,
                                          f.read(REALSIZE*self.npoin)))
               f.read(4)


   def get_value(self,itime,ivar,ipoin):
      """
         Returns the value on a point for a given variable and a given timestep
      """
      if ivar<0 or ivar>=self.nvar:
         print "ivar ",ivar,"must be between 0 and ",self.nvar
         return -1
      if itime<0 or itime>=self.ntimestep:
         print "itime ",itime,"must be between 0 and ",self.ntimestep
         return -1
      if ipoin<0 or ipoin>=self.npoin:
         print "ipoin ",ipoin,"must be between 0 and ",self.npoin
         return -1
      # Define the position in the file of the data
      pos_data = 4+80+4\
               + 4+2*INTSIZE+4\
               + (4+32+4)*self.nvar\
               + 4+10*INTSIZE+4\
               + 4+4*INTSIZE+4\
               + 4+self.nelem*self.ndp*INTSIZE+4\
               + 4+self.npoin*INTSIZE+4\
               + 4+self.npoin*REALSIZE+4\
               + 4+self.npoin*REALSIZE+4
      # Add date id necessary
      if(self.has_date != 0):
         pos_data += 4+6*INTSIZE+4
      # Sier of the data from a variable
      size_var_set = 4+self.npoin*REALSIZE+4
      # Size of the data from a whoel timestep
      size_data_set = 4+REALSIZE+4 + size_var_set*self.nvar
      # Reading the value
      with open(self.fname,'rb') as f:
         f.seek(pos_data + itime*size_data_set\
                + 4+REALSIZE+4\
                + ivar*size_var_set+ 4+ipoin*REALSIZE)
         return unpack(ENDIAN+REALTYPE,f.read(REALSIZE))[0]

   def write(self,fname):
      """
         Write a serafin file
      """
      with open(fname,"wb") as f:
         # Title
         f.write(pack(ENDIAN+INTTYPE,TITLESIZE))
         f.write(pack(ENDIAN+str(TITLESIZE)+CARTYPE,self.title))
         f.write(pack(ENDIAN+INTTYPE,TITLESIZE))
         # Number of variable
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*2))
         f.write(pack(ENDIAN+'2'+INTTYPE,self.nvar,0))
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*2))
         # Variable names
         for i in range(self.nvar):
            f.write(pack(ENDIAN+INTTYPE,VARSIZE))
            f.write(pack(ENDIAN+str(VARSIZE)+CARTYPE,self.varName[i]))
            f.write(pack(ENDIAN+INTTYPE,VARSIZE))
         # 10 integers
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*10))
         f.write(pack(ENDIAN+'10'+INTTYPE,1,0,0,0,0,0,
                                          self.nplan,self.nptfr,
                                          self.nptir,self.has_date))
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*10))
         # Reads the date if there is one
         if(self.has_date != 0):
            f.write(pack(ENDIAN+INTTYPE,INTSIZE*6))
            f.write(pack(ENDIAN+'6'+INTTYPE,self.date[0],
                                            self.date[1],
                                            self.date[2],
                                            self.date[3],
                                            self.date[4],
                                            self.date[5]))
            f.write(pack(ENDIAN+INTTYPE,INTSIZE*6))
         # Read nelem,npoin,ndp,1
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*4))
         f.write(pack(ENDIAN+'4'+INTTYPE, self.nelem,self.npoin,
                                          self.ndp,1))
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*4))
         # Read ikle
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*self.nelem*self.ndp))
         f.write(pack(ENDIAN+str(self.nelem*self.ndp)+INTTYPE,
                      *(self.ikle)))
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*self.nelem*self.ndp))
         # Read ipobo/knolg
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*self.npoin))
         f.write(pack(ENDIAN+str(self.npoin)+INTTYPE,
                      *(self.ipobo)))
         f.write(pack(ENDIAN+INTTYPE,INTSIZE*self.npoin))
         # Read x and y
         f.write(pack(ENDIAN+INTTYPE,REALSIZE*self.npoin))
         f.write(pack(ENDIAN+str(self.npoin)+REALTYPE,
                      *(self.x)))
         f.write(pack(ENDIAN+INTTYPE,REALSIZE*self.npoin))
         f.write(pack(ENDIAN+INTTYPE,REALSIZE*self.npoin))
         f.write(pack(ENDIAN+str(self.npoin)+REALTYPE,
                      *(self.y)))
         f.write(pack(ENDIAN+INTTYPE,REALSIZE*self.npoin))
         # Data informations
         for i in range(self.ntimestep):
            f.write(pack(ENDIAN+INTTYPE,REALSIZE))
            f.write(pack(ENDIAN+REALTYPE,self.times[i]))
            f.write(pack(ENDIAN+INTTYPE,REALSIZE))
            for j in range(self.nvar):
               f.write(pack(ENDIAN+INTTYPE,REALSIZE*self.npoin))
               f.write(pack(ENDIAN+str(self.npoin)+REALTYPE,
                            *(self.values[i][j])))
               f.write(pack(ENDIAN+INTTYPE,REALSIZE*self.npoin))

   def __str__(self):
      """
         What is return to print
      """
      string = "title "+self.title+"\n"
      string += "nvar "+str(self.nvar)+"\n"
      string += "nplan "+str(self.nplan)+"\n"
      string += "nptfr "+str(self.nptfr)+"\n"
      string += "nptir "+str(self.nptir)+"\n"
      string += "has_date "+str(self.has_date)+"\n"
      if(self.has_date != 0):
         string += "date "+str(self.date)+"\n"
      string += "nelem "+str(self.nelem)+"\n"
      string += "npoin "+str(self.npoin)+"\n"
      string += "ndp "+str(self.ndp)+"\n"
      string += "ntimestep "+str(self.ntimestep)+"\n"
      string += "times "+str(self.times)+"\n"
      return string

   def __repr__(self):
      """
         What is return to print
      """
      string = "title "+self.title+"\n"
      string += "nvar "+str(self.nvar)+"\n"
      string += "nplan "+str(self.nplan)+"\n"
      string += "nptfr "+str(self.nptfr)+"\n"
      string += "nptir "+str(self.nptir)+"\n"
      string += "has_date "+str(self.has_date)+"\n"
      if(self.has_date != 0):
         string += "date "+str(self.date)+"\n"
      string += "nelem "+str(self.nelem)+"\n"
      string += "npoin "+str(self.npoin)+"\n"
      string += "ndp "+str(self.ndp)+"\n"
      string += "ntimestep "+str(self.ntimestep)+"\n"
      string += "times "+str(self.times)+"\n"
      return string

if __name__ == "__main__":
   nsicla = 5

   mySrf = SrfObj('sis_WC2003.slf')
#   print(mySrf.varName)
   cpt = 0
   var_QS = -1
   # Variable index containing QS info
   for word in mySrf.varName:
      if "QS BEDLOAD      M2/S            " in word:
         var_QS = cpt
      else:
         cpt+=1
   print('QS VARIABLE INDEX :' + str(var_QS))

   var_QSi = []
   for i in range(0,nsicla):
      cpt = 0
      var_QSi.append(-1)
   # Variable index containing QS info
      for word in mySrf.varName:
         if 'QS CLASS '+str(i+1)+' ' in word:
            var_QSi[len(var_QSi)-1] = cpt
         else:
            cpt+=1
      print('QS CLASS '+str(i+1)+ ' VARIABLE INDEX :' + str(var_QSi[len(var_QSi)-1]))

   mySrf.read()
   mySrf.read_values()

   for ts in range(mySrf.ntimestep-1, mySrf.ntimestep):
      mean_QS = 0.0
      for j in range(mySrf.npoin):
            mean_QS += mySrf.get_value(ts,var_QS,j) # Variable QSBL
      mean_QS = mean_QS/mySrf.npoin
      QS_lin = mean_QS#*1000.0*1000.0*2.65#/0.6*0.6
      print("qb tot =\t" + str(QS_lin) + "\t[m2/s]")
      for i in range(0,nsicla):
         mean_QS = 0.0
         for j in range(mySrf.npoin):
            mean_QS += mySrf.get_value(ts,var_QSi[i],j) # Variable QSBL
         mean_QS = mean_QS/mySrf.npoin
         QS_lin = mean_QS#*1000.0*1000.0*2.65#/0.6*0.6
         print("qb,i CLASS "+str(i+1)+ " =\t" + str(QS_lin) + "\t[m2/s]")

