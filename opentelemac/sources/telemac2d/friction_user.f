!                       ************************
                        SUBROUTINE FRICTION_USER
!                       ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                   20/03/2014
!***********************************************************************
!
!brief    DEFINES FRICTION ZONES (BY NODE).
!+        The file format for giving a zone number to zones is here:
!+        node-number   zone-number
!+        for all the nodes, example:
!+        1  123
!+        2  123
!+        3  47
!+        4  47
!+        etc.
!+
!
!history  F. HUVELIN
!+        15/04/2004
!+        V5P4
!+   First version.
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/01/2014
!+        V7P0
!+   Provisionnal version, with ZONES FILE, but this file has yet to be
!+   treated in partel. KNOGL replaced by GLOBAL_TO_LOCAL_POINT.
!
!history  Y AUDOUIN (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Zones file now written in local numbering. Plus checking of point
!+   numbers added by JMH and 2 trailing temporary arrays removed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!------------------------------------------------------------------------
!
      INTEGER            :: I,K,IVAL2,NFILE
      CHARACTER(LEN=PATH_LEN) :: NOMFILE
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      NFILE   = T2D_FILES(T2DZFI)%LU
      NOMFILE = T2D_FILES(T2DZFI)%NAME
!
!     Reading File (which is now written in local numbering)
!
      DO K=1,NPOIN
        READ(NFILE,*,END=997,ERR=998) I, IVAL2
        IF(K.EQ.I) THEN
          KFROPT%I(I) = IVAL2
        ELSE
        WRITE(LU,*) 'ERROR IN THE ZONES FILE: ',NOMFILE
          WRITE(LU,*) 'POINT ',K,' EXPECTED, POINT ',I,' FOUND'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
      GOTO 997
!
!---------------------------------------------------------------------
!             ERROR WHEN READING
!---------------------------------------------------------------------
!
998   CONTINUE
      WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
      WRITE(LU,*) 'READ ERROR'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
997   CONTINUE
!
!=======================================================================
!=======================================================================
!
      RETURN
      END
