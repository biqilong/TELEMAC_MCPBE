!                    ************************
                     PROGRAM HOMERE_TELEMAC3D
!                    ************************
!
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    1) OPENS FILES, SETS POINTERS ACCORDING TO THE
!+                   PARAMETERS IMPOSED IN THE STEERING FILE AND
!+                   THE GIVEN GEOMETRY.
!+
!+            2) CALLS THE MAIN SUBROUTINE.
!+
!+            3) MEASURES CPU TIME.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history
!+        10/04/2009
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  R.ATA
!+        21/01/2016
!+        V7P2
!+   coupling with waqtel
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES,MAXLU_SIS
      USE DECLARATIONS_TOMAWAC, ONLY : WAC_FILES,MAXLU_WAC
      USE DECLARATIONS_WAQTEL , ONLY : WAQ_FILES,MAXLU_WAQ
      USE DECLARATIONS_GAIA,    ONLY : GAI_FILES,MAXLU_GAI
      USE DECLARATIONS_TELEMAC3D
      USE DECLARATIONS_TELEMAC2D, ONLY:WINDX,WINDY
      USE INTERFACE_TELEMAC3D
      USE INTERFACE_WAQTEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER TDEB(8),TFIN(8),NCAR
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC3D               '
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
      CHARACTER(LEN=24), PARAMETER :: CODE4='WAQTEL                  '
      CHARACTER(LEN=24), PARAMETER :: CODE5='GAIA                    '
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD),FILE_DESC(4,MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) DUMMY
!
!======================================================================
!
! STARTS COUNTING CPU TIME
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
! INITIALISES FILES (ESPECIALLY IMPORTANT FOR A PARALLEL MACHINE)
!
      CALL BIEF_INIT(PATH,NCAR,.TRUE.)
!
! WRITES A BANNER TO THE LISTING
!
      CALL PRINT_HEADER(CODE1,'                        ')
!
!-----------------------------------------------------------------------
! READS THE STEERING FILE
      DUMMY = ' '
!
      CALL LECDON_TELEMAC3D(MOTCAR,FILE_DESC,PATH,NCAR,DUMMY,DUMMY)
!-----------------------------------------------------------------------
! OPENS THE FILES
!
      CALL BIEF_OPEN_FILES(CODE1,T3D_FILES,MAXLU_T3D,PATH,NCAR,
     &                     1,.FALSE.)
!
!-----------------------------------------------------------------------
!
! ALLOCATES VECTORS, MATRICES AND BLOCKS
!
      CALL POINT_TELEMAC3D
!
!-----------------------------------------------------------------------
!
! INITIALISES SISYPHE IF COUPLING THE 2 MODELS
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
        CALL PRINT_HEADER(CODE2,CODE1)
!
        DUMMY = ' '
!
        CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE1,
     &                      DUMMY,DUMMY)
!
        CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_SIS,PATH,NCAR,
     &                       2,.FALSE.)
!
!       RESETS TELEMAC3D CONFIGURATION
!
        CALL CONFIG_CODE(1)
!
!       MEMORY ORGANIZATION
!
        CALL POINT_SISYPHE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISES GAIA IF COUPLING THE 2 MODELS
!
      IF(INCLUS(COUPLING,'GAIA')) THEN
!
        CALL PRINT_HEADER(CODE5,CODE1)
        DUMMY = ' '
!
        CALL LECDON_GAIA(MOTCAR,FILE_DESC,PATH,NCAR,CODE1,
     &                   DUMMY,DUMMY)
!
        CALL BIEF_OPEN_FILES(CODE5,GAI_FILES,MAXLU_GAI,PATH,NCAR,
     &                       2,.FALSE.)
!
!       RESETS TELEMAC3D CONFIGURATION
!
        CALL CONFIG_CODE(1)
!
!       MEMORY ORGANIZATION
!
        CALL POINT_GAIA
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES TOMAWAC
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!
        CALL PRINT_HEADER(CODE3,CODE1)
!
        CALL T3D_WAC_CPL_UPDATE(NIT)
        CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,DUMMY,DUMMY)
        CALL BIEF_OPEN_FILES(CODE3,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                       3,.FALSE.)
!
!     RESETS TELEMAC3D CONFIGURATION
!
        CALL CONFIG_CODE(1)
!
!     MEMORY ORGANIZATION
!
        CALL POINT_TOMAWAC
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES WAQTEL
!
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
!
        CALL PRINT_HEADER(CODE4,CODE1)
!
      CALL LECDON_WAQTEL(FILE_DESC,PATH,NCAR,DUMMY,DUMMY)
      CALL BIEF_OPEN_FILES(CODE4,WAQ_FILES,MAXLU_WAQ,PATH,NCAR,
     &                     4,.FALSE.)
!
!     NAMETRAC IS NOW CALLED IN LECDON_TELEMAC3D
!
!     RESETS TELEMAC2D CONFIGURATION
!
      CALL CONFIG_CODE(1)
!
!     MEMORY ORGANISATION
!
      CALL POINT_WAQTEL(MESH2D,IELM2H,VENT,WINDX,WINDY,
     &                  ATMOS,PATMOS,MESH3D,IELM3)
!
      ENDIF
!
!=======================================================================
!
      CALL TELEMAC3D(PASS=-1,NIT_ORI=NIT)
!
! Valeur de PASS mise "par défaut"
!
!-----------------------------------------------------------------------
!
      CALL BIEF_CLOSE_FILES(T3D_FILES,MAXLU_T3D,.TRUE.)
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
        CALL CONFIG_CODE(2)
        CALL BIEF_CLOSE_FILES(SIS_FILES,MAXLU_SIS,.FALSE.)
        CALL DEALL_SISYPHE
      ENDIF
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
        CALL CONFIG_CODE(3)
        CALL BIEF_CLOSE_FILES(WAC_FILES,MAXLU_WAC,.FALSE.)
        CALL DEALL_TOMAWAC()
      ENDIF
!
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
        CALL CONFIG_CODE(4)
        CALL BIEF_CLOSE_FILES(WAQ_FILES,MAXLU_WAQ,.FALSE.)
        CALL DEALL_WAQTEL
      ENDIF
      !
      IF(INCLUS(COUPLING,'GAIA')) THEN
        CALL CONFIG_CODE(5)
        CALL BIEF_CLOSE_FILES(GAI_FILES,MAXLU_GAI,.FALSE.)
        CALL DEALL_GAIA
      ENDIF

      CALL DEALL_TELEMAC3D
      CALL DEALL_BIEF
!
!-----------------------------------------------------------------------
! HOPEFULLY GOOD NEWS
!
      WRITE(LU,11)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
! PRINTS THE CPU TIME CONSUMED
!
      CALL DATE_AND_TIME(VALUES=TFIN)
      CALL ELAPSE(TDEB,TFIN)
!
!-----------------------------------------------------------------------
!
      STOP 0
      END
