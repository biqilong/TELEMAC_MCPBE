!                    *****************
                     SUBROUTINE SMTRAC
!                    *****************
!
     &(NPOIN,DIMT,DT,SMTR,SMH,NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,ITRAC,
     & NREG,TNP,PT_IN_POLY)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE SECOND MEMBER FOR THE TRACER.
!
!history  INRIA
!+
!+        V5P8
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
!history  SARA PAVAN
!+        21/03/2018
!+        V8P0
!+   add the option of sources defined by region
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMT           |-->| DIMENSION OF THE TRACER
!| DT             |-->| HYDRO TIME STEP
!| ISCE           |-->| GLOBAL INDICES OF SOURCE POINTS
!| ITRAC          |-->| TRCER INDEX
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMER OF TRACERS
!| NPOIN          |-->| TOTAL NUMBER OF NODES IN THE MESH
!| NREG           |-->| NUMBER OF SOURCE REGIONS
!| NREJET         |-->| NUMBER OF SOURCE/SINK
!| PT_IN_POLY     |-->| NUMBERS OF NODES CONTAINED IN THE SOURCE REGIONS
!| SMH            |-->| SOURCE TERMS FOR CONTINUITY EQUATION
!| SMTR           |-->| SOURCE TERMS FOR TRACER
!| TNP            |-->| NUMBER OF MESH POINTS CONTAINED IN SOURCE REGIONS
!| TSCE2          |-->| VALUES OF TRACERS AT SOURCES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D,EX_SMTRAC => SMTRAC
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,NREJET,ISCE(*),DIMT,ITRAC
      INTEGER         , INTENT(IN)    :: MAXSCE,MAXTRA
      INTEGER         , INTENT(IN)    :: NREG
      INTEGER         , INTENT(IN)    :: TNP(NREG)
      INTEGER         , INTENT(IN)    :: PT_IN_POLY(MAXSCE,*)
      DOUBLE PRECISION, INTENT(IN)    :: DT,SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)
      DOUBLE PRECISION, INTENT(INOUT) :: SMTR(DIMT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IS,II,TTL,IREG
!
!-----------------------------------------------------------------------
!
      IF(NREJET.NE.0.AND.NREG.EQ.0) THEN
        DO I=1,NREJET
          IS =ISCE(I)
          IF(IS.GT.0) THEN
            SMTR(IS) = SMTR(IS) + DT*SMH(IS) * TSCE2(I,ITRAC)
          ENDIF
        ENDDO
      ELSEIF(NREJET.NE.0.AND.NREG.NE.0) THEN
        DO IREG=1,NREG
          TTL=TNP(IREG)
!         TEST USEFUL FOR PARALLEL MODE
          IF(TTL.NE.0) THEN
            DO I=1,TTL
              II=PT_IN_POLY(IREG,I)
              SMTR(II) = SMTR(II) + DT*SMH(II) * TSCE2(IREG,ITRAC)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
