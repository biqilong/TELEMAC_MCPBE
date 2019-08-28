!                    **********************
                     SUBROUTINE SOURCE_WAQ
!                    **********************
!
     &(NPOIN3,NPOIN2,TEXP,TIMP,TN,HPROP,U,V,CF,
     & T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12, T2_1,T2_2,T2_3,
     & PATMOS,DIMM,NPLAN,
     & LATIT,LONGIT,AT,MARDAT,MARTIM,ZPROP)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    GIVES CONTRIBUTION OF WAQ PROCESSES TO SOURCE TERMS
!+                FOR THE TRACER.
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+   CREATION
!
!history  M. JODEAU (LNHE)
!+        18/05/2016
!+        V7P3
!+   + AED2 source terms
!
!history  S.E. BOURBAN (HRW)
!+        21/09/2017
!+        V7P3
!+        WAQPROCESS is now a prime number, so that multiple processes
!+        can be called by multiplication of the prime numbers.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DIMM           |-->| 2D OR 3D
!| HPROP          |-->| PROPAGATION DEPTH (2D)
!| LONGIT         |-->| LONGITUTE OF ORIGIN POINT
!| LATIT          |-->| LATITUDE OF ORIGIN POINT
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NPLAN          |-->| NUMBER OF VERTICAL PLANES
!| T1,..,T12      |<->| WORKING STRUCTURES
!| T2_1,T2_2      |<->| 2D WORKING STRUCTURES
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!| ZPROP          |-->| Z COORDINATES FOR 3D NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL
      USE INTERFACE_WAQTEL, EX_SOURCE_WAQ => SOURCE_WAQ
#if defined HAVE_AED2
      USE DECLARATIONS_WAQTEL,ONLY : EXTCAED2,DEJA_SW
#endif
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER       , INTENT(IN)    :: NPOIN3,DIMM
      INTEGER       , INTENT(IN)    :: NPLAN,NPOIN2
      INTEGER       , INTENT(IN)    :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION,INTENT(IN  ) :: LATIT,LONGIT,AT
      TYPE(BIEF_OBJ), INTENT(IN)    :: TN,HPROP,CF,U,V
      TYPE(BIEF_OBJ), INTENT(IN)    :: PATMOS,ZPROP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TEXP,TIMP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T7,T8,T9,T10,T11,T12
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T2_1,T2_2,T2_3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: J,ITRAC,IERR
      LOGICAL :: FOUND
!
      FOUND = .FALSE.
!
!-----------------------------------------------------------------------
!
!     O2 MODULE
!
      IF( 2*INT(WAQPROCESS/2).EQ.WAQPROCESS ) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS~D_O2'
!
        IF(DIMM.EQ.2)THEN
          CALL CALCS2D_O2
     &         (NPOIN2,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &          PHOTO,RESP,TN,TEXP,TIMP,T2,T3,T4,HPROP,U,V,DEBUG)
!
!          YASMI(IND_O2) = YASMI(IND_O2) .OR. .FALSE.
!          YASMI(IND_OL) = .TRUE.
!          YASMI(IND_NH4) = .TRUE.
!
        ELSEIF(DIMM.EQ.3)THEN
          CALL CALCS3D_O2
     &         (NPOIN3,NPOIN2,NPLAN,WATTEMP,O2SATU,DEMBEN,
     &          FORMK2,K1,K44,K22,PHOTO,RESP,TN,TEXP,TIMP,
     &          T1,T2,T2_1,HPROP,ZPROP,U,V)
!
          TEXP%ADR(IND_O2)%P%TYPR='Q'
          TIMP%ADR(IND_OL)%P%TYPR='Q'
          TIMP%ADR(IND_NH4)%P%TYPR='Q'
!
        ENDIF
        FOUND = .TRUE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROMCALCS~D_O2'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BIOMASS MODULE
!
      IF( 3*INT(WAQPROCESS/3).EQ.WAQPROCESS ) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS~D_BIOMASS'
!
        IF(DIMM.EQ.2)THEN
          CALL CALCS2D_BIOMASS(NPOIN2,WATTEMP,TN,TEXP,RAYEFF,
     &                         HPROP,T1,T2,T3,T4,T5,T6,DEBUG)
!
!          YASMI(IND_PHY) = YASMI(IND_PHY) .OR. .FALSE.
!          YASMI(IND_PO4) = YASMI(IND_PO4) .OR. .FALSE.
!          YASMI(IND_POR) = YASMI(IND_POR) .OR. .FALSE.
!          YASMI(IND_NO3) = YASMI(IND_NO3) .OR. .FALSE.
!          YASMI(IND_NOR) = YASMI(IND_NOR) .OR. .FALSE.
!
        ELSEIF(DIMM.EQ.3)THEN
          CALL CALCS3D_BIOMASS(NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,
     &                         RAYEFF,ZPROP,T1,T2,T3,T4,T5,T6)
!
          TEXP%ADR(IND_PHY)%P%TYPR='Q'
          TEXP%ADR(IND_PO4)%P%TYPR='Q'
          TEXP%ADR(IND_POR)%P%TYPR='Q'
          TEXP%ADR(IND_NO3)%P%TYPR='Q'
          TEXP%ADR(IND_NOR)%P%TYPR='Q'
!
        ENDIF
        FOUND = .TRUE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS~D_BIOMASS'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EUTRO MODULE
!
      IF( 5*INT(WAQPROCESS/5).EQ.WAQPROCESS ) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS~D_EUTRO'
!
        IF(DIMM.EQ.2)THEN
          CALL CALCS2D_EUTRO(NPOIN2,WATTEMP,TN,TEXP,TIMP,RAYEFF,
     &                       HPROP,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     &                       T11,T12,DEBUG,U,V)
!
!          YASMI(IND_PHY) = YASMI(IND_PHY) .OR. .FALSE.
!          YASMI(IND_PO4) = YASMI(IND_PO4) .OR. .FALSE.
!          YASMI(IND_POR) = YASMI(IND_POR) .OR. .FALSE.
!          YASMI(IND_NO3) = YASMI(IND_NO3) .OR. .FALSE.
!          YASMI(IND_NOR) = YASMI(IND_NOR) .OR. .FALSE.
!          YASMI(IND_NH4) = .TRUE.
!          YASMI(IND_OL) = .TRUE.
!          YASMI(IND_O2) = YASMI(IND_O2) .OR. .FALSE.
!
        ELSEIF(DIMM.EQ.3)THEN
          CALL CALCS3D_EUTRO(NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,
     &                       TIMP,RAYEFF,HPROP,ZPROP,T1,T2_1,T3,T4,
     &                       T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,U,V)
!
          TEXP%ADR(IND_PHY)%P%TYPR='Q'
          TEXP%ADR(IND_PO4)%P%TYPR='Q'
          TEXP%ADR(IND_POR)%P%TYPR='Q'
          TEXP%ADR(IND_NO3)%P%TYPR='Q'
          TEXP%ADR(IND_NOR)%P%TYPR='Q'
          TEXP%ADR(IND_NH4)%P%TYPR='Q'
          TIMP%ADR(IND_NH4)%P%TYPR='Q'
          TEXP%ADR(IND_OL)%P%TYPR='Q'
          TIMP%ADR(IND_OL)%P%TYPR='Q'
          TEXP%ADR(IND_O2)%P%TYPR='Q'
!
        ENDIF
        FOUND = .TRUE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS~D_EUTRO'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     MICROPOL MODULE
!
      IF( 7*INT(WAQPROCESS/7).EQ.WAQPROCESS ) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS~D_MICROPOL'
!
        IF(DIMM.EQ.2)THEN
          CALL CALCS2D_MICROPOL(NPOIN2,TN,TEXP,TIMP,HPROP,
     &                          CF,U,V,T1,T2,T3,T4)
!
!          YASMI(IND_SS) = YASMI(IND_SS) .OR. .FALSE.
!          YASMI(IND_SF) = YASMI(IND_SF) .OR. .FALSE.
!          YASMI(IND_C) = .TRUE.
!          YASMI(IND_CSS) = .TRUE.
!          YASMI(IND_CSF) = YASMI(IND_CSF) .OR. .FALSE.
!
        ELSEIF(DIMM.EQ.3)THEN
          CALL CALCS3D_MICROPOL(NPOIN2,TN,TEXP,
     &                          TIMP,ZPROP,CF,U,V,T2_1,T2_2,T2_3,T1,T2,
     &                          DEBUG)
!
          TEXP%ADR(IND_SS)%P%TYPR='Q'
          TEXP%ADR(IND_SF)%P%TYPR='Q'
          TIMP%ADR(IND_C)%P%TYPR='Q'
          TEXP%ADR(IND_C)%P%TYPR='Q'
          TIMP%ADR(IND_CSS)%P%TYPR='Q'
          TEXP%ADR(IND_CSS)%P%TYPR='Q'
          TEXP%ADR(IND_CSF)%P%TYPR='Q'
!
        ENDIF
        FOUND = .TRUE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS~D_MICROPOL'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     THERMIC MODULE
!
      IF( 11*INT(WAQPROCESS/11).EQ.WAQPROCESS ) THEN
!
        IF(DIMM.EQ.2)THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS2D_THERMIC'
          CALL CALCS2D_THERMIC(NPOIN2,TN,TEXP,HPROP,PATMOS)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS2D_THERMIC'
!
!          YASMI(IND_T) = YASMI(IND_T) .OR. .FALSE.
!
        ELSEIF(DIMM.EQ.3)THEN
!         SOURCE TERMS (VOLUME)
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS3D_THERMICV'
          CALL CALCS3D_THERMICV(NPOIN3,NPOIN2,NPLAN,ZPROP%R,
     &                          TN,TEXP,LONGIT,LATIT,
     &                          AT,MARDAT,MARTIM)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS3D_THERMICV'
!
          TEXP%ADR(IND_T)%P%TYPR='Q'
!
        ENDIF
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     AED2 COUPLING
!
      IF( 13*INT(WAQPROCESS/13).EQ.WAQPROCESS ) THEN
#if defined HAVE_AED2
        IF(.NOT.DEJA_SW) THEN
          ALLOCATE(EXTCAED2(NPOIN2,NPLAN),STAT=IERR)
          DEJA_SW = .TRUE.
        ENDIF
        IF(DIMM.EQ.3)THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS3D_AED2'
          CALL CALCS3D_AED2(NPOIN3,NPOIN2,NPLAN,ZPROP%R,
     &                      TN,TEXP,LONGIT,LATIT,AT,
     &                      MARDAT,MARTIM,EXTCAED2,RAYAED2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS3D_AED2'
        ENDIF
        FOUND = .TRUE.
#else
        WRITE(LU,*) 'ERROR: AED2 LIBRARY NOT COMPILED'
        CALL PLANTE(1)
#endif
      ENDIF
!
!-----------------------------------------------------------------------
!
!     DEGRADATION LAW
!
      IF( 17*INT(WAQPROCESS/17).EQ.WAQPROCESS ) THEN
!
        DO J = 1,NWAQ_DEGRA
          ITRAC = RANK_DEGRA(J)
          IF(LOITRAC(ITRAC).EQ.1) THEN
            IF(DIMM.EQ.2)THEN
              CALL OS('X=X+CY  ',X=TIMP%ADR(ITRAC)%P,
     &             Y=HPROP,C=-2.3D0/COEF1TRAC(ITRAC)/3600.D0)
            ELSEIF(DIMM.EQ.3)THEN
              CALL OS('X=X+C   ',X=TIMP%ADR(ITRAC)%P,
     &             C=2.3D0/COEF1TRAC(ITRAC)/3600.D0)
            ENDIF
          ELSEIF(LOITRAC(ITRAC).GT.1) THEN
            WRITE(LU,21) LOITRAC(ITRAC),ITRAC
21    FORMAT(1X,'LOITRAC ',I4,' FOR TRACER ',I4,' NOT YET PROGRAMMED')
            CALL PLANTE(1)
            STOP
          ENDIF
!
          IF(DIMM.EQ.2)THEN
!            YASMI(ITRAC) = .TRUE.
          ELSEIF(DIMM.EQ.3)THEN
            TIMP%ADR(ITRAC)%P%TYPR='Q'
          ENDIF
!
        ENDDO
        FOUND = .TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     GHOST PROCESS IN WAITING FOR THE MERGE WITH ICE PROCESS
!
      IF( 19*INT(WAQPROCESS/19).EQ.WAQPROCESS ) THEN
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!      UNKNOWN PROCESS
!
      IF( .NOT.FOUND ) THEN
        WRITE(LU,20) WAQPROCESS
20    FORMAT(1X,'SOURCE_WAQ: UNKNOWN WAQ MODULE: ',I4)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
