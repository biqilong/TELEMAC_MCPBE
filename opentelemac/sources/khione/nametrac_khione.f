!                     **************************
                      SUBROUTINE NAMETRAC_KHIONE
!                     **************************
!
     &  (NAMETRAC,NTRAC)
!
!
!***********************************************************************
! KHIONE      V7P3
!***********************************************************************
!
!brief    Add tracers needed for the modelling of frazil ice
!
!history  F. SOUILLE (EDF)
!+        30/09/2019
!+        V8P0
!+        Fixed name and unit of frazil (conc to volumic fraction)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMETRAC |<->| ARRAY OF NAMES OF TRACERS
!| NTRAC    |<->| MODIFYING NUMBER OF TRACER IF NECESARY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE
      USE DECLARATIONS_TELEMAC
!      USE INTERFACE_KHIONE, EX_NAMETRAC_KHIONE => NAMETRAC_KHIONE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(INOUT)::  NTRAC
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: IFRA,I,K
      CHARACTER(LEN=2)   :: CHAR2
      LOGICAL :: FOUND
!
!-----------------------------------------------------------------------
!
      FOUND = .FALSE.
!
!-----------------------------------------------------------------------
!
!     ADDING TRACERS WHEN RELEVANT
!
!     THERMAL BUDGET
      IF( (THERMAL_BUDGET) .OR. (CLOGGING) ) THEN
        FOUND = .TRUE.
!
!   ~~> TEMPERATURE
        CALL ADDTRACER(NAMETRAC,NTRAC,
     &    IND_T,.TRUE.,
     &    'TEMPERATURE     ',
     &    'TEMPERATURE     ',
     &    '   oC           ')
!
!   ~~> SALINITY
        IF(SALINITY) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,
     &      IND_S,.TRUE.,
     &      'SALINITE        ',
     &      'SALINITY        ',
     &      '   ppt          ' )
        ENDIF
!
!   ~~> FRAZIL MULTI-CLASS
        IFRA = 1
        IF(NC_FRA.GT.1) THEN
!
          WRITE(LU,*) 'MULTI-CLASS IS NOT VALIDATED YET',
     &                'USE IT AT YOUR OWN RISK'
!
          IND_FRA = NTRAC+1
          DO I=1,NC_FRA
            WRITE(CHAR2,'(I2)') IFRA
            IFRA = IFRA + 1
            CALL ADDTRACER(NAMETRAC,NTRAC,K,.TRUE.,
     &        'FRASIL ' //ADJUSTL(CHAR2)//'       ',
     &        'FRAZIL ' //ADJUSTL(CHAR2)//'       ',
     &        'VOLUME FRACTION ')
          ENDDO

!   ~~> FRAZIL MONO-CLASS
        ELSE
          CALL ADDTRACER(NAMETRAC,NTRAC,
     &      IND_FRA,.TRUE.,
     &      'FRASIL          ',
     &      'FRAZIL          ',
     &      'VOLUME FRACTION ')
        ENDIF
!
!   ~~> FRAZIL PRECIPITATION
        IF(PREC) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,
     &      IND_PRE,.TRUE.,
     &      'FRASIL PRECIPITA',
     &      'FRAZIL PRECIPITA',
     &      'SURFAC FRACTION ')
          CALL ADDTRACER(NAMETRAC,NTRAC,
     &      IND_THI,.TRUE.,
     &      'EPAISSEUR COUV. ',
     &      'COVER THICKNESS ',
     &      '   M            ')
        ENDIF
!
!     STATIC ICE COVER
      ELSEIF( (ICOVER_IMPACT) .OR. (BD_ICE) ) THEN
!
        FOUND = .TRUE.
!
!   ~~> TEMPERATURE
        CALL ADDTRACER(NAMETRAC,NTRAC,
     &    IND_T,.TRUE.,
     &    'TEMPERATURE     ',
     &    'TEMPERATURE     ',
     &    '   oC           ')
!
!   ~~> SALINITY
        IF(SALINITY) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,
     &      IND_S,.TRUE.,
     &      'SALINITE        ',
     &      'SALINITY        ',
     &      '   ppt          ' )
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     UNKNOWN PROCESS
!
      IF(.NOT.FOUND ) THEN
        WRITE(LU,*) 'NAMETRAC_KHIONE: NO ACTIVE PROCESSES'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
