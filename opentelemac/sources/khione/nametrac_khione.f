!                      **************************
                       SUBROUTINE NAMETRAC_KHIONE
!                      **************************
!
     &  (NAMETRAC,NTRAC,PROCESS)
!
!
!***********************************************************************
! KHIONE      V7P3
!***********************************************************************
!
!brief    Gives names to tracers added by the ice modelling component
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        11/11/2016
!+        V7P3
!+        Coupling TELEMAC-2D with KHIONE (ice modelling component)
!+        Initial developments
!
!history  F. SOUILLE (EDF)
!+        30/09/2019
!+        V8P0
!+        Fixed name and unit of frazil (conc to volumic fraction)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMETRAC |<--| ARRAY OF NAMES OF TRACERS
!| NTRAC    |-->| MODIFYING NUMBER OF TRACER IF NECESARY
!| PROCESS  |-->| ALSO ICEPROCESS, DEFINES THE ICE PROCESSES
!|          |   | - 2: HEAT EXCHANGE WITH THE ATMOSPHER
!|          |   | - 3: IMPACT OF THE ICE COVER ON THE HYDRODYNAMICS
!|          |   | - 5: CLOGGING ON RACKS
!|          |   | - 7: STATIC BORDER ICE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE
!      USE INTERFACE_KHIONE, EX_NAMETRAC_KHIONE => NAMETRAC_KHIONE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   )::  PROCESS
      INTEGER          , INTENT(INOUT)::  NTRAC
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     TODO: THIS SHOULD BE MOVED TO DECLARATIONS_KHIONE,
!     EVEN IF SOME ARE DUPLICATED IN TELEMAC
!      INTEGER :: IND_F,IND_T
!
      LOGICAL :: FOUND
!
!-----------------------------------------------------------------------
!
      FOUND = .FALSE.
!
!-----------------------------------------------------------------------
!
!     INITIALISATION
!
      IF( PROCESS.EQ.1 ) THEN
        FOUND = .TRUE.
!
        ICETR = 0
!
      ELSE IF( PROCESS*INT(ICEPROCESS/PROCESS).NE.ICEPROCESS ) THEN
        WRITE(LU,21) PROCESS,ICEPROCESS
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ADDING RELEVANT TRACERS WHEN RELEVANT
!
!     2.: THERMAL BUDGET WITH FRAZIL PRODUCTION
      IF( ( 2*INT(ICEPROCESS/2) .EQ. ICEPROCESS ).OR.
!     5.:
     &    ( 5*INT(ICEPROCESS/5) .EQ. ICEPROCESS ) ) THEN
        FOUND = .TRUE.
!
!     1. ~~> FRAZIL
        CALL ADDTRACER(NAMETRAC,NTRAC,
     &    IND_F,
     &    'FRASIL          ','FRAZIL          ','VOLUME FRACTION ')
!     2. ~~> TEMPERATURE
        CALL ADDTRACER(NAMETRAC,NTRAC,
     &    IND_T,
     &    'TEMPERATURE     ','TEMPERATURE     ','   oC           ')
!
!     3.: STATIC ICE COVER
      ELSEIF( 3*INT(ICEPROCESS/3) .EQ. ICEPROCESS ) THEN
        FOUND = .TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     UNKNOWN PROCESS
!
      IF(.NOT.FOUND ) THEN 
        WRITE(LU,20) PROCESS
        CALL PLANTE(1)
        STOP
      ENDIF
!
20    FORMAT(1X,'NAMETRAC_KHIONE: UNKNOWN ICE PROCESS: ',I4)
!
21    FORMAT(1X,'NAMETRAC_KHIONE: PROCESS CALLED ',I4,
     &       ' IS NOT A MULTIPLYING VALUE OF: ',I4)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
