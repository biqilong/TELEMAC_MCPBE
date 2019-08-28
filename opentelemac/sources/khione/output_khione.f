!                     ************************
                      SUBROUTINE OUTPUT_KHIONE
!                     ************************
!
     &(TITLE,NPOIN,AT,LT,MESH,TELSOR,DATE,MARTIM,ISHEAD,ISMESH,ISVARS)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    "WRAPPER" FOR WRITE_HEADER, WRITE_MESH AND DESIMP SO THAT
!+            OUTPUTS CAN BE DONE FROM WITHIN TELEMAC-2D WHEN USING
!+            THE COUPLED MODEL RATHER THAN CONFLICTING PRINTOUT
!+            PERIODS, VARSOR, MAXVAR, ETC.
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        11/11/2016
!+        V7P3
!+        Coupling TELEMAC-2D with KHIONE (ice modelling component)
!+        Initial developments
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| CURRENT TIME IN SECONDS
!| LT             |-->| CURRENT NUMBER OF OF TIME STEP
!| NPOIN          |-->| NUMBER OF NODES
!| ISHEAD         !-->| 1: WRITE HEADER; 0: DOES NOT WRITE HEADER
!| ISMESH         !-->| 1: WRITE MESH; 0: DOES NOT WRITE MESH
!| ISVARS         !-->| 1: WRITE VARS; 0: DOES NOT WRITE VARS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=72),     INTENT(IN) :: TITLE
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
      TYPE(BIEF_OBJ),        INTENT(IN) :: TELSOR
      INTEGER, DIMENSION(3), INTENT(IN) :: DATE
      INTEGER, DIMENSION(3), INTENT(IN) :: MARTIM
      DOUBLE PRECISION,      INTENT(IN) :: AT
      INTEGER,               INTENT(IN) :: LT,NPOIN
      LOGICAL,               INTENT(IN) :: ISHEAD,ISMESH,ISVARS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     PREPARES THE RESULTS FILE FOR KHIONE
!
      IF( ICE_FILES(ICERES)%NAME(1:1).EQ.' ' ) RETURN
!     ______________________________________________________________
!     HEADER
      IF( ISHEAD ) THEN
!
        CALL WRITE_HEADER(
     &    ICE_FILES(ICERES)%FMT,  ! RESULTS FILE FORMAT
     &    ICE_FILES(ICERES)%LU,   ! LU FOR RESULTS FILE
     &    TITLE,                  ! TITLE
     &    MAXVAR,                 ! MAX NUMBER OF OUTPUT VARIABLES
     &    TEXTE,                  ! NAMES OF OUTPUT VARIABLES
     &    SORLEO)                 ! PRINT TO FILE OR NOT
!
      ENDIF
!
!     ______________________________________________________________
!     MESH
      IF( ISMESH ) THEN
!
        CALL WRITE_MESH(
     &    ICE_FILES(ICERES)%FMT,  ! RESULTS FILE FORMAT
     &    ICE_FILES(ICERES)%LU,   ! LU FOR RESULTS FILE
     &    MESH,
     &    1,                      ! NUMBER OF PLANES /NA/
     &    DATE,                   ! START DATE
     &    MARTIM,                 ! START TIME
     &    T1,T2,                  ! TEMPORARY ARRAYS
     &    NCSIZE.GT.1, NPTIR)
!
      ENDIF
!
!     ______________________________________________________________
!     TIME VARIABLES
      IF( ISVARS ) THEN
!
!       PREPARES THE RESULTS
        CALL PRERES_KHIONE(NPOIN,AT,LT,TELSOR)
!
!       OUTPUTS A STANDARD TIME STEP
        CALL BIEF_DESIMP(
     &    ICE_FILES(ICERES)%FMT,  ! RESULTS FILE FORMAT
     &    VARSOR,                 ! POINTERS TO CORE VARIABLES
     &    NPOIN,                  ! NUMBER OF POINTS IN THE LOCAL MESH
     &    ICE_FILES(ICERES)%LU,   ! LU FOR RESULTS FILE
     &    AT,LT,
     &    LISPRD,LEOPRD,
     &    SORLEO,SORIMP,
     &    MAXVAR,TEXTE,0,0 )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
