!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief $function to control TELEMAC3D execution
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       V6P3
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_RUN_T3D
!
      USE BIEF !, ONLY : BIEF_OPEN_FILES, BIEF_INIT, INCLUS
      USE API_HANDLE_ERROR
      USE API_HANDLE_VAR_T3D
      USE API_INSTANCE_T3D
      USE INTERFACE_TELEMAC3D !, ONLY : TELEMAC3D, LECDON_TELEMAC3D
      USE DECLARATIONS_PARALLEL, ONLY : COMM
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D
      USE DECLARATIONS_TELEMAC2D, ONLY:WINDX,WINDY
      USE INTERFACE_WAQTEL
!
      USE DECLARATIONS_SPECIAL
      
      IMPLICIT NONE
      PRIVATE
      ! COMMON VALUES TO DEFINE OUTPUT + LANGUAGE
!
      INTEGER TDEB(8),TFIN(8),NCAR,NIT_ORI
      DOUBLE PRECISION :: DT_ORI
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC3D               '
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
      CHARACTER(LEN=24), PARAMETER :: CODE4='WAQTEL                  '
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH

!
! List the public subroutines
!
      PUBLIC :: RUN_SET_CONFIG_T3D_D
      PUBLIC :: RUN_READ_CASE_T3D_D
      PUBLIC :: RUN_ALLOCATION_T3D_D
      PUBLIC :: RUN_INIT_T3D_D
      PUBLIC :: RUN_TIMESTEP_T3D_D
      PUBLIC :: RUN_FINALIZE_T3D_D

      CONTAINS
      ! SET THE LU AND LNG VALUES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE INSTANCE AND SET THE OUTPUT
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST [IN,OUT]    THE INSTANCE
      !PARAM LU       [IN]    OUTPUT STREAM ID
      !PARAM LNG      [IN]    OUTPUT KANGUAGE 2 ENGLISH 1 FRENCH
      !PARAM COMM     [IN]    THE MPI COMMUNICATOR (-1 IF NONE)
      !PARAM STD_OUTPUT[IN]    IF false listing in file
      !PARAM IERR    [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                      ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_T3D_D(INST, U_LU, U_LNG, U_COMM,
     &                                U_STD_OUTPUT, IERR)
        TYPE(INSTANCE_T3D),  INTENT(INOUT) :: INST
        INTEGER,             INTENT(IN) :: U_LU, U_LNG, U_COMM
        LOGICAL,             INTENT(IN) :: U_STD_OUTPUT
        INTEGER,             INTENT(OUT) :: IERR
!
        IERR = 0
!
        LU = U_LU
        LNG = U_LNG
        COMM = U_COMM
        STD_OUTPUT = U_STD_OUTPUT
!
      END SUBROUTINE RUN_SET_CONFIG_T3D_D
!
!!!!!!! FUNCTION HANDLING THE EXECUTION OF THE SIMULATION
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF READS THE CASE FILE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM CAS_FILE   [IN]    PATH TO THE CASE FILE
      !PARAM DICO_FILE  [IN]    PATH TO THE DICTIONARY FILE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_T3D_D(INST,CAS_FILE, DICO_FILE,INIT,IERR,
     &                              WAQ_CAS_FILE,WAQ_DICO_FILE)

        TYPE(INSTANCE_T3D), INTENT(INOUT) :: INST
        CHARACTER(LEN=250), INTENT(IN) :: CAS_FILE
        CHARACTER(LEN=250), INTENT(IN) :: DICO_FILE
        CHARACTER(LEN=250), OPTIONAL, INTENT(IN) :: WAQ_CAS_FILE
        CHARACTER(LEN=250), OPTIONAL, INTENT(IN) :: WAQ_DICO_FILE
        LOGICAL,            INTENT(IN) :: INIT
        INTEGER,            INTENT(OUT) :: IERR
!
        CHARACTER(LEN=250) MOTCAR(INST%MAXKEYWORD)
        CHARACTER(LEN=250) FILE_DESC(4,INST%MAXKEYWORD)
        CHARACTER(LEN=250) WAQ_FILE_DESC(4,INST%MAXKEYWORD)

        LOGICAL :: IS_COUPLED
!
        IERR = 0
!
        CALL BIEF_INIT(PATH,NCAR,INIT)
!
!       INITIAL TIME FOR COMPUTATION DURATION
!
        CALL DATE_AND_TIME(VALUES=TDEB)
!
!       PRINTS BANNER TO LISTING
!
        CALL PRINT_HEADER(CODE1,'                        ')
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
        CALL LECDON_TELEMAC3D(MOTCAR,FILE_DESC,
     &                       PATH,NCAR,CAS_FILE,DICO_FILE)

        IS_COUPLED = INCLUS(INST%COUPLING,'WAQTEL')

        IF(IS_COUPLED.AND.(PRESENT(WAQ_CAS_FILE)
     &                    .AND.PRESENT(WAQ_DICO_FILE))) THEN
!
          CALL PRINT_HEADER(CODE4,CODE1)
!
          CALL LECDON_WAQTEL(WAQ_FILE_DESC,PATH,NCAR,WAQ_CAS_FILE,
     &         WAQ_DICO_FILE)
        ELSE
          IERR = FILE_NOT_FOUND_ERROR
          ERR_MESS = 'THE COUPLING CAS AND DICO FILES ARE MISSING FOR'
     &               //TRIM(CODE4)
!
        ENDIF  
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE RUN_READ_CASE_T3D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF ALLOCATE ALL OF TELEMAC3D VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_T3D_D(INST,IERR)
        TYPE(INSTANCE_T3D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: IFLOT
        LOGICAL :: IS_COUPLED
        CHARACTER(LEN=250) MOTCAR(INST%MAXKEYWORD)
        CHARACTER(LEN=250) FILE_DESC(4,INST%MAXKEYWORD)
!
!       OPENS THE FILES FOR TELEMAC3D
!
        IERR = 0
!
        IFLOT = 0

        IS_COUPLED = INCLUS(INST%COUPLING,'WAQTEL')
!
        CALL BIEF_OPEN_FILES(CODE1,INST%T3D_FILES,
     &                       INST%MAXLU_T3D,
     &                       PATH,NCAR,
     &                       1,.TRUE.)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY
!
        CALL POINT_TELEMAC3D
!-----------------------------------------------------------------------
!
!     INITIALISES WAQTEL
!
      IF(IS_COUPLED) THEN
!
        CALL BIEF_OPEN_FILES(CODE4,INST%WAQ_FILES,INST%MAXLU_WAQ,PATH,
     &                       NCAR,4,.TRUE.)
!
        CALL CONFIG_CODE(1)
!
!     MEMORY ORGANISATION
!
        CALL POINT_WAQTEL(MESH2D,IELM2H,VENT,WINDX,WINDY,
     &                    ATMOS,PATMOS,MESH3D,IELM3)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!
      END SUBROUTINE RUN_ALLOCATION_T3D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE TELEMAC3D VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_T3D_D(INST,IERR)
!
        TYPE(INSTANCE_T3D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        NIT_ORI = INST%NIT
        DT_ORI = INST%DT
        CALL TELEMAC3D(PASS=0,NIT_ORI=NIT_ORI)
!
      END SUBROUTINE RUN_INIT_T3D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF RUN A TIMESTEP IN TELEMAC3D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_T3D_D(INST,IERR)
!
        TYPE(INSTANCE_T3D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        INST%NIT = INST%LT
        CALL TELEMAC3D(PASS=1,NIT_ORI=NIT_ORI)
      END SUBROUTINE RUN_TIMESTEP_T3D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF FINALIZE A TELEMAC3D RUN
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE RUN_FINALIZE_T3D_D(INST,IERR)
!
        TYPE(INSTANCE_T3D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        CALL BIEF_CLOSE_FILES(INST%T3D_FILES,
     &                        INST%MAXLU_T3D,.FALSE.)
!
!       DEALLOCATE ALL OF BIEF AND TELEMAC3D ARRAYS
        CALL DEALL_TELEMAC3D()
        CALL DEALL_BIEF()
!
        WRITE(LU,11)
11      FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!     TIME OF END OF COMPUTATION
!
        CALL DATE_AND_TIME(VALUES=TFIN)
        CALL ELAPSE(TDEB,TFIN)
        ! Closing log files
        IF(IPID.NE.0) THEN
          CLOSE(LU)
        ELSE
          IF(.NOT.STD_OUTPUT) CLOSE(LU)
        ENDIF
!
      END SUBROUTINE RUN_FINALIZE_T3D_D
!
      END MODULE API_RUN_T3D
