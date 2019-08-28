!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief module handling all the instance function
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       V6P3
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INSTANCE_T3D
!
      USE API_HANDLE_ERROR
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_SPECIAL, ONLY : MAXKEYWORD
      USE DECLARATIONS_TELEMAC3D
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES,MAXLU_SIS
      USE DECLARATIONS_TOMAWAC, ONLY : WAC_FILES,MAXLU_WAC
      IMPLICIT NONE

      PRIVATE

      PUBLIC :: CREATE_INSTANCE_T3D
      PUBLIC :: DELETE_INSTANCE_T3D
      PUBLIC :: CHECK_INSTANCE_T3D
      PUBLIC :: GET_INSTANCE_ERROR_T3D
      PUBLIC :: INSTANCE_T3D
      PUBLIC :: INSTANCE_LIST_T3D
!

      TYPE INSTANCE_T3D
        ! RUN POSITION
        INTEGER MYPOSITION
        ! ERROR MESSAGE
        CHARACTER(LEN=200) :: ERROR_MESSAGE
        ! LIST OF ALL THE VARIABLE FOR MODEL
        TYPE(BIEF_OBJ), POINTER :: HBOR
        TYPE(BIEF_OBJ), POINTER :: UBOR2D
        TYPE(BIEF_OBJ), POINTER :: VBOR2D
!
        TYPE(BIEF_OBJ), POINTER :: UBORF
        TYPE(BIEF_OBJ), POINTER :: VBORF
        TYPE(BIEF_OBJ), POINTER :: WBORF
!
        TYPE(BIEF_OBJ), POINTER :: UBORL
        TYPE(BIEF_OBJ), POINTER :: VBORL
        TYPE(BIEF_OBJ), POINTER :: WBORL
!
        TYPE(BIEF_OBJ), POINTER :: UBORS
        TYPE(BIEF_OBJ), POINTER :: VBORS
        TYPE(BIEF_OBJ), POINTER :: WBORS
!
        TYPE(BIEF_OBJ), POINTER :: U
        TYPE(BIEF_OBJ), POINTER :: V
        TYPE(BIEF_OBJ), POINTER :: W
        DOUBLE PRECISION, POINTER :: FLUX_BOUNDARIES(:)
        DOUBLE PRECISION, POINTER :: COTIMP(:)
        DOUBLE PRECISION, POINTER :: DEBIMP(:)
        DOUBLE PRECISION, POINTER :: VITIMP(:)
!
        TYPE(BIEF_MESH), POINTER :: MESH2D
        TYPE(BIEF_MESH), POINTER :: MESH3D
!
        TYPE(BIEF_OBJ), POINTER :: LIHBOR
!
        TYPE(BIEF_OBJ), POINTER :: LIUBOF
        TYPE(BIEF_OBJ), POINTER :: LIVBOF
        TYPE(BIEF_OBJ), POINTER :: LIWBOF
!
        TYPE(BIEF_OBJ), POINTER :: LIUBOL
        TYPE(BIEF_OBJ), POINTER :: LIVBOL
        TYPE(BIEF_OBJ), POINTER :: LIWBOL
!
        TYPE(BIEF_OBJ), POINTER :: LIUBOS
        TYPE(BIEF_OBJ), POINTER :: LIVBOS
        TYPE(BIEF_OBJ), POINTER :: LIWBOS
!
        TYPE(BIEF_OBJ), POINTER :: NUMLIQ
!
        INTEGER,        POINTER :: NIT
        INTEGER,        POINTER :: LT
        DOUBLE PRECISION,POINTER :: AT
        DOUBLE PRECISION,POINTER :: DUREE
        DOUBLE PRECISION,POINTER :: DT
!
        TYPE(BIEF_FILE), POINTER :: T3D_FILES(:)
        INTEGER :: MAXLU_T3D
        INTEGER :: MAXKEYWORD
        INTEGER, POINTER :: T3DRES
        INTEGER, POINTER :: T3DGEO
        INTEGER, POINTER :: T3DCLI
!
        CHARACTER(LEN=PATH_LEN), POINTER :: COUPLING
        CHARACTER(LEN=20), POINTER :: EQUA
!
        TYPE(BIEF_OBJ), POINTER :: H
        TYPE(BIEF_OBJ), POINTER :: DH
        TYPE(BIEF_OBJ), POINTER :: TA
!
        INTEGER, POINTER :: DEBUG
        ! LIST OF ALL THE VARIABLE FOR STATE
        INTEGER, POINTER :: BND_TIDE(:)
        DOUBLE PRECISION, POINTER :: CTIDE
        DOUBLE PRECISION, POINTER :: CTIDEV
        DOUBLE PRECISION, POINTER :: MSL
        !<new_var>
!

      END TYPE ! MODEL_T3D
!
      INTEGER, PARAMETER :: MAX_INSTANCES=10
      TYPE(INSTANCE_T3D), POINTER :: INSTANCE_LIST_T3D(:)
      LOGICAL, ALLOCATABLE :: USED_INSTANCE(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief creates a TELEMAC3D instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history y audouin (edf r&d, lnhe)
      !+       21/08/2013
      !+       V6P3
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id   [out]    id of the new instance
      !param ierr [out]    0 if subroutine successfull,
      !+                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CREATE_INSTANCE_T3D(ID,IERR)
      ! initialise instance for TELEMAC3D
        INTEGER, INTENT(OUT) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
        ID = 0
        IERR = 0
        ! If first time createing an instance allocating the instance array
        IF(.NOT. ALLOCATED(USED_INSTANCE)) THEN
          ALLOCATE(USED_INSTANCE(MAX_INSTANCES),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERR_MESS = 'ERROR WHILE ALLOCATING USED INSTANCE ARRAY'
            RETURN
          ENDIF
          USED_INSTANCE = .FALSE.
          ALLOCATE(INSTANCE_LIST_T3D(MAX_INSTANCES),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERR_MESS = 'ERROR WHILE ALLOCATING INSTANCE ARRAY'
            RETURN
          ENDIF
        ENDIF
!
        ! look for the first instance available
        I = 1
        DO WHILE(USED_INSTANCE(I).AND.I.LE.MAX_INSTANCES)
          I = I + 1
        ENDDO
        ID = I
        USED_INSTANCE(ID) = .TRUE.
!
        ! if still equals 0 no available instance was found then we crash
        IF(ID.EQ.(MAX_INSTANCES+1))THEN
          IERR = MAX_INSTANCE_ERROR
          ERR_MESS = "MAX INSTANCE REACHED "
          RETURN
        ENDIF
        !
        INSTANCE_LIST_T3D(ID)%MYPOSITION = NO_POSITION
!       Link with TELEMAC3D variables
        CALL UPDATE_INSTANCE_T3D(ID,IERR)

      END SUBROUTINE CREATE_INSTANCE_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief updates a TELEMAC3D instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history c goeury & y audouin (edf r&d, lnhe)
      !+       17/06/2016
      !+       V7P1
      !+       update the api instance
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id   [out]    id of the new instance
      !param ierr [out]    0 if subroutine successfull,
      !+                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE UPDATE_INSTANCE_T3D(ID,IERR)
      ! initialise instance for TELEMAC3D
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
!       Link with TELEMAC3D variables
        INSTANCE_LIST_T3D(ID)%HBOR   => HBOR
        INSTANCE_LIST_T3D(ID)%UBOR2D  =>  UBOR2D
        INSTANCE_LIST_T3D(ID)%VBOR2D  =>  VBOR2D
        INSTANCE_LIST_T3D(ID)%UBORF   =>  UBORF
        INSTANCE_LIST_T3D(ID)%VBORF   =>  VBORF
        INSTANCE_LIST_T3D(ID)%WBORF   =>  WBORF
        INSTANCE_LIST_T3D(ID)%UBORL   =>  UBORL
        INSTANCE_LIST_T3D(ID)%VBORL   =>  VBORL
        INSTANCE_LIST_T3D(ID)%WBORL   =>  WBORL
        INSTANCE_LIST_T3D(ID)%UBORS   =>  UBORS
        INSTANCE_LIST_T3D(ID)%VBORS   =>  VBORS
        INSTANCE_LIST_T3D(ID)%WBORS   =>  WBORS
        INSTANCE_LIST_T3D(ID)%H      =>  H
        INSTANCE_LIST_T3D(ID)%DH     =>  DH
        INSTANCE_LIST_T3D(ID)%U      =>  U
        INSTANCE_LIST_T3D(ID)%V      =>  V
        INSTANCE_LIST_T3D(ID)%W      =>  W
        ! For allocatable arrays nag crashes if we try to point towards
        ! an unallocated array
        IF(ALLOCATED(FLUX_BOUNDARIES)) THEN
          INSTANCE_LIST_T3D(ID)%FLUX_BOUNDARIES => FLUX_BOUNDARIES
        ENDIF
        IF(ALLOCATED(COTIMP)) THEN
          INSTANCE_LIST_T3D(ID)%COTIMP => COTIMP
        ENDIF
        IF(ALLOCATED(DEBIMP)) THEN
          INSTANCE_LIST_T3D(ID)%DEBIMP => DEBIMP
        ENDIF
        INSTANCE_LIST_T3D(ID)%VITIMP => VITIMP
!
        INSTANCE_LIST_T3D(ID)%MESH2D => MESH2D
        INSTANCE_LIST_T3D(ID)%MESH3D => MESH3D
        INSTANCE_LIST_T3D(ID)%LIHBOR => LIHBOR
        INSTANCE_LIST_T3D(ID)%LIUBOF => LIUBOF
        INSTANCE_LIST_T3D(ID)%LIVBOF => LIVBOF
        INSTANCE_LIST_T3D(ID)%LIWBOF => LIWBOF
        INSTANCE_LIST_T3D(ID)%LIUBOL => LIUBOL
        INSTANCE_LIST_T3D(ID)%LIVBOL => LIVBOL
        INSTANCE_LIST_T3D(ID)%LIWBOL => LIWBOL
        INSTANCE_LIST_T3D(ID)%LIUBOS => LIUBOS
        INSTANCE_LIST_T3D(ID)%LIVBOS => LIVBOS
        INSTANCE_LIST_T3D(ID)%LIWBOS => LIWBOS
        INSTANCE_LIST_T3D(ID)%NUMLIQ => NUMLIQ
        INSTANCE_LIST_T3D(ID)%MAXLU_T3D = MAXLU_T3D
        INSTANCE_LIST_T3D(ID)%MAXKEYWORD = MAXKEYWORD

        INSTANCE_LIST_T3D(ID)%NIT    => NIT
        INSTANCE_LIST_T3D(ID)%LT    => LT
        INSTANCE_LIST_T3D(ID)%TA     => TA
        INSTANCE_LIST_T3D(ID)%AT     => AT
        INSTANCE_LIST_T3D(ID)%DT     => DT
        INSTANCE_LIST_T3D(ID)%DUREE   => DUREE
        INSTANCE_LIST_T3D(ID)%EQUA   => EQUA
!
        INSTANCE_LIST_T3D(ID)%T3D_FILES => T3D_FILES
        INSTANCE_LIST_T3D(ID)%T3DRES => T3DRES
        INSTANCE_LIST_T3D(ID)%T3DGEO => T3DGEO
        INSTANCE_LIST_T3D(ID)%T3DCLI => T3DCLI
        INSTANCE_LIST_T3D(ID)%MAXLU_T3D = MAXLU_T3D
        INSTANCE_LIST_T3D(ID)%MAXKEYWORD = MAXKEYWORD
!
        INSTANCE_LIST_T3D(ID)%DEBUG  => DEBUG
        IF(ALLOCATED(BND_TIDE)) THEN
          INSTANCE_LIST_T3D(ID)%BND_TIDE  => BND_TIDE
        ENDIF
        INSTANCE_LIST_T3D(ID)%CTIDE  => CTIDE
        INSTANCE_LIST_T3D(ID)%CTIDEV => CTIDEV
        INSTANCE_LIST_T3D(ID)%MSL  => MSL
        ! <new_link>
!
      END SUBROUTINE UPDATE_INSTANCE_T3D
!

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief deletes a TELEMAC3D instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history y audouin (edf r&d, lnhe)
      !+       21/08/2013
      !+       V6P3
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id    [in]    id of the instance
      !param ierr [out]    0 if subroutine successfull,
      !+                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE DELETE_INSTANCE_T3D(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        !
        CALL CHECK_INSTANCE_T3D(ID,IERR)
        IF(IERR.NE.0) RETURN
        USED_INSTANCE(ID) = .FALSE.
      END SUBROUTINE DELETE_INSTANCE_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief check if the id is following convention
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history y audouin (edf r&d, lnhe)
      !+       21/08/2013
      !+       V6P3
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id    [in]    id of the instance
      !param ierr [out]    0 if subroutine successfull,
      !+                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_INSTANCE_T3D(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        IF(ID.LE.0 .OR. ID.GT.MAX_INSTANCES) THEN
          IERR = INVALID_INSTANCE_NUM_ERROR
          ERR_MESS = 'INVALID INSTANCE NUMBER'
          RETURN
        ENDIF
        IF(.NOT.USED_INSTANCE(ID)) THEN
          IERR = UNUSED_INSTANCE_ERROR
          ERR_MESS = 'INSTANCE NUMBER WAS NOT CREATED'
          RETURN
        ENDIF
        CALL UPDATE_INSTANCE_T3D(ID,IERR)
      END SUBROUTINE CHECK_INSTANCE_T3D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history y audouin (edf r&d, lnhe)
      !+       21/08/2013
      !+       V6P3
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id    [in]    id of the instance
      !param mess  [out]   The erro message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INSTANCE_ERROR_T3D(ID,MESS)
        INTEGER, INTENT(IN) :: ID
        CHARACTER(LEN=200), INTENT(OUT) :: MESS
!
        MESS = INSTANCE_LIST_T3D(ID)%ERROR_MESSAGE
!
      END SUBROUTINE GET_INSTANCE_ERROR_T3D
      END MODULE API_INSTANCE_T3D
