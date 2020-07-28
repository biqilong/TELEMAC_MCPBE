!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@brief Getter/setter of telemac2d variables
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_HANDLE_VAR_T2D

        USE API_HANDLE_ERROR
        USE API_INSTANCE_T2D
        IMPLICIT NONE
        !> Size of the string containing the name of a variable
        INTEGER, PARAMETER :: T2D_VAR_LEN=40
        !> Size of the string containing the type of a variable
        INTEGER, PARAMETER :: T2D_TYPE_LEN=12
        !> Size of the string containing the information about a variable
        INTEGER, PARAMETER :: T2D_INFO_LEN=200
        !> The maximum number of variables
        INTEGER, PARAMETER :: NB_VAR_T2D=83
        !> List of variable names
        CHARACTER(LEN=40),ALLOCATABLE,DIMENSION(:) :: VNAME_T2D
        !> List of variable info
        CHARACTER(LEN=200),ALLOCATABLE,DIMENSION(:) :: VINFO_T2D
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double variable from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Contains the read value
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_T2D_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_T2D),         INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN) :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALEUR = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          VALEUR = INST%HBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          VALEUR = INST%UBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          VALEUR = INST%VBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VALEUR = INST%MESH%XNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VALEUR = INST%MESH%YNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          VALEUR = INST%H%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          VALEUR = INST%DH%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VALEUR = INST%ZF%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          VALEUR = INST%U%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          VALEUR = INST%V%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR = INST%MESH%X%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR = INST%MESH%Y%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          VALEUR = INST%FLUX_BOUNDARIES(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          VALEUR = INST%TE5%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          VALEUR = INST%COTE(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VALEUR = INST%CHESTR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
          VALEUR = INST%AT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DT') THEN
          VALEUR = INST%DT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TMAX') THEN
          VALEUR = INST%TMAX
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          VALEUR = INST%DEBIT(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALRANGE') THEN
          VALEUR = INST%CTIDE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALVELOCITY') THEN
          VALEUR = INST%CTIDEV
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SEALEVEL') THEN
          VALEUR = INST%MSL
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FAIR') THEN
          VALEUR = INST%FAIR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AK') THEN
          VALEUR =  INST%AK%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EP') THEN
          VALEUR = INST%EP%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INIT_DEPTH') THEN
          VALEUR = INST%H0%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TRACER') THEN
          VALEUR = INST%T%ADR(INDEX1)%P%R(INDEX2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          VALEUR = INST%Q%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DCLA') THEN
          VALEUR = INST%DCLA(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          VALEUR = INST%AC(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          VALEUR = INST%XWC(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VALEUR = INST%Z%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          VALEUR = INST%QBOR%ADR(INDEX1)%P%R(INDEX2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          VALEUR = INST%EBOR%ADR(INDEX1)%P%R(INDEX2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VALEUR = INST%FLBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VALEUR = INST%TOB%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          VALEUR = INST%CS%ADR(INDEX1)%P%R(INDEX2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VALEUR = INST%E%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PARTHENIADES') THEN
          VALEUR = INST%PARTHENIADES(INDEX1,INDEX2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VOLU2D') THEN
          VALEUR = INST%VOLU2D%R(INDEX1)
        ! <get_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_T2D_D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a double variable of telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_T2D_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          INST%HBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          INST%UBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          INST%VBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          INST%H%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          INST%DH%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          INST%ZF%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          INST%U%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          INST%V%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          INST%FLUX_BOUNDARIES(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          INST%TE5%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          INST%COTE(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          INST%DEBIT(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          INST%CHESTR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
          INST%AT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DT') THEN
          INST%DT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALRANGE') THEN
          INST%CTIDE = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALVELOCITY') THEN
          INST%CTIDEV = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SEALEVEL') THEN
          INST%MSL = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FAIR') THEN
          INST%FAIR = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AK') THEN
          INST%AK%R(INDEX1)=VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EP') THEN
          INST%EP%R(INDEX1)=VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INIT_DEPTH') THEN
          INST%H0%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TRACER') THEN
          INST%T%ADR(INDEX1)%P%R(INDEX2) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          INST%Q%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DCLA') THEN
          INST%DCLA(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          INST%AC(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          INST%XWC(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          INST%Z%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          INST%QBOR%ADR(INDEX1)%P%R(INDEX2) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          INST%EBOR%ADR(INDEX1)%P%R(INDEX2) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          INST%FLBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          INST%TOB%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          INST%CS%ADR(INDEX1)%P%R(INDEX2) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          INST%E%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PARTHENIADES') THEN
          INST%PARTHENIADES(INDEX1,INDEX2) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VOLU2D') THEN
          INST%VOLU2D%R(INDEX1) = VALEUR
        ! <set_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a double array from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_ARRAY_T2D_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        TYPE(INSTANCE_T2D),         INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN) :: VARNAME
        INTEGER,                    INTENT(IN) :: DIM1
        DOUBLE PRECISION,           INTENT(INOUT):: VALEUR(*)
        INTEGER,                    INTENT(OUT):: IERR
        INTEGER, OPTIONAL,          INTENT(IN) :: BLOCK_INDEX
!
        IERR = 0
!
        IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          VALEUR(1:INST%HBOR%DIM1) = INST%HBOR%R(1:INST%HBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          VALEUR(1:INST%UBOR%DIM1) = INST%UBOR%R(1:INST%UBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          VALEUR(1:INST%VBOR%DIM1) = INST%VBOR%R(1:INST%VBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VALEUR(1:INST%MESH%XNEBOR%DIM1) =
     &        INST%MESH%XNEBOR%R(1:INST%MESH%XNEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VALEUR(1:INST%MESH%YNEBOR%DIM1) =
     &        INST%MESH%YNEBOR%R(1:INST%MESH%YNEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          VALEUR(1:INST%H%DIM1) = INST%H%R(1:INST%H%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          VALEUR(1:INST%DH%DIM1) = INST%DH%R(1:INST%DH%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VALEUR(1:INST%ZF%DIM1) = INST%ZF%R(1:INST%ZF%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          VALEUR(1:INST%U%DIM1) = INST%U%R(1:INST%U%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          VALEUR(1:INST%V%DIM1) = INST%V%R(1:INST%V%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR(1:INST%MESH%X%DIM1) = INST%MESH%X%R(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR(1:INST%MESH%Y%DIM1) = INST%MESH%Y%R(1:INST%MESH%Y%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          VALEUR(1:SIZE(INST%FLUX_BOUNDARIES)) =
     &        INST%FLUX_BOUNDARIES(1:SIZE(INST%FLUX_BOUNDARIES))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          VALEUR(1:INST%TE5%DIM1) = INST%TE5%R(1:INST%TE5%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          VALEUR(1:SIZE(INST%COTE)) = INST%COTE(1:SIZE(INST%COTE))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VALEUR(1:INST%CHESTR%DIM1) = INST%CHESTR%R(1:INST%CHESTR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          VALEUR(1:SIZE(INST%DEBIT)) = INST%DEBIT(1:SIZE(INST%DEBIT))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INIT_DEPTH') THEN
          VALEUR(1:SIZE(INST%H0%R)) =
     &          INST%H0%R(1:SIZE(INST%H0%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TRACER') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
          VALEUR(1:INST%T%ADR(BLOCK_INDEX)%P%DIM1) =
     &  INST%T%ADR(BLOCK_INDEX)%P%R(1:INST%T%ADR(BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          VALEUR(1:SIZE(INST%Q%R)) =
     &     INST%Q%R(1:SIZE(INST%Q%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DCLA') THEN
          VALEUR(1:SIZE(INST%DCLA)) =
     &     INST%DCLA(1:SIZE(INST%DCLA))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          VALEUR(1:SIZE(INST%AC)) =
     &     INST%AC(1:SIZE(INST%AC))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          VALEUR(1:SIZE(INST%XWC)) =
     &     INST%XWC(1:SIZE(INST%XWC))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VALEUR(1:SIZE(INST%Z%R)) =
     &     INST%Z%R(1:SIZE(INST%Z%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            VALEUR(1:SIZE(INST%QBOR%ADR(BLOCK_INDEX)%P%R)) =
     &       INST%QBOR%ADR(BLOCK_INDEX)%P%R
     &       (1:SIZE(INST%QBOR%ADR(BLOCK_INDEX)%P%R))
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BLOCK INDEX IS MISSING FOR'//TRIM(VARNAME)
          ENDIF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            VALEUR(1:SIZE(INST%EBOR%ADR(BLOCK_INDEX)%P%R)) =
     &       INST%EBOR%ADR(BLOCK_INDEX)%P%R
     &       (1:SIZE(INST%EBOR%ADR(BLOCK_INDEX)%P%R))
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BLOCK INDEX IS MISSING FOR'//TRIM(VARNAME)
          ENDIF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VALEUR(1:SIZE(INST%FLBOR%R)) =
     &     INST%FLBOR%R(1:SIZE(INST%FLBOR%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VALEUR(1:SIZE(INST%TOB%R)) =
     &     INST%TOB%R(1:SIZE(INST%TOB%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            VALEUR(1:SIZE(INST%CS%ADR(BLOCK_INDEX)%P%R)) =
     &       INST%CS%ADR(BLOCK_INDEX)%P%R
     &       (1:SIZE(INST%CS%ADR(BLOCK_INDEX)%P%R))
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BLOCK INDEX IS MISSING FOR'//TRIM(VARNAME)
          ENDIF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VALEUR(1:SIZE(INST%E%R)) =
     &     INST%E%R(1:SIZE(INST%E%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VOLU2D') THEN
          VALEUR(1:SIZE(INST%VOLU2D%R)) =
     &     INST%VOLU2D%R(1:SIZE(INST%VOLU2D%R))
        ! <get_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_ARRAY_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Set a double array from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_ARRAY_T2D_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR, BLOCK_INDEX)
!
        TYPE(INSTANCE_T2D),         INTENT(INOUT) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN) :: VARNAME
        INTEGER,                    INTENT(IN) :: DIM1
        DOUBLE PRECISION,           INTENT(IN):: VALEUR(DIM1)
        INTEGER,                    INTENT(OUT):: IERR
        INTEGER, OPTIONAL,          INTENT(IN) :: BLOCK_INDEX
!
        IERR = 0
!
        IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          INST%HBOR%R(1:INST%HBOR%DIM1) = VALEUR(1:INST%HBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          INST%UBOR%R(1:INST%UBOR%DIM1) = VALEUR(1:INST%UBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          INST%VBOR%R(1:INST%VBOR%DIM1) = VALEUR(1:INST%VBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          INST%MESH%XNEBOR%R(1:INST%MESH%XNEBOR%DIM1) =
     &    VALEUR(1:INST%MESH%XNEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          INST%MESH%YNEBOR%R(1:INST%MESH%YNEBOR%DIM1) =
     &    VALEUR(1:INST%MESH%YNEBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          INST%H%R(1:INST%H%DIM1) = VALEUR(1:INST%H%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          INST%DH%R(1:INST%DH%DIM1) = VALEUR(1:INST%DH%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          INST%ZF%R(1:INST%ZF%DIM1) = VALEUR(1:INST%ZF%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          INST%U%R(1:INST%U%DIM1) = VALEUR(1:INST%U%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          INST%V%R(1:INST%V%DIM1) = VALEUR(1:INST%V%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(1:INST%MESH%X%DIM1) = VALEUR(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%Y%R(1:INST%MESH%Y%DIM1) = VALEUR(1:INST%MESH%Y%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          INST%FLUX_BOUNDARIES(1:SIZE(INST%FLUX_BOUNDARIES)) =
     &    VALEUR(1:SIZE(INST%FLUX_BOUNDARIES))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          INST%TE5%R(1:INST%TE5%DIM1) = VALEUR(1:INST%TE5%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          INST%COTE(1:SIZE(INST%COTE)) = VALEUR(1:SIZE(INST%COTE))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          INST%CHESTR%R(1:INST%CHESTR%DIM1) = VALEUR(1:INST%CHESTR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          INST%DEBIT(1:SIZE(INST%DEBIT)) = VALEUR(1:SIZE(INST%DEBIT))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INIT_DEPTH') THEN
          INST%H0%R(1:SIZE(INST%H0%R)) =
     &          VALEUR(1:SIZE(INST%H0%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TRACER') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
        INST%T%ADR(BLOCK_INDEX)%P%R(1:INST%T%ADR(BLOCK_INDEX)%P%DIM1) =
     &            VALEUR(1:INST%T%ADR(BLOCK_INDEX)%P%DIM1)
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BOCK NUMBER IS MISSING FOR'//TRIM(VARNAME)
          END IF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          INST%Q%R(1:SIZE(INST%Q%R)) =
     &     VALEUR(1:SIZE(INST%Q%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DCLA') THEN
          INST%DCLA(1:SIZE(INST%DCLA)) =
     &     VALEUR(1:SIZE(INST%DCLA))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          INST%AC(1:SIZE(INST%AC)) =
     &     VALEUR(1:SIZE(INST%AC))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          INST%XWC(1:SIZE(INST%XWC)) =
     &     VALEUR(1:SIZE(INST%XWC))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          INST%Z%R(1:SIZE(INST%Z%R)) =
     &     VALEUR(1:SIZE(INST%Z%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            INST%QBOR%ADR(BLOCK_INDEX)%P%R
     &       (1:SIZE(INST%QBOR%ADR(BLOCK_INDEX)%P%R))=
     &       VALEUR(1:SIZE(INST%QBOR%ADR(BLOCK_INDEX)%P%R))
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BLOCK INDEX IS MISSING FOR'//TRIM(VARNAME)
          ENDIF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            INST%EBOR%ADR(BLOCK_INDEX)%P%R
     &       (1:SIZE(INST%EBOR%ADR(BLOCK_INDEX)%P%R))=
     &       VALEUR(1:SIZE(INST%EBOR%ADR(BLOCK_INDEX)%P%R))
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BLOCK INDEX IS MISSING FOR'//TRIM(VARNAME)
          ENDIF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          INST%FLBOR%R(1:SIZE(INST%FLBOR%R)) =
     &     VALEUR(1:SIZE(INST%FLBOR%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          INST%TOB%R(1:SIZE(INST%TOB%R)) =
     &     VALEUR(1:SIZE(INST%TOB%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          IF(PRESENT(BLOCK_INDEX))THEN
            INST%CS%ADR(BLOCK_INDEX)%P%R
     &       (1:SIZE(INST%CS%ADR(BLOCK_INDEX)%P%R))=
     &       VALEUR(1:SIZE(INST%CS%ADR(BLOCK_INDEX)%P%R))
          ELSE
            IERR = INDEX_BLOCK_MISSING
            ERR_MESS = 'THE BLOCK INDEX IS MISSING FOR'//TRIM(VARNAME)
          ENDIF
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          INST%E%R(1:SIZE(INST%E%R)) =
     &     VALEUR(1:SIZE(INST%E%R))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VOLU2D') THEN
          INST%VOLU2D%R(1:SIZE(INST%VOLU2D%R)) = 
     &     VALEUR(1:SIZE(INST%VOLU2D%R))
        ! <set_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_ARRAY_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer array from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Contains the read value
      !>@param[in] DIM1 Size of valeur
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_ARRAY_T2D_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(OUT) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VALEUR(1:INST%LIHBOR%DIM1) = INST%LIHBOR%I(1:INST%LIHBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          VALEUR(1:INST%LIUBOR%DIM1) = INST%LIUBOR%I(1:INST%LIUBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          VALEUR(1:INST%LIVBOR%DIM1) = INST%LIVBOR%I(1:INST%LIVBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
          VALEUR(1:INST%MESH%KP1BOR%DIM1) =
     &         INST%MESH%KP1BOR%I(1:INST%MESH%KP1BOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
          VALEUR(1:INST%NUMLIQ%DIM1) = INST%NUMLIQ%I(1:INST%NUMLIQ%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBOR') THEN
          VALEUR(1:INST%MESH%NBOR%DIM1) =
     &         INST%MESH%NBOR%I(1:INST%MESH%NBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR(1:SIZE(INST%MESH%IKLE%I)) =
     &       INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VALEUR(1:SIZE(INST%MESH%NACHB%I)) =
     &         INST%MESH%NACHB%I(1:SIZE(INST%MESH%NACHB%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          VALEUR(1:INST%MESH%KNOLG%DIM1) =
     &         INST%MESH%KNOLG%I(1:INST%MESH%KNOLG%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE') THEN
          VALEUR(1:SIZE(INST%BND_TIDE)) =
     &         INST%BND_TIDE(1:SIZE(INST%BND_TIDE))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VALEUR(1:SIZE(INST%CLU%I)) =
     &     INST%CLU%I(1:SIZE(INST%CLU%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VALEUR(1:SIZE(INST%CLV%I)) =
     &     INST%CLV%I(1:SIZE(INST%CLV%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VALEUR(1:SIZE(INST%LIQBOR%I)) =
     &     INST%LIQBOR%I(1:SIZE(INST%LIQBOR%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VALEUR(1:SIZE(INST%LIEBOR%I)) =
     &     INST%LIEBOR%I(1:SIZE(INST%LIEBOR%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARDAT') THEN
          VALEUR(1:SIZE(INST%MARDAT)) =
     &     INST%MARDAT(1:SIZE(INST%MARDAT))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARTIM') THEN
          VALEUR(1:SIZE(INST%MARTIM)) =
     &     INST%MARTIM(1:SIZE(INST%MARTIM))
        ! <get_integer_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_INTEGER_ARRAY_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Set an integer array from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] DIM1 Size of valeur
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_ARRAY_T2D_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(IN) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          INST%LIHBOR%I(1:INST%LIHBOR%DIM1) = VALEUR(1:INST%LIHBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          INST%LIUBOR%I(1:INST%LIUBOR%DIM1) = VALEUR(1:INST%LIUBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          INST%LIVBOR%I(1:INST%LIVBOR%DIM1) = VALEUR(1:INST%LIVBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
          INST%MESH%KP1BOR%I(1:INST%MESH%KP1BOR%DIM1) =
     &    VALEUR(1:INST%MESH%KP1BOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
          INST%NUMLIQ%I(1:INST%NUMLIQ%DIM1) = VALEUR(1:INST%NUMLIQ%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBOR') THEN
          INST%MESH%NBOR%I(1:INST%MESH%NBOR%DIM1) =
     &    VALEUR(1:INST%MESH%NBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I)) =
     &    VALEUR(1:SIZE(INST%MESH%IKLE%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          INST%MESH%NACHB%I(1:SIZE(INST%MESH%NACHB%I)) =
     &    VALEUR(1:SIZE(INST%MESH%NACHB%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          INST%MESH%KNOLG%I(1:INST%MESH%KNOLG%DIM1) =
     &    VALEUR(1:INST%MESH%KNOLG%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE') THEN
          INST%BND_TIDE(1:SIZE(INST%BND_TIDE)) =
     &    VALEUR(1:SIZE(INST%BND_TIDE))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          INST%CLU%I(1:SIZE(INST%CLU%I)) =
     &     VALEUR(1:SIZE(INST%CLU%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          INST%CLV%I(1:SIZE(INST%CLV%I)) =
     &     VALEUR(1:SIZE(INST%CLV%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          INST%LIQBOR%I(1:SIZE(INST%LIQBOR%I)) =
     &     VALEUR(1:SIZE(INST%LIQBOR%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          INST%LIEBOR%I(1:SIZE(INST%LIEBOR%I)) =
     &     VALEUR(1:SIZE(INST%LIEBOR%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARDAT') THEN
          INST%MARDAT(1:SIZE(INST%MARDAT)) =
     &     VALEUR(1:SIZE(INST%MARDAT))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARTIM') THEN
          INST%MARTIM(1:SIZE(INST%MARTIM)) =
     &     VALEUR(1:SIZE(INST%MARTIM))
        ! <set_integer_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_ARRAY_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get an integer variable from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] inst the instance
      !>@param[in] varname name of the variable to read
      !>@param[out] valeur containis the read value
      !>@param[in] index1 index on the first dimension
      !>@param[in] index2 index on the second dimension
      !>@param[in] index3 index on the third dimension
      !>@param[out] ierr 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_T2D_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = -1
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VALEUR = INST%LIHBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          VALEUR = INST%LIUBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          VALEUR = INST%LIVBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
          VALEUR = INST%MESH%KP1BOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
          VALEUR = INST%NUMLIQ%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBOR') THEN
          VALEUR = INST%MESH%NBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
          VALEUR = INST%MESH%NPOIN
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
          VALEUR = INST%MESH%NELEM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
          VALEUR = INST%MESH%NPTFR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VALEUR = INST%NIT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
          VALEUR = INST%LT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBMAXNSHARE') THEN
          VALEUR = INST%NBMAXNSHARE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COMPLEO') THEN
          VALEUR = INST%COMPLEO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PTINIG') THEN
          VALEUR = INST%PTINIG
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTIR') THEN
          VALEUR = INST%NPTIR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELMAX') THEN
          VALEUR = INST%MESH%NELMAX
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR = INST%MESH%IKLE%I((INDEX2-1)*INST%MESH%IKLE%DIM1
     &          + INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VALEUR = INST%MESH%NACHB%I((INDEX2-1)*INST%NBMAXNSHARE
     &          + INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          VALEUR = INST%MESH%KNOLG%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CPL_PERIOD') THEN
          VALEUR = INST%SIS%PERCOU
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE') THEN
          VALEUR = INST%BND_TIDE(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ITURB') THEN
          VALEUR = INST%ITURB
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTRAC') THEN
          VALEUR = INST%NTRAC
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VALEUR = INST%CLU%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VALEUR = INST%CLV%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VALEUR = INST%LIQBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VALEUR = INST%LIEBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NSICLA') THEN
          VALEUR = INST%NSICLA
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NOMBLAY') THEN
          VALEUR = INST%NOMBLAY
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARDAT') THEN
          VALEUR = INST%MARDAT(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARTIM') THEN
          VALEUR = INST%MARTIM(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.START_RECORD') THEN
          VALEUR = INST%START_RECORD
        ! <get_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_INTEGER_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of an integer variable of telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_T2D_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          INST%LIHBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          INST%LIUBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          INST%LIVBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          INST%NIT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CPL_PERIOD') THEN
          INST%SIS%PERCOU = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LISTIN_PERIOD') THEN
          INST%SIS%LISPRD = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GRAPH_PERIOD') THEN
          INST%SIS%LEOPRD = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
          INST%LT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBMAXNSHARE') THEN
          INST%NBMAXNSHARE = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COMPLEO') THEN
          INST%COMPLEO = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PTINIG') THEN
          INST%PTINIG = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTIR') THEN
          INST%NPTIR = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE') THEN
          INST%BND_TIDE(INDEX1)=VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ITURB') THEN
          INST%ITURB = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTRAC') THEN
          INST%NTRAC = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          INST%CLU%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          INST%CLV%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          INST%LIQBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          INST%LIEBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NSICLA') THEN
          INST%NSICLA = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NOMBLAY') THEN
          INST%NOMBLAY = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARDAT') THEN
          INST%MARDAT(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARTIM') THEN
          INST%MARTIM(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.START_RECORD') THEN
          INST%START_RECORD = VALEUR
        ! <set_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a string variable from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] VALUELEN Length of the string
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_T2D_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        CHARACTER,             INTENT(OUT) :: VALEUR(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER I,J
!
        IERR = 0
        VALEUR = ""
        IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          I = INST%T2DRES
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%T2D_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          I = INST%T2DCLI
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%T2D_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          I = INST%T2DGEO
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%T2D_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.METEOFILE') THEN
          I = INST%T2ATMB
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%T2D_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FO2FILE') THEN
          I = INST%T2DFO2
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%T2D_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBCFILE') THEN
          I = INST%T2DIMP
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%T2D_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EQUATION') THEN
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%EQUA(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PREFILE') THEN
          DO J=1,VALUELEN
            VALEUR(J:J) = INST%T2D_FILES(INST%T2DPRE)%NAME(J:J)
          ENDDO
        ! <get_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_STRING_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a string variable of telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] VALUELEN Length of the string
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_T2D_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        CHARACTER,             INTENT(IN) :: VALEUR(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER I,J
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          I = INST%T2DRES
          DO J=1,VALUELEN
            INST%T2D_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          I = INST%T2DGEO
          DO J=1,VALUELEN
            INST%T2D_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.METEOFILE') THEN
          I = INST%T2ATMB
          DO J = 1,VALUELEN
            INST%T2D_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FO2FILE') THEN
          I = INST%T2DFO2
          DO J = 1,VALUELEN
            INST%T2D_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBCFILE') THEN
          I = INST%T2DIMP
          DO J = 1,VALUELEN
            INST%T2D_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PREFILE') THEN
          DO J=1,VALUELEN
            INST%T2D_FILES(INST%T2DPRE)%NAME(J:J) = VALEUR(J)
          ENDDO
        ! <set_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_STRING_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get a boolean variable from telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to read
      !>@param[out] VALEUR Containis the read value
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN_T2D_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
          VALEUR = INST%DEBUG
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RAZTIM') THEN
          IF(INST%RAZTIM) THEN
            VALEUR = 0
          ELSE
            VALEUR = 1
          ENDIF
        ! <get_boolean>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_BOOLEAN_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Defines the value of a boolean variable of telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in,out] INST The instance
      !>@param[in] VARNAME Name of the variable to write
      !>@param[in] VALEUR The value to write in the variable
      !>@param[in] INDEX1 Index on the first dimension
      !>@param[in] INDEX2 Index on the second dimension
      !>@param[in] INDEX3 Index on the third dimension
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN_T2D_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
          INST%DEBUG = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RAZTIM') THEN
          INST%RAZTIM = VALEUR.EQ.0
        ! <set_boolean>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_BOOLEAN_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get informations on a variable of telemac2d
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] INST Instance
      !>@param[in] VARNAME Name of the variable
      !>@param[out] DIM1 Size of the first dimension 0 if ndim=0
      !>@param[out] DIM2 Size of the second dimension 0 if ndim<1
      !>@param[out] DIM3 Size of the third dimension 0 if ndim<2
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_SIZE_T2D_D
     &         (INST, VARNAME, DIM1, DIM2, DIM3, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER :: TWODIM(2)

        IERR = 0
        DIM1 = 0
        DIM2 = 0
        DIM3 = 0
!
        IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          DIM1 = INST%HBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          DIM1 = INST%UBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          DIM1 = INST%VBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          DIM1 = INST%MESH%XNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          DIM1 = INST%MESH%YNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          DIM1 = INST%H%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          DIM1 = INST%DH%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          DIM1 = INST%ZF%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          DIM1 = INST%U%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          DIM1 = INST%V%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
          DIM1 = INST%MESH%KP1BOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          DIM1 = INST%MESH%X%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          DIM1 = INST%MESH%Y%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          DIM1 = SIZE(INST%TE5%R)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBOR') THEN
          DIM1 = INST%MESH%NBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
          DIM1 = INST%NUMLIQ%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          DIM1 = INST%LIHBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          DIM1 = INST%LIUBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          DIM1 = INST%LIVBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          DIM1 = SIZE(INST%COTE)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          DIM1 = SIZE(INST%DEBIT)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          DIM1 = SIZE(INST%FLUX_BOUNDARIES)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          DIM1 = SIZE(INST%CHESTR%R)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EQUATION') THEN
          DIM1 = 20
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.METEOFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FO2FILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PREFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBCFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE')THEN
          DIM1 = SIZE(INST%BND_TIDE)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE')THEN
          ! This is to have it properly (ndp, nelem)
          DIM1 = INST%MESH%IKLE%DIM2
          DIM2 = INST%MESH%IKLE%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB')THEN
          DIM1 = INST%NPTIR
          DIM2 = INST%NBMAXNSHARE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          DIM1 = INST%MESH%KNOLG%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AK') THEN
          DIM1 = INST%AK%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EP') THEN
          DIM1 = INST%EP%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INIT_DEPTH') THEN
          DIM1 = INST%H0%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TRACER') THEN
          DIM1 = INST%T%N
          DIM2 = INST%T%ADR(1)%P%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          DIM1 = INST%Q%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DCLA') THEN
          DIM1 = SIZE(INST%DCLA)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          DIM1 = SIZE(INST%AC)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          DIM1 = SIZE(INST%XWC)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          DIM1 = INST%Z%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          DIM1 = INST%QBOR%N
          DIM2 = INST%QBOR%ADR(1)%P%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          DIM1 = INST%EBOR%N
          DIM2 = INST%EBOR%ADR(1)%P%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          DIM1 = INST%FLBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          DIM1 = INST%TOB%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          DIM1 = INST%CLU%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          DIM1 = INST%CLV%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          DIM1 = INST%LIQBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          DIM1 = INST%LIEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          DIM1 = INST%CS%N
          DIM2 = INST%CS%ADR(1)%P%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          DIM1 = INST%E%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PARTHENIADES') THEN
          TWODIM = SHAPE(INST%PARTHENIADES)
          DIM1 = TWODIM(1)
          DIM2 = TWODIM(2)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARDAT') THEN
          DIM1 = SIZE(INST%MARDAT)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARTIM') THEN
          DIM1 = SIZE(INST%MARTIM)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VOLU2D') THEN
          DIM1 = INST%VOLU2D%DIM1
        ! <get_var_size>
        ENDIF
!
      END SUBROUTINE GET_VAR_SIZE_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get type information of a varaible
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] VARNAME Name of the variable
      !>@param[out] VARTYPE Type of the variable (INTEGER, STRING,
      !!                    DOUBLE, BOOLEAN)
      !>@param[out] NDIM Number of dimension
      !>@param[out] READONLY True if the variable cannot be modified
      !>@param[out] IENT 1 if the numbering is on point
      !>@param[out] JENT 1 if the numbering is on point
      !>@param[out] KENT 1 if the numbering is on point
      !>@param[out] GETPOS Postion after which the get is posible
      !!                        on the variable
      !>@param[out] SETPOS Postion after which the set is posible
      !!                        on the variable
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE_T2D_D
     &        (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &         GETPOS,SETPOS,IERR)
!
        CHARACTER(LEN=T2D_VAR_LEN),  INTENT(IN)  :: VARNAME
        CHARACTER(LEN=T2D_TYPE_LEN), INTENT(OUT) :: VARTYPE
        LOGICAL,                     INTENT(OUT) :: READONLY
        INTEGER,                     INTENT(OUT) :: NDIM
        INTEGER,                     INTENT(OUT) :: IERR
        INTEGER,                     INTENT(OUT) :: IENT
        INTEGER,                     INTENT(OUT) :: JENT
        INTEGER,                     INTENT(OUT) :: KENT
        INTEGER,                     INTENT(OUT) :: GETPOS
        INTEGER,                     INTENT(OUT) :: SETPOS
!
        IERR = 0
        VARTYPE = ''
        READONLY = .TRUE.
        NDIM = 0
        IENT = 0
        JENT = 0
        KENT = 0
        GETPOS = NO_POSITION
        SETPOS = NO_POSITION
!
        IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DT') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GRAPH_PERIOD') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LISTIN_PERIOD') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CPL_PERIOD') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_SET_CONFIG_POS
          SETPOS = RUN_SET_CONFIG_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VARTYPE = 'INTEGER'
          READONLY = .TRUE.
          NDIM = 2
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VARTYPE = 'INTEGER'
          READONLY = .TRUE.
          NDIM = 2
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          VARTYPE = 'INTEGER'
          READONLY = .TRUE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBMAXNSHARE') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COMPLEO') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PTINIG') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTIR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          VARTYPE =  'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.METEOFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FO2FILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PREFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBCFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FAIR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          IENT = 1
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELMAX') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALRANGE') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALVELOCITY') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SEALEVEL') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EQUATION') THEN
          VARTYPE = 'STRING'
          READONLY = .TRUE.
          NDIM = 1
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AK') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EP') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          IENT = 1
          GETPOS = RUN_TIMESTEP_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ITURB') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_ALLOCATION_POS
          SETPOS = RUN_ALLOCATION_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INIT_DEPTH') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TRACER') THEN
          VARTYPE = 'DOUBLE_BLOCK'
          READONLY = .FALSE.
          NDIM = 2
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTRAC') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DCLA') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XWC') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          VARTYPE = ''
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          VARTYPE = ''
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NSICLA') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NOMBLAY') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CONCENTRATION') THEN
          VARTYPE = ''
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PARTHENIADES') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 2
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARDAT') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MARTIM') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RAZTIM') THEN
          VARTYPE = 'BOOLEAN'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.START_RECORD') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VOLU2D') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
          GETPOS = NO_POSITION
          SETPOS = NO_POSITION
        ! <get_var_type>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_VAR_TYPE_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Get the description of the ith variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[in] I Number of the variable
      !>@param[in] VAR_LEN Size of varname
      !>@param[in] INFO_LEN Size of varinfo
      !>@param[out] VARNAME Name of the variable
      !>@param[out] VARINFO Description of the variable
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_INFO_T2D_D(I, VAR_LEN, INFO_LEN,
     &                              VARNAME, VARINFO, IERR)
!
        INTEGER, INTENT(IN) :: I
        INTEGER, INTENT(IN) :: VAR_LEN
        INTEGER, INTENT(IN) :: INFO_LEN
        CHARACTER, INTENT(OUT) :: VARNAME(VAR_LEN)
        CHARACTER, INTENT(OUT) :: VARINFO(INFO_LEN)
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: J
!
        IERR = 0

        DO J=1,T2D_VAR_LEN
          VARNAME(J:J) = VNAME_T2D(I)(J:J)
        ENDDO
        DO J=1,T2D_INFO_LEN
          VARINFO(J:J) = VINFO_T2D(I)(J:J)
        ENDDO

        RETURN
      END SUBROUTINE GET_VAR_INFO_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@brief Build the description of each variable
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !>@param[out] IERR 0 if subroutine successfull,
      !!                        error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_LIST_T2D_D(IERR)
!
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
        IF(.NOT.ALLOCATED(VNAME_T2D)) THEN
          ALLOCATE(VNAME_T2D(NB_VAR_T2D),STAT=IERR)
          IF(IERR.NE.0) RETURN
          ALLOCATE(VINFO_T2D(NB_VAR_T2D),STAT=IERR)
          IF(IERR.NE.0) RETURN
!
          I = I + 1
          VNAME_T2D(I) = 'MODEL.AT'
          VINFO_T2D(I) = 'CURRENT TIME'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.DT'
          VINFO_T2D(I) = 'TIME STEP'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.BCFILE'
          VINFO_T2D(I) = 'BOUNDARY CONDITION FILE NAME'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.BND_TIDE'
          VINFO_T2D(I) = 'OPTION FOR TIDAL BOUNDARY CONDITIONS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.BOTTOMELEVATION'
          VINFO_T2D(I) = 'LEVEL OF THE BOTTOM'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.CHESTR'
          VINFO_T2D(I) = 'STRIKLER ON POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.FAIR'
          VINFO_T2D(I) = 'FAIR ON POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.COTE'
          VINFO_T2D(I) = 'xxx'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.CPL_PERIOD'
          VINFO_T2D(I) = 'COUPLING PERIOD WITH SISYPHE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.DEBIT'
          VINFO_T2D(I) = 'DISCHARGE ON FRONTIER'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.DEBUG'
          VINFO_T2D(I) = 'ACTIVATING DEBUG MODE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.FLUX_BOUNDARIES'
          VINFO_T2D(I) = 'FLUX AT BOUNDARIES'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.GEOMETRYFILE'
          VINFO_T2D(I) = 'NAME OF THE GEOMERY FILE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.METEOFILE'
          VINFO_T2D(I) = 'NAME OF THE BINARY ATMOSPHERIC FILE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.FO2FILE'
          VINFO_T2D(I) = 'NAME OF THE FORMATTED DATA FILE 2'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIQBCFILE'
          VINFO_T2D(I) = 'NAME OF THE LIQUID BOUNDARIES FILE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.PREFILE'
          VINFO_T2D(I) = 'NAME OF THE PREVIOUS COMPUTATION FILE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.GRAPH_PERIOD'
          VINFO_T2D(I) = 'GRAPHICAL OUTPUT PERIOD'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.HBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON H FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.IKLE'
          VINFO_T2D(I) = 'CONNECTIVITY TABLE BETWEEN ELEMENT AND NODES'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NACHB'
          VINFO_T2D(I) = 'NUMBERS OF PROC CONTAINING A GIVEN POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.KNOLG'
          VINFO_T2D(I) =
     &         'GIVES THE INITIAL GLOBAL NUMBER OF A LOCAL POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.INCWATERDEPTH'
          VINFO_T2D(I) = 'INCREASE IN THE THE DEPTH OF THE WATER'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.KP1BOR'
          VINFO_T2D(I) =
     &           'POINTS FOLLOWING AND PRECEDING A BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIHBOR'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON H FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LISTIN_PERIOD'
          VINFO_T2D(I) = 'LISTING OUTPUT PERIOD'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIUBOR'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIVBOR'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LT'
          VINFO_T2D(I) = 'CURRENT TIME STEP'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NBMAXNSHARE'
          VINFO_T2D(I) = 'MAXIMUM GEOMETRICAL MULTIPLICITY OF A NODE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.COMPLEO'
          VINFO_T2D(I) = 'GRAPHIC OUTPUT COUNTER'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.PTINIG'
          VINFO_T2D(I) =
     &      'NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NPTIR'
          VINFO_T2D(I) = 'NUMBER OF INTERFACE POINTS OF THE SUB-DOMAIN'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NBOR'
          VINFO_T2D(I) = 'GLOBAL NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NELEM'
          VINFO_T2D(I) = 'NUMBER OF ELEMENT IN THE MESH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NELMAX'
          VINFO_T2D(I) = 'MAXIMUM NUMBER OF ELEMENTS ENVISAGED'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NPOIN'
          VINFO_T2D(I) = 'NUMBER OF POINT IN THE MESH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NPTFR'
          VINFO_T2D(I) = 'NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NTIMESTEPS'
          VINFO_T2D(I) = 'NUMBER OF TIME STEPS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NUMLIQ'
          VINFO_T2D(I) = 'LIQUID BOUNDARY NUMBERS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.POROSITY'
          VINFO_T2D(I) = 'POROSITY'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.RESULTFILE'
          VINFO_T2D(I) = 'NAME OF THE RESULT FILE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.SEALEVEL'
          VINFO_T2D(I) = 'COEFFICIENT TO CALIBRATE SEA LEVEL'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.TIDALRANGE'
          VINFO_T2D(I) = 'COEFFICIENT TO CALIBRATE TIDAL RANGE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.UBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.VBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.VELOCITYU'
          VINFO_T2D(I) = 'VELOCITY ON U'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.VELOCITYV'
          VINFO_T2D(I) = 'VELOCITY ON V'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.WATERDEPTH'
          VINFO_T2D(I) = 'DEPTH OF THE WATER'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.X'
          VINFO_T2D(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.XNEBOR'
          VINFO_T2D(I) = 'NORMAL X TO 1D BOUNDARY POINTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.Y'
          VINFO_T2D(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.YNEBOR'
          VINFO_T2D(I) = 'NORMAL Y TO 1D BOUNDARY POINTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.EQUATION'
          VINFO_T2D(I) = 'NAME OF THE EQUATION USED'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.AK'
          VINFO_T2D(I) = 'K '
          I = I + 1
          VNAME_T2D(I) = 'MODEL.EP'
          VINFO_T2D(I) = 'EPSILON '
          I = I + 1
          VNAME_T2D(I) = 'MODEL.ITURB'
          VINFO_T2D(I) = 'TURBULENCE MODEL '
          I = I + 1
          VNAME_T2D(I) = 'MODEL.INIT_DEPTH'
          VINFO_T2D(I) = 'INITIAL DEPTH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.TRACER'
          VINFO_T2D(I) = 'TRACERS VALUE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NTRAC'
          VINFO_T2D(I) = 'NUMBER OF TRACERS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.FLOWRATEQ'
          VINFO_T2D(I) = 'SOLID TRANSPORT FLOWRATE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.DCLA'
          VINFO_T2D(I) = 'MEDIAN GRAIN SIZE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.SHIELDS'
          VINFO_T2D(I) = 'CRITICAL SHIELDS PARAMETER'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.XWC'
          VINFO_T2D(I) = 'SETTLING VELOCITY'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.Z'
          VINFO_T2D(I) = 'FREE SURFACE ELEVATION'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.QBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON Q FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.EBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON E FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.FLBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON ZF FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.TOB'
          VINFO_T2D(I) = 'SHEAR STRESS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.CLU'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.CLV'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIQBOR'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON Q FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIEBOR'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON E FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NSICLA'
          VINFO_T2D(I) = 'NUMBER OF SIZE-CLASSES OF BED MATERIAL'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NOMBLAY'
          VINFO_T2D(I) = 'NUMBER OF LAYER IN THE BED'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.CONCENTRATION'
          VINFO_T2D(I) = 'CONCENTRATION AT TIME N'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.EVOLUTION'
          VINFO_T2D(I) = 'EVOLUTION OF BED'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.PARTHENIADES'
          VINFO_T2D(I) = 'PARTHENIADES EROSION COEFICIANT FOR EACH '//
     &                   'BED LAYER FOR EADCH CLASS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.MARDAT'
          VINFO_T2D(I) = 'A 1 DIMENSIONAL INTEGER ARRAY'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.MARTIM'
          VINFO_T2D(I) = 'A 1 DIMENSIONAL INTEGER ARRAY'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.RAZTIM'
          VINFO_T2D(I) = 'A SIMPLE BOOLEAN'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.START_RECORD'
          VINFO_T2D(I) = 'A SIMPLE INTEGER'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.VOLU2D'
          VINFO_T2D(I) = 'INTEGRAL OF BASES DOUBLE BIEF_OBJ'
          ! <set_var_list>
          IF(I.NE.NB_VAR_T2D) THEN
            IERR = INCREASE_NB_VAR_T2D_ERROR
            RETURN
          ENDIF
        ENDIF
!
      END SUBROUTINE SET_VAR_LIST_T2D_D
!
      END MODULE API_HANDLE_VAR_T2D
