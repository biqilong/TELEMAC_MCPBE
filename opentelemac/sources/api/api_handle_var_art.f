!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief GETTER/SETTER OF TELEMAC2D VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       V6P3
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_HANDLE_VAR_ART

        USE API_HANDLE_ERROR
        USE API_INSTANCE_ART
        IMPLICIT NONE
        ! Size of the string containing the name of a variable
        INTEGER, PARAMETER :: ART_VAR_LEN=40
        ! Size of the string containing the type of a variable
        INTEGER, PARAMETER :: ART_TYPE_LEN=12
        ! Size of the string containing the information about a variable
        INTEGER, PARAMETER :: ART_INFO_LEN=200
        ! The maximum number of variable
        INTEGER, PARAMETER :: NB_VAR_ART=18
        CHARACTER(LEN=ART_VAR_LEN),ALLOCATABLE :: VNAME_ART(:)
        CHARACTER(LEN=ART_INFO_LEN),ALLOCATABLE :: VINFO_ART(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A DOUBLE ARRAY FROM ARTEMIS
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_ARRAY_ART_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_ART),         INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN) :: VARNAME
        INTEGER,                    INTENT(IN) :: DIM1
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR(DIM1)
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALEUR = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR(1:INST%MESH%X%DIM1) = INST%MESH%X%R(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR(1:INST%MESH%Y%DIM1) = INST%MESH%Y%R(1:INST%MESH%Y%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          VALEUR(1:SIZE(INST%PHAS%R)) =
     &     INST%PHAS%R(1:SIZE(INST%PHAS%R))
        ! <get_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_ARRAY_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A DOUBLE ARRAY OF ARTEMIS
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_ARRAY_ART_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(1:INST%MESH%X%DIM1) = VALEUR(1:INST%MESH%X%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%X%R(1:INST%MESH%Y%DIM1) = VALEUR(1:INST%MESH%Y%DIM1)
        ! <set_double_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_ARRAY_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET AN INTEGER ARRAY
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_ARRAY_ART_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(OUT) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = -1
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VALEUR(1:INST%LIHBOR%DIM1) = INST%LIHBOR%I(1:INST%LIHBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          VALEUR(1:INST%LIUBOR%DIM1) = INST%LIUBOR%I(1:INST%LIUBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          VALEUR(1:INST%LIVBOR%DIM1) = INST%LIVBOR%I(1:INST%LIVBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
          VALEUR(1:INST%MESH%KP1BOR%DIM1) =
     &    INST%MESH%KP1BOR%I(1:INST%MESH%KP1BOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR(1:SIZE(INST%MESH%IKLE%I)) =
     &    INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VALEUR(1:SIZE(INST%MESH%NACHB%I)) =
     &         INST%MESH%NACHB%I(1:SIZE(INST%MESH%NACHB%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          VALEUR(1:INST%MESH%KNOLG%DIM1) =
     &         INST%MESH%KNOLG%I(1:INST%MESH%KNOLG%DIM1)
        ! <get_integer_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_INTEGER_ARRAY_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF AN INTEGER VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_ARRAY_ART_D
     &     (INST, VARNAME, VALEUR, DIM1, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: DIM1
        INTEGER,               INTENT(IN) :: VALEUR(DIM1)
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          INST%LIHBOR%I(1:INST%LIHBOR%DIM1) = VALEUR(1:INST%LIUBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          INST%LIUBOR%I(1:INST%LIUBOR%DIM1) = VALEUR(1:INST%LIUBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          INST%LIVBOR%I(1:INST%LIVBOR%DIM1) = VALEUR(1:INST%LIVBOR%DIM1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          INST%MESH%IKLE%I(1:SIZE(INST%MESH%IKLE%I)) =
     &    VALEUR(1:SIZE(INST%MESH%IKLE%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          INST%MESH%NACHB%I(1:SIZE(INST%MESH%NACHB%I)) =
     &    VALEUR(1:SIZE(INST%MESH%NACHB%I))
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          INST%MESH%KNOLG%I(1:INST%MESH%KNOLG%DIM1) =
     &    VALEUR(1:INST%MESH%KNOLG%DIM1)
        ! <set_integer_array>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_ARRAY_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A DOUBLE VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALEUR    [OUT]    CONTAINS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),         INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN) :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALEUR = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR = INST%MESH%X%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR = INST%MESH%Y%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          VALEUR = INST%PHAS%R(INDEX1)
        ! <get_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A DOUBLE VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALEUR     [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'xxx') THEN
          CONTINUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          INST%PHAS%R(INDEX1) = VALEUR
        ! <set_double>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET AN INTEGER VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALEUR    [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
          VALEUR = INST%MESH%NPOIN
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
          VALEUR = INST%MESH%NELEM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
          VALEUR = INST%MESH%NPTFR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VALEUR = INST%NIT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR = INST%MESH%IKLE%I((INDEX2-1)*INST%MESH%IKLE%DIM1
     &                               + INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB') THEN
          VALEUR = INST%MESH%NACHB%I((INDEX2-1)*INST%NBMAXNSHARE
     &          + INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          VALEUR = INST%MESH%KNOLG%I(INDEX1)
        ! <get_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_INTEGER_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF AN INTEGER VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALEUR     [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
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
        ! <set_integer>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_INTEGER_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A STRING VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALEUR    [OUT]    CONTAINIS THE READ VALUE
      !PARAM VALUELEN   [IN]    Length of the string
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_ART_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
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
          I = INST%ARTRES
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%ART_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          I = INST%ARTCLI
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%ART_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          I = INST%ARTGEO
          DO J = 1,VALUELEN
            VALEUR(J:J) = INST%ART_FILES(I)%NAME(J:J)
          ENDDO
        ! <get_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_STRING_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A STRING VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALEUR     [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM VALUELEN   [IN]    LENGTH OF THE STRING
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_ART_D
     &     (INST, VARNAME, VALEUR, VALUELEN, INDEX1, INDEX2, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
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
          I = INST%ARTRES
          DO J=1,VALUELEN
            INST%ART_FILES(I)%NAME(J:J) = VALEUR(J)
          ENDDO
        ! <set_string>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_STRING_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A BOOLEAN VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALEUR    [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
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
        ! <get_boolean>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_BOOLEAN_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A BOOLEAN VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALEUR     [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN_ART_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(INOUT) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
          INST%DEBUG = VALEUR
        ! <set_boolean>
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_BOOLEAN_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET INFORMATIONS ON A VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !+HISTORY C. GOEURY (EDF R&D LNHE)
      !+        04/09/2016
      !+        V7P1
      !++=
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE
      !PARAM VARTYPE   [OUT]    TYPE OF THE VARIABLE
      !+                        (INTEGER, DOUBLE, STRING, BOOLEAN)
      !PARAM READONLY  [OUT]    0 IF THE VARIABLE IS READ ONLY
      !+                        1 IF IT IS WRITTABLE
      !PARAM NDIM      [OUT]    NUMBER OF DIMENSION
      !+                        (0 IF IT IS NOT AN ARRAY)
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_SIZE_ART_D
     &         (INST, VARNAME, DIM1, DIM2, DIM3, IERR)
!
        TYPE(INSTANCE_ART),    INTENT(IN) :: INST
        CHARACTER(LEN=ART_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        DIM1 = 0
        DIM2 = 0
        DIM3 = 0
!
        IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          DIM1 = INST%MESH%X%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          DIM1 = INST%MESH%Y%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          DIM1 = INST%LIHBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          DIM1 = INST%LIUBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          DIM1 = INST%LIVBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EQUATION') THEN
           DIM1 = 20
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          DIM1 = 250
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE')THEN
          DIM1 = INST%MESH%IKLE%DIM2
          DIM2 = INST%MESH%IKLE%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NACHB')THEN
          DIM1 = INST%NPTIR
          DIM2 = INST%NBMAXNSHARE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KNOLG') THEN
          DIM1 = INST%MESH%KNOLG%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
          DIM1 = INST%PHAS%DIM1
        ! <get_var_size>
        ENDIF
!
      END SUBROUTINE GET_VAR_SIZE_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET THE SIZE OF EACH DIMENSION OF A VARAIBLE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !HISTORY C GOEURY (EDF R&D, LNHE)
      !+       01/09/2016
      !+       V7P1
      !+       IENT,JENT AND KENT ADDED FOR MPI CONTROL IN GET AND SET
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARAIBLE
      !PARAM DIM1      [OUT]    SIZE OF THE FIRST DIMENSION
      !PARAM DIM2      [OUT]    SIZE OF THE SECOND DIMENSION
      !PARAM DIM3      [OUT]    SIZE OF THE THIRD DIMENSION
      !PARAM IENT      [OUT]    1 if the numbering is on point
      !PARAM JENT      [OUT]    1 if the numbering is on point
      !PARAM KENT      [OUT]    1 if the numbering is on point
      !PARAM GETPOS    [OUT]    Postion after which the get is posible
      !+                        on the variable
      !PARAM SETPOS    [OUT]    Postion after which the Set is posible
      !+                        on the variable
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE_ART_D
     &        (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,
     &         GETPOS,SETPOS,IERR)
!
        CHARACTER(LEN=ART_VAR_LEN),  INTENT(IN)  :: VARNAME
        CHARACTER(LEN=ART_TYPE_LEN), INTENT(OUT) :: VARTYPE
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
        IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          VARTYPE = 'STRING'
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 0
          GETPOS = RUN_READ_CASE_POS
          SETPOS = RUN_READ_CASE_POS
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WAVEPHASE') THEN
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
      END SUBROUTINE GET_VAR_TYPE_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET THE DESCRIPTION OF THE ITH VARIABLE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM IERR      [IN]     Integer
      !PARAM VAR_LEN   [IN]     Size of varname
      !PARAM INFO_LEN  [IN]     Size of varinfo
      !PARAM VARNAME   [OUT]    Name of the variable
      !PARAM VARINFO   [OUT]    Description of the variable
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_INFO_ART_D(I, VAR_LEN, INFO_LEN,
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

        DO J=1,ART_VAR_LEN
          VARNAME(J:J) = VNAME_ART(I)(J:J)
        ENDDO
        DO J=1,ART_INFO_LEN
          VARINFO(J:J) = VINFO_ART(I)(J:J)
        ENDDO

        RETURN
      END SUBROUTINE GET_VAR_INFO_ART_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A DESCRIPTION OF EACH VARIABLE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_LIST_ART_D(IERR)
!
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
        IF(.NOT.ALLOCATED(VNAME_ART)) THEN
          ALLOCATE(VNAME_ART(NB_VAR_ART),STAT=IERR)
          IF(IERR.NE.0) RETURN
          ALLOCATE(VINFO_ART(NB_VAR_ART),STAT=IERR)
          IF(IERR.NE.0) RETURN
!
          I = I + 1
          VNAME_ART(I) = 'MODEL.BCFILE'
          VINFO_ART(I) = 'BOUNDARY CONDITION FILE NAME'
          I = I + 1
          VNAME_ART(I) = 'MODEL.DEBUG'
          VINFO_ART(I) = 'ACTIVATING DEBUG MODE'
          I = I + 1
          VNAME_ART(I) = 'MODEL.GEOMETRYFILE'
          VINFO_ART(I) = 'NAME OF THE GEOMERY FILE'
          I = I + 1
          VNAME_ART(I) = 'MODEL.IKLE'
          VINFO_ART(I) = 'CONNECTIVITY TABLE BETWEEN ELEMENT AND NODES'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NACHB'
          VINFO_ART(I) = 'NUMBERS OF PROC CONTAINING A GIVEN POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.KNOLG'
          VINFO_ART(I) =
     &         'GIVES THE INITIAL GLOBAL NUMBER OF A LOCAL POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.LIHBOR'
          VINFO_ART(I) = 'BOUNDARY TYPE ON H FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.LIUBOR'
          VINFO_ART(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.LIVBOR'
          VINFO_ART(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NELEM'
          VINFO_ART(I) = 'NUMBER OF ELEMENT IN THE MESH'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NELMAX'
          VINFO_ART(I) = 'MAXIMUM NUMBER OF ELEMENTS ENVISAGED'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NPOIN'
          VINFO_ART(I) = 'NUMBER OF POINT IN THE MESH'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NPTFR'
          VINFO_ART(I) = 'NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_ART(I) = 'MODEL.NTIMESTEPS'
          VINFO_ART(I) = 'NUMBER OF TIME STEPS'
          I = I + 1
          VNAME_ART(I) = 'MODEL.RESULTFILE'
          VINFO_ART(I) = 'NAME OF THE RESULT FILE'
          I = I + 1
          VNAME_ART(I) = 'MODEL.X'
          VINFO_ART(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_ART(I) = 'MODEL.Y'
          VINFO_ART(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
          ! <get_var_list>
          IF(I.NE.NB_VAR_ART) THEN
            IERR = INCREASE_NB_VAR_ART_ERROR
            RETURN
          ENDIF
        ENDIF
!
      END SUBROUTINE SET_VAR_LIST_ART_D
!
      END MODULE API_HANDLE_VAR_ART
