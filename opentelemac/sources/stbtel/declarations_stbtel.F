!
!  DECLARATION OF GLOBAL DATA STRUCTURE IN TELEMAC-2D
!
      MODULE DECLARATIONS_STBTEL
!
!***********************************************************************
!  STBTEL VERSION 6.2
!***********************************************************************
!
!
      USE DECLARATIONS_SPECIAL
      USE UTILS_MED
      CHARACTER(LEN=MAXLENFILE) NOMGEO,NOMFO1,NOMFON,NOMFO2,NOMIMP,
     &                          NOMSOU,NOMFRC
      CHARACTER(LEN=MAXLENFILE) NOMFOR,NOMCAS,NOMLIM,NOMRES,NOMBND2
      INTEGER NGEO,NCLE,NCAS,NLIM,NFO1,NFON,NFO2,NIMP,NSOU,NFRC,NRES
      INTEGER NBND2
!
!     COMMON GEO
!
      INTEGER NELEM
      INTEGER MESH
      INTEGER NDP
      INTEGER NPOIN
      INTEGER NELMAX
      INTEGER NPMAX
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!-----------------------------------------------------------------------
!
!       KEY-WORDS AND PARAMETERS
!
      INTEGER NBAT,LGVEC,NSOM,NSOM2,NBFOND,IHAUT,INOP5,NSEC2,NSEC3
      INTEGER NSEC11,NSEC12
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
      LOGICAL OPTASS,DECTRI,COLOR,ELIDEP,DIV4,FONTRI,ADDFAS,PROJEX
      LOGICAL FUSION,ELISEC,ELPSEC,STOTOT
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION EPSI,DM,CORTRI,SOM(10,2),SOM2(10,2),SEUSEC
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=3)  STD
      CHARACTER(LEN=9)  MAILLE
      CHARACTER(LEN=PATH_LEN) FOND(5)

      ! VARIABLE FOR HERMES
      INTEGER :: TYP_ELEM, TYP_BND_ELEM, NPTIR
      CHARACTER(LEN=8) :: FFORMAT, OUT_FORMAT
!
!-----------------------------------------------------------------------
!     CONVERTER
!-----------------------------------------------------------------------
!
!!!!  DICTIONARY KEYWORDS
      ! NAME OF THE INPUT FORMAT
      CHARACTER(LEN=9)  INFMT
      ! NAME OF THE OUTPUT FORMAT
      CHARACTER(LEN=9)  OUTFMT
      ! NAME OF THE INPUT FILES
      CHARACTER(LEN=MAXLENFILE) INFILE,BOUNDFILE,LOGFILE
      INTEGER NINP,NBND,NLOG
      ! NAME OF THE OUTPUT FILES
      CHARACTER(LEN=MAXLENFILE) OUTFILE,OUTBNDFILE,OUTLOGFILE
      INTEGER NOUT,NOBND,NOLOG
      ! DICTIONARY KEY WORDS
      LOGICAL CONVER,DEBUG,SRF_BND
      ! INFO FOR TRANSLATION OF MESH
      LOGICAL :: TRANSLATE
      DOUBLE PRECISION DX,DY
!!!!  PARAMETERS
      ! MAX NUMBER OF FAMILY
      INTEGER, PARAMETER :: MAXFAM = 40
      ! LENGTH OF THE STRINGS
#if defined HAVE_MED
      INTEGER, PARAMETER :: NAME_SIZE = MED_NAME_SIZE
      INTEGER, PARAMETER :: LNAME_SIZE = MED_LNAME_SIZE
      INTEGER, PARAMETER :: COMMENT_SIZE = MED_COMMENT_SIZE
      INTEGER, PARAMETER :: SNAME_SIZE = MED_SNAME_SIZE
#else
      INTEGER, PARAMETER :: NAME_SIZE = 64
      INTEGER, PARAMETER :: LNAME_SIZE = 80
      INTEGER, PARAMETER :: COMMENT_SIZE = 200
      INTEGER, PARAMETER :: SNAME_SIZE = 16
#endif
      INTEGER, PARAMETER :: TITLE_SIZE = 80
!
      ! DESCRIBE A MESH AND RESULTS
      TYPE MESH_OBJ2
!!!! GENERALITIES
        !NAME OF THE MESH
        CHARACTER(LEN=TITLE_SIZE) :: TITLE
        ! MESH DESCRIPTION
        CHARACTER(LEN=COMMENT_SIZE) :: DESCRIPTION
        !NUMBER OF ELEMENTS
        INTEGER :: NELEM
        !NUMBER OF POINTS
        INTEGER :: NPOIN
        !NUMBER OF BOUNDAR POINTS
        INTEGER :: NPTFR
        !NUMBER OF POINTS PER ELEMENT
        INTEGER :: NDP
        ! TYPE OF THE ELEMENTS SEE TYPES NUMBER BELOW
        INTEGER :: TYPE_ELEM
        ! NUMBER OF DIMENSION
        INTEGER :: NDIM
        !SERAFIN 10 INTEGER
        INTEGER :: IB(10)
        ! CONNECTIVITY TABLE
        INTEGER, ALLOCATABLE :: IKLES(:)
        ! BOUNDARY TABLE
        INTEGER, ALLOCATABLE :: IPOBO(:)
        !X COORDINATES
        DOUBLE PRECISION, ALLOCATABLE :: X(:)
        !Y COORDINATES
        DOUBLE PRECISION, ALLOCATABLE :: Y(:)
        !Z COORDINATES
        DOUBLE PRECISION, ALLOCATABLE :: Z(:)
        !NAME OF COORDINATES
        CHARACTER(LEN=SNAME_SIZE), ALLOCATABLE :: NAMECOO(:)
        !NAME OF COORDINATES
        CHARACTER(LEN=SNAME_SIZE), ALLOCATABLE :: UNITCOO(:)
        !PARALLEL NUMBERING LOCAL TO GLOBAL
        INTEGER,ALLOCATABLE :: KNOLG(:)
!!!! RESULTS
        !TOTAL NUMBER OF VARIABLES
        INTEGER :: NVAR
        !NAME OF VARIABLES
        CHARACTER(LEN=SNAME_SIZE), ALLOCATABLE :: NAMEVAR(:)
        !UNIT OF VARIABLES
        CHARACTER(LEN=SNAME_SIZE), ALLOCATABLE :: UNITVAR(:)
        !NUMBER OF TIME STEPS
        INTEGER :: TIMESTEP
        !TIMES STEPS TABLE
        DOUBLE PRECISION, ALLOCATABLE :: TIMES(:)
        !RESULT TABLE (TIMESTEP,VAR,POINT)
        DOUBLE PRECISION, ALLOCATABLE :: RESULTS(:,:,:)
!!!! FAMILY INFORMATIONS
        ! NUMBER OF FAMILY
        INTEGER :: NFAM
        ! FAMILY'S NAME
        CHARACTER(LEN=NAME_SIZE), ALLOCATABLE :: NAMEFAM(:)
        ! ID OF THE FAMILY
        INTEGER, ALLOCATABLE :: IDFAM(:)
        ! FAMILY VALUE
        INTEGER, ALLOCATABLE :: VALFAM(:)
        ! NUMBER OF GROUP FOR EACH FAMILY
        INTEGER, ALLOCATABLE :: NGROUPFAM(:)
        ! FAMILY'S GROUP
        CHARACTER(LEN=LNAME_SIZE), ALLOCATABLE :: GROUPFAM(:,:)
!!!! BOUNDARY INFORMATIONS
        ! BOUNDARY TABLE
        INTEGER, ALLOCATABLE :: LIHBOR(:)
        ! LOCAL TO BOUNDARY NUMBER
        INTEGER, ALLOCATABLE :: NBOR(:)
!!!! ESTEL BOUNDARY ELEMENTS ONLY FOR UNV AND MED
        ! NUMBER OF ELEMENTS
        INTEGER :: NELEM2
        ! NUMBER OF POINT ER ELEMENT
        INTEGER :: NDP2
        ! TYPE OF THE ELEMENTS SEE TYPES NUMBER BELOW
        INTEGER :: TYPE_ELEM2
        ! CONNECTIVITY TABLE
        INTEGER, ALLOCATABLE :: IKLES2(:)
!!!! COLOR INFORMATIONS
        ! COLOR FOR NODES
        INTEGER, ALLOCATABLE :: COLOR(:)
        ! COLOR FOR 2D ELEMENTS
        INTEGER, ALLOCATABLE :: NCOLOR(:)
        ! COLOR FOR 3D ELEMENTS
        INTEGER, ALLOCATABLE :: NCOLOR2(:)

      END TYPE MESH_OBJ2
!
      TYPE(MESH_OBJ2) MESH2
!
      SAVE
!
      CONTAINS
!
      SUBROUTINE RENAME_VECTOR(STR,N)
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: STR
      INTEGER, INTENT(IN) :: N
      INTEGER :: POS
      POS = INDEX(STR,'*')
      IF (STR(POS+1:POS+1).EQ.'0') THEN
        IF(N.EQ.1)STR(POS:POS)='U'
        IF(N.EQ.2)STR(POS:POS)='V'
        IF(N.EQ.3)STR(POS:POS)='W'
      ELSE
        IF(N.EQ.1)STR(POS:POS)='X'
        IF(N.EQ.2)STR(POS:POS)='Y'
        IF(N.EQ.3)STR(POS:POS)='Z'
      ENDIF
      END SUBROUTINE RENAME_VECTOR
!
!
      CHARACTER(LEN=11) FUNCTION EXTENS
     &                               (N,IPID)
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER IPID,N
!
      IF(N.GT.0) THEN
!
        EXTENS='00000-00000'
!
        IF(N.LT.10) THEN
          WRITE(EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(EXTENS(01:05),'(I5)') N
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(EXTENS(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(EXTENS(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(EXTENS(09:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(EXTENS(08:11),'(I4)') IPID
        ELSE
          WRITE(EXTENS(07:11),'(I5)') IPID
        ENDIF
!
      ELSE
!
        EXTENS='       '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION EXTENS

!
      ! INITIALIZE THE MESH OBJECT
      SUBROUTINE INI_MESH
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      MESH2%TITLE = 'NO TITLE'
      MESH2%DESCRIPTION = 'NO DESCRIPTION'
      MESH2%NELEM = 0
      MESH2%NELEM2 = 0
      MESH2%TYPE_ELEM = 0
      MESH2%TYPE_ELEM2 = 0
      MESH2%NPOIN = 0
      MESH2%NPTFR = 0
      MESH2%NDP = 0
      MESH2%NDP2 = 0
      MESH2%NDIM = 0
      MESH2%NVAR = 0
      MESH2%TIMESTEP = 0
      MESH2%IB(:) = 0
      MESH2%IB(1) = 1
      MESH2%NFAM = 0
      END SUBROUTINE INI_MESH
!
      ! CHECK THAT EVERYTHING HAS BEEN INITIALISE/ALLOCATED
      SUBROUTINE CHECK_MESH
      !
      !
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
      INTEGER I,J
      WRITE(LU,*) '--------------------------------------------------'
      WRITE(LU,*) '------CHECKING MESH OBJECT'
      WRITE(LU,*) '--------------------------------------------------'
      IF(MESH2%TITLE.EQ.' ') WRITE(LU,*) 'NO TITLE'
      IF(MESH2%DESCRIPTION.EQ.' ') WRITE(LU,*) 'NO DESCRIPTION'
      IF(MESH2%NELEM.EQ.0) WRITE(LU,*) 'NO ELEMENTS, NELEM = 0'
      IF(MESH2%NPOIN.EQ.0) WRITE(LU,*) 'NO POINTS NPOIN = 0'
      IF(MESH2%NDP.EQ.0) WRITE(LU,*) 'NDP NOT INITIALISE'
      IF(MESH2%TYPE_ELEM.EQ.0) WRITE(LU,*) 'TYPE_ELEM NOT INITIALISE'
      IF(MESH2%NDIM.EQ.0) WRITE(LU,*) 'NDIM NOT INITIALISE'
      IF(MESH2%NVAR.NE.0) THEN
        IF(.NOT.ALLOCATED(MESH2%NAMEVAR))
     &      WRITE(LU,*) 'NAMEVAR NOT ALLOCATED'
        IF(.NOT.ALLOCATED(MESH2%UNITVAR))
     &      WRITE(LU,*) 'UNITVAR NOT ALLOCATED'
      ENDIF
      IF(.NOT.ALLOCATED(MESH2%IKLES))WRITE(LU,*) 'IKLES NOT ALLOCATED'
      IF(.NOT.ALLOCATED(MESH2%IPOBO))WRITE(LU,*) 'IPOBO NOT ALLOCATED'
      IF(.NOT.ALLOCATED(MESH2%X)) WRITE(LU,*) 'X NOT ALLOCATED'
      IF(.NOT.ALLOCATED(MESH2%Y)) WRITE(LU,*) 'Y NOT ALLOCATED'
      IF(MESH2%NDIM.EQ.3) THEN
        IF(.NOT.ALLOCATED(MESH2%Z)) WRITE(LU,*) 'Z NOT ALLOCATED'
      ENDIF
      IF(.NOT.ALLOCATED(MESH2%NAMECOO))
     &    WRITE(LU,*) 'NAMECOO NOT ALLOCATED'
      IF(.NOT.ALLOCATED(MESH2%UNITCOO))
     &    WRITE(LU,*) 'UNITCOO NOT ALLOCATED'
      IF(MESH2%TIMESTEP.NE.0) THEN
        IF(.NOT.ALLOCATED(MESH2%TIMES))
     &    WRITE(LU,*) 'TIMES NOT ALLOCATED'
        IF(.NOT.ALLOCATED(MESH2%RESULTS))
     &    WRITE(LU,*) 'RESULTS NOT ALLOCATED'
      ENDIF
      IF(MESH2%NFAM.NE.0) THEN
        IF(.NOT.ALLOCATED(MESH2%IDFAM))
     &       WRITE(LU,*) 'IDFAM NOT ALLOCATED'
        IF(.NOT.ALLOCATED(MESH2%VALFAM))
     &       WRITE(LU,*) 'VALFAM NOT ALLOCATED'
        IF(.NOT.ALLOCATED(MESH2%NAMEFAM))
     &       WRITE(LU,*) 'NAMEFAM NOT ALLOCATED'
      ENDIF
      IF(MESH2%NPTFR.NE.0) THEN
        IF(.NOT.ALLOCATED(MESH2%LIHBOR))
     &      WRITE(LU,*) 'LIHBOR NOT ALLOCATED'
        IF(.NOT.ALLOCATED(MESH2%NBOR))
     &      WRITE(LU,*) 'NBOR NOT ALLOCATED'
      ENDIF
      IF(MESH2%NELEM2.NE.0) THEN
        IF(MESH2%NDP2.EQ.0) WRITE(LU,*) 'NDP2 NOT INITIALISE'
        IF(MESH2%TYPE_ELEM2.EQ.0) WRITE(LU,*)
     &               'TYPE_ELEM2 NOT INITIALISE'
        IF(.NOT.ALLOCATED(MESH2%IKLES2))
     &       WRITE(LU,*) 'IKLES2 NOT ALLOCATED'
        IF(.NOT.ALLOCATED(MESH2%NCOLOR))
     &       WRITE(LU,*) 'NCOLOR NOT ALLOCATED'
        IF(.NOT.ALLOCATED(MESH2%NCOLOR2))
     &       WRITE(LU,*) 'NCOLOR2 NOT ALLOCATED'

      ENDIF
      WRITE(LU,*) '--------------------------------------------------'
      WRITE(LU,*) '------DUMPING MESH OBJECT'
      WRITE(LU,*) '--------------------------------------------------'
      WRITE(LU,*) '---GENERIC INFORMATION'
      WRITE(LU,*) 'TITLE :',TRIM(MESH2%TITLE)
      WRITE(LU,*) 'DESCRIPTION :',TRIM(MESH2%DESCRIPTION)
      WRITE(LU,*) 'NUMBER OF DIMENSION :',MESH2%NDIM
      WRITE(LU,*) 'COORDINATES NAMES :',MESH2%NAMECOO
      WRITE(LU,*) 'COORDINATES UNTIS :',MESH2%UNITCOO
      SELECT CASE(MESH2%TYPE_ELEM)
        CASE(TRIANGLE_ELT_TYPE)
          WRITE(LU,*) 'ELEMENT TYPE : TRIANGLE'
        CASE(QUADRANGLE_ELT_TYPE)
          WRITE(LU,*) 'ELEMENT TYPE : QUADRATIC'
        CASE(TETRAHEDRON_ELT_TYPE)
          WRITE(LU,*) 'ELEMENT TYPE : TETRAHEDRON'
        CASE(PRISM_ELT_TYPE)
          WRITE(LU,*) 'ELEMENT TYPE : PRISM'
      END SELECT
      WRITE(LU,*) 'NUMBER OF ELEMENT :',MESH2%NELEM
      IF(MESH2%NELEM2.NE.0) THEN
        SELECT CASE(MESH2%TYPE_ELEM2)
          CASE(TRIANGLE_ELT_TYPE)
            WRITE(LU,*) 'ELEMENT TYPE : TRIANGLE'
          CASE(QUADRANGLE_ELT_TYPE)
            WRITE(LU,*) 'ELEMENT TYPE : QUADRATIC'
          CASE(TETRAHEDRON_ELT_TYPE)
            WRITE(LU,*) 'ELEMENT TYPE : TETRAHEDRON'
          CASE(PRISM_ELT_TYPE)
            WRITE(LU,*) 'ELEMENT TYPE : PRISM'
        END SELECT
        WRITE(LU,*) 'NUMBER OF ELEMENT :',MESH2%NELEM2
      ENDIF
      WRITE(LU,*) 'NUMBER OF POINTS :',MESH2%NPOIN
      WRITE(LU,*) '---FAMILIES INFORMATION'
      IF(MESH2%NFAM.EQ.0) THEN
        WRITE(LU,*) 'NO FAMILIES'
      ELSE
        WRITE(LU,*) 'NUMBER OF FAMILIES'
        DO I=1,MESH2%NFAM
          WRITE(LU,*) 'NAMEFAM : ',MESH2%NAMEFAM(I)
          WRITE(LU,*) 'VALFAM : ',MESH2%VALFAM(I)
          WRITE(LU,*) 'IDFAM : ',MESH2%IDFAM(I)
          WRITE(LU,*) 'NGROUP : ',MESH2%NGROUPFAM(I)
          IF(MESH2%NGROUPFAM(I).NE.0) THEN
            DO J=1,MESH2%NGROUPFAM(I)
              WRITE(LU,*) 'GROUP : ',TRIM(MESH2%GROUPFAM(I,J))
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      WRITE(LU,*) '---RESULTS INFORMATION'
      IF(MESH2%TIMESTEP.EQ.0) THEN
        WRITE(LU,*) 'NO RESULTS'
      ELSE
        WRITE(LU,*) 'NUMBER OF TIME STEPS :',MESH2%TIMESTEP
        WRITE(LU,*) 'NUMBER OF VARIABLES :',MESH2%NVAR
        DO I=1,MESH2%NVAR
          WRITE(LU,*) 'NAME OF THE VARIABLE : ',MESH2%NAMEVAR(I)
          WRITE(LU,*) 'UNIT OF THE VARIABLE : ',MESH2%UNITVAR(I)
        ENDDO
      ENDIF
      END SUBROUTINE CHECK_MESH
!
      ! DEALLOCTE THE MESH OBJ TABLES
      SUBROUTINE FREE_MESH
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      IF(MESH2%NVAR.NE.0) THEN
        DEALLOCATE(MESH2%NAMEVAR)
        DEALLOCATE(MESH2%UNITVAR)
      ENDIF
      DEALLOCATE(MESH2%IKLES)
      DEALLOCATE(MESH2%IPOBO)
      DEALLOCATE(MESH2%X)
      DEALLOCATE(MESH2%Y)
      DEALLOCATE(MESH2%NAMECOO)
      DEALLOCATE(MESH2%UNITCOO)
      IF(MESH2%NDIM.EQ.3) DEALLOCATE(MESH2%Z)
      IF(MESH2%TIMESTEP.NE.0) THEN
        DEALLOCATE(MESH2%TIMES)
        DEALLOCATE(MESH2%RESULTS)
      ENDIF
      IF(MESH2%NFAM.NE.0) THEN
        DEALLOCATE(MESH2%NAMEFAM)
        DEALLOCATE(MESH2%IDFAM)
        DEALLOCATE(MESH2%VALFAM)
        DEALLOCATE(MESH2%NGROUPFAM)
        DEALLOCATE(MESH2%GROUPFAM)
      ENDIF
      IF(MESH2%NPTFR.NE.0) THEN
        DEALLOCATE(MESH2%LIHBOR)
        DEALLOCATE(MESH2%NBOR)
      ENDIF
      IF(MESH2%NELEM2.NE.0) THEN
        DEALLOCATE(MESH2%IKLES2)
      ENDIF
      IF(ALLOCATED(MESH2%NCOLOR)) DEALLOCATE(MESH2%NCOLOR)
      IF(ALLOCATED(MESH2%COLOR)) DEALLOCATE(MESH2%COLOR)
      IF(ALLOCATED(MESH2%NCOLOR2))DEALLOCATE(MESH2%NCOLOR2)
      END SUBROUTINE FREE_MESH
!
      ! CONVERT AN INT INTO A STRING WITH ZEROS
      SUBROUTINE NUMBER2STRING(RES,VAL)
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: VAL
      CHARACTER(LEN=*), INTENT(OUT) :: RES
      IF(VAL.LT.10) THEN
        WRITE(RES,'(A7,I1)') '0000000',VAL
      ELSE IF(VAL.LT.100) THEN
        WRITE(RES,'(A6,I2)') '000000',VAL
      ELSE IF(VAL.LT.1000) THEN
        WRITE(RES,'(A5,I3)') '00000',VAL
      ELSE IF(VAL.LT.10000) THEN
        WRITE(RES,'(A4,I4)') '0000',VAL
      ELSE IF(VAL.LT.100000) THEN
        WRITE(RES,'(A3,I5)') '000',VAL
      ELSE IF(VAL.LT.1000000) THEN
        WRITE(RES,'(A2,I6)') '00',VAL
      ELSE IF(VAL.LT.10000000) THEN
        WRITE(RES,'(A1,I7)') '0',VAL
      ELSE
        WRITE(RES,'(I8)') VAL
      ENDIF
      END SUBROUTINE NUMBER2STRING
!
!
!
      END MODULE DECLARATIONS_STBTEL
!
