!                    ************************
                     PROGRAM IDENTIFY_LIQ_BND
!                    ************************
!
!
!***********************************************************************
! IDENTIFY_LIQ_BND
!***********************************************************************
!
!brief    Returns the liquid boundary informations
!
!history  Y. AUDOUIN (EDF)
!+        01/2018
!+        V8P0
!+        FIRST  VERSION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARTEL
      USE MOD_NUMBERING_OPEN_BOUNDARIES
      USE BIEF, ONLY : NBMAXNSHARE, NPTIR,
     &                 READ_MESH_INFO, FRONT2, NCSIZE
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CHARACTER(LEN=PATH_LEN) :: NAMEGEO
      CHARACTER(LEN=PATH_LEN) :: NAMECLI
      CHARACTER(LEN=8) :: FFORMAT
!
      CHARACTER(LEN=80)  :: TITLE
      LOGICAL :: IS
      INTEGER NVAR, NPLAN, NPTFR, NELEBD
      INTEGER NELEM, NPOIN, NDP, NELEM2, NPOIN2, NDP_BND
      INTEGER TYP_ELEM, TYP_BND_ELEM
      INTEGER DIM_MESH
!
      INTEGER, ALLOCATABLE :: IKLES(:)
      INTEGER, ALLOCATABLE :: IKLES3D(:)
      INTEGER, ALLOCATABLE :: LIHBOR(:),LIUBOR(:)
      INTEGER, ALLOCATABLE :: LIVBOR(:),LITBOR(:),COLOR(:)
      DOUBLE PRECISION, ALLOCATABLE :: HBOR(:),UBOR(:),VBOR(:)
      DOUBLE PRECISION, ALLOCATABLE :: CHBORD(:)
      DOUBLE PRECISION, ALLOCATABLE :: TBOR(:),ATBOR(:),BTBOR(:)
      INTEGER, ALLOCATABLE :: NBOR(:),IKLE_BND(:)
      INTEGER, ALLOCATABLE :: NUMLIQ(:)
      INTEGER, ALLOCATABLE :: IFABOR(:,:), NELBOR(:)
      INTEGER, ALLOCATABLE :: IKLE(:,:)
      INTEGER, ALLOCATABLE :: KP1BOR(:,:)
!
!     FOR DOUBLE PRECISION SERAFIN FORMAT
!
      DOUBLE PRECISION, ALLOCATABLE    :: F(:,:)
!
      INTEGER :: NINP
      INTEGER :: NOUT
      INTEGER TIME(3),DATE(3), DATE_TMP(6)
      INTEGER :: I,J,K,IERR
      integer :: first, last, nfrliq
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      LNG=2 ! JE NE PARLE PAS FRANCAIS, JE SUIS BARBARIEN
      LU=6  ! FORTRAN STANDARD OUPUT CHANNEL
      LI=5  ! FORTRAN STANDARD INPUT CHANNEL
!
!----------------------------------------------------------------------
! INTRODUCE YOURSELF
!
!----------------------------------------------------------------------
! NAMES OF THE INPUT FILES:
!
!
      WRITE(LU,*) '--INPUT GEOMETRY FILE NAME <INPUT_NAME>: '
      READ(LI,*) NAMEGEO
!
      IF (NAMEGEO.EQ.' ') THEN
        WRITE (LU,*) ' NO FILENAME '
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'INPUT: ',TRIM(NAMEGEO)
      ENDIF

      INQUIRE (FILE=NAMEGEO,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*)' FILE DOES NOT EXIST: ',TRIM(NAMEGEO)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      WRITE(LU,*)
     & '--INPUT FILE FORMAT <FFORMAT> [MED,SERAFIN,SERAFIND]: '
      READ(LI,*) FFORMAT
      IF ( (FFORMAT .NE. 'MED     ') .AND.
     &     (FFORMAT(1:7) .NE. 'SERAFIN') ) THEN
        WRITE(LU,*)
     &  ' FILE FORMAT MUST BE "MED" OR "SERAFIN" OR "SERAFIND" '
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) ' INPUT: ', FFORMAT
      ENDIF
!
      WRITE(LU, *) '--BOUNDARY CONDITIONS FILE NAME: '
      READ(LI,*) NAMECLI
      IF (NAMECLI.EQ.' ') THEN
        WRITE (LU,*) ' NO FILENAME '
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'INPUT: ',TRIM(NAMECLI)
      ENDIF
!
      INQUIRE (FILE=NAMECLI,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ',TRIM(NAMECLI)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     Reading mesh informations
!
      ! SET NCSIZE TO 1 TO USE VOISIN AND READ_MESH_INFO IN SERIAL MODE
      NCSIZE=1
!
      CALL OPEN_MESH(FFORMAT, NAMEGEO, NINP, 'READ     ', IERR)
      CALL CHECK_CALL(IERR, 'PARTEL:OPENMESH:INP')
!
      CALL OPEN_BND(FFORMAT,NAMECLI,NINP,'READ     ',IERR)
      CALL CHECK_CALL(IERR,'PARTEL:OPEN_BND:NCLI')
!
!----------------------------------------------------------------------
!
! START READING THE GEOMETRY OR RESULT FILE
!
      CALL READ_MESH_INFO(FFORMAT,NINP,TITLE,NVAR,NPOIN,TYP_ELEM,NELEM,
     &                    NPTFR,NPTIR,NDP,NPLAN,TYP_BND_ELEM,NELEBD)
!
! READ THE REST OF THE SELAFIN FILE
! 10 INTEGERS, THE FIRST IS THE NUMBER OF RECORDS (TIMESTEPS)
!
      CALL GET_MESH_DATE(FFORMAT,NINP,DATE_TMP,IERR)
      CALL CHECK_CALL(IERR,'PARTEL:GET_MESH_DATE:INP')
      DO I=1,3
        DATE(I) = DATE_TMP(I)
        TIME(I) = DATE_TMP(I+3)
      ENDDO
!
      IF(NPLAN.GT.1) THEN
        NPOIN2 = NPOIN/NPLAN
        NELEM2 = NELEM/(NPLAN-1)
        DIM_MESH = 3
      ELSE
        NPOIN2 = NPOIN
        NELEM2 = NELEM
        DIM_MESH = 2
      ENDIF
!
! NOW LET US ALLOCATE
!
      ALLOCATE (IKLES(NELEM2*3),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'IKLES')
      IF(NPLAN.GT.1) THEN
        ALLOCATE (IKLES3D(NELEM*NDP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'IKLES3D')
      ENDIF
!
!     SIZE 3: FIRST TWO FUNCTIONS ARE X AND Y, 3 IS ALL OTHER
!             VARIABLES (THEY WILL BE COPIED AND WRITTEN
!             ONE AFTER THE OTHER...)
!     NPOIN IS 3D HERE IN 3D
!
      ALLOCATE (F(NPOIN,2),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'F')
!
! CONNECTIVITY TABLE:
!
      IF(NPLAN.LE.1) THEN
        CALL GET_MESH_CONNECTIVITY(FFORMAT,NINP,TYP_ELEM,IKLES,
     &                             NELEM,NDP,IERR)
        CALL CHECK_CALL(IERR,'PARTEL:GET_MESH_CONNECTIVITY:2D')
      ELSE
        CALL GET_MESH_CONNECTIVITY(FFORMAT,NINP,TYP_ELEM,IKLES3D,
     &                             NELEM,NDP,IERR)
        CALL CHECK_CALL(IERR,'PARTEL:GET_MESH_CONNECTIVITY:3D')
!       BUILDING IKLES
        DO J=1,3
          DO K=1,NELEM2
            IKLES((K-1)*3+J)=IKLES3D((K-1)*6+J)
          ENDDO
        ENDDO
      ENDIF
!
! X-, Y-COORDINATES
!
      CALL GET_MESH_COORD(FFORMAT,NINP,1,2,NPOIN,F(:,1),IERR)
      CALL CHECK_CALL(IERR,'PARTEL:GET_MESH_COORD:X')
      CALL GET_MESH_COORD(FFORMAT,NINP,2,2,NPOIN,F(:,2),IERR)
      CALL CHECK_CALL(IERR,'PARTEL:GET_MESH_COORD:Y')
!
!----------------------------------------------------------------------
!
!     READ THE BOUNDARY CONDITIONS FILE
!
      !
      CALL GET_NODES_PER_ELEMENT(TYP_BND_ELEM,NDP_BND)
      ! GET THE NUMBER OF BOUNDARY POINTS AND ELEMENTS
      CALL GET_BND_NELEM(FFORMAT,NINP,TYP_BND_ELEM,NELEBD,IERR)
      CALL CHECK_CALL(IERR,'PARTEL:GET_BND_NELEBD:NCLI')
      CALL GET_BND_NPOIN(FFORMAT,NINP,TYP_BND_ELEM,NPTFR,IERR)
      CALL CHECK_CALL(IERR,'PARTEL:GET_BND_NPOIN:NCLI')
      !
      ALLOCATE(IKLE_BND(NELEBD*NDP_BND),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:IKLE_BND')
      ALLOCATE(NBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:NBOR')
      ! ALLOCATING ARRAY FOR THE BOUNDARY CONDITIONS
      ALLOCATE(LIHBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:LIHBOR')
      ALLOCATE(LIUBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:LIUBOR')
      ALLOCATE(LIVBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:LIVBOR')
      ALLOCATE(HBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:HBOR')
      ALLOCATE(UBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:UBOR')
      ALLOCATE(VBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:VBOR')
      ALLOCATE(CHBORD(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:CHBORD')
      ALLOCATE(LITBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:LITBOR')
      ALLOCATE(TBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:TBOR')
      ALLOCATE(ATBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:ATBOR')
      ALLOCATE(BTBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'PARTEL:BTBOR')
      ALLOCATE (NUMLIQ(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'NUMLIQ')
      ALLOCATE (COLOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'COLOR')
      ! Get the connectivity table for the boundary elements
      CALL GET_BND_CONNECTIVITY(FFORMAT,NINP,TYP_BND_ELEM,NELEBD,
     &                    NDP_BND,IKLE_BND,IERR)
      CALL CHECK_CALL(IERR,'PARTEL:GET_BND_CONNECTIVITY:NCLI')
      ! FILL NBOR
      CALL GET_BND_NUMBERING(FFORMAT,NINP,TYP_BND_ELEM,NPTFR,NBOR,IERR)
      ! GET THE VALUE OF EACH BOUNDARY
      CALL GET_BND_VALUE(FFORMAT,NINP,TYP_BND_ELEM,NELEBD,
     &                   LIHBOR,LIUBOR,
     &                   LIVBOR,HBOR,UBOR,VBOR,CHBORD,.TRUE.,
     &                   LITBOR,TBOR,ATBOR,BTBOR,NPTFR,IERR)
      CALL CHECK_CALL(IERR,'PARTEL:GET_BND_VALUE:NCLI')
      CALL GET_BND_COLOR(FFORMAT,NINP,TYP_BND_ELEM,NELEBD,
     &                   COLOR,IERR)
      CALL CHECK_CALL(IERR,'PARTEL:GET_BND_COLOR:NCLI')
!
!     Computing liquid bounndaries info
!
      CALL NUMBERING_OPEN_BOUNDARIES
     & (NAMEGEO, IKLE, IKLES,
     &  KP1BOR, NUMLIQ, DIM_MESH, NPOIN2, NPTFR, NPOIN, NELEM2,
     &  NELBOR, LIUBOR, LIHBOR, NBOR, IFABOR, F)

      ! Closing the files
      CALL CLOSE_BND(FFORMAT,NINP,IERR)
      CALL CHECK_CALL(IERR,'PARTEL:CLOSE_BND:NCLI')
      CALL CLOSE_MESH(FFORMAT,NINP,IERR)
      CALL CHECK_CALL(IERR,'PARTEL:CLOSE_MESH:NINP')
!
!     Output informations in file
!
      CALL GET_FREE_ID(NOUT)
      OPEN(FILE="liq_bnd.txt", ACTION='WRITE', UNIT=nout,
     &     FORM='FORMATTED', IOSTAT=IERR)
      CALL CHECK_CALL(IERR, 'opening liq_bnd.txt')
      ! Finding the first liquid boundary
      NFRLIQ = MAXVAL(NUMLIQ)
      WRITE(NOUT,*) NFRLIQ
      DO I=1,NFRLIQ
!
!  MARKS THE NUMBERS OF THE LIQUID BOUNDARIES
!
        ! LOKKING FOR FIRST POINT
        DO J=1,NPTFR
          IF(NUMLIQ(J).NE.I .AND. NUMLIQ(MOD(J,NPTFR)+1).EQ.I) THEN
            FIRST = MOD(J,NPTFR)+1
            EXIT
          ENDIF
        ENDDO
        ! LOOKING FOR LAST POINT
        DO J=1,NPTFR
          IF(NUMLIQ(J).EQ.I .AND. NUMLIQ(MOD(J,NPTFR)+1).NE.I) THEN
            LAST = J
            EXIT
          ENDIF
        ENDDO
        WRITE(NOUT,*) I, FIRST, NBOR(FIRST),
     &                F(NBOR(FIRST),1), F(NBOR(FIRST),2),
     &                LAST, NBOR(LAST),
     &                F(NBOR(LAST),1), F(NBOR(LAST),2)
      ENDDO

      CLOSE(NOUT)
!
!     Deallocation
!
      DEALLOCATE(IKLES)
      IF(NPLAN.GT.1) THEN
        DEALLOCATE(IKLES3D)
      ENDIF
      DEALLOCATE(F)
      DEALLOCATE(IKLE_BND)
      DEALLOCATE(NBOR)
      DEALLOCATE(LIHBOR)
      DEALLOCATE(LIUBOR)
      DEALLOCATE(LIVBOR)
      DEALLOCATE(HBOR)
      DEALLOCATE(UBOR)
      DEALLOCATE(VBOR)
      DEALLOCATE(CHBORD)
      DEALLOCATE(LITBOR)
      DEALLOCATE(TBOR)
      DEALLOCATE(ATBOR)
      DEALLOCATE(BTBOR)
      DEALLOCATE (NUMLIQ)
      DEALLOCATE (COLOR)
      STOP 0
      END PROGRAM
