!                    ***************************
                     MODULE DECLARATIONS_TELEMAC
!                    ***************************
!
!
!***********************************************************************
! BIEF
!***********************************************************************
!
!brief    DECLARATIONS COMMON TO ALL PROGRAMS
!+
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        09/05/2014
!+        V7P0
!+   Parameter MODASS added.
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        19/11/2016
!+        V7P3
!+        New modelling component added KHIONE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      IMPLICIT NONE
!----------------------------------------------------------------------
!
! 1./ INTEGER VALUES TO DESCRIBE BOUNDARY CONDITIONS:
!
!
!     FOR THE BOUNDARY CONDITIONS FILE:
!
!     ENTRANCE: PRESCRIBED VALUES (SAVE VELOCITIES)
      INTEGER, PARAMETER :: KENT  =  5
!
!     VELOCITY IMPOSED (INSTEAD OF DISCHARGE)
      INTEGER, PARAMETER :: KENTU =  6
!
!     FREE OUTPUT
      INTEGER, PARAMETER :: KSORT =  4
!
!     NO-SLIP CONDITION
      INTEGER, PARAMETER :: KADH  =  0
!
!     WALL WITH OR WITHOUT FRICTION
      INTEGER, PARAMETER :: KLOG  =  2
!
!     OPEN BOUNDARY WITH INCIDENT WAVE
      INTEGER, PARAMETER :: KINC  =  1
!
!     ESTEL-2D : FREE DRAINAGE
      INTEGER, PARAMETER :: KDRAIN  =  3
!
!     ESTEL-2D : MIXED CONDITION
      INTEGER, PARAMETER :: KMIX  =  4
!
!     DEPENDING ON ALGORITHMS AND CASES, THESE VALUES WILL BE
!     TRANSFORMED INTO:
!
!     TECHNICAL BOUNDARY CONDITIONS
!
!     NEUMANN
      INTEGER, PARAMETER :: KNEU  =  1
!
!     DIRICHLET
      INTEGER, PARAMETER :: KDIR  =  2
!
!     DEGREE OF FREEDOM
      INTEGER, PARAMETER :: KDDL  =  3
!
!     INCIDENT WAVE
      INTEGER, PARAMETER :: KOND  =  4
!
!----------------------------------------------------------------------
!
! 2./ INTEGER VALUES TO DESCRIBE ADVECTION SCHEMES:
!
!     NO ADVECTION
      INTEGER, PARAMETER :: ADV_VOID    =  0
!     CHARACTERISTICS
      INTEGER, PARAMETER :: ADV_CAR     =  1
!     SUPG
      INTEGER, PARAMETER :: ADV_SUP     =  2
!     LEO POSTMA
      INTEGER, PARAMETER :: ADV_LPO     =  3
!     DISTRIBUTIVE N-SCHEME
      INTEGER, PARAMETER :: ADV_NSC     =  4
!     DISTRIBUTIVE PSI SCHEME
      INTEGER, PARAMETER :: ADV_PSI     =  5
!     LEO POSTMA, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_LPO_TF  = 13
!     DISTRIBUTIVE N SCHEME, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_NSC_TF  = 14
!     DISTRIBUTIVE PSI SCHEME, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_PSI_TF  = 15
!
!-----------------------------------------------------------------------
!
! 3./ CODE COUPLING
!
      CHARACTER(LEN=PATH_LEN), TARGET :: COUPLING
!
! 4./ NAME OF CURRENT CODE (SEE BIEF_OPEN_FILES AND CONFIG_CODE)
!
      CHARACTER(LEN=24) :: NAMECODE,NNAMECODE(6)
!
! 5./ DIFFERENT WAYS OF SPLITTING PRISMS :
!
!     TO ENSURE MATCHING OF TETRAHEDRONS, FACES OF TRIANGLES ARE "SIGNED"
!     WITH 1 OR 2 DEPENDING OF THE GLOBAL NUMBERS OF THEIR POINTS, TAKEN IN
!     COUNTER-CLOCKWISE DIRECTION. A FACE 1 IN A TRIANGLE WILL BE 2 IN ITS
!     NEIGHBOUR AND THIS IS USED TO HAVE A CORRECT SPLITTING. THE SPLITTING
!     DEPENDING ON THE "SIGNS" OF THE 3 FACES IS GIVEN IN ARRAY TETRA.
!
!
!     TETRA(2,2,2,3,4)
!
!     FIRST 3 DIMENSIONS : TYPE OF FACE
!                      1 : CUT RECTANGLE BETWEEN  LOW-LEFT AND HIGH-RIGHT
!                      2 : CUT RECTANGLE BETWEEN  HIGH-LEFT AND LOW-RIGHT
!
!     4TH DIMENSION : NUMBER OF TETRAHEDRON
!     5TH DIMENSION : 4 POINTS OF THE TETRAHEDRON (IN LOCAL PRISM NUMBERING)
!
!     1 1 2 SPLITTING
!
!     TETRA(1,1,2,1,1)= 1
!     TETRA(1,1,2,1,2)= 2
!     TETRA(1,1,2,1,3)= 3
!     TETRA(1,1,2,1,4)= 6
!
!     TETRA(1,1,2,2,1)= 4
!     TETRA(1,1,2,2,2)= 6
!     TETRA(1,1,2,2,3)= 5
!     TETRA(1,1,2,2,4)= 1
!
!     TETRA(1,1,2,3,1)= 5
!     TETRA(1,1,2,3,2)= 2
!     TETRA(1,1,2,3,3)= 1
!     TETRA(1,1,2,3,4)= 6
!
!     2 1 1 SPLITTING
!
!     TETRA(2,1,1,1,1)= 1
!     TETRA(2,1,1,1,2)= 2
!     TETRA(2,1,1,1,3)= 3
!     TETRA(2,1,1,1,4)= 4
!
!     TETRA(2,1,1,2,1)= 4
!     TETRA(2,1,1,2,2)= 6
!     TETRA(2,1,1,2,3)= 5
!     TETRA(2,1,1,2,4)= 2
!
!     TETRA(2,1,1,3,1)= 6
!     TETRA(2,1,1,3,2)= 3
!     TETRA(2,1,1,3,3)= 2
!     TETRA(2,1,1,3,4)= 4
!
!     1 2 1 SPLITTING
!
!     TETRA(1,2,1,1,1)= 1
!     TETRA(1,2,1,1,2)= 2
!     TETRA(1,2,1,1,3)= 3
!     TETRA(1,2,1,1,4)= 5
!
!     TETRA(1,2,1,2,1)= 4
!     TETRA(1,2,1,2,2)= 6
!     TETRA(1,2,1,2,3)= 5
!     TETRA(1,2,1,2,4)= 3
!
!     TETRA(1,2,1,3,1)= 4
!     TETRA(1,2,1,3,2)= 1
!     TETRA(1,2,1,3,3)= 3
!     TETRA(1,2,1,3,4)= 5
!
!     2 2 1 SPLITTING
!
!     TETRA(2,2,1,1,1)= 1
!     TETRA(2,2,1,1,2)= 2
!     TETRA(2,2,1,1,3)= 3
!     TETRA(2,2,1,1,4)= 4
!
!     TETRA(2,2,1,2,1)= 4
!     TETRA(2,2,1,2,2)= 6
!     TETRA(2,2,1,2,3)= 5
!     TETRA(2,2,1,2,4)= 3
!
!     TETRA(2,2,1,3,1)= 5
!     TETRA(2,2,1,3,2)= 2
!     TETRA(2,2,1,3,3)= 4
!     TETRA(2,2,1,3,4)= 3
!
!     1 2 2 SPLITTING
!
!     TETRA(1,2,2,1,1)= 1
!     TETRA(1,2,2,1,2)= 2
!     TETRA(1,2,2,1,3)= 3
!     TETRA(1,2,2,1,4)= 5
!
!     TETRA(1,2,2,2,1)= 4
!     TETRA(1,2,2,2,2)= 6
!     TETRA(1,2,2,2,3)= 5
!     TETRA(1,2,2,2,4)= 1
!
!     TETRA(1,2,2,3,1)= 6
!     TETRA(1,2,2,3,2)= 3
!     TETRA(1,2,2,3,3)= 5
!     TETRA(1,2,2,3,4)= 1
!
!     2 1 2 SPLITTING
!
!     TETRA(2,1,2,1,1)= 1
!     TETRA(2,1,2,1,2)= 2
!     TETRA(2,1,2,1,3)= 3
!     TETRA(2,1,2,1,4)= 6
!
!     TETRA(2,1,2,2,1)= 4
!     TETRA(2,1,2,2,2)= 6
!     TETRA(2,1,2,2,3)= 5
!     TETRA(2,1,2,2,4)= 2
!
!     TETRA(2,1,2,3,1)= 4
!     TETRA(2,1,2,3,2)= 1
!     TETRA(2,1,2,3,3)= 6
!     TETRA(2,1,2,3,4)= 2
!
!     IMPORTANT : ON EACH LAYER THE BOTTOM TETRAHEDRONS MUST BE
!                 TREATED FIRST, SO THAT IKLE SENT TO SUBROUTINE
!                 VOISIN BE THE SAME AS WITH PRISMS OR TRIANGLES
!                 FOR THE NELEM2 FIRST ELEMENTS.
!                 CONSEQUENTLY THE FIRSt 3 POINTS OF TETRAHEDRON 1
!                 ARE ALWAYS 1,2 AND 3.
!
!     TETRA : SEE EXPLANATIONS ABOVE, THE 0 CORRESPOND TO SITUATIONS
!             THAT NEVER HAPPEN (TETRA(1,1,1,... OR TETRA(2,2,2,...)
      INTEGER :: TETRA(2,2,2,3,4)
      PARAMETER ( TETRA = RESHAPE( (/
     &    0,1,1,1,1,1,1,0,0,4,4,4,4,4,4,0,0,6,4,5,5,4,6,0,
     &    0,2,2,2,2,2,2,0,0,6,6,6,6,6,6,0,0,3,1,2,2,1,3,0,
     &    0,3,3,3,3,3,3,0,0,5,5,5,5,5,5,0,0,2,3,4,1,6,5,0,
     &    0,4,5,4,6,6,5,0,0,2,3,3,1,2,1,0,0,4,5,3,6,2,1,0
     &    /), SHAPE=(/ 2,2,2,3,4 /) ) )
!
!     NUMBERING OF SEGMENTS IN A TETRAHEDRON
!     ISEGT(N,I) : POINT NUMBER I OF SEGMENT N. GIVES THE POINT IN
!                  LOCAL NUMBERING
!
      INTEGER :: ISEGT(6,2)
      PARAMETER ( ISEGT = RESHAPE( (/
     &    1,2,3,1,2,3,2,3,1,4,4,4 /), SHAPE=(/ 6,2 /) ) )
!
! 6./ ASSEMBLY MODE
!
!     1: FINITE ELEMENT ASSEMBLY IN PARALLEL DONE DIRECTLY ON
!        DOUBLE PRECISION VALUES
!
!     2: FINITE ELEMENT ASSEMBLY IN PARALLEL DONE WITH INTEGERS TO AVOID
!        TRUNCATION ERRORS IN PARALLEL, DUE TO DIFFERENT ORDER OF
!        ADDITIONS OF MORE THAN 2 NUMBERS.
!
!     HERE INITIALISED AT 1, IN CASE NO KEYWORD IS DEDICATED TO THIS
!     PARAMETER
!
      INTEGER :: MODASS = 1
!
!     SAVED VARIABLE
!
      ! TODO: Init values in bief_ini
      ! matvec
      LOGICAL :: W_IS_FULL = .FALSE.
      ! solve
      TYPE(BIEF_OBJ)          :: TBB
      TYPE(BIEF_OBJ), TARGET  :: BB,BX
      LOGICAL :: FIRST_SOLVE = .TRUE.
      ! paraco
      INTEGER :: PARACO_MSG_TAG = 5000
      ! paracoi
      INTEGER :: PARACOI_MSG_TAG = 5000
      ! paracoi8
      INTEGER :: PARACOI8_MSG_TAG = 5000
      ! cvtrvf_pos_2
      LOGICAL :: DEJA_CPOS2 = .FALSE.
      INTEGER, ALLOCATABLE :: INDIC_CPOS2(:)
      ! positive_depths
      LOGICAL :: DEJA_PDEPT_ERIA = .FALSE.
      INTEGER, ALLOCATABLE :: INDIC_PDEPT_ERIA(:)
      LOGICAL :: DEJA_PDEPT_NERD = .FALSE.
      INTEGER, ALLOCATABLE :: INDIC_PDEPT_NERD(:)
      ! cvtrvf_pos
      LOGICAL :: DEJA_CPOS = .FALSE.
      INTEGER, ALLOCATABLE :: INDIC_CPOS(:)
      ! sd_solve_1
!     MANAGES THE SIZE OF ALLOCATABLE ARRAYS
      INTEGER :: SIZE_IN = 0
      INTEGER :: SIZE_IP = 0
      INTEGER :: SIZE_ISEGIP = 0
      INTEGER :: SIZE_IW1 = 0
      INTEGER :: SIZE_INDTRI = 0
      INTEGER :: SIZE_INX = 0
      INTEGER :: SIZE_IPX = 0
      INTEGER :: SIZE_AC = 0
      INTEGER :: SIZE_ACTRI = 0
      INTEGER :: SIZE_ISP = 0
      INTEGER :: SIZE_RSP = 0
      INTEGER, ALLOCATABLE :: INDTRI_SS1(:),INX_SS1(:),IPX_SS1(:)
      INTEGER, ALLOCATABLE :: IN_SS1(:),IP_SS1(:),ISP_SS1(:),IW1_SS1(:)
      INTEGER, ALLOCATABLE :: ISEGIP_SS1(:)
      DOUBLE PRECISION, ALLOCATABLE :: AC_SS1(:),ACTRI_SS1(:),RSP_SS1(:)
      ! sd_solve_4
      INTEGER, ALLOCATABLE          :: GLOSEG4_SS4(:)
      DOUBLE PRECISION, ALLOCATABLE :: XA_SS4(:),DA_SS4(:)
      DOUBLE PRECISION, ALLOCATABLE :: RHS_SS4(:),XINC_SS4(:)
      INTEGER :: SIZE_GLOSEG4 = 0
      INTEGER :: SIZE_DA = 0
      INTEGER :: SIZE_XA = 0
      INTEGER :: SIZE_RHS = 0
      INTEGER :: SIZE_XINC = 0
      ! pre4_mumps
      INTEGER, ALLOCATABLE          :: GLOSEG4_P4M(:)
      DOUBLE PRECISION, ALLOCATABLE :: XA_P4M(:),DA_P4M(:)
      DOUBLE PRECISION, ALLOCATABLE :: RHS_P4M(:),XINC_P4M(:)
      INTEGER :: SIZE_GLOSEG4_P4M = 0
      INTEGER :: SIZE_DA_P4M = 0
      INTEGER :: SIZE_XA_P4M = 0
      INTEGER :: SIZE_RHS_P4M = 0
      INTEGER :: SIZE_XINC_P4M = 0
      ! charac
      LOGICAL :: DEJA_CHARAC = .FALSE.
      TYPE(BIEF_OBJ), TARGET  :: T1WEAK,T2WEAK,T3WEAK,T4WEAK,T5WEAK
      TYPE(BIEF_OBJ), TARGET  :: T6WEAK,T7WEAK,SHPWEA
      TYPE(BIEF_OBJ), TARGET  :: FTILD_WEAK,SHPBUF,SHZBUF,SHZWEA
      ! derive
      LOGICAL :: DEJA_DERIVE = .FALSE.
      TYPE(BIEF_OBJ) :: SVOID_DERIVE
!     DEFINE VARIABLES THAT ARE USED IN ALGAE TRANSPORT
!     THESE ARE NECESSARY IF NFLOT_MAX IS TOO LARGE
      LOGICAL :: INIT_ALG = .TRUE.
      INTEGER SIZEBUF2_D
      DOUBLE PRECISION,DIMENSION(:)  ,ALLOCATABLE::BUFF_1D_D
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::BUFF_2D_D
!
! 7./ CHECKING THE MESH
!
!     KEYWORD INITIALISED TO FALSE (MAY BE MODIFIED BY KEYWORDS)
!
      LOGICAL :: CHECK_MESH = .FALSE.
!
!-----------------------------------------------------------------------
!
      INTEGER :: GRACJG_CNT = 0

      SAVE
!
!-----------------------------------------------------------------------
!
      END MODULE DECLARATIONS_TELEMAC