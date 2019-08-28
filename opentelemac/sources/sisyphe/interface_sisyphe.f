!
        MODULE INTERFACE_SISYPHE
!
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE_BEDLOAD
      USE INTERFACE_SISYPHE_SUSPENSION
!
      INTERFACE
        SUBROUTINE AFFECT_MAT(IELMU)
          USE BIEF_DEF
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: IELMU
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE SIS_ARRET
     &(ESM,EMAX,HN,VARSOR,NPOIN,MN,NRES,FMTRES,MAXVAR,AT0,RC,
     & TEXTE,SORLEO,SORIMP,T1,T2)
        USE BIEF_DEF
        IMPLICIT NONE
      TYPE(BIEF_OBJ),    INTENT(IN)    :: ESM, EMAX, HN, VARSOR
      INTEGER,           INTENT(IN)    :: NPOIN, MN, NRES, MAXVAR
      DOUBLE PRECISION,  INTENT(IN)    :: AT0, RC
      CHARACTER(LEN=32), INTENT(IN)    :: TEXTE(MAXVAR)
      CHARACTER(LEN=8),  INTENT(IN)    :: FMTRES
      LOGICAL,           INTENT(IN)    :: SORLEO(*), SORIMP(*)
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T1, T2
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE BILAN_SISYPHE
     &(E,ESOMT,T1,VCUMU,DT,NPTFR,
     & INFO,ZFCL_C,ZFCL_S,ZFCL_MS,
     & NSICLA,VOLTOT,
     & NUMLIQ,NFRLIQ,FLBCLA,LT,NIT,NPOIN,VOLU2D,CSF_SABLE,MASDEP,
     & MASDEPT,CHARR,SUSP,SLIDE)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)          :: NPTFR,NFRLIQ,NSICLA,LT,NIT
      INTEGER, INTENT(IN)          :: NPOIN,NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: DT
      LOGICAL, INTENT(IN)          :: INFO,SUSP,SLIDE,CHARR
      DOUBLE PRECISION, INTENT(INOUT) :: VCUMU
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE,VOLTOT(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: MASDEP(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASDEPT(NSICLA)
      TYPE(BIEF_OBJ), INTENT(IN)    :: ZFCL_C
      TYPE(BIEF_OBJ), INTENT(IN)    :: E,ESOMT,VOLU2D,ZFCL_S
      TYPE(BIEF_OBJ), INTENT(IN)    :: ZFCL_MS
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,FLBCLA
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE CALCUW( UW, H, HW, TW, GRAV ,NPOIN)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: NPOIN
        DOUBLE PRECISION, INTENT(INOUT) :: UW(NPOIN)
        DOUBLE PRECISION, INTENT(IN) :: H(NPOIN)
        DOUBLE PRECISION, INTENT(IN) :: TW(NPOIN),HW(NPOIN)
        DOUBLE PRECISION, INTENT(IN) :: GRAV
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE COEFRO_SISYPHE
     &  (CF,H,KFROT,CHESTR,GRAV,NPOIN,HMIN,KARMAN)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER, INTENT(IN):: NPOIN,KFROT
        DOUBLE PRECISION,INTENT(IN):: GRAV,KARMAN,HMIN
        TYPE(BIEF_OBJ), INTENT(INOUT) :: CF
        TYPE(BIEF_OBJ),INTENT(IN) :: CHESTR,H
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE CONDIM_SISYPHE
     & (U      , V   , QU    , QV  , H   , ZF , Z ,
     &  ESOMT   , THETAW ,  Q     , HW  , TW  ,
     &  X      , Y   , NPOIN , AT  , PMAREE)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: NPOIN
        DOUBLE PRECISION, INTENT(IN) :: X(NPOIN) , Y(NPOIN)
        DOUBLE PRECISION, INTENT(IN) :: AT , PMAREE
        DOUBLE PRECISION, INTENT(INOUT) ::  ZF(NPOIN)
        DOUBLE PRECISION, INTENT (INOUT)::  ESOMT(NPOIN)
        DOUBLE PRECISION, INTENT(INOUT) ::  Z(NPOIN) , H(NPOIN)
        DOUBLE PRECISION, INTENT(INOUT) ::  U(NPOIN) , V(NPOIN)
        DOUBLE PRECISION, INTENT (INOUT)::  QU(NPOIN), QV(NPOIN)
        DOUBLE PRECISION, INTENT (INOUT):: Q(NPOIN)
        DOUBLE PRECISION, INTENT (INOUT)::  HW(NPOIN) , TW(NPOIN)
        DOUBLE PRECISION, INTENT (INOUT):: THETAW (NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE CONDIM_SUSP(CS,CS0,NSICLA)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER, INTENT(IN)           :: NSICLA
        DOUBLE PRECISION,INTENT(IN)   :: CS0(NSICLA)
        TYPE(BIEF_OBJ), INTENT(INOUT) :: CS
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE CONDIS_SISYPHE (CONSTFLOW)
        USE BIEF_DEF
        IMPLICIT NONE
        LOGICAL, INTENT(INOUT) :: CONSTFLOW
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE CONLIT(NBOR,AT)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER, INTENT(IN):: NBOR(*)
        DOUBLE PRECISION, INTENT(IN):: AT
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        DOUBLE PRECISION FUNCTION CGL( I , AT)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: I
          DOUBLE PRECISION, INTENT(IN):: AT
        END FUNCTION
      END INTERFACE
!
      INTERFACE
        SUBROUTINE USER_CGL( CGL, I , AT)
          IMPLICIT NONE
          DOUBLE PRECISION, INTENT(INOUT) :: CGL
          INTEGER, INTENT(IN) :: I
          DOUBLE PRECISION, INTENT(IN):: AT
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        DOUBLE PRECISION FUNCTION QGL( I , AT)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: I
          DOUBLE PRECISION, INTENT(IN):: AT
        END FUNCTION
      END INTERFACE
!
      INTERFACE
        SUBROUTINE USER_QGL( QGL, I , AT)
          IMPLICIT NONE
          DOUBLE PRECISION, INTENT(INOUT) :: QGL
          INTEGER, INTENT(IN) :: I
          DOUBLE PRECISION, INTENT(IN):: AT
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE CORSTR_SISYPHE
        IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE

! UHM + PAT: required to compile with Python
      INTERFACE
        DOUBLE PRECISION FUNCTION CVSP_INTEGRATE_VOLUME
     &(J,I,Z_HIGH,Z_LOW,A)
        USE DECLARATIONS_SISYPHE
        IMPLICIT NONE
        INTEGER,          INTENT(IN)   :: J
        INTEGER,          INTENT(IN)   :: I
        DOUBLE PRECISION, INTENT(IN)    :: Z_HIGH
        DOUBLE PRECISION, INTENT(IN)    :: Z_LOW
        DOUBLE PRECISION, INTENT(OUT) :: A(NSICLA)
        END FUNCTION
      END INTERFACE
!

! UHM + PAT
      INTERFACE
      LOGICAL FUNCTION CVSP_CHECK_F
     &     (J,K, SOMETEXT)
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: K
      CHARACTER(LEN=10),     INTENT(IN)    :: SOMETEXT
      END FUNCTION
      END INTERFACE

      INTERFACE
      LOGICAL FUNCTION CVSP_CHECK_L
     &     (J,K, SOMETEXT)
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: K
      CHARACTER(LEN=10),     INTENT(IN)    :: SOMETEXT
      END FUNCTION
      END INTERFACE
!

! UHM + PAT
      INTERFACE
        DOUBLE PRECISION FUNCTION CVSP_ALT(J, FORMULA)
        IMPLICIT NONE
        INTEGER,          INTENT(IN)    :: J
        INTEGER,          INTENT(IN)    :: FORMULA
        END FUNCTION
      END INTERFACE
!

! UHM + PAT
      INTERFACE
        LOGICAL FUNCTION CVSP_DB(J_GLOBAL, TIMESTAMP)
        IMPLICIT NONE
        INTEGER, INTENT(IN)    :: J_GLOBAL
        INTEGER, INTENT(IN)    :: TIMESTAMP
        END FUNCTION
      END INTERFACE
!

      INTERFACE
        SUBROUTINE DEBUG_SISYPHE (NAME, ILOOP, NLOOP)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER, PARAMETER :: SIZE =100
        CHARACTER(LEN=SIZE)  , INTENT(IN) :: NAME
        INTEGER,   INTENT(IN), OPTIONAL :: ILOOP, NLOOP
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE DISIMP
     &(Q,Q2BOR,NUMLIQ,IFRLIQ,NSOLDIS,WORK1,QBOR,NPTFR,MASK,MESH)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPTFR,IFRLIQ,NSOLDIS
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: MASK(NPTFR),Q
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: WORK1,QBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: Q2BOR
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE ENTETE_SISYPHE(IETAPE,AT,LT)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: AT
        INTEGER, INTENT(IN):: LT,IETAPE
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
      SUBROUTINE FLUSEC_SISYPHE
     &(U,V,H,QSXC,QSYC,CHARR,QSXS,QSYS,SUSP,
     & IKLE,NELMAX,NELEM,X,Y,DT,NCP,CTRLSC,INFO,TPS)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER, INTENT(IN)          :: NELMAX,NELEM,NCP
        INTEGER, INTENT(IN)          :: IKLE(NELMAX,*)
        INTEGER, INTENT(IN)          :: CTRLSC(NCP)
        DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),TPS,DT
        LOGICAL, INTENT(IN)          :: INFO,SUSP,CHARR
        TYPE(BIEF_OBJ), INTENT(IN)   :: U,V,H,QSXC,QSYC,QSXS,QSYS
      END SUBROUTINE
      END INTERFACE
!
      INTERFACE
      SUBROUTINE FLUXPR_SISYPHE
     &(NSEC, CTRLSC, FLX, VOLNEG, VOLPOS, INFO, TPS, NSEG, NCSIZE,  !JAJ #### TPS
     & FLXS,VOLNEGS,VOLPOSS,SUSP,FLXC,VOLNEGC,VOLPOSC,CHARR)
      IMPLICIT NONE
      INTEGER, INTENT(IN)          :: NSEC,NCSIZE
      INTEGER, INTENT(IN)          :: CTRLSC(*)
      INTEGER, INTENT(IN)          :: NSEG(NSEC)
      LOGICAL, INTENT(IN)          :: INFO,SUSP,CHARR
      DOUBLE PRECISION, INTENT(IN) :: FLX(NSEC), TPS                !JAJ #### TPS
      DOUBLE PRECISION, INTENT(IN) :: VOLNEG(NSEC),VOLPOS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: FLXS(NSEC),FLXC(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEGS(NSEC),VOLPOSS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEGC(NSEC),VOLPOSC(NSEC)
      END SUBROUTINE
      END INTERFACE
!
      INTERFACE
      SUBROUTINE GF_USER(TBEG_GF,TEND_GF,AT0)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT) :: TBEG_GF, TEND_GF
      DOUBLE PRECISION, INTENT(IN)    :: AT0
      END SUBROUTINE
      END INTERFACE
!
      INTERFACE
      SUBROUTINE INIT_AVAI
      USE BIEF_DEF
      IMPLICIT NONE
      END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE INIT_COMPO(NCOUCHES)
          USE BIEF_DEF
          IMPLICIT NONE
          INTEGER, INTENT(INOUT):: NCOUCHES(*)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE INIT_COMPO_COH
     &(ES,CONC_VASE,CONC,NPOIN,NOMBLAY,NSICLA,AVAIL,AVA0)
        USE BIEF
      INTEGER, INTENT(IN)              :: NPOIN,NOMBLAY,NSICLA
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
        END SUBROUTINE
      END INTERFACE

!
      INTERFACE
        SUBROUTINE INIT_CONSTANT
     &  (KARIM_HOLLY_YANG, KARMAN,  PI)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(INOUT) :: KARIM_HOLLY_YANG
        DOUBLE PRECISION, INTENT(INOUT) :: KARMAN
        DOUBLE PRECISION, INTENT(INOUT) :: PI
!
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE INIT_MIXTE
     &(XMVS,NPOIN,AVAIL,NSICLA,ES,ES_SABLE, ES_VASE,ELAY,NOMBLAY,
     & CONC_VASE,MS_SABLE,MS_VASE,ZF,ZR,AVA0,CONC,DEBU,MIXTE)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)              :: NPOIN,NSICLA,NOMBLAY
      DOUBLE PRECISION, INTENT(IN)     :: XMVS
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: ELAY(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: ZR(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
!
      DOUBLE PRECISION,  INTENT(INOUT) :: ES_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: ES_VASE(NPOIN,NOMBLAY)
!
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
      LOGICAL, INTENT (IN)             :: DEBU, MIXTE
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
      SUBROUTINE INIT_SEDIMENT
     &(NSICLA,ELAY,ZF,ZR,NPOIN,AVAIL,FRACSED_GF,AVA0,
     & LGRAFED,CALWC,XMVS,XMVE,GRAV,VCE,XWC,FDM,
     & CALAC,AC,SEDCO,ES,ES_SABLE, ES_VASE ,NOMBLAY,CONC_VASE,
     & MS_SABLE,MS_VASE,ACLADM,UNLADM,TOCE_SABLE,
     & CONC,DEBU,MIXTE)
      USE BIEF
      IMPLICIT NONE
      INTEGER,           INTENT(IN)     :: NSICLA,NPOIN,NOMBLAY
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ELAY,ZF,ZR
      TYPE(BIEF_OBJ), INTENT(INOUT)     :: MS_SABLE, MS_VASE
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ACLADM, UNLADM
      LOGICAL,           INTENT(IN)     :: LGRAFED,CALWC
      LOGICAL,           INTENT(IN)     :: CALAC
      DOUBLE PRECISION,  INTENT(IN)     :: XMVS,XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVA0(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FRACSED_GF(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FDM(NSICLA),XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AC(NSICLA),TOCE_SABLE
      LOGICAL,           INTENT(IN)     :: SEDCO(NSICLA), DEBU
      LOGICAL,           INTENT(IN)     :: MIXTE
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      END SUBROUTINE
      END INTERFACE
!
!
      INTERFACE
        SUBROUTINE INIT_TRANSPORT
     &(TROUVE,DEBU,HIDING,NSICLA,NPOIN,
     & T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     & CHARR,QS_C,QSXC,QSYC,CALFA_CL,SALFA_CL,COEFPN,SLOPEFF,
     & SUSP,QS_S,QS,QSCL,QSCL_C,QSCL_S,QSCLXS,QSCLYS,
     & UNORM,U2D,V2D,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,HOULE,
     & AVAIL,ACLADM,UNLADM,KSP,KSR,KS,
     & ICF,HIDFAC,XMVS,XMVE,GRAV,VCE,HMIN,KARMAN,
     & ZERO,PI,AC,IMP_INFLOW_C,ZREF,ICQ,CSTAEQ,CSRATIO,
     & CMAX,CS,CS0,SECCURRENT,BIJK,
     & IELMT,FDM,XWC,FD90,SEDCO,VITCE,PARTHENIADES,VITCD,
     & U3D,V3D,CODE)
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NOMBLAY,MPM_ARAY,MPM
      IMPLICIT NONE
      INTEGER, INTENT(IN)              :: NSICLA,NPOIN,TROUVE(*),ICQ
      INTEGER, INTENT(IN)              :: ICF,HIDFAC,IELMT,SLOPEFF
      LOGICAL, INTENT(IN)              :: CHARR,DEBU,SUSP,IMP_INFLOW_C
      LOGICAL, INTENT(IN)              :: SECCURRENT,SEDCO(*)
      LOGICAL, INTENT(IN)              :: HOULE
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U2D,V2D,UNORM,HN,CF
      TYPE(BIEF_OBJ),    INTENT(IN)    :: MU,TOB,TOBW,UW,TW,THETAW,FW
      TYPE(BIEF_OBJ),    INTENT(IN)    :: ACLADM,UNLADM,KSP,KSR,KS
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QS_C, QSXC, QSYC
      TYPE(BIEF_OBJ),    INTENT(INOUT) ::  CALFA_CL,SALFA_CL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T9,T10,T11,T12
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: ZREF,CSTAEQ,CSRATIO
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: CS
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QS_S,QS,QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: COEFPN
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QSCLXS,QSCLYS,QSCL
      DOUBLE PRECISION,  INTENT(IN)    :: XMVS,XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(IN)    :: HMIN,KARMAN,ZERO,PI
      DOUBLE PRECISION,  INTENT(IN)    :: PARTHENIADES,BIJK,XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(IN)    :: FD90(NSICLA),CS0(NSICLA)
      DOUBLE PRECISION,  INTENT(IN)    :: VITCE,VITCD
      DOUBLE PRECISION,  INTENT(INOUT) :: AC(NSICLA),CMAX,FDM(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE INTEG ( A , B , IEIN , NPOIN)
        IMPLICIT NONE
        INTEGER, INTENT(IN):: NPOIN
        DOUBLE PRECISION, INTENT(IN)::A(NPOIN), B(NPOIN)
        DOUBLE PRECISION, INTENT(INOUT)::IEIN(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE LAYER
     &(ZFCL_W,NLAYER,ZR,ZF,ESTRAT,ELAY,MASBAS,
     & ACLADM,NSICLA,NPOIN,ELAY0,VOLTOT,
     & ES,AVAIL,CONST_ALAYER,ESTRATNEW,NLAYNEW)
      USE BIEF_DEF
      USE DECLARATIONS_SISYPHE, ONLY : NOMBLAY
      IMPLICIT NONE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZFCL_W,ZR,ZF,MASBAS,ACLADM
      INTEGER,          INTENT(IN)    :: NSICLA, NPOIN
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: NLAYER,ESTRAT,ELAY
      DOUBLE PRECISION, INTENT(INOUT) :: ELAY0
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLTOT(NSICLA),ESTRATNEW(NPOIN)
      INTEGER         , INTENT(INOUT) :: NLAYNEW(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE,
     & CAS_FILE,DICO_FILE)
          USE DECLARATIONS_SPECIAL
          USE DECLARATIONS_SPECIAL
          IMPLICIT NONE
          INTEGER, INTENT(IN)               :: NCAR
          CHARACTER(LEN=24), INTENT(IN)     :: CODE
          CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
          CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: MOTCAR(MAXKEYWORD)
          CHARACTER(LEN=PATH_LEN), INTENT(INOUT) ::
     & FILE_DESC(4,MAXKEYWORD)
!         API
          CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
          CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE MASKAB(HN , Q , QU , QV , NPOIN)
        IMPLICIT NONE
        INTEGER, INTENT(IN):: NPOIN
        DOUBLE PRECISION , INTENT(IN)::HN(NPOIN)
        DOUBLE PRECISION, INTENT(INOUT):: Q(NPOIN),QU(NPOIN),QV(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE MAXSLOPE
     &(SLOPE,ZF,ZR,XEL,YEL,NELEM,NELMAX,NPOIN,IKLE,EVOL,UNSV2D,MESH,
     & ZFCL_MS,AVAIL,NOMBLAY,NSICLA)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN,NOMBLAY,NSICLA
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN   ) :: SLOPE
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: EVOL,ZFCL_MS
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D
      TYPE(BIEF_MESH) :: MESH
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE MEAN_GRAIN_SIZE
          USE BIEF
          IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE NOMVAR_SISYPHE
     &(TEXTE,TEXTPR,MNEMO,NSICLA,UNITE,MAXVAR,NPRIV,NOMBLAY,
     & N_NAMES_PRIV,NAMES_PRIVE,NADVAR,NAMES_ADVAR)
      IMPLICIT NONE
      INTEGER, INTENT(IN)              :: NSICLA,MAXVAR,NPRIV,NOMBLAY
      INTEGER, INTENT(IN)              :: N_NAMES_PRIV
      INTEGER, INTENT(IN)              :: NADVAR
      CHARACTER(LEN=8), INTENT(INOUT)  :: MNEMO(MAXVAR)
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_PRIVE(N_NAMES_PRIV)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_ADVAR(*)
      LOGICAL, INTENT(IN)              :: UNITE
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE NOEROD
     &  (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
        IMPLICIT NONE
        INTEGER, INTENT(IN):: NPOIN , CHOIX
        INTEGER, INTENT(INOUT):: NLISS
        DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
        DOUBLE PRECISION, INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
        DOUBLE PRECISION, INTENT(INOUT)::  ZR(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE PREDES(LLT,AAT,YAGOUT,CODE)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: LLT
          DOUBLE PRECISION, INTENT(IN) :: AAT
          CHARACTER(LEN=24), INTENT(IN):: CODE
          LOGICAL,INTENT(IN)           :: YAGOUT
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE QSFORM
     &     (U2D, V2D, TOB, HN, XMVE, TETAP, MU, NPOIN, DM,
     &       DENS, GRAV, DSTAR, AC, QSC, QSS)
        USE BIEF_DEF
          IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,TOB,HN,TETAP,MU
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, DM, DENS, GRAV, DSTAR, AC
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE READ_FIC_CONC(CGL, WHAT, AT, NFIC, FOUND )
      IMPLICIT NONE
      CHARACTER(LEN=9)     , INTENT(IN)       :: WHAT
      DOUBLE PRECISION, INTENT(IN)       :: AT
      DOUBLE PRECISION, INTENT(INOUT)    :: CGL
      INTEGER         , INTENT(IN)       :: NFIC
      LOGICAL         , INTENT(OUT)      :: FOUND
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
      SUBROUTINE RESCUE_SISYPHE
     &(H,S,ZF,ZR,ES,HW,TW,THETAW,NPOIN,NOMBLAY,NSICLA,
     & TROUVE,ALIRE,PASS,ICF,LISTI,MAXVAR)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: MAXVAR,NOMBLAY,NSICLA
      INTEGER, INTENT(IN) :: ALIRE(MAXVAR),NPOIN,ICF
      LOGICAL, INTENT(IN) :: PASS,LISTI
      DOUBLE PRECISION, INTENT(IN) :: ES(NPOIN,NOMBLAY)
      INTEGER, INTENT(INOUT) :: TROUVE(MAXVAR)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN) , ZF(NPOIN), H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZR(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HW(NPOIN), TW(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: THETAW(NPOIN)
      END SUBROUTINE
      END INTERFACE
!
      INTERFACE
      SUBROUTINE RESCUE_SISYPHE_NOTPERMA
     &(QU,QV,Q,U,V,H,S,ZF,HW,TW,THETAW,NPOIN,TROUVE,ALIRE,ICF,
     & ENTET,MAXVAR)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: MAXVAR
      INTEGER, INTENT(IN) :: TROUVE(MAXVAR),ALIRE(MAXVAR),NPOIN,ICF
      LOGICAL, INTENT(IN) :: ENTET
!
      DOUBLE PRECISION, INTENT(INOUT) :: QU(NPOIN), QV(NPOIN), Q(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN) , ZF(NPOIN), H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HW(NPOIN), TW(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: THETAW(NPOIN)
!
      END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE RIDE
     & (KS,TW,UW,UNORM,GRAV,XMVE,XMVS,VCE,NPOIN,KSPRATIO,ACLADM)
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::NPOIN
        DOUBLE PRECISION, INTENT(INOUT) :: KS(NPOIN)
        DOUBLE PRECISION, INTENT(IN) :: GRAV,XMVE,XMVS, VCE
        DOUBLE PRECISION, INTENT(IN) :: UNORM(NPOIN)
        DOUBLE PRECISION, INTENT(IN) :: UW(NPOIN), TW(NPOIN)
        DOUBLE PRECISION, INTENT(IN) :: KSPRATIO
        DOUBLE PRECISION, INTENT(IN) :: ACLADM(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE RIDE_VR
     & (KSR,KS,UNORM,HN,GRAV,XMVE,XMVS,NPOIN,ACLADM)
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NPOIN
      DOUBLE PRECISION, INTENT(IN)   :: GRAV,XMVE,XMVS
      DOUBLE PRECISION, INTENT(INOUT):: KSR(NPOIN),KS(NPOIN)
      DOUBLE PRECISION, INTENT(IN)   :: HN(NPOIN), UNORM(NPOIN)
      DOUBLE PRECISION, INTENT(IN)   :: ACLADM(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!
      INTERFACE
        SUBROUTINE SISYPHE(PART,LOOPCOUNT,GRAFCOUNT,LISTCOUNT,TELNIT,
     &                     U_TEL,V_TEL,H_TEL,HN_TEL,
     &                     ZF_SIS,UETCAR,CF_TEL,KS_TEL,
     &                     CONSTFLOW,NSIS_CFD,SISYPHE_CFD,CODE,PERICOU,
     &                     U3D,V3D,T_TEL,VISC_TEL,
     &                     DT_TEL,CHARR_TEL,SUSP_TEL,FLBOR_TEL,
     &                     SOLSYS,DM1,UCONV_TEL,VCONV_TEL,ZCONV,
     &                     THETAW_TEL,HW_TEL,TW_TEL,UW_TEL,YAGOUT,
     &                     API_ITER,GRCOMP)
          USE BIEF_DEF
          IMPLICIT NONE
          INTEGER,          INTENT(IN)   :: PART,LOOPCOUNT,GRAFCOUNT
          INTEGER,          INTENT(IN)   :: LISTCOUNT,TELNIT,PERICOU
          CHARACTER(LEN=24),INTENT(IN)   :: CODE
          TYPE(BIEF_OBJ),   INTENT(IN)   :: U_TEL,V_TEL,H_TEL,HN_TEL
          TYPE(BIEF_OBJ),   INTENT(INOUT):: ZF_SIS, UETCAR
          INTEGER,          INTENT(INOUT):: NSIS_CFD
          LOGICAL,          INTENT(INOUT):: CONSTFLOW,SISYPHE_CFD
          TYPE(BIEF_OBJ),   INTENT(IN)   :: U3D,V3D,VISC_TEL
          TYPE(BIEF_OBJ),   INTENT(INOUT):: CF_TEL,KS_TEL
          DOUBLE PRECISION, INTENT(IN)   :: T_TEL
          LOGICAL, INTENT(INOUT)         :: CHARR_TEL,SUSP_TEL
          DOUBLE PRECISION,  INTENT(IN)  :: DT_TEL
          INTEGER,           INTENT(IN)  :: SOLSYS
          TYPE(BIEF_OBJ), INTENT(IN)     :: FLBOR_TEL,DM1,ZCONV
          TYPE(BIEF_OBJ), INTENT(IN)     :: UCONV_TEL,VCONV_TEL
          TYPE(BIEF_OBJ), INTENT(IN)     :: THETAW_TEL,HW_TEL,TW_TEL
          TYPE(BIEF_OBJ), INTENT(IN)     :: UW_TEL
          LOGICAL,        INTENT(IN)     :: YAGOUT
          INTEGER, OPTIONAL, INTENT(IN)  :: API_ITER,GRCOMP
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE TASSEMENT
     &(NPOIN,DTS,ELAY,DZF_TASS,T2,AVAIL,NSICLA,ES,XMVS,
     & XKV,TRANS_MASS,CONC_VASE,NOMBLAY,MS_SABLE,MS_VASE)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN,NSICLA,NOMBLAY
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DZF_TASS,ELAY,T2
      DOUBLE PRECISION,  INTENT(INOUT):: MS_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT):: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)    :: TRANS_MASS(NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XKV
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE TASSEMENT_2
     &(NPOIN,DTS,ELAY,DZF_TASS,T2,LT,XMVS,XMVE,NOMBLAY,
     & ES,CONC_VASE,MS_VASE,XWC,COEF_N,CONC_GEL,CONC_MAX)
      USE BIEF_DEF
      USE BIEF
      IMPLICIT NONE
      INTEGER,INTENT(IN)              :: NPOIN
      INTEGER, INTENT(IN)             :: LT,NOMBLAY
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      DOUBLE PRECISION, INTENT(IN)    :: XMVS,XMVE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DZF_TASS,ELAY,T2
      DOUBLE PRECISION, INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)    :: XWC
      DOUBLE PRECISION, INTENT(IN)    :: COEF_N,CONC_GEL,CONC_MAX
        END SUBROUTINE
      END INTERFACE
!
!
      INTERFACE
        SUBROUTINE TOBW_SISYPHE(TOBW ,CF, FW, UW,TW,HN,NPOIN,XMVE)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: NPOIN
          DOUBLE PRECISION, INTENT(IN) :: CF(NPOIN)
          DOUBLE PRECISION, INTENT(IN) :: UW(NPOIN),TW(NPOIN),HN(NPOIN)
          DOUBLE PRECISION, INTENT(IN)    :: XMVE
          DOUBLE PRECISION, INTENT(INOUT) :: TOBW(NPOIN),FW(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE TOB_SISYPHE
     & (TOB, TOBW, MU, KS,KSP, KSR,CF,FW,CHESTR,UETCAR,
     &  CF_TEL,KS_TEL,CODE,
     &  KFROT,ICR, KSPRATIO, HOULE,GRAV,XMVE,XMVS, VCE, KARMAN,
     &  ZERO,HMIN,HN, ACLADM, UNORM,UW, TW, NPOIN,KSPRED,IKS)
      USE BIEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN,KFROT,ICR,IKS
      LOGICAL, INTENT(IN) :: HOULE,KSPRED
      CHARACTER(LEN=24),  INTENT(IN) :: CODE
      DOUBLE PRECISION,   INTENT(IN) :: XMVE,XMVS, VCE,GRAV,KARMAN
      DOUBLE PRECISION,   INTENT(IN) :: ZERO,HMIN,KSPRATIO
      TYPE(BIEF_OBJ), INTENT(IN)   :: UETCAR
      TYPE(BIEF_OBJ), INTENT(IN)   :: HN,UNORM
      TYPE(BIEF_OBJ), INTENT(IN)   :: TW,UW
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: KS,KSP,KSR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: CHESTR,MU
      TYPE(BIEF_OBJ), INTENT(IN)   :: ACLADM
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CF,TOB
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FW,TOBW
      TYPE(BIEF_OBJ), INTENT(IN)    :: CF_TEL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: KS_TEL
      END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE VITCHU_SISYPHE( WS , DENS , DM , GRAV , VCE )
          IMPLICIT NONE
          DOUBLE PRECISION, INTENT(IN)    :: DENS,  DM,  GRAV, VCE
          DOUBLE PRECISION, INTENT(INOUT) :: WS
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
       SUBROUTINE USER_CONDIM_SISYPHE
     & (U      , V       , QU    , QV   , H    , ZF , Z ,
     &  ESOMT  , THETAWR ,  Q    , HWR  , TWR  ,
     &  X      , Y       , NPOIN , AT   , PMAREE)
      IMPLICIT NONE
      INTEGER, INTENT(IN)::NPOIN
      DOUBLE PRECISION, INTENT(IN):: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN):: AT , PMAREE
      DOUBLE PRECISION, INTENT(INOUT) ::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::  ESOMT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: Z(NPOIN) , H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::QU(NPOIN), QV(NPOIN), Q(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT):: HWR(NPOIN) , TWR(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT):: THETAWR(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE USER_CONLIT
     &(NBOR,AT)
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
      INTEGER, INTENT(IN)          :: NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: AT
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE USER_INIT_COMPO_COH
     &(ES,CONC_VASE,CONC,NPOIN,NOMBLAY,NSICLA,AVAIL,AVA0,
     & EPAI_VASE,EPAI_SABLE)
      USE DECLARATIONS_SISYPHE, ONLY : NLAYMAX
      IMPLICIT NONE
      INTEGER, INTENT(IN)              :: NPOIN,NOMBLAY,NSICLA
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT)  :: EPAI_VASE(NLAYMAX)
      DOUBLE PRECISION, INTENT(INOUT)  :: EPAI_SABLE(NLAYMAX)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
                     SUBROUTINE USER_INIT_COMPO
     &(NCOUCHES)
      IMPLICIT NONE
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE USER_NOEROD
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
      IMPLICIT NONE
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
        SUBROUTINE USER_PREDES
     &(LLT,AAT,YAGOUT,CODE,LEO,IMP)
      IMPLICIT NONE
      INTEGER          , INTENT(IN) :: LLT
      DOUBLE PRECISION , INTENT(IN) :: AAT
      CHARACTER(LEN=24), INTENT(IN) :: CODE
      LOGICAL          , INTENT(IN) :: YAGOUT
      LOGICAL          , INTENT(IN) :: LEO
      LOGICAL          , INTENT(IN) :: IMP
        END SUBROUTINE
      END INTERFACE
!
      INTERFACE
                     SUBROUTINE USER_QSFORM
     &(U2D, V2D, TOB, HN, XMVE, TETAP, MU, NPOIN, DM,
     & DENS, GRAV, DSTAR, AC, QSC, QSS)
      USE BIEF_DEF
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,TOB,HN,TETAP,MU
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, DM, DENS, GRAV, DSTAR, AC
        END SUBROUTINE
      END INTERFACE
!
!======================================================================!
!======================================================================!
!
      END MODULE INTERFACE_SISYPHE
