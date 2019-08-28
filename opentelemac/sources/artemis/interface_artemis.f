!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                  INTERFACES FOR ARTEMIS SUBROUTINES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
!#######################################################################
!
      MODULE INTERFACE_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
!-----------------------------------------------------------------------
!
!     DEFINES INTERFACES
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE BERKHO(LT)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: LT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCFW
     &(I,H,K,HMU,NPOIN,OMEGA,GRAV,VISCO,
     & DIAM90,DIAM50,MVSED,MVEAU,
     & REGIDO,RICOEF,ENTREG,ENTRUG,FFW)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER I, NPOIN, REGIDO
      DOUBLE PRECISION K(NPOIN)
      DOUBLE PRECISION HMU(NPOIN),H(NPOIN),GRAV,OMEGA
      DOUBLE PRECISION VISCO, DIAM90, DIAM50,MVSED
      DOUBLE PRECISION MVEAU, RICOEF,FFW
      LOGICAL ENTREG,ENTRUG
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCQB
     &(Q1,Q2,Q3)
      IMPLICIT NONE
      DOUBLE PRECISION Q1,Q2,Q3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
!      INTERFACE
!        SUBROUTINE CALDIR()
!       USE BIEF_DEF
!       IMPLICIT NONE
!        END SUBROUTINE
!      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALTETAP(TETA,XSGBOR,YSGBOR,ADIR,NPTFR)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER                      :: NPTFR
      DOUBLE PRECISION, INTENT(IN) :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)
      DOUBLE PRECISION TETA(NPTFR),ADIR(NPTFR)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CNTPRE
     &(DAM,NPOIN,IPRECO,IPREC2)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(INOUT) :: IPRECO, IPREC2
      DOUBLE PRECISION, INTENT(IN) :: DAM(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DIRALE
     &(DALE,EXPOS,TETAH,TETMIN,TETMAX,
     & NDALE)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER NDALE
      DOUBLE PRECISION DALE(NDALE)
      DOUBLE PRECISION EXPOS,TETAH,TETMIN,TETMAX
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DISMOY
     &(NPOIN,NELEM,X,Y,IKLE,K,LISHHO)
      IMPLICIT NONE
      INTEGER NPOIN,NELEM,LISHHO
      INTEGER IKLE(NELEM,*)
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),K(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ENTART
     &(ITITRE,X,NBR,NBRTOT,ALEMON,ALEMUL,BALAYE)
      IMPLICIT NONE
      INTEGER ITITRE,NBR,NBRTOT
      DOUBLE PRECISION X
      LOGICAL ALEMON,ALEMUL,BALAYE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION FCTE1(KH)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: KH
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION FCTE2(KH)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: KH
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FWSPEC
     &(FW,FWCOEF,X,Y,NPOIN,PRIVE,ZF)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER NPOIN
      DOUBLE PRECISION FW(NPOIN),X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION ZF(NPOIN),FWCOEF
      TYPE(BIEF_OBJ) :: PRIVE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_FWSPEC
     &(FW,FWCOEF,X,Y,NPOIN,PRIVE,ZF)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER NPOIN
      DOUBLE PRECISION FW(NPOIN),X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION ZF(NPOIN),FWCOEF
      TYPE(BIEF_OBJ) :: PRIVE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDON_ARTEMIS
     &(FILE_DESC,PATH,NCAR,
     & CAS_FILE,DICO_FILE)
        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!
!
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
!
        END SUBROUTINE
      END INTERFACE

!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NOMVAR_ARTEMIS
     &(TEXTE,TEXTPR,MNEMO)
      IMPLICIT NONE
      CHARACTER(LEN=32) TEXTE(26),TEXTPR(26)
      CHARACTER(LEN=8)  MNEMO(26)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PENTCO(II)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: II
        END SUBROUTINE
      END INTERFACE
!
!
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PERALE
     &(PALE,GAMMA,PERPIC,NPALE,PMIN,PMAX)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER NPALE
      DOUBLE PRECISION PALE(NPALE)
      DOUBLE PRECISION PERPIC,GAMMA,PMIN,PMAX
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RADIA1(LISHHO)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER LISHHO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RADIA2(LISHHO)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER LISHHO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION SPD(TETA)
      IMPLICIT NONE
      DOUBLE PRECISION TETA
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION SPE(F)
      IMPLICIT NONE
      DOUBLE PRECISION F
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION STWC1(F,DIR,SPEC,I)
        USE BIEF_DEF, ONLY: SPECTRUM
        USE DECLARATIONS_ARTEMIS
        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
        TYPE(SPECTRUM)   , INTENT(IN) :: SPEC
        DOUBLE PRECISION F,DIR
        INTEGER I
        END FUNCTION
      END INTERFACE
!
      INTERFACE
        SUBROUTINE STWC2
     &(IMIN,IMAX,N,DIR2,SDIR)
        USE BIEF_DEF, ONLY: SPECTRUM
        USE DECLARATIONS_ARTEMIS
        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
        INTEGER :: IMIN,IMAX,N
        DOUBLE PRECISION :: SDIR(N),DIR2(N)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE UTIMP_ART
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_UTIMP_ART
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ALLSPEC
     &(SPEC,NOM)
       USE DECLARATIONS_ARTEMIS
       IMPLICIT NONE
       TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
       CHARACTER(LEN=6) , INTENT(IN)           :: NOM
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_ART_CORFON
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_BORH
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_CONDIH
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_CONDIH_PARTICULAR
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!=======================================================================
!
      END MODULE INTERFACE_ARTEMIS
!
!#######################################################################
!
