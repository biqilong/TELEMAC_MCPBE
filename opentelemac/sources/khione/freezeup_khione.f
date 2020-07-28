!                     **********************
                      MODULE FREEZEUP_KHIONE
!                     **********************
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Module containing all subroutines to deal with the physics
!+        of freeze-up and frazil ice growth, at a node level.
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: THERMAL_GROWTH,MELTING_POINT,SEEDING,
     &          SECONDARY_NUCLEATION, FLOCCULATION_BREAKUP,
     &          NUSSELT,TURBULENT_PARAMETERS,BUOYANCY_VELOCITY,
     &          EROSION_DEPOSITION,CLOGGED_ON_BAR,PRECIPITATION
!
!=======================================================================
!
      CONTAINS
!
!=======================================================================
!
!                   *************************
                    SUBROUTINE THERMAL_GROWTH
!                   *************************
!
     &  ( I,TN,TFRZ,DT,SRCGM_EXP,SRCGM_IMP,SUM_SRCGM,SUM_FRZL,
     &    EPS,ALPHA,NUT,CONSTSS,LIMFLAG)
!
!***********************************************************************
! KHIONE   V8P2
!***********************************************************************
!
!brief    Source term for frazil cristals thermal growth/decay
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I          |-->| CELL INDEX
!| TN         |-->| TRACERS
!| TFRZ       |-->| FREEZING TEMPERATURE
!| DT         |-->| TIME STEP
!| SRCGM_EXP  |<->| EXPLICIT SOURCE OF FRAZIL
!| SRCGM_IMP  |<->| IMPLICIT SOURCE OF FRAZIL
!| SUM_SRCGM  |<->| SUM OF THERMAL GROWTH SOURCES (FOR TEMPERATURE SRC)
!| SUM_FRZL   |<->| SUM OF FRAZIL (TOTAL FRAIL CONC. FOR TEMPERATURE SRC)
!| EPS        |-->| TURBULENT DISSIPATION RATE
!| ALPHA      |-->| TURBULENT INTENSITY
!| NUT        |-->| TURBULENT VISCOSITY
!| CONSTSS    |-->| 1.D0/(RO0*CP_EAU)
!| LIMFLAG    |<->| LIMITOR FLAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE, ONLY : NC_FRA,IND_FRA,IND_T,
     &  RHO_ICE,LH_ICE,TC_WT,RK_FRZL,EK_FRZL,VK_FRZL,
     &  NUSST,NUSSI,NUSS,ITGM,MINNK,ISEED
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)       :: TN
      INTEGER, INTENT(IN)              :: I
      LOGICAL, INTENT(INOUT)           :: LIMFLAG
      DOUBLE PRECISION, INTENT(IN)     :: DT,TFRZ,CONSTSS
      DOUBLE PRECISION, INTENT(IN)     :: EPS,ALPHA,NUT
      DOUBLE PRECISION, INTENT(INOUT)  :: SUM_SRCGM,SUM_FRZL
      DOUBLE PRECISION, INTENT(INOUT)  :: SRCGM_EXP(NC_FRA)
      DOUBLE PRECISION, INTENT(INOUT)  :: SRCGM_IMP(NC_FRA)
      DOUBLE PRECISION, PARAMETER      :: EPS0 = 1.E-8
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                          :: K,J
      DOUBLE PRECISION                 :: TW,QK
      DOUBLE PRECISION                 :: CF(NC_FRA)
      DOUBLE PRECISION                 :: SRCGM(NC_FRA)
      DOUBLE PRECISION                 :: RM1,RP1,FACT
      DOUBLE PRECISION                 :: MAXDT0,SRGMSTB
!
!-----------------------------------------------------------------------
!
!     TEMPERATURE AND FRAZIL CONC.
      TW = TN%ADR(IND_T)%P%R(I)
      DO K=1,NC_FRA
        J = IND_FRA+K-1
        CF(K) = TN%ADR(J)%P%R(I)
      ENDDO
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     COMPUTING THERMAL GROWTH/DECAY
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ~~> LOOP ON ALL CLASS OF FRAZIL CRISTALS
      DO K=1,NC_FRA
        J = IND_FRA+K-1
!
! ~~>   COMPUTE NUSSLET NUMBER OF FRAZIL CLASS K
        IF (NUSSI.EQ.1) THEN
          NUSST(K) = NUSS
        ELSE IF (NUSSI.EQ.2) THEN
          NUSST(K) = NUSSELT(RK_FRZL(K), EPS, ALPHA, NUT)
        ENDIF
!
! ~~>   GROWTH
        IF(TW.LT.TFRZ)THEN
          QK = NUSST(K)*TC_WT/RK_FRZL(K)
          SRCGM(K) = 2.D0*QK/(RK_FRZL(K)*RHO_ICE*LH_ICE)
! ~~>   MELTING
        ELSE IF(TW.GT.TFRZ)THEN
          QK = NUSST(K)*TC_WT/RK_FRZL(K)
          FACT = (1./EK_FRZL(K)) + (1./RK_FRZL(K))
          SRCGM(K) = 2.D0*QK*FACT/(RHO_ICE*LH_ICE)
          IF ((ISEED.EQ.1).AND.
     &        (TN%ADR(J)%P%R(I).LE.MINNK*VK_FRZL(K)/NC_FRA)) THEN
            SRCGM(K) = 0.D0
          ENDIF
! ~~>   EQUILIBIRUM
        ELSE
          SRCGM(K) = 0.D0
        ENDIF
!
        IF(ITGM.EQ.1) THEN
          SRCGM(K) = SRCGM(K)*(TFRZ-TW)*CF(K)
        ENDIF
!
      ENDDO
!
      SRGMSTB = SRCGM(1)
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     COMPUTING MASS EXCHANGES BETWEEN CALSSES
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      LIMFLAG = .FALSE.
      SUM_SRCGM = 0.D0
      SUM_FRZL = 0.D0
!
! ~~> LOOP ON ALL CLASS OF FRAZIL CRISTALS
      DO K=1,NC_FRA
        J = IND_FRA+K-1
!
!       ~~~~~~~~~~~~~~~~~~~~~~~
!       EXPLICIT IMPLEMENTATION
!       ~~~~~~~~~~~~~~~~~~~~~~~
        IF(ITGM.EQ.1) THEN
!
! ~~>     COMPUTING SOURCE TERM OF FRAZIL CONC. (MONO-CLASS)
          IF(NC_FRA.EQ.1) THEN
            SRCGM_EXP(K) = MAX(-TN%ADR(J)%P%R(I)/DT, SRCGM(K))
!
! ~~>     COMPUTING SOURCE TERM OF FRAZIL CONC. OF CLASS K
          ELSE
            RM1 = (RK_FRZL(MAX(K-1, 1))/RK_FRZL(K))**3
            RP1 = (RK_FRZL(MIN(K+1,NC_FRA))/RK_FRZL(K))**3
!
!           GROWTH
            IF(TW.LT.TFRZ)THEN
              IF(K.EQ.1) THEN
                SRCGM_EXP(K) =-SRCGM(K)/(RP1-1.D0)
              ELSEIF(K.EQ.NC_FRA) THEN
                SRCGM_EXP(K) = SRCGM(K-1)/(1.D0-RM1)
              ELSE
                SRCGM_EXP(K) = SRCGM(K-1)/(1.D0-RM1)-SRCGM(K)/(RP1-1.D0)
              ENDIF
!
!           MELTING
            ELSEIF(TW.GT.TFRZ)THEN
              IF(K.EQ.1) THEN
                SRCGM_EXP(K) = SRCGM(K)-SRCGM(K+1)/(RP1-1.D0)
              ELSEIF(K.EQ.NC_FRA) THEN
                SRCGM_EXP(K) = SRCGM(K)/(1.D0-RM1)
              ELSE
                SRCGM_EXP(K) = SRCGM(K)/(1.D0-RM1)-SRCGM(K+1)/(RP1-1.D0)
              ENDIF
!
!           EQUILIBRIUM
            ELSE
              SRCGM_EXP(K) = 0.D0
            ENDIF
!
!           LIMITOR
            IF (SRCGM_EXP(K).LE.-TN%ADR(J)%P%R(I)/DT)THEN
              LIMFLAG = .TRUE.
              SRCGM_EXP(K) = -TN%ADR(J)%P%R(I)/DT
            ENDIF
          ENDIF
!
!         RATE OF TOTAL VOLUME FRACTION CHANGE
          SUM_SRCGM = SUM_SRCGM + SRCGM_EXP(K)
!
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       SEMI-IMPLICIT IMPLEMENTATION
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ELSEIF(ITGM.EQ.2) THEN
!
! ~~>     COMPUTING SOURCE TERM OF FRAZIL CONC. (MONO-CLASS)
          IF(NC_FRA.EQ.1) THEN
            SRCGM_EXP(K) = 0.D0
            SRCGM_IMP(K) = SRCGM(K)*(TFRZ-TW)
            SRCGM(K) = SRCGM(K)*CF(K)
!
! ~~>     COMPUTING SOURCE TERM OF FRAZIL CONC. OF CLASS K
          ELSE
            RM1 = (RK_FRZL(MAX(K-1, 1))/RK_FRZL(K))**3
            RP1 = (RK_FRZL(MIN(K+1,NC_FRA))/RK_FRZL(K))**3
!
!           GROWTH
            IF(TW.LT.TFRZ)THEN
              IF(K.EQ.1) THEN
                SRCGM_EXP(K) = 0.D0
                SRCGM_IMP(K) =-SRCGM(K)*(TFRZ-TW)/(RP1-1.D0)
              ELSEIF(K.EQ.NC_FRA) THEN
                SRCGM_EXP(K) = SRCGM(K-1)*(TFRZ-TW)*CF(K-1)/(1.D0-RM1)
                SRCGM_IMP(K) = 0.D0
              ELSE
                SRCGM_EXP(K) = SRCGM(K-1)*(TFRZ-TW)*CF(K-1)/(1.D0-RM1)
                SRCGM_IMP(K) =-SRCGM(K)*(TFRZ-TW)/(RP1-1.D0)
              ENDIF
!
!           MELTING
            ELSEIF(TW.GT.TFRZ)THEN
              IF(K.EQ.1) THEN
                SRCGM_EXP(K) =-SRCGM(K+1)*(TFRZ-TW)*CF(K+1)/(RP1-1.D0)
                SRCGM_IMP(K) = SRCGM(K)*(TFRZ-TW)
              ELSEIF(K.EQ.NC_FRA) THEN
                SRCGM_EXP(K) = 0.D0
                SRCGM_IMP(K) = SRCGM(K)*(TFRZ-TW)/(1.D0-RM1)
              ELSE
                SRCGM_EXP(K) =-SRCGM(K+1)*(TFRZ-TW)*CF(K+1)/(RP1-1.D0)
                SRCGM_IMP(K) = SRCGM(K)*(TFRZ-TW)/(1.D0-RM1)
              ENDIF
!
!           EQUILIBRIUM
            ELSE
              SRCGM_EXP(K) = 0.D0
              SRCGM_IMP(K) = 0.D0
            ENDIF
!
!           LIMITOR
            IF (SRCGM_EXP(K).LE.-TN%ADR(J)%P%R(I)/DT)THEN
              LIMFLAG = .TRUE.
              SRCGM_EXP(K) = -TN%ADR(J)%P%R(I)/DT
            ENDIF
          ENDIF
!
!         RATE OF TOTAL VOLUME FRACTION CHANGE
          IF (TFRZ.NE.TW) THEN
            SUM_SRCGM = SUM_SRCGM + SRCGM_EXP(K)/(TFRZ-TW)
     &                            + SRCGM_IMP(K)*CF(K)/(TFRZ-TW)
          ENDIF
!
        ENDIF
!
!       TOTAL FRAZIL CONC.
        SUM_FRZL = SUM_FRZL + CF(K)
!
      ENDDO
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     STABILITY/POSITIVITY WARNING
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF(SUM_SRCGM.GT.EPS0)THEN
        IF(ITGM.EQ.1) THEN
          MAXDT0 = MIN(2.D0/(RHO_ICE*LH_ICE*CONSTSS*SRGMSTB
     &            *MAX(EPS0, SUM_FRZL)),
     &             1.D0/(SRGMSTB*MAX(EPS0, ABS(TW))))
        ELSEIF(ITGM.EQ.2) THEN
          MAXDT0 = 1.D0/(SRGMSTB*MAX(EPS0, ABS(TW)))
        ENDIF
!
        IF( DT.GT.MAXDT0) THEN
          WRITE(LU,*) ''
          WRITE(LU,*) 'STABILITY OF THERMAL GROWTH '
          WRITE(LU,*) 'OR POSITIVITY OF FRAZIL CONC. '
          WRITE(LU,*) 'MAY BE COMPROMISED '
          WRITE(LU,*) ''
          WRITE(LU,*) 'MAKE SURE TIME STEP '
          WRITE(LU,*) 'IS LOWER THAN ', MAXDT0
          WRITE(LU,*) 'OR CHECK MODEL PARAMETERS '
          WRITE(LU,*) ''
        ENDIF
      ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE THERMAL_GROWTH
!
!=======================================================================
!
!                     ******************
                      SUBROUTINE SEEDING
!                     ******************
!
     &  ( I, TN, TFRZ, SRCSE, MINFLAG )
!
!***********************************************************************
! KHIONE   V8P2
!***********************************************************************
!
!brief    Computes frazil ice seeding
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I          |-->| CELL INDEX
!| TN         |<->| TRACERS
!| TFRZ       |-->| FREEZING TEMPERATURE
!| SRCSE      |<->| EXPLICIT SOURCE OF FRAZIL
!| MINFLAG    |<->| LOGICAL FLAG TO DISABLE SRCSN WHEN MIN CONC. REACHED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE, ONLY : NC_FRA,IND_FRA,IND_T,VK_FRZL,
     &  SEED,ISEED,SEEDR,MINNK
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)    :: TN
      LOGICAL, INTENT(INOUT)           :: MINFLAG
      DOUBLE PRECISION, INTENT(INOUT)  :: SRCSE(NC_FRA)
      DOUBLE PRECISION, INTENT(IN)     :: TFRZ
      INTEGER, INTENT(IN)              :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                          :: K,J,FROZNB
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     ~~~~~~~~~~
!     NO SEEDING
!     ~~~~~~~~~~
      IF(ISEED.EQ.0) THEN
        DO K=1,NC_FRA
          SRCSE(K) = 0.D0
        ENDDO

!     ~~~~~~~~~~~~~~~~~~~~~~~
!     MINIMUM CONC. THRESHOLD
!     ~~~~~~~~~~~~~~~~~~~~~~~
      ELSE IF((ISEED.EQ.1).OR.(ISEED.EQ.3)) THEN
        FROZNB = 0
        DO K=1,NC_FRA
          J = IND_FRA+K-1
          SRCSE(K) = 0.D0
!
!         RMK: TRACER VALUES NEED TO BE MODIFIED DIRECTLY OTHERWISE
!         SEMI-IMPLICIT THERMAL GROWTH WOULD NOT WORK AS INTENDED
          TN%ADR(J)%P%R(I) = MAX(MINNK*VK_FRZL(K)/NC_FRA,
     &                           TN%ADR(J)%P%R(I))
!
!         IN CASE OF MELTING SECONDARY NUCLEATION AND FLOCCULATION
!         ARE NOT COMPUTED IF ALL FRAZIL CLASS CONC. ARE LE MIN CONC.
!         (ONLY WHEN MIN CONC THRESHOLD IS SELECTED AS SEEDING METHOD)
          IF ((TN%ADR(IND_T)%P%R(I).GT.TFRZ).AND.
     &        (TN%ADR(J)%P%R(I).LE.MINNK*VK_FRZL(K)/NC_FRA)) THEN
            FROZNB = FROZNB + 1
          ENDIF
        ENDDO
        IF (FROZNB.EQ.NC_FRA) THEN
          MINFLAG = .TRUE.
        ELSE
          MINFLAG = .FALSE.
        ENDIF
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     CONSTANT SEEDING RATE PER CLASS
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ELSE IF((ISEED.EQ.2).OR.(ISEED.EQ.3)) THEN
        DO K=1,NC_FRA
          SRCSE(K) = VK_FRZL(K)*SEEDR
        ENDDO
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~
!     SEEDING RATE COMPUTED
!     FROM ATMOSPHERIC DRIVERS
!     ~~~~~~~~~~~~~~~~~~~~~~~~
!     TODO SRCSE(K) = SEED%R(I)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SEEDING
!
!=======================================================================
!
!             *******************************
              SUBROUTINE SECONDARY_NUCLEATION
!             *******************************
!
     &  ( I, TN, DT, SRCSN, EPS, LIMFLAG, MINFLAG )
!
!***********************************************************************
! KHIONE   V8P2
!***********************************************************************
!
!brief    Computes secondary nucleation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I          |-->| CELL INDEX
!| TN         |-->| TRACERS
!| DT         |-->| TIME STEP
!| SRCSN      |<->| EXPLICIT SOURCE OF FRAZIL
!| EPS        |-->| TURBULENT DISSIPATION RATE
!| LIMFLAG    |-->| LOGICAL FLAG TO DISABLE SRCSN WHEN MAX SRCGM REACHED
!| MINFLAG    |-->| LOGICAL FLAG TO DISABLE SRCSN WHEN MIN CONC. REACHED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE, ONLY : NC_FRA, IND_FRA, ISNUC, XNU,
     &  RK_FRZL, VK_FRZL, VBK, SNNMAX
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)       :: TN
      INTEGER, INTENT(IN)              :: I
      LOGICAL, INTENT(IN)              :: LIMFLAG, MINFLAG
      DOUBLE PRECISION, INTENT(INOUT)  :: SRCSN(NC_FRA)
      DOUBLE PRECISION, INTENT(IN)     :: DT,EPS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                          :: K,J
      DOUBLE PRECISION                 :: SUMSN,WTSQ,CF,WRK,NTILDE
      DOUBLE PRECISION, PARAMETER      :: PI = 4.D0*ATAN(1.D0)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     ~~~~~~~~~~~~~~~~~~~~~~~
!     NO SECONDARY NUCLEATION
!     ~~~~~~~~~~~~~~~~~~~~~~~
      IF((ISNUC.EQ.0).OR.(NC_FRA.EQ.1).OR.LIMFLAG.OR.MINFLAG) THEN
        DO K=1,NC_FRA
          SRCSN(K) = 0.D0
        ENDDO
!
!     ~~~~~~~~~~~~~~~~~~~~~~~
!     SVENSSON & OMSTEDT 1994
!     ~~~~~~~~~~~~~~~~~~~~~~~
      ELSE IF(ISNUC.EQ.1) THEN
!
!       COMPUTATION OF AVERAGE NUMBER OF ICE CRYSTALS
        NTILDE = 0.D0
        DO K=1,NC_FRA
          J = IND_FRA+K-1
          CF = TN%ADR(J)%P%R(I)
          NTILDE = NTILDE + CF/VK_FRZL(K)
        ENDDO
        NTILDE = MIN(FLOAT(SNNMAX), NTILDE)
!
        IF(NTILDE.GT.1.D0) THEN
!         LOOP ON ALL CLASSES EXCEPT 1
          SUMSN = 0.D0
          DO K=2,NC_FRA
            J = IND_FRA+K-1
            CF = TN%ADR(J)%P%R(I)
!           TURBULENT COMPONENT OF RELATIVE VELOCITY
            WTSQ = (4.D0/15.D0)*(EPS/XNU)*RK_FRZL(K)**2
!           RELATIVE VELOCITY
            WRK = SQRT(WTSQ + VBK(K)**2)
!           NUCLEATION SINK TERM
            SRCSN(K) = -PI*NTILDE*WRK*CF*RK_FRZL(K)**2
!           LIMITOR
            SRCSN(K) = MAX(-CF/DT, SRCSN(K))
!           SUM FOR FIRST CLASS SOURCE
            SUMSN = SUMSN + SRCSN(K)
          ENDDO
!         NUCLEATION SOURCE TERM FOR FIRST CLASS
          SRCSN(1) = -SUMSN
        ELSE
          DO K=1,NC_FRA
            SRCSN(K) = 0.D0
          ENDDO
        ENDIF
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~
!     WANG & DOERING 2005
!     WARNING: NOT CONSERVATIVE
!     ~~~~~~~~~~~~~~~~~~~~~~~~~
      ELSE IF(ISNUC.EQ.2) THEN
!
!       COMPUTATION OF AVERAGE NUMBER OF ICE CRYSTALS
        NTILDE = 0.D0
        DO K=1,NC_FRA
          J = IND_FRA+K-1
          CF = TN%ADR(J)%P%R(I)
          NTILDE = NTILDE + CF/VK_FRZL(K)
        ENDDO
        NTILDE = MIN(FLOAT(SNNMAX), NTILDE)
!
        IF(NTILDE.GT.1.D0) THEN
!         LOOP ON ALL CLASSES EXCEPT 1
          SUMSN = 0.D0
          DO K=2,NC_FRA
            J = IND_FRA+K-1
            CF = TN%ADR(J)%P%R(I)
!           TURBULENT COMPONENT OF RELATIVE VELOCITY
            WTSQ = (4.D0/15.D0)*(EPS/XNU)*RK_FRZL(K)**2
!           RELATIVE VELOCITY
            WRK = SQRT(WTSQ + VBK(K)**2)
!           NUCLEATION SINK TERM
            SRCSN(K) = -PI*NTILDE*WRK*CF*RK_FRZL(K)**2
!           LIMITOR
            SRCSN(K) = MAX(-CF/DT, SRCSN(K))
!           SUM FOR FIRST CLASS SOURCE
            SUMSN = SUMSN + SRCSN(K)
            SRCSN(K) = SRCSN(K)*(RK_FRZL(1)/RK_FRZL(K))**3
          ENDDO
!         NUCLEATION SOURCE TERM FOR FIRST CLASS
          SRCSN(1) = -SUMSN
        ELSE
          DO K=1,NC_FRA
            SRCSN(K) = 0.D0
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SECONDARY_NUCLEATION
!
!=======================================================================
!
!             *******************************
              SUBROUTINE FLOCCULATION_BREAKUP
!             *******************************
!
     &  ( I, TN, DT, SRCFB, LIMFLAG, MINFLAG )
!
!***********************************************************************
! KHIONE   V8P2
!***********************************************************************
!
!brief    Computes floculation & breaking
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I          |-->| CELL INDEX
!| TN         |-->| TRACERS
!| DT         |-->| TIME STEP
!| SRCFB      |<->| EXPLICIT SOURCE OF FRAZIL
!| LIMFLAG    |-->| LOGICAL FLAG TO DISABLE SRCFB WHEN MAX SRCGM REACHED
!| MINFLAG    |-->| LOGICAL FLAG TO DISABLE SRCFB WHEN MIN CONC. REACHED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE, ONLY : NC_FRA, IND_FRA, RK_FRZL,
     &  IFLOC, AFLOC
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)       :: TN
      LOGICAL, INTENT(IN)              :: LIMFLAG, MINFLAG
      INTEGER, INTENT(IN)              :: I
      DOUBLE PRECISION, INTENT(IN)     :: DT
      DOUBLE PRECISION, INTENT(INOUT)  :: SRCFB(NC_FRA)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                          :: K,J
      DOUBLE PRECISION                 :: CF, CF0
      DOUBLE PRECISION                 :: BETA, BETA0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     ~~~~~~~~~~~~~~~~~~~~~~~
!     NO FLOCULATION & BREAUP
!     ~~~~~~~~~~~~~~~~~~~~~~~
      IF((IFLOC.EQ.0).OR.(NC_FRA.EQ.1).OR.LIMFLAG.OR.MINFLAG) THEN
        DO K=1,NC_FRA
          SRCFB(K) = 0.D0
        ENDDO
!
!     ~~~~~~~~~~~~~~~~~~~~~~~
!     SVENSSON & OMSTEDT 1994
!     ~~~~~~~~~~~~~~~~~~~~~~~
      ELSE IF(IFLOC.EQ.1) THEN
        DO K=1,NC_FRA
          J = IND_FRA+K-1
!
          IF(K.EQ.1) THEN
            CF = TN%ADR(J)%P%R(I)
            BETA = AFLOC*RK_FRZL(K)/RK_FRZL(1)
            BETA = MIN(1.D0/DT, BETA) !LIMITOR
            SRCFB(K) = -BETA*CF
!
          ELSE IF(K.EQ.NC_FRA) THEN
            BETA0 = BETA
            CF0 = TN%ADR(J-1)%P%R(I)
            SRCFB(K) = BETA0*CF0
!
          ELSE
            CF = TN%ADR(J)%P%R(I)
            CF0 = TN%ADR(J-1)%P%R(I)
            BETA0 = BETA
            BETA = AFLOC*RK_FRZL(K)/RK_FRZL(1)
            BETA = MIN(1.D0/DT, BETA) !LIMITOR
            SRCFB(K) = -BETA*CF + BETA0*CF0
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE FLOCCULATION_BREAKUP
!
!=======================================================================
!
!             *******************************
              SUBROUTINE TURBULENT_PARAMETERS
!             *******************************
!
     &  ( I, VMAG, H, KT, EPS, ALPHA, NUT, CF, AK, EP, ITURB_TEL)
!
!***********************************************************************
! KHIONE   V8P2
!***********************************************************************
!
!brief    Computes turbulent parameters for frazil ice model
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK       |-->| TURBULENT KINETIC ENERGY K AT TIME T(N+1)
!| EP       |-->| TURBULENT ENERGY DISSIPATION AT TIME T(N+1)
!| ITURB_TEL|-->| TURBULENT MODEL IN T2D
!| VMAG     |-->| VELOCITY MAGNITUDE
!| H        |-->| WATER DEPTH
!| KT       |<->| MEAN TURBULENT KINETIC ENERGY
!| EPS      |<->| MEAN TURBULENT DISSIPATION RATE
!| ALPHA    |<->| TURBULENT INTENSITY
!| NUT      |<->| TURBULENT VISCOSITY
!| CF       |<->| QUADRATIC FRICTION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE, ONLY : XNU, ITURB
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)       :: AK,EP
      INTEGER, INTENT(IN)              :: I,ITURB_TEL
      DOUBLE PRECISION, INTENT(IN)     :: VMAG,H,CF
      DOUBLE PRECISION, INTENT(INOUT)  :: KT,EPS,ALPHA,NUT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, PARAMETER      :: KARMAN_CONST = 0.4
      DOUBLE PRECISION, PARAMETER      :: HEPS = 1.D-3
      DOUBLE PRECISION, PARAMETER      :: UEPS = 1.D-6
      DOUBLE PRECISION                 :: USTAR,FACT0,FACT1,HTEMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     CONSTANTS (FOR TEST PURPOSES)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF(ITURB.EQ.0) THEN
        KT = 9.6D-4
        EPS = 12.D-4
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     DEPTH AVERAGED K, EPS
!     FROM MIXING LENGTH VERTICAL PROFILE
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ELSE IF(ITURB.EQ.1) THEN
        USTAR = 0.5D0 * CF * VMAG * VMAG
        USTAR = SQRT(USTAR)
        HTEMP = MAX(H, HEPS)
        FACT0 = (HTEMP/2.D0 - XNU/USTAR + XNU**2/(2.D0*HTEMP*USTAR**2))
        FACT1 = (LOG(HTEMP*USTAR/XNU) - 1.D0 + XNU/(HTEMP*USTAR))
        KT = (USTAR**2)*FACT0/0.3D0
        EPS = (USTAR**3)*FACT1/KARMAN_CONST
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~
!     DEPTH AVERAGED K, EPS
!     FROM TELEMAC2D K-EPS MODEL
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~
      ELSE IF(ITURB.EQ.2) THEN
        IF(ITURB_TEL.NE.3) THEN
          WRITE(LU,*) ''
          WRITE(LU,*) 'TURBULENCE ESTIMATION MODEL', ITURB
          WRITE(LU,*) 'MUST BE USED WITH K-EPS MODEL'
          WRITE(LU,*) ''
          WRITE(LU,*) 'PLEASE SET TURBULENCE MODEL = 3'
          WRITE(LU,*) 'IN T2D STEERING FILE'
          WRITE(LU,*) ''
          CALL PLANTE(1)
          STOP
        ENDIF
        KT = AK%R(I)
        EPS = EP%R(I)
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~
      ELSE
        WRITE(LU,*) ''
        WRITE(LU,*) 'TURBULENCE ESTIMATION MODEL', ITURB
        WRITE(LU,*) 'NOT IMPLEMENTED'
        WRITE(LU,*) ''
        CALL PLANTE(1)
        STOP
      ENDIF
!
      NUT = 0.09*KT**2/EPS
      ALPHA = SQRT(2*KT)/MAX(UEPS, VMAG)
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE TURBULENT_PARAMETERS
!
!=======================================================================
!
!             *********************************
              DOUBLE PRECISION FUNCTION NUSSELT
!             *********************************
!
     &  ( RADIUS, EPS, ALPHA, NUT )
!
!***********************************************************************
! KHIONE   V8P2
!***********************************************************************
!
!brief    Computes the Nusselt number for thermal growth
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| RADIUS   |-->| RADIUS OF FRAZIL PARTICLE
!| EPS      |-->| TURBULENT DISSIPATION RATE
!| ALPHA    |-->| TURBULENT INTENSITY
!| NUT      |-->| TURBULENT VISCOSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY : TC_WT, CP_EAU, RO0, XNU
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)     :: RADIUS, ALPHA, EPS, NUT
      DOUBLE PRECISION                 :: MSTAR, KOLML, PR, PR12, PR13
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      KOLML = (XNU**3/EPS)**(1.D0/4.D0)
      MSTAR = RADIUS/KOLML
      PR = RO0*CP_EAU*NUT/TC_WT
      PR12 = PR**0.5D0
      PR13 = PR**(1.D0/3.D0)
!
      IF (MSTAR .LE. 1.D0/PR12) THEN
        NUSSELT = 1.D0 + 0.17D0*MSTAR*PR12
      ELSE IF ((1.D0/PR12 .LT. MSTAR) .AND. (MSTAR .LE. 1.D0)) THEN
        NUSSELT = 1.D0 + 0.55D0*(MSTAR**(2.D0/3.D0))*PR13
      ELSE IF (MSTAR .GT. 1.D0) THEN
        IF (ALPHA*MSTAR**(4.D0/3.D0) .LE. 1.E3) THEN
          NUSSELT = 1.1D0 + 0.77D0*(ALPHA**0.035D0)
     &              *(MSTAR**(2.D0/3.D0))*PR13
        ELSE
          NUSSELT = 1.1D0 + 0.77D0*(ALPHA**0.25D0)*MSTAR*PR13
        ENDIF
      ELSE
        NUSSELT = 1.D0
      ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END FUNCTION NUSSELT
!
!=======================================================================
!
!           *******************************************
            DOUBLE PRECISION FUNCTION BUOYANCY_VELOCITY
!           *******************************************
!
     &  ( RADIUS, THICKNESS )
!
!***********************************************************************
! KHIONE   V8P2
!***********************************************************************
!
!brief    Computes the buoyancy velocity of frazil particles
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| RADIUS    |-->| RADIUS OF FRAZIL PARTICLE
!| THICKNESS |-->| THICKNESS OF FRAZIL PARTICLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY : IBUOY, XNU, DE,
     &                                RO0, RHO_ICE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)     :: RADIUS, THICKNESS
      DOUBLE PRECISION, PARAMETER      :: CKD = 0.47D0
      DOUBLE PRECISION, PARAMETER      :: GRAV = 9.81D0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(IBUOY.EQ.1) THEN
!       DALY (1984) INTERMEDIATE RANGE
!       RMK: VALID FOR 0.03 < R < 0.14 CM
        BUOYANCY_VELOCITY = 1.D-2*30.D0*(RADIUS*1.D2)**1.2D0

      ELSEIF(IBUOY.EQ.2) THEN
!       MATOUSEK (1992)
        BUOYANCY_VELOCITY = (1.31D-5)*((2.D0*RADIUS)**0.29D0)
     &                               *(THICKNESS**0.61D0)/XNU

      ELSEIF(IBUOY.EQ.3) THEN
!       GOSIK & OSTERKAMP (1983)
!       RMK1: STOKES REGIME (ONLY VALID FOR R < 0.03 CM)
!       RMK2: DRAG COEFFICIENT CKD NEEDS TO BE ADJUSTED
        BUOYANCY_VELOCITY = SQRT(4.D0*(RO0-RHO_ICE)
     &                      *GRAV*RADIUS/(DE*RO0*CKD))
!
      ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END FUNCTION BUOYANCY_VELOCITY
!
!=======================================================================
!
!             ***************************************
              DOUBLE PRECISION FUNCTION MELTING_POINT
!             ***************************************
!
     &  ( SAL )
!
!***********************************************************************
! KHIONE   V7P2
!***********************************************************************
!
!brief    Computes the melting point as a function of salinity
!
!reference:
!+   Millero, F.J. “Freezing point of seawater”. In “Eighth Report of
!+     the Joint Panel on Oceanographic Tables and Standards”,
!+     UNESCO Tech. Pap. Mar. Sci. No.28, Annex 6. UNESCO, Paris, 1978
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SAL   |-->| SALINITY CONCENTRATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)     :: SAL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      MELTING_POINT =
     &  -0.0575*SAL + 0.001710523*( SAL**1.5D0 )
     &  -0.0002154996*( SAL**2 )
!    &  -0.00753 * PRESSURE BELOW SURFACE
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END FUNCTION MELTING_POINT
!
!=======================================================================
!
!                   *****************************
                    SUBROUTINE EROSION_DEPOSITION
!                   *****************************
!
     &  ( FRZL,SRCGM,THETA0,THETA1,BETA1,VBB,THIFEMF,HUN,
     &    ANFEM,VMAG,DEPTH,ISBAR )
!
!***********************************************************************
! KHIONE   V7P2
!***********************************************************************
!
!brief    Computes frazil ice sink/source term due to deposition/erosion
!         under the ice cover
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ANFEM   |-->| CONCENTRATION OF SURFACE ICE PARTICLES
!| BETA1   |-->| RATE OF REENTRAINMENT OF SURFACE ICE PER UNIT AREA
!| DEPTH   |-->| WATER DEPTH
!| FRZL    |-->| FRAZIL CONCENTRATION
!| HUN     |-->| UNCOVERED ICE THICKNESS
!| ISBAR   |-->| WHETHER OR NOT ICE SETTLES AT THE SURFACE OR ON A BAR
!| SRCGM    |<->| EXPLICIT SOURCE OF FRAZIL
!| THETA0  |-->| PROBABILITY OF DEPOSITION OF FRAZIL - OPEN WATER
!| THETA1  |-->| PROBABILITY OF DEPOSITION OF FRAZIL - ICE
!| THIFEMF |-->| FRAZIL ICE THICKNESS
!| VBB     |-->| BUOYANCY VELOCITY OF FRAZIL GRANULES
!| VMAG    |-->| CURRENT VELOCITY MAGNITUDE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY : AF, SURF_EF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)     :: FRZL,ANFEM
      DOUBLE PRECISION, INTENT(INOUT)  :: SRCGM
      DOUBLE PRECISION, INTENT(IN)     :: THETA0,THETA1,BETA1
      DOUBLE PRECISION, INTENT(IN)     :: VMAG,DEPTH,VBB,THIFEMF,HUN
      LOGICAL, INTENT(IN)              :: ISBAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION HE,PV
!
!-----------------------------------------------------------------------
!
!     NODES ON THE BAR: NO DEPOSITION ON THE WATER SURFACE
      IF( ISBAR ) THEN
        PV = ( AF*VMAG )/DEPTH * FRZL
!     NO BAR: DEPOSITION ON WATER SURFACE
      ELSE
        PV = ( (THETA0*VBB*(1-ANFEM) ) +
     &    ( THETA1*VBB*ANFEM) )/DEPTH * FRZL
      ENDIF
!
!     EROSION / DEPOSITION SOURCE TERM
      HE = ( 1.D0-SURF_EF ) * ( THIFEMF + HUN )
      SRCGM = (BETA1*HE*ANFEM)/DEPTH - PV
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE EROSION_DEPOSITION
!
!=======================================================================
!
!                   ************************
                    SUBROUTINE PRECIPITATION
!                   ************************
!
     &  (VBB, FRZL, SRCP, VMAG, G, RK, IT)
!
!***********************************************************************
! KHIONE   V8P2
!***********************************************************************
!
!brief    Computes frazil precipitation on the surface
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FRZL    |-->| FRAZIL CONCENTRATION
!| G       |-->| GRAVITY CONSTANT
!| IT      |-->| LOGICAL FOR ICE COVER
!| RK      |-->| FRAZIL RADIUS
!| SRCP    |<->| EXPLICIT SOURCE OF FRAZIL
!| VBB     |-->| BUOYANCY VELOCITY OF FRAZIL GRANULES
!| VMAG    |-->| CURRENT VELOCITY MAGNITUDE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY : RO0, RHO_ICE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN)              :: IT
      DOUBLE PRECISION, INTENT(IN)     :: FRZL,G,RK
      DOUBLE PRECISION, INTENT(INOUT)  :: SRCP
      DOUBLE PRECISION, INTENT(IN)     :: VMAG,VBB
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION UC2,TC,M
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     COMPUTATION OF THE CRITICAL VELOCITY (USEFUL IF ICE COVER IS
!     PRESENT)
      IF(IT) THEN
        TC = 2.D2/3.D0
        UC2 = TC*(RO0-RHO_ICE)*G*RK/RO0
        M = MAX(0.D0,1.D0-(VMAG*VMAG/UC2))
        SRCP = -VBB*FRZL*M
      ELSE
        SRCP = -VBB*FRZL
      ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE PRECIPITATION
!
!=======================================================================
!
!                   *************************
                    SUBROUTINE CLOGGED_ON_BAR
!                   *************************
!
     &  ( RFR0,RFR1,DB,BAR,NBAR,ANG1,FM1,FMT )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Computes the accumulated mass on vertical and/or
!         transverse bars.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ANG1   |-->| ACCUMULATION ANGLE
!| BAR    |-->| PHYSICAL SIZE OF LENGTHS OF THE BAR
!| DB     |-->| BAR DIAMETER
!| FM1    |<->| MASS OF FRAZIL ICE ACCULATED ON ONE BAR
!| FMT    |<->| TOTAL MASS ACCUMULATED
!| NBAR   |-->| REPRESENTATIVE NUMBER OF BAR
!| RFR0   |-->| INITIAL RADIUS BEFORE THE FRAZIL ICE ACCUMULATION
!| RFR1   |-->| RADIUS OF THE FRAZIL ICE ACCUMULATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY : RHO_ICE,CLOG_EF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: NBAR
      DOUBLE PRECISION, INTENT(IN)    :: RFR0,RFR1,BAR,ANG1,DB
      DOUBLE PRECISION, INTENT(INOUT) :: FM1,FMT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: RB,DR,AMNU,ASH,RATIO,ADONT,AICE
!
!-----------------------------------------------------------------------
!
!      RB = RFR0/2.0/COS(ANG1)
      RB = DB/2.D0
      DR = 2*RB - RFR0
!
      AMNU = RFR0**2*COS(ANG1)*SIN(ANG1) + 2*ANG1*RB**2 -
     &       RB**2*COS(2*ANG1)*SIN(2*ANG1)
      ASH = 2*ANG1*RB**2 - (ANG1*RFR0**2 - RB*RFR0*SIN(ANG1)) ! SHADE AREA
!
      IF (RFR1.GT.2*RB) THEN
        AICE = ANG1*RFR1**2 - AMNU
      ELSE
        ADONT = ( RFR1**2 - RFR0**2 )*ANG1
        RATIO = ( RFR1 - RFR0 ) / DR
        AICE = ADONT - RATIO*ASH
      ENDIF
!
      FM1 = AICE*BAR*RHO_ICE*( 1.D0-CLOG_EF )
      FMT = FM1*NBAR
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CLOGGED_ON_BAR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE FREEZEUP_KHIONE
