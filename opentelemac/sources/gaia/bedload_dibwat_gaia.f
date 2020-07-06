!                   ******************************
                    SUBROUTINE BEDLOAD_DIBWAT_GAIA
!                   ******************************
!
     &(U2D,V2D,UNORM, CF, TOB, TOBW, UW, TW, FW, THETAW, NPOIN,
     & XMVE, DENS, GRAV, DCLA, XWC, PI, ALPHAW, T2, T3, UCW, UCN,
     & UW1, UW2, TW1, TW2, THETAC, FCW, QSC,XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Dibajnia & Watanabe formulation (1992).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] ALPHAW Angle between wave and current
!>@param[in]     CF     Quadratic friction coefficient
!>@param[in]     DENS   Relative density of sediment
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in]     FCW    Wave-current friction factor
!>@param[in]     FW     Quadratic friction coefficient (wave)
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     NPOIN  Number of points
!>@param[in]     PI     Pi
!>@param[in,out] QSC    Bed load transport rate
!>@param[in,out] T2     Work bief_obj structure
!>@param[in,out] T3     Work bief_obj structure
!>@param[in,out] THETAC Current angle to the x axis
!>@param[in]     THETAW Wave direction (deg wrt ox axis)
!>@param[in]     TOB    Bed shear stress (total friction)
!>@param[in]     TOBW   Wave induced shear stress
!>@param[in]     TW     Mean wave period
!>@param[in,out] TW1    Mid period (u(t)>0)
!>@param[in,out] TW2    Mid period (u(t)<0)
!>@param[in]     U2D    Depth-averaged velocity x-direction
!>@param[in]     UNORM  Mean current
!>@param[in,out] UCN    Mean current at an angle to the wave
!>@param[in,out] UCW    Mean current projected in the wave direction
!>@param[in]     UW     Orbital wave velocity
!>@param[in,out] UW1    Mean current in the wave direction
!>@param[in,out] UW2    Mean current in the opposite direction
!>@param[in]     V2D    Depth-averaged velocity y-direction
!>@param[in]     XMVE   Fluid density
!>@param[in]     XMVS   Sediment density
!>@param[in]     XWC    Settling velocity
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_DIBWAT => BEDLOAD_DIBWAT_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     GLOBAL VARIABLES
!     -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UNORM, CF, TOB, TOBW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UW, TW, FW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: THETAW
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, DENS, GRAV, DCLA, XWC, PI
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW          ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2, T3          !
      TYPE(BIEF_OBJ),   INTENT(INOUT) ::  UCW, UCN ! WORK ARRAY T4, T5, T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UW1, UW2, TW1   ! WORK ARRAY T7, T8, T9
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TW2, THETAC     ! WORK ARRAY T10, T11
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FCW, QSC        ! WORK ARRAY T12
!
!     LOCAL VARIABLES
!     ------------------
      INTEGER                     :: I
      DOUBLE PRECISION            :: UCW2, SHIELDS, OMEGACR, OMEGA1
      DOUBLE PRECISION            :: OMEGA2, W1, WP1, W2, WP2, GAMMAW
      DOUBLE PRECISION            :: GAMMAN, GAMMA, QSW, QSN
      DOUBLE PRECISION, PARAMETER :: ZERO  = 1.D-6
      DOUBLE PRECISION, PARAMETER :: ALPHA = 0.0001D0
      DOUBLE PRECISION, PARAMETER :: BETA  = 0.55D0
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     ANGLE OF VELOCITY WITH OX
!
      CALL BEDLOAD_DIRECTION_GAIA(U2D,V2D,NPOIN,PI,THETAC)
!
!     PROJECTIONS OF UNORM IN THE WAVE DIRECTION
!
      CALL OS('X=CY    ', X=ALPHAW, Y=THETAW, C=-PI/180.D0)
      CALL OS('X=X+C   ', X=ALPHAW, C=0.5D0*PI)
      CALL OS('X=Y-Z   ', X=ALPHAW, Y=ALPHAW, Z=THETAC)
      CALL OS('X=COS(Y)', X=T2    , Y=ALPHAW)
      CALL OS('X=SIN(Y)', X=T3    , Y=ALPHAW)
      CALL OS('X=YZ    ', X=UCW   , Y=UNORM , Z=T2)
      CALL OS('X=CYZ   ', X=UCN   , Y=UNORM , Z=T3, C= -1.D0)
!
!     QUADRATIC VELOCITIES (UW1=U(T)> 0 AND UW2=U(T)
!
      CALL BEDLOAD_CALCDW_GAIA(UCW,UW,TW,NPOIN,PI,UW1,UW2,TW1,TW2)
!
!     FRICTION COEFFICIENT
!
      CALL BEDLOAD_INTERACT_GAIA
     &     (UNORM,TOBW,TOB,ALPHAW,FW,CF,UW,NPOIN,XMVE,FCW)
!
      DO I = 1, NPOIN
!
!       SHIELDS PARAMETER
        UCW2    = UNORM%R(I)**2 + 0.5D0 * UW%R(I)**2
        SHIELDS = FCW%R(I)*UCW2/DENS/GRAV/DCLA
!
!       CRITICAL OMEGA (TEMPERVILLE AND GUIZA,2000)  !
        IF (SHIELDS <= 0.2D0) THEN
          OMEGACR = 0.03D0
        ELSEIF (SHIELDS <= 0.4D0) THEN
          OMEGACR = 1.D0 - SQRT(1.D0-((SHIELDS-0.2D0)/0.58D0)**2)
        ELSEIF (SHIELDS <= 1.5D0) THEN
          OMEGACR = 0.8104D0 * SQRT(SHIELDS) - 0.4225D0
        ELSE
          OMEGACR = 0.7236D0 * SQRT(SHIELDS) - 0.3162D0
        ENDIF
!
!       OMEGA1, OMEGA2  !
        IF(TW1%R(I) > ZERO) THEN
          OMEGA1 = UW1%R(I)**2 / (2.D0*DENS*GRAV*XWC*TW1%R(I))
        ELSE
          OMEGA1 = ZERO
        ENDIF
!
        IF(TW2%R(I) > ZERO) THEN
          OMEGA2 = UW2%R(I)**2 / (2.D0*DENS*GRAV*XWC*TW2%R(I))
        ELSE
          OMEGA2 = ZERO
        ENDIF
!
!       QUANTITIES OF SAND DEPOSITED AND
!       IN SUSPENSION AT EACH PHASE OF THE CYCLE
!
!       PHASE 1
        IF (OMEGA1 <= OMEGACR) THEN
          W1  = 2.D0 * OMEGA1 * XWC * TW1%R(I) / DCLA
          WP1 = 0.D0
        ELSE
          W1  = 2.D0 * OMEGACR * XWC * TW1%R(I) / DCLA
          WP1 = 2.D0 * (OMEGA1-OMEGACR) * XWC * TW1%R(I) / DCLA
        ENDIF
!
!       PHASE 2
        IF (OMEGA2 <= OMEGACR) THEN
          W2  = 2.D0 * OMEGA2 * XWC * TW2%R(I) / DCLA
          WP2 = 0.D0
        ELSE
          W2  = 2.D0 * OMEGACR * XWC * TW2%R(I) / DCLA
          WP2 = 2.D0 * (OMEGA2-OMEGACR) * XWC * TW2%R(I) / DCLA
        ENDIF
!
!       GAMMAW, GAMMAN,GAMMA
        IF ((UW2%R(I)*TW2%R(I) + UW1%R(I)*TW1%R(I)) > 0.D0 ) THEN
          GAMMAW = (  UW1%R(I) * TW1%R(I) * (W1**3+WP2**3)
     &              - UW2%R(I) * TW2%R(I) * (W2**3+WP1**3))
     &           / ( UW1%R(I)*TW1%R(I)+UW2%R(I)*TW2%R(I))
        ELSE
          GAMMAW = (2.D0 * UCW%R(I)**2 / DENS / GRAV / DCLA)**3
        ENDIF
        GAMMAN = (2.D0 * UCN%R(I)**2 / DENS / GRAV / DCLA)**3
        GAMMA  = MAX(SQRT(GAMMAW**2 + GAMMAN**2),1.D-10)
!
!       SOLID TRANSPORT IN THE WAVE DIRECTION : QSW
!       AND IN THE NORMAL DIRECTION : QSN
!
        QSW      = ALPHA * GAMMAW * XWC * DCLA / GAMMA**(1.D0-BETA)
        QSN      = ALPHA * GAMMAN * XWC * DCLA / GAMMA**(1.D0-BETA)
        QSC%R(I) = SQRT(QSW**2 + QSN**2)
!
      ENDDO
!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!----------------------------------------------------------------------
!
      RETURN
      END
