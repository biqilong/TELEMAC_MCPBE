!                    *******************************
                     SUBROUTINE BEDLOAD_BAILARD_GAIA
!                    *******************************
!
     &(U2D,V2D,UNORM,TOB,TOBW,THETAW,UW,FW,CF,NPOIN,PI,
     & XMVE,GRAV,DENS,XWC,ALPHAW,QSCX,QSCY,QSSX,QSSY,
     & UC3X,UC3Y,US4X,US4Y,THETAC,FCW,QSC,QSS,HOULE,XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Bailard formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] ALPHAW Angle of waves with ox
!>@param[in]     CF     Quadratic friction coefficient
!>@param[in]     DENS   Relative density of sediment
!>@param[in,out] FCW    Wave-current friction angle
!>@param[in]     FW     Quadratic friction coefficient (wave)
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     HOULE  Logical, for wave effects
!>@param[in]     NPOIN  Number of points
!>@param[in]     PI     Pi
!>@param[in,out] QSC    Bedload transport rate
!>@param[in,out] QSCX   Bedload transport rate in the x-direction
!>@param[in,out] QSCY   Bedload transport rate in the y-direction
!>@param[in,out] QSS    Suspended load transport rate
!>@param[in,out] QSSX   Suspended load transport rate in the x-direction
!>@param[in,out] QSSY   Suspended load transport rate in the y-direction
!>@param[in,out] THETAC Current angle to the x direction
!>@param[in]     THETAW Wave direction (deg wrt ox axis)
!>@param[in]     TOB    Bed shear stress (total friction)
!>@param[in]     TOBW   Wave induced shear stress
!>@param[in]     U2D    Depth-averaged velocity x-direction
!>@param[in,out] UC3X   Work array
!>@param[in,out] UC3Y   Work array
!>@param[in]     UNORM  Norm of the mean flow velocity
!>@param[in,out] US4X   Work array
!>@param[in,out] US4Y   Work array
!>@param[in]     UW     Orbital wave velocity
!>@param[in]     V2D    Depth-averaged velocity y-direction
!>@param[in]     XMVE   Fluid density
!>@param[in]     XWC    Settling velocity
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_BAILARD => BEDLOAD_BAILARD_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     GLOBAL VARIABLES
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UNORM, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, THETAW, UW, FW, CF
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: PI, XMVE, GRAV, DENS, XWC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW        ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCX, QSCY    ! WORK ARRAY T2 AND T3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSSX, QSSY    ! WORK ARRAY T4 AND T5
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UC3X, UC3Y    ! WORK ARRAY T6 AND T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: US4X, US4Y    ! WORK ARRAY T8 AND T9
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: THETAC, FCW   ! WORK ARRAY T10 AND T11
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
!
!     LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: C3, C4, PHI
      DOUBLE PRECISION, PARAMETER :: EPSC = 0.21D0   ! BEDLOAD
      DOUBLE PRECISION, PARAMETER :: EPSS = 0.025D0  ! SUSPENSION
      DOUBLE PRECISION            :: U3X, U3Y, NUM
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     CASE WITH WAVES
!
      IF(HOULE) THEN
!
!     ANGLE OF VELOCITY WITH OX (IN RADIANS)
!
      CALL BEDLOAD_DIRECTION_GAIA(U2D,V2D,NPOIN,PI,THETAC)
!
!     ANGLE OF WAVES WITH OX (IN RADIANS)
!
      CALL OS('X=CY    ', X=ALPHAW, Y=THETAW, C=-PI/180.D0)
      CALL OS('X=X+C   ', X=ALPHAW, C=0.5D0*PI)
      CALL OS('X=Y-Z   ', X=ALPHAW, Y=ALPHAW, Z=THETAC)
!
!     PARAMETERS  ,
!
!
!     US4X AND US4Y ARE WORK ARRAYS, THEIR STRUCTURE IS GIVEN HERE
!     THE STRUCTURE OF THETAC (CATHERINE DON'T REMOVE THIS PLEASE)
      CALL CPSTVC(THETAC,US4X)
      CALL CPSTVC(THETAC,US4Y)
!
      DO I = 1, NPOIN
        ! ********************* !
        ! I - CURRENT REFERENCE SYSTEM !
        ! ********************* !
        U3X = UNORM%R(I)**3
     &      + UNORM%R(I)*UW%R(I)**2 * (1 + COS(2.D0*ALPHAW%R(I))/ 2.D0)
        U3Y = UNORM%R(I)*UW%R(I)**2 * SIN(2.D0*ALPHAW%R(I)) / 2.D0
        ! ********************************************** !
        ! II - 3RD ORDER MOMENTUM (LINEAR WAVE THEORY)   !
        ! ********************************************** !
        UC3X%R(I) = U3X * COS(THETAC%R(I)) - U3Y * SIN(THETAC%R(I))
        UC3Y%R(I) = U3X * SIN(THETAC%R(I)) + U3Y * COS(THETAC%R(I))
        ! ************************************************************ !
        ! III -  4TH ORDER MOMENTUM (COLINEAR WAVES AND CURRENTS)      !
        ! ************************************************************ !
        NUM = ( 8.D0*UNORM%R(I)**4 + 3.D0*UW%R(I)**4
     &          + 24.D0*(UNORM%R(I)**2)*(UW%R(I)**2) )*0.125D0
        US4X%R(I) = NUM * COS(THETAC%R(I))
        US4Y%R(I) = NUM * SIN(THETAC%R(I))
      ENDDO
      ! *********************************************** !
      ! IV -  FRICTION COEFFICIENT WAVE + CURRENT       !
      ! *********************************************** !
      CALL BEDLOAD_INTERACT_GAIA
     &     (UNORM,TOBW,TOB,ALPHAW,FW,CF,UW,NPOIN,XMVE,FCW)
      ! ******************************** !
      ! V - TRANSPORT RATES              !
      ! ******************************** !
      PHI = PI   / 6.D0  ! FRICTION ANGLE
      C3  = EPSC / (GRAV*DENS*TAN(PHI))
      C4  = EPSS / (GRAV*DENS*XWC)
      CALL OS('X=CYZ   ', X=QSCX, Y=FCW,  Z=UC3X, C=C3)
      CALL OS('X=CYZ   ', X=QSCY, Y=FCW,  Z=UC3Y, C=C3)
      CALL OS('X=CYZ   ', X=QSSX, Y=FCW,  Z=US4X, C=C4)
      CALL OS('X=CYZ   ', X=QSSY, Y=FCW,  Z=US4Y, C=C4)
!
!     CASE WITHOUT WAVES
!
      ELSE
!
        WRITE(LU,*) 'BAILARD WITHOUT WAVES NOT PROGRAMMED'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     NORMS
!
      CALL OS('X=N(Y,Z)', X=QSC,  Y=QSCX, Z=QSCY)
      CALL OS('X=N(Y,Z)', X=QSS,  Y=QSSX, Z=QSSY)
!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
      CALL OS('X=CX    ', X=QSS, C=XMVS)
!======================================================================!
      RETURN
      END
