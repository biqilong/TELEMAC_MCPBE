
!                    *************************************
                     SUBROUTINE BEDLOAD_WILCOCK_CROWE_GAIA
!                    *************************************
!
     &(TOB, MU, ACLADM, DCLA, RATIO_SAND, GRAV, XMVE, XMVS, SANFRA, QSC,
     & AC, ACP, SLOPEFF,COEFPN)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!brief    WILCOCK AND CROWE NON-UNIFORM TRANSPORT FORMULATION.
!
!history  F.CORDIER & P. TASSI (EDF-LNHE)
!+        16/09/2018
!+        V8P0 (in GAIA)
!+  Implementation of the Wilcock and Crowe formula
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] AC      Critical Shields parameter
!>@param[in,out] ACP     Modified Shields parameter
!>@param[in]     COEB    Power coeff of the hiding-exposure function
!>@param[in,out] COEFPN  Correction of transort for sloping bed effect
!>@param[in]     DENS    Relative density
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in]     GRAV    Acceleration of gravity
!>@param[in,out] QSC     Bed load transport
!>@param[in]     SANFRA  Sand fraction
!>@param[in]     SLOPEFF Logical, sloping bed effect or not
!>@param[in]     TETAP   Adimensional skin friction
!>@param[in]     TORATIO Ratio between bed shear stress and tori
!>@param[in]     TORI    Reference shear stress of the i-th size sediment
!>@param[in]     TORM    Reference shear stress of the median sediment
!>@param[in]     WI      Dimensionless transport rate of WC-2003
!>@param[in]     WCC     Coefficient of calibration
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_GAIA,
     &      EX_BEDLOAD_WILCOCK_CROWE_GAIA => BEDLOAD_WILCOCK_CROWE_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: SLOPEFF
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, COEFPN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, GRAV, DCLA, AC,
     &                                   RATIO_SAND(QSC%DIM1)
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(QSC%DIM1)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION TORM, TORI, TORATIO, WI, COEFB, WCC
!
!======================================================================!
!                               PROGRAM                                !
!=======================================================================
!
      CALL CPSTVC(QSC,ACP)
      CALL OS('X=C     ', X=ACP, C=AC)
!
!     SLOPE EFFECT: SOULBY FORMULATION
!
      IF(SLOPEFF.EQ.2) THEN
        CALL OS('X=XY    ', X=ACP, Y=COEFPN)
      ENDIF
!
!     COEFFICIENT TO CALIBRATE THE FORMULA (by default = 1.0)
      WCC = 1.0D0
!
      DO I=1,QSC%DIM1
        TORM = (0.021D0 + 0.015D0*EXP(-20.D0*SANFRA(I)))*
     &       (XMVS/XMVE-1.D0)*XMVE*GRAV*ACLADM%R(I)
        COEFB = 0.67D0/(1.0D0+EXP(1.5D0-DCLA/ACLADM%R(I)))
        TORI = TORM*((DCLA/ACLADM%R(I))**COEFB)
        TORATIO = TOB%R(I)*MU%R(I)/TORI
        IF (TORATIO.LT.1.35D0) THEN
          WI = 2.D-3*(TORATIO**7.5D0)
        ELSE
          WI = 14.D0*((1.D0-0.894D0/SQRT(TORATIO))**4.5D0)
        ENDIF
!
        QSC%R(I)=WCC*WI*RATIO_SAND(I)*((TOB%R(I)*MU%R(I)
     &  /XMVE)**1.5D0)/((XMVS/XMVE-1.D0)*GRAV)
!       IF VERY LOW TRANSPORT WE IMPOSE QB = 0 (TO AVOID NUMERICAL
!       ARTIFACTS)
        IF (QSC%R(I).LT.1.D-13) THEN
          QSC%R(I)=0.0D0
        ENDIF
      ENDDO
!
!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!=======================================================================
!
      RETURN
      END
!
