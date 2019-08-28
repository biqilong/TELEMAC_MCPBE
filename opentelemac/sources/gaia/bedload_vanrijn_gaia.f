!                 *******************************
                  SUBROUTINE BEDLOAD_VANRIJN_GAIA
!                 *******************************
!
     &(TETAP, NPOIN, DCLA, DENS, GRAV, DSTAR, AC, QSC, XMVS)
!
!***********************************************************************
! GAIA   V6P1                                   21/07/2011
!***********************************************************************
!
!>@brief Van Rijn bedload transport formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     AC     Critical shields parameter
!>@param[in]     DENS   Relative density of sediment
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in]     DSTAR  Non-dimensional diameter
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     NPOIN  Number of points
!>@param[in,out] QSC    Bedload transport rate
!>@param[in]     TETAP  Adimensional skin friction
!>@param[in]     XMVS   Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_VANRIJN => BEDLOAD_VANRIJN_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)  :: TETAP
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: DCLA, DENS, GRAV, DSTAR, AC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
      INTEGER          :: I
      DOUBLE PRECISION :: C1, C2, T
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      C1 = DENS * GRAV * DCLA
      C2 = 0.053D0 * SQRT(DCLA**3*DENS*GRAV) * DSTAR**(-0.3D0)
      DO I = 1, NPOIN
!       ******************************
!       I - TRANSPORT STAGE PARAMETER
!       ******************************
        IF(TETAP%R(I) .LE. AC) THEN
          T = 0.D0
        ELSE
          T = (TETAP%R(I)-AC)/MAX(AC,1.D-06)
        ENDIF
!
!       *****************************
!       II - BEDLOAD TRANSPORT RATE
!       *****************************
        QSC%R(I) = C2 * T**2.1D0
      ENDDO
!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_VANRIJN_GAIA
