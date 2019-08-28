!                 **********************************
                  SUBROUTINE BEDLOAD_HUNZ_MEYER_GAIA
!                 **********************************
!
     &  (TOB, MU, ACLADM, UNLADM, NPOIN, DENS, XMVE, GRAV, DCLA, AC,
     &    TETAP, AHUNZI, ACP, HIDING, QSC,XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Hunziker bedload formulation (1995)
!!       (adapted from Meyer-Peter formulation).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     AC     Critical shields parameter
!>@param[in]     ACLADM Mean diameter of active layer
!>@param[in,out] ACP    Modified shields parameter
!>@param[in,out] AHUNZI Coefficient of hunziker formula
!>@param[in]     DENS   Relative density of sediment
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     HIDING Hiding factor correction
!>@param[in,out] MU     Skin friction correction factor for bed roughness
!>@param[in]     NPOIN  Number of points
!>@param[in,out] QSC    Bed load transport rate
!>@param[in,out] TETAP  Adimensional skin friction
!>@param[in]     TOB    Bed shear stress (total friction)
!>@param[in]     UNLADM Mean diameter of under-layer
!>@param[in]     XMVE   Fluid density
!>@param[in]     XMVS   Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,
     &          EX_BEDLOAD_HUNZ_MEYER => BEDLOAD_HUNZ_MEYER_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU, ACLADM, UNLADM
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, XMVE, GRAV, DCLA, AC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETAP, AHUNZI ! WORK ARRAY T1, T2
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP           ! WORK ARRAY T3
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: HIDING, QSC
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: C1, C2
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ************************************* !
      ! I - ADIMENSIONAL SKIN STRESS          !
      ! ************************************* !
      C1 = 1.D0/(DENS*XMVE*GRAV*DCLA)
      C2 = 5.D0*SQRT(GRAV*DENS*DCLA**3)
      CALL OS('X=CYZ   ', X=TETAP, Y=TOB, Z=MU, C=C1)
!
!     CHANGED BY JMH ON 28/10/2009 AFTER MODIFICATIONS BY
!     REBEKKA KOPMANN TRANSMITTED BY JACEK JANKOWSKI
!     CALL OS('X=+(Y,C)', X=TETAP , Y=TETAP, C= 1.D-06 )
      CALL OS('X=+(Y,C)', X=TETAP , Y=TETAP, C= 1.D-02 )
!
      CALL OS('X=Y**C  ', X=AHUNZI, Y=TETAP, C=-1.5D0  )
      CALL OS('X=CX    ', X=AHUNZI, C= 0.011D0)
      CALL OS('X=X+C   ', X=AHUNZI, C=-0.3D0  )
!
! RK COMMENT:
! BEWARE: AHUNZI CAN BECOME SO LARGE THAT THE HIDING FACTOR BECOMES
! INFINITE; HUNZIKER HIMSELF SUGGESTS THAT IT BE CAPPED TO 2.3.
! THIS INITIALLY ADOPTS A LIMIT OF APPROXIMATELY 10.
! (WHICH IS APPARENT IN TETAP BEING SET TO VALUES .GE. 0.01)
!
!     REMARK BY JMH: I WOULD STRONGLY RECOMMEND A SINGLE LOOP
!                    WITH THE WHOLE FORMULA, INSTEAD OF PILING
!                    UP CALLS TO OS
!
      DO I = 1, NPOIN
        HIDING%R(I) = (DCLA/ACLADM%R(I))**(-AHUNZI%R(I))
      ENDDO
      ! ************************************************* !
      ! IV - CORRECTS THE ADIMENSIONAL CRITICAL STRESS    !
      ! ************************************************* !
      CALL OS('X=Y/Z   ', X=ACP, Y=UNLADM, Z=ACLADM)
      CALL OS('X=Y**C  ', X=ACP, Y=ACP   , C=0.33D0)
      CALL OS('X=CX    ', X=ACP, C=AC)
      ! ********************* !
      ! V - TRANSPORT RATE    !
      ! ********************* !
      CALL OS('X=Y-Z   ', X=QSC, Y=TETAP , Z=ACP )
      CALL OS('X=+(Y,C)', X=QSC, Y=QSC   , C=0.D0)
      CALL OS('X=XY    ', X=QSC, Y=HIDING)
      CALL OS('X=Y**C  ', X=QSC, Y=QSC   , C=1.5D0)

      CALL OS('X=CX    ', X=QSC, C=C2)
!======================================================================!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!======================================================================!
      RETURN
      END
