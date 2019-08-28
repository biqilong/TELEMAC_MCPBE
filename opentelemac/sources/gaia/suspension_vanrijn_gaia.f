!                     **********************************
                      SUBROUTINE SUSPENSION_VANRIJN_GAIA
!                     **********************************
!
     &(DCLA,TAUP,NPOIN,GRAV,XMVE,XMVS,VCE,ZERO,AC,CSTAEQ,ZREF,
     & RATIO_TOCE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@todo Add brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] AC     Critical shields parameter
!>@param[in]     ACLADM Mean diameter of active layer
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     NPOIN  Number of points
!>@param[in]     VCE    Water viscosity
!>@param[in]     XMVE   Fluid density
!>@param[in]     XMVS   Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    ::  TAUP,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    ::  GRAV,  XMVE, XMVS,VCE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,AC,DCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ
      TYPE(BIEF_OBJ),   INTENT(IN)    :: RATIO_TOCE
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   ::  TAUC,AUX,DSTAR,DENS
!
!======================================================================!
!======================================================================!
!                               PROGRAMME                              !
!======================================================================!
!======================================================================!
!
      ! ******************************** !
      !    I - CRITICAL SHIELD PARAMETER !
      ! ******************************** !
!
!
      DO I=1,NPOIN
!
! ****************** !
! II - SKIN FRICTION !
! ****************** !
!
!        TOCE IS MODIFIED IN TOCE_MIX : TOCE_MIX=TOCE_SAND*RATIO_TOCE
!        WE CAN APPLICATE DIRECTLY THIS RATIO ON TAUC
!        (ratio_toce=1 if no mud)
!        why recalculate TAUC?? and not use TOCE
!        TAUC = AC * GRAV*(XMVS-XMVE)*DCLA
        TAUC = AC * GRAV*(XMVS-XMVE)*DCLA*RATIO_TOCE%R(I)
        DENS  = (XMVS - XMVE )/ XMVE
        DSTAR = DCLA*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
!
! ***************** !
! IV - EROSION FLUX !
! ***************** !
! Concentration increased by AVA because it is assumed
! that it is computed only with one class of sediment
!
        IF(DSTAR.LE.ZERO) THEN
          WRITE(LU,*) 'ERROR SUSPENSION_VANRIJN_GAIA'
          CALL PLANTE(1)
          STOP
        ENDIF
        AUX=(TAUP%R(I)-TAUC)/TAUC
        IF(AUX.GT.ZERO) THEN
          CSTAEQ%R(I)=0.015*DCLA*SQRT(AUX**3)/(ZREF%R(I)*DSTAR**0.3D0)
        ELSE
          CSTAEQ%R(I) = 0.D0
        ENDIF
!------------  in kg/m3
        CSTAEQ%R(I) = XMVS*CSTAEQ%R(I)
!
      ENDDO
!
!======================================================================!
!======================================================================!
!======================================================================!
!
      RETURN
      END
