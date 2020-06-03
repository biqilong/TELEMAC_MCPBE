!                       ******************
                        SUBROUTINE FLUX_FV
!                       ******************
!
     &(W,LIMPRO,NUBO,VNOIN,FLUX,FLUSORT,FLUENT,FLBOR,ELTSEG,IFABOR,DT,
     & FLUHBTEMP,FLUHTEMP,LEO,GPRDTIME,NEISEG)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    Compute fluxes for finite volume numerical schemes
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  DT         TIME STEP
!>@param  [in]      ELTSEG     SEGMENT NUMBERS PER ELEMENT
!>@param  [in,out]  FLBOR      IN AND OUT WATER MASS FLUX
!>@param  [in,out]  FLUHBOR    BORD FLUX FOR TRACER
!>@param  [in,out]  FLUHTEMP   FLUX FOR TRACER
!>@param  [in,out]  FLUHBTEMP  BOUNDARY FLUX FOR TRACER
!>@param  [in,out]  FLUXT      FLUX FOR TRACER
!>@param  [in,out]  FLUENT     MASS FLUX INLET FROM TN TO TN+1
!>@param  [in,out]  FLUSORT    MASS FLUX OUTLET FROM TN TO TN+1
!>@param  [in,out]  FLUX       FLUX
!>@param  [in]      GPRDTIME   TEST TIME STEP BIGGER THAN GRAPHIC OUTPUT
!>@param  [in]      IFABOR     ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!>@param  [in,out]  LEO        LOGICAL FOR GRAPHICAL OUTPUT
!>@param  [in]      LIMPRO     TYPES OF BOUNDARY CONDITION
!>@param  [in]      NEISEG     NEIGHBORS OF SEGMENT (FOR LIMITER)
!>@param  [in]      NUBO       GLOBAL INDICES OF EDGE EXTREMITIES
!>@param  [in]      VNOIN      NORMAL TO THE INTERFACE
!>@param  [in,out]  W          WORKING TABLE CONTAINING H,HU,HV
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : PTINIG,BNDCIN,NPOIN,NPTFR,
     &                            MESH,NELEM,NSEG,SORDER,NTRAC,HROPT,
     &                            ICIN,LT,GRAV,X,Y,ZF,TMAX,HBOR,UBOR,
     &                            VBOR,AT,CFLWTD
      USE DECLARATIONS_TELEMAC,ONLY: KDIR,KNEU
      USE INTERFACE_TELEMAC2D, EX_FLUX_FV => FLUX_FV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IMPLICIT NONE
      LOGICAL, INTENT(INOUT) :: LEO
      INTEGER, INTENT(IN) :: LIMPRO(NPTFR,6)
      INTEGER, INTENT(IN) :: NUBO(2,*)
      INTEGER, INTENT(IN) :: ELTSEG(NELEM,3),IFABOR(NELEM,*)
      INTEGER, INTENT(IN) :: NEISEG(2,NSEG)
      DOUBLE PRECISION, INTENT(IN) :: VNOIN(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: GPRDTIME
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),FLUSORT,FLUENT
      DOUBLE PRECISION, INTENT(INOUT) :: DT,W(3,NPOIN)
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: FLBOR
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: FLUHTEMP,FLUHBTEMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL THEEND
      INTEGER IS
      DOUBLE PRECISION RESTE
!
!-----------------------------------------------------------------------
!
      CALL HYD_FV(NUBO,W,VNOIN,ELTSEG,FLUX,IFABOR,FLUHTEMP,NEISEG)
!
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(FLUX(:,1),FLUX(:,2),FLUX(:,3),NPOIN,1,2,3,MESH)
      ENDIF
!
      IF((ICIN.EQ.1).AND.(BNDCIN.EQ.1)) THEN
        CALL CDL_CIN(LIMPRO,W,FLUX,FLUENT,FLUSORT,FLBOR,DT,FLUHBTEMP)
      ELSE
        CALL CDL_FV(LIMPRO,W,FLUX,FLUENT,FLUSORT,FLBOR,FLUHBTEMP)
      ENDIF
!
      IF(SORDER.EQ.2)THEN
        THEEND =.FALSE.
        LEO    =.FALSE.
        IF((TMAX-AT).LE.DT)THEEND=.TRUE. !LAST TIME STEP
!       ADAPT DT TO TAKE INTO ACCOUNT GRAPHIC OUTPUT
        IS=CEILING(AT/GPRDTIME)
        RESTE=IS*GPRDTIME-AT
!
        IF(THEEND.OR.
     &    (RESTE.LE.DT.AND.
     &     RESTE.GT.EPSILON(RESTE).AND.LT.GT.PTINIG))THEN
!         HERE THERE IS GRAPHICAL OUTPUT
          LEO = .TRUE.
          DT=MIN(RESTE,DT)
        ENDIF
      ENDIF
!
      DT = MIN(DT,TMAX-AT)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
