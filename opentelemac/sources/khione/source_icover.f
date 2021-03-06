!                   ************************
                    SUBROUTINE SOURCE_ICOVER
!                   ************************
     &(NPOIN,FU,FV,H,U,V,T1,T2,T3,GRAV,KARMAN,CHESTR,DT,AT)
!
!***********************************************************************
! KHIONE   V7P2                                             02/11/2016
!***********************************************************************
!
!brief    COMPUTES CONTRIBUTION TO MOMENTUM FORCES AND WATER LEVEL
!+        TERMS RESULTING FROM ICE PROCESSES.
!
!history  F. SOUILLE (EDF)
!+        30/10/2019
!+        V8P0
!+        Updated friction source term
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT         |-->| TIME IN SECONDS
!| CHESTR     |-->| FRICTION COEFFICIENT
!| DT         |-->| TIME STEP
!| FU         |<->| SOURCE TERMS ON VELOCITY U
!| FV         |<->| SOURCE TERMS ON VELOCITY V
!| GRAV       |-->| GRAVITY
!| H          |-->| WATER DEPTH
!| KARMAN     |-->| VON KARMAN'S CONSTANT
!| NPOIN      |-->| NUMBER OF NODES IN THE MESH
!| T1         |<->| WORKING ARRAY
!| T2         |<->| WORKING ARRAY
!| T3         |<->| WORKING ARRAY
!| U          |-->| X COMPONENT OF THE VELOCITY
!| V          |-->| Y COMPONENT OF THE VELOCITY
!| WINDX      |<->| FIRST COMPONENT OF WIND VELOCITY
!| WINDY      |<->| SECOND COMPONENT OF WIND VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY: ICOVER_IMPACT,BD_ICE,RHO_AIR,FICE,
     &                               FICE_MAX,VZ,IFROT,IFICE,ICESTR,
     &                               THIE,THIFEMS,THIFEMF,HUN,RO0
!
      USE METEO_KHIONE,        ONLY: SYNC_METEO,WINDX,WINDY
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT,GRAV,KARMAN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FU,FV
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CHESTR, H,U,V
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: SP_EAU
      DOUBLE PRECISION            :: VMAG,WMAG,CD,CSTAR,CWSTAR,VZ31
      DOUBLE PRECISION            :: VZ32,UNTIER,UNSIX
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!=======================================================================
!
!     FOR GOOD MEASURE - SHOULD BE DONE AT THE TELEMAC-2D LEVEL
!
!-----------------------------------------------------------------------
!
!     NOTE THAT "AT" FROM PROSOU IS AREADY TOO FAR GONE
      CALL SYNC_METEO(AT-DT)
!
!
!=======================================================================
!
!     STATIC BORDER ICE GROWTH
!
!-----------------------------------------------------------------------
!
      IF( BD_ICE ) THEN
!
!-----------------------------------------------------------------------
!       PREPARATION TO STATIC BORDER ICE GROWTH
!
        CSTAR  = 0.25D0
        CWSTAR = 1.D0
        CD     = 1.3D-3
        UNTIER = 1.D0/3.D0
        UNSIX  = 1.D0/6.D0
!
        DO I = 1,NPOIN
!
! ~~>     WIND SPEED EFFECTS ON ICE
          WMAG = SQRT( WINDX%R(I)**2 + WINDY%R(I)**2 )
! ~~>     FLOW SPEED EFFECTS ON ICE
          VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!
! ~~>     VERTICAL TURBULENT INTENSITY /!\ REPLACE BY CF%R
          VZ31 = ( SQRT( CSTAR*GRAV ) * VMAG*CHESTR%R(I)
     &             / MAX( H%R(I), 0.05D0 )**UNSIX)**3.D0
          VZ32 = CWSTAR * (CD*RHO_AIR/RO0)**(1.5D0)*WMAG**3.D0
          VZ%R(I) = (VZ31 + VZ32)**UNTIER
!
        ENDDO
!
      ENDIF
!
!
!=======================================================================
!
!     THERMAL BALANCE AND ICE COVER GROWTH
!
!     FOLLOW-UP IMPACT ON THE HYDRODYNAMICS
!
!     TODO: Implement THERMAL_BUDGET=TRUE
!
!=======================================================================
!
!     ICE COVER IMPACT ON HYDRODYNAMICS
!
!-----------------------------------------------------------------------
!
      IF( ICOVER_IMPACT ) THEN
!
! ~~>   PREPARATORY WORK
!
!       COMPUTATION OF TOTAL ICE THICKNESS
        CALL OS( 'X=Y+Z   ', X=T2,Y=THIFEMS,Z=THIFEMF )
        CALL OS( 'X=X+Y   ', X=T2,Y=HUN)
!
!       COMPUTATION OF SURFACE ICE GRADIENTS (TODO)
!        CALL OS( 'X=Y+Z   ', X=T1,Y=H,Z=ZF)
!        CALL OS( 'X=Y+CZ  ', X=T1,Y=T1,Z=T2,C=RHO_ICE/RO0 )
!        CALL VECTOR(DCOVX,'=','GRADF          X',U%ELM,1.D0,T1,
!     &    S,S,S,S,S,MESH,MSK,T3)
!        CALL VECTOR(DCOVY,'=','GRADF          Y',U%ELM,1.D0,T1,
!     &    S,S,S,S,S,MESH,MSK,T3)
!        IF(NCSIZE.GT.1) THEN
!          CALL PARCOM(DCOVX,2,MESH)
!          CALL PARCOM(DCOVY,2,MESH)
!        ENDIF
!        CALL OS( 'X=XY    ',X=DCOVX,Y=UNSV2D )
!        CALL OS( 'X=XY    ',X=DCOVY,Y=UNSV2D )
!
!       UPDATE FRICTION IF LINEAR FRICTION COEF LAW IS SELECTED
        IF (IFICE .EQ. 1) THEN
          CALL OS( 'X=CY    ', X=ICESTR, Y=T2, C=( FICE/THIE ) )
          CALL OS( 'X=+(Y,C)', X=ICESTR, Y=ICESTR, C=FICE )
          CALL OS( 'X=-(Y,C)', X=ICESTR, Y=ICESTR, C=FICE_MAX )
        ENDIF
!
        CALL FRICTION_KHIONE(NPOIN,IFROT,GRAV,KARMAN,ICESTR,T1,H,U,V)
!
!       COMPUTATION WATER ELEVATION
        CALL OS( 'X=+(Y,C)', X=T3, Y=H, C=EPS )
!
! ~~>   EFFECT OF THE ICE COVER ON THE HYDRODYNAMICS
        DO I = 1,NPOIN
!         T1: ICE FRICTION COEFFICIENT
!         T2: TOTAL ICE THICKNESS
!         T3: WATER DEPTH
!
          SP_EAU = SQRT( U%R(I)**2 + V%R(I)**2 )
          IF( H%R(I).LT.EPS ) SP_EAU =
     &      MAX( SP_EAU, SQRT(GRAV*(EPS-H%R(I))*H%R(I)/EPS) )
!
! ~~>     UPDATE SOURCE TERM WITH ICE COVER FRICTION
          FU%R(I) = FU%R(I) - 0.5D0 * T1%R(I)*SP_EAU*U%R(I) / T3%R(I)
          FV%R(I) = FV%R(I) - 0.5D0 * T1%R(I)*SP_EAU*V%R(I) / T3%R(I)
!
! ~~>     LOCAL STATIC PRESSURE INCREASE DUE TO ICE THICKNESS (TODO)
!          IF( H%R(I).GT.EPS ) THEN
!            PATMOS%R(I) = PATMOS%R(I) + GRAV * RHO_ICE * T2%R(I)
!          ENDIF
        ENDDO
!
! ~~>   SEEPAGE (TODO)
!
      ENDIF
!
!=======================================================================
!
!     CLOGGING
!
!     NO IMPACT ON THE HYDRODYNAMICS
!
!     TODO: Implement CLOGGING=TRUE
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END
