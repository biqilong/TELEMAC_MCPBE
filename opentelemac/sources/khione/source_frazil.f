!                    ************************
                     SUBROUTINE SOURCE_FRAZIL
!                    ************************
!
     &( NPOIN,TEXP,TN,HPROP,U,V, T1,S,MESH,
     &  DT,AT,MARDAT,MARTIM, LAMBD0 )
!
!***********************************************************************
! KHIONE   V7P2                                             02/11/2016
!***********************************************************************
!
!brief    COMPUTES CONTRIBUTION TO TRACER SOURCE TERMS RESULTING
!+        FROM ICE PROCESSES.
!+        IN PARTICULAR (DEPENDING ON ICEPROCESS):
!+          #2.- THERMAL BALANCE
!+          #3.- ...
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        19/11/2016
!+        V7P2
!+        INITIAL DEVELOPMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN      |-->| NUMBER OF NODES IN THE MESH
!| IND_T      |-->| TRACER INDEX FOR WATER TEMPERATURE
!| DT         |-->| TIME STEP
!| AT         |-->| TIME IN SECONDS
!| NTRAC      |-->| NUMBER OF TRACERS
!| TEXP       |<--| EXPLICIT SOURCE TERM.
!| TN         |-->| TRACERS AT TIME N
!| HPROP      |-->| PROPAGATION DEPTH
!| LAMBD0     |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL, ONLY : RO0,CP_EAU
      USE DECLARATIONS_KHIONE, ONLY : ICEPROCESS,IND_F,IND_T,ICETYPE,
     &                SUMPH, PHCL,PHRI,PHPS,PHIB,PHIE,PHIH,PHIP,
     &                ANFEM,THETA0,THETA1,BETA1,VBB,THIFEMS,THIFEMF,HUN,
     &                SURF_EF,TCR
      USE METEO_KHIONE,        ONLY : SYNC_METEO,
     &                            WINDX,WINDY,TAIR,TDEW,CLDC,VISBI,PLUIE
      USE THERMAL_KHIONE,      ONLY : THERMAL_FLUXES,ICOVER_GROWTH,
     &                                FRAZIL_HEAT_COEF
      USE FREEZEUP_KHIONE,     ONLY : SUPER_COOLING
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN
      INTEGER, INTENT(IN)            :: MARDAT(3),MARTIM(3)
!
      DOUBLE PRECISION,INTENT(IN)    :: DT,AT
!
      TYPE(BIEF_OBJ), INTENT(IN)     :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TEXP
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: T1,S
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: HPROP,U,V
      DOUBLE PRECISION, INTENT(IN)   :: LAMBD0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                     :: I,IT
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-2
      DOUBLE PRECISION            :: CONSTSS
      DOUBLE PRECISION            :: VMAG,WMAG, SRCT,SRCF
      DOUBLE PRECISION            :: B1,B2,B3, FHC,HIN
!
      INTRINSIC MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! ICEPROCESS:
!    PRIME NUMBER DEFINING WHICH PROCESS IS SWITCHED ON:
!    - 2: THERMAL BALANCE
!    - 3: IMPACT OF THE ICE COVER ON THE HYDRODYNAMICS
!    - 5: CLOGGING ON RACKS
!    - 0: ALL PROCESSES ABOVE BECAUSE N*INT(0/N) = 0
!    - 1: NONE OF THE PROCESSES BECAUSE N*INT(1/N) <> 1
!
!-----------------------------------------------------------------------
!
!=======================================================================
!
!     1 - FOR GOOD MEASURE - SHOULD BE DONE AT THE TELEMAC-2D LEVEL
!
!-----------------------------------------------------------------------
!
!     NOTE THAT "AT" FROM DIFSOU IS AREADY TOO FAR GONE
      CALL SYNC_METEO(AT-DT)
!
!
!=======================================================================
!
!     2 - THERMAL BALANCE, SUPERCOOLING AND FRAZIL GROWTH
!
!-----------------------------------------------------------------------
!
      IF( INT(ICEPROCESS/2)*2 .EQ. ICEPROCESS ) THEN
!
!       MAJORATED RADIATION
        CONSTSS = 1.D0/(RO0*CP_EAU)
!
        DO I = 1,NPOIN
!
! ~~>     DOES NOT APPLY HEAT EXCHANGES ON DRY BANKS
          IF( HPROP%R(I).GT.EPS ) THEN
!
! ~~>       WIND SPEED EFFECTS ON ICE
            WMAG = SQRT( WINDX%R(I)**2 + WINDY%R(I)**2 )
! ~~>       FLOW SPEED EFFECTS ON ICE
            VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!
! ~~>       ATMOSPHERIC HEAT FLUXES
            CALL THERMAL_FLUXES( TAIR%R(I),TN%ADR(IND_T)%P%R(I),
     &        SRCT,
     &        TDEW%R(I),CLDC%R(I),VISBI%R(I),WMAG,PLUIE%R(I),
     &        SUMPH%R(I), PHCL%R(I),PHRI%R(I),
     &        PHPS%R(I),PHIB%R(I),PHIE%R(I),PHIH%R(I),PHIP%R(I),
     &        CONSTSS,ANFEM%R(I),DT,AT,HPROP%R(I),MARDAT,MARTIM,
     &        LAMBD0 )
!
! ~~>       SOURCE TERM FOR EXCHANGES BETWEEN AIR AND OPEN WATER
            IF( ICETYPE%I(I).LE.2 ) THEN
              TEXP%ADR(IND_T)%P%R(I) = TEXP%ADR(IND_T)%P%R(I) + SRCT
            ENDIF
!
! ~~>       SUPERCOOLING AND FRAZIL ICE GROWTH
            CALL SUPER_COOLING(
     &        TN%ADR(IND_T)%P%R(I),TN%ADR(IND_F)%P%R(I),
     &        SRCT,SRCF,
     &        THETA0%R(I),THETA1%R(I),BETA1%R(I),
     &        VBB%R(I),THIFEMF%R(I),HUN%R(I),
     &        CONSTSS,ANFEM%R(I), DT,VMAG,HPROP%R(I),.FALSE.  )
!     &        ANFEM%R(I), DT,VMAG,HPROP%R(I),(LIFCG%I(I).EQ.2)  )
!
! ~~>       SOURCE TERM WITHIN THE WATER COLUMN
            TEXP%ADR(IND_T)%P%R(I) = TEXP%ADR(IND_T)%P%R(I) + SRCT
            TEXP%ADR(IND_F)%P%R(I) = TEXP%ADR(IND_F)%P%R(I) + SRCF
!
! ~~>       SOURCE TERM FOR EXCHANGES WITH THE ICE COVER
            IF( ICETYPE%I(I).GT.2 ) THEN
              FHC = 0.D0
              IF( TN%ADR(IND_T)%P%R(I).GT.0.D0 ) THEN
                FHC = FRAZIL_HEAT_COEF(
     &            2*HPROP%R(I),VMAG,TN%ADR(IND_T)%P%R(I) )
              ENDIF
!
              CALL ICOVER_GROWTH( TAIR%R(I),TN%ADR(IND_T)%P%R(I),
     &          SUMPH%R(I), ANFEM%R(I),THIFEMS%R(I),THIFEMF%R(I),
     &          SURF_EF,(0.D0), HIN,FHC, DT )
!
            ENDIF
!
! ~~>       COMPLETE MELT DOWN
            IF( (THIFEMS%R(I)+THIFEMF%R(I)).LE.0.D0 .AND.
     &           ANFEM%R(I).LE.0.D0 ) THEN
              IT = ICETYPE%I(I)
!
              IF( INT(IT/3)*3 .EQ. IT ) ICETYPE%I(I) = ICETYPE%I(I) / 3
              IF( INT(IT/5)*5 .EQ. IT ) ICETYPE%I(I) = ICETYPE%I(I) / 5
              IF( INT(IT/7)*7 .EQ. IT ) ICETYPE%I(I) = ICETYPE%I(I) / 7
!
            ENDIF
!
! ~~>       TODO: HEAT FLUXES BETWEEN WATER AND ICE
!
          ELSE
!
! ~~>       ATMOSPHERIC HEAT FLUXES
            PHCL%R(I) = 0.D0
            PHRI%R(I) = 0.D0
            PHPS%R(I) = 0.D0
            PHIB%R(I) = 0.D0
            PHIE%R(I) = 0.D0
            PHIH%R(I) = 0.D0
            PHIP%R(I) = 0.D0
!
            SUMPH%R(I) = 0.D0
!
! ~~>       SUPERCOOLING AND ICE GROWTH

          ENDIF
!
        ENDDO
!
      ENDIF
!
!=======================================================================
!
!     3 - ICE COVER
!
!-----------------------------------------------------------------------
!
!      IF( INT(ICEPROCESS/3)*3 .EQ. ICEPROCESS ) THEN
!      ENDIF
!
!=======================================================================
!
!     5 - CLOGGING
!
!-----------------------------------------------------------------------
!
!      IF( INT(ICEPROCESS/5)*5 .EQ. ICEPROCESS ) THEN
!      ENDIF
!
!=======================================================================
!
!     7 - STATIC BORDER ICE GROWTH
!
!-----------------------------------------------------------------------
!
      IF( INT(ICEPROCESS/7)*7 .EQ. ICEPROCESS ) THEN
!
!-----------------------------------------------------------------------
!       PREPARATION TO STATIC BORDER ICE GROWTH
!
!       TODO: CAN BE REPLACED BY VOLU2DPAR
!
!       AREA AROUND NODES
        CALL VECTOR( T1,'=','MASBAS          ',U%ELM,1.D0,
     &                S,S,S,S,S,S,MESH,.FALSE.,S )
!       /!\ TODO: PARALELISATION
!
        B1 = 15.D0              ! /!\ TODO: USER CALIBRATION PARAMETER
        B2 = B1 - 5.87D0 * LOG(B1)
        DO I = 1,NPOIN
!
! ~~>     WIND SPEED EFFECTS ON ICE
          WMAG = SQRT( WINDX%R(I)**2 + WINDY%R(I)**2 )
! ~~>     FLOW SPEED EFFECTS ON ICE
          VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!
! ~~> CRITICAL AIR TEMPERATURE FOR BORDER ICE GROWTH
          B3 = B2 + 5.87D0 * LOG( MAX( B1, 2.D0*SQRT(T1%R(I)) ) )
          TCR%R(I) = TN%ADR(IND_T)%P%R(I) +
     &       SUMPH%R(I) / ( 1130.D0 * VMAG + B3 * WMAG )
          IF( ( TCR%R(I).LT.0.3D0*TAIR%R(I) ).AND.
     &        ( TAIR%R(I).LT.0.0D0 ) ) THEN
            TCR%R(I) = 0.3D0 * TAIR%R(I)
          ENDIF
          IF( SUMPH%R(I).GT.0.D0 ) TCR%R(I) = TAIR%R(I)
!         TODO: BUOYANT VELOCITY
!
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
      END
