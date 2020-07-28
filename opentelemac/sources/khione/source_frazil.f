!                   ************************
                    SUBROUTINE SOURCE_FRAZIL
!                   ************************
!
     &( NPOIN,TEXP,TIMP,TN,HPROP,U,V,T1,S,MESH,
     &  DT,AT,MARDAT,MARTIM,LAMBD0,CF,AK,EP,ITURB_TEL,GRAV )
!
!***********************************************************************
! KHIONE   V7P2                                             02/11/2016
!***********************************************************************
!
!brief    COMPUTES CONTRIBUTION TO TRACER SOURCE TERMS RESULTING
!+        FROM FRAZIL ICE PROCESSES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK         |-->| TURBULENT KINETIC ENERGY K AT TIME T(N+1)
!| AT         |-->| TIME IN SECONDS
!| DT         |-->| TIME STEP
!| EP         |-->| TURBULENT ENERGY DISSIPATION AT TIME T(N+1)
!| GRAV       |-->| GRAVITY CONSTANT
!| HPROP      |-->| PROPAGATION DEPTH
!| IND_T      |-->| TRACER INDEX FOR WATER TEMPERATURE
!| ITURB_TEL  |-->| T2D TURBULENCE MODEL
!| LAMBD0     |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| MARDAT     |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM     |-->| TIME (HOUR,MINUTE,SECOND)
!| MESH       |<->| MESH STRUCTURE
!| NPOIN      |-->| NUMBER OF NODES IN THE MESH
!| NTRAC      |-->| NUMBER OF TRACERS
!| S          |<->| VOID STRUCTURE
!| T1         |<->| WORKING ARRAY
!| TEXP       |<--| EXPLICIT SOURCE TERM.
!| TIMP       |<--| IMPLICIT SOURCE TERM.
!| TN         |-->| TRACERS AT TIME N
!| U          |-->| X COMPONENT OF THE VELOCITY
!| V          |-->| Y COMPONENT OF THE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY : THERMAL_BUDGET,BD_ICE,
     &                IND_FRA,IND_T,IND_S,NC_FRA,ICETYPE,ICELOC,
     &                ICETYPEP,SUMPH,PHCL,PHRI,PHPS,PHIB,PHIE,PHIH,PHIP,
     &                ANFEM,THETA0,THETA1,BETA1,VBB,THIFEM,THIFEMS,
     &                THIFEMF,HUN,TMELT,TCR,BCH,CP_EAU,RO0,RHO_ICE,
     &                LH_ICE,ITGM,KGM,EPSGM,ALPGM,NUTGM,VK_FRZL,
     &                SALINITY,INRJB,RK_FRZL,IND_PRE,VBK,PREC,NTOT,CTOT
      USE METEO_KHIONE, ONLY : SYNC_METEO,
     &                         WINDX,WINDY,TAIR,TDEW,CLDC,VISBI,PLUIE
      USE THERMAL_KHIONE, ONLY : THERMAL_FLUXES,ICOVER_GROWTH,
     &                           FRAZIL_HEAT_COEF
      USE FREEZEUP_KHIONE, ONLY : THERMAL_GROWTH,MELTING_POINT,
     &  SEEDING,SECONDARY_NUCLEATION,FLOCCULATION_BREAKUP,
     &  TURBULENT_PARAMETERS,EROSION_DEPOSITION,PRECIPITATION
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN,ITURB_TEL
      INTEGER, INTENT(IN)            :: MARDAT(3),MARTIM(3)
!
      DOUBLE PRECISION,INTENT(IN)    :: DT,AT,GRAV
!
      TYPE(BIEF_OBJ), INTENT(IN)     :: AK,EP
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TEXP,TIMP
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: T1,S
      TYPE(BIEF_MESH),INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: HPROP,U,V,CF
      DOUBLE PRECISION, INTENT(IN)   :: LAMBD0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL                     :: LIMFLAG,MINFLAG
      INTEGER                     :: I,IT,J,IL,ITP,K
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-2
      DOUBLE PRECISION            :: CONSTSS
      DOUBLE PRECISION            :: VMAG,WMAG
      DOUBLE PRECISION            :: SRCT,SRCF,SRCSR
      DOUBLE PRECISION            :: SRCGM_EXP(NC_FRA)
      DOUBLE PRECISION            :: SRCGM_IMP(NC_FRA)
      DOUBLE PRECISION            :: SRCSE(NC_FRA)
      DOUBLE PRECISION            :: SRCSN(NC_FRA)
      DOUBLE PRECISION            :: SRCFB(NC_FRA)
      DOUBLE PRECISION            :: SRCP
      DOUBLE PRECISION            :: SUM_FRZL,SUM_SRCGM
      DOUBLE PRECISION            :: B2,B3,FHC,HIN
!
      INTRINSIC MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
!=======================================================================
!
!     FOR GOOD MEASURE - SHOULD BE DONE AT THE TELEMAC-2D LEVEL
!
!-----------------------------------------------------------------------
!
!     NOTE THAT "AT" FROM DIFSOU IS AREADY TOO FAR GONE
      HIN = 0.D0
      CALL SYNC_METEO(AT-DT)
!
!=======================================================================
!
!     MELTING POINT
!
!-----------------------------------------------------------------------
!
      IF( IND_S.NE.0 ) THEN
        DO I = 1,NPOIN
          TMELT%R(I) = MELTING_POINT( TN%ADR(IND_S)%P%R(I) )
        ENDDO
      ENDIF
!
!=======================================================================
!
!     THERMAL BALANCE, SUPERCOOLING AND FRAZIL GROWTH
!
!-----------------------------------------------------------------------
!
      IF( THERMAL_BUDGET ) THEN
!
!       MAJORATED RADIATION
        CONSTSS = 1.D0/(RO0*CP_EAU)
!
        DO I = 1,NPOIN
!
! ~~>     DOES NOT APPLY HEAT EXCHANGES ON DRY BANKS
          IF( HPROP%R(I).GT.EPS ) THEN
!
! ~~>       WIND SPEED
            WMAG = SQRT( WINDX%R(I)**2 + WINDY%R(I)**2 )
! ~~>       FLOW SPEED
            VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!
!           ~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       ATMOSPHERIC HEAT FLUXES
!           ~~~~~~~~~~~~~~~~~~~~~~~
            CALL THERMAL_FLUXES(TAIR%R(I),TN%ADR(IND_T)%P%R(I),
     &        TMELT%R(I),SRCT,TDEW%R(I),CLDC%R(I),VISBI%R(I),WMAG,
     &        PLUIE%R(I),SUMPH%R(I),PHCL%R(I),PHRI%R(I),PHPS%R(I),
     &        PHIB%R(I),PHIE%R(I),PHIH%R(I),PHIP%R(I),CONSTSS,
     &        ANFEM%R(I),DT,AT,HPROP%R(I),MARDAT,MARTIM,LAMBD0)
!
! ~~>       SOURCE TERM FOR EXCHANGES BETWEEN AIR AND OPEN WATER
            IF( ICETYPE%I(I).EQ.1) THEN
              TEXP%ADR(IND_T)%P%R(I) = TEXP%ADR(IND_T)%P%R(I) + SRCT
            ENDIF
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       SUPER COOLING AND FRAZIL ICE EVOLUTION
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ~~>       TURBULENT PARAMETERS
            CALL TURBULENT_PARAMETERS(I,VMAG,HPROP%R(I),
     &        KGM%R(I),EPSGM%R(I),ALPGM%R(I),NUTGM%R(I),
     &        CF%R(I),AK,EP,ITURB_TEL)
!
! ~~>       SEEDING
            CALL SEEDING(I,TN,TMELT%R(I),SRCSE,MINFLAG)
!
! ~~>       COMPUTE THERMAL GROWTH/DECAY
            CALL THERMAL_GROWTH(I,TN,TMELT%R(I),DT,
     &        SRCGM_EXP,SRCGM_IMP,SUM_SRCGM,SUM_FRZL,
     &        EPSGM%R(I),ALPGM%R(I),NUTGM%R(I),CONSTSS,
     &        LIMFLAG)

! ~~>       COMPUTE SECONDARY NUCLEATION AND FLOCCULATION
            CALL SECONDARY_NUCLEATION(I,TN,DT,SRCSN,
     &        EPSGM%R(I),LIMFLAG,MINFLAG)
!
! ~~>       COMPUTE FLOCCULATION AND BREAKUP
            CALL FLOCCULATION_BREAKUP(I,TN,DT,SRCFB,
     &        LIMFLAG,MINFLAG)
!
! ~~>       EXPLICIT/IMPLICIT SOURCE TERM FOR FRAZIL CONC. K
            DO K=1,NC_FRA
              J = IND_FRA+K-1
              IF(ITGM.EQ.1) THEN
                TEXP%ADR(J)%P%R(I) = TEXP%ADR(J)%P%R(I)
     &                             + SRCGM_EXP(K) + SRCSE(K)
     &                             + SRCSN(K) + SRCFB(K)
              ELSE IF(ITGM.EQ.2) THEN
                TEXP%ADR(J)%P%R(I) = TEXP%ADR(J)%P%R(I)
     &                             + SRCGM_EXP(K) + SRCSE(K)
     &                             + SRCSN(K) + SRCFB(K)
                TIMP%ADR(J)%P%R(I) = TIMP%ADR(J)%P%R(I)
     &                             + SRCGM_IMP(K)*HPROP%R(I)
              ENDIF
!
!             POSITIVITY WARNING
              IF(TEXP%ADR(J)%P%R(I).LT.(-TN%ADR(J)%P%R(I)/DT))THEN
                WRITE(LU,*) ''
                WRITE(LU,*) 'POSITIVITY OF FRAZIL CONC. '
                WRITE(LU,*) 'MAY BE COMPROMISED '
                WRITE(LU,*) ''
                WRITE(LU,*) 'CHECK TIME STEP '
                WRITE(LU,*) 'OR CHECK MODEL PARAMETERS '
                WRITE(LU,*) ''
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDDO
!
! ~~>       EXPLICIT/IMPLICIT SOURCE TERM FOR TEMPERATURE
            IF (INRJB.EQ.1) THEN
              SRCT = SUM_SRCGM*RHO_ICE*LH_ICE*CONSTSS
            ELSE IF(INRJB.EQ.2) THEN
              SRCT = SUM_SRCGM*RHO_ICE*LH_ICE*CONSTSS/(1-SUM_FRZL)
     &             + SUM_SRCGM*RHO_ICE*(TN%ADR(IND_T)%P%R(I)-TMELT%R(I))
     &               /(RO0*(1-SUM_FRZL))
            ENDIF
!
            IF(ITGM.EQ.1) THEN
              TEXP%ADR(IND_T)%P%R(I) = TEXP%ADR(IND_T)%P%R(I) + SRCT
            ELSE IF(ITGM.EQ.2) THEN
              TEXP%ADR(IND_T)%P%R(I) = TEXP%ADR(IND_T)%P%R(I)
     &                               + SRCT*TMELT%R(I)
              TIMP%ADR(IND_T)%P%R(I) = TIMP%ADR(IND_T)%P%R(I)
     &                               - SRCT*HPROP%R(I)
            ENDIF
!
! ~~>       EXPLICIT SOURCE TERM FOR SALINITY (SALT REJECTION)
            IF (SALINITY) THEN
              SRCSR = RHO_ICE*TN%ADR(IND_S)%P%R(I)*SUM_SRCGM/RO0
              TEXP%ADR(IND_S)%P%R(I) = TEXP%ADR(IND_S)%P%R(I) + SRCSR
            ENDIF
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       FRAZIL ICE SINK/SOURCE TERM DUE TO MASS EXCHANGES
!           (DEPOSITION/EROSION) WITH ICE COVER
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!           TODO
            DO K=1,NC_FRA
              J = IND_FRA+K-1
              CALL EROSION_DEPOSITION(
     &          TN%ADR(J)%P%R(I),SRCF,
     &          THETA0%R(I),THETA1%R(I),BETA1%R(I),
     &          VBB%R(I),THIFEMF%R(I),HUN%R(I),
     &          ANFEM%R(I),VMAG,HPROP%R(I),.FALSE.)
!     &          ANFEM%R(I), VMAG,HPROP%R(I),(LIFCG%I(I).EQ.2))

! ~~>         SOURCE TERM FOR FRAZIL ICE
              TEXP%ADR(J)%P%R(I) = TEXP%ADR(J)%P%R(I) + SRCF
            ENDDO
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       FRAZIL PRECIPITATION SOURCE TERM
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF(PREC) THEN
              DO K=1,NC_FRA
                J = IND_FRA+K-1
                CALL PRECIPITATION(VBK(K),TN%ADR(J)%P%R(I),SRCP,VMAG,
     &                             GRAV,RK_FRZL(K),ICETYPE%I(I).GT.1)
!
! ~~>           SOURCE TERM FOR FRAZIL ICE
                TEXP%ADR(J)%P%R(I) = TEXP%ADR(J)%P%R(I) + SRCP
                TEXP%ADR(IND_PRE)%P%R(I) = TEXP%ADR(IND_PRE)%P%R(I) -
     &                                     (SRCP / HPROP%R(I))
              ENDDO
            ENDIF
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       THERMAL EXPANSION/DECAY OF THE ICE COVER
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF(ICETYPE%I(I).EQ.2) THEN
              FHC = 0.D0
              IF( TN%ADR(IND_T)%P%R(I).GT.0.D0 ) THEN
                FHC = FRAZIL_HEAT_COEF(
     &            2*HPROP%R(I),VMAG,TN%ADR(IND_T)%P%R(I),TMELT%R(I) )
              ENDIF
!
!             COMPUTING ICE COVER THERMAL GROWTH
              CALL ICOVER_GROWTH( TAIR%R(I),TN%ADR(IND_T)%P%R(I),
     &          TMELT%R(I),SUMPH%R(I),
     &          ANFEM%R(I),THIFEMS%R(I),
     &          FHC, DT )
!
!             COMPUTING TOTAL ICE THICKNESS
              THIFEM%R(I) = THIFEMS%R(I) + THIFEMF%R(I) + HUN%R(I)
            ENDIF
!
! ~~>       COMPLETE MELT DOWN
            IF( (THIFEMS%R(I)+THIFEMF%R(I)).LE.0.D0 .AND.
     &           ANFEM%R(I).LE.0.D0 ) THEN
              IT = ICETYPE%I(I)
              ITP = ICETYPEP%I(I)
              IL = ICELOC%I(I)
!
              IF( IT.EQ.2 ) ICETYPE%I(I) = 1
              IF( IL.EQ.3 ) ICELOC%I(I) = 1
              IF( ITP.EQ.2 ) ICETYPEP%I(I) = 1
!
            ENDIF
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       THERMAL EXCHANGES BETWEEN WATER AND ICE COVER
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       TODO: HEAT FLUXES BETWEEN WATER AND ICE
!!            IF( ANFEM%R(I).GT.0.5D0 ) THEN
!             CONSIDER ICE EFFECTS
!              DH = 2.D0*HPROP%R(I)
!            ELSE
!             DOES NOT CONSIDER ICE EFFECTS
!              DH = 4.D0*HPROP%R(I)
!            ENDIF
!            PHIW%R(I) = - ( TN%ADR(IND_T)%P%R(I)-TMELT%R(I) ) *
!     &        FRAZIL_HEAT_COEF( DH,VMAG,TN%ADR(IND_T)%P%R(I)-TMELT%R(I) )
!
!            TEXP%ADR(IND_T)%P%R(I) = TEXP%ADR(IND_T)%P%R(I) +
!     &        CONSTSS*PHIW%R(I)*ANFEM%R(I) / HPROP%R(I)
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
          ENDIF
!
        ENDDO
!
      ENDIF
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
!       AREA AROUND NODES
!       TODO: CAN BE REPLACED BY VOLU2DPAR
        CALL VECTOR( T1,'=','MASBAS          ',U%ELM,1.D0,
     &                S,S,S,S,S,S,MESH,.FALSE.,S )
!       /!\ TODO: PARALELISATION
!
        B2 = BCH - 5.87D0 * LOG(BCH)
!
        DO I = 1,NPOIN
! ~~>     WIND SPEED EFFECTS ON ICE
          WMAG = SQRT( WINDX%R(I)**2 + WINDY%R(I)**2 )
!
! ~~>     FLOW SPEED EFFECTS ON ICE
          VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!
! ~~>     CRITICAL AIR TEMPERATURE FOR BORDER ICE GROWTH
          B3 = B2 + 5.87D0 * LOG( MAX( BCH, 2.D0*SQRT(T1%R(I)) ) )
          TCR%R(I) = TN%ADR(IND_T)%P%R(I) +
     &       SUMPH%R(I) / ( 1130.D0 * VMAG + B3 * WMAG )
          IF( ( TCR%R(I).LT.0.3D0*TAIR%R(I) ).AND.
     &        ( TAIR%R(I).LT.0.0D0 ) ) THEN
            TCR%R(I) = 0.3D0 * TAIR%R(I)
          ENDIF
          IF( SUMPH%R(I).GT.0.D0 ) TCR%R(I) = TAIR%R(I)
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
