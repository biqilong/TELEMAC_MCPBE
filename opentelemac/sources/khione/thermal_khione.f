!                     *********************
                      MODULE THERMAL_KHIONE
!                     *********************
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Module containing all subroutines to deal with the physics
!+        of thermal exchanges, at a node level.
!+        To be joined up with THERMAL_WAQTEL.
!
!history  F. SOUILLE (EDF)
!+        30/09/2019
!+        V8P0
!+        Added coefficients for full heat budget
!+        Added freezing temperature
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: THERMAL_FLUXES,FRAZIL_HEAT_COEF,ICOVER_GROWTH,LEAP,
     &          DAYNUM
!
!=======================================================================
!
!       1) THERMAL FLUXES
!
      CONTAINS
!
!=======================================================================
!
!                   *************************
                    SUBROUTINE THERMAL_FLUXES
!                   *************************
!
     &(TAIR,TWAT,TFRZ,SRCT,TDEW,CC,VISB,WIND,PLUIE,SUMPH,PHCL,PHRI,PHPS,
     & PHIB,PHIE,PHIH,PHIP,CONSTSS,ANFEM,DT,AT,DEPTH,MARDAT,MARTIM,
     & LAMBD0)
!
!***********************************************************************
! RICE-2D    V7P2                                          11/11/2016
!***********************************************************************
!
!brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ANFEM   |-->| CONCENTRATION OF SURFACE ICE PARTICLES
!| AT      |-->| CURRENT TIME
!| CONSTSS |-->| MAJORATED RADIATION
!| CC      |-->| CLOUD COVER
!| DN      |-->| CURRENT TIME
!| DT      |-->| TIME STEP
!| DEPTH   |-->| PROPAGATION DEPTH
!| LAMBD0  |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| MARDAT  |-->| DATE (YEAR, MONTH,DAY)
!| MARTIM  |-->| TIME (HOUR, MINUTE,SECOND)
!| PHCL    |<->| SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLEAR SKY
!| PHIB    |<->| EFFECTIVE BACK RADIATION (OPEN WATER OR ICE)
!| PHIE    |<->| EVAPORATIVE HEAT TRANSFER
!| PHIH    |<->| CONVECTIVE HEAT TRANSFER
!| PHIP    |<->| HEAT TRANSFER DUE TO PRECIPITATION
!| PHPS    |<->| NET SOLAR RADIATION (FLUX) AFTER REFLEXION
!| PHRI    |<->| SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLOUDY SKY
!| PLUIE   |-->| RAIN
!| SRCT    |<->| SOURCE TERM FOR EXCHANGES BETWEEN AIR AND OPEN WATER
!| SUMPH   |<->| NET SUM OF ALL THERMAL FLUXES
!| T1      |-->| STARTING HOUR FOR SOLAR RADIATION CALCULATION (HRS)
!| T2      |-->| ENDING HOUR FOR SOLAR RADIATION CALCULATION (HRS)
!| TAIR    |-->| AIR TEMPERATURE
!| TDEW    |-->| DEWPOINT TEMPERATURE
!| TFRZ    |-->| FREEZING TEMPERATURE
!| TWAT    |-->| WATER TEMPERATURE
!| VISB    |-->| VISIBILITY
!| WIND    |-->| WIND SPEED EFFECT ON ICE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE,      ONLY: LH_ICE,CP_ICE,
     &  LIN_WATAIR,CST_WATAIR,LIN_ICEAIR,CST_ICEAIR,
     &  COEF_PHIB, COEF_PHIE, COEF_PHIH, COEF_PHIP, SGMA,
     &  CP_EAU, ATMOEXCH
      USE METEO_KHIONE,             ONLY: WINDZ
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: MARDAT(3),MARTIM(3)
!
      DOUBLE PRECISION, INTENT(IN)    :: TWAT,CONSTSS,ANFEM,TFRZ
      DOUBLE PRECISION, INTENT(IN)    :: TAIR,TDEW,CC,VISB,WIND,PLUIE
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT,DEPTH
      DOUBLE PRECISION, INTENT(INOUT) :: SUMPH,SRCT,PHCL,PHRI
      DOUBLE PRECISION, INTENT(INOUT) :: PHPS,PHIB,PHIE,PHIH,PHIP
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DAY NUMBER, ORBITAL CORRECTION
      INTEGER IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC
      INTEGER NDLT, CICE,I
!
      DOUBLE PRECISION DN, DAY,DAYREEL,NDAYS,DTHR
!
      DOUBLE PRECISION PHBA,PHBC,PHBR,PHBW
      DOUBLE PRECISION ES1,EA1,EPINA,AKN,VA,ASV
      DOUBLE PRECISION PCL,PRI,PPS,TAK,TSK,TDK
      DOUBLE PRECISION T1,T2,T10,T20
!
!-----------------------------------------------------------------------
!
      IYEAR  = MARDAT(1)
      IMONTH = MARDAT(2)
      IDAY   = MARDAT(3)
      IHOUR  = MARTIM(1)
      IMIN   = MARTIM(2)
      ISEC   = MARTIM(3)
!
! ~~> OPEN WATER CONDITIONS
      CICE = 0
! ~~> ICE COVER CONDITIONS
      IF( ANFEM.GT.0.5D0 ) CICE = 1
!
!-----------------------------------------------------------------------
!
!     PHCL: SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLEAR SKY
!     PHRI: SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLOUDY SKY
!     PHPS: NET SOLAR RAD (FLUX) AFTER REFLECTION
!     PHIE: EVAPORATIVE HEAT TRANSFER
!     PHIH: CONVECTIVE HEAT TRANSFER
!     PHIP: HEAT TRANSFER DUE TO PRECIPITATION
      SUMPH = 0.D0
!
!-----------------------------------------------------------------------
!
!     DAY NUMBER, ORBITAL CORRECTION
      DAY = DAYNUM(IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC)
     &    + FLOOR(AT/86400.D0)            ! DAY FROM JAN.1
      NDAYS = 365.D0 + REAL(LEAP(IYEAR))
      DAYREEL = MODULO(DAY, NDAYS)        ! DAY NUMBER IN DATE
!
      DN = DAY       ! INT # OF DAYS FROM JAN 1 = DAYREEL
      DTHR = DT / 3600.D0

      PHCL = 0.D0
      PHRI = 0.D0
      PHPS = 0.D0

      PHIB = 0.D0
      PHIE = 0.D0
      PHIH = 0.D0
      PHIP = 0.D0

!     MSEC = NDAYS * 86400 + IHOUR * 3600 + IMIN * 60 + ISEC
!
!     T10 = FRACTION # OF HOURS FROM 0:00 @ CURRENT TIME (TSUM1)
!     T20 = FRACTION # OF HOURS FROM 0:00 AFTER DTHR
!
      T10 = IHOUR + MODULO(AT,86400.D0)/3600.D0
      T20 = T10 + DTHR
      T1  = T10
!
      IF (DTHR.GE.1.D0) THEN  ! IF TIME INTERVAL > 1.0 HR (RARE)
        NDLT = INT(DTHR + 0.001D0)
        DO I = 1,NDLT
          T2 = T1 + 1.D0
          IF (T2.GT.T20) THEN
            CALL SOLAR(DN,T1,T20,CC,PCL,PRI,PPS,CICE,LAMBD0)
            PHCL = PHCL + PCL  ! ADD FRACTION OF HOUR LEFT
            PHRI = PHRI + PRI
            PHPS = PHPS + PPS
            EXIT
          ELSE
            CALL SOLAR(DN,T1,T2,CC,PCL,PRI,PPS,CICE,LAMBD0)
            PHCL = PHCL + PCL  ! ADD HOUR INCREMENTS
            PHRI = PHRI + PRI
            PHPS = PHPS + PPS
          ENDIF
          T1 = T2
        ENDDO
      ELSE
        CALL SOLAR(DN,T10,T20,CC,PCL,PRI,PPS,CICE,LAMBD0)
        PHCL = PHCL + PCL  ! IF TIME INTERVAL A FRACTION OF AN HOUR
        PHRI = PHRI + PRI
        PHPS = PHPS + PPS
      ENDIF
!
      PHPS = PHPS / DTHR  ! NET SOLAR RAD AT SURFACE, W/M^2
!
!-----------------------------------------------------------------------
!
!     LINEAR HEAT TRANSFER FOR AIR-WATER INTERFACE ONLY
!
      IF( ATMOEXCH.EQ.3 ) THEN
        IF(CICE.EQ.1) THEN  ! ICE
          PHIH = - CST_ICEAIR - ( TAIR-TFRZ )*LIN_ICEAIR
        ELSE              ! OPEN WATER
          PHIH = - CST_WATAIR - ( TAIR-TWAT )*LIN_WATAIR
        ENDIF
        SUMPH = PHPS - PHIH
!
!-----------------------------------------------------------------------
!
!     FULL BUDGET FOR AIR-WATER INTERFACE
!
      ELSEIF( ATMOEXCH.EQ.4 ) THEN
!       FBH 2016-11
        TAK = TAIR + 273.16  ! TAK = AIR TEMPERATURE
        TSK = TWAT + 273.16  ! TSK = WATER TEMP
        TDK = TDEW + 273.16  ! TDK = DEW POINT
!
        IF (CICE.EQ.0) THEN  ! OPEN WATER ONLY, NO ICE EFFECTS
!
!  FOR WATER SURFACE, ES1 FOUND USING WATER TEMP (TSK), (4.32)
          ES1 = 7.95357242E+10*EXP((-18.1972839*373.16/TSK)+
     &          5.02808*LOG(373.16/TSK)-20242.1852*
     &          EXP(-26.1205253*TSK/373.16)+
     &          58.0691913*EXP(-8.039282*373.16/TSK))
          EA1 = 7.95357242E+10*EXP((-18.1972839*373.16/TDK)+
     &          5.02808*LOG(373.16/TDK)-20242.1852*
     &          EXP(-26.1205253*TDK/373.16)+
     &          58.0691913*EXP(-8.039282*373.16/TDK))
        ELSE
!
!  FOR ICE SURFACE, ES1 FOUND USING AIR TEMP (TAK), (4.33)
!  USUALLY CONSIDERED WHEN ANFEM(I) > 0.5, MORE ICE THAN WATER AT SURFACE
          ES1 = 5.75185606E+10*EXP((-20.947031*273.16/TAK)-
     &          3.56654*LOG(273.16/TAK)-2.01889049/273.16*TAK)
          EA1 = 5.75185606E+10*EXP((-20.947031*273.16/TDK)-
     &        3.56654*LOG(273.16/TDK)-2.01889049/273.16*TDK)
        ENDIF
!
!  EMISSIVITY OF ATMOSPHERE = PHBA (INCL. EFFECTS OF CLOUDS)
        IF (TAIR.LT.0.D0) THEN
          ! (4.31) SATTERLUND (1979)
          EPINA = 1.08D0 * (1.D0 - EXP(-EA1 ** (TAK / 2016.D0)))
        ELSE
          ! TK 03-2010 IDSO-JACKSON(1969)
          EPINA = 1-0.261D0*EXP(-0.000777*(273.16-TAK)**2)
        ENDIF
        ! (4.30)
        PHBC = EPINA * SGMA * TAK ** 4
        ! = ATMOSPHERIC RADIATION UNDER CLOUDY SKY (TELEMAC 2D)
        PHBA = PHBC * (1.D0 + 0.0017D0 * CC **2)
!  EMISSIVITY OF RIVER SURFACE = PHBW
        ! (4.29), TSK = WATER TEMP (ALWAYS)
        PHBW = 0.97D0 * SGMA * TSK ** 4

!  REFLECTED LONG WAVE RADIATION = PHBR
        ! (4.36)
        PHBR = 0.03D0 * PHBA
! EFFECTIVE BACK RADIATION
        PHIB = PHBR + PHBW - PHBA
        PHIB = COEF_PHIB*PHIB

!  EVAPORATIVE HEAT FLUX AND CONVECTIVE HEAT FLUX
        AKN = 8.D0 + 0.35D0 * (TWAT - TAIR)
        VA = (2.D0/WINDZ) ** 0.15 * WIND  !
!     WINDZ = HEIGHT WIND VELOCITY IS MEASURED (*.PAR)

!   EVAPORATIVE AND CONDUCTIVE HEAT TRANSFER ONLY OCCURS OVER WATER
!   HEAT TRANSFER BETWEEN ICE AND WATER IS CALCULATED USING HIW (FROM FHC)
        !   EVAPORATION
        PHIE = (1.56D0*AKN + 6.08D0 * VA) * (ES1-EA1)*4.1855D0/8.64D0
        PHIE = COEF_PHIE*PHIE
        !  CONDUCTIVE HEAT TRANSFER
        PHIH = (AKN + 3.9D0*VA) * (TSK-TAK)*4.1855D0/8.64D0
        PHIH = COEF_PHIH*PHIH

!  HEAT TRANSFER DUE TO PRECIPITATION

        IF (VISB.GT.0.D0.AND.VISB.LT.1.D0) THEN
!         MIN VALUE = 1.0 KM, PHIP = 380.28 W/M^2
          ASV = 78.5D0 / 86400.D0
        ELSE
!         AT VISB = 10 KM, PHIP = 1.604 W/M^2
          ASV = 78.5D0 / 86400.D0 * VISB ** (-2.375)
        ENDIF
!
! ~~>   NO SNOW
        IF(VISB.LT.1E-5) THEN ! NO SNOW
          ASV = 0.D0
        ENDIF
! ~~>   SNOW FALL
        IF(CICE.EQ.0) THEN
          PHIP = ASV * (LH_ICE + CP_ICE * (TWAT - TAIR))
        ELSE
          PHIP = 0.D0
        ENDIF
!
! ~~> RAIN FALL
        IF(PLUIE.GT.0.D0) THEN
          ASV = PLUIE/3600.D0
          IF(CICE.EQ.0) THEN   ! WATER
            IF(TAIR.LT.0.D0) THEN
              PHIP = - ASV*CP_EAU*(TWAT-0.0)  ! HEAT LOSS
            ELSE
              PHIP = - ASV*CP_EAU*(TWAT - TAIR) ! HEAT GAIN
            ENDIF
          ELSE              ! ICE
            IF(TAIR.GT.0) THEN
              PHIP = - ASV*CP_EAU*(0.D0 - TAIR) ! HEAT GAIN
            ELSE
              PHIP = 0.D0
            ENDIF
          ENDIF
        ENDIF
        PHIP = COEF_PHIP*PHIP
!
!    SUMMATION AND OUTPUT /!\ this is now done outside in SOURCE_RICED2D
        SUMPH = PHPS - PHIB - PHIE - PHIH - PHIP
!
!  OUTPUT, DETAILED THERMAL BUDGET AT RIVER SURFACE, + = HEAT LOSS
!  NET SOLAR RAD (PHPS) ASSUMED TO BE ABSORBED BY WATER COLUMN (HEAT GAIN)
!     RAY3 = PHPS    SOLAR RADIATION
!     RA = - PHBR + PHBA = 0.97*PHBA

!     RE = PHBW
!     CV = PHIH
!     CE = PHIE
!     RAY3+RA-RE-CV-CE
!
      ENDIF
!
!     EXPLICITE SOURCE TERM - HOW ABOUT IMPLICITE ?
!
      SRCT = CONSTSS*SUMPH*( 1.D0-ANFEM ) / DEPTH
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE THERMAL_FLUXES
!
!=======================================================================
!
!                   ************************
                    SUBROUTINE ICOVER_GROWTH
!                   ************************
!
     &(TAIR,TWAT,TMELT,SUMPH,ANFEM,THIFEMS,FHC,DT)
!
!***********************************************************************
! RICE-2D    V7P3                                          11/11/2016
!***********************************************************************
!
!brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ANFEM   |<->| CONCENTRATION OF SURFACE ICE PARTICLES
!| DT      |-->| TIME STEP
!| FHC     |-->| FRAZIL HEAT COEFFICIENT
!| SUMPH   |-->| NET HEAT EXCHANGE FLUX WITH THE ATMOSPHERE
!| TAIR    |-->| AIR TEMPERATURE
!| THIFEMS |<->| SOLID ICE THICKNESS
!| TMELT   |-->| FREEZING POINT OF WATER
!| TWAT    |-->| WATER TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY: RHO_ICE,LH_ICE,TC_BI,LIN_ICEAIR
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: FHC
      DOUBLE PRECISION, INTENT(IN)    :: TAIR,TWAT,TMELT,SUMPH
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: ANFEM,THIFEMS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      DOUBLE PRECISION B1,B3, DH
!
!-----------------------------------------------------------------------
!
!     PHIH = - CST_ICEAIR - ( TAIR-TFRZ )*LIN_ICEAIR
!     PHIH = - CST_WATAIR - ( TAIR-TWAT )*LIN_WATAIR
!     SUMPH = PHPS + CST_ICEAIR + ( TAIR-TFRZ )*LIN_ICEAIR
!     GROWTH OF ICE COVER DUE TO COLD SURFACE AIR TEMPERATURE
!
      IF( TAIR.LT.TMELT ) THEN
!
        DH = MAX( 0.D0, - SUMPH * ( DT/RHO_ICE/LH_ICE ) /
     &    ( THIFEMS*LIN_ICEAIR/TC_BI + 1.D0  ) )
        THIFEMS = THIFEMS + DH
!
      ENDIF
!-----------------------------------------------------------------------
!
!     MELTING OF ICE COVER DUE TO WARM SURFACE AIR TEMPERATURE
!
      IF( TAIR.GT.TMELT ) THEN
        DH = MIN( 0.D0, - SUMPH * DT /( RHO_ICE*LH_ICE ) )
        B1 = THIFEMS
        IF( B1.GT.ABS(DH) ) THEN
          THIFEMS = THIFEMS + DH
        ELSE
          THIFEMS = 0.D0
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     MELTING OF ICE COVER DUE TO WARM UNDER WATER TEMPERATURE
!
      IF( TWAT.GT.TMELT ) THEN
        DH = FHC * ( TWAT-TMELT ) * DT / ( RHO_ICE * LH_ICE )
        B3 = THIFEMS
        IF( DH.LT.B3 ) THEN
          THIFEMS = THIFEMS - DH
        ELSE
          THIFEMS = 0.0
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NOT ALLOWING NEGATIVE ICE COVER THICKNESS
!
      IF( THIFEMS.LE.0.D0 ) THIFEMS = 0.D0
      IF( THIFEMS.LE.0.D0 ) ANFEM = 0.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ICOVER_GROWTH
!
!=======================================================================
!
!                     ****************
                      SUBROUTINE SOLAR
!                     ****************
     &(DN,T1,T2,CC,PHCL,PHRI,PHPS,CICE,LAMBD0)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Same as SOLRAD within EXCHANGE_WITH_ATMOSPHERE (WAQTEL)
!+        TODO: Combined with SOLAR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CC      |-->| CLOUD COVER, IN THENTHS, 0-10 (/!\ NOT IN OCTAS)
!| CICE    |-->| ICE CONDITION, OPEN WATER : 0 ; ICE : 1
!| DN      |-->| CURRENT TIME (THE NUMBER OF DAYS FROM JAN. 1)
!| T1      |-->| STARTING HOUR FOR SOLAR RADIATION CALCULATION (HRS)
!| T2      |-->| ENDING HOUR FOR SOLAR RADIATION CALCULATION (HRS)
!| PHCL    |<->| SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLEAR SKY
!| PHPS    |<->| NET SOLAR RADIATION (FLUX) AFTER REFLEXION
!| PHRI    |<->| SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLOUDY SKY
!| LAMBD0  |-->| LATITUDE OF ORIGIN POINT (NORTH +, SOUTH -, DEGREES)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY: ALSM,ALLM,ETADIR,SIO,ALBE
      USE METEO_KHIONE,        ONLY: MODELZ,ALPHSD,ALPHRD
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: CICE
      DOUBLE PRECISION, INTENT(IN)    :: CC
      DOUBLE PRECISION, INTENT(IN)    :: DN,T1,T2
      DOUBLE PRECISION, INTENT(INOUT) :: PHCL,PHRI,PHPS
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: PI
      DOUBLE PRECISION :: AA, ALPHA,ALPHR,ALPHS,ALHA
      DOUBLE PRECISION R,BB,AMO,AM,CR,DELTA,EO,ET,HRIS,HSS,
     &  PAPO,PHISO,PHRR,PHS,RHS1,RHS2,SIA,ALR,HS1,HS2,HSET
!
!-----------------------------------------------------------------------
!
! DELTA = SOLAR DECLINATION OF THE SUN;
! HSS = LOCAL HOUR ANGLE OF THE SUNSET, RADIANS;
! HSR = LOCAL HOUR ANGLE OF THE SUNRISE, RADIANS;
! ALMST = LOCAL MEAN SOLAR TIME IN HRS;
! ALST = STANDARD TIME OF THE TIME ZONE, IN HRS, COUNTED FROM MIDNIGHT, 0-24.0;
! ALSM = LONGITUDE OF THE STANDARD MERIDEAN, IN DEGREE; (*.PAR FILE)
! ALLM = LONGITUDE OF THE LOCAL MERIDEAN, IN DEGREES; (*.PAR FILE)
! ETA = -1, FOR WEST; + FOR EAST; (*.PAR FILE)
! MODELZ = ELEVATION ABOVE SEA LEVEL, M. (*.PAR FILE)
! ALPHSD = SUN EXIT ANGLE, 180 FOR HORIZONTAL; (*.PAR FILE)
! ALPHRD = SUN EMISION ANGLE, 0 DEGREE FOR HORIZONTAL; (*.PAR FILE)
! NM = NUMBER OF MONTH
! ND = DAY NUMBER IN THE DATE
!
!-----------------------------------------------------------------------
!
      PHCL = 0.D0
      PHRI = 0.D0
      PHPS = 0.D0
!
      PI  = 4.D0*ATAN(1.D0)

!  LOCAL GEOGRAPHIC LATITUDE, CONVERT TO RADIANS
      PHS = LAMBD0 * PI / 180.  ! LATITUDE
      ALPHS = ALPHSD * PI / 180.  ! EXIT ANGLE, RADIANS
      ALPHR = ALPHRD * PI / 180.  ! EMISSION ANGLE, RADIANS

!  COOPER 1969 SOLAR DECLINATION, IN RADIANS  (4.6)
      DELTA = 23.45 * PI / 180. * SIN(360.*(284.+DN) / 365. * PI / 180.)

!  DIFFERENCE BETWEEN TRUE SOLAR TIME AND MEAN SOLAR TIME
      ! (4.3)
      R = 2. * PI * (DN - 1.) / 365.
!  DUFFIE AND BECKMAN 1959, ECCENTRICITY CORRECTION FACTION OF THE EARTH ORBIT
      ! (4.5)
      EO = 1. + 0.033 * COS(2. * PI / 365. * DN)
!  EQUATION OF TIME, IN HRS  (4.2)
      ET = 3.8197 * (0.000075 + 0.001868 * COS(R) - 0.032077 * SIN(R)
     &     -0.014615 * COS(2. * R) - 0.04089 * SIN(2. * R))

!  HOUR ANGLE AT SUNRISE, RADIANS  (4.9)
      HSS = ACOS( -TAN(PHS) * TAN(DELTA) )
      IF (HSS.LT.0.0) THEN
        HSS=-HSS
      ENDIF
!  SUN SET, HRS  (4.11)
      HSET = 12. + HSS * 12. / PI - (PI - ALPHS) / PI * 12.0
!  SUN RISE, HRS  (4.10)
      HRIS = 12. - HSS * 12. / PI + ALPHR / PI * 12.0
!  CALCULATE HOUR ANGLE, TIME CORRECTION  (4.4)
      HS1 = T1 - ETADIR / 15. * (ALSM - ALLM) + ET  ! HOURS
      RHS1 = (12. - HS1) * PI / 12.   ! RADIANS
      HS2 = T2 - ETADIR / 15. * (ALSM - ALLM) + ET  ! HOURS
      RHS2 = (12. - HS2) * PI / 12.   ! RADIANS
!  RHS1/RHS2 IS # OF RADIANS OFF OF NOON, + BEFORE, - AFTER

! =========NET SOLAR RADIATION, PHPS

      PHCL = 0.0D0  ! CLEAR SKY SOLAR RADIATION
      PHRI = 0.0D0  ! INCL. CLOUD EFFECTS, IF ANY
      PHPS = 0.0D0  ! NET SOLAR, AFTER REFLECTION AT EARTH SURFACE

!  NO SUN, BEFORE SUNRISE OR AFTER SUNSET
      IF (HS1.GT.HSET.AND.HS2.GT.HSET) THEN
        RETURN
      ENDIF
      IF (HS1.LT.HRIS.AND.HS2.LT.HRIS) THEN
        RETURN
      ENDIF

      IF (HS1.LT.HRIS .AND. HS2.GT.HRIS) THEN
        RHS1=(12.D0-HRIS)*PI/12.D0
        HS1=HRIS
      ENDIF
      IF (HS2.GT.HSET.AND.HS1.LT.HSET) THEN
        RHS2=(12.0-HSET)*PI/12.D0
        HS2=HSET
      ENDIF

!  AVERAGE SOLAR TIME ANGLE
      ALHA = (RHS1 + RHS2) / 2.D0  ! RADIANS
!  TOTAL SOLAR RADIATION T1 TO T2, PER UNIT AREA, (4.13)-RADIANS, (4.14)-HOURS
      PHISO = 12.D0/PI*SIO*EO*((RHS1-RHS2)*SIN(DELTA)*SIN(PHS)+
     &        (SIN(RHS1)-SIN(RHS2))*COS(DELTA)*COS(PHS))

      SIA = SIN(DELTA) * SIN(PHS) + COS(DELTA) * COS(PHS) * COS(ALHA)
!     LINE ABOVE -- (4.7)
      ALPHA = ASIN( SIA ) * 180.D0 / PI  ! CONVERT TO DEGREES
      ! (4.17)
      AMO = 1.D0 / (SIA + 0.15D0 * (ALPHA + 3.885D0) ** (-1.253D0))
      ! (4.18)
      PAPO = EXP(-0.0001184D0 * MODELZ)
      ! (4.16)
      AM = AMO * PAPO
      ! (4.15)
      AM = 0.99D0 - 0.17D0 * AM
      ! ENERGY FLUX, REACHING GROUND UNDER CLEAR SKY
      PHCL = AM * PHISO

      IF (PHCL.LT.0.D0) PHCL=0.D0
      ! (4.19)
      PHRI = PHCL * ( 1.D0 - 0.0065D0 * CC **2 )
!  SOLAR RADIATION REACHING THE EARTH UNDER CLOUDY SKIES (ABOVE)

      IF (CICE.EQ.0) THEN  ! IF ALBE = 0, "OPEN WATER" CONDITIONS
        IF (PHCL.EQ.0.D0) THEN
          PHRR=0.D0
        ELSE  ! ESTIMATE REFLECTIVITY OF OPEN WATER SURFACE
          ! (4.25) - CLOUDINESS RATIO
          CR = MAX((1.D0 - PHRI / PHCL),0.D0)
          AA =  2.2D0 +CR ** 0.7/4.D0 -  (CR ** 0.7 - 0.4D0)**2 / 0.16D0
          BB = -1.02D0+CR ** 0.7/16.D0 + (CR ** 0.7 - 0.4D0)**2 / 0.64D0
          ! ESTIMATE OPEN WATER ALBEDO (4.22-4.24)
          ALR = AA * ALPHA ** BB
          ! AMOUNT OF SOLAR RADIATION REFLECTED
          PHRR = ALR * PHRI
        ENDIF

!  USE THE ALBEDO SPECIFIED TO INCLUDE REFLECTIVE EFFECTS OF ICE PRESENT
      ELSE
        ! (4.20)
        PHRR = ALBE * PHRI
        IF (PHCL.EQ.0.) PHRR=0.D0
      ENDIF
      ! NET SOLAR RADIATION BETWEEN T1-T2, (4.20)
      PHPS = PHRI - PHRR

      RETURN
      END SUBROUTINE SOLAR
!
!=======================================================================
!
!               ******************************************
                DOUBLE PRECISION FUNCTION FRAZIL_HEAT_COEF
!               ******************************************
     &(DH,U,TWAT,TFRZ)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Computes the heat exchange coefficient with the ice cover
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DH      |-->| PROPAGATION DEPTH
!| U       |-->| VELOCITY MAGNITUDE
!| TWAT    |-->| WATER TEMPERATURE
!| TFRZ    |-->| FREEZING POINT FOR WATER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY: TC_WT,XNU,CWI1,CIW1,ATA
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION DH,U,TWAT,TFRZ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION RE
!
!-----------------------------------------------------------------------
!
!     REYNOLDS NUMBER
      RE = U * DH / XNU
!
!     DRY BANKS
      IF(DH.LE.0.001D0.AND.DH.GT.0.D0) THEN
        FRAZIL_HEAT_COEF = 1394.D0
        RETURN
      ENDIF
!
!     LAMINAR FLOW
      IF(RE.LT.2200.D0) THEN
        FRAZIL_HEAT_COEF = ATA*TC_WT/DH
!
!     TURBULENT FLOW
      ELSEIF( TWAT.GT.TFRZ ) THEN ! WATER TEMP.>TFRZ
        FRAZIL_HEAT_COEF = CWI1*U**0.8/DH**0.2 ! CWI1 = 1448
      ELSE ! WATER TEMP.<TFRZ
        FRAZIL_HEAT_COEF = CIW1*U**0.8/DH**0.2 ! CIW1 = 1118
      ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END FUNCTION FRAZIL_HEAT_COEF
!
!                       *********************
                        INTEGER FUNCTION LEAP
!                       *********************
!
     &(IYEAR)
!
!***********************************************************************
! TELEMAC-3D V6P2                             27/06/2012
!***********************************************************************
!
!brief    DETERMINES WHETHER IYEAR IS A LEAP YEAR
!+        DESCRIPTION - RETURNS 1 IF IYEAR IS A LEAP YEAR, 0 OTHERWISE
!+
!
!history  C. GUILBAUD (SOGREAH)
!+        JUNE 2001
!+        V6P0?
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        27/06/2012
!+        V6P2
!+   Introduction into EXCHANGE_WITH_ATMOSPHERE module + INTENT
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IYEAR          |-->| INDEX OF YEAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IYEAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF( MOD(IYEAR,4).EQ.0.AND.
     &   (MOD(IYEAR,100).NE.0.OR.MOD(IYEAR,400).EQ.0)) THEN
        LEAP = 1
      ELSE
        LEAP = 0
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION LEAP

!                   ********************************
                    DOUBLE PRECISION FUNCTION DAYNUM
!                   ********************************
!
     &(IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC)
!
!***********************************************************************
! TELEMAC-3D V6P2                             27/06/2012
!***********************************************************************
!
!brief    RETURNS DAY NUMBER OF THE YEAR (FRACTIONAL)
!+
!
!history  C. GUILBAUD (SOGREAH)
!+        JUNE 2001
!+        V6P0?
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        27/06/2012
!+        V6P2
!+   Introduction into EXCHANGE_WITH_ATMOSPHERE module
!+   Change for type of result (double precision, not integer)
!+   + REAL conversion + addition of seconds ISEC
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDAY           |-->| INDEX OF DAY
!| IHOUR          |-->| INDEX OF HOUR
!| IMIN           |-->| INDEX OF MINUTE
!| IMONTH         |-->| INDEX OF MONTH
!| ISEC           |-->| INDEX OF SECOND
!| IYEAR          |-->| INDEX OF YEAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     INTEGER  LEAP
!     EXTERNAL LEAP
!
      INTEGER MONTH(12)
      PARAMETER ( MONTH=(/0,31,59,90,120,151,181,212,243,273,304,334/) )
!
!-----------------------------------------------------------------------
!
      DAYNUM = REAL(MONTH(IMONTH)+IDAY)
     &       + REAL(IHOUR)/24.D0+REAL(IMIN)/1440.D0+REAL(ISEC)/86400.D0
      IF(IMONTH.GT.2) DAYNUM = DAYNUM + REAL(LEAP(IYEAR))
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION DAYNUM

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE THERMAL_KHIONE
