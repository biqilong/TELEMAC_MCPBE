!                     ************************
                      SUBROUTINE NOMVAR_KHIONE
!                     ************************
!
     &(TEXTE,TEXTPR,MNEMO,NADVAR,NAMES_ADVAR)
!
!***********************************************************************
! KHIONE
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (IN TEXTPR).
!+
!+                TEXTE AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!history  S.E.BOURBAN (HRW)
!+        14/06/2017
!+        V7P3
!+   Initial implementation with heat fluxes as an example.
!
!history  F.SOUILLE (EDF)
!+        30/09/2019
!+        V8P0
!+   Added new outputs
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO          |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| TEXTE          |<--| SEE ABOVE
!| TEXTPR         |<--| SEE ABOVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NADVAR
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_ADVAR(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2) CHAR2
      INTEGER I,ILAST,INEXT
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.2) THEN
!
        TEXTE (1 ) = 'SOLRAD CLEAR SKY[SI]            '
        TEXTE (2 ) = 'SOLRAD CLOUDY   [SI]            '
        TEXTE (3 ) = 'NET SOLRAD      [SI]            '
        TEXTE (4 ) = 'EFFECTIVE SOLRAD[SI]            '
        TEXTE (5 ) = 'EVAPO HEAT FLUX [SI]            '
        TEXTE (6 ) = 'CONDUC HEAT FLUX[SI]            '
        TEXTE (7 ) = 'FRAZIL THETA0   [SI]            '
        TEXTE (8 ) = 'FRAZIL THETA1   [SI]            '
        TEXTE (9 ) = 'REENTRAINMENT   [SI]            '
        TEXTE (10) = 'SETTLING VEL.   [SI]            '
        TEXTE (11) = 'SOLID ICE CONC. [SI]            '      !     ANFEM
        TEXTE (12) = 'SOLID ICE THICK.[SI]            '      !   THIFEMS
        TEXTE (13) = 'FRAZIL THICKNESS[SI]            '      !   THIFEMF
        TEXTE (14) = 'UNDER ICE THICK.[SI]            '      !       HUN
        TEXTE (15) = 'ICE VELOCITY U  M/S             '      !     U_ICE
        TEXTE (16) = 'ICE VELOCITY V  M/S             '      !     V_ICE
        TEXTE (17) = 'EQUIV. SURFACE  M               '
        TEXTE (18) = 'TOP ICE COVER   M               '
        TEXTE (19) = 'BOTTOM ICE COVERM               '
        TEXTE (20) = 'TOTAL ICE THICK.M               '
        TEXTE (21) = 'CHARACTERISTICS                 '      !   ICETYPE
        TEXTE (22) = 'DYNAMIC GROWTH  FRACTION OF AREA'      !       DWB
        TEXTE (23) = 'PRECIP HEAT FLUX[SI]            '
!
! TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
! FROM ANOTHER CODE WITH DIFFERENT VARIABLE NAMES, WHICH MUST
! BE GIVEN HERE:
!
        TEXTPR(1 ) = 'SOLRAD CLEAR SKY[SI]            '
        TEXTPR(2 ) = 'SOLRAD CLOUDY   [SI]            '
        TEXTPR(3 ) = 'NET SOLRAD      [SI]            '
        TEXTPR(4 ) = 'EFFECTIVE SOLRAD[SI]            '
        TEXTPR(5 ) = 'EVAPO HEAT FLUX [SI]            '
        TEXTPR(6 ) = 'CONDUC HEAT FLUX[SI]            '
        TEXTPR(7 ) = 'FRAZIL THETA0   [SI]            '
        TEXTPR(8 ) = 'FRAZIL THETA1   [SI]            '
        TEXTPR(9 ) = 'REENTRAINMENT   [SI]            '
        TEXTPR(10) = 'SETTLING VEL.   [SI]            '
        TEXTPR(11) = 'SOLID ICE CONC. [SI]            '
        TEXTPR(12) = 'SOLID ICE THICK.[SI]            '
        TEXTPR(13) = 'FRAZIL THICKNESS[SI]            '
        TEXTPR(14) = 'UNDER ICE THICK.[SI]            '
        TEXTPR(15) = 'ICE VELOCITY U  M/S             '
        TEXTPR(16) = 'ICE VELOCITY V  M/S             '
        TEXTPR(17) = 'EQUIV. SURFACE  M               '
        TEXTPR(18) = 'TOP ICE COVER   M               '
        TEXTPR(19) = 'BOTTOM ICE COVERM               '
        TEXTPR(20) = 'TOTAL ICE THICK.M               '
        TEXTPR(21) = 'CHARACTERISTICS                 '
        TEXTPR(22) = 'DYNAMIC GROWTH  FRACTION OF AREA'
        TEXTE (23) = 'PRECIP HEAT FLUX[SI]            '
!
!-----------------------------------------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
        TEXTE (1 ) = 'SOLRAD CLEAR SKY[SI]            '
        TEXTE (2 ) = 'SOLRAD CLOUDY   [SI]            '
        TEXTE (3 ) = 'NET SOLRAD      [SI]            '
        TEXTE (4 ) = 'EFFECTIVE SOLRAD[SI]            '
        TEXTE (5 ) = 'EVAPO HEAT FLUX [SI]            '
        TEXTE (6 ) = 'CONDUC HEAT FLUX[SI]            '
        TEXTE (7 ) = 'FRAZIL THETA0   [SI]            '
        TEXTE (8 ) = 'FRAZIL THETA1   [SI]            '
        TEXTE (9 ) = 'REENTRAINMENT   [SI]            '
        TEXTE (10) = 'SETTLING VEL.   [SI]            '
        TEXTE (11) = 'SOLID ICE CONC. [SI]            '
        TEXTE (12) = 'SOLID ICE THICK.[SI]            '
        TEXTE (13) = 'FRAZIL THICKNESS[SI]            '
        TEXTE (14) = 'UNDER ICE THICK.[SI]            '
        TEXTE (15) = 'ICE VELOCITY U  M/S             '
        TEXTE (16) = 'ICE VELOCITY V  M/S             '
        TEXTE (17) = 'EQUIV. SURFACE  M               '
        TEXTE (18) = 'TOP ICE COVER   M               '
        TEXTE (19) = 'BOTTOM ICE COVERM               '
        TEXTE (20) = 'TOTAL ICE THICK.M               '
        TEXTE (21) = 'CARACTERISTIQUES                '
        TEXTE (22) = 'DYNAMIC GROWTH  FRACTION OF AREA'
        TEXTE (23) = 'PRECIP HEAT FLUX[SI]            '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
        TEXTPR(1 ) = 'SOLRAD CLEAR SKY[SI]            '
        TEXTPR(2 ) = 'SOLRAD CLOUDY   [SI]            '
        TEXTPR(3 ) = 'NET SOLRAD      [SI]            '
        TEXTPR(4 ) = 'EFFECTIVE SOLRAD[SI]            '
        TEXTPR(5 ) = 'EVAPO HEAT FLUX [SI]            '
        TEXTPR(6 ) = 'CONDUC HEAT FLUX[SI]            '
        TEXTPR(7 ) = 'FRAZIL THETA0   [SI]            '
        TEXTPR(8 ) = 'FRAZIL THETA1   [SI]            '
        TEXTPR(9 ) = 'REENTRAINMENT   [SI]            '
        TEXTPR(10) = 'SETTLING VEL.   [SI]            '
        TEXTPR(11) = 'SOLID ICE CONC. [SI]            '
        TEXTPR(12) = 'SOLID ICE THICK.[SI]            '
        TEXTPR(13) = 'FRAZIL THICKNESS[SI]            '
        TEXTPR(14) = 'UNDER ICE THICK.[SI]            '
        TEXTPR(15) = 'ICE VELOCITY U  M/S             '
        TEXTPR(16) = 'ICE VELOCITY V  M/S             '
        TEXTPR(17) = 'EQUIV. SURFACE  M               '
        TEXTPR(18) = 'TOP ICE COVER   M               '
        TEXTPR(19) = 'BOTTOM ICE COVERM               '
        TEXTPR(20) = 'TOTAL ICE THICK.M               '
        TEXTPR(21) = 'CARACTERISTIQUES                '
        TEXTPR(22) = 'DYNAMIC GROWTH  FRACTION OF AREA'
        TEXTE (23) = 'PRECIP HEAT FLUX[SI]            '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIASES FOR THE VARIABLES IN THE STEERING FILE
!
!     THERMAL BUDGET: SOLAR RADIATION UNDER CLEAR SKY
      MNEMO(1)   = 'PHCL    '
!     THERMAL BUDGET: SOLAR RADIATION UNDER CLOUDY SKY
      MNEMO(2)   = 'PHRI    '
!     THERMAL BUDGET: NET SOLAR RADIATION AFTER REFLECTION
      MNEMO(3)   = 'PHPS    '
!     THERMAL BUDGET: EFFECTIVE BACK RADIATION
      MNEMO(4)   = 'PHIB    '
!     THERMAL BUDGET: EVAPORATION HEAT FLUX
      MNEMO(5)   = 'PHIE    '
!     THERMAL BUDGET: CONDUCTIVITY HEAT FLUX
      MNEMO(6)   = 'PHIH    '
!     FEEZE-UP AND ICE GROWTH:
      MNEMO(7)   = 'COV_TH0 '
!     FEEZE-UP AND ICE GROWTH:
      MNEMO(8)   = 'COV_TH1 '
!     FEEZE-UP AND ICE GROWTH:
      MNEMO(9)   = 'COV_BT1 '
!     FEEZE-UP AND ICE GROWTH:
      MNEMO(10)  = 'COV_VBB '
!     ICE CONCENTRATION AS COMPUTED FROM SURFACE ICE PARTICLES
      MNEMO(11)  = 'COV_FC  '
!     FEEZE-UP AND ICE GROWTH: SOLID ICE COVER THICKNESS
      MNEMO(12)  = 'COV_THS '
!     FEEZE-UP AND ICE GROWTH: FRAZIL ICE COVER THICKNESS
      MNEMO(13)  = 'COV_THF '
!     ICE COVER: UNDER ICE THICKNESS
      MNEMO(14)  = 'COV_THUN'
!     ICE COVER: VELOCITY U
      MNEMO(15)  = 'COV_VELU'
!     ICE COVER: VELOCITY V
      MNEMO(16)  = 'COV_VELV'
!     EQUIVALENT SURFACE ELEVATION
      MNEMO(17)  = 'COV_EQ  '
!     TOP OF THE ICE COVER
      MNEMO(18)  = 'COV_ET  '
!     BOTTOM OF THE ICE COVER (ALSO THE FREE SURFACE))
      MNEMO(19)  = 'COV_EB  '
!     TOTAL ICE THICKNESS
      MNEMO(20)  = 'COV_THT '
!     ICE CHARACTERISATION
!       (PRIMARY NUMBERS FOR COMBINED CHARACTERISATIONS)
!       0 - OPEN WATER
!       2 - STATIC BODER ICE, INSIDE NODE
!       3 - STATIC BODER ICE, BORDER NODE
!       ? -
      MNEMO(21)  = 'ICETYPE '
!
      MNEMO(22)  = 'COV_DYN '
!
      MNEMO(23)   = 'PHIP    '
!
!     THE LAST RANK
!
      ILAST = 23
      INEXT = ILAST+1
!
!-----------------------------------------------------------------------
!
!     DIFFERENTIATORS
!
      IF(NADVAR.GT.0) THEN
        DO I=1,NADVAR
          TEXTE(ILAST+I)  = NAMES_ADVAR(I)
          TEXTPR(ILAST+I) = NAMES_ADVAR(I)
          WRITE(CHAR2,'(I2)') I
          MNEMO(ILAST+I) = 'AD'//ADJUSTL(CHAR2)//'    '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

