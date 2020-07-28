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
!history  F.SOUILLE (EDF)
!+        30/09/2019
!+        V8P0
!+   Added new outputs
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO        |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| NADVAR       |-->| NUMBER OF VARIABLES FOR AUTOMATIC DIFFERENTIATION
!| NAMES_ADVAR  |-->| NAMES OF VARIABLES FOR AUTOMATIC DIFFERENTIATION
!| TEXTE        |<--| SEE ABOVE
!| TEXTPR       |<--| SEE ABOVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY: NC_FRA, SALINITY, THERMAL_BUDGET
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
        TEXTE (7 ) = 'PRECIP HEAT FLUX[SI]            '
        TEXTE (8 ) = 'FRAZIL THETA0   [SI]            '
        TEXTE (9 ) = 'FRAZIL THETA1   [SI]            '
        TEXTE (10) = 'REENTRAINMENT   [SI]            '
        TEXTE (11) = 'SETTLING VEL.   [SI]            '
        TEXTE (12) = 'SOLID ICE CONC. [SI]            '      !     ANFEM
        TEXTE (13) = 'SOLID ICE THICK.[SI]            '      !   THIFEMS
        TEXTE (14) = 'FRAZIL THICKNESS[SI]            '      !   THIFEMF
        TEXTE (15) = 'UNDER ICE THICK.[SI]            '      !       HUN
        TEXTE (16) = 'EQUIV. SURFACE  M               '
        TEXTE (17) = 'TOP ICE COVER   M               '
        TEXTE (18) = 'BOTTOM ICE COVERM               '
        TEXTE (19) = 'TOTAL ICE THICK.M               '
        TEXTE (20) = 'CHARACTERISTICS                 '      !   ICETYPE
        TEXTE (21) = 'PARTICLES NUMBER                '
        TEXTE (22) = 'TOTAL CONCENTRATION OF FRAZIL   '
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
        TEXTPR(7 ) = 'PRECIP HEAT FLUX[SI]            '
        TEXTPR(8 ) = 'FRAZIL THETA0   [SI]            '
        TEXTPR(9 ) = 'FRAZIL THETA1   [SI]            '
        TEXTPR(10) = 'REENTRAINMENT   [SI]            '
        TEXTPR(11) = 'SETTLING VEL.   [SI]            '
        TEXTPR(12) = 'SOLID ICE CONC. [SI]            '
        TEXTPR(13) = 'SOLID ICE THICK.[SI]            '
        TEXTPR(14) = 'FRAZIL THICKNESS[SI]            '
        TEXTPR(15) = 'UNDER ICE THICK.[SI]            '
        TEXTPR(16) = 'EQUIV. SURFACE  M               '
        TEXTPR(17) = 'TOP ICE COVER   M               '
        TEXTPR(18) = 'BOTTOM ICE COVERM               '
        TEXTPR(19) = 'TOTAL ICE THICK.M               '
        TEXTPR(20) = 'CHARACTERISTICS                 '
        TEXTPR(21) = 'PARTICLES NUMBER                '
        TEXTPR(22) = 'TOTAL CONCENTRATION OF FRAZIL   '
!
!-----------------------------------------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
        TEXTPR(1 ) = 'SOLRAD CLEAR SKY[SI]            '
        TEXTPR(2 ) = 'SOLRAD CLOUDY   [SI]            '
        TEXTPR(3 ) = 'NET SOLRAD      [SI]            '
        TEXTPR(4 ) = 'EFFECTIVE SOLRAD[SI]            '
        TEXTPR(5 ) = 'EVAPO HEAT FLUX [SI]            '
        TEXTPR(6 ) = 'CONDUC HEAT FLUX[SI]            '
        TEXTPR(7 ) = 'PRECIP HEAT FLUX[SI]            '
        TEXTPR(8 ) = 'FRAZIL THETA0   [SI]            '
        TEXTPR(9 ) = 'FRAZIL THETA1   [SI]            '
        TEXTPR(10) = 'REENTRAINMENT   [SI]            '
        TEXTPR(11) = 'SETTLING VEL.   [SI]            '
        TEXTPR(12) = 'SOLID ICE CONC. [SI]            '
        TEXTPR(13) = 'SOLID ICE THICK.[SI]            '
        TEXTPR(14) = 'FRAZIL THICKNESS[SI]            '
        TEXTPR(15) = 'UNDER ICE THICK.[SI]            '
        TEXTPR(16) = 'EQUIV. SURFACE  M               '
        TEXTPR(17) = 'TOP ICE COVER   M               '
        TEXTPR(18) = 'BOTTOM ICE COVERM               '
        TEXTPR(19) = 'TOTAL ICE THICK.M               '
        TEXTPR(20) = 'CHARACTERISTICS                 '
        TEXTPR(21) = 'PARTICLES NUMBER                '
        TEXTPR(22) = 'TOTAL CONCENTRATION OF FRAZIL   '
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
        TEXTPR(7 ) = 'PRECIP HEAT FLUX[SI]            '
        TEXTPR(8 ) = 'FRAZIL THETA0   [SI]            '
        TEXTPR(9 ) = 'FRAZIL THETA1   [SI]            '
        TEXTPR(10) = 'REENTRAINMENT   [SI]            '
        TEXTPR(11) = 'SETTLING VEL.   [SI]            '
        TEXTPR(12) = 'SOLID ICE CONC. [SI]            '
        TEXTPR(13) = 'SOLID ICE THICK.[SI]            '
        TEXTPR(14) = 'FRAZIL THICKNESS[SI]            '
        TEXTPR(15) = 'UNDER ICE THICK.[SI]            '
        TEXTPR(16) = 'EQUIV. SURFACE  M               '
        TEXTPR(17) = 'TOP ICE COVER   M               '
        TEXTPR(18) = 'BOTTOM ICE COVERM               '
        TEXTPR(19) = 'TOTAL ICE THICK.M               '
        TEXTPR(20) = 'CHARACTERISTICS                 '
        TEXTPR(21) = 'PARTICLES NUMBER                '
        TEXTPR(22) = 'TOTAL CONCENTRATION OF FRAZIL   '
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
!     PRECIPITATION HEAT FLUX
      MNEMO(7)   = 'PHIP    '
!     FEEZE-UP AND ICE GROWTH:
      MNEMO(8)   = 'COV_TH0 '
!     FEEZE-UP AND ICE GROWTH:
      MNEMO(9)   = 'COV_TH1 '
!     FEEZE-UP AND ICE GROWTH:
      MNEMO(10)   = 'COV_BT1 '
!     FEEZE-UP AND ICE GROWTH:
      MNEMO(11)  = 'COV_VBB '
!     ICE CONCENTRATION AS COMPUTED FROM SURFACE ICE PARTICLES
      MNEMO(12)  = 'COV_FC  '
!     FEEZE-UP AND ICE GROWTH: SOLID ICE COVER THICKNESS
      MNEMO(13)  = 'COV_THS '
!     FEEZE-UP AND ICE GROWTH: FRAZIL ICE COVER THICKNESS
      MNEMO(14)  = 'COV_THF '
!     ICE COVER: UNDER ICE THICKNESS
      MNEMO(15)  = 'COV_THUN'
!     EQUIVALENT SURFACE ELEVATION
      MNEMO(16)  = 'COV_EQ  '
!     TOP OF THE ICE COVER
      MNEMO(17)  = 'COV_ET  '
!     BOTTOM OF THE ICE COVER (ALSO THE FREE SURFACE))
      MNEMO(18)  = 'COV_EB  '
!     TOTAL ICE THICKNESS
      MNEMO(19)  = 'COV_THT '
!     ICE CHARACTERISATION
!       (PRIMARY NUMBERS FOR COMBINED CHARACTERISATIONS)
!       1 - OPEN WATER
!       2 - STATIC BODER ICE
      MNEMO(20)  = 'ICETYPE '
!     TOTAL ICE THICKNESS
      MNEMO(21)  = 'NTOT    '
!     TOTAL ICE THICKNESS
      MNEMO(22)  = 'CTOT    '
!
!     THE LAST RANK
!
      ILAST = 22
      INEXT = ILAST+1
!
!-----------------------------------------------------------------------
!
!     FRAZIL CONCENTRATIONS
!
      IF(THERMAL_BUDGET) THEN
        DO I=1,NC_FRA
          WRITE(CHAR2,'(I2)') I
          TEXTE(ILAST+I)  = 'FRAZIL CLASS '
     &                      //ADJUSTL(CHAR2)//''
          TEXTPR(ILAST+I) = 'FRAZIL CLASS '
     &                      //ADJUSTL(CHAR2)//''
          MNEMO(ILAST+I)  = 'F'//ADJUSTL(CHAR2)//'   '
          TEXTE(ILAST+I+NC_FRA)='NB PARTICLE '
     &                          //ADJUSTL(CHAR2)//' '
          TEXTPR(ILAST+I+NC_FRA)  = 'NB PARTICLE '
     &                              //ADJUSTL(CHAR2)//' '
          MNEMO(ILAST+I+NC_FRA)  = 'N'//ADJUSTL(CHAR2)//'   '
        ENDDO
        TEXTE(ILAST+2*NC_FRA+1) = 'TEMPERATURE        '
        TEXTPR(ILAST+2*NC_FRA+1) = 'TEMPERATURE        '
        MNEMO(ILAST+2*NC_FRA+1) = 'TEMP    '
        IF(SALINITY) THEN
          TEXTE(ILAST+2*NC_FRA+2) = 'SALINITY        '
          TEXTPR(ILAST+2*NC_FRA+2) = 'SALINITY        '
          MNEMO(ILAST+2*NC_FRA+2) = 'SAL     '
        ENDIF
      ENDIF



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

