!                    *****************************
                     SUBROUTINE GAIA_WRITE_RESULTS
!                    *****************************
     &(CODE,GRAFCOUNT,GRCOMP,COMP,LISTCOUNT,YAGOUT)
!
!***********************************************************************
! GAIA   V7P3
!***********************************************************************
!
!>@brief Write results informations into the result file
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      CODE       NAME OF CALLING PROGRAMME (TELEMAC2D OR 3D)
!>@param  [in]      GRAFCOUNT  PERIOD OF GRAPHICAL OUTPUTS
!>@param  [in]      GRCOMP     COUNTER FOR GRAPHICAL OUTPUTS
!>@param  [in]      LISTCOUNT  PERIODE DE SORTIE LISTING
!>@param  [in]      YAGOUT     LOGICAL: IF YES GRAPHIC OUTPUT (STEERED BY T2D)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_GAIA_WRITE_RESULTS => GAIA_WRITE_RESULTS
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=24), INTENT(IN) :: CODE
      INTEGER,           INTENT(IN) :: GRAFCOUNT
      INTEGER,           INTENT(IN) :: LISTCOUNT
      LOGICAL,           INTENT(IN) :: YAGOUT
      INTEGER,           INTENT(IN) :: GRCOMP
      LOGICAL,           INTENT(IN) :: COMP
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IF(GRAFCOUNT*(LT/GRAFCOUNT).EQ.LT) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREDES_GAIA'
        CALL PREDES_GAIA(LT,AT0,YAGOUT,CODE,LISTCOUNT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREDES_GAIA'
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
        IF(COMP)THEN
          CALL BIEF_DESIMP(GAI_FILES(GAIRES)%FMT,VARSOR,
     &                     NPOIN,GAI_FILES(GAIRES)%LU,
     &                     AT0,LT,LISTCOUNT,GRAFCOUNT,
     &                     SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL,
     &                     ILEO=YAGOUT,COMPGRAPH=GRCOMP+1)
        ELSE
          CALL BIEF_DESIMP(GAI_FILES(GAIRES)%FMT,VARSOR,
     &                     NPOIN,GAI_FILES(GAIRES)%LU,
     &                     AT0,LT,LISTCOUNT,GRAFCOUNT,
     &                     SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL,
     &                     ILEO=YAGOUT)
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
