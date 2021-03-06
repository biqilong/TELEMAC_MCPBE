!                   **********************
                    SUBROUTINE INTERPMETEO
!                   **********************
!
     &(WW,WINDX,WINDY,TAIR,PATM,HREL,NEBU,RAINFALL,SOLRAD,AT,NFO)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    READS AND INTERPOLATES VARIABLES IN AN ASCII FILE
!+
!
!history  R. SAMIE, E. RAZAFINDRAKOTO, C.-T. PHAM (EDF-LNHE)
!+        09/07/2014
!+        V7P0
!+        FROM LAPLUIE AND LECENT
!+
!
!history  A. LEROY (EDF-LNHE)
!+        25/11/2015
!+        V7P1
!+        The interpolation of the norm of the wind velocity
!+        is now correct and the Secchi length is added in the
!+        variables to be read
!+
!history  R.ATA (EDF-LNHE)
!+        25/03/2016
!+        V7P2
!+        harmonization with meteo file, last column is evaporation
!+         not secchi length
!+        all out are changed to inout
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME OF TIME STEP
!| HREL           |<--| RELATIVE HUMIDITY
!| NEBU           |<--| NEBULOSITY
!| NFO            |-->| LOGICAL UNIT OF THE FORMATTED DATA FILE
!| PATM           |<--| ATMOSPHERIC PRESSURE
!| RAINFALL       |<--| RAINFALL
!| SOLAR RADIATION|<--| SOLAR RADIATION IF AVAILABLE
!| TAIR           |<--| AIR TEMPERATURE
!| WINDX          |<--| WIND ALONG X
!| WINDY          |<--| WIND ALONG Y
!| WW             |<--| MAGNITUDE OF WIND VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC2D, ONLY: DEJA_IPM,DUMMY_IPM,TABENT_IPM,
     &                                  POSTAB_IPM,NBENR_IPM
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NFO
      DOUBLE PRECISION, INTENT(IN)    :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: WW,WINDX,WINDY,TAIR,PATM,HREL
      DOUBLE PRECISION, INTENT(INOUT) :: NEBU,RAINFALL,SOLRAD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!     NUMBER OF VARIABLES IN THE ASCII FILE NFO
      INTEGER, PARAMETER :: NINPUTVAR = 9
      INTEGER, PARAMETER :: NLINESTOSKIP = 2
!
      DOUBLE PRECISION DELTAT,ALPHA
      DOUBLE PRECISION DTR
!
!-----------------------------------------------------------------------
!
      DTR = ATAN(1.D0)/45.D0
!
!-----------------------------------------------------------------------
!
!  READS INPUT DATA FILE
!  AT THE FIRST TIME STEP AND FILLS IN TABENT_IPM ARRAY
!
      IF (.NOT.DEJA_IPM) THEN
!
        WRITE(LU,*)'=================================='
        WRITE(LU,*)'BEGINNING OF READING OF INPUT FILE'
        WRITE(LU,*)'=================================='
!
        REWIND NFO
!
!  READS THE HEADLINE OF THE DATA FILE
!
        DO I=1,NLINESTOSKIP
          READ(NFO,*)
        ENDDO
!
        NBENR_IPM = 0
!
        ALLOCATE(DUMMY_IPM(NINPUTVAR))
!
!  READS VARIABLES AND FILLS IN TABENT_IPM ARRAY
!
!       AVOID PART-REF WITH NON-ZERO RANK IN MODE T1V
 100    READ(NFO,*,END=20,ERR=40) ( DUMMY_IPM(J), J=1, NINPUTVAR )
        NBENR_IPM = NBENR_IPM + 1
        GO TO 100
!
 20     CONTINUE
!
!  +2 TO STORE 2 EXTRA DATA (THE X AND Y COMPONENTS OF WIND VELOCITY)
        ALLOCATE(TABENT_IPM(NBENR_IPM,NINPUTVAR+2))
        DEJA_IPM = .TRUE.
!
!-----------------------------------------------------------------------
!
        REWIND NFO
!
!  READS THE HEADLINE OF THE DATA FILE
!
        DO I=1,NLINESTOSKIP
          READ(NFO,*)
        ENDDO
!
!  READS VARIABLES AND FILLS IN TABENT_IPM ARRAY
!
        DO I=1,NBENR_IPM
!         AD: AVOID PART-REF WITH NON-ZERO RANK IN MODE T1V
          READ(NFO,*,ERR=40) ( TABENT_IPM(I,J),  J=1, NINPUTVAR )
        ENDDO
!
        WRITE(LU,*)'======================================='
        WRITE(LU,*)'  END OF READING OF INPUT DATA         '
        WRITE(LU,*)'  THERE ARE ',NBENR_IPM,' RECORDS          '
        WRITE(LU,*)'  FROM T = ',TABENT_IPM(1,1), ' TO = ',
     &             TABENT_IPM(NBENR_IPM,1),' SECONDS '
        WRITE(LU,*)'======================================='
!
!  FILLS IN WITH X AND Y COMPONENTS OF WIND VELOCITY
!
        DO I=1,NBENR_IPM
          TABENT_IPM(I,NINPUTVAR+1) =
     &             -TABENT_IPM(I,2)*SIN(TABENT_IPM(I,3)*DTR)
          TABENT_IPM(I,NINPUTVAR+2) =
     &             -TABENT_IPM(I,2)*COS(TABENT_IPM(I,3)*DTR)
        ENDDO
!
!  END TO FILL IN TABENT_IPM ARRAY
!  INITIALISATION OF THE POINTER POSTAB_IPM
!
        POSTAB_IPM = 1
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INTERPOLATES DATA AT EACH TIME STEP
!  POINTER POSTAB_IPM POSITIONNED AT EACH TIME STEP
!
120   IF(AT.LT.TABENT_IPM(POSTAB_IPM,1) .OR.
     &   AT.GE.TABENT_IPM(POSTAB_IPM+1,1))
     &  THEN
        IF(AT.LT.TABENT_IPM(POSTAB_IPM,1))   POSTAB_IPM = POSTAB_IPM - 1
        IF(AT.GE.TABENT_IPM(POSTAB_IPM+1,1)) POSTAB_IPM = POSTAB_IPM + 1
        IF(POSTAB_IPM.GT.NBENR_IPM) THEN
          WRITE(LU,*)'==============================================='
          WRITE(LU,*)'WARNING: TIME OF CALCULATION AT = ', AT
          WRITE(LU,*)'IS BIGGER THAN MAXIMUM TIME IN YOUR INPUT DATA '
          WRITE(LU,*)'FILE T = ', TABENT_IPM(NBENR_IPM,1)
          WRITE(LU,*)'==============================================='
          CALL PLANTE(1)
        ELSEIF(POSTAB_IPM.LT.1) THEN
          WRITE(LU,*)'==============================================='
          WRITE(LU,*)'WARNING: TIME OF CALCULATION AT = ', AT
          WRITE(LU,*)'IS LOWER THAN MINIMUM TIME IN YOUR INPUT DATA '
          WRITE(LU,*)'FILE T = ', TABENT_IPM(1,1)
          WRITE(LU,*)'==============================================='
          CALL PLANTE(1)
        ENDIF
        GO TO 120
      ENDIF
!
      DELTAT = TABENT_IPM(POSTAB_IPM+1,1)-TABENT_IPM(POSTAB_IPM,1)
      ALPHA  = (AT-TABENT_IPM(POSTAB_IPM,1))/DELTAT
!
!-----------------------------------------------------------------------
!
      WINDX =  TABENT_IPM(POSTAB_IPM,NINPUTVAR+1)
     &      + (TABENT_IPM(POSTAB_IPM+1,NINPUTVAR+1)
     &      -  TABENT_IPM(POSTAB_IPM,NINPUTVAR+1))*ALPHA
      WINDY =  TABENT_IPM(POSTAB_IPM,NINPUTVAR+2)
     &      + (TABENT_IPM(POSTAB_IPM+1,NINPUTVAR+2)
     &      -  TABENT_IPM(POSTAB_IPM,NINPUTVAR+2))*ALPHA
      WW    =  SQRT(WINDX**2+WINDY**2)
!
      TAIR  =  TABENT_IPM(POSTAB_IPM,4)
     &      + (TABENT_IPM(POSTAB_IPM+1,4)
     &         -TABENT_IPM(POSTAB_IPM,4))*ALPHA
      PATM  =  TABENT_IPM(POSTAB_IPM,5)
     &      + (TABENT_IPM(POSTAB_IPM+1,5)
     &         -TABENT_IPM(POSTAB_IPM,5))*ALPHA
      HREL  =  TABENT_IPM(POSTAB_IPM,6)
     &      + (TABENT_IPM(POSTAB_IPM+1,6)
     &         -TABENT_IPM(POSTAB_IPM,6))*ALPHA

      NEBU  =  TABENT_IPM(POSTAB_IPM,7)
     &      + (TABENT_IPM(POSTAB_IPM+1,7)
     &         -TABENT_IPM(POSTAB_IPM,7))*ALPHA
!
      RAINFALL = TABENT_IPM(POSTAB_IPM+1,8)/DELTAT
!
      SOLRAD = TABENT_IPM(POSTAB_IPM,9)
     &     + (TABENT_IPM(POSTAB_IPM+1,9)-TABENT_IPM(POSTAB_IPM,9))*ALPHA
!
!-----------------------------------------------------------------------
!
      GO TO 200
!
!-----------------------------------------------------------------------
!
 40     CONTINUE
        WRITE(LU,*) 'INTERPMETEO: PROBLEM TO READ THE ASCII ' //
     &              'ATMOSPHERIC DATA FILE'
        WRITE(LU,*) 'THE EXPECTED FORMAT IS 1-TIME 2-WIND MAGN ' //
     &              '3-WIND DIR 4-TAIR 5-PATM 6-HREL 7-NEBULO ' //
     &              '8-RAIN 9-SOLAR RAD'
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
 200    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
