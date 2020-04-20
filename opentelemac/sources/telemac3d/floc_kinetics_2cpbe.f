!                    ******************************
                     SUBROUTINE FLOC_KINETICS_2CPBE
!                    ******************************
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    COMPUTE FLOCCULATION KINETICS FOR MCPBE FLOCCULATION MODEL
!+               THIS SUBROUTINE IS ONLY USED FOR 2CPBE CASE.
!
!history  QILONG BI, BJ. LEE & X.SHEN
!+        09/05/19
!+        V8P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN3          |-->| NUMBER OF POINTS IN 3D MESH
!| TA              |-->| TRACER (CONCENTRATIONS OF FLOCS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_TELEMAC3D, ONLY: NPOIN3,TA,S0TA,S1TA,DNUVIV,EP,
     &                                  FLOCMAC_DIA,RHO0,WCHU,NPOIN2,
     &                                  UETCAR,H
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: CNUM_P,CNUM_F,CNUM_T,NC,
     &                                 SHR_G,BETA_PP,BETA_FF,BETA_PF,
     &                                 BRK_SH,ABKE_P,ABKE_F,ABKE_T

      DOUBLE PRECISION :: FLOCMIC_VOL,VMU,WCHU_P,WCHU_F
      DOUBLE PRECISION, PARAMETER :: C1        =  0.5D0
      DOUBLE PRECISION, PARAMETER :: C2        =  0.5D0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      IF(FLOC .AND. FLOC_TYPE.EQ.3 .AND. MCPBE_VER.EQ.1) THEN
!       SOURCE TERM FOR MICROFLOCS
        S0TA%ADR(IMICFLC)%P%TYPR='Q'
        S1TA%ADR(IMICFLC)%P%TYPR='0'
        CALL OS('X=C     ',S0TA%ADR(IMICFLC)%P,C=0.D0)
        CALL OS('X=C     ',S1TA%ADR(IMICFLC)%P,C=0.D0)
!       SOURCE TERM FOR MACROFLOCS
        S0TA%ADR(IMACFLC)%P%TYPR='Q'
        S1TA%ADR(IMACFLC)%P%TYPR='0'
        CALL OS('X=C     ',S0TA%ADR(IMACFLC)%P,C=0.D0)
        CALL OS('X=C     ',S1TA%ADR(IMACFLC)%P,C=0.D0)
!       SOURCE TERM FOR MICROFLOCS INSIDE MACROFLOCS
        S0TA%ADR(IMICF_MACF)%P%TYPR='Q'
        S1TA%ADR(IMICF_MACF)%P%TYPR='0'
        CALL OS('X=C     ',S0TA%ADR(IMICF_MACF)%P,C=0.D0)
        CALL OS('X=C     ',S1TA%ADR(IMICF_MACF)%P,C=0.D0)
      ENDIF

!     COMPUTE FLOCCULATION KINETICS

      FLOCMIC_VOL=(1.D0/6.D0)*3.14159D0*FLOCMIC_DIAFIX**3.D0
      VMU = DNUVIV * RHO0

      DO IPOIN = 1,NPOIN3 

        CNUM_P(IPOIN) = 
     &         TA%ADR(IMICFLC)%P%R(IPOIN)/(FLOCMIC_VOL*FLOCMIC_DEN)
        CNUM_F(IPOIN) = 
     &         TA%ADR(IMACFLC)%P%R(IPOIN)/(FLOCMIC_VOL*FLOCMIC_DEN)
        CNUM_T(IPOIN) = 
     &      TA%ADR(IMICF_MACF)%P%R(IPOIN)/(FLOCMIC_VOL*FLOCMIC_DEN)
        
        NC(IPOIN) = MAX(2.D0, CNUM_T(IPOIN)/CNUM_F(IPOIN))

! Modification for testing TCBPE kinetics only
!       SHR_G(IPOIN)=50.0D0

! Here, Please create a depth and shear velocity dependent "SHR_G"			 
        SHR_G(IPOIN) = SQRT(EP%R(IPOIN)/DNUVIV)

!       FOR THE BOTTOM NODE, EP COULD ALSO USE THEORETICAL VALUE (ML)            
        IF(IPOIN.LE.NPOIN2) THEN
          SHR_G(IPOIN) = SQRT(UETCAR%R(IPOIN)**1.5/(KARMAN*0.001)
     &                                 * (1-0.001/H%R(IPOIN))/DNUVIV)
        ENDIF

        WCHU_P = WCHU%ADR(IMICFLC)%P%R(IPOIN)
        WCHU_F = WCHU%ADR(IMACFLC)%P%R(IPOIN)

        BETA_PP(IPOIN) = 2.D0*KBOLZ*TEMPSTD/(3.D0*VMU)*4.D0
     &            +(1.D0/6.D0)*(2.D0*FLOCMIC_DIAFIX)**3.D0*SHR_G(IPOIN)

        BETA_FF(IPOIN) = 2.D0*KBOLZ*TEMPSTD/(3.D0*VMU)*4.D0
     &      +(1.D0/6.D0)*(2.D0*FLOCMAC_DIA%R(IPOIN))**3.D0*SHR_G(IPOIN)

        BETA_PF(IPOIN) = 2.D0*KBOLZ*TEMPSTD
     &         *(1.D0/FLOCMIC_DIAFIX+1.D0/FLOCMAC_DIA%R(IPOIN))
     &         *(FLOCMIC_DIAFIX+FLOCMAC_DIA%R(IPOIN))/(3.D0*VMU)
     &         +(1.D0/6.D0)*(FLOCMIC_DIAFIX+FLOCMAC_DIA%R(IPOIN))**3.D0
     &         *SHR_G(IPOIN)+3.14159D0/4.D0
     &         *(FLOCMIC_DIAFIX+FLOCMAC_DIA%R(IPOIN))**2.D0
     &         *ABS(WCHU_P-WCHU_F)

!       Modification Proposed by Kyle Strom (2018) 
        BRK_SH(IPOIN) = BRK_EFF*SHR_G(IPOIN)*((FLOCMAC_DIA%R(IPOIN)
     &          -FLOCMIC_DIAFIX)/FLOCMIC_DIAFIX)**(3.D0-FRACDIM_MAC)
     &          *(VMU*SHR_G(IPOIN)/(BRK_FY/FLOCMAC_DIA%R(IPOIN)**2.D0))
     &          **(C1+C2*FLOCMAC_DIA%R(IPOIN)/((VMU*1.D-3)**3.D0
     &          /EP%R(IPOIN))**0.25D0)
        
! CALCULATE THE AGGREGATION AND BREAKAGE KERNELS
        ABKE_P(IPOIN) = -0.5D0*AGG_ALPHA*BETA_PP(IPOIN)*CNUM_P(IPOIN)
     &            *CNUM_P(IPOIN)*(NC(IPOIN)/(NC(IPOIN)-1.D0))
     &            -AGG_ALPHA*BETA_PF(IPOIN)*CNUM_F(IPOIN)*CNUM_P(IPOIN)
     &            +BRKFRAC_P1*NC(IPOIN)*BRK_SH(IPOIN)*CNUM_F(IPOIN)

        ABKE_F(IPOIN) = 0.5D0*AGG_ALPHA*BETA_PP(IPOIN)
     &      *(CNUM_P(IPOIN)*CNUM_P(IPOIN))*(1.D0/(NC(IPOIN)-1.D0))
     &      -0.5D0*AGG_ALPHA*BETA_FF(IPOIN)*CNUM_F(IPOIN)*CNUM_F(IPOIN)
     &      +BRK_SH(IPOIN)*CNUM_F(IPOIN)

        ABKE_T(IPOIN) = 0.5D0*AGG_ALPHA*BETA_PP(IPOIN)
     &      *(CNUM_P(IPOIN)*CNUM_P(IPOIN))*(NC(IPOIN)/(NC(IPOIN)-1.D0))
     &      +AGG_ALPHA*BETA_PF(IPOIN)*CNUM_P(IPOIN)*CNUM_F(IPOIN)
     &      -BRKFRAC_P1*NC(IPOIN)*BRK_SH(IPOIN)*CNUM_F(IPOIN)

        IF (S0TA%ADR(IMICFLC)%P%R(IPOIN).LE.0.D0) THEN
          S0TA%ADR(IMICFLC)%P%R(IPOIN) = MAX(
     &                       ABKE_P(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN,
     &                       -TA%ADR(IMICFLC)%P%R(IPOIN)/DT)
          S0TA%ADR(IMACFLC)%P%R(IPOIN) =
     &                       ABKE_F(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN
          S0TA%ADR(IMICF_MACF)%P%R(IPOIN) =
     &                       -S0TA%ADR(IMICFLC)%P%R(IPOIN)
        ELSE 
          S0TA%ADR(IMICF_MACF)%P%R(IPOIN) = MAX(
     &                       ABKE_T(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN,
     &                       -TA%ADR(IMICF_MACF)%P%R(IPOIN)/DT)
          S0TA%ADR(IMACFLC)%P%R(IPOIN) = MAX(
     &                       ABKE_F(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN,
     &                       -TA%ADR(IMACFLC)%P%R(IPOIN)/DT)
          S0TA%ADR(IMICFLC)%P%R(IPOIN) =           
     &                       -S0TA%ADR(IMICF_MACF)%P%R(IPOIN)
        ENDIF
      
      ENDDO


      RETURN
      END