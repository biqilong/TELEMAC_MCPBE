!                    ******************************
                     SUBROUTINE FLOC_KINETICS_3CPBE
!                    ******************************
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    COMPUTE FLOCCULATION KINETICS FOR MCPBE FLOCCULATION MODEL
!+               THIS SUBROUTINE IS ONLY USED FOR 3CPBE CASE.
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
     &                  FLOCMIC_DIA,FLOCMAC_DIA,FLOCMEG_DIA,RHO0,WCHU,
     &                  NPOIN2,UETCAR,H
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: CNUM_P,CNUM_F1,CNUM_T1,
     &                                   CNUM_F2,CNUM_T2,NC1,NC2,SHR_G
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: BETA_PP,BETA_PF1,BETA_PF2,
     &                                   BETA_F1F1,BETA_F1F2,BETA_F2F2
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: BRK_SH1,BRK_SH2
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: ABSS_P, ABSS_F1, ABSS_T1,
     &                                       ABSS_F2, ABSS_T2
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: DIN,KL_CAP,BIOGR
      DOUBLE PRECISION :: FLOCMIC_VOL,WCHU_P,WCHU_F1,WCHU_F2
!
      DOUBLE PRECISION, PARAMETER :: BIOGR_MAX =  0.D0
      DOUBLE PRECISION, PARAMETER :: OMG1      =  0.0467D0
      DOUBLE PRECISION, PARAMETER :: GAMMA_F   =  0.6D0
      DOUBLE PRECISION, PARAMETER :: DIN_HS    =  0.D0
      DOUBLE PRECISION, PARAMETER :: VMU       =  1.002D-3
      DOUBLE PRECISION, PARAMETER :: C1        =  0.5D0
      DOUBLE PRECISION, PARAMETER :: C2        =  0.5D0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      IF(FLOC .AND. FLOC_TYPE.EQ.3 .AND. MCPBE_VER.GE.2) THEN
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
!       SOURCE TERM FOR MEGAFLOCS
        S0TA%ADR(IMEGFLC)%P%TYPR='Q'
        S1TA%ADR(IMEGFLC)%P%TYPR='0'
        CALL OS('X=C     ',S0TA%ADR(IMEGFLC)%P,C=0.D0)
        CALL OS('X=C     ',S1TA%ADR(IMEGFLC)%P,C=0.D0)
!       SOURCE TERM FOR MICROFLOCS INSIDE MACROFLOCS
        S0TA%ADR(IMICF_MACF)%P%TYPR='Q'
        S1TA%ADR(IMICF_MACF)%P%TYPR='0'
        CALL OS('X=C     ',S0TA%ADR(IMICF_MACF)%P,C=0.D0)
        CALL OS('X=C     ',S1TA%ADR(IMICF_MACF)%P,C=0.D0)
!       SOURCE TERM FOR MICROFLOCS INSIDE MEGAFLOCS
        S0TA%ADR(IMICF_MEGF)%P%TYPR='Q'
        S1TA%ADR(IMICF_MEGF)%P%TYPR='0'
        CALL OS('X=C     ',S0TA%ADR(IMICF_MEGF)%P,C=0.D0)
        CALL OS('X=C     ',S1TA%ADR(IMICF_MEGF)%P,C=0.D0)
!       WRITE(LU,*) 'TIME STEP IN GAIA IS ',DT
      ENDIF

!     COMPUTE FLOCCULATION KINETICS
      DO IPOIN = 1,NPOIN3

!       Modification for testing TCBPE kinetics only
!       SHR_G(IPOIN) = 7.31D0 !FOR VAL LEUSSEN'S SETTLING COLUMN EXPERIMENTS
!       Here, Please create a depth and shear velocity dependent "SHR_G"			 
        SHR_G(IPOIN) = SQRT(EP%R(IPOIN)/DNUVIV)
!       FOR THE BOTTOM NODE, EP COULD ALSO USE THEORETICAL VALUE (ML)            
        IF(IPOIN.LE.NPOIN2) THEN
          SHR_G(IPOIN) = SQRT(UETCAR%R(IPOIN)**1.5/(KARMAN*0.001)
     &                             * (1-0.001/H%R(IPOIN))/DNUVIV)
        ENDIF 

        KL_CAP(IPOIN) = GAMMA_F*(1+OMG1)*(1.D-6/SHR_G(IPOIN))
     &         **0.5D0/NC2(IPOIN)**(1.D0/FRACDIM_MEG)
        DIN(IPOIN) = 2.D-3

        IF(BIOGR_MAX .NE. 0.D0) THEN
          BIOGR(IPOIN) = BIOGR_MAX*DIN(IPOIN)/(DIN(IPOIN)+DIN_HS)
!         DT IN GAIA CAN BE DIFFERENT FROM DT_TEL IF MORFAC IS APPLIED
          FLOCMIC_DIA%R(IPOIN) = FLOCMIC_DIA%R(IPOIN)
     &                   +DT*BIOGR(IPOIN)*FLOCMIC_DIA%R(IPOIN) 
     &                   *(1.D0-FLOCMIC_DIA%R(IPOIN)/KL_CAP(IPOIN))
        END IF 

        FLOCMIC_VOL=(1.D0/6.D0)*PI*FLOCMIC_DIA%R(IPOIN)**3.D0

        CNUM_P(IPOIN) = 
     &         TA%ADR(IMICFLC)%P%R(IPOIN)/(FLOCMIC_VOL*FLOCMIC_DEN)
        CNUM_F1(IPOIN) = 
     &         TA%ADR(IMACFLC)%P%R(IPOIN)/(FLOCMIC_VOL*FLOCMIC_DEN)
        CNUM_T1(IPOIN) = 
     &      TA%ADR(IMICF_MACF)%P%R(IPOIN)/(FLOCMIC_VOL*FLOCMIC_DEN)
        CNUM_T2(IPOIN) = 
     &      TA%ADR(IMICF_MEGF)%P%R(IPOIN)/(FLOCMIC_VOL*FLOCMIC_DEN)
        
        NC1(IPOIN) = MAX(2.D0, CNUM_T1(IPOIN)/CNUM_F1(IPOIN))

        IF(MCPBE_VER .EQ. 2) THEN   ! FIXED 3RD CLASS
          NC2(IPOIN)=(FLOCMEG_DIAFIX/FLOCMIC_DIA%R(IPOIN))**FRACDIM_MEG
          CNUM_F2(IPOIN)= CNUM_T2(IPOIN)/NC2(IPOIN)
        ELSEIF(MCPBE_VER .EQ. 3) THEN ! VARYING 3RD CLASS
          CNUM_F2(IPOIN) = 
     &             TA%ADR(IMEGFLC)%P%R(IPOIN)/(FLOCMIC_VOL*FLOCMIC_DEN)
          NC2(IPOIN) = 
     &           MAX(NC1(IPOIN)*2.D0,CNUM_T2(IPOIN)/CNUM_F2(IPOIN))
        ENDIF

        WCHU_P  = WCHU%ADR(IMICFLC)%P%R(IPOIN)
        WCHU_F1 = WCHU%ADR(IMACFLC)%P%R(IPOIN)
        WCHU_F2 = WCHU%ADR(IMEGFLC)%P%R(IPOIN)

!       COLLISION FREQUENCY  
        BETA_PP(IPOIN) = (1.D0/6.D0)*(FLOCMIC_DIA%R(IPOIN) 
     &                  +FLOCMIC_DIA%R(IPOIN))**3.D0*SHR_G(IPOIN) ! TURBULENT SHEAR
     &                  +2.D0*KBOLZ*TEMPSTD/(3.D0*VMU)*4.D0       ! BROWIAN MOTION
            
        BETA_PF1(IPOIN) = (1.D0/6.D0)*(FLOCMIC_DIA%R(IPOIN)
     &           +FLOCMAC_DIA%R(IPOIN))**3.D0*SHR_G(IPOIN)      ! TURBULENT SHEAR
     &           +2.D0*KBOLZ*TEMPSTD/(3.D0*VMU)                 ! BROWIAN MOTION
     &           *(1.D0/FLOCMIC_DIA%R(IPOIN)+1.D0/FLOCMAC_DIA%R(IPOIN))
     &           *(FLOCMIC_DIA%R(IPOIN)+FLOCMAC_DIA%R(IPOIN))
     &           +PI/4.D0*(FLOCMIC_DIA%R(IPOIN)+FLOCMAC_DIA%R(IPOIN))
     &           **2.D0*ABS(WCHU_P-WCHU_F1)                     ! DIFFERENTIAL SETTLING
            
        BETA_PF2(IPOIN) = (1.D0/6.D0)*(FLOCMIC_DIA%R(IPOIN)
     &           +FLOCMEG_DIA%R(IPOIN))**3.D0*SHR_G(IPOIN)
     &           +2.D0*KBOLZ*TEMPSTD/(3.D0*VMU)
     &           *(1.D0/FLOCMIC_DIA%R(IPOIN)+1.D0/FLOCMEG_DIA%R(IPOIN))
     &           *(FLOCMIC_DIA%R(IPOIN)+FLOCMEG_DIA%R(IPOIN))
     &           +PI/4.D0*(FLOCMIC_DIA%R(IPOIN)+FLOCMEG_DIA%R(IPOIN))
     &           **2.D0*ABS(WCHU_P-WCHU_F2)
            
        BETA_F1F1(IPOIN) = SHR_G(IPOIN)*(1.D0/6.D0)
     &      *(FLOCMAC_DIA%R(IPOIN)+FLOCMAC_DIA%R(IPOIN))**3.D0
     &      +2.D0*KBOLZ*TEMPSTD/(3.D0*VMU)*4.D0
            
        BETA_F1F2(IPOIN) = SHR_G(IPOIN)*(1.D0/6.D0)
     &      *(FLOCMAC_DIA%R(IPOIN)+FLOCMEG_DIA%R(IPOIN))**3.D0
     &      +2.D0*KBOLZ*TEMPSTD/(3.D0*VMU)
     &      *(1.D0/FLOCMAC_DIA%R(IPOIN)+1.D0/FLOCMEG_DIA%R(IPOIN))
     &      *(FLOCMAC_DIA%R(IPOIN)+FLOCMEG_DIA%R(IPOIN))
     &      +PI/4.D0*(FLOCMAC_DIA%R(IPOIN)+FLOCMEG_DIA%R(IPOIN))
     &      **2.D0*ABS(WCHU_F1-WCHU_F2)
            
        BETA_F2F2(IPOIN) = SHR_G(IPOIN)*(1.D0/6.D0)
     &        *(FLOCMEG_DIA%R(IPOIN)+FLOCMEG_DIA%R(IPOIN))**3.D0
     &        +2.D0*KBOLZ*TEMPSTD/(3.D0*VMU)*4.D0

!       BREAKAGE FREQUENCY
!       Modification Proposed by Kyle Strom (2018) 
        BRK_SH1(IPOIN) = BRK_EFF*SHR_G(IPOIN)*((FLOCMAC_DIA%R(IPOIN)
     &                  -FLOCMIC_DIA%R(IPOIN))/FLOCMIC_DIA%R(IPOIN))
     &                  **(3.D0-FRACDIM_MAC)*(VMU*SHR_G(IPOIN)
     &                  *FLOCMAC_DIA%R(IPOIN)**2.D0/BRK_FY)
     &                  **BRK_Q
   !  &                  **(C1+C2*FLOCMAC_DIA%R(IPOIN)/((VMU*1.D-3)
   !  &                  **3.D0/EP%R(IPOIN))**0.25D0)

        BRK_SH2(IPOIN) = BRK_EFF*SHR_G(IPOIN)*((FLOCMEG_DIA%R(IPOIN)
     &                  -FLOCMIC_DIA%R(IPOIN))/FLOCMIC_DIA%R(IPOIN))
     &                  **(3.D0-FRACDIM_MEG)*(VMU*SHR_G(IPOIN)
     &                  *FLOCMEG_DIA%R(IPOIN)**2.D0/BRK_FY)
     &                  **BRK_Q
   !  &                  **(C1+C2*FLOCMEG_DIA%R(IPOIN)/((VMU*1.D-3)
   !  &                  **3.D0/EP%R(IPOIN))**0.25D0)
        
!       CALCULATE THE AGGREGATION AND BREAKAGE KERNELS
        ABSS_P(IPOIN) = -0.5D0*AGG_ALPHA*BETA_PP(IPOIN)
     &      *CNUM_P(IPOIN)*CNUM_P(IPOIN)*(NC1(IPOIN)/(NC1(IPOIN)-1.D0))
     &      -AGG_ALPHA*BETA_PF1(IPOIN)*CNUM_P(IPOIN)*CNUM_F1(IPOIN)
     &      -AGG_ALPHA*BETA_PF2(IPOIN)*CNUM_P(IPOIN)*CNUM_F2(IPOIN)
     &      +BRKFRAC_P1*NC1(IPOIN)*BRK_SH1(IPOIN)*CNUM_F1(IPOIN)
     &      +BRKFRAC_P2*NC2(IPOIN)*BRK_SH2(IPOIN)*CNUM_F2(IPOIN)    
         
        ABSS_F1(IPOIN) = 0.5D0*AGG_ALPHA*BETA_PP(IPOIN)
     &       *CNUM_P(IPOIN)*CNUM_P(IPOIN)/(NC1(IPOIN)-1.D0)
     &       -0.5D0*AGG_ALPHA*BETA_F1F1(IPOIN)
     &       *CNUM_F1(IPOIN)*CNUM_F1(IPOIN)
     &       *NC2(IPOIN)/NC1(IPOIN)/(NC2(IPOIN)/NC1(IPOIN)-1.D0) 
     &       -AGG_ALPHA*BETA_F1F2(IPOIN)*CNUM_F1(IPOIN)*CNUM_F2(IPOIN)
     &       +(BRK_K1-1.D0)*BRK_SH1(IPOIN)*CNUM_F1(IPOIN)   
     &       +BRK_K2*BRK_SH2(IPOIN)*CNUM_F2(IPOIN)
         
        ABSS_T1(IPOIN) = 0.5D0*AGG_ALPHA*BETA_PP(IPOIN)
     &      *(CNUM_P(IPOIN)*CNUM_P(IPOIN))*NC1(IPOIN)/(NC1(IPOIN)-1.D0)
     &      +AGG_ALPHA*BETA_PF1(IPOIN)*CNUM_P(IPOIN)*CNUM_F1(IPOIN)
     &      -0.5D0*AGG_ALPHA*BETA_F1F1(IPOIN)
     &      *CNUM_F1(IPOIN)*CNUM_F1(IPOIN)
     &      *NC2(IPOIN)/(NC2(IPOIN)/NC1(IPOIN)-1.D0)   
     &      -NC1(IPOIN)*AGG_ALPHA*BETA_F1F2(IPOIN)
     &      *CNUM_F1(IPOIN)*CNUM_F2(IPOIN)
     &      -BRKFRAC_P1*NC1(IPOIN)*BRK_SH1(IPOIN)*CNUM_F1(IPOIN)  
     &      +(1.D0-BRKFRAC_P2-BRKFRAC_F2)*NC2(IPOIN)        
     &      *BRK_SH2(IPOIN)*CNUM_F2(IPOIN)

        IF(MCPBE_VER .EQ. 2) THEN
          ABSS_F2(IPOIN) = 0.D0
!         ABSS_F2(IPOIN) = 0.5D0*AGG_ALPHA*BETA_F1F1(IPOIN)
!    &      *CNUM_F1(IPOIN)*CNUM_F1(IPOIN)/(NC2(IPOIN)/NC1(IPOIN)-1.D0)
!    &      -0.5D0*AGG_ALPHA*BETA_F2F2(IPOIN)
!    &      *CNUM_F2(IPOIN)*CNUM_F2(IPOIN)
!    &      +(BRK_K3-1.D0)*BRK_SH2(IPOIN)*CNUM_F2(IPOIN)          
        ELSEIF(MCPBE_VER .EQ. 3) THEN  
          ABSS_F2(IPOIN) = 0.5D0*AGG_ALPHA*BETA_F1F1(IPOIN)
     &      *CNUM_F1(IPOIN)*CNUM_F1(IPOIN)/(NC2(IPOIN)/NC1(IPOIN)-1.D0)
     &      -0.5D0*AGG_ALPHA*BETA_F2F2(IPOIN)
     &      *CNUM_F2(IPOIN)*CNUM_F2(IPOIN)
     &      +(BRK_K3-1.D0)*BRK_SH2(IPOIN)*CNUM_F2(IPOIN)
        ENDIF

        ABSS_T2(IPOIN) = AGG_ALPHA*BETA_PF2(IPOIN)
     &                 *CNUM_P(IPOIN)*CNUM_F2(IPOIN)
     &                 +0.5D0*AGG_ALPHA*BETA_F1F1(IPOIN)
     &                 *CNUM_F1(IPOIN)*CNUM_F1(IPOIN)
     &                 *NC2(IPOIN)/(NC2(IPOIN)/NC1(IPOIN)-1.D0)
     &                 +NC1(IPOIN)*AGG_ALPHA*BETA_F1F2(IPOIN)
     &                 *CNUM_F1(IPOIN)*CNUM_F2(IPOIN)
     &                 -(1.D0-BRKFRAC_F2)*NC2(IPOIN)
     &                 *BRK_SH2(IPOIN)*CNUM_F2(IPOIN)

        S0TA%ADR(IMICFLC)%P%R(IPOIN) = MAX(
     &                       ABSS_P(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN,
     &                       -TA%ADR(IMICFLC)%P%R(IPOIN)/DT)
        S0TA%ADR(IMACFLC)%P%R(IPOIN) = MAX(
     &                       ABSS_F1(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN,
     &                       -TA%ADR(IMACFLC)%P%R(IPOIN)/DT)
        S0TA%ADR(IMICF_MACF)%P%R(IPOIN) = MAX(
     &                       ABSS_T1(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN,
     &                       -TA%ADR(IMICF_MACF)%P%R(IPOIN)/DT)
        S0TA%ADR(IMEGFLC)%P%R(IPOIN) = MAX(
     &                       ABSS_F2(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN,
     &                       -TA%ADR(IMEGFLC)%P%R(IPOIN)/DT)
        S0TA%ADR(IMICF_MEGF)%P%R(IPOIN) = MAX(
     &                       ABSS_T2(IPOIN)*FLOCMIC_VOL*FLOCMIC_DEN,
     &                       -TA%ADR(IMICF_MEGF)%P%R(IPOIN)/DT)

      ENDDO


      RETURN
      END