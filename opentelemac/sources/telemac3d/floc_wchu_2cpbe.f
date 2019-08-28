!                    **************************
                     SUBROUTINE FLOC_WCHU_2CPBE
!                    **************************
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    COMPUTE SETTLING VELOCITY FOR MCPBE FLOCCULATION MODEL
!+              THIS SUBROUTINE IS ONLY USED FOR 2CPBE CASE.
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
      USE DECLARATIONS_TELEMAC3D, ONLY: PRIVE,RHO0,NPOIN3,TA,WCHU,
     &                                  FLOCMAC_DEN,FLOCMAC_DIA 
      USE DECLARATIONS_GAIA, ONLY: HINDER,IMICFLC,IMACFLC,IMICF_MACF,
     &                         FRACDIM_MAC,FLOCMIC_DIAFIX,FLOCMIC_DEN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: NC, CVOL
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: PHI_HSP, PHI_HSF
      DOUBLE PRECISION, PARAMETER :: DVISC_W = 1.02D-3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO IPOIN = 1,NPOIN3

         NC(IPOIN) =  MAX(2.D0,
     &      TA%ADR(IMICF_MACF)%P%R(IPOIN)/TA%ADR(IMACFLC)%P%R(IPOIN))
  
         FLOCMAC_DEN%R(IPOIN) = RHO0+(FLOCMIC_DEN-RHO0)
     &                          *NC(IPOIN)**(1.D0-3.D0/FRACDIM_MAC)

         FLOCMAC_DIA%R(IPOIN) = 
     &                   NC(IPOIN)**(1.D0/FRACDIM_MAC)*FLOCMIC_DIAFIX

         CVOL(IPOIN) = (TA%ADR(IMICFLC)%P%R(IPOIN)
     &                 +TA%ADR(IMICF_MACF)%P%R(IPOIN))/(FLOCMIC_DEN)
  
         IF(HINDER) THEN
            PHI_HSP(IPOIN) = (1.D0-CVOL(IPOIN))**4.D0
         ELSE
            PHI_HSP(IPOIN) = 1.D0
         ENDIF  
  
         WCHU%ADR(IMICFLC)%P%R(IPOIN) = 
     &                       (1.D0/18.D0*9.81*(FLOCMIC_DEN/RHO0-1.D0)
     &                       *FLOCMIC_DIAFIX**2.D0)/DVISC_W

         WCHU%ADR(IMICFLC)%P%R(IPOIN) = 
     &                    PHI_HSP(IPOIN)*WCHU%ADR(IMICFLC)%P%R(IPOIN)
  
         IF(HINDER) THEN
            PHI_HSF(IPOIN) = (1.D0-CVOL(IPOIN))**4.D0
         ELSE
            PHI_HSF(IPOIN) = 1.D0
         ENDIF
  
         WCHU%ADR(IMACFLC)%P%R(IPOIN) = 
     &              (1.D0/18.D0*9.81*(FLOCMAC_DEN%R(IPOIN)/RHO0-1.D0)
     &              *FLOCMAC_DIA%R(IPOIN)**2.D0)/DVISC_W

         WCHU%ADR(IMACFLC)%P%R(IPOIN) = 
     &                    PHI_HSF(IPOIN)*WCHU%ADR(IMACFLC)%P%R(IPOIN)

         WCHU%ADR(IMICF_MACF)%P%R(IPOIN)=WCHU%ADR(IMACFLC)%P%R(IPOIN)
  
!        WRITE INTO RESULTS
         PRIVE%ADR(1)%P%R(IPOIN)=FLOCMAC_DIA%R(IPOIN)
         PRIVE%ADR(2)%P%R(IPOIN)=FLOCMAC_DEN%R(IPOIN)
         PRIVE%ADR(3)%P%R(IPOIN)=WCHU%ADR(IMACFLC)%P%R(IPOIN)
  
      ENDDO

      WRITE(LU,*) WCHU%ADR(IMICFLC)%P%R(141),
     &    WCHU%ADR(IMACFLC)%P%R(141),WCHU%ADR(IMICF_MACF)%P%R(141),
     &    NC(141),TA%ADR(IMICFLC)%P%R(141),TA%ADR(IMACFLC)%P%R(141),
     &    TA%ADR(IMICF_MACF)%P%R(141),FLOCMAC_DEN%R(141),
     &    FLOCMAC_DIA%R(141),CVOL(141)
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
      RETURN
      END