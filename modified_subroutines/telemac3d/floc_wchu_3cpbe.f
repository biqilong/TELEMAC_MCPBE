!                    **************************
                     SUBROUTINE FLOC_WCHU_3CPBE
!                    **************************
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    COMPUTE SETTLING VELOCITY FOR MCPBE FLOCCULATION MODEL
!+              THIS SUBROUTINE IS ONLY USED FOR 3CPBE CASE.
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
      USE DECLARATIONS_TELEMAC3D, ONLY: NPOIN3,TA,WCHU,FLOCMIC_DIA,
     &                                  FLOCMAC_DEN,FLOCMAC_DIA,PRIVE,
     &                                  FLOCMEG_DEN,FLOCMEG_DIA,RHO0
      USE DECLARATIONS_GAIA, ONLY: MCPBE_VER,HINDER,IMICFLC,IMACFLC,
     &                             IMEGFLC,IMICF_MACF,IMICF_MEGF,
     &                             FRACDIM_MAC,FRACDIM_MEG,
     &                             FLOCMIC_DEN,FLOCMEG_DIAFIX
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN
      DOUBLE PRECISION, DIMENSION(NPOIN3) :: NC1,NC2,CVOL,PHI_HS
      DOUBLE PRECISION            :: FLOCMIC_DIAM
      DOUBLE PRECISION, PARAMETER :: DVISC_W = 1.02D-3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO IPOIN = 1,NPOIN3
        
        FLOCMIC_DIAM = FLOCMIC_DIA%R(IPOIN)

        IF(MCPBE_VER .EQ. 2) THEN

          FLOCMEG_DIA%R(IPOIN) = FLOCMEG_DIAFIX
          NC2(IPOIN) = (FLOCMEG_DIAFIX/FLOCMIC_DIAM)**FRACDIM_MEG
          FLOCMEG_DEN%R(IPOIN) = RHO0+(FLOCMIC_DEN-RHO0)
     &                            *NC2(IPOIN)**(1.D0-3.D0/FRACDIM_MEG)
          NC1(IPOIN) =  MAX(2.D0,
     &        TA%ADR(IMICF_MACF)%P%R(IPOIN)/TA%ADR(IMACFLC)%P%R(IPOIN))  
          FLOCMAC_DEN%R(IPOIN) = RHO0+(FLOCMIC_DEN-RHO0)
     &                            *NC1(IPOIN)**(1.D0-3.D0/FRACDIM_MAC)
          FLOCMAC_DIA%R(IPOIN) = 
     &                  NC1(IPOIN)**(1.D0/FRACDIM_MAC)*FLOCMIC_DIAM

        ELSEIF(MCPBE_VER .EQ. 3) THEN

          NC1(IPOIN) =  MAX(2.D0,
     &        TA%ADR(IMICF_MACF)%P%R(IPOIN)/TA%ADR(IMACFLC)%P%R(IPOIN))
          FLOCMAC_DEN%R(IPOIN) = RHO0+(FLOCMIC_DEN-RHO0)
     &                            *NC1(IPOIN)**(1.D0-3.D0/FRACDIM_MAC)
          FLOCMAC_DIA%R(IPOIN) = 
     &                  NC1(IPOIN)**(1.D0/FRACDIM_MAC)*FLOCMIC_DIAM
          NC2(IPOIN) = MAX(2.D0*NC1(IPOIN),
     &        TA%ADR(IMICF_MEGF)%P%R(IPOIN)/TA%ADR(IMEGFLC)%P%R(IPOIN))
          FLOCMEG_DEN%R(IPOIN) = RHO0+(FLOCMIC_DEN-RHO0)
     &                            *NC2(IPOIN)**(1.D0-3.D0/FRACDIM_MEG)
          FLOCMEG_DIA%R(IPOIN) = 
     &                  NC2(IPOIN)**(1.D0/FRACDIM_MEG)*FLOCMIC_DIAM

        ENDIF

        CVOL(IPOIN) = (TA%ADR(IMICFLC)%P%R(IPOIN)
     &                +TA%ADR(IMICF_MACF)%P%R(IPOIN)
     &                +TA%ADR(IMICF_MEGF)%P%R(IPOIN))/(FLOCMIC_DEN)

!       SETTLING VELOCITY WITH/WITHOUT HINDERED SETTLING
        IF(HINDER) THEN
          PHI_HS(IPOIN) = (1.D0-MIN(CVOL(IPOIN),0.7D0))**5.5D0
        ELSE
          PHI_HS(IPOIN) = 1.D0
        ENDIF
  
        WCHU%ADR(IMICFLC)%P%R(IPOIN) = 
     &                  1.D0/18.D0*9.81*(FLOCMIC_DEN/RHO0-1.D0)/DVISC_W
     &                      *(FLOCMIC_DIAM**2.D0)
        WCHU%ADR(IMACFLC)%P%R(IPOIN) = 
     &                  1.D0/18.D0*9.81*(FLOCMIC_DEN/RHO0-1.D0)/DVISC_W
     &                      *FLOCMIC_DIAM**(3.D0-FRACDIM_MAC)
     &                      *FLOCMAC_DIA%R(IPOIN)**(FRACDIM_MAC-1.D0)
        WCHU%ADR(IMEGFLC)%P%R(IPOIN) = 
     &                  1.D0/18.D0*9.81*(FLOCMIC_DEN/RHO0-1.D0)/DVISC_W
     &                      *FLOCMIC_DIAM**(3.D0-FRACDIM_MEG)
     &                      *FLOCMEG_DIA%R(IPOIN)**(FRACDIM_MEG-1.D0)

        WCHU%ADR(IMICFLC)%P%R(IPOIN) = PHI_HS(IPOIN)*
     &                                    WCHU%ADR(IMICFLC)%P%R(IPOIN)
        WCHU%ADR(IMACFLC)%P%R(IPOIN) = PHI_HS(IPOIN)*
     &                                    WCHU%ADR(IMACFLC)%P%R(IPOIN)
        WCHU%ADR(IMEGFLC)%P%R(IPOIN) = PHI_HS(IPOIN)*
     &                                    WCHU%ADR(IMEGFLC)%P%R(IPOIN)

        WCHU%ADR(IMICF_MACF)%P%R(IPOIN) = WCHU%ADR(IMACFLC)%P%R(IPOIN)
        WCHU%ADR(IMICF_MEGF)%P%R(IPOIN) = WCHU%ADR(IMEGFLC)%P%R(IPOIN)

!       WRITE INTO RESULTS
        PRIVE%ADR(1)%P%R(IPOIN)=FLOCMAC_DIA%R(IPOIN)
        PRIVE%ADR(2)%P%R(IPOIN)=FLOCMEG_DIA%R(IPOIN)
        PRIVE%ADR(3)%P%R(IPOIN)=WCHU%ADR(IMACFLC)%P%R(IPOIN)
        PRIVE%ADR(3)%P%R(IPOIN)=WCHU%ADR(IMEGFLC)%P%R(IPOIN)
    
      ENDDO

      WRITE(LU,*) WCHU%ADR(IMICFLC)%P%R(141),
     &    WCHU%ADR(IMACFLC)%P%R(141),WCHU%ADR(IMICF_MACF)%P%R(141),
     &    WCHU%ADR(IMEGFLC)%P%R(141),WCHU%ADR(IMICF_MEGF)%P%R(141),
     &    NC1(141),NC2(141),TA%ADR(IMICFLC)%P%R(141),
     &    TA%ADR(IMACFLC)%P%R(141),TA%ADR(IMICF_MACF)%P%R(141),
     &    TA%ADR(IMEGFLC)%P%R(141),TA%ADR(IMICF_MEGF)%P%R(141),
     &    FLOCMAC_DEN%R(141),FLOCMAC_DIA%R(141),
     &    FLOCMEG_DEN%R(141),FLOCMEG_DIA%R(141),CVOL(141)
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
      RETURN
      END