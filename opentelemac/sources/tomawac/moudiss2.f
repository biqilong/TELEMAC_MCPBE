!                    *****************
                     SUBROUTINE MOUDISS2
     &  (FWX, FWY, NPOIN2, XK, NPLAN, FS,NF, TAUX1, F_INT)
!  SURFACE STRESS DUE TO WIND INPUT ENERGY AND WHITECAPPING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FS             |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FWX            |<--| SURFACE STRESS DUE TO WIND ALONG X
!| FWY            |<--| SURFACE STRESS DUE TO WIND ALONG Y 
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                    *****************
 
       USE DECLARATIONS_TOMAWAC, ONLY : FREQ, DFREQ, SINTET, COSTET,
     & DEUPI, CMOUT3,CMOUT4, CMOUT5, CMOUT6, VARIAN, FMOY, XKMOY,
     & DEPTH, USOLD, PROINF, GRAVIT
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN2, NPLAN,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F_INT(NPOIN2),TAUX1(NPOIN2)
!     
      DOUBLE PRECISION DTETAR, SIGMA, AUX, BETAMOU, AUX1
      DOUBLE PRECISION W, SURDEUPIFREQ, SQBSCMOUT4, SURCMOUT4
      DOUBLE PRECISION PO, P0O, KD, DEUKD
      DOUBLE PRECISION CG1, CPHAS, C3, C2, C1, BETAO, BETA, B
      INTEGER IP, JF, JP
      DTETAR=DEUPI/DBLE(NPLAN)
      C1 = - CMOUT5*DEUPI**9/GRAVIT**4
      C2 = - CMOUT5*DEUPI
      W = 25.D0
      SURCMOUT4 = 1.D0/CMOUT4
      IF (PROINF) THEN
 !       DEEP WATER CASE, ARRAY DEPENDING ONLY ON THE SPATIAL MESH NODE
        DO IP = 1,NPOIN2
          TAUX1(IP) = C1 * VARIAN(IP)**2 * FMOY(IP)**9
        ENDDO
      ELSE
!       FINITE DEPTH CASE
        DO IP=1,NPOIN2
          TAUX1(IP) = C2 * VARIAN(IP)**2 * FMOY(IP) * XKMOY(IP)**4
        ENDDO
      ENDIF
      DO JF=1,NF
        SIGMA=DEUPI*FREQ(JF)
        SURDEUPIFREQ=1.D0/(DEUPI*FREQ(JF))
        AUX1=DFREQ(JF)*DTETAR        
        DO IP=1,NPOIN2
          F_INT(IP)=FS(IP,1,JF)
        ENDDO
        DO JP=2,NPLAN
          DO IP=1,NPOIN2
            F_INT(IP)=F_INT(IP)+FS(IP,JP,JF)
          ENDDO
        ENDDO
        DO IP=1,NPOIN2
          F_INT(IP)=F_INT(IP)*DTETAR
        ENDDO
 !
        IF(PROINF) THEN
!
          DO IP = 1,NPOIN2
!
            CPHAS = XK(IP,JF)*SURDEUPIFREQ
            P0O = 3.D0+TANH(W*(USOLD(IP)*CPHAS-0.1D0))
            CG1 = 0.5D0*GRAVIT*SURDEUPIFREQ
            B   = CG1*F_INT(IP)*XK(IP,JF)**3
            SQBSCMOUT4=SQRT(B*SURCMOUT4)
!           COMPUTES THE BREAK/NON-BREAK TRANSITION
            PO = 0.5D0*(1.D0+TANH(10.D0*(SQBSCMOUT4-1.D0)))
!           COMPUTES THE BREAK BETA
            C3 = -CMOUT3*SQRT(GRAVIT*XK(IP,JF))
            BETAO = C3*SQBSCMOUT4**P0O
            BETAMOU = BETA+PO*(BETAO-BETA)
            
            DO JP=1,NPLAN
              FWX(IP)=FWX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &             *BETAMOU*FS(IP,JP,JF))*AUX1
              FWY(IP)=FWY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &             *BETAMOU*FS(IP,JP,JF))*AUX1
            ENDDO
          ENDDO
        ELSE
!       FINITE WATER DEPTH (USES K).
          DO IP=1,NPOIN2
            CPHAS = XK(IP,JF)*SURDEUPIFREQ
            KD=MIN(XK(IP,JF)*DEPTH(IP),350.D0)
            DEUKD=KD+KD
            CG1=( 0.5D0+XK(IP,JF)*DEPTH(IP)/SINH(DEUKD) )/CPHAS
            B = CG1*F_INT(IP)*XK(IP,JF)**3
            SQBSCMOUT4=SQRT(B*SURCMOUT4)
!           COMPUTES THE BREAK BETA
            C3=-CMOUT3*SQRT(GRAVIT*XK(IP,JF))
            AUX=TANH(KD)
            P0O=3.D0+TANH(W*(USOLD(IP)*CPHAS-0.1D0))
            BETAO=C3*SQBSCMOUT4**P0O*AUX**((2.D0-P0O)*0.25D0)
!           COMPUTES THE NON-BREAK BETA
            AUX = XK(IP,JF) / XKMOY(IP)
!           COMPUTES THE TOTAL BETA
            BETA=TAUX1(IP)*AUX*(1.D0-CMOUT6+CMOUT6*AUX)
!           COMPUTES THE BREAK/NON-BREAK TRANSITION
            PO = 0.5D0*(1.D0+TANH(10.D0*(SQBSCMOUT4-1.D0)))
            BETAMOU=BETA+PO*(BETAO-BETA)

            DO JP=1,NPLAN
              FWX(IP)=FWX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &             *BETAMOU*FS(IP,JP,JF))*AUX1
              FWY(IP)=FWY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &             *BETAMOU*FS(IP,JP,JF))*AUX1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END
      
