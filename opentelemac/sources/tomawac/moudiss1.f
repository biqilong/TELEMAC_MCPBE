!                    *****************
                     SUBROUTINE MOUDISS1
     &  (FWX, FWY, NPOIN2, XK, NPLAN, FS,NF)
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
     & DEUPI, VARIAN, FMOY, XKMOY, CMOUT1, CMOUT2, GRAVIT, PROINF
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN2, NPLAN,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
!
      DOUBLE PRECISION DTETAR, SIGMA, AUX, BETAMOU, TAUX1
      DOUBLE PRECISION AUX1, C1, C2
      INTEGER IP, JF, JP
      DTETAR=DEUPI/DBLE(NPLAN)
      C1 = - CMOUT1*DEUPI**9/GRAVIT**4
      C2 = - CMOUT1*DEUPI
      IF (PROINF) THEN
!
!       INFINITE WATER DEPTH (USES F).

!.....LOOP ON THE DISCRETISED FREQUENCIES
!     """"""""""""""""""""""""""""""""""""""""""""
        DO JF=1,NF
          SIGMA=DEUPI*FREQ(JF)
          AUX1=DFREQ(JF)*DTETAR        
          DO IP=1,NPOIN2
            AUX = (FREQ(JF)/FMOY(IP))**2
            TAUX1=C1 * VARIAN(IP)**2 * FMOY(IP)**9
            write(*,*) 'TAUX1',TAUX1
            BETAMOU=TAUX1*AUX*(1.D0-CMOUT2+CMOUT2*AUX)
            DO JP=1,NPLAN
              FWX(IP)=FWX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &             *BETAMOU*FS(IP,JP,JF))*AUX1
              FWY(IP)=FWY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &             *BETAMOU*FS(IP,JP,JF))*AUX1
            ENDDO
          ENDDO
        ENDDO
      ELSE
!       FINITE WATER DEPTH (USES K).
        DO JF=1,NF
          SIGMA=DEUPI*FREQ(JF)
          AUX1=DFREQ(JF)*DTETAR        
          DO IP=1,NPOIN2
            AUX = XK(IP,JF) / XKMOY(IP)
            TAUX1 = C2 * VARIAN(IP)**2 * FMOY(IP) * XKMOY(IP)**4
            BETAMOU=TAUX1*AUX*(1.D0-CMOUT2+CMOUT2*AUX)
            DO JP=1,NPLAN
              FWX(IP)=FWX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &             *BETAMOU*FS(IP,JP,JF))*AUX1
              FWY(IP)=FWY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &             *BETAMOU*FS(IP,JP,JF))*AUX1
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      RETURN
      END
      
