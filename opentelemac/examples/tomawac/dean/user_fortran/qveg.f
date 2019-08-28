!                    ***************
                     SUBROUTINE QVEG
!                    ***************
!
     &( TSTOT , TSDER , F , VARIAN ,  FMOY ,
     &  XKMOY , NF    , NPLAN  , NPOIN2   )
!
!***********************************************************************
! TOMAWAC   V7P0
!***********************************************************************
!
!brief    Takes into account the friction due to vegetation
!
!history  VITO BACCHI (EDF - LNHE)
!+        12/09/2014
!+        V7P0
!+   First version, on birthday eve...
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| FMOY           |-->| MEAN SPECTRAL FRQUENCY FMOY (relative frequency)
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| VARIAN         |-->| ENERGY SPECTRUM VARIANCE
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT,PI,X, DEPTH
!
      USE INTERFACE_TOMAWAC, EX_QVEG => QVEG
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NPLAN,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2),VARIAN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: FMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NPLAN,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION C1,CVEG,CD,NV,BV,ALFA,KH,AKH,RO,DV
      DOUBLE PRECISION AUX,NUM,DENUM,SHAKH, BETA
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!     DISSIPATION COEFFICIENTS OF VEGETATION
      RO = 1.D0
!     NUMBER OF PLANTS*SQM
      NV = 20.D0
!     PLANT AREA PER UNINT HEIGHT
      BV = 0.25D0
!     BULK DRAG COEFFICIENT
      CD = 0.2D0
!     VEGETATION HEIGHT
      DV = 1.D0
      ALFA = 0.1D0
      C1 = - SQRT(2.D0/PI)*GRAVIT**2
      CVEG = C1*CD*BV*NV/(DEUPI**3)
!
!     VEGETATION OVER A CONSTANT DEPTH
!     COMPUTES THE BETA COEFFICIENT : QVEG1 = BETA * F
!
      DO IP=1,NPOIN2
        IF(X(IP).LT.50.D0.OR.X(IP).GT.150.D0) THEN
          ALFA = 0.D0
        ELSE
          ALFA = DV/DEPTH(IP)
        ENDIF
        KH = XKMOY(IP)*DEPTH(IP)
        AKH = ALFA*KH
        SHAKH = SINH(AKH)
        NUM = SHAKH*(SHAKH**2 + 3.D0)
        DENUM = 3.D0*XKMOY(IP)*COSH(KH)**3
        AUX = (XKMOY(IP)/FMOY(IP))**3
        BETA = RO*CVEG*AUX*(NUM/DENUM)*SQRT(VARIAN(IP))
        DO JF=1,NF
          DO JP=1,NPLAN
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETA*F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETA
          ENDDO
        ENDDO
      ENDDO
!
!
!     LOOP OVER THE DISCRETISED FREQUENCIES
!
!     TAKES THE SOURCE TERM INTO ACCOUNT
!

!
!-----------------------------------------------------------------------
!
      RETURN
      END
