!                      **********************
                       MODULE FREEZEUP_KHIONE
!                      **********************
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Module containing all subroutines to deal with the physics
!+        of freeze-up and frazil ice growth, at a node level.
!+
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        11/11/2017
!+        V7P3
!+        Initial developments
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: SUPER_COOLING,CLOGGED_ON_BAR
!
!=======================================================================
!
!       1) THERMAL FLUXES
!
      CONTAINS
!
!=======================================================================
!
!                    ************************
                     SUBROUTINE SUPER_COOLING
!                    ************************
!
     &  ( TWAT,FRZL,SRCT,SRCF, THETA0,THETA1,BETA1,VBB,THIFEMF,HUN,
     &    CONSTSS,ANFEM, DT,VMAG,DEPTH, ISBAR )
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    Super cooling process and frazil production.
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+   11/11/2017
!+   V7P3
!+   Initial implementation
!
!reference
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TWAT    |-->| WATER TEMPERATURE
!| FRZL    |-->| FRAZIL CONCENTRATION
!| SRCT    |<->| EXPLCIIT SOURCE OF TEMPERATURE
!| SRCF    |<->| EXPLICIT SOURCE OF FRAZIL
!| ANFEM   |-->| CONCENTRATION OF SURFACE ICE PARTICLES
!| DT      |-->| TIME STEP
!| VMAG    |-->| CURRENT VELOCITY MAGNITUDE
!| DEPTH   |-->| WATER DEPTH
!| ISBAR   |-->| WHETHER OR NOT ICE SETTLES AT THE SURFACE OR ON A BAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY : DF,DE,RHO_ICE,LH_ICE,
     &  AF,TC_WT,SURF_EF,VNU,CV0
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)     :: TWAT,FRZL,CONSTSS,ANFEM
      DOUBLE PRECISION, INTENT(IN)     :: THETA0,THETA1,BETA1
      DOUBLE PRECISION,INTENT(INOUT)   :: SRCT,SRCF
      DOUBLE PRECISION, INTENT(IN)     :: DT,VMAG,DEPTH,VBB,THIFEMF,HUN
      LOGICAL, INTENT(IN)              :: ISBAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,AF0,VF0,VNF,QF,DELVF,HE,PV
!     &  ,PT,VEXP,SPECW,VF0,AF0,CVI,CVMAX,QVPV
!
!-----------------------------------------------------------------------
!
!     FRAZIL CRYSTAL SIZES
      PI = 4.D0*ATAN(1.D0)
      VF0 = DF**2*PI*DE/4.0    ! VOLUME OF A SINGLE FRAZIL CRYSTAL
      AF0 = PI*DF*DE
!
      IF( FRZL.GT.CV0 ) THEN
        VNF = FRZL/VF0
      ELSE
        VNF = CV0/VF0
      ENDIF
      VNF = MAX( VNF, 1.D0 )   ! IF( VNF.LT.1.D0 ) VNF = 1.D0
!
      IF( TWAT.LT.0.D0 ) THEN
        QF = VNU*TC_WT/DE * ( 0.0-TWAT ) * AF0*VNF
        DELVF = QF/RHO_ICE/LH_ICE ! THERMAL GROWTH OF FRAZIL
      ELSE
        IF( FRZL.LE.CV0 ) THEN  ! CHANGE TO FRZL.LE.CV0, TW WILL CHANGE
          QF = 0.0              ! TW >= 0 AND CV <= CV0, QF = 0
          DELVF = 0.0           ! TEMPERAURE CHANGED IN FE CALCS
        ELSEIF (FRZL.GT.CV0 .AND. TWAT.NE.0.0) THEN
          QF = VNU*TC_WT/DE*(0.0-TWAT)*AF0*VNF
          DELVF = QF/RHO_ICE/LH_ICE  ! DECAY OF FRAZIL CONCENTRATION
          IF( DELVF.LT.(-FRZL/DT) ) THEN
            DELVF = -FRZL/DT  ! MAX AMOUNT OF DECAY IS AVAILABLE FRAZIL
            QF = DELVF*RHO_ICE*LH_ICE
          ENDIF
        ELSE
          QF = 0.0
          DELVF = 0.0
        ENDIF
      ENDIF
!
!     SOURCE TERM FOR WATER TEMPERATURE
      SRCT = CONSTSS*QF/( 1-FRZL ) +
     &  QF/RHO_ICE/LH_ICE*( 0.0-TWAT )/( 1-FRZL )
!
!     SOURCE TERM FOR FRAZIL CONCENTRATION
      IF( ISBAR ) THEN
!       NODES ON THE BAR: NO DEPOSITION ON THE WATER SURFACE
        PV = ( AF*VMAG )/DEPTH * FRZL
      ELSE
        PV = ( (THETA0*VBB*(1-ANFEM) ) +
     &    ( THETA1*VBB*ANFEM) )/DEPTH * FRZL
      ENDIF
!
      HE = ( 1.D0-SURF_EF ) * ( THIFEMF + HUN )
      SRCF = ( DELVF + ( (BETA1*HE*ANFEM)/ DEPTH ) - PV )
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SUPER_COOLING
!
!=======================================================================
!
!                    *************************
                     SUBROUTINE CLOGGED_ON_BAR
!                    *************************
!
     &  ( RFR0,RFR1,DB,BAR,NBAR,ANG1,FM1,FMT )
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    Computes the accumulated mass on vertical and/or
!         transverse bars.
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+   11/03/2018
!+   V7P3
!+   Initial implementation
!
!reference
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TWAT    |-->| WATER TEMPERATURE
!| FRZL    |-->| FRAZIL CONCENTRATION
!| SRCT    |<->| EXPLCIIT SOURCE OF TEMPERATURE
!| SRCF    |<->| EXPLICIT SOURCE OF FRAZIL
!| ANFEM   |-->| CONCENTRATION OF SURFACE ICE PARTICLES
!| DT      |-->| TIME STEP
!| VMAG    |-->| CURRENT VELOCITY MAGNITUDE
!| DEPTH   |-->| WATER DEPTH
!| ISBAR   |-->| WHETHER OR NOT ICE SETTLES AT THE SURFACE OR ON A BAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY : RHO_ICE,CLOG_EF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: NBAR
      DOUBLE PRECISION, INTENT(IN)    :: RFR0,RFR1,BAR,ANG1,DB
      DOUBLE PRECISION, INTENT(INOUT) :: FM1,FMT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: RB,DR,AMNU,ASH,RATIO,ADONT,AICE
!
!-----------------------------------------------------------------------
!
!      RB = RFR0/2.0/COS(ANG1)
      RB = DB/2.D0
      DR = 2*RB - RFR0
!
      AMNU = RFR0**2*COS(ANG1)*SIN(ANG1) + 2*ANG1*RB**2 -
     &       RB**2*COS(2*ANG1)*SIN(2*ANG1)
      ASH = 2*ANG1*RB**2 - (ANG1*RFR0**2 - RB*RFR0*SIN(ANG1)) ! SHADE AREA
!
      IF (RFR1.GT.2*RB) THEN
        AICE = ANG1*RFR1**2 - AMNU
      ELSE
        ADONT = ( RFR1**2 - RFR0**2 )*ANG1
        RATIO = ( RFR1 - RFR0 ) / DR
        AICE = ADONT - RATIO*ASH
      ENDIF
!
      FM1 = AICE*BAR*RHO_ICE*( 1.D0-CLOG_EF )
      FMT = FM1*NBAR
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CLOGGED_ON_BAR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE FREEZEUP_KHIONE
