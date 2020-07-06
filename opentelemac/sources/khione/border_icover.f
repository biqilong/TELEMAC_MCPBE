!                     ************************
                      SUBROUTINE BORDER_ICOVER
!                     ************************
!
     &( U,V,TWAT, ROEAU, DT, MESH )
!
!***********************************************************************
! KHIONE   V8P0
!***********************************************************************
!
!brief    COMPUTES PRESENCE OF STATIC BORDER ICE
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        20/06/2018
!+        V7P3
!+        Coupling TELEMAC-2D with KHIONE (ice modelling component)
!+        Initial developments
!
!history  S.E. BOURBAN (HRW)
!+        21/08/2018
!+        V7P3
!+  Use of ICETYPE to characterise and track expansion of both the
!+    dynamic and the static border ice. ICETYPE is a multiplicative
!+    combination of prime numbers defined as follows:
!+    1 - open water
!+    2 - domain boundary node
!+    3 - static border ice (within or at the edge of the cover)
!+    7 - border ice (both static and dynamic, and including the edge)
!+    5 - edge of the border ice where dynamic border ice accumulates
!+  Note that dynamic border ice (where ice is being accumulated),
!+    cannot happen where static border ice has formed
!+  The following combination exist:
!+    1 - open water
!+    7 - node within the dynamic border ice cover
!+    14 - dynamic border ice node on the edge of the domain not not
!+      on the edge of the dynamic border ice cover.
!+    21 - node within the static border ice cover
!+    35 - node on the edge of the dynamic border ice cover, where
!+      dynamic border ice is being accumulated
!+    42 - same as 21 but also on the edge of the domain
!+    70 - same as 35 but on the edge of the domain
!+  TODO: This method still need to be updated to account for the
!+    melting down of border ice (in patches if possible)
!
!history  R. ATA (LNHE)
!+        21/08/2018
!+        V8P0
!+  Few optimizations and typos
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| CURRENT TIME IN SECONDS
!| LT             |-->| CURRENT NUMBER OF OF TIME STEP
!| NPOIN          |-->| NUMBER OF NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE, ONLY:ICEPROCESS,ANFEM,THIFEMF,THIFEMS,
     &                              DWB,ANFEM0,IT1,IT2,T1,T2,
     &                              ICETYPE,VCRBOR,VCRBOM,TCR,VZ,
     &                              LH_ICE,LIN_WATAIR,TC,TMELT
      USE METEO_KHIONE       ,ONLY: TAIR
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U,V,TWAT
      DOUBLE PRECISION, INTENT(IN)    :: ROEAU,DT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          I,J,K,N1,N2,IT,NPOIN,NELEM,NELMAX
      DOUBLE PRECISION THICK, USTAR,R,DPHI,DW,VMAG,VB
!
!-----------------------------------------------------------------------
!
      NPOIN = MESH%NPOIN
      NELEM = MESH%NELEM
      NELMAX = MESH%NELMAX
!
!
!=======================================================================
!
!     7 - STATIC BORDER ICE GROWTH
!
!-----------------------------------------------------------------------
!
      IF( INT(ICEPROCESS/7)*7 .EQ. ICEPROCESS ) THEN
!
!-----------------------------------------------------------------------
!
!     MAYBE THIS SHOULD BE DONE ONCE /!\
!     TODO: T1 CAN BE REPLACED BY VOLUPAR (ALREADY COMPUTED)
!
        CALL VECTOR(T1,'=','MASBAS          ',U%ELM,1.D0,
     &              T2,T2,T2,T2,T2,T2,MESH,.FALSE.,T2)
!       ASSEMBLING THE SUM OF ALL VALUES
        IF( NCSIZE.GT.1 ) CALL PARCOM( T1,2,MESH )
!
!-----------------------------------------------------------------------
!
!     LOOKING FOR NEW BORDER ICE NODES (TO BE SWITCHED))
!
        DO I = 1,NPOIN
          IT = ICETYPE%I(I)
!
!         CURRENT VELOCITY
          VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!         BOYANT VELOCITY
          VB = MAX( 0.D0, -0.025D0*TCR%R(I) + 0.005D0 )
!         BORDER ICE THICKNESS /!\ TODO: DOUBLE CHECK VALIDITY
          THICK = THIFEMS%R(I)+THIFEMF%R(I)
!         VELOCITY CRITERIA FOR DYNAMIC BORDER ICE GROWTH
          USTAR = MAX( 0.175D0, VMAG / MAX(VCRBOM,1.D-12) )
!
!         IT1 IS USED AS A STATIC ICE MASK DEFINING NODES FOR WHICH
!         THE THRESHOLDS ARE TRUE (I.E. SUBJECT TO BE BODER ICE NODE).
!         - IT1 = 0 IF THRESHOLDS ARE --NOT-- TRUE
!         - IT1 = 1 IF THRESHOLDS ARE TRUE
!         - IT1 = 2 IF THRESHOLD ARE TRUE AND IS ALSO ON THE DOMAIN
!         BORDER IT IS NOTED THAT IF IT1 = 2, THEN THE NODE IS
!         --NOT-- BORDER ICE ALREADY, BUT ABOUT TO BE SWITCHED.
          IT1%I(I) = 0
!
!     > THRESHOLDS FOR STATIC BORDER ICE FORMATION
          IF( TCR%R(I).LE.(TC-TMELT%R(I)) .AND. ! THERMAL PROPERTY
     &        VB.GT.1.1*VZ%R(I) .AND.           ! BOYANT VS. TURBULANCE
     &        VMAG.LT.VCRBOR ) THEN             ! CRITICAL VELOCITY
            IF( INT(IT/3)*3 .NE. IT ) THEN
!           NODE OUTSIDE STATIC BORDER ICE COVER (NOT MULTIPLIER OF 3)
              IT1%I(I) = 1
!             BORDER NODE ABOUT TO BE SWITCH TO STATIC BORDER ICE
              IF( INT(IT/2)*2 .EQ. IT ) IT1%I(I) = 2
            ENDIF
          ENDIF
!
!         IT2 IS USED AS A DYNAMIC ICE MASK DEFINING NODES FOR WHICH
!         THE THRESHOLDS ARE TRUE (I.E. SUBJECT TO BE BODER ICE NODE).
!         - IT2 = 0 IF THRESHOLDS ARE --NOT-- TRUE
!         - IT2 = 1 IF THRESHOLDS ARE TRUE
!         - IT2 = 2 IF THRESHOLD ARE TRUE AND IS ALSO ON THE DOMAIN
!         BORDER IT IS NOTED THAT IF IT2 = 2, THEN THE NODE IS
!         --NOT-- BORDER ICE ALREADY, BUT ABOUT TO BE SWITCHED.
          IT2%I(I) = 0
!
!     > THRESHOLDS FOR DYNAMIC BORDER ICE FORMATION
          IF( !THICK.GE.0.0D0 .AND.              ! CRITICAL THICKNESS
     &        USTAR.LE.1.D0 ) THEN              ! CRITICAL VELOCITY
            IF( INT(IT/5)*5 .NE. IT .AND. INT(IT/7)*7 .NE. IT ) THEN
              IT2%I(I) = 1
!             BORDER NODE ABOUT TO BE SWITCH TO STATIC BORDER ICE
              IF( INT(IT/2)*2 .EQ. IT ) IT2%I(I) = 2
!
            ELSE !IF( INT(IT/5)*5 .EQ. IT ) THEN
!     > FURTHER GROWING DYNAMIC ICE
              R = 14.1 * USTAR ** (-0.93) * ANFEM%R(I) ** 1.08
              DPHI = LIN_WATAIR*( TWAT%R(I) - TAIR%R(I) )
              DW = DPHI * R / ( ROEAU * LH_ICE ) * DT
              DWB%R(I) = MIN( 1.D0, DWB%R(I) + DW / SQRT( T1%R(I) ) )
!
            ENDIF
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
!     CHECK BORDER NODES ABOUT TO BE SWITCHED INTO BORDER ICE NODES
!
        DO J = 1,NELEM
!
          DO K = 1,3
! RA: why we compute again node indices ? They are stored in IKLE (NELEM,3)
!      1- introduce IKLE as an argument like IKLE(NELEM,3)
!      2- nodes of element IELEM are :
!              I1 = IKLE(IELEM,1)
!              I2 = IKLE(IELEM,2)
!              I3 = IKLE(IELEM,3)
!
            I = MESH%IKLE%I( J + (K-1)*NELMAX )
!
            IT = ICETYPE%I(I)
!           NODES ABOUT TO BE SWITCHED NEED TO BE BELOW THRESHOLDS AND
!           --NOT-- ALREADY BODER ICE NODES. THEY ALSO NEED TO BE CLOSE
!           TO TWO OTHER (SUFFICIENTLY THICK) BORDER ICE NODES.
            IF( IT1%I(I).EQ.1 ) THEN
              N1 = ICETYPE%I( MESH%IKLE%I( J + MOD(K,3)*NELMAX ) )
              N2 = ICETYPE%I( MESH%IKLE%I( J + MOD(K+1,3)*NELMAX ) )
              IF( INT(N1/3)*3 .EQ. N1 .AND. INT(N2/3)*3 .EQ. N2 ) THEN
                IT1%I(I) = 2
              ENDIF
            ENDIF
            IF( IT2%I(I).EQ.1 ) THEN
              N1 = ICETYPE%I( MESH%IKLE%I( J + MOD(K,3)*NELMAX ) )
              N2 = ICETYPE%I( MESH%IKLE%I( J + MOD(K+1,3)*NELMAX ) )
              IF( INT(N1/7)*7 .EQ. N1 .AND. INT(N2/7)*7 .EQ. N2 ) THEN
                IT2%I(I) = 2
              ENDIF
            ENDIF
          ENDDO
!
        ENDDO
!
!     ASSEMBLING THE MAX OF ALL VALUES - THE TWOS WILL BE SWITCHED
!     CONVERSION BETWEEN INTEGER AND DOUBLE IS NECESSARY FOR PARCOM
        IF(NCSIZE.GT.1)CALL PARCOM2I(IT1%I,IT2%I,IT2%I,NPOIN,1,3,2,MESH)
!
!-----------------------------------------------------------------------
!
!     SWITCHES BORDER NODES INTO STATIC BORDER ICE NODES
!
        DO I = 1,NPOIN
          IT = ICETYPE%I(I)
!
          IF( IT1%I(I).EQ.2 ) THEN
!     > STATIC BORDER ICE
            ICETYPE%I(I) = ICETYPE%I(I) * 3           !=> INSTANT SWITCH
!
!         SWITCH FROM SOLID DYNAMIC TO STATIC BORDER ICE
            IF( INT(IT/7)*7 .EQ. IT ) THEN
!
!         SWITCH FROM GROWING DYNAMIC TO STATIC BORDER ICE
            ELSEIF( INT(IT/5)*5 .EQ. IT ) THEN
!           USE DWB TO COMPUTE THIFEMS, THIFEMF AND ANFEM ?
              ICETYPE%I(I) = ICETYPE%I(I) * 7         !=> INSTANT SWITCH
              ICETYPE%I(I) = ICETYPE%I(I) / 5         !=> INSTANT SWITCH
!
!         NEW STATIC BORDER ICE
            ELSE
!           NOT PART OF THE DYNAMIC ICE COVER
              ICETYPE%I(I) = ICETYPE%I(I) * 7         !=> INSTANT SWITCH
              THIFEMS%R(I) = 0.D0
              THIFEMF%R(I) = 0.D0
              ANFEM%R(I) = ANFEM0
!
            ENDIF
!
!     > DYNAMIC BORDER ICE
          ELSEIF( IT2%I(I).EQ.2 ) THEN
            IF( INT(IT/5)*5 .NE. IT ) THEN
              ICETYPE%I(I) = ICETYPE%I(I) * 5         !=> INSTANT SWITCH
              THIFEMS%R(I) = 0.D0
              THIFEMF%R(I) = 0.D0
              ANFEM%R(I) = ANFEM0
            ENDIF
          ENDIF
!
!     > EXPANDING DYNAMIC BORDER ICE COVER OVER THE EDGE
          IF( INT(ICETYPE%I(I)/5)*5 .EQ. ICETYPE%I(I) .AND.
     &        DWB%R(I).GE.1.D0 ) THEN
            ICETYPE%I(I) = ICETYPE%I(I) / 5           !=> INSTANT SWITCH
            IF( INT(ICETYPE%I(I)/7)*7 .NE. ICETYPE%I(I) ) THEN
              ICETYPE%I(I) = ICETYPE%I(I) * 7         !=> INSTANT SWITCH
            ENDIF
          ENDIF
!
        ENDDO
!
      ENDIF
!
!
!=======================================================================
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BORDER_ICOVER
