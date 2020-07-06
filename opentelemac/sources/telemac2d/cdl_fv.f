!                       *****************
                        SUBROUTINE CDL_FV
!                       *****************
!
     &(LIMPRO,W,CE,FLUENT,FLUSORT,FLBOR,FLUHBTEMP)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  COMPUTATION OF THE CONVECTIVE FLUXES AT BOUNDARIES
!!        FOR FINITE VOLUME SCHEMES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  CE         FLUX
!>@param  [in,out]  FLBOR      IN AND OUT WATER MASS FLUX
!>@param  [in,out]  FLUENT     ENTERING MASS FLUX
!>@param  [in,out]  FLUHBTEMP  BOUNDARY FLUX FOR THE TRACER
!>@param  [in,out]  FLUSORT    EXITING MASS FLUX
!>@param  [in]      LIMPRO     TYPES OF BOUNDARY CONDITION
!>@param  [in]      W          WORKING TABLE CONTAINING H,HU,HV
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC,ONLY: KDIR,KENT,KDDL,KNEU,KENTU
      USE DECLARATIONS_TELEMAC2D,ONLY:NUMLIQ,LIUBOR,ENTET,ICIN,NTRAC,
     &                           NPOIN,MESH,UBOR,VBOR,HBOR,ZF,GRAV,
     &                           NPTFR,EPS_FV
      USE INTERFACE_TELEMAC2D, EX_CDL_FV => CDL_FV
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NPOIN,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUHBTEMP
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,IDRY,ITRAC
!
      DOUBLE PRECISION :: VNX,VNY,XNN,YNN,VNL(NPTFR)
      DOUBLE PRECISION :: UNN,VNN,LAMBDA1,LAMBDA2
      DOUBLE PRECISION :: FLX(NPTFR,3), FLXG(3)
      DOUBLE PRECISION :: H1,U10,U1,V1,UGN,C1
      DOUBLE PRECISION :: HG,UG,VG,CG,UGTEMP
      DOUBLE PRECISION :: OUTFLOW,REGIME,DX
      LOGICAL          :: ROT,DEJA
!
      DEJA =.FALSE.
      ROT = .TRUE.
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     LOOP OVER BOUNDARY NODES
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF(NPTFR.GT.0)THEN    ! FOR PARALLEL CASES
      DO K=1,NPTFR
        IS=MESH%NBOR%I(K)
!
!       INITIALIZATION
        FLBOR%R(K) = 0.D0
        FLUENT     = 0.D0
        FLUSORT    = 0.D0
        OUTFLOW    = 0.D0
        FLX(K,1)   = 0.D0
        FLX(K,2)   = 0.D0
        FLX(K,3)   = 0.D0
        FLXG(1)  = 0.D0
        FLXG(2)  = 0.D0
        FLXG(3)  = 0.D0
!
!       INDICATOR FOR DRY CELLS
        IDRY=0
!
!       NORMALIZED NORMAL
        XNN=MESH%XNEBOR%R(K)
        YNN=MESH%YNEBOR%R(K)
!
!       NON NORMALIZED NORMAL
        VNX=MESH%XNEBOR%R(K+NPTFR)
        VNY=MESH%YNEBOR%R(K+NPTFR)
        VNL(K)=SQRT(VNX**2+VNY**2)
!
        H1 = W(1,IS)
        IF(H1.GT.EPS_FV)THEN
          U1 = W(2,IS)/H1
          V1 = W(3,IS)/H1
        ELSE
          U1 = 0.0D0
          V1 = 0.0D0
          IDRY=IDRY+1
        ENDIF
!
!       MEAN DISTANCE, FOR CFL WITH WAF
        DX = MESH%DTHAUT%R(IS)
!
!       **************************************************
!       WALL BOUNDARY
!       SLIPPING CONDITION
!       **************************************************
        IF(LIMPRO(K,1).EQ.KNEU) THEN

!         FIRST METHOD: STRONG IMPOSITION
!         ********************************
!         CE.n = 0  is done in cdlproj
!         DEFINITION OF THE GHOST STATE Ue
          HG = H1
!
!         ROTATION
          U10 = U1
          U1  = XNN*U10+YNN*V1
          V1  =-YNN*U10+XNN*V1
!
!         PUT NORMAL COMPONENT = 0
          U1 =  0.D0
          UG =  U1
          VG =  V1
!
!         INVERSE ROTATION
          U10 = U1
          U1  = -YNN*V1
          V1  =  XNN*V1
          UG  = -YNN*VG
          VG  =  XNN*VG
!
!         SECOND METHOD: WEAK IMPOSITION
!         ********************************
!         DEFINITION OF THE GHOST STATE Ue
!         HG = H1
!         INNER PRODUCT 2V.n
!         U10 = 2.D0*(U1*XNN + V1*YNN)
!         WEAK IMPOSITION: PUT VG = V1-2(V1.n)n
!         UG = U1 - U10*XNN
!         VG = V1 - U10*YNN
!
!       **************************************************
!       LIQUID BOUNDARIES
!       **************************************************
        ELSEIF(LIMPRO(K,1).EQ.KDIR.OR.LIMPRO(K,1).EQ.KDDL)THEN
!
!         ROTATION
          IF(H1.LT.EPS_FV)THEN
            UNN = 0.D0
            VNN = 0.D0
          ELSE
            UNN =  XNN*U1 + YNN*V1
            VNN = -YNN*U1 + XNN*V1
          ENDIF
!
!         ===============================
!         IF H GIVEN
!         ===============================
          IF(LIMPRO(K,1).EQ.KDIR) THEN
!
!           GHOST STATE FOR H DEFINED BY USER
            HG = HBOR%R(K)
!
!           REGIME ASSESSMENT: WE USE REAL H (H1)
            CG = SQRT(GRAV*HG)
            C1 = SQRT(GRAV*H1)
            LAMBDA1 = UNN + C1
            LAMBDA2 = UNN - C1
            REGIME  = LAMBDA1*LAMBDA2
!
!           SUBCRITICAL REGIME OR INFLOW
!           ----------------------------
            IF(REGIME.LT.0.D0.OR.UNN.LE.0.D0) THEN
!
              IF(HG.LT.EPS_FV)THEN
                UG = 0.D0
                VG = 0.D0
                IDRY = IDRY + 1
              ELSE
                IF(REGIME.LT.0.D0) THEN
!
!                 SUBCRITICAL
!                 -----------
                  UG = UNN + 2.D0*(C1-CG)
                  VG = VNN
!                 INVERSE ROTATION
                  UGTEMP = UG
                  UG = XNN*UGTEMP - YNN*VG
                  VG = YNN*UGTEMP + XNN*VG
!
                ELSE
!
!                 SUPERCRITICAL INFLOW
!                 --------------------
                  IF(LIUBOR%I(K).EQ.KENTU.OR.LIUBOR%I(K).EQ.KENT) THEN
!                   IMPOSED INFLOW
                    UG = UBOR%R(K)
                    VG = VBOR%R(K)
                  ELSE
!                   DATA MISSING
!                   WE SUPPOSE "THE LAKE AT REST"
                    UG = 0.D0
                    VG = 0.D0
                    IF(.NOT.DEJA.AND.ENTET)THEN
                      WRITE(LU,61) NUMLIQ%I(K)
                      DEJA=.TRUE.
                    ENDIF
                  ENDIF
!
                ENDIF
!
              ENDIF
!
!           SUPERCRITICAL OUTFLOW
!           ---------------------
            ELSE
              HG = 0.D0
              UG = 0.D0
              VG = 0.D0
              IF(.NOT.DEJA.AND.ENTET)THEN
                WRITE(LU,91) NUMLIQ%I(K),ABS(UNN)/MAX(EPS_FV,C1)
                DEJA=.TRUE.
!               NO CONTRIBUTION
              ENDIF
            ENDIF
!
!         ==================================
!         IF GIVEN VELOCITY OR DISCHARGE
!         ==================================
          ELSEIF(LIMPRO(K,2).EQ.KDIR)THEN     !   (LIUBOR%I(K).EQ.KENTU)THEN
!
!           GHOST STATE FOR U DEFINED BY USER
            UG = UBOR%R(K)
            VG = VBOR%R(K)
            UGN =  XNN*UG + YNN*VG ! TO RETRIEVE NORMAL COMPONENT OF UG
!           VGN = -YNN*UG + XNN*VG ! AND SO ON
!
!           IN CASE OF ROE: IMPOSSIBLE TO IMPOSE NEGATIVE DISCHARGE
            IF ((ICIN.EQ.0).AND.(UGN.GE.0.D0)) THEN
              WRITE(LU,21) NUMLIQ%I(K)
              CALL PLANTE(1)
              STOP
            ENDIF
!
!           REGIME ASSESSMENT: WE USE REAL H (H1)
            C1 = SQRT(GRAV*H1)
            LAMBDA1 = UNN + C1
            LAMBDA2 = UNN - C1
            REGIME  = LAMBDA1*LAMBDA2

!           SUBCRITICAL REGIME OR INFLOW
!           ----------------------------
            IF(REGIME.LT.0.D0.OR.UNN.LE.0.D0) THEN
!
              IF(REGIME.LT.0.D0) THEN
!
!               SUBCRITICAL
!               -----------
!               GHOST STATE DEFINED WITH THE CONSERVATION
!               OF SECOND RIEMANN INVARIANT
                HG = (UNN + 2.D0*SQRT(GRAV*H1)-UGN)**2/(4.D0*GRAV)
!
              ELSE
!
!               SUPERCRITICAL INFLOW
!               --------------------
                IF(LIUBOR%I(K).EQ.KENTU.OR.LIUBOR%I(K).EQ.KENT) THEN
!                 IMPOSED INFLOW
                  HG = HBOR%R(K)
                ELSE
                  ! DATA MISSING
                  HG = H1
                  IF(.NOT.DEJA.AND.ENTET)THEN
                    WRITE(LU,31) NUMLIQ%I(K)
                    DEJA=.TRUE.
                  ENDIF
                ENDIF

                ! DRY CELL
                IF(HG.LT.EPS_FV)THEN
                  IDRY = IDRY + 1
                ENDIF
!
              ENDIF
!
!           SUPERCRITICAL OUTFLOW
!           ---------------------
            ELSE
              HG = 0.D0
              UG = 0.D0
              VG = 0.D0
              IF(.NOT.DEJA.AND.ENTET)THEN
                WRITE(LU,91) NUMLIQ%I(K),ABS(UNN)/MAX(EPS_FV,C1)
                DEJA=.TRUE.
              ENDIF
            ENDIF
!
!         ===============================
!         CRITICAL OUTFLOW
!         ===============================
          ELSE
!
!           CRITICAL OUTFLOW FOR WAF/TCHAMEN/ZOKAGOA
            IF((ICIN.EQ.2).OR.(ICIN.EQ.3).OR.(ICIN.EQ.5)) THEN
              HG = H1
              IF(HG.GT.EPS_FV)THEN
                UG = U1
                VG = V1
              ELSE
                UG = 0.0D0
                VG = 0.0D0
                IDRY = IDRY + 1
              ENDIF
!
!           CRITICAL OUTFLOW
            ELSE
              HG = 0.D0
              UG = 0.D0
              VG = 0.D0
              IDRY = IDRY+1
            ENDIF
!
          ENDIF
        ENDIF
!
!       **************************************************
!       COMPUTE THE FLUX
!       **************************************************
!       AT LEAST ONE WET CELL
        IF(IDRY.LT.2)THEN
          CALL FLUX_CHOICE(H1,HG,H1,HG,U1,UG,V1,VG,ZF%R(IS),ZF%R(IS),
     &                     XNN,YNN,FLXG,FLXG,H1,HG,V1,VG,DX)
        ENDIF
        FLX(K,1) = FLXG(1)
        FLX(K,2) = FLXG(2)
        FLX(K,3) = FLXG(3)
!
      ENDDO
      ENDIF
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     FINAL BALANCE
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     FOR PARALLELISM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM_BORD(FLX(:,1),1,MESH)
        CALL PARCOM_BORD(FLX(:,2),1,MESH)
        CALL PARCOM_BORD(FLX(:,3),1,MESH)
      ENDIF
!
      IF(NPTFR.GT.0)THEN
      DO K=1,NPTFR
        IS=MESH%NBOR%I(K)
        IF(NCSIZE.GT.1)THEN
          OUTFLOW  = FLX(K,1)*VNL(K)*MESH%IFAC%I(IS)
        ELSE
          OUTFLOW  = FLX(K,1)*VNL(K)
        ENDIF
        IF(FLX(K,1).LE.0.D0)THEN ! INLET
          FLUENT = FLUENT + OUTFLOW
        ELSE                     ! OUTLET
          FLUSORT = FLUSORT + OUTFLOW
        ENDIF
        FLBOR%R(K) = OUTFLOW
!
        CE(IS,1)  = CE(IS,1) - VNL(K)*FLX(K,1)
        CE(IS,2)  = CE(IS,2) - VNL(K)*FLX(K,2)
        CE(IS,3)  = CE(IS,3) - VNL(K)*FLX(K,3)
!
!       TRACERS
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            FLUHBTEMP%ADR(ITRAC)%P%R(K) = VNL(K)*FLX(K,1)
          ENDDO
        ENDIF
!
      ENDDO
!
      ENDIF ! PARALLEL CASES
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     WARNINGS
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
21    FORMAT(1X,'CDL_FV: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUBRITICAL OUTLET WITH IMPOSED  ',/,1X,
     & '          DISCHARGE INCOMPATIBLE WITH ROE SCHEME')
!
31    FORMAT(1X,'CDL_FV: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUPERCRITICAL INLET        ',/,1X,
     & '          AND NO WATER DEPTH PROVIDED',/,1X)
!
61    FORMAT(1X,'CDL_FV: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUPERCRITICAL INLET        ',/,1X,
     & '          AND NO DISCHARGE PROVIDED',/,1X)
!
91    FORMAT(1X,'CDL_FV: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUPERCRITICAL OUTLET        ',/,1X,
     & '          DESIRED BOUNDARY CONDITION MAY BE UNSATISFIED',/,1X,
     & '          FROUDE AT BOUNDARY IS: ',G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
