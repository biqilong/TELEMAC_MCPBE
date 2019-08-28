!                       ************************
                        SUBROUTINE CALCUL_Q_WEIR
!                       ************************
!
!***********************************************************************
! TELEMAC2D   V7P2                                   22/03/2013
!***********************************************************************
!
!brief    COMPUTE THE DISCHARGE ON THE WEIRS WHEN THE TYPE IS EQUAL 2
!+
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        22/03/2013
!+        V6P3
!+   Creation
!
!
!history  C.COULET(ARTELIA)
!+        01/09/2016
!+        V7P2
!+   New management of parallelism
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DMAX,P_DMIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          N, ITRAC
      INTEGER          I_1A_1, I_1B_1, I_1A_2, I_1B_2
      INTEGER          I_2A_1, I_2B_1, I_2A_2, I_2B_2
!
      DOUBLE PRECISION PHI, RELAX
      DOUBLE PRECISION YS1, YS2
      DOUBLE PRECISION ZF_1A_1, ZF_1B_1, ZF_1A_2, ZF_1B_2
      DOUBLE PRECISION ZF_2A_1, ZF_2B_1, ZF_2A_2, ZF_2B_2
      DOUBLE PRECISION H_1A_1, H_1B_1, H_1A_2, H_1B_2
      DOUBLE PRECISION H_2A_1, H_2B_1, H_2A_2, H_2B_2
      DOUBLE PRECISION SLA, SLB
      DOUBLE PRECISION ZFA, ZFB
      DOUBLE PRECISION H_A, H_B, HMINI
!
      INTRINSIC MAX,SQRT,ABS
!
!-----------------------------------------------------------------------
!
!
      HMINI = 1.D-2
      PHI = 0.4
      RELAX = 0.5D0 ! TODO: DEFINE AS USER SETTING
!
!      CALL OS('X=0     ',X=UWEIRA)
!      CALL OS('X=0     ',X=UWEIRB)
!      CALL OS('X=0     ',X=VWEIRA)
!      CALL OS('X=0     ',X=VWEIRB)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING COLLECT_VALUES'
      CALL COLLECT_VALUES
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM COLLECT_VALUES'
!
      DO N = 1, NWEIRS   !LOOP ON ELEMENTARY PEACE OF WEIRS
        I_1A_1  = WEIRS(N)%N_1A_1
        I_1A_2  = WEIRS(N)%N_1A_2
        I_2A_1  = WEIRS(N)%N_2A_1
        I_2A_2  = WEIRS(N)%N_2A_2
        I_1B_1  = WEIRS(N)%N_1B_1
        I_1B_2  = WEIRS(N)%N_1B_2
        I_2B_1  = WEIRS(N)%N_2B_1
        I_2B_2  = WEIRS(N)%N_2B_2
!
        ZF_1A_1 = WNODES(I_1A_1)%ZFN
        ZF_1A_2 = WNODES(I_1A_2)%ZFN
        ZF_2A_1 = WNODES(I_2A_1)%ZFN
        ZF_2A_2 = WNODES(I_2A_2)%ZFN
        ZF_1B_1 = WNODES(I_1B_1)%ZFN
        ZF_1B_2 = WNODES(I_1B_2)%ZFN
        ZF_2B_1 = WNODES(I_2B_1)%ZFN
        ZF_2B_2 = WNODES(I_2B_2)%ZFN
!
        H_1A_1  = WNODES(I_1A_1)%HN
        H_1A_2  = WNODES(I_1A_2)%HN
        H_2A_1  = WNODES(I_2A_1)%HN
        H_2A_2  = WNODES(I_2A_2)%HN
        H_1B_1  = WNODES(I_1B_1)%HN
        H_1B_2  = WNODES(I_1B_2)%HN
        H_2B_1  = WNODES(I_2B_1)%HN
        H_2B_2  = WNODES(I_2B_2)%HN
!
        ZFA = 0.25D0 * (ZF_1A_1 + ZF_1A_2 + ZF_2A_1 + ZF_2A_2 )
        ZFB = 0.25D0 * (ZF_1B_1 + ZF_1B_2 + ZF_2B_1 + ZF_2B_2 )
        H_A = 0.25D0 * (H_1A_1 + H_1A_2 + H_2A_1 + H_2A_2 )
        H_B = 0.25D0 * (H_1B_1 + H_1B_2 + H_2B_1 + H_2B_2 )
        SLA = ZFA + H_A
        SLB = ZFB + H_B
!
!       COMPUTATION OF THE DISCHARGE
!
!       ADDING A SECURITY ON THE LEVEL OF THE WEIR
        YS1 = MAX(WEIRS(N)%Z1,ZFA+0.01D0,ZFB+0.01D0)
        YS2 = MAX(WEIRS(N)%Z2,ZFA+0.01D0,ZFB+0.01D0)
!
        WEIRS(N)%Q = 0.D0
!       UPSTREAM IS ON SIDE A
        IF (SLA.GT.SLB) THEN
          IF (H_A.GT.HMINI) THEN
            CALL LOI_W_INC(SLA,SLB,YS1,YS2,WEIRS(N)%WIDTH,
     &                     PHI,WEIRS(N)%Q,GRAV)
          ENDIF
!       UPSTREAM IS ON SIDE B
        ELSE
          IF (H_B.GT.HMINI) THEN
            CALL LOI_W_INC(SLB,SLA,YS1,YS2,WEIRS(N)%WIDTH,
     &                     PHI,WEIRS(N)%Q,GRAV)
            WEIRS(N)%Q = -WEIRS(N)%Q
          ENDIF
        ENDIF
!
        WEIRS(N)%Q  = WEIRS(N)%Q * (1D0-RELAX) + WEIRS(N)%Q0 * RELAX
        WEIRS(N)%Q0 = WEIRS(N)%Q
      ENDDO
!
! TODO: STREALINE DISCHARGES COMPUTED ON IDENTICAL ELEMENTS
!
!       NOW WE DISTRIBUTE THE COMPUTED DISCHARGE OF EACH ELEMENTS OF WEIRS ON NODES
!       QELEM > 0 means the flow is from side A to side B
!
      DO N = 1, NWEIRS_NODES
        WNODES(N)%QN = 0.D0
        DO ITRAC=1, NTRAC
          WNODES(N)%TRAC(ITRAC) = 0.D0
        ENDDO
      ENDDO
!
      DO N = 1, NWEIRS
        I_1A_1 = WEIRS(N)%N_1A_1
        I_1A_2 = WEIRS(N)%N_1A_2
        I_2A_1 = WEIRS(N)%N_2A_1
        I_2A_2 = WEIRS(N)%N_2A_2
        I_1B_1 = WEIRS(N)%N_1B_1
        I_1B_2 = WEIRS(N)%N_1B_2
        I_2B_1 = WEIRS(N)%N_2B_1
        I_2B_2 = WEIRS(N)%N_2B_2
        WNODES(I_1A_1)%QN = WNODES(I_1A_1)%QN - 0.25D0 * WEIRS(N)%Q
        WNODES(I_1A_2)%QN = WNODES(I_1A_2)%QN - 0.25D0 * WEIRS(N)%Q
        WNODES(I_2A_1)%QN = WNODES(I_2A_1)%QN - 0.25D0 * WEIRS(N)%Q
        WNODES(I_2A_2)%QN = WNODES(I_2A_2)%QN - 0.25D0 * WEIRS(N)%Q
        WNODES(I_1B_1)%QN = WNODES(I_1B_1)%QN + 0.25D0 * WEIRS(N)%Q
        WNODES(I_1B_2)%QN = WNODES(I_1B_2)%QN + 0.25D0 * WEIRS(N)%Q
        WNODES(I_2B_1)%QN = WNODES(I_2B_1)%QN + 0.25D0 * WEIRS(N)%Q
        WNODES(I_2B_2)%QN = WNODES(I_2B_2)%QN + 0.25D0 * WEIRS(N)%Q
      ENDDO
!
!     MANAGEMENT OF THE TRACER
!
      DO N = 1, NWEIRS_NODES
        DO ITRAC=1, NTRAC
          WNODES(N)%TRAC(ITRAC) = 0.D0
        ENDDO
      ENDDO
!
      IF(NTRAC.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING COLLECT_VALUES_TRAC'
        CALL COLLECT_VALUES_TRAC
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM COLLECT_VALUES_TRAC'
      ENDIF
!
      RETURN
      END
