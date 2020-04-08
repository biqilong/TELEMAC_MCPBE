!                ******************************
                 SUBROUTINE INIT_TRANSPORT_GAIA
!                ******************************
!
     &(HIDING,NSICLA,NPOIN,T1,T2,T3,T4,T5,T6,T7,T8,T9,
     & T10,T11,T12,T14,CHARR,QS_C,QSXC,QSYC,CALFA_CL,SALFA_CL,
     & COEFPN,SLOPEFF,SUSP,QS,QSCL,QSCL_C,QSCL_S,UNORM,U2D,V2D,HN,CF,MU,
     & TOB,TOBW,UW,TW,THETAW,FW,HOULE,ACLADM,UNLADM,KSP,KSR,
     & ICF,HIDFAC,XMVS0,XMVE,GRAV,VCE,HMIN,KARMAN,ZERO,PI,AC,CSTAEQ,
     & SECCURRENT,BIJK,IELMT,MESH,DCLA,XWC,SEDCO,U3D,V3D,CODE,H_TEL,
     & HW,THETAC,TOBCW_MEAN,TOBCW_MAX)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Initialise transport for all sediment classes
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] AC            Critical shields parameter
!>@param[in]     ACLADM        Mean diameter of active layer
!>@param[in]     BIJK          Coefficient of the bijker formula
!>@param[in,out] CALFA_CL      Cosinus of the angle between mean flow and transport
!>@param[in]     CF            Quadratic friction coefficient
!>@param[in]     CHARR         Bedload
!>@param[in]     CODE          Name of calling programme (telemac2d or 3d)
!>@param[in,out] COEFPN        Correction of transport for sloping bed effect
!>@param[in,out] CSTAEQ        Sediment equilibrium concentration
!>@param[in]     DCLA          Diameter dm for each class
!>@param[in]     FW            Quadratic friction coefficient (wave)
!>@param[in]     GRAV          Acceleration of gravity
!>@param[in]     HIDFAC        Hiding factor formulas
!>@param[in]     HIDING        Hiding factor correction
!>@param[in]     HMIN          Minimum value of water depth
!>@param[in]     HN            Water depth
!>@param[in]     HOULE         Logical, for wave effects
!>@param[in]     HW            Wave height
!>@param[in]     H_TEL         Water depth from tel h (n+1)
!>@param[in]     ICF           Bed-load or total load transport formulas
!>@param[in]     IELMT         Number of elements
!>@param[in]     KARMAN        Von karman constant
!>@param[in]     KSP           Bed skin roughness
!>@param[in]     KSR           Ripple bed roughness
!>@param[in,out] MESH          Mesh structure
!>@param[in,out] MU            Correction factor for bed roughness
!>@param[in]     NPOIN         Number of points
!>@param[in]     NSICLA        Number of sediment classes
!>@param[in]     PARTHENIADES  Constant of the krone and partheniades erosion law (kg/m2/s)
!>@param[in]     PI            Pi
!>@param[in,out] QS            Bedload transport rate
!>@param[in,out] QSCL          Suspended load transport rate
!>@param[in,out] QSCL_C        Bedload transport rate
!>@param[in,out] QSCL_S        Suspended load transport rate
!>@param[in,out] QSXC          Bedload transport rate along x
!>@param[in,out] QSYC          Bedload transport rate along y
!>@param[in]     QS_C          Bedload transport rate
!>@param[in]     RATIO_SAND    Mass fraction of sand
!>@param[in,out] SALFA_CL      Sinus of the angle between transport rate and current
!>@param[in]     SECCURRENT    Logical, parametrisation for secondary currents
!>@param[in]     SEDCO         Logical, sediment cohesive or not
!>@param[in]     SLOPEFF       Formula for slope effect
!>@param[in]     SUSP          Logical, suspension
!>@param[in,out] T1            Work bief_obj structure
!>@param[in,out] T10           Work bief_obj structure
!>@param[in,out] T11           Work bief_obj structure
!>@param[in,out] T12           Work bief_obj structure
!>@param[in,out] T14           Work bief_obj structure
!>@param[in,out] T2            Work bief_obj structure
!>@param[in,out] T3            Work bief_obj structure
!>@param[in,out] T4            Work bief_obj structure
!>@param[in,out] T5            Work bief_obj structure
!>@param[in,out] T6            Work bief_obj structure
!>@param[in,out] T7            Work bief_obj structure
!>@param[in,out] T8            Work bief_obj structure
!>@param[in,out] T9            Work bief_obj structure
!>@param[in]     THETAC        Curent direction
!>@param[in]     THETAW        Wave direction (deg wrt ox axis)
!>@param[in,out] TOB           Bed shear stress (total friction)
!>@param[in,out] TOBCW_MEAN    Current + wave bed shear stress (total friction)
!>@param[in,out] TOBC_MAX      Max bed shear stress (total friction)
!>@param[in]     TOBW          Wave induced shear stress
!>@param[in]     TW            Wave period
!>@param[in,out] U2D           Mean flow velocity x-direction
!>@param[in]     UNLADM        Mean diameter of active stratum layer
!>@param[in]     U3D           3D velocity sent by telemac 3d (u)
!>@param[in,out] UNORM         Norm of the mean flow velocity
!>@param[in]     UW            Orbital wave velocity
!>@param[in,out] V2D           Mean flow velocity y-direction
!>@param[in]     V3D           3D velocity sent by telemac 3d (v)
!>@param[in]     VCE           Water viscosity
!>@param[in]     TOCD          Critical shear stress
!>@param[in]     XMVE          Fluid density
!>@param[in]     XMVS0         Water density
!>@param[in]     XWC           Settling velocity
!>@param[in]     ZERO          Zero
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_GAIA, EX_INIT_TRANSPORT => INIT_TRANSPORT_GAIA
!
      USE DECLARATIONS_GAIA, ONLY : NOMBLAY,MPM_ARAY,MPM,NUM_ICLA_ISAND,
     &                              RATIO_SAND,SANFRA,CSRATIO,NSAND
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NSICLA,NPOIN
      INTEGER, INTENT(IN)              :: ICF,HIDFAC,IELMT,SLOPEFF
      LOGICAL, INTENT(IN)              :: CHARR,SUSP,HOULE
      LOGICAL, INTENT(IN)              :: SECCURRENT,SEDCO(*)
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U2D,V2D,UNORM,HN,CF
      TYPE(BIEF_OBJ),    INTENT(IN)    :: MU,TOB,TOBW,UW,TW,THETAW,FW
      TYPE(BIEF_OBJ),    INTENT(IN)    :: ACLADM,UNLADM,KSP,KSR
      TYPE(BIEF_OBJ),    INTENT(IN)    :: H_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: HIDING,CSTAEQ
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QS_C, QSXC, QSYC
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: CALFA_CL,SALFA_CL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T9,T10,T11,T12,T14
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QS,QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: COEFPN
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QSCL
      TYPE(BIEF_MESH),   INTENT(INOUT) :: MESH
      DOUBLE PRECISION,  INTENT(IN)    :: XMVS0(NSICLA),XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(IN)    :: HMIN,KARMAN,ZERO,PI
      DOUBLE PRECISION,  INTENT(IN)    :: BIJK,XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT) :: AC(NSICLA),DCLA(NSICLA)
!
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HW, THETAC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TOBCW_MEAN, TOBCW_MAX
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K
      DOUBLE PRECISION U3DNORM
      DOUBLE PRECISION, ALLOCATABLE :: TMP_RATIO(:)
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
! --- START : INITIALISES RATE OF TRANSPORT AND SUSPENSION
!
!     FOR INITIALISATION : SLOPE EFFECT AND DEVIATION ARE CANCELLED
!
!     RK in case of coupling with T3D, the direction should
!     come from the bottom velocity
!
!     Calculation of sand fraction content at each node (Wilcock and Crowe, 2003)
      IF (ICF == 10) THEN
        DO K = 1, NPOIN
          SANFRA(K) = 0.0D0
          DO I = 1, NSAND
            IF (DCLA(I).LT.2D-3) THEN
              SANFRA(K) = SANFRA(K) + RATIO_SAND(I,1,K)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      IF(CODE(1:9).EQ.'TELEMAC3D') THEN
        DO I=1,NPOIN
          U3DNORM=SQRT(U3D%R(I)*U3D%R(I)+V3D%R(I)*V3D%R(I))
          IF(U3DNORM.GE.1.D-12) THEN
            CALFA_CL%ADR(1)%P%R(I)=U3D%R(I)/U3DNORM
            SALFA_CL%ADR(1)%P%R(I)=V3D%R(I)/U3DNORM
          ELSE
            CALFA_CL%ADR(1)%P%R(I)=1.D0
            SALFA_CL%ADR(1)%P%R(I)=0.D0
          ENDIF
        ENDDO
      ELSE
        CALL OS('X=Y/Z   ',X=CALFA_CL%ADR(1)%P, Y=U2D, Z=UNORM,
     &           C=0.D0, IOPT=2, INFINI=1.D0, ZERO=1.D-12)
        CALL OS('X=Y/Z   ',X=SALFA_CL%ADR(1)%P, Y=V2D, Z=UNORM,
     &           C=0.D0, IOPT=2, INFINI=0.D0, ZERO=1.D-12)
      ENDIF
      IF(NSICLA.GT.1) THEN
        DO I=2,NSICLA
          CALL OS('X=Y     ', X=CALFA_CL%ADR(I)%P,
     &            Y=CALFA_CL%ADR(1)%P)
          CALL OS('X=Y     ', X=SALFA_CL%ADR(I)%P,
     &            Y=SALFA_CL%ADR(1)%P)
        ENDDO
      ENDIF
!
!     appel a effpnt ?
!
      CALL OS('X=C     ',X=COEFPN,C=1.D0)
!
      IF(CHARR) THEN
!
!       MPM for each Layer
!
        CALL OS('X=C     ', X=MPM_ARAY, C=MPM)
!
        CALL OS('X=C     ',X=HIDING,C=1.D0)
!
        ALLOCATE(TMP_RATIO(NPOIN))
        DO I = 1, NSICLA
!
          IF(SEDCO(I)) THEN
!           IF COHESIVE: NO BEDLOAD TRANSPORT
            CALL OS('X=0     ', X=QSCL_C%ADR(I)%P)
          ELSE
!           IF NON COHESIVE
            TMP_RATIO = RATIO_SAND(NUM_ICLA_ISAND(I),1,1:NPOIN)
            CALL BEDLOAD_FORMULA_GAIA
     &        (U2D,V2D,UNORM,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,
     &        ACLADM, UNLADM,KSP,KSR,
     &        TMP_RATIO,
     &        NPOIN,ICF,HIDFAC,XMVS0(I),XMVE,
     &        DCLA(I),GRAV,VCE,HMIN,XWC(I),KARMAN,ZERO,
     &        PI,SUSP,AC(I),HIDING,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     &        T11,T12,QSCL_C%ADR(I)%P,QSCL_S%ADR(I)%P,
     &        IELMT,SECCURRENT,SLOPEFF,
     &        COEFPN,CALFA_CL%ADR(I)%P,SALFA_CL%ADR(I)%P,
     &        BIJK,HOULE,H_TEL,
     &        HW,THETAC,TOBCW_MEAN,TOBCW_MAX,CSTAEQ%ADR(I)%P,
     &        SANFRA)
!
          ENDIF
!         SUM ON ALL CLASSES
          DO J=1,NPOIN
            QS_C%R(J) = QS_C%R(J) + QSCL_C%ADR(I)%P%R(J)
!
!       COMPUTES THE X AND Y COMPONENTS OF TRANSPORT
            QSXC%R(J) = QSXC%R(J) + QSCL_C%ADR(I)%P%R(J)
     &                  *CALFA_CL%ADR(I)%P%R(J)
            QSYC%R(J) = QSYC%R(J) + QSCL_C%ADR(I)%P%R(J)
     &                  *SALFA_CL%ADR(I)%P%R(J)
          ENDDO
!
        ENDDO
        DEALLOCATE(TMP_RATIO)
!
!
      ENDIF
!
!
!     COMPUTES THE TRANSPORT FOR EACH CLASS (IF NOT RESTART OR IF
!                                              DATA NOT FOUND)
      DO I=1, NSICLA
        WRITE(LU,*) 'QSCL REINITIALISED IN INIT_TRANSPORT_GAIA'
        WRITE(LU,*) 'FOR CLASS ',I
        CALL OS('X=Y     ',X=QSCL%ADR(I)%P,Y=QSCL_C%ADR(I)%P)
      ENDDO
!
!     COMPUTES TOTAL TRANSPORT QS
!
      WRITE(LU,*) 'QS REINITIALISED IN INIT_TRANSPORT_GAIA'
      CALL OS('X=Y     ',X=QS,Y=QS_C)
!
!-----------------------------------------------------------------------
!
!     INITIALISE CSRATIO
      CSRATIO%R = 1D0
!
      RETURN
      END
