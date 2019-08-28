!                    ***************************
                      SUBROUTINE CALCS3D_MICROPOL
!                    **************************
     & (NPOIN2,TN,TEXP,TIMP,ZPROP,CF,UN,VN,
     &  T2_1,T2_2,T2_3,T3_1,T3_2,DEBUG)
!
!
!***********************************************************************
! TELEMAC2D   V7P2                                        21/05/2016
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR MICROPOL WAQ PROCESS IN 3D
!          WAQ PROCESS OF CODE_TRACER (MASCARET SYSTEM)
!
!history  R. ATA
!+        21/05/2016
!+        V7P0
!+       REAL CREATION
!
!history  S.E. BOURBAN (HRW)
!+        07/06/2017
!+        V7P3
!+        Indexing tracer (IND_*) to avoid conflicting naming convention
!+        between user defined tracers, water quality processes and
!+        ice processes. Introduction of the array RANK_*.
!
!history  S.E. BOURBAN (HRW)
!+        25/09/2017
!+        V7P3
!+        TEXP and TIMP are now additive to account for a variety of
!+        of sources / sinks on a given TRACER
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CCSEDIM        |-->| CONSTANT OF EXPONENTIAL DESINTEGRATION
!| CDISTRIB       |-->| COEFFICIENT OF DISTRIBUTION (KD)
!| DEBUG          |-->| IF NE.0 THEN DEBUG MODE
!| DT             |-->| TIME STEP
!| ERO            |-->| EROSION RATE
!| KDESORP        |-->| KINETIC CONSTANT OF  DESORPTION
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| TAUB           |-->| BED SHEAR
!| TAUS           |-->| CRITICAL STRESS OF RESUSPENSION
!| TAUR           |-->| SEDIMENTATION CRITICAL STRESS
!| TEXP           |<--| EXPLICIT SOURCE TERMS OF TRACERS
!| TIMP           |<--| IMPLICIT SOURCE TERMS OF TRACERS
!| TN             |-->| TRACERS
!| VITCHU         |-->| SEDIMENT SETTLING VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY:ERO,TAUR,TAUS,VITCHU,CDISTRIB,
     &  RO0,KDESORP,CCSEDIM,
     &  IND_SS,IND_SF,IND_C,IND_CSS,IND_CSF
      USE INTERFACE_WAQTEL, EX_CALCS3D_MICROPOL => CALCS3D_MICROPOL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN   ) :: NPOIN2
      INTEGER          , INTENT(IN   ) :: DEBUG
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T2_1,T2_2,T2_3,T3_1,T3_2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CC
!
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 0'
!
!     =======================================
!     PRELIMINARY COMPUTATIONS
!     =======================================
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 1'
!
!     BED SHEAR STRESS (TAUB-STOCKED IN T2_1==>2D TABLE)
!
      CALL TAUB_WAQTEL(CF,RO0,T2_1,NPOIN2,UN,VN)
!
!     DEPOTION PROBABILITY (SED): STOCKED IN T2_2==>2D TABLE
!
      CALL DEPOS_FX(T2_2,T2_1,TN%ADR(IND_SS)%P,TAUS,VITCHU,NPOIN2)
!
!     EROSION FLUX (RS): STOCKED IN T2_3 ==> 2D TABLE
!
      CALL EROSION_FX(T2_3,T2_1,TN%ADR(IND_SF)%P,TAUR,ERO,1.D-10,
     &                NPOIN2)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 4'
!
!
!     =======================================
!     LET'S NOW COMPUTE SOURCE TERMS
!     =======================================
!
!     FIRST TRACER: SUSPENDED LOAD [SS] (IND_SS)
!
!     BED SOURCES
!      DO I=1,NPOIN2
!        TEXP%ADR(IND_SS)%P%R(I)=T2_3%R(I)-T2_2%R(I)
!      ENDDO
!      CALL OVD('X=Y/Z   ',TEXP%ADR(IND_SS)%P%R,TEXP%ADR(IND_SS)%P%R,
!     &         ZPROP%R,0.D0,NPOIN2,2,0.D0,EPS                      )
      CALL OS ('X=Y-Z   ', X=T2_1,Y=T2_3,Z=T2_2                     )
      CALL OVD('X=X+CY/Z',TEXP%ADR(IND_SS)%P%R,T2_1%R,
     &         ZPROP%R,1.D0,NPOIN2,2,0.D0,EPS                       )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 5'
!
!     SECOND TRACER: BED SEDIMENT [SF] (IND_SF)
!      warning: no advection neither diffusion for this tracer
!
      DO I=1,NPOIN2
!        TEXP%ADR(IND_SF)%P%R(I)=T2_2%R(I)-T2_3%R(I)
        TEXP%ADR(IND_SF)%P%R(I) = TEXP%ADR(IND_SF)%P%R(I) +
     &        T2_2%R(I) - T2_3%R(I)
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 6'
!
!     THIRD TRACER: POLLUTANT DENSITY [C] (IND_C)
!
!     implicit part
      CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_C)%P,C=CCSEDIM            )
!     explicit part
!      CALL OS( 'X=CY    ' ,X=TEXP%ADR(IND_C)%P,Y=TN%ADR(IND_CSS)%P,
!     &                     C=KDESORP                                   )
      CALL OS( 'X=CY    ' ,X=T3_2,Y=TN%ADR(IND_CSS)%P     ,C=KDESORP )
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_C)%P,Y=T3_2                )
      CC =-KDESORP*CDISTRIB
!  warning: the following term causes divergence of the code, it should
!           be traited implicitly- it is commented: to be investigated
!           more in depth
!      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(IND_C)%P,Y=TN%ADR(IND_C)%P,
!     &                     Z=TN%ADR(IND_SS)%P             ,C=CC       )
!
!
!     FORTH TRACER: ABSORBED POLLUTANT BY SUSPENDED LOAD [CSS] (IND_CSS)
!
!     implicit part
      CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_CSS)%P,C=CCSEDIM          )
!     explicit part
!      CALL OS( 'X=-Y    ' ,X=TEXP%ADR(IND_CSS)%P,Y=TEXP%ADR(IND_C)%P )
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T3_2 )
      DO I=1,NPOIN2
        T3_1%R(I) = T2_3%R(I)*TN%ADR(IND_CSF)%P%R(I) -
     &              T2_2%R(I)*TN%ADR(IND_CSS)%P%R(I)
      ENDDO
      CALL OVD('X=Y/Z   ' ,T3_1%R,T3_1%R,
     &                     ZPROP%R,0.D0,NPOIN2,2,0.D0,EPS           )
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T3_1             )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 8'
!
!     FIFTH TRACER: ABSORBED POLLUTANT BY BED SEDIMENT [CFF] (IND_CSF)
!
      DO I=1,NPOIN2
        TEXP%ADR(IND_CSF)%P%R(I) = TEXP%ADR(IND_CSF)%P%R(I) +
     &                           T2_2%R(I)*TN%ADR(IND_CSS)%P%R(I) -
     &                           T2_3%R(I)*TN%ADR(IND_CSF)%P%R(I)
      ENDDO
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_CSF)%P,Y=TN%ADR(IND_CSF)%P,
     &                     C=-CCSEDIM                               )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 9'
!
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS
!                   (IMPLICIT PART IS ADDED IN CVDFTR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
