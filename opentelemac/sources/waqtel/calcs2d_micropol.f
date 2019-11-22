!                     ***************************
                      SUBROUTINE CALCS2D_MICROPOL
!                     ***************************
!
     & (NPOIN,TN,TEXP,TIMP,HPROP,CF,UN,VN,
     &  T1,T2,T3,T4)
!
!***********************************************************************
! WAQTEL   V8P1
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR MICROPOL WAQ PROCESS
!          WAQ PROCESS OF CODE_TRACER (MASCARET SYSTEM)
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION (VOID)
!history  R. ATA
!+        28/09/2015
!+        V7P1
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
!history  C.-T. PHAM
!+        20/11/2019
!+        V8P1
!+        The calculation of the 4th ad 5th equations is with RS/SF
!+        and SEDP not RS and SED=SEDP*SS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CCSEDIM        |-->| CONSTANT OF EXPONENTIAL DESINTEGRATION
!| CDISTRIB       |-->| COEFFICIENT OF DISTRIBUTION (KD)
!| ERO            |-->| EROSION RATE
!| KDESORP        |-->| KINETIC CONSTANT OF  DESORPTION
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| NTRAC          |-->| NUMBER OF TRACERS
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
      USE INTERFACE_WAQTEL, EX_CALCS2D_MICROPOL => CALCS2D_MICROPOL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   ) :: NPOIN
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,T1,T2,T3,T4,TIMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CC
!
!-----------------------------------------------------------------------
!
!
!     =======================================
!     PRELIMINARY COMPUTATIONS
!     =======================================
!
!     BED SHEAR STRESS (TAUB-STORED IN T1)
!
      CALL TAUB_WAQTEL(CF,RO0,T1,NPOIN,UN,VN)
!
!     DEPOSITION PROBABILITY (SED): STORED IN T2, NOT SEDP
!
      CALL DEPOS_FX(T2,T1,TN%ADR(IND_SS)%P,TAUS,VITCHU,NPOIN)
!
!     SEDP = SED/SS: STORED IN T4, NOT SED
!
      CALL OS ('X=C     ', X=T3,C=1.D0)
      CALL DEPOS_FX(T4,T1,T3,TAUS,VITCHU,NPOIN)
!
!     EROSION FLUX (RS): STORED IN T3
!
      CALL EROSION_FX(T3,T1,TN%ADR(IND_SF)%P,TAUR,ERO,1.D-10,NPOIN)
!
!
!     =======================================
!     LET'S NOW COMPUTE SOURCE TERMS
!     =======================================
!
!     FIRST TRACER: SUSPENDED LOAD [SS] (IND_SS)
!                                               RS   SED
      CALL OS ('X=Y-Z   ', X=T1,              Y=T3,Z=T2              )
      CALL OVD('X=X+CY/Z', TEXP%ADR(IND_SS)%P%R,T1%R,HPROP%R,
     &                     1.D0, NPOIN,2,0.D0,EPS  )
!
!     SECOND TRACER: BED SEDIMENT [SF] (IND_SF)
!      warning: no advection neither diffusion for this tracer
!                                SED   RS
      CALL OS ('X=Y-Z   ', X=T1,Y=T2,Z=T3              )
      CALL OS ('X=X+Y   ', X=TEXP%ADR(IND_SF)%P, Y=T1  )
!
!     THIRD TRACER: POLLUTANT DENSITY [C] (IND_C)
!
!     implicit part
!     CCSEDIM = CONSTANT L IN THE DOC      
!      CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_C)%P,C=CCSEDIM             )
      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_C)%P,Y=HPROP,C=-CCSEDIM)
!     explicit part
      CALL OS( 'X=CY    ' ,X=T1,Y=TN%ADR(IND_CSS)%P,C=KDESORP        )
      CC =-KDESORP*CDISTRIB
      CALL OS( 'X=X+CYZ ' ,X=T1,Y=TN%ADR(IND_C)%P,
     &                     Z=TN%ADR(IND_SS)%P             ,C=CC       )
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_C)%P,Y=T1                  )
!
!     FOURTH TRACER: ADSORBED POLLUTANT BY SUSPENDED LOAD [CSS] (IND_CSS)
!
!     implicit part
!      CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_CSS)%P,C=CCSEDIM             )
      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_CSS)%P,Y=HPROP,C=-CCSEDIM)
!     explicit part
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T1                  )
!      CALL OS( 'X=YZ    ' ,X=T4,Y=T3,Z=TN%ADR(IND_CSF)%P               )
!      CALL OS( 'X=X+CYZ ' ,X=T4,Y=TN%ADR(IND_CSS)%P,Z=T2,C=-1.D0       )
!      CALL OVD('X=Y/Z   ' ,T4%R,T4%R,
!     &                     HPROP%R,0.D0,NPOIN,2,0.D0,EPS)
!      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T4                  )
!                                 RS
      CALL OS( 'X=Y     ' ,X=T1,Y=T3)
      CALL OS( 'X=CXY/Z ' ,X=T1,Y=TN%ADR(IND_CSF)%P,Z=TN%ADR(IND_SF)%P,
     &                     C=1.D0)
!                                 SEDP
      CALL OS( 'X=X-YZ  ' ,X=T1,Y=T4,Z=TN%ADR(IND_CSS)%P)
      CALL OVD('X=X+CY/Z', TEXP%ADR(IND_CSS)%P%R,T1%R,HPROP%R,
     &                     1.D0, NPOIN,2,0.D0,EPS  )

!
!     FIFTH TRACER: ADSORBED POLLUTANT BY BED SEDIMENT [CFF] (IND_CSF)
!
!      CALL OS( 'X=X+YZ  ' ,X=TEXP%ADR(IND_CSF)%P,Y=TN%ADR(IND_CSS)%P,
!     &                     Z=T2                                        )
!      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(IND_CSF)%P,Y=TN%ADR(IND_CSF)%P,
!     &                     Z=T3,C=-1.D0                                )
!      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_CSF)%P,Y=TN%ADR(IND_CSF)%P,
!     &                     C=-CCSEDIM                                  )
!     implicit part
      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_CSF)%P,Y=HPROP,C=-CCSEDIM)
!     explicit part
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSF)%P,Y=T1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
