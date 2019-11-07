!                     ************************
                      SUBROUTINE PRERES_KHIONE
!                     ************************
!
     &(NPOIN,AT,LT,TELSOR)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+         THE RESULTS FILE OR TO THE LISTING.
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        11/11/2016
!+        V7P3
!+        Coupling TELEMAC-2D with KHIONE (ice modelling component)
!+        Initial developments
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| CURRENT TIME IN SECONDS
!| LT             |-->| CURRENT NUMBER OF OF TIME STEP
!| NPOIN          |-->| NUMBER OF NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL, ONLY : RO0
      USE DECLARATIONS_KHIONE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION,      INTENT(IN) :: AT
      INTEGER,               INTENT(IN) :: LT,NPOIN
      TYPE(BIEF_OBJ),        INTENT(IN) :: TELSOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
      IMP = .FALSE.
      LEO = .FALSE.
      IF( LT.EQ.(LT/LISPRD)*LISPRD ) IMP = .TRUE.
      IF( LT.EQ.(LT/LEOPRD)*LEOPRD ) LEO = .TRUE.
!
!     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF( .NOT.(LEO.OR.IMP) ) RETURN
!
!-----------------------------------------------------------------------
!
!     INITIALISATION PART
!
      IF( LT.EQ.0 ) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR EVERY TIME STEP
!
!=======================================================================
!     COMPUTES THE EQUIVALENT SURFACE ELEVATION
!=======================================================================
!
      IF( (LEO.AND.SORLEO(17)).OR.(IMP.AND.SORIMP(17)) ) THEN
!       TELSOR%ADR(4)%P: H
!       TELSOR%ADR(6)%P: ZF
        DO I = 1,NPOIN
          T3%R(I) = TELSOR%ADR(4)%P%R(I) + TELSOR%ADR(6)%P%R(I) +
     &      RHO_ICE/RO0 * ( THIFEMS%R(I) + THIFEMF%R(I) + HUN%R(I) )
        ENDDO
      ENDIF
!
!=======================================================================
!     COMPUTES THE TOP OF THE ICE COVER
!=======================================================================
!
      IF( (LEO.AND.SORLEO(18)).OR.(IMP.AND.SORIMP(18)) ) THEN
!       TELSOR%ADR(4)%P: H
!       TELSOR%ADR(6)%P: ZF
        DO I = 1,NPOIN
          T4%R(I) = TELSOR%ADR(4)%P%R(I) + TELSOR%ADR(6)%P%R(I) +
     &      ( THIFEMS%R(I) + THIFEMF%R(I) + HUN%R(I) )
        ENDDO
      ENDIF
!
!=======================================================================
!     COMPUTES THE BOTTOM OF THE ICE COVER, ALSO THE SURFACE ELEVATION
!=======================================================================
!
      IF( (LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19)) ) THEN
!       TELSOR%ADR(4)%P: H
!       TELSOR%ADR(6)%P: ZF
        CALL OS( 'X=Y+Z   ', X=T5,Y=TELSOR%ADR(4)%P,Z=TELSOR%ADR(6)%P )
      ENDIF
!
!=======================================================================
!     CONVERTER ICE CHARACTERISTICS (PRIME NUMBER) INTO ITS REAL PART
!=======================================================================
!
      IF( (LEO.AND.SORLEO(21)).OR.(IMP.AND.SORIMP(21)) ) THEN
        DO I = 1,NPOIN
          ICETYPE%R(I) = 1.D0 * ICETYPE%I(I)
        ENDDO
      ENDIF
!
!=======================================================================
!
!-----------------------------------------------------------------------
!
      RETURN
      END
