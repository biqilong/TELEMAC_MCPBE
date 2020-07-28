!                     ************************
                      SUBROUTINE PRERES_KHIONE
!                     ************************
!
     &(NPOIN,LT,TELSOR,TN)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+         THE RESULTS FILE OR TO THE LISTING.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LT             |-->| CURRENT NUMBER OF OF TIME STEP
!| NPOIN          |-->| NUMBER OF NODES
!| TELSOR         |-->| OUTPUT OF TELEMAC2D
!| TN             |-->| TELEMAC2D TRACER VALUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,               INTENT(IN) :: LT,NPOIN
      TYPE(BIEF_OBJ),        INTENT(IN) :: TELSOR,TN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
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
      IF( (LEO.AND.SORLEO(16)).OR.(IMP.AND.SORIMP(16)) ) THEN
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
      IF( (LEO.AND.SORLEO(17)).OR.(IMP.AND.SORIMP(17)) ) THEN
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
      IF( (LEO.AND.SORLEO(18)).OR.(IMP.AND.SORIMP(18)) ) THEN
!       TELSOR%ADR(4)%P: H
!       TELSOR%ADR(6)%P: ZF
        CALL OS( 'X=Y+Z   ', X=T5,Y=TELSOR%ADR(4)%P,Z=TELSOR%ADR(6)%P )
      ENDIF
!
!=======================================================================
!     CONVERTER ICE CHARACTERISTICS (PRIME NUMBER) INTO ITS REAL PART
!=======================================================================
!
      IF( (LEO.AND.SORLEO(20)).OR.(IMP.AND.SORIMP(20)) ) THEN
        DO I = 1,NPOIN
          ICETYPE%R(I) = 1.D0 * ICETYPE%I(I)
        ENDDO
      ENDIF
!
!=======================================================================
!     COMPUTES TOTAL CONCENTRATION
!=======================================================================
!
      IF( (LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22)) ) THEN
        CALL OS('X=Y     ', X=CTOT, Y=TN%ADR(IND_FRA)%P)
        IF(NC_FRA.GT.1) THEN
          DO I = 2,NC_FRA
            CALL OS('X=X+Y   ', X=CTOT, Y=TN%ADR(IND_FRA+I-1)%P)
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!     COMPUTES TOTAL NUMBER OF PARTICLES
!=======================================================================
!
      IF( (LEO.AND.SORLEO(21)).OR.(IMP.AND.SORIMP(21)) ) THEN
        CALL OS('X=CY    ', X=NTOT, Y=TN%ADR(IND_FRA)%P,
     &                      C=1.D0/VK_FRZL(1))
        IF(NC_FRA.GT.1) THEN
          DO I = 2,NC_FRA
            CALL OS('X=X+CY  ', X=NTOT, Y=TN%ADR(IND_FRA+I-1)%P,
     &                          C=1.D0/VK_FRZL(I))
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!     VARIABLES OF THERMAL BUDGET FROM T2D TRACER
!=======================================================================
!
      DO I=1,NC_FRA
        IF( (LEO.AND.SORLEO(22+I)).OR.(IMP.AND.SORIMP(22+I)) ) THEN
          CALL OS('X=Y     ', X=FRZL%ADR(I)%P, Y=TN%ADR(IND_FRA+I-1)%P)
        ENDIF
      ENDDO
!
      DO I=1,NC_FRA
        IF( (LEO.AND.SORLEO(22+I+NC_FRA))
     &       .OR.(IMP.AND.SORIMP(22+I+NC_FRA)) ) THEN
          CALL OS('X=CY    ', X=NBP%ADR(I)%P, Y=TN%ADR(IND_FRA+I-1)%P,
     &                        C=VK_FRZL(I))
        ENDIF
      ENDDO
!
      IF( (LEO.AND.SORLEO(22+2*NC_FRA+1))
     &     .OR.(IMP.AND.SORIMP(22+2*NC_FRA+1)) ) THEN
        CALL OS('X=Y     ', X=TEMP, Y=TN%ADR(IND_T)%P)
      ENDIF
!
      IF( (LEO.AND.SORLEO(22+2*NC_FRA+2))
     &     .OR.(IMP.AND.SORIMP(22+2*NC_FRA+2)) ) THEN
        CALL OS('X=Y     ', X=SAL, Y=TN%ADR(IND_S)%P)
      ENDIF
!
!=======================================================================
!
!-----------------------------------------------------------------------
!
      RETURN
      END
