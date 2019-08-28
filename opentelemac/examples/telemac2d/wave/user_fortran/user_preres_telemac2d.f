!                    ********************************
                     SUBROUTINE USER_PRERES_TELEMAC2D
!                    ********************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    PREPARES THE USER VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N
      DOUBLE PRECISION PI,PER,HHPLG,A,PHI,B,WPLG
!
!=======================================================================
! Computing analytic solution
!=======================================================================
!
      PI = 4.D0*ATAN(1.D0)
      PER = 0.5D0
      WPLG = 2.D0*PI/PER
      A = 0.05D0
!
      IF((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))) THEN
        DO N = 1,NPOIN
          PHI = X(N)/SQRT(GRAV*10.D0)
          B = (2.D0*PI*(AT-PHI))/PER
          IF (AT.LE.PHI) THEN
            PRIVE%ADR(1)%P%R(N) = 10.D0
          ELSE
            PRIVE%ADR(1)%P%R(N) = 10.D0 + A*SIN(B)
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END