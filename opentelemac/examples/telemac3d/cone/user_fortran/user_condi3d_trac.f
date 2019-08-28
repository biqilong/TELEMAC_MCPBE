!                    ****************************
                     SUBROUTINE USER_CONDI3D_TRAC
!                    ****************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES TRACER(S)
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Creation from not splitted CONDIM
!+   Called by CONDIM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_USER_CONDI3D_TRAC => USER_CONDI3D_TRAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,J
      DOUBLE PRECISION EIKON
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          DO J=1,NPOIN3
            EIKON = ((X(J)-15.D0)**2 + (Y(J)-10.2D0)**2) / 2.D0
            TA%ADR(I)%P%R(J) = EXP(-EIKON)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
