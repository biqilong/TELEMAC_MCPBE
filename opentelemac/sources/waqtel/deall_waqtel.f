!                     ***********************
                      SUBROUTINE DEALL_WAQTEL
!                     ***********************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    Memory allocation of structures, aliases, blocks...
!
!history  S.E.BOURBAN (HRW)
!+        25/09/2017
!+        V7P3
!+        First implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!

!
!-----------------------------------------------------------------------
!
!     DEGRADATION LAW
!
      CALL BIEF_DEALLOBJ(K2)
      CALL BIEF_DEALLOBJ(RAYEFF)
      CALL BIEF_DEALLOBJ(TAIR)
      CALL BIEF_DEALLOBJ(RAYAED2)

      IF(ALLOCATED(LOITRAC)) THEN
        DEALLOCATE(LOITRAC)
        DEALLOCATE(COEF1TRAC)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DEALL_WAQTEL
