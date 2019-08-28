!                    **************************
                     SUBROUTINE USER_INIT_COMPO
!                    **************************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!brief    USER INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN

        NCOUCHES(J) = 3

        ES(J,1) = 0.1D0
        ES(J,2) = 0.1D0
        ES(J,3) = 9.8D0

        IF(MESH%X%R(J) > 600.0D0) THEN
          AVA0(1) = 1.0D0
          AVA0(2) = 0.0D0
          AVA0(3) = 0.0D0
        ELSE
          AVA0(1) = 0.0D0
          AVA0(2) = 0.0D0
          AVA0(3) = 1.0D0
        ENDIF

        DO I = 1, NSICLA
          DO K = 1, NCOUCHES(J)
            AVAIL(J,K,I) = AVA0(I)
          ENDDO
        ENDDO

      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
