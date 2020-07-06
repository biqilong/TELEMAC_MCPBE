!                   ***************************
                    SUBROUTINE FRICTION_HUTHOFF
!                   ***************************
!
     &(HA,CD,MD,HVEG,SP,CP)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR SUBMERGED AND NON-SUBMERGED
!+        VEGETATION FROM PARAMETERS WITH HUTHOFF ET AL. (2007) APPROACH
!
!history  FREDERIK FOLKE (BAW)
!+        07/11/2019
!+        V8P2
!+
!+   The algorithm was developed by Huthoff
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CD             |-->| BULK DRAG COEFFICIENT FOR VEGETATION
!| CP             |<--| VEGETATION FRICTION COEFFICIENT
!| HA             |-->| WATER DEPTH
!| HVEG           |-->| HEIGHT OF VEGETATION
!| MD             |-->| VEGETATION DENSITY * VEGETATION DIAMETER
!| SP             |-->| SPACING BETWEEN VEGETATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_HUTHOFF => FRICTION_HUTHOFF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: HA, CD, MD, HVEG, SP
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P, HM, Z
!
!-----------------------------------------------------------------------
!
      Z  = MIN(HVEG/HA,1.D0)
      P  = 2.D0/3.D0*(1.D0-Z**5)
      HM = SQRT(Z) + (1.D0-Z)*((HA-HVEG)/SP)**P
      CP = CD*MD*HA/HM/HM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
