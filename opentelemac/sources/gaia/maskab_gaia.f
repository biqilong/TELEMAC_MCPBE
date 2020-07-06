!                   **********************
                    SUBROUTINE MASKAB_GAIA
!                   **********************
!
     &(HN , Q , QU , QV , NPOIN)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Eliminates negative water depths.
!
!>@warning  USER SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] HN     Water depth
!>@param[in]     NPOIN  Number of points
!>@param[in]     Q      Liquid discharge
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN
!
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: Q(NPOIN),QU(NPOIN),QV(NPOIN)
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!
! CAPS WATER DEPTHS
!
!
      DO I=1,NPOIN
!
!  TREATS NEGATIVE VALUES IN THE DOMAIN
!
        IF(HN(I).LE.0.D0) THEN
          Q(I)  = 0.D0
          QU(I) = 0.D0
          QV(I) = 0.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE MASKAB_GAIA
