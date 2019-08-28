!                    *****************
                     SUBROUTINE VISCSA
!                    *****************
!
     &(VISCVI,VISCNU,NU,
     & DNUVIH,DNUVIV,AK,EP,STRAIN)
!
!***********************************************************************
! TELEMAC3D   V8P0                                 21/08/2018
!***********************************************************************
!
!brief    COMPUTES THE TURBULENT VISCOSITY
!+           AND TURBULENT THERMAL DIFFUSIVITY, K AND EPSILON.
!            USING THE SPALART ALLMARAS MODLE
!
!history A. Bourgoin & R. ATA
!+        21/08/2018
!+        V8P0
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT ENERGY
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EMIN           |-->| MINIMUM VALUE OF EPSILON FOR CLIPPING
!| EP             |-->| TURBULENT DISSIPATION
!| KMIN           |-->| MINIMUM VALUE OF K FOR CLIPPING
!| VISCVI         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR VELOCITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_VISCSA => VISCSA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV
      TYPE(BIEF_OBJ), INTENT(INOUT):: VISCVI,VISCNU,AK,EP
      TYPE(BIEF_OBJ), INTENT(IN)   :: NU,STRAIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: PROPNU2,CHI,CHI3,CV13,FV1,SQCMU,TIERS,SSIG
      INTEGER I,NPOIN3
!
!***********************************************************************
!
      NPOIN3 = NU%DIM1
      PROPNU2 = 1.D-6
      CV13=(7.1D0)**3
      SQCMU=0.3D0
      SSIG=3.D0/2.D0
      TIERS=1.D0/3.D0

      DO I=1,NPOIN3
        CHI=NU%R(I)/PROPNU2
        CHI3=CHI**3
        FV1=CHI3/(CHI3+CV13)
        AK%R(I)=FV1**TIERS*NU%R(I)*SQRT(STRAIN%R(I))/SQCMU
        EP%R(I)=FV1**0.5D0*(SQCMU*AK%R(I))**2/(NU%R(I)+DNUVIH)
!
        VISCVI%ADR(1)%P%R(I)=FV1*NU%R(I)+DNUVIH
        VISCVI%ADR(2)%P%R(I)=FV1*NU%R(I)+DNUVIH
        VISCVI%ADR(3)%P%R(I)=FV1*NU%R(I)+DNUVIV
        VISCNU%ADR(1)%P%R(I)=SSIG*(NU%R(I)+DNUVIH)
        VISCNU%ADR(2)%P%R(I)=SSIG*(NU%R(I)+DNUVIH)
        VISCNU%ADR(3)%P%R(I)=SSIG*(NU%R(I)+DNUVIV)
      ENDDO

!-----------------------------------------------------------------------
      RETURN
      END

