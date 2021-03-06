!                   ****************
                    SUBROUTINE GRADZ
!                   ****************
!
     &(IKLE,NUBO,CMI,DPX,DPY,DSZ,BETA,AIRST,DXIZ,DYIZ,DSP,DSM,CORR)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/06/2013
!***********************************************************************
!
!brief    Second order for finite volume.
!+
!+            COMPUTES THE Z VARIATIONS (2ND ORDER).
!+            SEE PAPER OF AUDUSSE AND BRISTEAU
!
!history  INRIA
!+
!+        V5P4
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  R.ATA (EDF-LNHE)
!+        21/06/2013
!+        V6P3
!+   adaptation for the new data structure common with FEM
!+   parallelization
!+   clean anr remove unused variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRST          |-->| AREAS OF SUBTRIANGLES WITHIN THE CELLS
!| BETA           |-->|  EXTRAPOLATION COEFFICIENT
!| CMI            |-->| COORDINATES OF THE INTERFACE MIDDLE POINT
!| DPX,DPY        |-->| GRADIENT OF P1 BASE FUNCTIONS
!|                |   | PER TRIANGLE
!| DSM            |<->| EXTRAPOLATED GRADIENTS
!| CORR           |<->| CORRECTION TO HAVE CONSERVATION
!| DSZ            |<--| VARIATION OF Z FOR ORDRE 2
!| DXIZ,DYIZ,DSP  |<->| WORKING TABLES
!|                |   | THE INTERFACE MIDDLE POINT
!| IKLE           |-->| NUMBERING OF NODES IN THE TRIANGLE
!| NUBO           |-->| NUMBERS OF THE TWO NODES FORMING ONE EDGE (SEGMENT)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE INTERFACE_TELEMAC2D, EX_GRADZ => GRADZ
      USE DECLARATIONS_TELEMAC2D, ONLY: NPOIN,NELMAX,NSEG,MESH,V2DPAR,
     &                                  ZF,X,Y
      USE DECLARATIONS_SPECIAL
      USE BIEF, ONLY: OV
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3),NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,*),BETA
      DOUBLE PRECISION, INTENT(INOUT) :: DXIZ(NPOIN),DYIZ(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DSP(NPOIN),DSM(*),CORR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NELMAX),DPY(3,NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: CMI(2,*),AIRST(2,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,I1,I2,I3,JT,J,NSG,NUBO1,NUBO2,ILIM,I
      DOUBLE PRECISION AIRJ,DXTZ,DYTZ,AIX,AIY,AJX,AJY,FACT,TEMPOR
      DOUBLE PRECISION GRADI,GRADJ,GRIJ,GRJI,AMDS,DSH
      DOUBLE PRECISION, ALLOCATABLE :: TMP_X1(:), TMP_X2(:)
!
!-----------------------------------------------------------------------
!
      ! MINMOD LIMITER
      ! EXTRAPOLATION COEFFICIENT
      ILIM = 1
      BETA = 1.D0
! INITIALISATION
      CALL OV('X=C     ', X=DXIZ, C=0.D0, DIM1=NPOIN)
      CALL OV('X=C     ', X=DYIZ, C=0.D0, DIM1=NPOIN)
      CALL OV('X=C     ', X=DSP, C=0.D0, DIM1=NPOIN)
      CALL OV('X=C     ', X=DSM, C=0.D0, DIM1=NPOIN)
      DSZ(1,1:NSEG)=(/(0.D0,I=1,NSEG)/)
      DSZ(2,1:NSEG)=(/(0.D0,I=1,NSEG)/)
!
!************************************************************************
!     THIS LOOP IS TO COMPUTE GRAD(Z) WITH EQUATION 5.2
!************************************************************************
      DO JT=1,NELMAX
!
        I1 = IKLE(JT,1)
        I2 = IKLE(JT,2)
        I3 = IKLE(JT,3)
!
        AIRJ = MESH%SURFAC%R(JT)
        DXTZ = ZF%R(I1)*DPX(1,JT)+ZF%R(I2)*DPX(2,JT)+ZF%R(I3)*DPX(3,JT) ! GRAD_X(Z)|_Tk
        DYTZ = ZF%R(I1)*DPY(1,JT)+ZF%R(I2)*DPY(2,JT)+ZF%R(I3)*DPY(3,JT) ! GRAD_Y(Z)|_Tk
!
        TEMPOR   = AIRJ*DXTZ
        DXIZ(I1) = DXIZ(I1) + TEMPOR ! SUM( |C_k|*GRAD_X(Z)|_Tk )
        DXIZ(I2) = DXIZ(I2) + TEMPOR ! SAME AS I1
        DXIZ(I3) = DXIZ(I3) + TEMPOR ! SAME AS I1 AND I2
!
        TEMPOR   = AIRJ*DYTZ
        DYIZ(I1) = DYIZ(I1) + TEMPOR ! SUM( |C_k|*GRAD_Y(Z)|_Tk )
        DYIZ(I2) = DYIZ(I2) + TEMPOR ! SAME AS I1
        DYIZ(I3) = DYIZ(I3) + TEMPOR ! SAME AS I1 AND I2
!
      ENDDO
!     FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN          ! NPON,NPLAN,ICOM,IAN
        CALL PARCOM2(DXIZ,DYIZ,DYIZ,NPOIN,1,2,2,MESH )
      ENDIF
!
      DO IS=1,NPOIN
        DXIZ(IS) = DXIZ(IS)/(3.D0*V2DPAR%R(IS))  ! DIVIDE BY SUM(|C_k|)
        DYIZ(IS) = DYIZ(IS)/(3.D0*V2DPAR%R(IS))  ! DIVIDE BY SUM(|C_k|)
      ENDDO
! ************************************************************************
!  GRAD(Z)_i IS NOW BUILT BY EQUATION 5.2
! ************************************************************************
!    REBUILDS BY INTERFACE
!
      DO NSG=1,NSEG
        FACT  = AIRST(1,NSG)! USEFUL FOR PARALLELISM
!
        J     = MESH%JMI%I(NSG) ! THIS THE TRIANGLE IN WHICH IS LOCATED CMI
        IF(NCSIZE.GT.1.AND.J.EQ.0)CYCLE  ! THAT MEANS CMI IS NOT LOCATED IN TRIANGLE J
        IF(J.EQ.0)THEN
            WRITE(LU,*)'@GRADZ: PROBLEM TO RETRIEVE ELEMENT'
            WRITE(LU,*)'IN WHICH IS LOCATED CMI'
            WRITE(LU,*)'LOOKING FOR EDGE NUMBER',NSG
            WRITE(LU,*)'WITH NODES NUMBER',NUBO(1,NSG),NUBO(2,NSG)
            CALL PLANTE(1)
            STOP
        ENDIF
!
        NUBO1 = NUBO(1,NSG)
        NUBO2 = NUBO(2,NSG)
!
        AIX   = CMI(1,NSG)-X(NUBO1) ! THESE ARE COORDINATES OF
        AIY   = CMI(2,NSG)-Y(NUBO1) !  VECTOR PM (EQ 5.1)
        AJX   = CMI(1,NSG)-X(NUBO2) ! P: NUBO1 OR NUBO2
        AJY   = CMI(2,NSG)-Y(NUBO2) ! M: CMI(NSG)
!
!        NODE GRADIENTS (PM.GRAD(Z) eq 5.1 of audusse paper)
!
        GRADI = AIX*DXIZ(NUBO1) + AIY*DYIZ(NUBO1)
        GRADJ = AJX*DXIZ(NUBO2) + AJY*DYIZ(NUBO2)
!
        I1 = IKLE(J,1)
        I2 = IKLE(J,2)
        I3 = IKLE(J,3)
!
!        GRADIENT BY TRIANGLE (GRAD(W)_M=GRAD(W)|Tk)
!
        DXTZ =ZF%R(I1)*DPX(1,J) +ZF%R(I2)*DPX(2,J) + ZF%R(I3)*DPX(3,J)
        DYTZ =ZF%R(I1)*DPY(1,J) +ZF%R(I2)*DPY(2,J) + ZF%R(I3)*DPY(3,J)
!
        GRIJ  = AIX*DXTZ + AIY*DYTZ
        GRJI  = AJX*DXTZ + AJY*DYTZ
!
!    EXTRAPOLATES AND CAPS  (EQUATION 5.4)
!
        DSZ(1,NSG)  =  EXLIM(ILIM,BETA,GRADI,GRIJ )
        DSZ(2,NSG)  =  EXLIM(ILIM,BETA,GRADJ,GRJI )
!   FOR PARALLELILSM nomore necessary while we use icom=1 in the next parcom2
!         IF(NCSIZE.GT.1)THEN
!          ! SEE IF WE ARE IN HALO REGION
!          IF(IFABOR(J,1).EQ.-2
!     &   .OR.IFABOR(J,2).EQ.-2
!     &   .OR.IFABOR(J,3).EQ.-2)THEN
!            FACT=0.5D0*AIRST(1,NSG)! TO ACCOUNT ONLY FOR THE HALF OD AIRST
!          ENDIF
!         ENDIF
!
        IF(DSZ(1,NSG).GE.0.D0) THEN
          DSP(NUBO1) = DSP(NUBO1) + FACT*DSZ(1,NSG)
        ELSE
          DSM(NUBO1) = DSM(NUBO1) - FACT*DSZ(1,NSG)
        ENDIF
        IF(DSZ(2,NSG).GE.0.D0) THEN
          DSP(NUBO2) = DSP(NUBO2) + FACT*DSZ(2,NSG)
        ELSE
          DSM(NUBO2) = DSM(NUBO2) - FACT*DSZ(2,NSG)
        ENDIF
!
      ENDDO
!  FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN      ! NPON,NPLAN,ICOM,IAN , HERE ICOM=1 VALUE WITH MAX | |
        CALL PARCOM2(DSP,DSM,DSM,NPOIN,1,2,2,MESH )
      ENDIF
!
!  COMPUTES THE CORRECTIONS NECESSARY TO HAVE CONSERVATION
!
      DO IS=1,NPOIN
        CORR(IS) =  DSM(IS) - DSP(IS)
        AMDS =MAX(DSP(IS),DSM(IS))
        IF(AMDS.GT.0.D0) THEN
          CORR(IS) = CORR(IS)/AMDS
        ENDIF
      ENDDO
!
      DO NSG=1,NSEG
!
        NUBO1 = NUBO(1,NSG)
        NUBO2 = NUBO(2,NSG)
!
        DSH        = DSZ(1,NSG)
        DSZ(1,NSG) = DSH +
     &               MIN(0.D0,CORR(NUBO1))*MAX(0.D0,DSH)+
     &               MAX(0.D0,CORR(NUBO1))*MAX(0.D0,-DSH)
!
        DSH        = DSZ(2,NSG)
        DSZ(2,NSG) = DSH +
     &               MIN(0.D0,CORR(NUBO2))*MAX(0.D0,DSH)+
     &               MAX(0.D0,CORR(NUBO2))*MAX(0.D0,-DSH)
      ENDDO
!-----------------------------------------------------------------------
!
!  FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN  !  X1,X2,X3,NSEG,NPLAN,ICOM,IAN,MESH,OPT,IELM )
        ! Manually creating contiguous arrays
        ALLOCATE(TMP_X1(NSEG))
        ALLOCATE(TMP_X2(NSEG))
        TMP_X1 = DSZ(1,1:NSEG)
        TMP_X2 = DSZ(2,1:NSEG)
        CALL PARCOM2_SEG(TMP_X1,TMP_X2,DSM(1:NSEG),
     &                   NSEG,1,2,2,
     &                   MESH,1,11)
        DSZ(1,1:NSEG) = TMP_X1
        DSZ(2,1:NSEG) = TMP_X2
        DEALLOCATE(TMP_X1)
        DEALLOCATE(TMP_X2)
      ENDIF

      RETURN
      END
