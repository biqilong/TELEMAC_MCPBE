!                    *******************************
                     SUBROUTINE USER_UTIMP_TELEMAC2D
!                    *******************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    WRITES OUT ADDITIONAL OUTPUT REQUIRED BY THE USER.
!
!note     THIS SUBROUTINE IS CALLED IN THE SAME PLACES AS THE
!+                MAIN TELEMAC2D OUTPUT SUBROUTINE (NAMED DESIMP),
!+                I.E. CALLED TWICE:
!+
!note   (1) ONCE PER RUN, WHEN LTL==0, INDEPENDENTLY OF WHETHER
!+             'OUTPUT OF INITIAL CONDITIONS : YES' IS SET OR NOT
!note   (2) EACH TIME STEP JUST AFTER DESIMP-OUTPUT
!
!history  JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!+        **/08/2003
!+        V5P4
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| GRADEBL        |-->| FIRST TIME STEP FOR GRAPHIC OUTPUTS
!| GRAPRDL        |-->| PERIOD OF GRAPHIC OUTPUTS
!| LISDEBL        |-->| FIRST TIME STEP FOR LISTING OUTPUTS
!| LISPRDL        |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,ITRAC,I1(20),I2(20),IPOIN,I
      LOGICAL DEJAUTIMP
      DOUBLE PRECISION MINIMORUM(20),MAXIMORUM(20),MAXIM(20),MINIM(20)
      DOUBLE PRECISION EIKON,C,AREA
      DATA DEJAUTIMP/.FALSE./
      SAVE MINIMORUM,MAXIMORUM,I1,I2
!
!     CUSTOM PRINTOUT PERIOD
      LOGICAL USE_CUSTOM_LEOPRD
      INTEGER CUSTOM_LEOPRD
      PARAMETER (USE_CUSTOM_LEOPRD=.TRUE.)
!
      TYPE(BIEF_OBJ) ONES, MASSM
      TYPE(BIEF_OBJ) SVIDE
!
      DOUBLE PRECISION MASSTOT
!
      ALLOCATE(SVIDE%R(NPOIN))
      ALLOCATE(ONES%R(NPOIN))
      ALLOCATE(MASSM%R(NPOIN))
!
      CALL BIEF_ALLVEC(1, SVIDE,'SVIDE   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, ONES,'ONES   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, MASSM,'MASSM   ',IELMH, 1,1,MESH)
!
      SVIDE%R(:) = 0.D0
      ONES%R(:) = 1.D0
      MASSM%R(:) = 0.D0
!
      SVIDE%TYPE = 2
      ONES%TYPE = 2
      MASSM%TYPE = 2
!
!-----------------------------------------------------------------------
!
!     DEFINING CUSTOM PRINTOUT PERIOD
      IF(USE_CUSTOM_LEOPRD) THEN
        CUSTOM_LEOPRD=1
      ELSE
        CUSTOM_LEOPRD=LEOPRD
      ENDIF
!
!-----------------------------------------------------------------------
!
!     OBSOLETE:
!      CALL CPSTVC(T%ADR(1)%P,T1)
!      IF(.NOT.DEJAUTIMP) THEN
!        DO K=1,20
!          MINIMORUM(K)=1.D99
!          MAXIMORUM(K)=-MINIMORUM(K)
!          I1(K)=0
!          I2(K)=0
!        ENDDO
!        DEJAUTIMP=.TRUE.
!      ENDIF
!      DO K=1,20
!        MINIM(K)=1.D99
!        MAXIM(K)=-MINIM(K)
!      ENDDO
!      DO ITRAC=1,T%N
!        DO K=1,T%ADR(ITRAC)%P%DIM1
!          IF(T%ADR(ITRAC)%P%R(K).LT.MINIMORUM(ITRAC)) THEN
!            I1(ITRAC)=K
!            MINIMORUM(ITRAC)=T%ADR(ITRAC)%P%R(K)
!          ENDIF
!          MINIM(ITRAC)=MIN(MINIM(ITRAC),T%ADR(ITRAC)%P%R(K))
!        ENDDO
!        DO K=1,T%ADR(ITRAC)%P%DIM1
!          IF(T%ADR(ITRAC)%P%R(K).GT.MAXIMORUM(ITRAC)) THEN
!            I2(ITRAC)=K
!            MAXIMORUM(ITRAC)=T%ADR(ITRAC)%P%R(K)
!          ENDIF
!          MAXIM(ITRAC)=MAX(MAXIM(ITRAC),T%ADR(ITRAC)%P%R(K))
!        ENDDO
!        DO IPOIN=1,T%ADR(ITRAC)%P%DIM1
!          EIKON=( (X(IPOIN)-15.D0)**2 + (Y(IPOIN)-10.D0)**2 ) / 5.D0
!          T1%R(IPOIN)=T%ADR(ITRAC)%P%R(IPOIN)-EXP(-EIKON)
!        ENDDO
!      ENDDO
!      DO ITRAC=1,NTRAC
!        MINIMORUM(ITRAC)=P_DMIN(MINIMORUM(ITRAC))
!        MINIM(ITRAC)=P_DMIN(MINIM(ITRAC))
!        WRITE(LU,*)
!     &  NAMETRAC(ITRAC),' MINIMORUM=',MINIMORUM(ITRAC),' EN ',I1(ITRAC)
!        WRITE(LU,*)
!     &  NAMETRAC(ITRAC),' MINIM=',MINIM(ITRAC),' EN ',I1(ITRAC)
!      ENDDO
!      DO ITRAC=1,NTRAC
!        MAXIMORUM(ITRAC)=P_DMAX(MAXIMORUM(ITRAC))
!        MAXIM(ITRAC)=P_DMAX(MAXIM(ITRAC))
!        WRITE(LU,*)
!     &  NAMETRAC(ITRAC),' MAXIMORUM=',MAXIMORUM(ITRAC),' EN ',I2(ITRAC)
!        WRITE(LU,*)
!     &  NAMETRAC(ITRAC),' MAXIM=',MAXIM(ITRAC),' EN ',I2(ITRAC)
!      ENDDO
!!     COMPUTING THE MEAN DEVIATION
!      AREA=0.D0
!      DO IPOIN=1,T%ADR(1)%P%DIM1
!        AREA=AREA+VOLU2D%R(IPOIN)
!      ENDDO
!      IF(NCSIZE.GT.1) AREA=P_DSUM(AREA)
!      DO ITRAC=1,T%N
!        C=0.D0
!        DO IPOIN=1,T%ADR(ITRAC)%P%DIM1
!          EIKON=( (X(IPOIN)-15.D0)**2 + (Y(IPOIN)-10.D0)**2 ) / 5.D0
!          C=C+VOLU2D%R(IPOIN)*(T%ADR(ITRAC)%P%R(IPOIN)-EXP(-EIKON))**2
!        ENDDO
!        IF(NCSIZE.GT.1) THEN
!          C=P_DSUM(C)
!        ENDIF
!        C=C/AREA
!        WRITE(LU,*)
!     &  NAMETRAC(ITRAC),' 10**3 STANDARD DEVIATION=',1.D3*SQRT(C)
!      ENDDO
!
!
!-----------------------------------------------------------------------
! MASS MATRIX AT FINAL TIME
!-----------------------------------------------------------------------
!
!     COMPUTE MASS MATRIX
      IF(MOD(LT,CUSTOM_LEOPRD).EQ.0) THEN
!
        CALL VECTOR(MASSM,'=     ','MASVEC          ',
     &        IELMH,
     &        1.D0,ONES,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &        MESH,MSK,MASKEL)
!
!       COMPUTE TOTAL MASS
        MASSTOT=0.D0
        DO I=1,MESH%NPOIN
          MASSTOT = MASSTOT + MASSM%R(I)
        ENDDO
      ENDIF
!
!     WRITE MASS AT FINAL TIME
      IF(LT.EQ.NIT) THEN
        IF(NCSIZE.GT.1) THEN
          IF(IPID.EQ.0) THEN
            OPEN(UNIT=561456,FILE='../mass_matrix_tf.txt')
          ENDIF
          DO I=1,MESH%NPOIN
            WRITE(561456,*) MASSM%R(I)
          ENDDO
          CLOSE(561456)
        ELSE
          OPEN(UNIT=561456,FILE='../mass_matrix_tf.txt')
          DO I=1,MESH%NPOIN
            WRITE(561456,*) MASSM%R(I)
          ENDDO
          CLOSE(561456)
        ENDIF
      ENDIF
!
      DEALLOCATE(MASSM%R)
      CALL BIEF_DEALLVEC(MASSM)
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
