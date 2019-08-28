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
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVAR,J,NRFO,INUM(14),IH,I,ITRAC
!
      DOUBLE PRECISION ERREUR,FLUX1,PERDUE,DENOM,RELATI
      DOUBLE PRECISION XMES(14),YMES(14),HAUT(14),ARR_TIME(3)
      DOUBLE PRECISION P_DMAX,P_DMIN,P_DSUM
      EXTERNAL P_DMAX,P_DMIN,P_DSUM
      LOGICAL FIND_ARR_TIME(3)
      LOGICAL DEJA_UTIMP
!
      DATA    DEJA_UTIMP /.FALSE./
!
      SAVE INUM,IH,HAUT,ARR_TIME,FIND_ARR_TIME,DEJA_UTIMP
      DOUBLE PRECISION ERRL1,ERRL2,ERRLINF,A
!
      INTEGER IREF
      DOUBLE PRECISION TIMEREF
      INTEGER FINDREF(500)
!
      INTEGER IELEM,I1,I2,I3
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3
      DOUBLE PRECISION DETM
!
!     CUSTOM PRINTOUT PERIOD
      LOGICAL USE_CUSTOM_LEOPRD
      INTEGER CUSTOM_LEOPRD
      PARAMETER (USE_CUSTOM_LEOPRD=.TRUE.)
!
!***********************************************************************
!
      TYPE(BIEF_OBJ) ECLOC, EPLOC, ETA
      TYPE(BIEF_OBJ) UM, U_2, V_2, UV
      TYPE(BIEF_OBJ) SVIDE
      TYPE(BIEF_OBJ) ONES, MASSM

      DOUBLE PRECISION EPTOT
      DOUBLE PRECISION ECTOT,ETOT
      DOUBLE PRECISION MASSBALANCE
!
!-----------------------------------------------------------------------
!
!     TABLE ALLOCATION
      ALLOCATE(ETA%R(NPOIN))
      ALLOCATE(EPLOC%R(NPOIN))
      ALLOCATE(ECLOC%R(NPOIN))
      ALLOCATE(UM%R(NPOIN))
      ALLOCATE(U_2%R(NPOIN))
      ALLOCATE(V_2%R(NPOIN))
      ALLOCATE(UV%R(NPOIN))
      ALLOCATE(SVIDE%R(NPOIN))
      ALLOCATE(ONES%R(NPOIN))
      ALLOCATE(MASSM%R(NPOIN))

      CALL BIEF_ALLVEC(1, ETA,'ETA     ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, EPLOC,'EPLOC   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, ECLOC,'ECLOC   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, UM,'UM      ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, U_2,'U_2     ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, V_2,'V_2     ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, UV,'UV     ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, SVIDE,'SVIDE   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, ONES,'ONES   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, MASSM,'MASSM   ',IELMH, 1,1,MESH)
!
      ETA%R(:) = 0.D0
      EPLOC%R(:) = 0.D0
      ECLOC%R(:) = 0.D0
      UM%R(:) = 0.D0
      U_2%R(:) = 0.D0
      V_2%R(:) = 0.D0
      UV%R(:) = 0.D0
      SVIDE%R(:) = 0.D0
      ONES%R(:) = 1.D0
      MASSM%R(:) = 0.D0

      ETA%TYPE = 2
      EPLOC%TYPE = 2
      ECLOC%TYPE = 2
      UM%TYPE = 2
      U_2%TYPE = 2
      V_2%TYPE = 2
      UV%TYPE = 2
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
! ENERGY BALANCE
!-----------------------------------------------------------------------
!
      IF(MOD(LT,CUSTOM_LEOPRD).EQ.0) THEN
!
! POTENTIAL ENERGY
!
!       EP :  0.5*g* int_S H**2 PSII PSJ dS
        CALL OS('X=YZ    ' , X=ETA , Y=H  , Z=H)
        CALL VECTOR(EPLOC,'=     ','MASVEC          ',
     &        IELMH,
     &        4.905D0,ETA,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &        MESH,MSK,MASKEL)
!
        EPTOT = 0.D0
        DO I=1,NPOIN
          EPTOT = EPTOT + EPLOC%R(I)
        ENDDO
!
! LOCAL KINETIC ENERGY
!
!       EC : 0.5*int_S H*U^2 PSII PSJ dS
        CALL OS('X=YZ    ' , X=U_2 , Y=U  , Z=U )
        CALL OS('X=YZ    ' , X=V_2 , Y=V  , Z=V )
        CALL OS('X=Y+Z    ' , X=UV , Y=U_2  , Z=V_2  )
        CALL OS('X=YZ     ' , X=UM , Y=UV  , Z=H  )

        CALL VECTOR(ECLOC,'=     ','MASVEC          ',
     &        IELMH,
     &        0.5D0,UM,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &        MESH,MSK,MASKEL)
!
! TOTAL KINETIC ENERGY
!
        ECTOT = 0.D0
        DO I=1,NPOIN
          ECTOT = ECTOT + ECLOC%R(I)
        ENDDO
!
! TOTAL ENERGY BALANCE
!
        IF(NCSIZE.GT.1) THEN
          EPTOT = P_DSUM(EPTOT)
          ECTOT = P_DSUM(ECTOT)
        ENDIF
!
        ETOT = EPTOT+ECTOT
!
! WRITE ENERGY BALANCE IN TXT FILE
!
!       IF PARALLEL ONLY WRITE WITH FIRST NODE
        IF(NCSIZE.GT.1) THEN
          IF(IPID.EQ.0) THEN
            IF(LT.EQ.0) THEN
              OPEN(UNIT=1846987,FILE='../energy_balance.txt')
            ELSE
              OPEN(UNIT=1846987,FILE='../energy_balance.txt',
     &             POSITION='append')
            ENDIF
            WRITE(1846987,1004) AT, ECTOT, EPTOT, ETOT
            CLOSE(1846987)
          ENDIF
        ELSE
          IF(LT.EQ.0) THEN
            OPEN(UNIT=1846987,FILE='../energy_balance.txt')
          ELSE
            OPEN(UNIT=1846987,FILE='../energy_balance.txt',
     &           POSITION='append')
          ENDIF
          WRITE(1846987,1004) AT, ECTOT, EPTOT, ETOT
          CLOSE(1846987)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! MASS BALANCE
!-----------------------------------------------------------------------
!
! WRITE MASS BALANCE IN TXT FILE
!
      IF(MOD(LT,CUSTOM_LEOPRD).EQ.0) THEN
!       IF PARALLEL ONLY WRITE WITH FIRST NODE
        IF(NCSIZE.GT.1) THEN
          IF(IPID.EQ.0) THEN
            IF(LT.EQ.0) THEN
              OPEN(UNIT=1846154,FILE='../mass_balance.txt')
            ELSE
              OPEN(UNIT=1846154,FILE='../mass_balance.txt',
     &             POSITION='append')
            ENDIF
!           WRITE CURRETN MASS AND MASS BALANCE
            MASSBALANCE = MASSE0+MASSET+MASENT-MASSE2
            WRITE(1846154,*) AT, MASSE2, MASSBALANCE
            CLOSE(1846154)
          ENDIF
        ELSE
          IF(LT.EQ.0) THEN
            OPEN(UNIT=1846154,FILE='../mass_balance.txt')
          ELSE
            OPEN(UNIT=1846154,FILE='../mass_balance.txt',
     &           POSITION='append')
          ENDIF
!         WRITE CURRETN MASS AND MASS BALANCE
          MASSBALANCE = MASSE0+MASSET+MASENT-MASSE2
          WRITE(1846154,*) AT, MASSE2, MASSBALANCE
          CLOSE(1846154)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! MASS MATRIX AT FINAL TIME
!-----------------------------------------------------------------------
!
      IF(LT.EQ.NIT) THEN

!     MASS MATRIX FILE COMPUTED WITH VECTOR
        OPEN(UNIT=561456,FILE="../mass_matrix.txt")
!       MASS MATRIX CALCULATION
        CALL VECTOR(MASSM,'=     ','MASVEC          ',
     &        IELMH,
     &        1.D0,ONES,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &        MESH,MSK,MASKEL)

        DO I=1,MESH%NPOIN
          WRITE(561456,*) MASSM%R(I)
        ENDDO
        CLOSE(561456)

      ENDIF
!
!-----------------------------------------------------------------------
! ERRORS TIME SERIES (WITH RESPECT TO ANALYTIC SOLUTION)
!-----------------------------------------------------------------------
!
!TODO: Linf, L1, L2
!
!-----------------------------------------------------------------------
! DEPTH TIME SERIES ON POINT
!-----------------------------------------------------------------------
!
!TODO: ANALYTIC AND COMPUTED TIME SERIES
!
!-----------------------------------------------------------------------
! VELOCITY TIME SERIES ON POINT
!-----------------------------------------------------------------------
!
!TODO: ANALYTIC AND COMPUTED TIME SERIES
!
!-----------------------------------------------------------------------
!
!     TABLE DEALLOCATION
      DEALLOCATE(ETA%R)
      DEALLOCATE(EPLOC%R)
      DEALLOCATE(ECLOC%R)
      DEALLOCATE(UM%R)
      DEALLOCATE(U_2%R)
      DEALLOCATE(V_2%R)
      DEALLOCATE(UV%R)
      DEALLOCATE(SVIDE%R)
      DEALLOCATE(MASSM%R)
      DEALLOCATE(ONES%R)

      CALL BIEF_DEALLVEC(ETA)
      CALL BIEF_DEALLVEC(EPLOC)
      CALL BIEF_DEALLVEC(ECLOC)
      CALL BIEF_DEALLVEC(UM)
      CALL BIEF_DEALLVEC(U_2)
      CALL BIEF_DEALLVEC(V_2)
      CALL BIEF_DEALLVEC(UV)
      CALL BIEF_DEALLVEC(SVIDE)
      CALL BIEF_DEALLVEC(MASSM)
      CALL BIEF_DEALLVEC(ONES)
!
 1001 FORMAT((A,I3,A,F6.2,A))
 1002 FORMAT((A,F7.1,A))
 1003 FORMAT((A,I5,A,E25.17,A))
 1004 FORMAT((E15.6,E25.10,E25.10,E25.10))
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE