!                   ***********************
                    SUBROUTINE POINT_WAQTEL
!                   ***********************
!
     &(MESH2D,IELM1,VENT,WINDX,WINDY,ATMOS,PATMOS,MESH3D,IELM3)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    Memory allocation of structures, aliases, blocks...
!
!history  R ATA (LNHE)
!+        13/05/2015
!+        V7P1
!+
!+        Creation of the file
!
!history  S.E. BOURBAN (HRW)
!+        21/09/2017
!+        V7P3
!+        WAQPROCESS is now a prime number, so that multiple processes
!+        can be called by multiplication of the prime numbers.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_WAQTEL,ONLY:K2,RAYEFF,TAIR,WAQPROCESS,RAYAED2
      USE INTERFACE_WAQTEL, EX_POINT_WAQTEL => POINT_WAQTEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL,         INTENT(IN   ) :: VENT,ATMOS
      INTEGER,         INTENT(IN   ) :: IELM1
      TYPE(BIEF_OBJ ), INTENT(INOUT) :: WINDX,WINDY
      TYPE(BIEF_OBJ ), INTENT(INOUT) :: PATMOS
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE(BIEF_MESH), INTENT(INOUT),OPTIONAL :: MESH3D
      INTEGER,         INTENT(IN   ),OPTIONAL :: IELM3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      IF( 2*INT(WAQPROCESS/2).EQ.WAQPROCESS .OR.
     &    5*INT(WAQPROCESS/5).EQ.WAQPROCESS ) THEN
        CALL BIEF_ALLVEC(1,K2   ,'K2    ',IELM1,1,1,MESH2D)
      ELSE
        CALL BIEF_ALLVEC(1,K2    ,'K2    ',    0,1,0,MESH2D)
      ENDIF
!
!     SUN RAY EFFECTS CAN BE 2D OR 3D
!
      IF( 3*INT(WAQPROCESS/3).EQ.WAQPROCESS .OR.
     &    5*INT(WAQPROCESS/5).EQ.WAQPROCESS ) THEN
        IF(PRESENT(MESH3D))THEN
          CALL BIEF_ALLVEC(1,RAYEFF,'RAYEFF',IELM3,1,1,MESH3D)
        ELSE
          CALL BIEF_ALLVEC(1,RAYEFF,'RAYEFF',IELM1,1,1,MESH2D)
        ENDIF
      ELSE
        IF(PRESENT(MESH3D))THEN
          CALL BIEF_ALLVEC(1,RAYEFF,'RAYEFF',    0,1,0,MESH3D)
        ELSE
          CALL BIEF_ALLVEC(1,RAYEFF,'RAYEFF',    0,1,0,MESH2D)
        ENDIF
      ENDIF
!
!     WIND GIVEN IN P1
!
      IF(.NOT.VENT) THEN
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',IELM1,1,1,MESH2D)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',IELM1,1,1,MESH2D)
      ENDIF
!
!     ATMOSPHERIC PRESSURE GIVEN IN P1
!
      IF(.NOT.ATMOS) THEN
        CALL BIEF_ALLVEC(1,PATMOS,'PATMOS',IELM1,1,1,MESH2D)
      ENDIF
!
!     AIR TEMPERATURE GIVEN IN P1
!
      CALL BIEF_ALLVEC(1,TAIR  ,'TAIR  ',IELM1,1,1,MESH2D)
!
!     SOLAR RADIATION FOR AED2
!
      IF(13*INT(WAQPROCESS/13).EQ.WAQPROCESS) THEN
        CALL BIEF_ALLVEC(1, RAYAED2,  'RAYAED', IELM1,1,1,MESH2D)
      ELSE
        CALL BIEF_ALLVEC(1, RAYAED2,  'RAYAED', 0,1,0,MESH2D)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
