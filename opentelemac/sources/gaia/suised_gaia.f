!                    *****************
                     SUBROUTINE SUISED
!                    *****************
!
     &(S3D_EPAI,S3D_HDEP,S3D_CONC,ZR,ZF,T2,NPOIN2,S3D_NCOUCH,S3D_ITASS,
     & NSUIS,FFORMAT,PRIVE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Reads sedimentological data for a resuming computation.
!
!>@warning  Implementation with selafin only works with s3d_gibson
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     FFORMAT    Binary of previous sediment computation file
!>@param[in]     NPOIN2     Number of 2d points
!>@param[in]     NSUIS      Logical unit number of the previous sediment
!!                          computation file
!>@param[in,out] S3D_CONC   Concentration of mud bed layer
!!                          (multilayer model)
!>@param[in,out] S3D_EPAI   Thickness of solid bed layer
!>@param[in,out] S3D_HDEP   Total thickness of mud deposit
!>@param[in]     S3D_NCOUCH Number of layers within the bed
!>@param[in]     ZF         Elevation of bed
!!                          (from geometry or 3d previous computation file)
!>@param[out]    ZR         Elevation of rigid bed
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!----------------------------------------------------------------------
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2, PRIVE
      INTEGER, INTENT(IN)             :: NPOIN2,S3D_NCOUCH
      INTEGER, INTENT(IN)             :: S3D_ITASS, NSUIS
!
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_EPAI(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_CONC(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(OUT)   :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_HDEP(NPOIN2)
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
!
!----------------------------------------------------------------------
!
      INTEGER IPOIN,ISTAT
!
      INTEGER           :: IB(10),IERR
      CHARACTER(LEN=80) :: TITLE
      INTEGER           :: RECORD
      LOGICAL           :: FOUND
      CHARACTER(LEN=16) :: VARNAME
      INTEGER :: NPOIN_SUIS,TYP_ELEM,NELEM,NPTFR,NDP_SUIS,NPLAN
      DOUBLE PRECISION :: TIME
!
      INTEGER           :: NVAR,I
!
!   READ THE HEADER AND CHECK QUANTITIES / OPTIONS
      WRITE(LU,*) ' '
      WRITE(LU,*)
     &   'READING BED SEDIMENT FROM PREVIOUS COMPUTATION FILE'
      WRITE(LU,*) ' '
!
      CALL READ_MESH_INFO(FFORMAT,NSUIS,TITLE,NVAR,NPOIN_SUIS,TYP_ELEM,
     &                    NELEM,NPTFR,NPTIR,NDP_SUIS,NPLAN)
!
      IF( (NPOIN_SUIS.NE.NPOIN2) .OR. (NDP_SUIS.NE.3) ) THEN
        WRITE(LU,*) 'SUISED: NUMBER OF NODES NOT COMPATIBLE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(S3D_ITASS.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!       LOOK FOR 1. BED THICKNESS PER LAYER
!       IN THE RESULT FILE:
!       VARIABLE NAMES ARE BED DZ1, BED DZ2, etc....FOR EACH LAYER
!       UP TO S3D_NCOUCH(N.B. S3D_NCOUCHIS THE LOWEST LAYER)
!
!       ZERO THE TOTAL THICKNESS ARRAY
        DO IPOIN = 1,NPOIN2
            S3D_HDEP(IPOIN) = 0.D0
        ENDDO
!
!       FIND LAYER THICKNESS VARIABLES IN THE 2D RESULT FILE
        DO I=1,S3D_NCOUCH
!
!         MAKE THE NUMBERED NAME STRING
          IF(I.LT.10) THEN
            WRITE(VARNAME,'(A5,I1,A10)')  'LAYER',I,'  THICKNES'
          ELSEIF(I.LT.100) THEN
            WRITE(VARNAME,'(A5,I2,A9)')  'LAYER',I,' THICKNES'
          ELSE
            WRITE (LU,*) 'SUISED: NOT IMPLEMENTED FOR ',S3D_NCOUCH,
     &                   ' LAYERS'
            CALL PLANTE(1)
            STOP
          ENDIF
!
          WRITE(LU,*)  'CHECKING PREVIOUS SED FILE FOR VARIABLE'

!         PUT RESULTS INTO T2 BIEF OBJECT
          RECORD = -1 ! To get the last time step
          CALL FIND_VARIABLE(FFORMAT,NSUIS,VARNAME,T2%R,NPOIN_SUIS,IERR,
     &                   RECORD=RECORD,TIME_RECORD=TIME)
!
          IF (IERR.EQ.0)  THEN
            WRITE(LU,*)
     &       'BED LAYER THICKNESS (LAYER',I,') FOUND IN GEOMETRY FILE'
!
!           TRANSFER THE DATA FROM BIEF OBJECT TO DOUBLE ARRAY
            DO IPOIN = 1,NPOIN2
              S3D_EPAI(IPOIN,I) = T2%R(IPOIN)
            ENDDO
          ELSE
            WRITE(LU,*)
     &       'BED LAYER THICKNESS (LAYER',I,
     &       ') NOT FOUND IN GEOMETRY FILE'
          ENDIF
!
!         SUM THE TOTAL THICKNESS OF DEPOSIT (S3D_HDEP)
          DO IPOIN = 1,NPOIN2
            S3D_HDEP(IPOIN) = S3D_HDEP(IPOIN)+S3D_EPAI(IPOIN,I)
          ENDDO
!
        ENDDO
!
!       ELEVATION OF RIGID BED
        DO IPOIN = 1,NPOIN2
          ZR(IPOIN) = ZF(IPOIN)-S3D_HDEP(IPOIN)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF (S3D_ITASS.EQ.2) THEN
        WRITE(LU,*) 'OPTION S3D_ITASS == 2 NOT IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     DEALLOCATE TEMPORARY STORAGE
!
      RETURN
      END
!
