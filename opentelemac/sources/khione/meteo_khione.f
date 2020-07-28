!                       *******************
                        MODULE METEO_KHIONE
!                       *******************
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Module containing all subroutines to deal with atmospheric
!+        exchange, whether its dynamics (wind, pressure, etc.) or its
!+        thermal budget (air temperature, solar radiation, cloud, etc.)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: POINT_METEO,SYNC_METEO,DEALL_METEO,
     &   WINDX,WINDY,WINDZ, MODELZ, ALPHSD,ALPHRD,
     &   TAIR,TDEW,CLDC,VISBI,PLUIE,PATM,
     &   CST_WINDX,CST_WINDY,CST_WINDS,CST_WINDD,CST_PATM,
     &   CST_TAIR,CST_TDEW,CST_CLDC,CST_VISBI,CST_PLUIE
!
!=======================================================================
!
!       1) METEOROLOGICAL DRIVERS
!
!     NOTE: A METEOROLOGICAL VARIBALE (SAY TAIR) IS ASSOCIATED WITH
!     A DOUBLE PRECISION CONSTANT (CST_TAIR, TAKEN FROM THE STEERING
!     FILE, OR THE DICO DEFAULT) AND A LOGICAL, INC_TAIR, WHETHER THE
!     VARIABLE WAS FOUND IN ANY OF THE METEO FILES (ASCII OR BINARY)
!
!-----------------------------------------------------------------------

!     TAIR: AIR TEMPERATURE
!     > Measured in degrees oC
!
      TYPE(BIEF_OBJ), TARGET :: TAIR
      DOUBLE PRECISION       :: CST_TAIR
      LOGICAL                :: INC_TAIR
!
!     TDEW: DEWPOINT TEMPERATURE
!     > Measured in degrees oC
!
!     The dewpoint temperature is the temperature at which the air can
!     no longer "hold" all of the water vapor which is mixed with it,
!     and some of the water vapor must condense into liquid water.
!     The dew point is always lower than (or equal to) the air
!     temperature.
!     If the air temperature cools to the dew point, or if the dew point
!     rises to equal the air temperature, then dew, fog or clouds begin
!     to form. At this point where the dew point temperature equals the
!     air temperature, the relative humidity is 100%.
!
      TYPE(BIEF_OBJ), TARGET :: TDEW
      DOUBLE PRECISION       :: CST_TDEW
      LOGICAL                :: INC_TDEW
!
!     CLDC: CLOUD COVER
!     > Measured in tenth
!
!     Clouds produce significant changes in the evolution of the other
!     meteorological components (the duration of the sun shine, solar
!     radiation, temperature, air humidity, atmospheric precipitation,
!     water  drops  or  ice  crystals  etc.), through  their  size  and
!     form, life duration and their constitution.
!     Cloud cover estimated percentages:
!     -      0%: No clouds
!     -   1-10%: Clear
!     -  11-25%: Isolated
!     -  26-50%: Scattered
!     -  51-90%: Broken
!     -   0>90%: Overcast
!     Cloud cover can also be referred to as nebulosity, or the
!     ambiguous nature of clouds, or cloud-like-ness, and therefore is
!     directly related to the humidity. The more significant the
!     extension and the vertical thickness of the clouds, the higher
!     the value of the nebulosity will be.
!
      TYPE(BIEF_OBJ), TARGET :: CLDC
      DOUBLE PRECISION       :: CST_CLDC
      LOGICAL                :: INC_CLDC
!
!     VISBI: VISIBILITY
!     > Measured in meters
!
!     Visibility is a measure of the distance at which an object or
!     light can be clearly discerned. Note that in the dark,
!     meteorological visibility is still the same as in daylight for
!     the same air.
!
      TYPE(BIEF_OBJ), TARGET :: VISBI
      DOUBLE PRECISION       :: CST_VISBI
      LOGICAL                :: INC_VISBI
!
!     PLUIE: RAIN
!     > Measured in meters
      TYPE(BIEF_OBJ), TARGET :: PLUIE
      DOUBLE PRECISION       :: CST_PLUIE
      LOGICAL                :: INC_PLUIE
!
!     SNOW: SNOW
!     > Measured in meters
      TYPE(BIEF_OBJ), TARGET :: SNOW
      DOUBLE PRECISION       :: CST_SNOW
      LOGICAL                :: INC_SNOW
!
!     ALPHSD: SUN SET ANGLE
!     > Measured in degrees, 180 degrees for the horizontal
      DOUBLE PRECISION    ALPHSD
!
!     ALPHRD: SUN RISE ANGLE
!     > Measured in degrees, 0 degrees for the horizontal
      DOUBLE PRECISION    ALPHRD
!
!
!     WINDX,WINDY : WIND
!     > Measured in m/s at WINDZ above the water surface
! or  WINDS,WINDD : WIND
!     > Measured in m/s and degree angle clockwise from north
!
      TYPE(BIEF_OBJ), TARGET :: WINDX,WINDY
      DOUBLE PRECISION       :: CST_WINDX,CST_WINDY
      LOGICAL                :: INC_WINDX,INC_WINDY
      DOUBLE PRECISION       :: CST_WINDS,CST_WINDD
      LOGICAL                :: INC_WINDS,INC_WINDD
!
!     WINDZ
!     > Heigh above the water surface at which the wind is measured
      DOUBLE PRECISION       :: WINDZ
!
!
      TYPE(BIEF_OBJ), TARGET :: PATM
      DOUBLE PRECISION       :: CST_PATM
      LOGICAL                :: INC_PATM
!
!     MODELZ
!     > Elevation of the model domain relative to mean sea levels
      DOUBLE PRECISION       :: MODELZ
!
!
!     WORKING ARRAYS
      TYPE(BIEF_OBJ), TARGET :: WTMP1,WTMP2
!
!
!=======================================================================
!
!       2) METEOROLOGICAL CONSTANTS
!
!-----------------------------------------------------------------------
!
!
!
!=======================================================================
!
!       3) METEOROLOGICAL FILE INPUTS
!
!-----------------------------------------------------------------------
!
!     ONLY TWO METEOROLOGY FILES FOR NOW - A:ASCII AND B:BINARY
!     TO RECORD TEMPORAL AND SPATIAL VARIATIONS
!
!     TYPE BIEF_FILE
!       INTEGER LU               : LOGICAL UNIT TO OPEN THE FILE
!       CHARACTER(LEN=PATH_LEN) NAME  : NAME OF FILE
!       CHARACTER(LEN=6) TELNAME : NAME OF FILE IN TEMPORARY DIRECTORY
!       CHARACTER(LEN=8) FMT     : FORMAT (SERAFIN, MED, ETC.)
!       CHARACTER(LEN=9) ACTION  : READ, WRITE OR READWRITE
!       CHARACTER(LEN=3) BINASC  : ASC FOR ASCII OR BIN FOR BINARY
!       CHARACTER(LEN=12) TYPE   : KIND OF FILE
!     TYPE BIEF_FILE
!
      TYPE(BIEF_FILE)  :: METEO_FILES(2)
!
!     READING THE FILES IN FULL ONLY ONCE
      LOGICAL          :: METEO_DEJA(2)
!
!-----------------------------------------------------------------------
!
!     COMMON TO BOTH ASCII AND BINARY FILES
!
!     MAXIMUM NUMBER OF VALUE ON EACH LINE (VARIABLES FOR EACH POINT)
      INTEGER, PARAMETER :: METEO_MAXVALUE = 210
      CHARACTER(LEN=16), TARGET  :: METEO_CHOIX(2,METEO_MAXVALUE)
      CHARACTER(LEN=16), TARGET  :: METEO_UNITS(2,METEO_MAXVALUE)
!
!-----------------------------------------------------------------------
!
!     SPECIFICS FOR THE ASCII FILE
!
!     INTERPOLATION IN TIME AND IN SPACE FOR MULTIPLE VALUES
      INTEGER          :: NTIMEA,NVALUEA,NPOINA
      INTEGER          :: ITIMEA1,ITIMEA2
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::
     &                    TIMEA, XPOINA,YPOINA
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::
     &                    VALUEA1,VALUEA2
!
!     MAXIMUM NUMBER OF CHARACTERS PER LIGNE (MAY BE CHANGED)
      INTEGER, PARAMETER :: SIZELIGN = 30000
!
!-----------------------------------------------------------------------
!
!     SPECIFICS FOR THE BINARY FILE
!
!     INTERPOLATION IN TIME AND IN SPACE FOR MULTIPLE VALUES
      INTEGER          :: NTIMEB,NVALUEB,NPOINB
      INTEGER          :: ITIMEB1,ITIMEB2
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::
     &                    TIMEB, XPOINB,YPOINB, VALUEB0
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::
     &                    VALUEB1,VALUEB2
!
!
!=======================================================================
!
!       4) WORTH SAVING
!
!-----------------------------------------------------------------------
      SAVE
!
!=======================================================================
!
!       5) METEOROLOGICAL SUBROUTINES
!
      CONTAINS
!
!=======================================================================
!
!                   **********************
                    SUBROUTINE POINT_METEO
!                   **********************
!
     &( FILES,ATMFILEA,ATMFILEB, MESH,IELMT )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Memory allocation of structures, aliases, blocks...
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATMFILEA   |-->| LOGICAL UNIT OF THE ASCII ATMOSPHERIC FILE
!| ATMFILEB   |-->| LOGICAL UNIT OF THE BINARY ATMOSPHERIC FILE
!| FILES      |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| IELMT      |-->| NUMBER OF ELEMENTS
!| MESH       |-->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      TYPE(BIEF_FILE), INTENT(IN) :: FILES(*)
      INTEGER,         INTENT(IN) :: ATMFILEA,ATMFILEB
      INTEGER,         INTENT(IN) :: IELMT
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
!
      INTEGER I,J
!
      CHARACTER(LEN=16), POINTER  :: CHOIX(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     GATHER THE SKELETON OF THE METEO THERMIC FILES
!
!     A: ASCII FILE
      METEO_DEJA(1) = .FALSE.
      NPOINA = 0
      IF( FILES(ATMFILEA)%NAME(1:1).NE.' ' ) THEN
        CALL INIT_FIC_ASCII( FILES,ATMFILEA )
        METEO_DEJA(1) = .TRUE.
      ENDIF
!
!     B: BINARY FILE
      METEO_DEJA(2) = .FALSE.
      NPOINB = 0
      IF( FILES(ATMFILEB)%NAME(1:1).NE.' ' ) THEN
        CALL INIT_FIC_BINARY( FILES,ATMFILEB )
        METEO_DEJA(2) = .TRUE.
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ALLOCATE MEMORY
!
!     WORKING ARRAY
!
      CALL BIEF_ALLVEC(1,WTMP1 ,'WTMP1 ',IELMT,1,1,MESH)
      CALL BIEF_ALLVEC(1,WTMP2 ,'WTMP2 ',IELMT,1,1,MESH)
!
!     METEOROLOGY
!
      CALL BIEF_ALLVEC(1,TAIR  ,'TAIR  ',IELMT,1,1,MESH)
      CALL OS('X=C     ', X=TAIR, C=CST_TAIR )
      INC_TAIR = .FALSE.
!
      CALL BIEF_ALLVEC(1,TDEW  ,'TDEW  ',IELMT,1,1,MESH)
      CALL OS('X=C     ', X=TDEW, C=CST_TDEW )
      INC_TDEW = .FALSE.
!
      CALL BIEF_ALLVEC(1,WINDX ,'WINDX ',IELMT,1,1,MESH)
      CALL BIEF_ALLVEC(1,WINDY ,'WINDY ',IELMT,1,1,MESH)
      CALL OS('X=C     ', X=WINDX, C=CST_WINDX )
      CALL OS('X=C     ', X=WINDY, C=CST_WINDY )
      INC_WINDX = .FALSE.
      INC_WINDY = .FALSE.
!     ONLY WINDS AND WINDD ARE CONVERTED INOT WINDX AND WINDY
      INC_WINDS = .FALSE.
      INC_WINDD = .FALSE.
!
      CALL BIEF_ALLVEC(1,CLDC  ,'CLDC  ',IELMT,1,1,MESH)
      CALL OS('X=C     ', X=CLDC, C=CST_CLDC )
      INC_CLDC = .FALSE.
!
      CALL BIEF_ALLVEC(1,VISBI ,'VISBI ',IELMT,1,1,MESH)
      CALL OS('X=C     ', X=VISBI, C=CST_VISBI )
      INC_VISBI = .FALSE.
!
      CALL BIEF_ALLVEC(1,PLUIE ,'PLUIE ',IELMT,1,1,MESH)
      CALL OS('X=C     ', X=PLUIE, C=CST_PLUIE )
      INC_PLUIE = .FALSE.
!
      CALL BIEF_ALLVEC(1,SNOW  ,'SNOW  ',IELMT,1,1,MESH)
      CALL OS('X=C     ', X=SNOW, C=CST_SNOW )
      INC_SNOW = .FALSE.
!
      CALL BIEF_ALLVEC(1,PATM  ,'PATM  ',IELMT,1,1,MESH)
      CALL OS('X=C     ', X=PATM, C=CST_PATM )
      INC_PATM = .FALSE.
!
!     1: ASCII FILE
!     2: BINARY FILE
!
      DO I = 1,2
        IF( METEO_DEJA(I) ) THEN
          CHOIX => METEO_CHOIX(I,1:METEO_MAXVALUE)
!
          J = FIND_NAME( 'TAIR', CHOIX, METEO_MAXVALUE )
          INC_TAIR = INC_TAIR .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'TDEW', CHOIX, METEO_MAXVALUE )
          INC_TDEW = INC_TDEW .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'WINDX', CHOIX, METEO_MAXVALUE )
          INC_WINDX = INC_WINDX .OR. ( J.NE.0 )
          J = FIND_NAME( 'WINDY', CHOIX, METEO_MAXVALUE )
          INC_WINDY = INC_WINDY .AND. ( J.NE.0 )
          J = FIND_NAME( 'WINDS', CHOIX, METEO_MAXVALUE )
          INC_WINDS = INC_WINDS .OR. ( J.NE.0 )
          J = FIND_NAME( 'WINDD', CHOIX, METEO_MAXVALUE )
          INC_WINDD = INC_WINDD .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'CLDC', CHOIX, METEO_MAXVALUE )
          INC_CLDC = INC_CLDC .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'VISBI', CHOIX, METEO_MAXVALUE )
          INC_VISBI = INC_VISBI .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'PLUIE', CHOIX, METEO_MAXVALUE )
          INC_PLUIE = INC_PLUIE .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'SNOW', CHOIX, METEO_MAXVALUE )
          INC_SNOW = INC_SNOW .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'PATM', CHOIX, METEO_MAXVALUE )
          INC_PATM = INC_PATM .OR. ( J.NE.0 )
!
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   **********************
                    SUBROUTINE DEALL_METEO
!                   **********************
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Memory de-allocation of structures, aliases, blocks...
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DE-ALLOCATE FILE RELATED VARIABLES (SEE INIT_FIC_*)
!
!     A: ASCII FILE
      IF( ALLOCATED(TIMEA) )   DEALLOCATE(TIMEA)
      IF( ALLOCATED(XPOINA) )  DEALLOCATE(XPOINA)
      IF( ALLOCATED(YPOINA) )  DEALLOCATE(YPOINA)
      IF( ALLOCATED(VALUEA1) ) DEALLOCATE(VALUEA1)
      IF( ALLOCATED(VALUEA2) ) DEALLOCATE(VALUEA2)
!
!     B: BINARY FILE
      IF( ALLOCATED(TIMEB) )   DEALLOCATE(TIMEB)
      IF( ALLOCATED(XPOINB) )  DEALLOCATE(XPOINB)
      IF( ALLOCATED(YPOINB) )  DEALLOCATE(YPOINB)
      IF( ALLOCATED(VALUEB0) ) DEALLOCATE(VALUEB0)
      IF( ALLOCATED(VALUEB1) ) DEALLOCATE(VALUEB1)
      IF( ALLOCATED(VALUEB2) ) DEALLOCATE(VALUEB2)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DE-ALLOCATE METEOROLOGICAL OBJECTS (SEE POINT_METEO)
!
      CALL BIEF_DEALLOBJ(WTMP1)
      CALL BIEF_DEALLOBJ(WTMP2)
!
      CALL BIEF_DEALLOBJ(TAIR)
      CALL BIEF_DEALLOBJ(TDEW)
      CALL BIEF_DEALLOBJ(WINDX)
      CALL BIEF_DEALLOBJ(WINDY)
      CALL BIEF_DEALLOBJ(CLDC)
      CALL BIEF_DEALLOBJ(VISBI)
      CALL BIEF_DEALLOBJ(PLUIE)
      CALL BIEF_DEALLOBJ(SNOW)
      CALL BIEF_DEALLOBJ(PATM)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   *********************
                    SUBROUTINE SYNC_METEO
!                   *********************
!
     &( WHEN )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Synchronise the ASCII and the BINARY file for spatial and
!         temporal interpolation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(IN) :: WHEN
!
      INTEGER          :: IPOIN
      DOUBLE PRECISION :: DTRS4
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SYNCHRONISE BOTH THE ASCII AND THE BINARY FILES
!
!     A: ASCII FILE
      IF( METEO_DEJA(1) ) CALL SYNC_FIC_ASCII( WHEN )
!
!     B: BINARY FILE
      IF( METEO_DEJA(2) ) CALL SYNC_FIC_BINARY( WHEN )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     UPDATE METEOROLOGICAL VARIABLES
!
!     TAIR: AIR TEMPERATURE
      CALL OS('X=C     ', X=TAIR, C=CST_TAIR )
      IF( INC_TAIR ) THEN
        CALL INTERP_METEO(WHEN,'TAIR',TAIR%R,TAIR%DIM1)
      ENDIF
!
!     TDEW: DEWPOINT TEMPERATURE
      CALL OS('X=C     ', X=TDEW, C=CST_TDEW )
      IF( INC_TDEW ) THEN
        CALL INTERP_METEO(WHEN,'TDEW',TDEW%R,TDEW%DIM1)
      ENDIF
!
!     CLDC: CLOUD COVER
      CALL OS('X=C     ', X=CLDC, C=CST_CLDC )
      IF( INC_CLDC ) THEN
        CALL INTERP_METEO(WHEN,'CLDC',CLDC%R,CLDC%DIM1)
      ENDIF
!
!     VISBI: VISIBILITY
      CALL OS('X=C     ', X=VISBI, C=CST_VISBI )
      IF( INC_VISBI ) THEN
        CALL INTERP_METEO(WHEN,'VISBI',VISBI%R,VISBI%DIM1)
      ENDIF
!
!     PLUIE: RAIN
      CALL OS('X=C     ', X=PLUIE, C=CST_PLUIE )
      IF( INC_PLUIE ) THEN
        CALL INTERP_METEO(WHEN,'PLUIE',PLUIE%R,PLUIE%DIM1)
      ENDIF
!
!     SNOW: SNOW
      CALL OS('X=C     ', X=SNOW, C=CST_SNOW )
      IF( INC_SNOW ) THEN
        CALL INTERP_METEO(WHEN,'SNOW',SNOW%R,SNOW%DIM1)
      ENDIF
!
!     WINDX,WINDY, OR WINDS,WINDD : WIND
      CALL OS('X=C     ', X=WINDX, C=CST_WINDX )
      IF( INC_WINDX ) THEN
        CALL INTERP_METEO(WHEN,'WINDX',WINDX%R,WINDX%DIM1)
      ENDIF
      CALL OS('X=C     ', X=WINDY, C=CST_WINDY )
      IF( INC_WINDY ) THEN
        CALL INTERP_METEO(WHEN,'WINDY',WINDY%R,WINDY%DIM1)
      ENDIF
      CALL OS('X=C     ', X=WTMP1, C=CST_WINDS )
      IF( INC_WINDS ) THEN
        CALL INTERP_METEO(WHEN,'WINDS',WTMP1%R,WTMP1%DIM1)
      ENDIF
      CALL OS('X=C     ', X=WTMP2, C=CST_WINDD )
      IF( INC_WINDD ) THEN
        CALL INTERP_METEO(WHEN,'WINDD',WTMP2%R,WTMP2%DIM1)
      ENDIF
      IF( ( INC_WINDS.OR.INC_WINDD ).AND.
     &    .NOT.( INC_WINDX.AND.INC_WINDY ) ) THEN
        DTRS4 = ATAN(1.D0) / 45.D0
        DO IPOIN = 1,WINDX%DIM1
          WINDX%R(IPOIN) = - WTMP1%R(IPOIN)*SIN( WTMP2%R(IPOIN)*DTRS4 )
          WINDY%R(IPOIN) = - WTMP1%R(IPOIN)*COS( WTMP2%R(IPOIN)*DTRS4 )
        ENDDO
      ENDIF
!
!     ATMOSPHERIC PRESSURE
      CALL OS('X=C     ', X=PATM, C=CST_PATM )
      IF( INC_PATM ) THEN
        CALL INTERP_METEO(WHEN,'PATM',PATM%R,PATM%DIM1)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                     ***********************
                      SUBROUTINE INTERP_METEO
!                     ***********************
!
     &( WHEN,WHAT,VALEURS,NPOIN )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Spatial and temporal interpolation of variables from either
!+        the ASCII or the BINARY file
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF NODES
!| VALEURS        |<->| VALUES CONTAINED IN THE VARIABLE
!| WHAT           |-->| VARIABLE TO CONSIDER
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER,             INTENT(IN) :: NPOIN
      CHARACTER(LEN=*),    INTENT(IN) :: WHAT
      DOUBLE PRECISION,    INTENT(IN) :: WHEN
      DOUBLE PRECISION, INTENT(INOUT) :: VALEURS(NPOIN)
!
      INTEGER           J,IPOIN
      DOUBLE PRECISION  A,B,C,D,ALPHA,DELTA
      DOUBLE PRECISION  X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,NX,NY,NZ,MX,MY
      CHARACTER(LEN=16), POINTER  :: CHOIX(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     A: ASCII FILE
      IF( METEO_DEJA(1) ) THEN
        CHOIX => METEO_CHOIX(1,1:METEO_MAXVALUE)
        J = FIND_NAME( WHAT, CHOIX, METEO_MAXVALUE )
        IF( J.NE.0 ) THEN
!         ______________________________________________________________
!         TEMPORAL INTERPOLATION
          DELTA = ( TIMEA(ITIMEA2)-TIMEA(ITIMEA1) )
          ALPHA = ( WHEN-TIMEA(ITIMEA1) )/DELTA
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - NO INTERPOLATION
          IF( NPOINA.EQ.1 ) THEN
            DO IPOIN = 1,NPOIN
              VALEURS(IPOIN) =
     &          ( 1.D0-ALPHA )*VALUEA1(J,1) + ALPHA*VALUEA2(J,1)
            ENDDO
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - TWO POINTS
          ELSEIF( NPOINA.EQ.2 ) THEN
!           THE PROJECTION OF THE POINT TO THE LINE WILL GIVE YOU THE Z
            X1 = XPOINA(1)
            Y1 = YPOINA(1)
            X2 = XPOINA(2)
            Y2 = YPOINA(2)
            A = ( Y2-Y1 )
            B = ( X1-X2 )
            C = Y1*( X2-X1 ) - X1*( Y2-Y1 )
            D = A*A + B*B
!           BEFORE
            Z1 = VALUEA1(J,1)
            Z2 = VALUEA1(J,2)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) =
     &            ( 1.D0-ALPHA )*( Z1+Z3*(XPOINA(IPOIN)-X1) )
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) =
     &            ( 1.D0-ALPHA )*( Z1+Z3*(YPOINA(IPOIN)-Y1) )
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = ( 1.D0-ALPHA )*( Z1 )
              ENDDO
            ENDIF
!           AFTER
            Z1 = VALUEA2(J,1)
            Z2 = VALUEA2(J,2)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = VALEURS(IPOIN)
     &                         + ALPHA*( Z1+Z3*(XPOINA(IPOIN)-X1) )
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = VALEURS(IPOIN)
     &                         + ALPHA*( Z1+Z3*(YPOINA(IPOIN)-Y1) )
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = VALEURS(IPOIN) + ALPHA*( Z1 )
              ENDDO
            ENDIF
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - THREE POINTS
          ELSEIF( NPOINA.EQ.3 ) THEN
!           THE NORMAL VECTOR AND THE FIRST POINT DEFINE THE PLAN
            X1 = XPOINA(1)
            Y1 = YPOINA(1)
            X2 = XPOINA(2)
            Y2 = YPOINA(2)
            X3 = XPOINA(3)
            Y3 = YPOINA(3)
!           BEFORE
            Z1 = VALUEA1(J,1)
            Z2 = VALUEA1(J,2)
            Z3 = VALUEA1(J,3)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VALEURS(IPOIN) = ( 1.D0-ALPHA )*( Z1-(MX+MY)/NZ )
            ENDDO
!           AFTER
            Z1 = VALUEA2(J,1)
            Z2 = VALUEA2(J,2)
            Z3 = VALUEA2(J,3)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VALEURS(IPOIN) = VALEURS(IPOIN) + ALPHA*( Z1-(MX+MY)/NZ )
            ENDDO
          ENDIF
!       ________________________________________________________________
        ENDIF
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     B: BINARY FILE
      IF( METEO_DEJA(2) ) THEN
        CHOIX => METEO_CHOIX(2,1:METEO_MAXVALUE)
        J = FIND_NAME( WHAT, CHOIX, METEO_MAXVALUE )
        IF( J.NE.0 ) THEN
!         ______________________________________________________________
!         TEMPORAL INTERPOLATION
          DELTA = ( TIMEB(ITIMEB2)-TIMEB(ITIMEB1) )
          ALPHA = ( WHEN-TIMEB(ITIMEB1) )/DELTA
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - NO INTERPOLATION
          DO IPOIN = 1,NPOIN
            VALEURS(IPOIN) =
     &         ( 1.D0-ALPHA )*VALUEB1(J,IPOIN) + ALPHA*VALUEB2(J,IPOIN)
          ENDDO
!
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   **************************
                    INTEGER FUNCTION FIND_NAME
!                   **************************
!
     &( NAME,CHOIX,MAXVALUE )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Search for NAME in a list of CHOIX (variables found in the
!+        METEO files). Return 0 if not found, the index in CHOIX
!+        otherwise.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHOIX          |-->| LIST OF VARIABLES PRESENT IN THE METEO FILE
!| MAXVALUE       |-->| MAXIMUM SIZE OF THE LIST CHOIX
!| NAME           |-->| MNEMO OF THE VARIABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*), INTENT(IN) :: NAME
      INTEGER,          INTENT(IN) :: MAXVALUE
      CHARACTER(LEN=*), INTENT(IN) :: CHOIX(MAXVALUE)
!
      INTEGER :: I
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DEFAULT IF NOT FOUND
      FIND_NAME = 0
!     LOOP THROUGH
      DO I = 1,MAXVALUE
        IF( INCLUS( CHOIX(I), NAME ) ) FIND_NAME = I
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION
!
!=======================================================================
!
!                   **************************
                    SUBROUTINE INIT_FIC_BINARY
!                   **************************
!
     &( FILES,ATMFILEB )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Scan the ASCII file and prepare skeleton for future calls
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATMFILEA       |-->| LOGICAL UNIT OF ASCII FILE FOR METEO
!| FILES          |-->| ARRAYS OF ALL FILES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      IMPLICIT NONE
!
      TYPE(BIEF_FILE),     INTENT(IN) :: FILES(*)
      INTEGER,             INTENT(IN) :: ATMFILEB
!
      INTEGER :: I,J, NFIC, IERR
      CHARACTER(LEN=8)  :: NFMT
!
      CHARACTER(LEN=16), POINTER  :: CHOIX(:)
      CHARACTER(LEN=16), POINTER  :: UNITS(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SETTING LOCAL ASCII FILE
      METEO_FILES(2)%LU      = FILES(ATMFILEB)%LU
      METEO_FILES(2)%NAME    = FILES(ATMFILEB)%NAME
      METEO_FILES(2)%TELNAME = FILES(ATMFILEB)%TELNAME
      METEO_FILES(2)%FMT     = FILES(ATMFILEB)%FMT
      METEO_FILES(2)%ACTION  = FILES(ATMFILEB)%ACTION
      METEO_FILES(2)%BINASC  = FILES(ATMFILEB)%BINASC
      METEO_FILES(2)%TYPE    = FILES(ATMFILEB)%TYPE
!
!     SIMPLIFYING NOTATIONS
      NFMT     = METEO_FILES(2)%FMT
      NFIC     = METEO_FILES(2)%LU
      UNITS =>   METEO_UNITS(2,1:METEO_MAXVALUE)
      CHOIX =>   METEO_CHOIX(2,1:METEO_MAXVALUE)
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SCAN THE SKELETON
!
!     __________________________________________________________________
!     GET THE NUMBER OF POINTS IN THE METEO FILE
      CALL GET_MESH_NPOIN(NFMT,NFIC,TRIANGLE_ELT_TYPE,NPOINB,IERR)
      CALL CHECK_CALL
     &  (IERR,"METEO,INIT_FIC_BINARY: GET_MESH_NPOIN")
!
!     __________________________________________________________________
!     FIND OUT WHAT VARIABLES ARE GIVEN IN THE FILE
!
      CALL GET_DATA_NVAR(NFMT,NFIC,NVALUEB,IERR)
      CALL CHECK_CALL
     &  (IERR, 'METEO,INIT_FIC_BINARY:GET_DATA_NVAR')
!
      CALL GET_DATA_VAR_LIST(NFMT,NFIC,NVALUEB,CHOIX,UNITS,IERR)
      CALL CHECK_CALL
     &  (IERR, 'METEO,INIT_FIC_BINARY:GET_DATA_VAR_LIST')
!
!     __________________________________________________________________
!     GET THE TIME PROFILE FOR FUTURE INTERPOLATION
      CALL GET_DATA_NTIMESTEP(NFMT,NFIC,NTIMEB,IERR)
      CALL CHECK_CALL
     &  (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_NTIMESTEP')
!
      ALLOCATE(TIMEB(NTIMEB))
      DO I = 1,NTIMEB
        CALL GET_DATA_TIME(NFMT,NFIC,I,TIMEB(I),IERR)
        CALL CHECK_CALL
     &    (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_TIME:I')
      ENDDO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     PREPARE FOR NEXT ACCESS TO THE FILE
!
      ALLOCATE(VALUEB1(NVALUEB,NPOINB),VALUEB2(NVALUEB,NPOINB))
      ALLOCATE(VALUEB0(NPOINB))
!     __________________________________________________________________
!     READ ONE TIME FRAME AT LEAST
      ITIMEB1 = 1
      ITIMEB2 = 1
      DO I = 1,NVALUEB
        CALL GET_DATA_VALUE
     &    (NFMT,NFIC,ITIMEB1,CHOIX(I),VALUEB0,NPOINB,IERR)
        CALL CHECK_CALL
     &    (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_VALUE:CHOIX')
        DO J = 1,NPOINB
          VALUEB1(I,J) = VALUEB0(J)
          VALUEB2(I,J) = VALUEB0(J)
        ENDDO
      ENDDO
!     __________________________________________________________________
!     READ A SECOND TIME FRAME IF THERE
      IF( NTIMEB.GT.1 )THEN
        ITIMEB2 = 1
        DO I = 1,NVALUEB
          CALL GET_DATA_VALUE
     &      (NFMT,NFIC,ITIMEB2,CHOIX(I),VALUEB0,NPOINB,IERR)
          CALL CHECK_CALL
     &      (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_VALUE:CHOIX')
          DO J = 1,NPOINB
            VALUEB2(I,J) = VALUEB0(J)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   *************************
                    SUBROUTINE INIT_FIC_ASCII
!                   *************************
!
     &( FILES,ATMFILEA )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Scan the ASCII file and prepare skeleton for future calls
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATMFILEA       |-->| LOGICAL UNIT OF ASCII FILE FOR METEO
!| FILES          |-->| ARRAYS OF ALL FILES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      TYPE(BIEF_FILE),     INTENT(IN) :: FILES(*)
      INTEGER,             INTENT(IN) :: ATMFILEA
!
      INTEGER :: I,J,K, NFIC, IDEB,IFIN, NPOINX,NPOINY
      CHARACTER(LEN=SIZELIGN) :: LIGNE
      DOUBLE PRECISION :: X1,X2,X3, Y1,Y2,Y3
!
      CHARACTER(LEN=16), POINTER  :: CHOIX(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SETTING LOCAL ASCII FILE
      METEO_DEJA(1) = .FALSE.
      METEO_FILES(1)%LU      = FILES(ATMFILEA)%LU
      METEO_FILES(1)%NAME    = FILES(ATMFILEA)%NAME
      METEO_FILES(1)%TELNAME = FILES(ATMFILEA)%TELNAME
      METEO_FILES(1)%FMT     = FILES(ATMFILEA)%FMT
      METEO_FILES(1)%ACTION  = FILES(ATMFILEA)%ACTION
      METEO_FILES(1)%BINASC  = FILES(ATMFILEA)%BINASC
      METEO_FILES(1)%TYPE    = FILES(ATMFILEA)%TYPE
!
!     SIMPLIFYING NOTATIONS
      NFIC     = METEO_FILES(1)%LU
      CHOIX => METEO_CHOIX(1,1:METEO_MAXVALUE)
!
!     DEFAULT NUMBER OF LOCATIONS
      NPOINX = 1
      NPOINY = 1
      X1 = 0.D0
      Y1 = 0.D0
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     READ THE FILE ONE TIME IN FULL TO SIZE UP ITS CONTENT
!     ( TODO: REPLACE GOTO STATEMENTS BY WHILE STATEMENT )
      REWIND(NFIC)
!     __________________________________________________________________
!     SKIP COMMENTS
!
 101  READ(NFIC,FMT='(A)',ERR=103) LIGNE
      GOTO 102
!
 103  CONTINUE
      WRITE(LU,*) 'ERROR WHLE READING THE ASCII FILE'
      WRITE(LU,*) 'USED FOR THE METEO (THERMAL PROCESSES)'
      WRITE(LU,*) 'PROBABLY A PROBLEM OF FORMAT'
      WRITE(LU,*) 'ANY WINDOWS CARRIAGE RETURNS ON UNIX OR LINUX'
      WRITE(LU,*) 'GUILTY LINE:'
      WRITE(LU,*) LIGNE
      CALL PLANTE(1)
      STOP
!
 102  CONTINUE
      IF( LIGNE(1:1).EQ.':' ) THEN
!       DEFINING X-LOCATIONS IF MORE THAN ONE (UP TO THREE)
        IF( INCLUS(LIGNE,':X') )THEN
          IDEB = 2
 151      IF( IDEB.GE.SIZELIGN ) GOTO 152
          IDEB = IDEB + 1
          IF( (LIGNE(IDEB:IDEB).NE.' ') .AND.
     &         LIGNE(IDEB:IDEB).NE.CHAR(9) ) GOTO 151
!         THE REST OF THE LINE SHOULD HAVE AT MOST THREE VALUES
          READ(LIGNE(IDEB:SIZELIGN),*,ERR=152) X1,X2
          NPOINX = NPOINX + 1
          READ(LIGNE(IDEB:SIZELIGN),*,ERR=152) X1,X2,X3
          NPOINX = NPOINX + 1
 152      CONTINUE
!       DEFINING Y-LOCATIONS IF MORE THAN ONE (UP TO THREE)
        ELSEIF( INCLUS(LIGNE,':Y') )THEN
          IDEB = 2
 153      IF( IDEB.GE.SIZELIGN ) GOTO 154
          IDEB = IDEB + 1
          IF( (LIGNE(IDEB:IDEB).NE.' ') .AND.
     &         LIGNE(IDEB:IDEB).NE.CHAR(9) ) GOTO 153
!         THE REST OF THE LINE SHOULD HAVE AT MOST THREE VALUES
          READ(LIGNE(IDEB:SIZELIGN),*,ERR=154) Y1,Y2
          NPOINY = NPOINY + 1
          READ(LIGNE(IDEB:SIZELIGN),*,ERR=154) Y1,Y2,Y3
          NPOINY = NPOINY + 1
 154      CONTINUE
        ENDIF
      ENDIF
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 101
!     __________________________________________________________________
!     FINALISING LOCATIONS
!
      IF( NPOINX.NE.NPOINY )THEN
        WRITE(LU,*) 'NUMBER OF LOCATIONS X AND Y LOCATIONS READ'
        WRITE(LU,*) 'IN THE ASCII FILE USED FOR THE METEO (THERMAL'
        WRITE(LU,*) 'PROCESSES) DIFFERENT.'
      ENDIF
      NPOINA = NPOINX
      ALLOCATE( XPOINA(NPOINA),YPOINA(NPOINA) )
      IF( NPOINA.GE.1 ) THEN
        XPOINA(1) = X1
        YPOINA(1) = Y1
      ENDIF
      IF( NPOINA.GE.2 ) THEN
        XPOINA(2) = X2
        YPOINA(2) = Y2
      ENDIF
      IF( NPOINA.EQ.3 ) THEN
        XPOINA(3) = X3
        YPOINA(3) = Y3
      ENDIF
!     __________________________________________________________________
!     FIND OUT WHAT AND HOW MANY VALUES ARE GIVEN IN THE FILE
!
      NVALUEA = -1
      IFIN = 1
 104  IDEB = IFIN
!
!     IDENTIFY FIRST CHARACTER OF NAME
 105  IF((LIGNE(IDEB:IDEB).EQ.' '.OR.LIGNE(IDEB:IDEB).EQ.CHAR(9))
     &   .AND.IDEB.LT.SIZELIGN) THEN
        IDEB = IDEB + 1
        GOTO 105
      ENDIF
!     IDENTIFY LAST CHARACTER OF NAME ( TODO: USE A WHILE STATEMENT )
      IFIN = IDEB
 106  IF( LIGNE(IFIN:IFIN).NE.' '.AND.LIGNE(IFIN:IFIN).NE.CHAR(9)
     &   .AND.IFIN.LT.SIZELIGN) THEN
        IFIN = IFIN + 1
        GOTO 106
      ENDIF
!
      IF( IDEB.EQ.IFIN ) GOTO 140  ! IDEB .EQ. IFIN .EQ. SIZELIGN
!
      NVALUEA = NVALUEA + 1
      IF( NVALUEA.EQ.0 ) THEN
!       SPECIAL CASE FOR TIME
        IF(LIGNE(IDEB:IFIN-1).NE.'T') THEN
          WRITE(LU,*) 'THE FIRST VARIABLE MUST BE TIME T IN THE'
          WRITE(LU,*) 'ASCII FILE USED FOR THE METEO (THERMAL'
          WRITE(LU,*) 'PROCESSES). OTHER POSSIBLE CAUSE:'
          WRITE(LU,*) 'THERE ARE TABS IN THE FILE'
          WRITE(LU,*) 'CHANGE TABS INTO SPACES'
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSEIF( NVALUEA.LE.METEO_MAXVALUE ) THEN
        CHOIX(NVALUEA) = '                '
        CHOIX(NVALUEA)(1:IFIN-IDEB+1) = LIGNE(IDEB:IFIN-1)
      ELSE
        WRITE(LU,*) 'INCREASE MAXVALUE FOR READ_FIC_ASCII'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(IFIN.LT.SIZELIGN) GO TO 104
!
      IF( INT(NVALUEA/NPOINA)*NPOINA .EQ. NVALUEA )THEN
        NVALUEA = INT(NVALUEA/NPOINA)
      ELSE
        WRITE(LU,*) 'NUMBER OF LOCATIONS AND TOTAL NUMBER OF VALUES'
        WRITE(LU,*) 'FOUND IN THE ASCCI FILE FOR METEO (THERMAL'
        WRITE(LU,*) 'PROCESSES) ARE INCOMPATIBLE.'
        WRITE(LU,*) 'ONE VALUE PER VARIABLE SHOULD BE THERE FOR EACH'
        WRITE(LU,*) 'LOCATION.'
        CALL PLANTE(1)
        STOP
      ENDIF
!     __________________________________________________________________
!     SKIP THE LINE WITH UNITS AS WELL AS COMMENTS
 140  READ(NFIC,FMT='(A)',ERR=103) LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 140
!     __________________________________________________________________
!     COUNT LINES OF DATA
      NTIMEA = 0
 201  READ(NFIC,*,END=202,ERR=203) LIGNE
      IF( LIGNE(1:1).NE.'#' .AND.
     &    LIGNE(1:1).NE.'!' .AND.
     &    LIGNE(1:1).NE.':' ) NTIMEA = NTIMEA + 1
      GOTO 201
!
 203  CONTINUE
      WRITE(LU,*) 'ERROR READING THE ASCII FILE USED FOR THE METEO'
      WRITE(LU,*) '(THERMAL PROCESSES) AT LINE: ',NTIMEA
      WRITE(LU,*) '(COMMENTS EXCLUDED)'
      CALL PLANTE(1)
      STOP
!
 202  CONTINUE
      IF( NTIMEA.LE.1 ) THEN
        WRITE(LU,*) 'TWO TIME STEP AT LEAST SHOULD BE PRESENT IN'
        WRITE(LU,*) 'THE ASCII FILE USED FOR THE METEO (THERMAL'
        WRITE(LU,*) 'PROCESSES)'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     READ THE FILE A SECOND TIME IN FULL TO KEEP ITS SCKELETON
!     ( TODO: REPLACE GOTO STATEMENTS BY WHILE STATEMENT )
      REWIND(NFIC)
!     __________________________________________________________________
!     ALLOCATE TIME SCKELETON IN MEMORY
      ALLOCATE(TIMEA(NTIMEA))
      ALLOCATE(VALUEA1(NVALUEA,NPOINA),VALUEA2(NVALUEA,NPOINA))
!     __________________________________________________________________
!     SKIP COMMENTS AND FIRST TWO MANDATORY LINES
 111  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 111
 112  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 112
!     __________________________________________________________________
!     SAVE ALL TIMES FOR TEMPORAL INTERPOLATION
      DO I = 1,NTIMEA
 113    READ(NFIC,FMT='(A)') LIGNE
        IF( LIGNE(1:1).EQ.'#' .OR.
     &      LIGNE(1:1).EQ.'!' .OR.
     &      LIGNE(1:1).EQ.':' ) GOTO 113
        READ(LIGNE,*) TIMEA(I),((VALUEA1(J,K),J=1,NVALUEA),K=1,NPOINA)
      ENDDO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     REWIND AND PREPARE FOR NEXT ACCESS TO THE FILE
      REWIND(NFIC)
!     __________________________________________________________________
!     SKIP COMMENTS AND FIRST TWO MANDATORY LINES
 121  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 121
 122  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 122
!     __________________________________________________________________
!     INITIAL TIMES AND VALUES TO THE FIRST TWO LINES
      ITIMEA1 = 1
 123  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 123
      READ(LIGNE,*) TIMEA(ITIMEA1),
     &  ((VALUEA1(J,K),J=1,NVALUEA),K=1,NPOINA)
      ITIMEA2 = 2
 124  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 124
      READ(LIGNE,*) TIMEA(ITIMEA2),
     &  ((VALUEA2(J,K),J=1,NVALUEA),K=1,NPOINA)
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   **************************
                    SUBROUTINE SYNC_FIC_BINARY
!                   **************************
!
     &( WHEN )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Synchronise the BINARY file for spatial and temporal
!         interpolation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(IN)    :: WHEN
!
      INTEGER :: I,J, IERR, NFIC
      CHARACTER(LEN=8)  :: NFMT
!
      CHARACTER(LEN=16), POINTER  :: CHOIX(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SIMPLIFYING NOTATIONS
      NFMT     = METEO_FILES(2)%FMT
      NFIC     = METEO_FILES(2)%LU
      CHOIX =>   METEO_CHOIX(2,1:METEO_MAXVALUE)
!
!
!     INTERPOLATE IN TIME FOR ONE PARTICULAR VARIABLE
!     __________________________________________________________________
!     TOO EARLY
      IF( WHEN.LT.TIMEB(1) ) THEN
        WRITE(LU,*) 'THE TIME REQUESTED IS TOO EARLY COMPARED TO'
        WRITE(LU,*) 'THE PROFIL OF BINARY FILE USED FOR THE METEO'
        WRITE(LU,*) '(THERMAL PROCESSES)'
        CALL PLANTE(1)
        STOP
!     __________________________________________________________________
!     TOO LATE
      ELSEIF( WHEN.GT.TIMEB(NTIMEB) ) THEN
        WRITE(LU,*) 'THE TIME REQUESTED IS TOO LATE COMPARED TO'
        WRITE(LU,*) 'THE PROFIL OF BINARY FILE USED FOR THE METEO'
        WRITE(LU,*) '(THERMAL PROCESSES)'
        CALL PLANTE(1)
        STOP
      ENDIF
!     __________________________________________________________________
!     FIND WHEN
      IF( WHEN.GT.TIMEB(ITIMEB2) )THEN
 132    ITIMEB1 = ITIMEB2
        DO I = 1,NVALUEB
          DO J = 1,NPOINB
            VALUEB1(I,J) = VALUEB2(I,J)
          ENDDO
        ENDDO
        ITIMEB2 = ITIMEB2 + 1
        DO I = 1,NVALUEB
          CALL GET_DATA_VALUE
     &      (NFMT,NFIC,ITIMEB2,CHOIX(I),VALUEB0,NPOINB,IERR)
          CALL CHECK_CALL
     &      (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_VALUE:CHOIX')
          DO J = 1,NPOINB
            VALUEB2(I,J) = VALUEB0(J)
          ENDDO
        ENDDO
        IF( WHEN.GT.TIMEB(ITIMEB2) ) GOTO 132
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   *************************
                    SUBROUTINE SYNC_FIC_ASCII
!                   *************************
!
     &( WHEN )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    Synchronise the ASCII file for spatial and temporal
!         interpolation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(IN)    :: WHEN
!
      INTEGER :: J,K, NFIC
      CHARACTER(LEN=SIZELIGN) :: LIGNE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SIMPLIFYING NOTATIONS
      NFIC     = METEO_FILES(1)%LU
!
!
!     INTERPOLATE IN TIME FOR ONE PARTICULAR VARIABLE
!     __________________________________________________________________
!     TOO EARLY
      IF( WHEN.LT.TIMEA(1) ) THEN
        WRITE(LU,*) 'THE TIME REQUESTED IS TOO EARLY COMPARED TO'
        WRITE(LU,*) 'THE PROFIL OF ASCII FILE USED FOR THE METEO'
        WRITE(LU,*) '(THERMAL PROCESSES)'
        CALL PLANTE(1)
        STOP
!     __________________________________________________________________
!     TOO LATE
      ELSEIF( WHEN.GT.TIMEA(NTIMEA) ) THEN
        WRITE(LU,*) 'THE TIME REQUESTED IS TOO LATE COMPARED TO'
        WRITE(LU,*) 'THE PROFIL OF ASCII FILE USED FOR THE METEO'
        WRITE(LU,*) '(THERMAL PROCESSES)'
        CALL PLANTE(1)
        STOP
      ENDIF
!     __________________________________________________________________
!     FIND WHEN
      IF( WHEN.GT.TIMEA(ITIMEA2) )THEN
!
 132    ITIMEA1 = ITIMEA2
        DO J = 1,NVALUEA
          DO K = 1,NPOINA
            VALUEA1(J,K) = VALUEA2(J,K)
          ENDDO
        ENDDO
        ITIMEA2 = ITIMEA2 + 1
 133    READ(NFIC,FMT='(A)') LIGNE
        IF( LIGNE(1:1).EQ.'#' .OR.
     &      LIGNE(1:1).EQ.'!' .OR.
     &      LIGNE(1:1).EQ.':' ) GOTO 133
        READ(LIGNE,*) TIMEA(ITIMEA2),
     &      ((VALUEA2(J,K),J=1,NVALUEA),K=1,NPOINA)
        IF( WHEN.GT.TIMEA(ITIMEA2) ) GOTO 132
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE METEO_KHIONE
