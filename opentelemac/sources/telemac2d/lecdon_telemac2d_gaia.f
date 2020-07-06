!                   *********************************
                    SUBROUTINE LECDON_TELEMAC2D_GAIA
!                   *********************************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR,CAS_FILE,DICO_FILE)
!
!***********************************************************************
! TELEMAC-2D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |<--| STORES STRINGS 'SUBMIT' OF DICTIONARY
!| MOTCAR         |<--| VALUES OF KEY-WORDS OF TYPE CHARACTER
!| NCAR           |-->| NUMBER OF LETTERS IN STRING PATH
!| PATH           |-->| FULL PATH TO CODE DICTIONARY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY: NAMETRAC, NTRAC, IND_SED
      USE DECLARATIONS_GAIA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
!     API
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: I,K,ICO,INCO
      INTEGER            :: MOTINT(MAXKEYWORD)
      INTEGER            :: TROUVE(4,MAXKEYWORD)
      INTEGER            :: ADRESS(4,MAXKEYWORD)
      INTEGER            :: DIMENS(4,MAXKEYWORD)
      DOUBLE PRECISION   :: MOTREA(MAXKEYWORD)
      LOGICAL            :: DOC
      LOGICAL            :: MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=250) :: NOM_CAS
      CHARACTER(LEN=250) :: NOM_DIC
      CHARACTER(LEN=72)  :: MOTCLE(4,MAXKEYWORD,2)
      CHARACTER(LEN=3)   :: SEDTYPE

      INTEGER :: ID_DICO, ID_CAS
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=2) CHAR2
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL :
!
      DO K = 1, MAXKEYWORD
!       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
!
        DIMENS(1,K) = 0
        DIMENS(2,K) = 0
        DIMENS(3,K) = 0
        DIMENS(4,K) = 0
      ENDDO
!
!     WRITES OUT INFO
      DOC = .FALSE.
!
!-----------------------------------------------------------------------
!     OPENS DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'GAIDICO'
        NOM_CAS=PATH(1:NCAR)//'GAICAS'
!
      ELSE
!
        NOM_DIC='GAIDICO'
        NOM_CAS='GAICAS'
!
      ENDIF
      IF((CAS_FILE(1:1).NE.' ').AND.(DICO_FILE(1:1).NE.' ')) THEN
        WRITE(LU,*) 'FIXED DICO AND STEERING FILE PRESENT'
        NOM_DIC=DICO_FILE
        NOM_CAS=CAS_FILE
        WRITE(LU,*) 'NOM_DIC',NOM_DIC
        WRITE(LU,*) 'NOM_CAS',NOM_CAS
      ENDIF
!
      CALL GET_FREE_ID(ID_DICO)
      OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      CALL GET_FREE_ID(ID_CAS)
      OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
!-----------------------------------------------------------------------
!     CALLS DAMOCLES
!-----------------------------------------------------------------------
!
      CALL DAMOCLE( ADRESS, DIMENS  ,MAXKEYWORD, DOC    , LNG , LU  ,
     &              MOTINT, MOTREA ,MOTLOG , MOTCAR ,
     &              MOTCLE, TROUVE ,ID_DICO, ID_CAS,.FALSE. ,FILE_DESC)
!
!-----------------------------------------------------------------------
!     CLOSES DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      CLOSE(ID_DICO)
      CLOSE(ID_CAS)
!
!-----------------------------------------------------------------------
!
      SUSP         = MOTLOG( ADRESS(3,  7) )
      NSICLA       = DIMENS(4,59)
!     TYPE OF SEDIMENT AND NSICLA
      ICO = 1
      INCO = 1
      IF(NSICLA.GT.0) THEN
        IND_SED =  NTRAC+1
        DO I=1,NSICLA
          SEDTYPE = MOTCAR(ADRESS(4,59)+I-1)(1:3)
          IF(SEDTYPE.EQ.'CO') THEN
            WRITE(CHAR2,'(I2)') ICO
            ICO = ICO + 1
            CALL ADDTRACER(NAMETRAC,NTRAC,K,.TRUE.,
     &                       'SEDIMENT COH' //ADJUSTL(CHAR2)//'  ',
     &                       'COH SEDIMENT' //ADJUSTL(CHAR2)//'  ',
     &                       'g/l             ')
          ELSE IF(SEDTYPE.EQ.'NCO') THEN
            IF(SUSP) THEN
              WRITE(CHAR2,'(I2)') INCO
              INCO = INCO + 1
              CALL ADDTRACER(NAMETRAC,NTRAC,K,.TRUE.,
     &                       'SEDIMENT NCOH' //ADJUSTL(CHAR2)//' ',
     &                       'NCOH SEDIMENT' //ADJUSTL(CHAR2)//' ',
     &                       'g/l             ')
            ENDIF
          ELSE IF(SEDTYPE.NE.'CO'.AND.SEDTYPE.NE.'NCO') THEN
            WRITE(LU,*)'LECDON_T2D_GAIA: CHECK TYPE OF SEDIMENT'
            WRITE(LU,*)'POSSIBLE CHOICES ARE: CO AND NCO'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
!
!----------------------------------------------------------------
!
      RETURN
      END
