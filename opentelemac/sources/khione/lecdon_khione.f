!                    ************************
                     SUBROUTINE LECDON_KHIONE
!                    ************************
!
     & (FILE_DESC,PATH,NCAR)
!
!***********************************************************************
! KHIONE   V7P2                                              02/11/2016
!***********************************************************************
!
!brief    READS THE STEERING FILE THROUGH A DAMOCLES CALL.
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        11/11/2016
!+        V7P3
!+        Coupling TELEMAC-2D with KHIONE (ice modelling component)
!+        Initial developments
!
!history  F. SOUILLE (EDF)
!+        30/09/2019
!+        V8P0
!+        Updated variables and sections
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |-->| STORES THE FILES 'SUBMIT' ATTRIBUTES
!|                |   | IN DICTIONARIES. IT IS FILLED BY DAMOCLES.
!| NCAR           |-->| LENGTH OF PATH
!| PATH           |-->| NAME OF CURRENT DIRECTORY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE
      USE METEO_KHIONE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8) ::   MNEMO(MAXVAR)
      INTEGER          ::   K,I
      DOUBLE PRECISION ::   DTRS4,ANG
      CHARACTER(LEN=2) CHAR2
!
      CHARACTER(LEN=PATH_LEN) :: NOM_CAS
      CHARACTER(LEN=PATH_LEN) :: NOM_DIC
!
!-----------------------------------------------------------------------
!
! ARRAYS USED IN THE DAMOCLES CALL
!
      INTEGER            ADRESS(4,MAXKEYWORD),DIMEN(4,MAXKEYWORD)
      DOUBLE PRECISION   MOTREA(MAXKEYWORD)
      INTEGER            MOTINT(MAXKEYWORD)
      LOGICAL            MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=72)  MOTCLE(4,MAXKEYWORD,2)
      INTEGER            TROUVE(4,MAXKEYWORD)
      LOGICAL            DOC
      INTEGER :: ID_DICO, ID_CAS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IF (LNG.EQ.1) WRITE(LU,1)
      IF (LNG.EQ.2) WRITE(LU,2)
1     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*     SOUS-PROGRAMME LECDON_KHIONE         *',/,
     &            19X, '*           APPEL DE DAMOCLES              *',/,
     &            19X, '*     VERIFICATION DES DONNEES LUES        *',/,
     &            19X, '*           SUR LE FICHIER CAS             *',/,
     &            19X, '********************************************',/)
2     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*        SUBROUTINE LECDON_KHIONE          *',/,
     &            19X, '*           CALL OF DAMOCLES               *',/,
     &            19X, '*        VERIFICATION OF READ DATA         *',/,
     &            19X, '*            ON STEERING FILE              *',/,
     &            19X, '********************************************',/)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DAMOCLE CALL - PARSING DICO AND CAS FILES
!
!     __________________________________________________________________
!     INITIALISES THE VARIABLES FOR DAMOCLES CALL
      DO K=1,MAXKEYWORD
!       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
!
        DIMEN(1,K) = 0
        DIMEN(2,K) = 0
        DIMEN(3,K) = 0
        DIMEN(4,K) = 0
      ENDDO
!
!     __________________________________________________________________
!     WRITES OUT INFO
      DOC = .FALSE.
!
!     __________________________________________________________________
!     OPENS DICTIONNARY AND STEERING FILES
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'ICEDICO'
        NOM_CAS=PATH(1:NCAR)//'ICECAS'
!
      ELSE
!
        NOM_DIC='ICEDICO'
        NOM_CAS='ICECAS'
!
      ENDIF
!
      CALL GET_FREE_ID(ID_DICO)
      OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      CALL GET_FREE_ID(ID_CAS)
      OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
!     __________________________________________________________________
!     CALL DAMOCLE
      CALL DAMOCLE
     &( ADRESS, DIMEN , MAXKEYWORD  , DOC    , LNG   , LU    , MOTINT,
     &  MOTREA, MOTLOG, MOTCAR, MOTCLE , TROUVE, ID_DICO, ID_CAS,
     &  .FALSE.,FILE_DESC)
!
!     __________________________________________________________________
!     CLOSES DICTIONNARY AND STEERING FILES
      CLOSE(ID_DICO)
      CLOSE(ID_CAS)
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     RETRIEVES FILE NUMBERS FROM KHIONE FORTRAN PARAMETERS
!
!     DECODES 'SUBMIT' CHAINS
      CALL READ_SUBMIT(ICE_FILES,MAXLU_ICE,FILE_DESC,MAXKEYWORD)
!
!     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
      DO I=1,MAXLU_ICE
        IF    (ICE_FILES(I)%TELNAME.EQ.'ICECLI') THEN
          ICECLI=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICEGEO') THEN
          ICEGEO=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICEREF') THEN
          ICEREF=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICERES') THEN
          ICERES=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICECOV') THEN
          ICECOV=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'ICEBLK') THEN
          ICEBLK=I
        ELSEIF(ICE_FILES(I)%TELNAME.EQ.'CLGRFO') THEN
          CLGRFO=I
        ENDIF
      ENDDO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ALGORITHMIC DIFFERENTIATION
!
      NADVAR    = MOTINT( ADRESS(1,13) )
!
!     NAMES OF DIFFERENTIATED VARIABLES
      N_NAMES_ADVAR = DIMEN(4,13)
      NADVAR = MAX( NADVAR,N_NAMES_ADVAR ) ! WARNING HERE ?
      IF(NADVAR.GT.0) THEN
        DO I=1,NADVAR
          WRITE(CHAR2,'(I2)') I
          NAMES_ADVAR(I) =  'DERIVATIVE '//ADJUSTL(CHAR2)//'   '
     &                   // '??              '
        ENDDO
      ENDIF
      IF(N_NAMES_ADVAR.GT.0) THEN
        DO K=1,N_NAMES_ADVAR
          NAMES_ADVAR(K) = MOTCAR(ADRESS(4,13)+K-1)(1:32)
        ENDDO
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     LISTING AND GRAPHICAL OUTPUTS
!     __________________________________________________________________
!     TITLE OF THE STUDY
      TITICECAS = MOTCAR( ADRESS(4, 3) )(1:72)
!     __________________________________________________________________
!     GRAPHIC PRINTOUT PERIOD
      LEOPRD    = MOTINT( ADRESS(1,  1) )
!     __________________________________________________________________
!     LISTING PRINTOUT PERIOD
      LISPRD    = MOTINT( ADRESS(1,  2) )
!     __________________________________________________________________
!     FILES IN THE STEERING FILE
!
      ICE_FILES(ICECLI)%NAME=MOTCAR( ADRESS(4,4 ) )
      ICE_FILES(ICEGEO)%NAME=MOTCAR( ADRESS(4,5 ) )
      ICE_FILES(ICEGEO)%FMT=MOTCAR( ADRESS(4,35) )(1:8)
      ICE_FILES(ICEREF)%NAME=MOTCAR( ADRESS(4,6) )
      ICE_FILES(ICEREF)%FMT=MOTCAR( ADRESS(4,36) )(1:8)
      ICE_FILES(ICERES)%NAME=MOTCAR( ADRESS(4,7 ) )
      ICE_FILES(ICERES)%FMT=MOTCAR( ADRESS(4,37) )(1:8)
      ! PREVIOUS ICE COVER FILE
      ICE_FILES(ICECOV)%NAME=MOTCAR( ADRESS(4,8) )
      ICE_FILES(ICECOV)%FMT=MOTCAR( ADRESS(4,14) )(1:8)
      CALL MAJUS(ICE_FILES(ICECOV)%FMT)
      ! PREVIOUS ICE BLOCKS FILE
      ICE_FILES(ICEBLK)%NAME=MOTCAR( ADRESS(4,15) )
      ICE_FILES(ICEBLK)%FMT=MOTCAR( ADRESS(4,16) )(1:8)
      CALL MAJUS(ICE_FILES(ICECOV)%FMT)
      ! CLOGGING RESULTS FILE
      ICE_FILES(CLGRFO)%NAME=MOTCAR( ADRESS(4,25 ) )
!     __________________________________________________________________
!     UPDATE OF MNEMO AND NOMVARS
      DO I=1,MAXVAR
        MNEMO(I) = '        '
      ENDDO
      CALL NOMVAR_KHIONE( TEXTE,TEXTPR,MNEMO,NADVAR,NAMES_ADVAR )
!     __________________________________________________________________
!     GRAPHICAL OUTPUT VARIABLES
      VARDES = MOTCAR( ADRESS(4, 10) )(1:72)
      CALL MAJUS( VARDES )
      CALL SORTIE( VARDES , MNEMO , MAXVAR , SORLEO )
      VARIMP = MOTCAR( ADRESS(4, 11) )(1:72)
      CALL MAJUS( VARIMP )
      CALL SORTIE( VARIMP , MNEMO , MAXVAR , SORIMP )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     PHYSICAL PRPERTIES AND CONSTANTS RELATED KEYWORDS
!
!     __________________________________________________________________
!     DENSITIES
      RHO_AIR   = MOTREA( ADRESS(2, 11) )
      RHO_ICE   = MOTREA( ADRESS(2, 12) )
!     __________________________________________________________________
!     SPECIFIC AND LATENT HEATS
!      CP_EAU    = MOTREA( ADRESS(2, 15) )
      CP_ICE    = MOTREA( ADRESS(2, 16) )
      LH_ICE    = MOTREA( ADRESS(2, 17) )
!     __________________________________________________________________
!     ICE ALBEDO
      ALBE = MOTREA( ADRESS(2,34) )

!     __________________________________________________________________
!     LOGICAL SWITCHES
!
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ATMOSPHERIC PROPERTY RELATED KEYWORDS (IN METEO_KHIONE)
!
!     __________________________________________________________________
!     VERTICAL DATUM
      WINDZ     = MOTREA( ADRESS(2,22) )
      MODELZ    = MOTREA( ADRESS(2,23) )
!     __________________________________________________________________
!     SUN RISE AND SUN SET ANGLES
      ALPHSD    = MOTREA( ADRESS(2,25) )     !? sun exit angle
      ALPHRD    = MOTREA( ADRESS(2,26) )     !? sun emission angle
!     __________________________________________________________________
!     SOLAR CONSTANT
      SIO       = MOTREA( ADRESS(2,33) )     !? solar constant
!     __________________________________________________________________
!     HEAT EXCHANGE CALIBRATION COEFFICIENTS
!     (ATMOSPHERE-WATER EXCHANGE MODEL=3)
      LIN_WATAIR = MOTREA( ADRESS(2, 18) )   ! replaces HWA
      CST_WATAIR = MOTREA( ADRESS(2, 20) )   ! replaces ALPW
      LIN_ICEAIR = MOTREA( ADRESS(2, 19) )   ! replaces HIA
      CST_ICEAIR = MOTREA( ADRESS(2, 21) )   ! replaces ALP
!     __________________________________________________________________
!     HEAT EXCHANGE CALIBRATION COEFFICIENTS
!     (ATMOSPHERE-WATER EXCHANGE MODEL=4)
      COEF_PHIB = MOTREA( ADRESS(2, 56) )
      COEF_PHIE = MOTREA( ADRESS(2, 57) )
      COEF_PHIH = MOTREA( ADRESS(2, 58) )
      COEF_PHIP = MOTREA( ADRESS(2, 59) )
!     __________________________________________________________________
!     METEOROLOGY, IN CASE ABSENT FROM METEO FILES
      CST_TAIR  = MOTREA( ADRESS(2, 41) )
      CST_TDEW  = MOTREA( ADRESS(2, 42) )
      CST_CLDC  = MOTREA( ADRESS(2, 43) )
      CST_VISBI = MOTREA( ADRESS(2, 45) )
      CST_PLUIE = MOTREA( ADRESS(2, 46) )
      IF( DIMEN(2,47).EQ.2 ) THEN
        CST_WINDS = MOTREA( ADRESS(2,47) + 0 )
        CST_WINDD = MOTREA( ADRESS(2,47) + 1 )
        DTRS4 = ATAN(1.D0) / 45.D0
        CST_WINDX = - CST_WINDS * SIN( CST_WINDD*DTRS4 )
        CST_WINDY = - CST_WINDS * COS( CST_WINDD*DTRS4 )
      ENDIF
      IF( DIMEN(2,48).EQ.2 ) THEN
        CST_WINDX = MOTREA( ADRESS(2,48) + 0 )
        CST_WINDY = MOTREA( ADRESS(2,48) + 1 )
      ENDIF
      CST_PATM  = MOTREA( ADRESS(2, 49) )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     WATER PROPERTY RELATED KEYWORDS
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     FREEZEUP RELATED KEYWORDS
!
      VNU  = MOTREA( ADRESS(2,24) )      ! NUSSELT NUMBER
      DF   = MOTREA( ADRESS(2,27) )      ! LENGTH OF A-AXIS OF A FRAZIL CRYSTAL(M)
      DE   = MOTREA( ADRESS(2,28) )      ! FRAZIL CRYSTAL THICKNESS(M)
      CV0  = MOTREA( ADRESS(2,40) )      ! THRESHOLD OF FRAZIL CONCENTRATION FOR THERMAL GROWTH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ICE COVER PROPERTY RELATED KEYWORDS
!
!
      THI0 = MOTREA(ADRESS(2,53))       ! (0.2) INITIAL ICE THICKNESS
      ANMAX = MOTREA(ADRESS(2,54))      ! (0.6) MAXIMUM ICE CONCENTRATION
!
      DARCYILD = MOTREA(ADRESS(2,13))   ! (1.) DARCY COEFFICIENT IN ICE ISLAND
      DARCYRUB = MOTREA(ADRESS(2,14))   ! (1.) DARCY COEFFICIENT IN RUBBLE ICE
!
      CA0 = MOTREA(ADRESS(2,38))        ! (1E-4) initial surface ice conc.
!
      IFROT = MOTINT(ADRESS(1,8))       ! ICE COVER FRICTION LAW
      IFICE = MOTINT(ADRESS(1,16))      ! LAW FOR FRICTION COEF
      FICE = MOTREA(ADRESS(2,4))        ! ICE COVER FRICTION COEFFICIENT
      FICE_MAX = MOTREA(ADRESS(2,52))   ! ICE COVER MAX FRICTION COEFFICIENT
      THIE = MOTREA( ADRESS(2,75) )     ! EQUIVALENT SURFACE ICE THCIKNESS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     STATIC BORDER ICE RELATED KEYWORDS
!
!
      BCH = MOTREA( ADRESS(2,98) )       ! (15) CHANNEL WIDTH FOR THE COMPUTATION OF SURFACE WATER TEMPERATURE
      TC = MOTREA( ADRESS(2,71) )        ! (-1.1) CRITICAL WATER SURFACE TEMPERATURE FOR BORDER ICE FORMATION
      VCRBOR = MOTREA(ADRESS(2,50))      ! (0.07) THRESHOLD VELOCITY FOR STATIC BORDER ICE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DYNAMIC BORDER ICE RELATED KEYWORDS
!
      VCRBOM = MOTREA(ADRESS(2,73))      ! (0.4) THRESHOLD VELOCITY ABOVE WHICH DYNAMIC BORDER ICE WILL NOT FORM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SKIM ICE RELATED KEYWORDS
!
      VCRSKM = MOTREA( ADRESS(2,72) )    ! CRITICAL VELOCITY ABOVE WHICH SKIM ICE WILL NOT FORM (M/S)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ICE PROPERTY RELATED KEYWORDS
!
!
!*******************************
!     REAL KEYWORDS            *
!*******************************
!       RHOW = MOTREA( ADRESS(2,1) )
!       ROI = MOTREA( ADRESS(2,4) )
!       LDISP = MOTREA( ADRESS(2,9) )
!       TDISP = MOTREA( ADRESS(2,10) )
! /!\ TO BE REMOVED
      ALSM = MOTREA( ADRESS(2,30) )     ! standard longitude, in degrees
      ALLM = MOTREA( ADRESS(2,31) )     ! local longitude, in degrees
! /!\ TO BE REPLACED BY LONGITUDE OF ORIGIN POINT
      ETADIR = MOTREA( ADRESS(2,32) )   ! - for west longitude, + for east longitude
!
!       CWI1 = MOTREA( ADRESS(2,22) )
!       CIW1 = MOTREA( ADRESS(2,23) )
!       ATA = MOTREA( ADRESS(2,24) )
!       XKI = MOTREA( ADRESS(2,25) )      ! => TC_BI
!       XKW = MOTREA( ADRESS(2,26) )      ! => TC_WI
!
!       XKS = MOTREA( ADRESS(2,27) )      ! => TC_S
!       RHOS = MOTREA( ADRESS(2,28) )
!       SGMA = MOTREA( ADRESS(2,29) )
!
!       TC = MOTREA( ADRESS(2,31) )
!       VCRSKM = MOTREA( ADRESS(2,32) )
!       VCRBOM = MOTREA( ADRESS(2,33) )
!       ANMAXBORDER = MOTREA( ADRESS(2,34) )

!       HF0 = MOTREA( ADRESS(2,36) )
!       ANMAXFRA = MOTREA( ADRESS(2,37) )
!       ANMINFRA = MOTREA( ADRESS(2,38) )
!
      AF   = MOTREA( ADRESS(2,29) )      ! (1.) DEPOSITION COEFFICIENT OF FRAZIL ON THE BAR
      SURF_EF  = MOTREA( ADRESS(2,39) )  ! POROSITY OF SURFACE ICE
      TC_WT = MOTREA( ADRESS(2,44) )     ! => XKWP: (0.56594) WATER-ICE THERMAL CONDUCTIVITY
!
!       STPV = MOTREA( ADRESS(2,51) )
!       CRIFR = MOTREA( ADRESS(2,52) )
!       UEROS = MOTREA( ADRESS(2,53) )
!       VCRFRZ = MOTREA( ADRESS(2,54) )
!       STRENGTH = MOTREA( ADRESS(2,55) )
!
      CLOG_EF    = MOTREA( ADRESS(2,37) )! POROSITY OF FRAZIL ICE
      CLOG_THETA = MOTREA( ADRESS(2,36) )
      IF( DIMEN(2,35).EQ.4 ) THEN
        CLOG_TDIST = MOTREA( ADRESS(2,35) + 0 )
        CLOG_TDIAM = MOTREA( ADRESS(2,35) + 1 )
        CLOG_VDIST = MOTREA( ADRESS(2,35) + 2 )
        CLOG_VDIAM = MOTREA( ADRESS(2,35) + 3 )
      ENDIF
!
      NFRCLOG = DIMEN(1,14)
      IF( MOTINT( ADRESS(1,14) ).EQ.0 ) NFRCLOG = 0
      IF( NFRCLOG.GT.0 ) THEN
!
        ALLOCATE( LINES%CELLS(NFRCLOG) )
        LINES%NVAL = NFRCLOG
        ALLOCATE(NUMCLOG(NFRCLOG))
        ALLOCATE(CLOG_TLGTH(NFRCLOG))
        ALLOCATE(CLOG_TWDTH(NFRCLOG))
        ALLOCATE(CLOG_VLGTH(NFRCLOG))
        ALLOCATE(CLOG_VWDTH(NFRCLOG))
        ALLOCATE(CLOG_VOLUM(NFRCLOG))
!
!       ACUMULATION ANGLE
        ANG = 4.D0*ATAN(1.D0) * CLOG_THETA / 180.0
        DO K = 1,NFRCLOG
          LINES%CELLS(K)%NVAL = 0
          NUMCLOG(K) = MOTINT( ADRESS(1,14) + K-1 )
          CLOG_TLGTH(K) = 0.D0
          CLOG_TWDTH(K) = 2.0*CLOG_TDIAM*COS(ANG)*SIN(ANG)
          CLOG_VLGTH(K) = 0.D0
          CLOG_VWDTH(K) = 2.0*CLOG_VDIAM*COS(ANG)*SIN(ANG)
          CLOG_VOLUM(K) = 0.D0
        ENDDO
!
      ENDIF
!

!     TODO: THE FOLLOWING CODE SHOULD BE REMOVED
!     IT IS INCLUDED ONLY TO CHECK THAT THERE IS NO DUPLICATION
!     BETWEEN THE WHAT IS LEFT IN ICE2D AND WHAT HAS COME INTO KHIONE
!***********************************************************************
!     PRINTOUT WAQ PERIOD
      MAXNOD = MOTINT( ADRESS(1,  3) )    ! MAXIMUM NODES
      MAXEL = MAXNOD*2
      MAXLNK = MOTINT( ADRESS(1,  4) )    ! MAXIMUM NEIGHBORING NODES
      MAXPAR = MOTINT( ADRESS(1,  5) )    ! MAXIMUM ICE PARCEL NUMBER
      MAXKRN = MAXPAR*40
      MAXBMSP = MOTINT( ADRESS(1,  6))    ! MAXIMUM NUMBER OF BOOM SPANS
      MAXGDB = MOTINT( ADRESS(1,  7) )    ! MAXIMUM NUMBER OF ICE BOUNDARY SEGMENT
      MAXBM = MOTINT( ADRESS(1,  108) )     ! MAXIMUM NUMBER OF ICE BOOM
      MAXNBP = MOTINT( ADRESS(1,  9) )    ! MAXIMUM BOUNDARY PARCEL NUMBER
      MINGDX = MOTINT( ADRESS(1,  10) )   ! PARCEL SEARCH XMIN
      MAXGDX = MOTINT( ADRESS(1,  11) )   ! PARCEL SEARCH XMAX
      MINGDY = MOTINT( ADRESS(1,  12) )   ! PARCEL SEARCH YMIN
      MAXGDY = MOTINT( ADRESS(1,  18) )   ! PARCEL SEARCH YMAX
      MAXBRK = MOTINT( ADRESS(1,  19) )   ! MAXIMUM NUMBER OF BREAKUP REACHES
      MAXSDB = MOTINT( ADRESS(1,  15) )   ! MAXIMUM NUMBER OF SOLID BOUNDARY NODE
      MAXOPB = MOTINT( ADRESS(1,  20) )   ! MAXIMUM NUMBER OF OPEN BOUNDARY NODE
!     ICE PROCESS

!     WEATHER TYPE
      IWEATYPE = MOTINT( ADRESS(1, 21) )  ! WEATHER TYPE

      RHOW = MOTREA( ADRESS(2,1) )        ! WATER DENSITY
      RHOICE = MOTREA( ADRESS(2,2) )      ! ICE DENSITY
      RHOAIR = MOTREA( ADRESS(2,3) )      ! AIR DENSITY
      ROI = RHOICE / RHOW                 ! MOTREA( ADRESS(2,4) )
!      XLATEN = MOTREA( ADRESS(2,5) )      ! ICE LATENT HEAT      => LH_ICE
      CP = MOTREA( ADRESS(2,6) )          ! WATER SPECIFIC HEAT
      CI = MOTREA( ADRESS(2,7) )          ! ICE SPECIFIC HEAT
      XNU = MOTREA( ADRESS(2,8) )         ! KINEMATIC WATER VISCOSITY
      LDISP = MOTREA( ADRESS(2,9) )       ! DISPERSION ALONG THE FLOW/FRAZIL
      TDISP = MOTREA( ADRESS(2,10) )      ! DISPERSION ACROSS THE FLOW/FRAZIL
      PHID = MOTREA( ADRESS(2,61) )       ! GEOGRAPHIC LATITUDE
!     Z1 = MOTREA( ADRESS(2,12) )         ! => WINDZ
!     ZH = MOTREA( ADRESS(2,13) )         ! => MODELZ

!      HWA = MOTREA( ADRESS(2,19) )       ! => LIN_WATAIR
!      ALPW = MOTREA(ADRESS(2,17))        ! => CST_WATAIR
!      HIA = MOTREA( ADRESS(2,20) )       ! => LIN_ICEAIR
!      ALP = MOTREA( ADRESS(2,21) )       ! => CST_ICEAIR

      CWI1 = MOTREA( ADRESS(2,62) )       ! CONSTANT FOR HEAT TRANSFER BETWEEN TURBULENT WATER AND ICE
      CIW1 = MOTREA( ADRESS(2,63) )       ! CONSTANT FOR HEAT TRANSFER FOR SUPERCOOLED TURBULENT FLOW
      ATA = MOTREA( ADRESS(2,64) )        ! NUSSELT NUMBER FOR HEAT TRANSFER BETWEEN LAMINAR WATER AND ICE
      TC_BI = MOTREA( ADRESS(2,65) )      ! THERMAL CONDUCTIVITY OF BLACK ICE (W/M/OC) => XKI
      TC_WI = MOTREA( ADRESS(2,66) )      ! THERMAL CONDUCTIVITY OF WHITE ICE(W/M/OC) => XKW

      TC_S = MOTREA( ADRESS(2,67) )       ! THERMAL CONDUCTIVITY OF SNOW(W/M/OC) => XKS
      RHOS = MOTREA( ADRESS(2,68) )       ! HEAT TRANSFER COEFFICIENT FROM WATER TO ICE
      SGMA = MOTREA( ADRESS(2,69) )       ! BOLTZMANN CONSTANT (WM-2K-4)
      CST_TMELT = MOTREA( ADRESS(2,70) )  ! FREEZING POINT OF WATER

!
      ANFEM0 = MOTREA( ADRESS(2,74) )     ! (1.) MAXIMUM CONCENTRATION FOR BORDER ICE FORMATION
!
      HF0 = MOTREA( ADRESS(2,76) )        ! INITIAL THICKNESS OF FRAZIL PART OF ICE FLOES(M)
      ANMAXFRA = MOTREA( ADRESS(2,77) )   ! MAXIMUM FRAZIL CONCENTRATION FROM SUSPENDED TO SURFACE LAYER
      ANMINFRA = MOTREA( ADRESS(2,78) )   ! MINIMUM FRAZIL CONCENTRATION FROM SUSPENDED TO SURFACE LAYER

!      EF = MOTREA( ADRESS(2,39) )        ! => SURF_EF
!      EF0 = MOTREA( ADRESS(2,60) )       ! => CLOG_EF

      TUN = MOTREA( ADRESS(2,83) )        ! (0.) NUCLEATION TEMPERATURE(OC)

      UNPSIZE = MOTREA( ADRESS(2,87) )    ! (0.001) NOMINAL DIAMETER OF ICE PARTICLES(M)
!       BOUYANT VELOCITY
      VBUN = SQRT( 9.81 * UNPSIZE * (1 - ROI))
      UNTHETAC = MOTREA( ADRESS(2,88) )   ! (0.041) CRITICAL FLOW STRENGTH, BELOW WHICH THERE IS NO ICE TRANSPORT
      UNMAXANP = MOTREA( ADRESS(2,89) )   ! (0.6) MAXIMUM CONCENTRATION OF UNDERCOVER LOAD
      UNALPHAV = MOTREA( ADRESS(2,90) )   ! (1.0) THE RATIO OF ICELOAD TO FLOW VELOCITY

      STPV = MOTREA( ADRESS(2,91) )       ! (0.) ICE PARCEL STOPPAGE CRITERION
      CRIFR = MOTREA( ADRESS(2,92) )      ! (0.09) CRITICAL FROUDE NUMBER FOR ICE PARCEL TO SUBMERGE
      UEROS = MOTREA( ADRESS(2,93) )      ! (1.5) EROSION VELOCITY OF ICE PARCELS
      VCRFRZ = MOTREA( ADRESS(2,94) )     ! (0.001) CRITICAL VELOCITY FOR FREEZING CALCULATION
      STRENGTH = MOTREA( ADRESS(2,95) )   ! (5000.) COMPRESSIVE STRENGTH OF THE ICE COVER

!      HBAR = MOTREA(ADRESS(2,62))        ! /!\ COMPUTED: VERTICAL BAR LENGTH
!      TICEBG = MOTREA(ADRESS(2,79))      ! /!\ REMOVE: BEGINNING TIME FOR ICEDYNAMICS
!      IF(TICEBG.LT.AT0) TICEBG = AT0
      TIMICE = TICEBG
      TICE0 = TICEBG

      DELTMI = MOTREA(ADRESS(2,79))       ! (0.5) TIME STEP FOR ICEDYNAMICS
      TINTVL = MOTREA(ADRESS(2,80))       ! (900.) COUPLING TIME ICEDYNAMICS
!      ITHIMAX = INT(TINTVL/DT/2.0)
      ITHIMAX = MAX(ITHIMAX,1)
      ITICE = IDINT(TINTVL/DELTMI + 0.45)
      ITHIUP = 1
      ISEED = 101

      TUNBG = MOTREA(ADRESS(2,81))        ! INITIAL TIME FOR UNDERCOVER TRANSPORT
!      IF(TUNBG.LT.AT0) TUNBG = AT0

      DELTMU = MOTREA(ADRESS(2,82))       ! (0.5) TIME STEP FOR UNDERCOVER TRANSPORT
      TINUN = MOTREA(ADRESS(2,84))        ! (0.5) COUPLING TIME UNDERCOVER
!      IUND = IDINT(DT/TINUN + 0.45)

      SGRD = MOTREA(ADRESS(2,85))         ! (50.) AUXILIARY GRID SIZE
      ADPL = MOTREA(ADRESS(2,86))         ! (999.) UNDERCOVER TRANSPORT ADAPTATION LENGTH
      GRAVT = MOTREA(ADRESS(2,96))        ! /!\ REMOVE: (9.81) GRAVITATIONAL ACCELERATION

      DTBMF = MOTREA(ADRESS(2,97))        ! /!\ REMOVE: (900.) TIME INTERVAL FOR OUTPUT ICE FORCE ON THE BOOM
!     ANMAXSKIM = MOTREA(ADRESS(2,98))    ! (1.0) MAXIMUM CONCENTRATION FOR SKIM ICE RUN
!
      ILINKSWITCH = MOTLOG(ADRESS(3,7))   ! CREATE LINK FILE
!      ICEDYNAMICS = MOTLOG(ADRESS(3,9))  ! /!\ REMOVED
      ITHERMOSWITCH = MOTLOG(ADRESS(3,10))      ! THERMAL SWITCH
      IUNDERCOVERSWITCH = MOTLOG(ADRESS(3,11))  ! UNDERCOVER TRANSPORT SWITCH
      ITHERMOICESWITH = MOTLOG(ADRESS(3,12))    ! THERMOICE SWITH
      IBREAKUPSWITCH = MOTLOG(ADRESS(3,13))     ! BREAKUP SWITCH
!      LKFSWITCH = MOTLOG(ADRESS(3,13))

      IFCLOGSWITCH = MOTLOG(ADRESS(3,63)) ! FRAZIL CLOG SWITCH
!      IOUTPUT = MOTLOG(ADRESS(3,64))     ! /!\ REMOVED
!      ICIPUT = MOTLOG(ADRESS(3,72))      ! /!\ REMOVED
!
!***********************************************************************
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
