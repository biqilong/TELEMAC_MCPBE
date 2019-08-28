!                    ******************
                     SUBROUTINE CONDICE
!                    ******************
!
     &( NPOIN,H,RECORD,AT,LISTIN )
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETERS FOR ICE.
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        09/09/2017
!+        V7P3
!+        INITIAL DEVELOPMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF POINT IN THE GEOMETRY MESH
!| RECORD         |-->| TIME STEP OF THE DATASET
!| AT             |<->| TIME OF THE DATASET
!| LISTIN         |-->| IF YES, INFORMATIONS PRINTED ON LISTING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE
      USE DECLARATIONS_WAQTEL, ONLY : RO0
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL          :: LISTIN
      INTEGER          :: NPOIN,RECORD, I
      DOUBLE PRECISION :: AT
!
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: H
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER TROUVE(MAXVAR)
      TROUVE = 0
!
!-----------------------------------------------------------------------
!
!   ATMOSPHERIC HEAT FLUXES
!
!     PHCL: SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLEAR SKY
      CALL OS('X=C     ', X=PHCL, C=0.D0 )
!     PHRI: SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLOUDY SKY
      CALL OS('X=C     ', X=PHRI, C=0.D0 )
!     PHPS: NET SOLAR RAD (FLUX) AFTER REFLECTION
      CALL OS('X=C     ', X=PHPS, C=0.D0 )
!     PHIB: EFFECTIVE BACK RADIATION (OPEN WATER OR ICE)
      CALL OS('X=C     ', X=PHIB, C=0.D0 )
!     PHIE: EVAPORATIVE HEAT TRANSFER
      CALL OS('X=C     ', X=PHIE, C=0.D0 )
!     PHIH: CONVECTIVE HEAT TRANSFER
      CALL OS('X=C     ', X=PHIH, C=0.D0 )
!     PHIP: HEAT TRANSFER DUE TO PRECIPITATION
      CALL OS('X=C     ', X=PHIP, C=0.D0 )
!     PHIW: HEAT TRANSFER BETWEEN WATER AND ICE
      CALL OS('X=C     ', X=PHIW, C=0.D0 )
!     SUMPH: NET SUM OF ALL THERMAL FLUXES
      CALL OS('X=C     ', X=SUMPH, C=0.D0 )
!
!-----------------------------------------------------------------------
!
!   ICE COVER
!
!     PROBABILITY OF FRAZIL DEPOSITION - OPEN WATER
      CALL OS('X=C     ', X=THETA0, C=0.D0 )
!     PROBABILITY OF FRAZIL DEPOSITION - ICE COVER
      CALL OS('X=C     ', X=THETA1, C=0.D0 )
!     RATE OF REENTRAINMENT OF SURFACE PER UNIT AREA
      CALL OS('X=C     ', X=BETA1, C=0.D0 )
!     SETTLING VELOCITY OF FRAZIL ICE IN THE TURBULENT FLOW
      CALL OS('X=C     ', X=VBB, C=0.D0 )
!
!     MORE ICE THAN WATER AT SURFACE IF ANFEM(I) > 0.5
      CALL OS('X=C     ', X=ANFEM, C=0.D0 )
!     SOLID ICE THICKNESS
      CALL OS('X=C     ', X=THIFEMS, C=0.D0 )
!     FRAZIL ICE THICKNESS
      CALL OS('X=C     ', X=THIFEMF, C=0.D0 )
!     DYNAMIC ICE GROWTH AREA AS A FRACTION OF THE NODE AREA
      CALL OS('X=C     ', X=DWB, C=0.D0 )
!
!     UNDERCOVER ICE THICKNESS
      CALL OS('X=C     ', X=HUN, C=0.D0 )
!
!     ICE VELOCITY COMPONENTS
      CALL OS('X=C     ', X=U_ICE, C=0.D0 )
      CALL OS('X=C     ', X=V_ICE, C=0.D0 )
      CALL OS('X=C     ', X=H_ICE, C=0.D0 )
!
      CALL OS('X=C     ', X=TIWX, C=0.D0 )
      CALL OS('X=C     ', X=TIWY, C=0.D0 )
      CALL OS('X=C     ', X=ICESTR, C=FICE )
!
!     VERTICAL TURBULENT INTENSITY
      CALL OS('X=C     ', X=VZ, C=0.D0 )
!
!-----------------------------------------------------------------------
!
!   STATIC ICE COVER
!
!
!-----------------------------------------------------------------------
!
!***********************************************************************
!
        CALL OS('X=C     ', X=XFEM, C=0.D0 )
        CALL OS('X=C     ', X=YFEM, C=0.D0 )
        CALL OS('X=C     ', X=GAMC, C=0.D0 )
        CALL OS('X=C     ', X=GAMA, C=0.D0 )
        CALL OS('X=C     ', X=HTW, C=0.D0 )
        CALL OS('X=C     ', X=ZWI, C=0.D0 )
!        CALL OS('X=C     ', X=VB, C=0.D0 )
        CALL OS('X=C     ', X=DTHIFEM, C=0.D0 )
        CALL OS('X=C     ', X=UICE, C=0.D0 )
        CALL OS('X=C     ', X=VICE, C=0.D0 )
        CALL OS('X=C     ', X=UQX, C=0.D0 )
        CALL OS('X=C     ', X=UQY, C=0.D0 )
        CALL OS('X=C     ', X=QX, C=0.D0 )
        CALL OS('X=C     ', X=QY, C=0.D0 )
        CALL OS('X=C     ', X=TMICE, C=0.D0 )
        CALL OS('X=C     ', X=CNIEND, C=0.D0 )
        CALL OS('X=C     ', X=ETA, C=0.D0 )
        CALL OS('X=C     ', X=ETAB, C=0.D0 )
        CALL OS('X=C     ', X=ZWAT, C=0.D0 )
        CALL OS('X=C     ', X=ZICE, C=0.D0 )
        CALL OS('X=C     ', X=HBED, C=0.D0 )
        CALL OS('X=C     ', X=HICE, C=0.D0 )
        CALL OS('X=C     ', X=TISFEM, C=0.D0 )
        CALL OS('X=C     ', X=TIPFEM, C=0.D0 )
!        CALL OS('X=C     ', X=THUN, C=0.D0 )
        CALL OS('X=C     ', X=UNQIC0, C=0.D0 )
        CALL OS('X=C     ', X=UNQIC, C=0.D0 )
        CALL OS('X=C     ', X=UNQP0, C=0.D0 )
        CALL OS('X=C     ', X=UNQP, C=0.D0 )
        CALL OS('X=C     ', X=UNFORML, C=0.D0 )
        CALL OS('X=C     ', X=UNUXP, C=0.D0 )
        CALL OS('X=C     ', X=UNUYP, C=0.D0 )
        CALL OS('X=C     ', X=UNFS, C=0.D0 )
        CALL OS('X=C     ', X=UNVP, C=0.D0 )
        CALL OS('X=C     ', X=TIOUT, C=0.D0 )
        CALL OS('X=C     ', X=TISP, C=0.D0 )
        CALL OS('X=C     ', X=TIPP, C=0.D0 )
        CALL OS('X=C     ', X=DHIOUT, C=0.D0 )
        CALL OS('X=C     ', X=TIWX, C=0.D0 )
        CALL OS('X=C     ', X=TIWY, C=0.D0 )
        CALL OS('X=C     ', X=ALAFAN, C=0.D0 )
!        CALL OS('X=C     ', X=IRDIV, C=0.D0 )

        DO I = 1,MAXPAR
          NPOUT%I(I) = 0
          NPARELM%I(I) = 0
          IDELTA%I(I) = 0
        ENDDO

      DO I = 1, NPOIN
        ISBORDER%I(I) = 0
        ISBANK%I(I) = 0
        IFUND%I(I) = 0
        JAMFEM%I(I) = 0
        ICEREGION%I(I) = 0
        LIFCG%I(I) = 0
      ENDDO

!     DEFINE THE CLOGGING NODES
!      IF(IFCLOGSWITCH) THEN
!       NBC = 7
!       DO I = 1, NPTFR
!        IF(LITBOR(I).EQ.KSORT) THEN
!          NB = NBOR(I) - NBC
!          LIFCG%I(NB) = 1  ! KCGQ DISCHARGE
!          LIFCG%I(NBOR(I)) = 2 ! KCGB
!        ENDIF
!       ENDDO
!      ENDIF
!

!------------------------------------------------------------
!       ICEDYNAMICS INPUT FILES
!        CASENAME1 = 'IN'
!        KN1 = 0
!        DO 10 I = LEN(CASENAME1), 1, -1
!          IF ( CASENAME1(I:I) .NE. ' ' ) THEN
!            GOTO 20
!          ELSE
!            KN1 = KN1 + 1
!          END IF
!  10    CONTINUE
!  20    CONTINUE
!        KN1 = LEN(CASENAME1) - KN1
!
!         INQUIRE ( FILE = CASENAME1(1:KN1), EXIST = LG )
!
!        IF ( .NOT. LG ) THEN
!            WRITE(LU,*) '"IN" FOLDER DOES NOT EXIST UNDER CASE NAME'
!            STOP
!        END IF
!
!        FNAME = 'ICE'   !'ICE INPUT FILES'
!        IFRZ = 107
!        ICSG = 108
!        ILNK = 109
!        IQBD = 110
!        INBM = 111
!        IBRK = 112
!        IUDC = 113
!        IHOT0 = 114
!        ISTR0 = 115
!        IHDW0 = 116
!
!        IF(ITHERMOSWITCH) THEN
!          OPEN(901,FILE='IN\WEATHER.TXT')
!          READ(901,*)
!          READ(901,*)
!          READ(901,*) TIMA0,TAIR0
!          READ(901,*) TIMA1,TAIR1
!        ENDIF
!
!        KFLXI = 2
!
!        DO I = LEN(FNAME),1,-1
!          IF(FNAME(I:I) .NE.' ') THEN
!!            GEOFILE = 'IN\'//FNAME(1:I)//NSERIAL//'.GEO'
!            LNKFILE = CASENAME1(1:KN1)//'\LINK.DAT'
!            FZFILE = CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.IFZ'
!            TRFILE = CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.ITR'
!            IQBFILE = CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.IQB'
!            BMFILE =  CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.NBM'
!            BKFILE =  CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.BRK'
!            UDFILE =  CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.UND'
!            HT0FILE = CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.HOT'
!            ST0FILE = CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.STR'
!            HD0FILE = CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.HDW'
!            GOTO 30
!          ENDIF
!        ENDDO
!  30    CONTINUE
!
!        CALL GEONORM_ICE2D(NELEM,NPOIN,ALAFAE,ELMHT,ALAFAN%R)
!
!        CALL GEOLINK_ICE2D
!     &(MINGDX,MINGDY,MAXGDX,MAXGDY,MESH%X%R,MESH%Y%R,XFEM%R,
!     & YFEM%R,NPOIN,NELEM,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,MESH%IKLE%I,
!     & NECODE,NCODE,NCBDY,NSTAR,NEND,NODE1,NODE2,NODE3,NPTFR,JAMFEM%I,
!     & ILINKSWITCH,ILNK,LNKFILE,SGRD,MESH%SURFAC%R,CLMP,XMIN,XGMIN,YMIN,
!     & YGMIN,XMAX,YMAX,XMAXH,YMAXH,A1,A2,A3,B1,B2,B3,
!     & C1,C2,C3,MEGRID,NDGRID,LINKED,MAXLNK,MAXNOD,NLB,NOB,NMLBN,NMOBN,
!     & NODELB,NODEOB,KLB,KLE,KOB,KOE,MAXSDB,MAXOPB,ALAFAE,ELMHT,
!     & ALAFAN%R,NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,MAXFRO,
!     & KFROPT%I,IRDIV%I,NZONES)
!        WRITE(*,*) 'BACK FROM GEOLINK_ICE2D'
!
!        IF(ITHERMOSWITCH) THEN
!          CALL READFRZ_ICE2D(IFRZ,FZFILE,NZONES,KFROPT%I,NPOIN,THETA0%R,
!     &            THETA1%R,BETA1%R,VBB%R)
!          WRITE(*,*) 'BACK FROM READFRZ_ICE2D'
!        ENDIF
!
!        IF(IFCLOGSWITCH) THEN
!          CALL READCLG_ICE2D
!     &(ICSG,TRFILE,NITK,LIFCG%I,NPOIN,NITKCS,NIFZCS,NCGNO,NFZNO,
!     & MAXFRO)
!        ENDIF
!
!!        READ ICE BOUNDARY FILE
!        CALL READIBD_ICE2D
!     &(IQBD,IQBFILE,CNISLD,DARCYILD,DARCYRUB,ANMAX,AN0,THI0,THI0F,HPI0,
!     & FRIC1,FRIC2,WDCA,CNI,CNIMAX,PHI,PJ,MSBD,IQBJB,IBDF,KPBD,KPBD1,
!     & NPBD,XFEM%R,YFEM%R,NPOIN,ANB,QIB,XBFLX,YBFLX,DLFLX,SLPB,ITBDF,
!     & TIMICE,FLXITM,PMI0,PMI0F,MAXFRO)
!        WRITE(*,*) 'BACK FROM READIBD_ICE2D'
!
!!        READ ICE BOOM FILE
!        CALL READIBM_ICE2D
!     & (INBM,BMFILE,NBM,DWALL,GATE,BMLINELD,THIEMERG,BMBUOY,
!     & BMFRIC,FRNUM1,NBMSP,MAXBM,XBM,YBM,XBMSP,YBMSP,BMTYPE,XGMIN,YGMIN)
!        WRITE(*,*) 'BACK FROM READIBM_ICE2D'
!
!!       READ EXISTING ICE INFORMATION
!        CALL READHOT_ICE2D
!     &(ICIPUT,IHOT0,ISTR0,HT0FILE,ST0FILE,RHOICE,ROI,THI0,HPI0,PHI,
!     & TIMICE,KNP,NPARIN,NPAROUT,NTOTALPARCELS,NBADPARCELS,ITBDF,TR,
!     & XGMIN,YGMIN,XP%R,YP%R,UPX%R,UPY%R,HPX%R,HPY%R,UM%R,ANP%R,THI0P%R,
!     & THIPS%R,THIPF%R,PMS%R,PMF%R,AREAP%R,ICETYPEP%I,ICEORGP%I,
!     & NPARELM%I,EP%R,TISP%R,TIPP%R,NSORT%I,HHPK%R,MAXPAR,MAXNBP,
!     & UM0%R,XP0%R,YP0%R,UPX0%R,UPY0%R,PM%R,DUDXT%R,
!     & DUDYT%R,DVDXT%R,DVDYT%R,DUDYDVDXT%R,SIGXX%R,SIGYY%R,SIGXY%R,
!     & IDELTA%I,
!     & HYDROPR%R,KSTRESSSTATE%I,GRAVT,TINTVL,DELTMI,
!     & ITICE,IHDW0,HD0FILE,JAMFEM%I,THIFEM%R,THIFEMS%R,THIFEMF%R,
!     & ANFEM%R,NPOIN,NELEM,
!     & ICEDYNAMICS,MESH%IKLE%I,IPBOOM%I,NAPHA%I,NAPHABM,PAPHA1,PAPHA2,
!     & PARDIS,PAPHABM1,PAPHABM2,PARDISBM,XFEM%R,YFEM%R,ISBANK%I,
!     & ISBORDER%I,NCBDY,MESH%SURFAC%R,NPOUT%I,STRFX%R,STRFY%R,
!     & THIP%R,UPXW%R,UPYW%R,IFUND%I,IUNDERCOVERSWITCH)
!       WRITE(*,*) 'BACK FROM READHOT_ICE2D',
!     &          ' EXISTING PARCEL NUMBER =',KNP
!
!       CALL IMAGEINFOR_ICE2D
!     & (NPOIN,NELEM,ALAFAN%R,NECODE,NCODE,NCBDY,NODE1,NODE2,NODE3,
!     & JAMFEM%I,
!     & NEND,NSTAR,ALAFAE,ELMHT,XFEM%R,YFEM%R,MESH%IKLE%I,LINKED,
!     & MAXLNK,MAXNOD)
!
!       CALL PREBORDERICE_ICE2D(NPOIN,NCBDY,ISBANK%I,JAMFEM%I,ISBORDER%I)
!
!!     SETUP IMAGED ELEMENT DATA
!!       CALL IMGELEMENTVSLANDBOUNARY_ICE2D
!!     &(NPOIN,MAXNOD,NELEM,IMGELM,XFEM%R,YFEM%R,NODE1,NODE2,NODE3)
!!       WRITE(*,*) 'BACK FROM IMGELEMENTVSLANDBOUNARY_ICE2D'
!
!      IF(KNP.GT.0) THEN
!         WRITE(*,*) 'READING INITIAL ICE PARCEL INFORMATION'
!         CALL PARXPARPMC_ICE2D
!     &(KNP,XP%R,YP%R,UPX%R,UPY%R,HPX%R,HPY%R,UM%R,ANP%R,THI0P%R,THIPS%R,
!     & THIPF%R,NAPHABM,NELEM,NAPHA%I,PMS%R,PMF%R,IDELTA%I,PM%R,
!     & AREAP%R,NSORT%I,XFEM%R,YFEM%R,IPBOOM%I,VB%R,VZ%R,
!     & NODE1,NODE2,NODE3,NPOIN,ICETYPEP%I,ICEORGP%I,NPARELM%I,EP%R,
!     & TISP%R,TIPP%R,
!     & NPOUT%I,STRFX%R,STRFY%R,SIGXX%R,SIGYY%R,SIGXY%R,UPXW%R,UPYW%R,
!     & HHPK%R,THIP%R,DUDXT%R,
!     & DUDYT%R,DVDXT%R,DVDYT%R,DUDYDVDXT%R,KSTRESSSTATE%I,XP0%R,YP0%R,
!     & UPX0%R,UPY0%R)
!         WRITE(*,*) 'BACK FROM PARXPARPMC_ICE2D'
!
!        CALL PARCELSTATISTIC_ICE2D
!     & (NELEM,NPOIN,KNP,NPARELM%I,NPARSINEL%I,NPARSONOD%I,NODE1,NODE2,
!     &  NODE3)
!        WRITE(*,*) 'BACK FROM PARCELSTATISTIC_ICE2D'
!
!        CALL PARXFEMPMC_ICE2D
!     &(KNP,NPOIN,XFEM%R,YFEM%R,JAMFEM%I,ANFEM%R,THIFEMS%R,THIFEMF%R,
!     & ICETYPEP%I,UICE%R,VICE%R,VB%R,VZ%R,THIFEM%R,DTHIFEM%R,DHIOUT%R,
!     & NSORT%I,XP%R,YP%R,HPX%R,HPY%R,UPX%R,UPY%R,THIPS%R,THIPF%R,AREAP%R
!     & ,TISP%R,PMS%R,PMF%R,PM%R,PMCRUST%R,UM%R,IPBOOM%I,NAPHA%I,
!     & NAPHABM,TISFEM%R,TIPFEM%R)
!        WRITE(*,*) 'BACK FROM PARXFEMPMC_ICE2D'
!
!!       WITH PREVIOUS ICE CONDITION, BUILD UP ICE THICKNESS
!         DO K = 1,NPOIN
!            THIFEM%R(K) = THIFEM%R(K) + DTHIFEM%R(K)*FLOAT(ITHIMAX)
!            DHIOUT%R(K) = 0.0
!            DTHIFEM%R(K) = 0.0
!         END DO
!      ENDIF
!      WRITE(*,*) 'FINISH ICE THICKNESS'
!
!!       READ UNDERCOVER FILE
!      IF(IUNDERCOVERSWITCH) THEN
!         CALL READUND_ICE2D
!     &(IUDC,UDFILE,IUQBC,KUNPD,NUNQP,UNQBC,IUNDT,UNDTM,ENDTIME,
!     & MAXFRO,NUQSEC,DUREE,AT0)
!      WRITE(*,*) 'BACK FROM READUND_ICE2D'
!      ENDIF
!
!!       READ BREAKUP FILE
!      IF(IBREAKUPSWITCH) THEN
!          CALL READBRK_ICE2D
!     &(IBRK,BKFILE,TINTVL,IREFNOD,IBRKDTI,IBRKWSE,BRKTIM,BRKWSE,BRKDWSE
!     &  ,NBKZON,IBKRCH,IBRZON,IBKCTR,WSERD,COVTI,MAXBRK,AT,NCOV,NRCH)
!
!          CALL BRKSETINITIALCOVER_ICE2D
!     &(NCOV,KNP,IBRZON,NELEM,IRDIV%I,XFEM%R,YFEM%R,JAMFEM%I,
!     & NPOIN,COVTI,MAXPAR,MAXBRK,XP%R,YP%R,UPX%R,UPY%R,HPX%R,HPY%R,UM%R,
!     & ANP%R,THI0P%R,
!     & THIPS%R,THIPF%R,IDELTA%I,PM%R,HYDROPR%R,
!     & IPBOOM%I,PMS%R,PMF%R,AREAP%R,ICETYPEP%I,ICEORGP%I,NPARELM%I,EP%R,
!     & TISP%R,TIPP%R,NSORT%I,
!     & NPOUT%I,STRFX%R,STRFY%R,SIGXX%R,SIGYY%R,SIGXY%R,UPXW%R,UPYW%R,
!     & HHPK%R,THIP%R,DUDXT%R,
!     & DUDYT%R,DVDXT%R,DVDYT%R,DUDYDVDXT%R,KSTRESSSTATE%I,XP0%R,YP0%R,
!     & UPX0%R,UPY0%R,UM0%R,MESH%SURFAC%R,NODE1,NODE2,NODE3,H)
!          WRITE(*,*) 'BACK FROM BRKSETINITIALCOVER_ICE2D'
!      ENDIF
!
!!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!!      OUTPUT FILES
!      IF(.NOT.IOUTPUT) RETURN
!      KHDW = 2
!      KHDI = 2
!      KHOT = 2
!      KSTR = 2
!
!!      IHDW0 = 301
!      IHDW =  302
!!      IHOT0 = 303
!      IHOT =  304
!!      ISTR0 = 305
!      ISTR =  306
!      IPRO =  307
!      IPLT =  308
!
!      CASENAME2 = 'OUT'
!      KN2 = 0
!      DO 11 I = LEN(CASENAME2), 1, -1
!       IF ( CASENAME2(I:I) .NE. ' ' ) THEN
!          GOTO 21
!       ELSE
!          KN2 = KN2 + 1
!       END IF
!  11  CONTINUE
!  21  CONTINUE
!      KN2 = LEN(CASENAME2) - KN2
!!         WRITE(*,*) 'FILE',KN2,CASENAME2(1:KN2)
!!     INQUIRE ( FILE = CASENAME2(1:KN2), EXIST = LG )
!!         WRITE(*,*) 'FILE 1'
!!      IF ( .NOT. LG ) THEN
!!        WRITE(*,*) '"OUT" FOLDER DOES NOT EXIST UNDER CASE NAME'
!!        STOP
!!      END IF
!
!!      DO I = 1,10
!!!        IQBFILE = CASENAME1(1:KN1)//'\'//FNAME(1:I)//'.IQB'
!!        PLTFILE(I) = CASENAME2(1:KN2)//'\'//'ICE'//'.PLT'
!!        WRITE(*,*) PLTFILE(I)
!!      ENDDO
!
!      FNAME = 'ICE'
!      DO I = LEN(FNAME),1,-1
!       IF(FNAME(I:I).NE.' ') THEN
!        HDWFILE(1)= CASENAME2(1:KN2)//'\'//FNAME(1:I)//'.HDW'
!         PLTFILE(1)=CASENAME2(1:KN2)//'\'//FNAME(1:I)//'.PLT'
!         HOTFILE(1)=CASENAME2(1:KN2)//'\'//FNAME(1:I)//'.HOT'
!         STRFILE(1)=CASENAME2(1:KN2)//'\'//FNAME(1:I)//'.STR'
!!          PROFILE(1)='OUT\'//FNAME(1:I)//NSERIAL//'.DAT'
!        DO J=2,1000
!          K=J-1
!          IF (K.LT.10) THEN
!             J1=48
!             J2=48
!             J3=K+48
!          ELSEIF (K.LT.100) THEN
!             J1=48
!             J2=K/10+48
!             J3=K-K/10*10+48
!          ELSE
!             J1=K/100+48
!             J2=(K-K/100*100)/10+48
!             J3=MOD(K,10)+48
!          END IF
!          NSERIAL = CHAR(J1)//CHAR(J2)//CHAR(J3)
!         HDWFILE(J)=CASENAME2(1:KN2)//'\'//FNAME(1:I)//NSERIAL//'.HDW'
!         PLTFILE(J)=CASENAME2(1:KN2)//'\'//FNAME(1:I)//NSERIAL//'.PLT'
!         HOTFILE(J)=CASENAME2(1:KN2)//'\'//FNAME(1:I)//NSERIAL//'.HOT'
!         STRFILE(J)=CASENAME2(1:KN2)//'\'//FNAME(1:I)//NSERIAL//'.STR'
!!          PROFILE(J)='OUT\'//FNAME(1:I)//NSERIAL//'.DAT'
!         ENDDO
!
!         IF(NBM.GT.0.AND.ICEDYNAMICS) THEN
!           BFFILE = CASENAME2(1:KN2)//'\'//FNAME(1:I)//'.BMF'
!         ENDIF
!
!         IF(IFCLOGSWITCH) THEN
!!            CLGFILE = 'OUT\'//FNAME(1:I)//'.CLG'
!            CLGFILE = CASENAME2(1:KN2)//'\'//FNAME(1:I)//'.CLG'
!         ENDIF
!
!         GOTO 40
!       ENDIF
!      ENDDO
!  40  CONTINUE
!
!      IF(IFCLOGSWITCH) THEN
!        ICLG = 309
!        OPEN(ICLG,FILE = CLGFILE,STATUS = 'UNKNOWN')
!      ENDIF
!
!      IF(NBM.GT.0.AND.ICEDYNAMICS) THEN
!        IBMF = 310
!        OPEN(IBMF,FILE = BFFILE,STATUS = 'UNKNOWN')
!      ENDIF

!
!***********************************************************************
!
!-----------------------------------------------------------------------
!
!     PREVIOUS ICE COVER COMPUTATION FILE
!
      IF( ICE_FILES(ICECOV)%NAME.NE.' ' ) THEN
        CALL READ_DATASET(ICE_FILES(ICECOV)%FMT,ICE_FILES(ICECOV)%LU,
     &    VARSOR,NPOIN,RECORD,AT,TEXTPR,TROUVE,
     &    ALIRE,LISTIN,RECORD.EQ.0,MAXVAR)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISE ICE CHARACTERISATION (PRIME INTEGER)
      IF( TROUVE(21).EQ.1 ) THEN
        DO I = 1,NPOIN
          ICETYPE%I(I) = INT( ICETYPE%R(I)+1.D-3 )
        ENDDO
      ELSE
        ICETYPE%I = 1
      ENDIF
!     TODO: THE FOLLOWING CODE SHOULD BE REMOVED
!     IT IS INCLUDED ONLY TO CHECK THAT THERE IS NO DUPLICATION
!     BETWEEN THE WHAT IS LEFT IN ICE2D AND WHAT HAS COME INTO KHIONE
!***********************************************************************
!
!      CALL PREBORDERICE_ICE2D(NPOIN,NCBDY,ISBANK%I,JAMFEM%I,ISBORDER%I)
!
!***********************************************************************
!
!-----------------------------------------------------------------------
!
!     ADJUST INITIAL WATER DEPTH AS A RESULT OF ICE COVER
!
      CALL OS('X=X+CY  ', X=H, Y=THIFEMS, C=-RHO_ICE/RO0 )
      CALL OS('X=X+CY  ', X=H, Y=THIFEMF, C=-RHO_ICE/RO0 )
!
!     ADJUST INITIAL VELOCITY COMPONENTS AS A RESULT OF ICE COVER ?
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
