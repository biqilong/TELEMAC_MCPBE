!                       *****************
                        SUBROUTINE VOLFIN
!                       *****************
!
     & (W,DTN,FLUX,H,U,V,QU,QV,FLUSORT,FLUENT,SMH,MASSES,T,HTN,TN,
     &  MASSOU,FLUTENT,FLUTSOR,DJX,DJY,DX,DY,DSZ,FLBOR,LOGFR,FLUXT,
     &  FLUXT_OLD,FLUHTEMP,FLUHBTEMP,SMTR,DXT,DYT,DJXT,DJYT,T1,T2,T3,T4,
     &  T5,FLUX_OLD,MESH,MASS_RAIN,LEO,YASMO,DT,YASMH,CORR_I,CORR_J,
     &  CORR_ZL,CORR_ZR,CORR_HL,CORR_HR,CORR_UL,CORR_UR,CORR_VL,CORR_VR)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Solves the Shallow Water Equations system using finite volume
!!        schemes.
!
!>@history  N.GOUTAL; INRIA
!!        22/03/1998
!!
!!   ROE SCHEME (NG); KINETIC SCHEMES (INRIA)
!
!>@history  J-M HERVOUET (LNHE)
!!        05/09/2007
!!
!!   MULTIPLE TRACERS
!
!>@history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!!        13/07/2010
!!        V6P0
!!   Translation of French comments within the FORTRAN sources into
!!   English comments
!
!>@history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!!        21/08/2010
!!        V6P0
!!   Creation of DOXYGEN tags for automated documentation and
!!   cross-referencing of the FORTRAN sources
!
!>@history  R. ATA (EDF-LNHE)
!!        03/15/2011
!!        V6P1
!!    CHANGE EXPLICIT EULER BY NEWMARK SCHEME
!!    ADD TCHAMEN AND ZOKAGOA FLUXES
!
!>@history  R. ATA (EDF-LNHE)
!!        07/15/2012
!!        V6P2
!!     ADD HLLC AND WAF FLUXES
!
!>@history  R. ATA (EDF-LNHE)
!!
!!        01/07/2013
!!        V6P3
!!      adaptation with the new data structure (common with FEM)
!!      remove unused variables
!!      parallel version
!
!>@history  R. ATA
!!        28/01/2014
!!        V7P0
!!    change diemensions of CMI
!!    from (2,NSEG) to (NSEG,2)
!
!>@history S.PAVAN
!!        02/05/2014
!!        V7P0
!!    Initialization of flux_old
!!    for kinetic schemes
!
!>@history R. ATA (EDF R&D-LNHE)
!!        20/06/2014
!!        V7P0
!!    change winf values which are directly
!!    obtained by bord
!!    add parcom_bord after cdl routines
!!    change cdl routines to exactly impose boundary conditions
!!    initiliaze QU,QV and Hn
!!
!>@history R. ATA (EDF R&D-LNHE)
!!        20/01/2015
!!        V7P0
!!    correction for parallelization
!!    parcom_bord removed and parcom placed
!!    after cdl of each scheme
!
!>@history  R. ATA
!!        25/12/2016
!!        V7P2
!!    include rain and evaporation
!
!>@history  J,RIEHME (ADJOINTWARE)
!!        November 2016
!!        V7P2
!!     Replaced EXTERNAL statements to parallel functions / subroutines
!!     by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  CORR_I     SECOND ORDER CORRECTION FOR SLOPE ALONG X
!>@param  [in,out]  CORR_J     SECOND ORDER CORRECTION FOR SLOPE ALONG Y
!>@param  [in,out]  CORR_HL    SECOND ORDER CORRECTION FOR LEFT DEPTH
!>@param  [in,out]  CORR_HR    SECOND ORDER CORRECTION FOR RIGHT DEPTH
!>@param  [in,out]  CORR_UL    SECOND ORDER CORRECTION FOR LEFT X-VEL
!>@param  [in,out]  CORR_UR    SECOND ORDER CORRECTION FOR RIGHT X-VEL
!>@param  [in,out]  CORR_VL    SECOND ORDER CORRECTION FOR LEFT Y-VEL
!>@param  [in,out]  CORR_VR    SECOND ORDER CORRECTION FOR RIGHT Y-VEL
!>@param  [in,out]  CORR_ZL    SECOND ORDER CORRECTION FOR LEFT BOTTOM
!>@param  [in,out]  CORR_ZR    SECOND ORDER CORRECTION FOR RIGHT BOTTOM
!>@param  [in,out]  DJX        GRADIENT PER TRAINGLE ALONG X
!>@param  [in,out]  DJXT       GRADIENT PER TRAINGLE FOR TRACER ALONG X
!>@param  [in,out]  DJY        GRADIENT PER TRAINGLE ALONG Y
!>@param  [in,out]  DJYT       GRADIENT PER TRAINGLE FOR TRACER ALONG Y
!>@param  [in,out]  DSZ        VARIATION OF Z FOR ORDER 2
!>@param  [in,out]  DT         TIME STEP
!>@param  [in,out]  DTN        TIME STEP AT PREVIOUS ITERATION
!>@param  [in,out]  DX         WORKING TABLE
!>@param  [in,out]  DXT        GRADIENT AT NODE ALONG X
!>@param  [in,out]  DYT        GRADIENT AT NODE ALONG Y
!>@param  [in,out]  DY         WORKING TABLE
!>@param  [in,out]  FLBOR      BOUNDARY MASS FLUXES
!>@param  [in,out]  FLUHTEMP   FLUX FOR TRACER
!>@param  [in,out]  FLUHBTEMP  BOUNDARY FLUX FOR TRACER
!>@param  [in,out]  FLUENT     MASS FLUX INLET FROM TN TO TN+1
!>@param  [in,out]  FLUSORT    MASS FLUX OUTLET FROM TN TO TN+1
!>@param  [in,out]  FLUTENT    FLUX TRACER INLET
!>@param  [in,out]  FLUTSOR    FLUX TRACER OUTLET
!>@param  [in,out]  FLUX       FLUX AT TIME N
!>@param  [in,out]  FLUX_OLD   FLUX AT TIME N-1
!>@param  [in,out]  FLUXT      FLUX FOR TRACER AT TIME N
!>@param  [in,out]  FLUXT_OLD  FLUX FOR TRACER AT TIME N-1
!>@param  [in,out]  H          WATER DEPTH AT TIME N+1
!>@param  [in,out]  HTN        TRACER*DEPTH AT TIME N
!>@param  [in,out]  LEO        LOGICAL FOR GRAPHICAL OUTPUT
!>@param  [in,out]  LOGFR      REFERENCE OF BOUNDARY NODES
!>@param  [in,out]  MASSES     ADDED MASS BY SOURCE TERMS
!>@param  [in,out]  MASSOU     ADDED TRACER MASS BY SOURCE TERM
!>@param  [in,out]  MASS_RAIN  MASS ADDED BY RAIN OR EVAPORATION
!>@param  [in,out]  QU         FLOW COMPONENTS AT TN THEN AT TN+1
!>@param  [in,out]  QV         FLOW COMPONENTS AT TN THEN AT TN+1
!>@param  [in,out]  SMH        SOURCE TERMS FOR CONTINUITY EQUATION
!>@param  [in,out]  SMTR       SOURCE TERMS FOR TRACEUR
!>@param  [in,out]  T          TRACER UPDATED
!>@param  [in,out]  T1         WORKING TABLE
!>@param  [in,out]  T2         WORKING TABLE
!>@param  [in,out]  T3         WORKING TABLE
!>@param  [in,out]  T4         WORKING TABLE
!>@param  [in,out]  T5         WORKING TABLE
!>@param  [in,out]  TN         TRACER AT TIME N
!>@param  [in,out]  U          X-VELOCITY COMPONENTS AT TIME N+1
!>@param  [in,out]  V          Y-VELOCITY COMPONENTS AT TIME N+1
!>@param  [in,out]  W          WORKING TABLE
!>@param  [in]      YASMH      LOGICAL: TO TAKE INTO ACCOUNT SMH
!>@param  [in]      YASMO      IF TRUE FU AND FV ARE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_VOLFIN => VOLFIN
      USE DECLARATIONS_TELEMAC2D, ONLY : AT,LT,BILMAS,CFLWTD,
     &                            FCOR,DIFVIT,DTVARI,FU,FV,GRAV,
     &                            HBOR,HN,HROPT,ITURB,KFROT,
     &                            LIMPRO,LIMTRA,ENTET,MAXSCE,
     &                            MAXTRA,NPOIN,NPTFR,NREJET,ISCE,NTRAC,
     &                            OPTVF,PLUIE,PROPNU,RAIN,NSEG,
     &                            SPHERI,TBOR,TMAX,UBOR,VBOR,CORIOL,
     &                            ZF,NREG,TNP,PT_IN_POLY,MXPTVS,
     &                            DIFT,GAMMA,TSCE2,DIFNU,HC,NEISEG,
     &                            V2DPAR,ICIN,ILIMHZ,ILIMT,ILIMUV,
     &                            SORDER,TORDER,DTINI,LEOPRD,NELEM,
     &                            NELMAX,IKLE,X,Y,EPS_FV
      USE DECLARATIONS_TELEMAC, ONLY : KDDL,KDIR,KNEU
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DMIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT) :: LOGFR(*)
!
      LOGICAL, INTENT(INOUT)    :: LEO,YASMO
      LOGICAL, INTENT(IN)       :: YASMH
      DOUBLE PRECISION, INTENT(INOUT) :: T1(*),T2(*),T3(*),T4(*),T5(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DT,DTN,MASS_RAIN
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN),SMH(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),FLUX_OLD(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSORT,FLUENT,MASSES
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DXT(NPOIN),DYT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJYT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NPOIN),DY(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,*)
!
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: T,HTN,TN
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUXT,FLUXT_OLD
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUHTEMP,FLUHBTEMP,SMTR
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: CORR_I,CORR_J,CORR_ZL,CORR_ZR
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: CORR_HL,CORR_HR,CORR_UL,CORR_UR
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: CORR_VL,CORR_VR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVIS
      DOUBLE PRECISION BETA,GPRDTIME
!
!-----------------------------------------------------------------------
!
      CALL INIT_FV(GPRDTIME,ICIN,QU,QV,FLUX_OLD,W,IVIS,NEISEG%I,CORR_I,
     &             CORR_J,CORR_HL,CORR_HR,CORR_UL,CORR_UR,CORR_VL,
     &             CORR_VR,CORR_ZL,CORR_ZR)
!
      IF(NTRAC.GT.0) THEN
        CALL INIT_TRAC(T,HTN,SMTR,FLUXT,FLUHTEMP,FLUHBTEMP,MASSOU,
     &                 FLUTENT,FLUTSOR,FLBOR,MESH)
      ENDIF
!
      CALL CALDT(DT,DTN,LEO)
!
      IF(SORDER.EQ.2) THEN
        CALL SECOND_ORDER(DSZ,BETA,T1,T2,T3,T4,T5,LOGFR,W,DJX,DJY,DX,DY,
     &                    FLUX,DT,CORR_I,CORR_J,CORR_HL,CORR_HR,CORR_UL,
     &                    CORR_UR,CORR_VL,CORR_VR,CORR_ZL,CORR_ZR,IVIS,
     &                    MESH%AIRST%R,MESH%VNOIN%R,MESH%ELTSEG%I,
     &                    MESH%IFABOR%I,MESH%NUBO%I,MESH%CMI%R,
     &                    LIMPRO%I,HC%R)
      ENDIF
!
      CALL FLUX_FV(W,LIMPRO%I,MESH%NUBO%I,MESH%VNOIN%R,FLUX,FLUSORT,
     &             FLUENT,FLBOR,MESH%ELTSEG%I,MESH%IFABOR%I,DT,
     &             FLUHBTEMP,FLUHTEMP,LEO,GPRDTIME,NEISEG%I)
!
      IF(NTRAC.GT.0) THEN
        CALL FLUX_TRAC(MESH%NUBO%I,IKLE%I,FLUTENT,FLUTSOR,
     &                 MESH%CMI%R,DJXT,DJYT,DXT,DYT,MESH%DPX%R,
     &                 MESH%DPY%R,BETA,DSZ,MESH%AIRST%R,HC%R,FLUXT,
     &                 MESH%ELTSEG%I,MESH%IFABOR%I,MESH%VNOIN%R)
      ENDIF
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,QU,QV,LIMPRO%I,T)
!
      CALL SOURCE_MOMENT(W,YASMO)
!
      CALL FV_BALANCE(MASSES,MASS_RAIN,YASMH,SMH,H,QU,QV,FLUX,FLUX_OLD,
     &              W,U,V,T,FLUXT_OLD,MASSOU,TN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
