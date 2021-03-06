!                   ************************
                    SUBROUTINE POINT_TOMAWAC
!                   ************************
!
!
!***********************************************************************
! TOMAWAC   V7P0
!***********************************************************************
!
!brief    ALLOCATES MEMORY.
!
!history  MICHEL BENOIT (EDF R&D LNHE)
!+        06/12/2004
!+        V6P0
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
!history  G.MATTAROLO (EDF)
!+        16/05/2011
!+        V6P1
!+   Memory allocation for new variables defined by
!+       E. GAGNAIRE-RENOU for solving non linear source terms models
!+       (MDIA and GQM methods)
!
!history  G.MATTAROLO (EDF)
!+        25/06/2012
!+        V6P2
!+   Memory allocation for variables used for diffraction
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        06/11/2014
!+        V7P0
!+   Conditional allocation of SUC1, SVC1, SUC2, SVC2 changed because
!+   they are used by lecsui.f in case of restart.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX
      IMPLICIT NONE
!
!
      INTEGER IELBT,IELB1,CFG(2),NC,NS
!
!***********************************************************************
!
      WRITE(LU,40)
40    FORMAT(1X,///,21X,'*******************************',/,
     &21X,              '*     MEMORY ORGANISATION     *',/,
     &21X,              '*******************************',/)
!
!-----------------------------------------------------------------------
!
      IELM2 = 11
      IELM3 = 41
      IELB1 = IELBOR(IELM2,1)
      CFG(1) = 1
      CFG(2) = 1
!
!-----------------------------------------------------------------------
!
!     ALLOCATES THE 2D MESH STRUCTURE
!
      CALL ALMESH(MESH,'MESH  ',IELM2,SPHE,CFG, FMTGEO, LUGEO,
     &            EQUA,0)
!
!     ALLOCATES THE 3D MESH STRUCTURE
!
      CALL ALMESH(MESH3D,'MESH3D',IELM3,SPHE,CFG, FMTGEO, LUGEO,
     &            EQUA,0,NPLAN=NDIRE)
!
!     ALIAS FOR CERTAIN COMPONENTS OF MESH
!
      IKLE2  => MESH%IKLE%I
      IFABOR => MESH%IFABOR%I
      NBOR   => MESH%NBOR%I
!
      X     => MESH%X%R
      Y     => MESH%Y%R
      XEL   => MESH%XEL%R
      YEL   => MESH%YEL%R
      SURDET=> MESH%SURDET%R
!
      NELEM2=>MESH%NELEM
      NPOIN2=>MESH%NPOIN
      NPTFR =>MESH%NPTFR
!
      IELBT = IELBOR(IELM2,1)
!
      NPOIN3=NPOIN2*NDIRE
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) THEN
        CALL BIEF_ALLVEC(2,IKLE_EXT,'IK_EXT',P_MAX(NELEM2),3,0,MESH)
      ELSE
!       HERE POINTING IKLE_EXT%I ON IKLE2 WOULD WORK ALSO...
        CALL BIEF_ALLVEC(2,IKLE_EXT,'IK_EXT',NELEM2,3,0,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     VARIABLES 4D TO ADVECT
!
      CALL BIEF_ALLVEC(1,SF,'SF    ',NPOIN3*NF , 1 , 0 ,MESH)
!
!     COEFFICIENT B FOR ADVECTION
!
      CALL BIEF_ALLVEC(1,SB,'SB    ',NPOIN2*NF , 1 , 0 ,MESH)
!
!     ARRAY OF DISCRETISED FREQUENCIES, AND OF DELTA F
!
      CALL BIEF_ALLVEC(1,SFR,'SFR   ' ,NF , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SDFR,'SDFR  ',NF , 1 , 0 ,MESH)
      FREQ     =>SFR%R
      DFREQ    =>SDFR%R
!
!     ARRAY OF VARIANCE DENSITY FREQUENCY SPECTRUM
!
      CALL BIEF_ALLVEC(1,SSPEC,'SSPEC  ' ,NF , 1 , 0 ,MESH)
      SPEC     =>SSPEC%R
!
!     ARRAY OF DIRECTIONAL SPREADING FUNCTION VALUES
!
      CALL BIEF_ALLVEC(1,SFRA,'SFRA  ',NDIRE , 1 , 0 ,MESH)
      FRA    =>SFRA%R
!
!     "PHYSICAL" VARIABLES OF SIZE NPOIN3
!
      CALL BIEF_ALLVEC(1,SXK,'SXK   ',NPOIN2*NF, 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SCG,'SCG   ',NPOIN2*NF, 1 , 0 ,MESH)
!
!     FOR SOURCE TERMS (BUT ALWAYS ALLOCATED, USED AS WORK ARRAYS)
!
      CALL BIEF_ALLVEC(1,STSDER ,'STSDER' ,NF*NPOIN3 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STSTOT ,'STSTOT' ,NF*NPOIN3 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SDF_LIM,'SDF_LIM',IELM2     , 1 , 2 ,MESH)

      IF (POROUS) THEN
        CALL BIEF_ALLVEC(1,SAMORP,'SAMORP',NPOIN2*NF, 1 , 0 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SAMORP,'SAMORP',1,1,0,MESH)
      ENDIF
      AMORP => SAMORP%R

!
      TSDER   => STSDER%R
      TSTOT   => STSTOT%R
      DF_LIM  => SDF_LIM%R
!
!     FOR THE BOUNDARY CONDITIONS
!
      CALL BIEF_ALLVEC(1,SFBOR,'SFBOR ',IELBT, NDIRE*NF , 2 ,MESH)
!
!     ARRAYS FOR NON-LINEAR INTERACTIONS
!
      IF(STRIF.EQ.1) THEN
        CALL BIEF_ALLVEC(1,SCOEF,'SCOEF ',16   , 1 , 0 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SCOEF ,'SCOEF ', 1, 1, 0 ,MESH)
      ENDIF
      COEFNL   =>SCOEF%R
!
!     ADVECTION FIELD
!
      IF(COUSTA .OR. MAREE.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
        NC=NPOIN3*NF
        NS=NPOIN3*NF
      ELSE
        NS=NPOIN3*NF
        NC=NPOIN3
      ENDIF
!
      F     =>SF%R
      B     =>SB%R
      XK    =>SXK%R
      CG    =>SCG%R
      FBOR  =>SFBOR%R
!
      CALL ALLBLO(SSHP1,'SSHP1 ')
      CALL ALLBLO(SSHZ ,'SSHZ  ')
      CALL ALLBLO(SSHF ,'SSHF  ')
!
      IF(PROP) THEN
!       FOOT OF THE CHARACTERISTICS
        CALL BIEF_ALLVEC_IN_BLOCK(SSHP1,NF,1,'SHP   ',NPOIN3,3,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(SSHZ ,NF,1,'SHZ   ',NPOIN3,1,0,MESH)
        CALL BIEF_ALLVEC(1,SCT,'SCT   ',NC , 1 , 0 ,MESH)
        IF(COURAN.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
          CALL BIEF_ALLVEC_IN_BLOCK(SSHF,NF,1,'SHF   ',NPOIN3,1,0,MESH)
          CALL BIEF_ALLVEC(1,SCF ,'SCF   ',NC , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(SSHF ,NF,1,'SHF   ',1,1,0,MESH)
          CALL BIEF_ALLVEC(1,SCF   ,'SCF   ', 1, 1, 0 ,MESH)
        ENDIF
      ELSE
        CALL BIEF_ALLVEC_IN_BLOCK(SSHP1,NF,1,'SHP   ',1,3,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(SSHZ ,NF,1,'SHZ   ',1,1,0,MESH)
        CALL BIEF_ALLVEC(1,SCT   ,'SCT   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(SSHF ,NF,1,'SHF   ',1,1,0,MESH)
        CALL BIEF_ALLVEC(1,SCF   ,'SCF   ', 1, 1, 0 ,MESH)
      ENDIF
!
      SHZ   =>SSHZ%R
      CT    =>SCT%R
      SHF   =>SSHF%R
      CF    =>SCF%R
!
! ARRAYS OF SIZE NPOIN2
!
      CALL BIEF_ALLVEC(1,SZF,'SZF   ',IELM2 , 1 , 2 ,MESH)
      ZF    =>SZF%R
      CALL BIEF_ALLVEC(1,SDEPTH,'SDEPTH',IELM2 , 1 , 2 ,MESH)
      DEPTH =>SDEPTH%R
!
      CALL BIEF_ALLVEC(1,SBETBR,'SBETBR',IELM2,1,2,MESH)
      BETABR => SBETBR%R
!
      IF(.NOT.PROINF) THEN
        CALL BIEF_ALLVEC(1,SDZX  ,'SDZX  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SDZY  ,'SDZY  ',IELM2 , 1 , 2 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SDZX  ,'SDZX  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SDZY  ,'SDZY  ', 1, 1, 0 ,MESH)
      ENDIF
      DZX     =>SDZX%R
      DZY     =>SDZY%R
!
!     NAMECODE IS IN DECLARATIONS_TELEMAC AND IS MONITORED BY
!     SUBROUTINE CONFIG_CODE
!
      CALL BIEF_ALLVEC(1,SDZHDT,'SDZHDT',IELM2 , 1 , 2 ,MESH)
!
      IF(COURAN.OR.DONTEL.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
        CALL BIEF_ALLVEC(1,SUC ,'SUC   ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SVC ,'SVC   ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SDUX,'SDUX  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SDUY,'SDUY  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SDVX,'SDVX  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SDVY,'SDVY  ',IELM2 , 1 , 2 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SUC   ,'SUC   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SVC   ,'SVC   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SDUX  ,'SDUX  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SDUY  ,'SDUY  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SDVX  ,'SDVX  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SDVY  ,'SDVY  ', 1, 1, 0 ,MESH)
      ENDIF
      UC      =>SUC%R
      VC      =>SVC%R
      DUX     =>SDUX%R
      DUY     =>SDUY%R
      DVX     =>SDVX%R
      DVY     =>SDVY%R
!
      IF(COURAN.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
        CALL BIEF_ALLVEC(1,SVC1,'SVC1  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SUC1,'SUC1  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SVC2,'SVC2  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SUC2,'SUC2  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SZM1,'SZM1  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SZM2,'SZM2  ',IELM2 , 1 , 2 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SVC1  ,'SVC1  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SUC1  ,'SUC1  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SVC2  ,'SVC2  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SUC2  ,'SUC2  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SZM1  ,'SZM1  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SZM2  ,'SZM2  ', 1, 1, 0 ,MESH)
      ENDIF
      DZHDT   =>SDZHDT%R
      UC1     =>SUC1%R
      VC1     =>SVC1%R
      UC2     =>SUC2%R
      VC2     =>SVC2%R
      ZM1     =>SZM1%R
      ZM2     =>SZM2%R
!
      IF (VENT) THEN
        CALL BIEF_ALLVEC(1,SUV,'SUV   ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SVV,'SVV   ',IELM2 , 1 , 2 ,MESH)
        IF (NAMVEB.NE.' '.OR.NAMVEB.NE.' ') THEN
          CALL BIEF_ALLVEC(1,SVV1,'SVV1  ',IELM2 , 1 , 2 ,MESH)
          CALL BIEF_ALLVEC(1,SUV1,'SUV1  ',IELM2 , 1 , 2 ,MESH)
          CALL BIEF_ALLVEC(1,SVV2,'SVV2  ',IELM2 , 1 , 2 ,MESH)
          CALL BIEF_ALLVEC(1,SUV2,'SUV2  ',IELM2 , 1 , 2 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,SVV1  ,'SVV1  ', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,SUV1  ,'SUV1  ', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,SVV2  ,'SVV2  ', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,SUV2  ,'SUV2  ', 1, 1, 0 ,MESH)
        ENDIF
      ELSE
        CALL BIEF_ALLVEC(1,SUV   ,'SUV   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SVV   ,'SVV   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SVV1  ,'SVV1  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SUV1  ,'SUV1  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SVV2  ,'SVV2  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SUV2  ,'SUV2  ', 1, 1, 0 ,MESH)
      ENDIF
      UV      =>SUV%R
      VV      =>SVV%R
      VV1     =>SVV1%R
      UV1     =>SUV1%R
      VV2     =>SVV2%R
      UV2     =>SUV2%R
!
      IF(SPHE) THEN
        CALL BIEF_ALLVEC(1,SCOSF,'SCOSF ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,STGF,'STGF  ',IELM2 , 1 , 2 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SCOSF ,'SCOSF ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,STGF  ,'STGF  ', 1, 1, 0 ,MESH)
      ENDIF
      COSF    =>SCOSF%R
      TGF     =>STGF%R
!
!
!
!
!     ARRAYS WITH THE RELATIVE POSITIONS OF THE DIRECTION PLANES,
!     AND WITH THE COS AND SIN TETA
      CALL BIEF_ALLVEC(1,STETA,'STETA ',NDIRE+1 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SCOSTE,'SCOSTE',NDIRE , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SSINTE,'SSINTE',NDIRE , 1 , 0 ,MESH)
      TETA     =>STETA%R
      COSTET   =>SCOSTE%R
      SINTET   =>SSINTE%R
!
!     POINTERS FOR WORKING ARRAYS (BY POINTS AND ELEMENTS)
!
      CALL BIEF_ALLVEC(1,ST0, 'ST0   ',IELM2 , 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ST1, 'ST1   ',IELM2 , 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ST2, 'ST2   ',IELM2 , 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ST3, 'ST3   ',IELM2 , 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ST4, 'ST4   ',IELM2 , 1 , 2 ,MESH)
      T0      =>ST0%R
      T1      =>ST1%R
      T2      =>ST2%R
      T3      =>ST3%R
      T4      =>ST4%R
!
!     POINTERS FOR MATRICES, AM1 SYMMETRICAL MATRIX
      CALL BIEF_ALLMAT(AM1,'AM1   ',IELM2,IELM2,CFG,'Q','S',MESH)
!
!     VARIOUS WORK ARRAYS
!
      CALL BIEF_ALLVEC(1,STRA31,'STRA31',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA32,'STRA32',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA34,'STRA34',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA35,'STRA35',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA36,'STRA36',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA37,'STRA37',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA38,'STRA38',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA39,'STRA39',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STAUWA,'STAUWA',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SUSOLD,'SUSOLD',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SUSNEW,'SUSNEW',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SFMOY ,'SFMOY ',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SVARIA,'SVARIA',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SXKMOY,'SXKMOY',NPOIN2 , 1 , 0 ,MESH)
      TRA31   => STRA31%R
      TRA32   => STRA32%R
      TRA34   => STRA34%R
      TRA35   => STRA35%R
      TRA36   => STRA36%R
      TRA37   => STRA37%R
      TRA38   => STRA38%R
      TRA39   => STRA39%R
      TAUWAV  => STAUWA%R
      USOLD   => SUSOLD%R
      USNEW   => SUSNEW%R
      FMOY    => SFMOY%R
      XKMOY   => SXKMOY%R
      VARIAN  => SVARIA%R
!
!     VARIOUS WORK ARRAYS
!
      CALL BIEF_ALLVEC(1,STRA01,'STRA01',NPOIN3,6,0,MESH)
      TRA01   =>STRA01%R
!
      IF(TSOU) THEN
        CALL BIEF_ALLVEC(1,STOLD,'STOLD ',NPOIN3 , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,STNEW,'STNEW ',NPOIN3 , 1 , 0 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,STOLD ,'STOLD ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,STNEW ,'STNEW ', 1, 1, 0 ,MESH)
      ENDIF
      TOLD    => STOLD%R
      TNEW    => STNEW%R
!
      CALL BIEF_ALLVEC(1,STWOLD,'STWOLD',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SZ0OLD,'SZ0OLD',NPOIN2 , 1 , 0 ,MESH)
      TWOLD   => STWOLD%R
      Z0OLD   => SZ0OLD%R
!
!     USER DEDICATED ARRAY (2-DIMENSIONAL * NPRIV)
!
      CALL BIEF_ALLVEC(1,SPRIVE,'SPRIVE',NPOIN2*NPRIV,1,0,MESH)
      PRIVE => SPRIVE%R
!
!     ADDED FOR CHARACTERISTICS
!
      CALL ALLBLO(TB, 'TB    ')
      CALL BIEF_ALLVEC_IN_BLOCK(TB,10,1,'TB    ',IELM3,1,2,MESH3D)
!
      T3_01 => TB%ADR(01)%P
      T3_02 => TB%ADR(02)%P
      T3_03 => TB%ADR(03)%P
      T3_04 => TB%ADR(04)%P
      T3_05 => TB%ADR(05)%P
      T3_06 => TB%ADR(06)%P
      T3_07 => TB%ADR(07)%P
      T3_08 => TB%ADR(08)%P
      T3_09 => TB%ADR(09)%P
      T3_10 => TB%ADR(10)%P
!
!     ONLY FOR OUTPUTS, ALLOCATED WITHOUT MEMORY, THE MEMORY WILL
!     BE TAKEN IN ARRAYS OF BLOCK TB
!
      CALL BIEF_ALLVEC(1,FORCEX,'FORCEX',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,FORCEY,'FORCEY',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,CONTXX,'CONTXX',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,CONTXY,'CONTXY',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,CONTYY,'CONTYY',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,PFREA5,'PFREA5',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,PFREA8,'PFREA8',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SCDRA2,'SCDRA2',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SVIFON,'SVIFON',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SPOWER,'SPOWER',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SPTMOY,'SPTMOY',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SPTM01,'SPTM01',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SPTM02,'SPTM02',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SPPTPD,'SPPTPD',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,PPREA5,'PPREA5',1, 1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,PPREA8,'PPREA8',1, 1, 0 ,MESH)
!
!     NOW MEMORY TAKEN IN TB
!
      DEALLOCATE(FORCEX%R)
      FORCEX%R=>T3_01%R(1:NPOIN2)
      DEALLOCATE(FORCEY%R)
      FORCEY%R=>T3_02%R(1:NPOIN2)
      DEALLOCATE(CONTXX%R)
      CONTXX%R=>T3_03%R(1:NPOIN2)
      DEALLOCATE(CONTXY%R)
      CONTXY%R=>T3_04%R(1:NPOIN2)
      DEALLOCATE(CONTYY%R)
      CONTYY%R=>T3_05%R(1:NPOIN2)
      DEALLOCATE(PFREA5%R)
      PFREA5%R=>T3_06%R(1:NPOIN2)
      DEALLOCATE(PFREA8%R)
      PFREA8%R=>T3_07%R(1:NPOIN2)
      DEALLOCATE(SCDRA2%R)
      SCDRA2%R=>T3_08%R(1:NPOIN2)
      DEALLOCATE(SVIFON%R)
      SVIFON%R=>T3_09%R(1:NPOIN2)
      DEALLOCATE(SPOWER%R)
      SPOWER%R=>T3_10%R(1:NPOIN2)
      DEALLOCATE(SPTMOY%R)
      SPTMOY%R=>T3_01%R(NPOIN2+1:2*NPOIN2)
      DEALLOCATE(SPTM01%R)
      SPTM01%R=>T3_02%R(NPOIN2+1:2*NPOIN2)
      DEALLOCATE(SPTM02%R)
      SPTM02%R=>T3_03%R(NPOIN2+1:2*NPOIN2)
      DEALLOCATE(SPPTPD%R)
      SPPTPD%R=>T3_04%R(NPOIN2+1:2*NPOIN2)
      DEALLOCATE(PPREA5%R)
      PPREA5%R=>T3_05%R(NPOIN2+1:2*NPOIN2)
      DEALLOCATE(PPREA8%R)
      PPREA8%R=>T3_06%R(NPOIN2+1:2*NPOIN2)
!
      FX    => FORCEX%R
      FY    => FORCEY%R
      SXX   => CONTXX%R
      SXY   => CONTXY%R
      SYY   => CONTYY%R
      FREA5   => PFREA5%R
      FREA8   => PFREA8%R
      CDRA2   => SCDRA2%R
      VIFOND  => SVIFON%R
      POWER   => SPOWER%R
      PTMOY   => SPTMOY%R
      PTM01   => SPTM01%R
      PTM02   => SPTM02%R
      PPTPD   => SPPTPD%R
      PREA5   => PPREA5%R
      PREA8   => PPREA8%R
!
!     BLOCK FOR GRAPHICAL OUTPUTS: VARSOR
!
      CALL ALLBLO(VARSOR,'VARSOR')
!     1: M0
      CALL ADDBLO(VARSOR,SVARIA)
!     2: HM0
      CALL ADDBLO(VARSOR,STRA38)
!     3: MEAN DIRECTION
      CALL ADDBLO(VARSOR,STRA32)
!     4:
      CALL ADDBLO(VARSOR,STRA31)
!     5:
      CALL ADDBLO(VARSOR,SZF)
!     6:
      CALL ADDBLO(VARSOR,SDEPTH)
!     7:
      CALL ADDBLO(VARSOR,SUC)
!     8:
      CALL ADDBLO(VARSOR,SVC)
!     9:
      CALL ADDBLO(VARSOR,SUV)
!     10:
      CALL ADDBLO(VARSOR,SVV)
!     11:
      CALL ADDBLO(VARSOR,FORCEX)
!     12:
      CALL ADDBLO(VARSOR,FORCEY)
!     13:
      CALL ADDBLO(VARSOR,CONTXX)
!     14:
      CALL ADDBLO(VARSOR,CONTXY)
!     15:
      CALL ADDBLO(VARSOR,CONTYY)
!     16:
      CALL ADDBLO(VARSOR,SVIFON)
!     17:
      CALL ADDBLO(VARSOR,SPRIVE)
!     18:
      CALL ADDBLO(VARSOR,SFMOY)
!     19:
      CALL ADDBLO(VARSOR,STRA34)
!     20:
      CALL ADDBLO(VARSOR,STRA35)
!     21:
      CALL ADDBLO(VARSOR,STRA36)
!     22: FPR5
      CALL ADDBLO(VARSOR,PFREA5)
!     23: FPR8
      CALL ADDBLO(VARSOR,PFREA8)
!     24: USTAR
      CALL ADDBLO(VARSOR,SUSOLD)
!     25: CDRAG
      CALL ADDBLO(VARSOR,SCDRA2)
      CALL ADDBLO(VARSOR,SZ0OLD)
      CALL ADDBLO(VARSOR,STAUWA)
      CALL ADDBLO(VARSOR,SPTMOY)
      CALL ADDBLO(VARSOR,SPTM01)
      CALL ADDBLO(VARSOR,SPTM02)
      CALL ADDBLO(VARSOR,SPPTPD)
      CALL ADDBLO(VARSOR,PPREA5)
      CALL ADDBLO(VARSOR,PPREA8)
      CALL ADDBLO(VARSOR,SPOWER)
!     VARIABLE 35
      CALL ADDBLO(VARSOR,SBETBR)
!
!.....BLOCK FOR VALIDATION
!
      IF(VALID) THEN
!
!       MAKE SURE THIS IS CONSISTENT WITH THE ALIRE VECTOR
!       DECLARED IN DECLARATIONS_TOMAWAC.F
!       7 VARIABLES HAVE BEEN USED FOR VALIDATION
!          SIGNIFICANT WAVE HEIGHT    HM0       ( 2)
!          MEAN DIRECTION             DMOY      ( 3)
!          DIRECTIONAL SPREADING      SPD       ( 4)
!          DRIVING FORCE ALONG X      FX        (11)
!          DRIVING FORCE ALONG Y      FY        (12)
!          MEAN FREQUENCY FM-10       FMOY      (18)
!          MEAN FREQUENCY FM01        FM01      (19)
!
        CALL ALLBLO(BST1,'BST1  ')
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST1)
        CALL ADDBLO(BST1,ST2)
        CALL ADDBLO(BST1,ST3)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST4)
        CALL ADDBLO(BST1,T3_05)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,T3_06)
        CALL ADDBLO(BST1,T3_07)
      ENDIF
!
!V6P2 Diffraction : allocation of bief objects if diffraction
!                   is taken into account
      IF(DIFFRA.GT.0) THEN
        CALL BIEF_ALLVEC(1,SA      ,'SA    ', IELM2, 1, 2, MESH)
        CALL BIEF_ALLVEC(1,SCCG    ,'SCCG  ', IELM2, 1, 2, MESH)
        CALL BIEF_ALLVEC(1,SDIV    ,'SDIV  ', IELM2, 1, 2, MESH)
        CALL BIEF_ALLVEC(1,SDELTA  ,'SDELTA', IELM2, 1, 2, MESH)
        CALL BIEF_ALLVEC(1,SDDX    ,'SDDX  ', IELM2, 1, 2, MESH)
        CALL BIEF_ALLVEC(1,SDDY    ,'SDDY  ', IELM2, 1, 2, MESH)
        CALL BIEF_ALLVEC(1,SA_RMSE ,'SA_RMSE',IELM2, 1, 2, MESH)
        CALL BIEF_ALLVEC(1,SXKONPT ,'SXKONPT',IELM2, 1, 2, MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SA      ,'SA    ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SCCG    ,'SCCG  ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SDIV    ,'SDIV  ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SDELTA  ,'SDELTA', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SDDX    ,'SDDX  ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SDDY    ,'SDDY  ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SA_RMSE ,'SA_RMSE',1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SXKONPT ,'SXKONPT',1, 1, 0, MESH)
      ENDIF
!
      AMPLI     =>SA%R
      CCG       =>SCCG%R
      DIV       =>SDIV%R
      DELTA     =>SDELTA%R
      DDX       =>SDDX%R
      DDY       =>SDDY%R
      A_RMSE    =>SA_RMSE%R
      XKONPT    =>SXKONPT%R
!
      NRK_C=NPOIN2*MAXNSP
      IF(DIFFRA.GT.0) THEN
        CALL BIEF_ALLVEC(1,SRK  ,'SRK   ', NRK_C, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SRX  ,'SRX   ', NRK_C, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SRY  ,'SRY   ', NRK_C, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SRXX ,'SRXX  ', NRK_C, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SRYY ,'SRYY  ', NRK_C, 1, 0, MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SRK  ,'SRK   ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SRX  ,'SRX   ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SRY  ,'SRY   ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SRXX ,'SRXX  ', 1, 1, 0, MESH)
        CALL BIEF_ALLVEC(1,SRYY ,'SRYY  ', 1, 1, 0, MESH)
      ENDIF
!
      RK   => SRK%R
      RX   => SRX%R
      RY   => SRY%R
      RXX   => SRXX%R
      RYY   => SRYY%R
!V6P2 End diffraction
!-----------------------------------------------------------------------
!
!                     **********************
!                     * POINTER: ARRAY IA*
!                     **********************
!
!-----------------------------------------------------------------------
!
!
      CALL BIEF_ALLVEC(2,SLIFBR,'SLIFBR',IELB1, 1 , 1 ,MESH)
      CALL BIEF_ALLVEC(2,SIBOR,'SIBOR ',NELEM2, 7 , 0 ,MESH)
      CALL BIEF_ALLVEC(2,SBOUNDARY_COLOUR,'BNDCOL',IELB1,1,1,MESH)
      LIFBOR  => SLIFBR%I
      IBOR    => SIBOR%I
      BOUNDARY_COLOUR => SBOUNDARY_COLOUR%I

!
! FOOT OF THE CHARACTERISTICS
!
      IF(PROP) THEN
        CALL BIEF_ALLVEC(2,SELT ,'SELT  ',NS , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(2,SETA ,'SETA  ',NS , 1 , 0 ,MESH)
        IF(NCSIZE.GT.1) THEN
          CALL BIEF_ALLVEC(2,SISUB,'SISUB ',NS , 1, 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(2,SISUB,'SISUB ', 1 , 1, 0 ,MESH)
        ENDIF
        IF (COURAN.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
          CALL BIEF_ALLVEC(2,SFRE,'SFRE  ',NS , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(2,SFRE  ,'SFRE  ', 1, 1, 0 ,MESH)
        ENDIF
      ELSE
        CALL BIEF_ALLVEC(2,SELT  ,'SELT  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(2,SETA  ,'SETA  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(2,SISUB ,'SISUB ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(2,SFRE  ,'SFRE  ', 1, 1, 0 ,MESH)
      ENDIF
      ELT   => SELT%I
      ETA   => SETA%I
      ISUB  => SISUB%I
      FRE   => SFRE%I
!
! WORKING ARRAYS USED IN THE CALL TO INBIEF AND INIPIE
!
      CALL BIEF_ALLVEC(2,SITR31,'SITR31',IELM2 , 1 , 1 ,MESH)
      CALL BIEF_ALLVEC(2,SITR32,'SITR32',IELM2 , 1 , 1 ,MESH)
      CALL BIEF_ALLVEC(2,SITR33,'SITR33',IELM2 , 1 , 1 ,MESH)
      ITR31   => SITR31%I
      ITR32   => SITR32%I
      ITR33   => SITR33%I
!
!     WORKING ARRAYS OF INTEGERS
!
      CALL BIEF_ALLVEC(2,SITR01,'SITR01',NPOIN3 ,3,0,MESH)
      ITR01 => SITR01%I
!
!.....NON-LINEAR INTERACTIONS
!
      IF(STRIF.EQ.1) THEN
        CALL BIEF_ALLVEC(2,SIAGNL,'SIAGNL',8*NDIRE,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,SIAGNL,'SIAGNL',1,1,0,MESH)
      ENDIF
      IANGNL => SIAGNL%I
!
!GM V6P1 - NEW SOURCE TERMS
!............MDIA method
!
      IF(STRIF.EQ.2) THEN
        ALLOCATE(XLAMDI(1:MDIA))
        ALLOCATE(XMUMDI(1:MDIA))
        ALLOCATE(IANMDI(1:NDIRE,1:16,1:MDIA))
        ALLOCATE(COEMDI(1:32,1:MDIA))
      ELSE
        ALLOCATE(XLAMDI(1))
        ALLOCATE(XMUMDI(1))
        ALLOCATE(IANMDI(1,1,1))
        ALLOCATE(COEMDI(1,1))
      ENDIF
!............GQM method
      IF(STRIF.EQ.3) THEN
        IF(IQ_OM1.EQ.1) THEN
          NF1=14
        ELSEIF (IQ_OM1.EQ.2) THEN
          NF1=26
        ELSEIF (IQ_OM1.EQ.3) THEN
          NF1=11
        ELSEIF (IQ_OM1.EQ.4) THEN
          NF1=40
        ELSEIF (IQ_OM1.EQ.7) THEN
          NF1=20
        ELSE
          WRITE(LU,*) 'ARRET DANS POINT : VALEUR INCONNUE DE IQ_OM1'
          CALL PLANTE(1)
          STOP
        ENDIF
        NT1=2*NQ_TE1
        NF2=NQ_OM2
        NCONFM=NQ_OM2*NT1*NF1
        ALLOCATE(K_IF1 (1:NF1))
        ALLOCATE(K_IF2 (1:NF2,1:NT1,1:NF1),K_IF3 (1:NF2,1:NT1,1:NF1))
        ALLOCATE(K_1P  (1:NT1,1:NF1)      ,K_1M  (1:NT1,1:NF1))
        ALLOCATE(K_1P2P(1:NF2,1:NT1,1:NF1),K_1P3M(1:NF2,1:NT1,1:NF1))
        ALLOCATE(K_1P2M(1:NF2,1:NT1,1:NF1),K_1P3P(1:NF2,1:NT1,1:NF1))
        ALLOCATE(K_1M2P(1:NF2,1:NT1,1:NF1),K_1M3M(1:NF2,1:NT1,1:NF1))
        ALLOCATE(K_1M2M(1:NF2,1:NT1,1:NF1),K_1M3P(1:NF2,1:NT1,1:NF1))
        ALLOCATE(TB_V14(1:NF1))
        ALLOCATE(TB_V24(1:NF2,1:NT1,1:NF1),TB_V34(1:NF2,1:NT1,1:NF1))
        ALLOCATE(TB_TPM(1:NF2,1:NT1,1:NF1),TB_TMP(1:NF2,1:NT1,1:NF1))
        ALLOCATE(TB_FAC(1:NF2,1:NT1,1:NF1))
        ALLOCATE(IDCONF(1:NCONFM,1:3))
      ELSE
        ALLOCATE(K_IF1 (1))
        ALLOCATE(K_IF2 (1,1,1),K_IF3 (1,1,1))
        ALLOCATE(K_1P  (1,1)  ,K_1M  (1,1))
        ALLOCATE(K_1P2P(1,1,1),K_1P3M(1,1,1))
        ALLOCATE(K_1P2M(1,1,1),K_1P3P(1,1,1))
        ALLOCATE(K_1M2P(1,1,1),K_1M3M(1,1,1))
        ALLOCATE(K_1M2M(1,1,1),K_1M3P(1,1,1))
        ALLOCATE(TB_V14(1))
        ALLOCATE(TB_V24(1,1,1),TB_V34(1,1,1))
        ALLOCATE(TB_TPM(1,1,1),TB_TMP(1,1,1))
        ALLOCATE(TB_FAC(1,1,1))
        ALLOCATE(IDCONF(1,1))
      ENDIF
!
!.......END NON LINEAR INTERACTIONS
!GM Fin
!
!.......RELATIVE SPECTRUM ->  ABSOLUTE SPECTRUM (TRANSF)
!
      IF(COUSTA .OR.MAREE.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
        CALL BIEF_ALLVEC(2,SITR11,'SITR11',NPOIN2,1,0,MESH)
        CALL BIEF_ALLVEC(2,SITR12,'SITR12',NPOIN2,1,0,MESH)
        CALL BIEF_ALLVEC(2,SITR13,'SITR13',NPOIN2,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,SITR11,'SITR11', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(2,SITR12,'SITR12', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(2,SITR13,'SITR13', 1, 1, 0 ,MESH)
      ENDIF
      ITR11   => SITR11%I
      ITR12   => SITR12%I
      ITR13   => SITR13%I
!
      IF(DIFFRA.GT.0) THEN
        CALL BIEF_ALLVEC(2,SNEIGB,'SNEIGB',   NPOIN2,MAXNSP, 0, MESH)
        CALL BIEF_ALLVEC(2,SNB_CLOSE,'SNB_CLOSE',NPOIN2, 1 , 0, MESH)
      ELSE
        CALL BIEF_ALLVEC(2,SNEIGB,'SNEIGB',   1, 1, 0, MESH)
        CALL BIEF_ALLVEC(2,SNB_CLOSE,'SNB_CLOSE', 1, 1 , 0, MESH)
      ENDIF
!
      NEIGB  => SNEIGB%I
      NB_CLOSE => SNB_CLOSE%I

!-----------------------------------------------------------------------
!
! NEW TELEMAC TO TOMAWAC COUPLING


      !COUPLING VARIABLES
      IF(INCLUS(COUPLING,'TOMAWAC2')) THEN
!                                                       WAC2
      ! TODO SOMETHING FOR QUASI BUBBLE DISCRETISATION
        CALL BIEF_ALLVEC(1,U_TEL ,     'U_TEL ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,V_TEL ,     'H_TEL ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,H_TEL ,     'H_TEL ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,ORBVEL_TEL ,'ORBVEL',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,FX_WAC ,    'FX_WAC',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,FY_WAC ,    'FY_WAC',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,UV_WAC ,    'UV_WAC',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,VV_WAC ,    'VV_WAC',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,DIRMOY_TEL ,'DIRTEL',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,HM0_TEL ,   'HM0TEL',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,TPR5_TEL ,  'TPRTEL',IELM2 , 1 , 2 ,MESH)

!        NVARTOM2TEL = 0
!        NVARTEL2TOM = 0

        ! SENDING TEST VARIABLES
!        CALL ALLBLO(TEL2TOM ,'TEL2TO')
!        CALL ADDBLO(TEL2TOM,SPRIVE)
!        CALL ALLBLO(TOM2TEL ,'TOM2TE')
!        CALL ADDBLO(TOM2TEL,SPRIVE)
!        CALL ADDBLO(TOM2TEL,SPRIVE)
!        NVARTOM2TEL = 2
!        NVARTEL2TOM = 1
!        CALL OS('X=0     ',X=U_TEL)
!        CALL OS('X=0     ',X=V_TEL)
!        CALL OS('X=C     ',X=H_TEL,C=-10.0D0)

        ! SENDING VARIABLES TO TOMAWAC
        CALL ALLBLO(TEL2TOM ,'TEL2TO')
        CALL ADDBLO(TEL2TOM,U_TEL)
        CALL ADDBLO(TEL2TOM,V_TEL)
        CALL ADDBLO(TEL2TOM,H_TEL)
        NVARTEL2TOM = 3
        ! RECEIVING VARIABLES FROM TOMAWAC
        NVARTOM2TEL = 0
        CALL ALLBLO(TOM2TEL ,'TOM2TE')
        IF (COPSIS_TEL) THEN
          CALL ADDBLO(TOM2TEL,DIRMOY_TEL)
          CALL ADDBLO(TOM2TEL,HM0_TEL)
          CALL ADDBLO(TOM2TEL,TPR5_TEL)
          CALL ADDBLO(TOM2TEL,ORBVEL_TEL)
          ORBVEL_TEL%R = 0.0D0
          TPR5_TEL%R   = 0.0D0
          HM0_TEL%R    = 0.0D0
          DIRMOY_TEL%R = 0.0D0
          NVARTOM2TEL  = NVARTOM2TEL + 4
        ENDIF
        IF (COUROU_TEL) THEN
          CALL ADDBLO(TOM2TEL,FX_WAC)
          CALL ADDBLO(TOM2TEL,FY_WAC)
          FX_WAC%R = 0.0D0
          FY_WAC%R = 0.0D0
          NVARTOM2TEL = NVARTOM2TEL + 2
        ENDIF
        ! WIND CHECK IS THIS IS POSSIBLE
        IF (VENT_TEL) THEN
          CALL ADDBLO(TOM2TEL,UV_WAC)
          CALL ADDBLO(TOM2TEL,VV_WAC)
          UV_WAC%R = 0.0D0
          VV_WAC%R = 0.0D0
          NVARTOM2TEL = NVARTOM2TEL + 2
        ENDIF

!
!-----------------------------------------------------------------------
      ELSEIF (.NOT.INCLUS(COUPLING,'TOMAWAC') ) THEN
      ! Dummy association for stand alone tomawac
        CPL_WAC_DATA%U_TEL => SZF
        CPL_WAC_DATA%V_TEL => SZF
        CPL_WAC_DATA%H_TEL => SZF
        CPL_WAC_DATA%WIPDXW => SZF
        CPL_WAC_DATA%WIPDYW => SZF
        CPL_WAC_DATA%USTW => SZF
        CPL_WAC_DATA%VSTW => SZF
        CPL_WAC_DATA%ZTELW => SZF
        CPL_WAC_DATA%WSTW => SZF
        CPL_WAC_DATA%WIPW => SZF
        CPL_WAC_DATA%FDXW => SZF
        CPL_WAC_DATA%FDYW => SZF
        CPL_WAC_DATA%FBXW => SZF
        CPL_WAC_DATA%FBYW => SZF
        CPL_WAC_DATA%CFWCW => SZF
        CPL_WAC_DATA%FDKW => SZF
        CPL_WAC_DATA%FWX => SZF
        CPL_WAC_DATA%FWY => SZF
      ENDIF
!
!***********************************************************************
!
! CHECKS AND WRITES OUT
!
      WRITE(LU,23)
   23 FORMAT(1X,///,21X,'*************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION:    *',/,
     &21X,              '*************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

