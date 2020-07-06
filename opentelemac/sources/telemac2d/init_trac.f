!                   ********************
                    SUBROUTINE INIT_TRAC
!                   ********************
!
     &(T,HTN,SMTR,FLUXT,FLUHTEMP,FLUHBTEMP,MASSOU,FLUTENT,FLUTSOR,
     & FLBOR,MESH)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    INITIALISES TRACERS.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  FLBOR         BOUNDARY MASS FLUXES
!>@param  [in,out]  FLUXT         FLUX FOR TRACER AT TIME N
!>@param  [in,out]  FLUHBTEMP     BOUNDARY FLUX FOR TRACER
!>@param  [in,out]  FLUHTEMP      FLUX FOR TRACER
!>@param  [in,out]  FLUTENT       FLUX TRACER INLET
!>@param  [in,out]  FLUTSOR       FLUX TRACER OUTLET
!>@param  [in,out]  HTN           TRACER*DEPTH AT TIME N
!>@param  [in,out]  MASSOU        ADDED TRACER MASS BY SOURCE TERM
!>@param  [in,out]  SMTR          SOURCE TERMS FOR TRACEUR
!>@param  [in,out]  T             TRACER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: FLUER_ADV,FLUDPT_ADV,NSUSP_TEL
      USE DECLARATIONS_TELEMAC2D, ONLY: IND_SED,LITBOR,HN,U,V,TEXP,T1,S,
     &                                  MARTIM,MARDAT,LAMBD0,TIMP,CF,T1,
     &                                  T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,
     &                                  T12,PATMOS,PHI0,NPOIN,NSEG,
     &                                  NPTFR,NTRAC,NREJET,ISCE,NREG,
     &                                  TNP,PT_IN_POLY,TSCE2,SMH,H,DT,
     &                                  AT,V2DPAR,TN,TBOR
      USE DECLARATIONS_TELEMAC, ONLY: COUPLING,KENT
      USE DECLARATIONS_KHIONE, ONLY: IND_T,IND_F
      USE INTERFACE_TELEMAC2D, EX_INIT_TRAC => INIT_TRAC
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T,HTN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SMTR,FLUXT,FLUHTEMP,FLUHBTEMP
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,I,II,TTL,IREG,NSG,ITRAC,ISUSP
!
      DO ITRAC=1,NTRAC
!
!       INIT GLOBAL BALANCE
        MASSOU(ITRAC) = 0.D0
        FLUTENT(ITRAC)= 0.D0
        FLUTSOR(ITRAC)= 0.D0
        CALL OS('X=0     ',X=TEXP%ADR(ITRAC)%P)
        CALL OS('X=0     ',X=TIMP%ADR(ITRAC)%P)
!
        DO IS=1,NPOIN
!         INIT CONSERVATIVE VAR FOR TRACER
          HTN%ADR(ITRAC)%P%R(IS)=H%R(IS)*TN%ADR(ITRAC)%P%R(IS)
!         INIT SOURCE TERM FOR TRACER
          SMTR%ADR(ITRAC)%P%R(IS)=0.D0
        ENDDO
!
!       INIT SOURCE TERM FOR TRACER
        IF(NREJET.NE.0.AND.NREG.EQ.0) THEN
          DO I=1,NREJET
            IS =ISCE(I)
            IF(IS.GT.0) THEN
              SMTR%ADR(ITRAC)%P%R(IS)=SMH%R(IS)*TSCE2(I,ITRAC)
            ENDIF
          ENDDO
        ELSEIF(NREJET.NE.0.AND.NREG.NE.0) THEN
          DO IREG=1,NREG
            TTL=TNP(IREG)
!           TEST USEFUL FOR PARALLEL MODE
            IF(TTL.NE.0) THEN
              DO I=1,TTL
                II=PT_IN_POLY(IREG,I)
                SMTR%ADR(ITRAC)%P%R(II)=SMH%R(II)*TSCE2(IREG,ITRAC)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
!
        IF(INCLUS(COUPLING,'GAIA').AND.
     &    (ITRAC.GE.IND_SED.AND.ITRAC.LT.IND_SED+NSUSP_TEL)) THEN
          ISUSP=ITRAC-IND_SED+1
          CALL PREP_ADVECTION_GAIA(U,V,0,1,
     &         ISUSP,LITBOR%ADR(ITRAC)%P%I,TBOR%ADR(ITRAC)%P%R,
     &         TN%ADR(ITRAC)%P%R,KENT,FLBOR,HN)
          DO IS=1,NPOIN
            SMTR%ADR(ITRAC)%P%R(IS)=(FLUER_ADV%R(IS)-FLUDPT_ADV%R(IS))*
     &                              H%R(IS)
          ENDDO
        ENDIF
!
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
        CALL SOURCE_WAQ
     & (NPOIN,NPOIN,TEXP,TIMP,TN,HN,U,V,CF,
     &  T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12, T1,T2,T3,
     &  PATMOS,2,1,
     &  LAMBD0,PHI0,AT,MARDAT,MARTIM,MESH%X)
      ENDIF
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
!
        CALL SOURCE_FRAZIL
     &    ( NPOIN,TEXP,TN,HN,U,V,T1,S,MESH,
     &      DT,AT,MARDAT,MARTIM,LAMBD0 )
!
        IF(ITRAC.EQ.IND_T.OR.
     &     ITRAC.EQ.IND_F) THEN
!          (ITRAC.GE.IND_FRA.AND.ITRAC.LE.IND_FRA-NC_FRA)) THEN
          DO IS=1,NPOIN
            SMTR%ADR(ITRAC)%P%R(IS)=SMTR%ADR(ITRAC)%P%R(IS)+
     &                     TEXP%ADR(ITRAC)%P%R(IS)*H%R(IS)*V2DPAR%R(IS)
          ENDDO
        ENDIF
      ENDIF

!
!       INIT FLUXES
        DO NSG=1,NSEG
          FLUXT%ADR(ITRAC)%P%R(NSG)=0.D0
          FLUHTEMP%ADR(ITRAC)%P%R(NSG)=0.D0
        ENDDO
!
!       INIT BOUNDARY FLUXES
        DO IS=1,NPTFR
          FLUHBTEMP%ADR(ITRAC)%P%R(IS)=0.D0
        ENDDO
!
        CALL OS('X=Y     ',X=T%ADR(ITRAC)%P,Y=TN%ADR(ITRAC)%P)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
