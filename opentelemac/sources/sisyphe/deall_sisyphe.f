!                   **************************
                    SUBROUTINE DEALL_SISYPHE
!                   **************************
!
!
!***********************************************************************
! SISYPHE   V7P1                                   19/05/2016
!***********************************************************************
!
!brief    Memory deallocation of structures, aliases, blocks...
!
!Author  R-S MOURADI (LNHE)
!
!history  P TASSI, F CORDIER, S PAVAN
!+        18/09/2018
!+        V8P0
!     + Deallocates memory for SANFRA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I, K
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      CALL OUTBIEF(MESH)
!     Deallocate the mesh structure
      CALL DEALMESH(MESH)

      NULLIFY(IKLE)
      NULLIFY(X)
      NULLIFY(Y)
      NULLIFY(NELEM)
      NULLIFY(NELMAX)
      NULLIFY(NPTFR)
      NULLIFY(NPTFRX)
      NULLIFY(TYPELM)
      NULLIFY(NPOIN)
      NULLIFY(NPMAX)
      NULLIFY(MXPTVS)
      NULLIFY(LV)
      CALL BIEF_DEALLOBJ(VARSOR)
!
!     Deallocating all the blocks first
      CALL BIEF_DEALLOBJ(S     )
      CALL BIEF_DEALLOBJ(E     )
      CALL BIEF_DEALLOBJ(Z     )
      CALL BIEF_DEALLOBJ(DEL_Z )
      CALL BIEF_DEALLOBJ(ZF_C  )
      CALL BIEF_DEALLOBJ(ZF_S  )
      CALL BIEF_DEALLOBJ(ESOMT )
      CALL BIEF_DEALLOBJ(EMAX  )
      CALL BIEF_DEALLOBJ(Q     )
      CALL BIEF_DEALLOBJ(QU    )
      CALL BIEF_DEALLOBJ(QV    )
      CALL BIEF_DEALLOBJ(DEL_QU)
      CALL BIEF_DEALLOBJ(DEL_QV)
      CALL BIEF_DEALLOBJ(DEL_UW)
      CALL BIEF_DEALLOBJ(U2D   )
      CALL BIEF_DEALLOBJ(V2D   )
      CALL BIEF_DEALLOBJ(QS    )
      CALL BIEF_DEALLOBJ(QSX   )
      CALL BIEF_DEALLOBJ(QSY   )
      CALL BIEF_DEALLOBJ(QS_C  )
      CALL BIEF_DEALLOBJ(QSXC  )
      CALL BIEF_DEALLOBJ(QSYC  )
      CALL BIEF_DEALLOBJ(QS_S  )
      CALL BIEF_DEALLOBJ(QSXS  )
      CALL BIEF_DEALLOBJ(QSYS  )
      CALL BIEF_DEALLOBJ(HIDING)
      CALL BIEF_DEALLOBJ(ZF    )
      CALL BIEF_DEALLOBJ(ZR    )
      CALL BIEF_DEALLOBJ(RADSEC)
      CALL BIEF_DEALLOBJ(ZREF  )
      CALL BIEF_DEALLOBJ(CHESTR)
      CALL BIEF_DEALLOBJ(COEFPN)
      CALL BIEF_DEALLOBJ(CF    )
      CALL BIEF_DEALLOBJ(TOB   )
      CALL BIEF_DEALLOBJ(TOBW  )
      CALL BIEF_DEALLOBJ(MU    )
      CALL BIEF_DEALLOBJ(KSP   )
      CALL BIEF_DEALLOBJ(KS    )
      CALL BIEF_DEALLOBJ(KSR   )
      CALL BIEF_DEALLOBJ(THETAW)
      CALL BIEF_DEALLOBJ(FW    )
      CALL BIEF_DEALLOBJ(UW    )
      CALL BIEF_DEALLOBJ(HW    )
      CALL BIEF_DEALLOBJ(TW    )
      CALL BIEF_DEALLOBJ(DZF_GF)
      CALL BIEF_DEALLOBJ(ACLADM)
      CALL BIEF_DEALLOBJ(UNLADM)
      CALL BIEF_DEALLOBJ(HCPL  )
      CALL BIEF_DEALLOBJ(ECPL  )
      CALL BIEF_DEALLOBJ(ELAY  )
      CALL BIEF_DEALLOBJ(ESTRAT)
      CALL BIEF_DEALLOBJ(KX    )
      CALL BIEF_DEALLOBJ(KY    )
      CALL BIEF_DEALLOBJ(KZ    )
      CALL BIEF_DEALLOBJ(UCONV )
      CALL BIEF_DEALLOBJ(VCONV )
      CALL BIEF_DEALLOBJ(UNORM )
      CALL BIEF_DEALLOBJ(DISP  )
      CALL BIEF_DEALLOBJ(DISP_C)
      CALL BIEF_DEALLOBJ(MASKB )
      CALL BIEF_DEALLOBJ(MASK  )
      CALL BIEF_DEALLOBJ(AFBOR )
      CALL BIEF_DEALLOBJ(BFBOR )
      CALL BIEF_DEALLOBJ(FLBOR )
      CALL BIEF_DEALLOBJ(Q2BOR )
!     BOUNDARY FLUX FOR CALL TO CVDFTR
      CALL BIEF_DEALLOBJ(FLBOR_SIS )
      CALL BIEF_DEALLOBJ(FLBORTRA  )
      CALL BIEF_DEALLOBJ(CSTAEQ)
!     MAK ADDITION
      CALL BIEF_DEALLOBJ(CSRATIO)
      CALL BIEF_DEALLOBJ(HN    )
      CALL BIEF_DEALLOBJ(HCLIP )
      CALL BIEF_DEALLOBJ(HPROP )
      CALL BIEF_DEALLOBJ(VOLU2D)
      CALL BIEF_DEALLOBJ(V2DPAR)
      CALL BIEF_DEALLOBJ(UNSV2D)
      CALL BIEF_DEALLOBJ(MPM_ARAY)
      CALL BIEF_DEALLOBJ(FLULIM  )

      ! MSK
      CALL BIEF_DEALLOBJ(MASKEL)
      CALL BIEF_DEALLOBJ(MSKTMP)
      CALL BIEF_DEALLOBJ(MASKPT)
!     FOR MIXED SEDIMENTS
      CALL BIEF_DEALLOBJ(FLUER_VASE )
      CALL BIEF_DEALLOBJ(TOCE_MIXTE )
      CALL BIEF_DEALLOBJ(MS_SABLE   )
      CALL BIEF_DEALLOBJ(MS_VASE    )
      CALL BIEF_DEALLOBJ( LIEBOR)
      CALL BIEF_DEALLOBJ( LIQBOR)
      CALL BIEF_DEALLOBJ( LIMTEC)
      CALL BIEF_DEALLOBJ( NUMLIQ)
      CALL BIEF_DEALLOBJ( CLT   )
      CALL BIEF_DEALLOBJ( CLU   )
      CALL BIEF_DEALLOBJ( CLV   )
      CALL BIEF_DEALLOBJ( LIMDIF)
      CALL BIEF_DEALLOBJ( LICBOR)
      CALL BIEF_DEALLOBJ( LIHBOR)
      CALL BIEF_DEALLOBJ( BOUNDARY_COLOUR)
      CALL BIEF_DEALLOBJ( LIMPRO)
      CALL BIEF_DEALLOBJ( INDIC )
      CALL BIEF_DEALLOBJ( IT1   )
      CALL BIEF_DEALLOBJ( IT2   )
      CALL BIEF_DEALLOBJ( IT3   )
      CALL BIEF_DEALLOBJ( IT4   )
! NUMBER OF LAYERS
      CALL BIEF_DEALLOBJ( NLAYER)
      CALL BIEF_DEALLOBJ(BREACH)
      CALL BIEF_DEALLOBJ(IFAMAS)

      DEALLOCATE(AVAIL)
      DEALLOCATE(ES)
      DEALLOCATE(ES_SABLE)
      DEALLOCATE(ES_VASE)
      DEALLOCATE(CONC)
      DEALLOCATE(IVIDE)
      DEALLOCATE(SANFRA)

      CALL BIEF_DEALLOBJ(MASKTR)
      CALL BIEF_DEALLOBJ(EBOR  )
      CALL BIEF_DEALLOBJ(QBOR  )
      DO I=1,NSICLA
        DO K=1,NOMBLAY
          ALLOCATE(AVAI%ADR(K+(I-1)*NOMBLAY)%P%R(1))
        ENDDO
      ENDDO
      CALL BIEF_DEALLOBJ(AVAI  )
      DO K=1,NOMBLAY
        ALLOCATE(LAYTHI%ADR(K)%P%R(1))
      ENDDO
      CALL BIEF_DEALLOBJ(LAYTHI)
      DO K=1,NOMBLAY
        ALLOCATE(LAYCONC%ADR(K)%P%R(1))
      ENDDO
      CALL BIEF_DEALLOBJ(LAYCONC)
!
      CALL BIEF_DEALLOBJ(QSCL  )
      CALL BIEF_DEALLOBJ(QSCL_C)
      CALL BIEF_DEALLOBJ(QSCLXC)
      CALL BIEF_DEALLOBJ(QSCLYC)
      CALL BIEF_DEALLOBJ(ZFCL  )
      CALL BIEF_DEALLOBJ(ZFCL_C)
!
      CALL BIEF_DEALLOBJ(CBOR  )
      CALL BIEF_DEALLOBJ(QSCL_S)
      CALL BIEF_DEALLOBJ(QSCLXS)
      CALL BIEF_DEALLOBJ(QSCLYS)
      CALL BIEF_DEALLOBJ(ZFCL_S)
      CALL BIEF_DEALLOBJ(FLUDP )
      CALL BIEF_DEALLOBJ(FLUDPT)
      CALL BIEF_DEALLOBJ(FLUER )
      CALL BIEF_DEALLOBJ(FLUERT)
      CALL BIEF_DEALLOBJ(CS    )
      CALL BIEF_DEALLOBJ(CTILD )
      CALL BIEF_DEALLOBJ(CST   )
!
      CALL BIEF_DEALLOBJ(ZFCL_MS)
      CALL BIEF_DEALLOBJ(CALFA_CL)
      CALL BIEF_DEALLOBJ(SALFA_CL)
      CALL BIEF_DEALLOBJ(FLBCLA)

      CALL BIEF_DEALLOBJ(AM1_S)
      CALL BIEF_DEALLOBJ(AM2_S)
      CALL BIEF_DEALLOBJ(MBOR )

      CALL BIEF_DEALLOBJ( W1 )
      CALL BIEF_DEALLOBJ( TE1)
      CALL BIEF_DEALLOBJ( TE2)
      CALL BIEF_DEALLOBJ( TE3)

      CALL BIEF_DEALLOBJ(VARCL)
      CALL BIEF_DEALLOBJ(PRIVE)
      CALL BIEF_DEALLOBJ(TB   )
      CALL BIEF_DEALLOBJ(TB2  )

      IF(NADVAR.GT.0) THEN
        CALL BIEF_DEALLOBJ(ADVAR )
      ENDIF

      ! Resetting variable
      INIT_FLUXPR = .TRUE.
      IF(DEJA_RFC) THEN
        DEJA_RFC = .FALSE.
        DEALLOCATE(INFIC_RFC)
        DEALLOCATE(TIME_RFC)
      ENDIF
      IF(DEJA_FLUSEC) THEN
        DEJA_FLUSEC = .FALSE.
        OLD_METHOD_FLUSEC = .FALSE.
        DEALLOCATE(NSEG)
        DEALLOCATE(LISTE)
        DEALLOCATE(VOLNEGS)
        DEALLOCATE(VOLPOSS)
        DEALLOCATE(VOLNEGC)
        DEALLOCATE(VOLPOSC)
        DEALLOCATE(FLX)
        DEALLOCATE(VOLNEG)
        DEALLOCATE(VOLPOS)
        DEALLOCATE(FLXS)
        DEALLOCATE(FLXC)
      ENDIF
      IF(DEJA_FLUSEC2) THEN
        OLD_METHOD_FLUSEC = .FALSE.
        DEJA_FLUSEC2 = .FALSE.
        DO I = 1,NUMBEROFLINES_FLUSEC2
          DEALLOCATE(FLUXLINEDATA_FLUSEC2(I)%SECTIONIDS)
          DEALLOCATE(FLUXLINEDATA_FLUSEC2(I)%DIRECTION)
        ENDDO
        DEALLOCATE(FLUXLINEDATA_FLUSEC2)
        DEALLOCATE(FLUX_FLUSEC2)
        DEALLOCATE(VOLFLUX_FLUSEC2)
      ENDIF

      ! Lecdon
      DEALLOCATE(SOLDIS)
      DEALLOCATE(OKCGL)
      DEALLOCATE(OKQGL)
      DEALLOCATE(CBOR_CLASSE)
      DEALLOCATE(CTRLSC)
!
!-----------------------------------------------------------------------
!
!=======================================================================
!
! WRITES OUT TO LISTING :
!
!      IF(LISTIN) THEN
!        WRITE(LU,23)
!      ENDIF
!23    FORMAT(1X,///,21X,'*************************************',/,
!     &21X,              '*    END OF MEMORY ORGANIZATION:    *',/,
!     &21X,              '*************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DEALL_SISYPHE
