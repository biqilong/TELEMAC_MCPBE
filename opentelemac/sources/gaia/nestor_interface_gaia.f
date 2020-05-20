!                       ********************************
                        SUBROUTINE NESTOR_INTERFACE_GAIA
!                       ********************************
!
     &(OPTION,GRAFCOUNT,XMVS0,XKV01,VOLU2D)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief This is the interface to nestor, containing all
!!           dependencies to nestor libraries
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] OPTION  1 : Initialisation (called in gaia)
!!                   2 : called every time step (from
!!                   bedload_posttreatment)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: NESTOR,NPOIN,IELMH_GAI
     &                            ,GAI_FILES,SINACT,SINPOL
     &                            ,SINREF,SINRST
     &                            ,ZF, HN, T13, T14
     &                            ,MESH, MSK, MASKEL
     &                            ,LT, DT, AT0
     &                            ,MARDAT, MARTIM
     &                            ,ZFCL_C, AVAIL
     &                            ,ZRL
     &                            ,ZR, NSICLA
     &                            ,MOFAC, ES
     &                            ,EVCL_MB      !     evolution of mass of each sand class for laver-1 [ kg/m**2 ]
     &                            ,MASS_SAND    !     mass of each sand class for each laver           [ kg/m**2 ]
     &                            ,NSAND        !     number of sand classes
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: OPTION, GRAFCOUNT ! period of graphical outputs
      DOUBLE PRECISION, INTENT(IN) :: XMVS0(NSICLA)     ! density sediment classes
      DOUBLE PRECISION, INTENT(IN) :: XKV01             ! non cohesive bed porosity of layer 1
      TYPE (BIEF_OBJ),  INTENT(IN) :: VOLU2D            ! node area
!
!--------------------- local variables ---------------
      LOGICAL             :: CALLEDBY_T2D
      DOUBLE PRECISION, ALLOCATABLE :: TMP_AVAIL(:,:,:)
      INTEGER             :: I, ICLA
!
      DOUBLE PRECISION,ALLOCATABLE,SAVE,DIMENSION(:,:) ::   M2T  ! conversion faktor Mass to Thickness
      DOUBLE PRECISION,ALLOCATABLE,SAVE,DIMENSION(:,:) :: MPA2T  ! conversion faktor MassPerArea to Thickness
!
!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IF(OPTION.EQ.1) THEN    ! initialisation of Nestor
!
!     INITIALISATION
!
      CALLEDBY_T2D = .FALSE.

      ALLOCATE(  M2T(NSICLA,NPOIN))
      ALLOCATE(MPA2T(NSICLA,NPOIN))
!
!     +----  conversion faktor Mass to Thickness  ---------------------+
!     |   faktor = 1 / (  density * area * ( 1 - porosity )  )         |
!     |   =>  mass * faktor = thickness                                |
!     | but VARIOUS MASSES ARE STILL IN [kg/m**2]  ==>
!
      DO ICLA = 1,NSICLA  ! calc. conversion factor "MassPerArea" to Thickness
        DO I = 1,NPOIN
            M2T(ICLA,I) = 1.D0/( XMVS0(ICLA)*VOLU2D%R(I)*(1.D0-XKV01) )
          MPA2T(ICLA,I) = 1.D0/(  XMVS0(ICLA) * (1.D0-XKV01)  )
        ENDDO
      ENDDO
!
!
!       !      _____________________________________________________________________
!       !     /________ calculation of Node-areas for all nodes of grid ___________/
!             ! calculation of Node-areas in T13      !  for parallel: here the interface-nodes
!             !                                          node-area is allready reduced
!             CALL VECTOR(T13,'=','MASBAS          '
!            &      ,IELMH_GAI,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)

!
!
      CALL INTERFACEINITNESTOR(  NCSIZE, IPID              ! Number of parallel threads, ID of thread
     &                         , NPOIN, NSICLA             ! Number of: nodes, SIze CLAsses
     &                         , MARDAT, MARTIM            ! Sis start: date , time
     &                         , MOFAC                     ! morphological factor
     &                         , GRAFCOUNT                 ! period of graphical outputs
     &                         , MESH%X%R                  ! X-coordinate of node
     &                         , MESH%Y%R                  ! Y-coordinate of node
     &                         , VOLU2D%R                  ! node areas
     &                         , MAXVAL( MESH%KNOLG%I(:) ) ! local maxIndex, needed to calc. npion of global mesh
     &                         , ZF%R                      ! bottom at time 0 [m+NN]    Itera
     &                         , LU                        ! logical unit for standard output
     &                         , GAI_FILES(SINACT)%LU      ! logical unit to NESTOR ACTION FILE
     &                         , GAI_FILES(SINPOL)%LU      ! logical unit to NESTOR POLYGON FILE
     &                         , GAI_FILES(SINREF)%LU      ! logical unit to NESTOR SURFACE REFERENCE FILE
     &                         , GAI_FILES(SINRST)%LU      ! logical unit to NESTOR RESTART FILE
     &                         , CALLEDBY_T2D              ! true if coupled with telemac2d
     &                         , ZRL%R                     ! reference level [m+NN]
     &                        )
!
!
! LEOPRD aus T2D, GRAFCOUNT in gaia.f and sisyphe.F
!
!
! NO QUASI BUBBLE SUPPORTED
        IF(IELMH_GAI.NE.11) THEN
          WRITE(*,*)'ONLY LINEAR 2D ELEMENTS ARE SUPPORTED'
          WRITE(*,*)'NESTOR -STOP, PLEASE START WITHOUT'
          WRITE(*,*)'NESTOR OR USE ANOTHER ELEMENT TYPE'
          WRITE(*,*)'ACTUAL ELEMENT TYPE OF ZF: ',IELMH_GAI
          CALL PLANTE(1)
          STOP
        ENDIF
!
!
      !#########################################
      !
      ! Run Nestor
      !
      !#########################################
      ELSEIF(OPTION.EQ.2) THEN
!
!     CALL FROM WITHIN BEDLOAD_MAIN_GAIA
!
        IF (NESTOR.EQV..TRUE.) THEN
!
        IF(NSAND.NE.NSICLA)THEN
          WRITE(*,*)'A L A R M   there seems to be mud !'
          WRITE(*,*)'Coupling of Nestor with mud is not realized'
          WRITE(*,*)'stop in SUBROUTINE NESTOR_INTERFACE_GAIA'
          STOP
        ENDIF
!
!       !==== conversion of mass to thickness and fraction =============!
!       EVCL_MB  and  MASS_SAND   given in [ kg/m**2 ]
        DO ICLA = 1,NSICLA  ! convert for layer-1 for each sand class,
          DO I = 1,NPOIN
            ZFCL_C%ADR(ICLA)%P%R(I)                              !>  mass evolution to thickness evolution
     &         = EVCL_MB%ADR(ICLA)%P%R(I) * MPA2T(ICLA,I)        !   thickness evolution

            AVAIL(I,1,ICLA)                                      !>  for layer-1
     &         = MASS_SAND(ICLA,1,I) * MPA2T(ICLA,I) / ES(I,1)   !   mass to fraction
          ENDDO
        ENDDO
!
!        DO ICLA = 1,NSICLA                                                  !debug
!          DO I = 1,NPOIN                                                    !debug
!            IF( (I.GE.1    .AND. I.LE.5)  .OR.                              !debug
!     &         (I.GE.3001 .AND. I.LE.3005)    ) THEN                        !debug
!              WRITE(*,*)'AVAIL I=',I,'1  icla=', ICLA                       !debug
!     &                                         ,'Z=',AVAIL(I,1,ICLA)        !debug
!     &                                         ,'F=',M2T(ICLA,I)            !debug
!     &                                         ,'V=',VOLU2D%R(I)            !debug
!     &                                         ,'M=',MASS_SAND(ICLA,1,I)    !debug
!            ENDIF                                                           !debug
!          ENDDO                                                             !debug
!        ENDDO                                                               !debug
!
!
          ALLOCATE(TMP_AVAIL(NPOIN,1,NSICLA))
          TMP_AVAIL = AVAIL(1:NPOIN,1:1,1:NSICLA)
          CALL INTERFACERUNNESTOR(  NPOIN      !  Number of POINts (nodes)
     &                            , NSICLA     !  Number of SIze CLAsses
     &                            , LT         !  Telemac time step
     &                            , DT         !  duration of Sisyphe time step
     &                            , AT0        !  time
     &                            , ES(1:NPOIN,1)   !(non const.) thickness of active laver [m]
     &                            , ZF%R       !  bottom [m+NN]
     &                            , ZFCL_C     !  evolution per class per time step [m]
     &                            , TMP_AVAIL  !fraction of each class for layer 1
     &                            , MESH%KNOLG%I    ! index list: Local to Global node index
     &                            , HN%R       !  water depth [m]                               ! Itera
     &                           )
          DEALLOCATE(TMP_AVAIL)
        ENDIF
!
!       !==== conversion of thickness to mass  ========================!
        DO ICLA = 1,NSICLA  ! convert for each class thickness evolution to mass evolution
          DO I = 1,NPOIN
            EVCL_MB%ADR(ICLA)%P%R(I)
     &         =  ZFCL_C%ADR(ICLA)%P%R(I) / MPA2T(ICLA,I)
          ENDDO
        ENDDO
!
!
      ELSE  ! OPTION is neither 1 nor 2
!
!       ERROR
!
        IF(LNG.EQ.1) WRITE(LU,*) 'MAUVAISE OPTION POUR NESTOR'
        IF(LNG.EQ.2) WRITE(LU,*) 'BAD OPTION FOR NESTOR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
