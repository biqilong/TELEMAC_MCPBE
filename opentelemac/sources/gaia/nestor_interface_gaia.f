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
      INTEGER             :: i, ICLA
!
      DOUBLE PRECISION,ALLOCATABLE,SAVE,DIMENSION(:,:) ::   M2T  ! conversion faktor Mass to Thickness
      DOUBLE PRECISION,ALLOCATABLE,SAVE,DIMENSION(:,:) :: MpA2T  ! conversion faktor MassPerArea to Thickness
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
      ALLOCATE(MpA2T(NSICLA,NPOIN))
!
!     +----  conversion faktor Mass to Thickness  ---------------------+
!     |   faktor = 1 / (  density * area * ( 1 - porosity )  )         |
!     |   =>  mass * faktor = thickness                                |
!     | but VARIOUS MASSES ARE STILL IN [kg/m**2]  ==>
!
      DO ICLA = 1,NSICLA  ! calc. conversion factor "MassPerArea" to Thickness
        DO i = 1,NPOIN
            M2T(ICLA,i) = 1.D0/( XMVS0(ICLA)*VOLU2D%R(i)*(1.D0-XKV01) )
          MpA2T(ICLA,i) = 1.D0/(  XMVS0(ICLA) * (1.D0-XKV01)  )
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
          DO i = 1,NPOIN
            ZFCL_C%ADR(ICLA)%P%R(i)                              !>  mass evolution to thickness evolution
     &         = EVCL_MB%ADR(ICLA)%P%R(i) * MpA2T(ICLA,i)        !   thickness evolution

            AVAIL(i,1,ICLA)                                      !>  for layer-1 
     &         = MASS_SAND(ICLA,1,i) * MpA2T(ICLA,i) / ES(i,1)   !   mass to fraction
          ENDDO
        ENDDO
!        
!        DO ICLA = 1,NSICLA                                                  !debug
!          DO i = 1,NPOIN                                                    !debug
!            IF( (i.GE.1    .AND. i.LE.5)  .OR.                              !debug
!     &         (i.GE.3001 .AND. i.LE.3005)    ) THEN                        !debug
!              WRITE(*,*)'AVAIL i=',i,'1  icla=', ICLA                       !debug
!     &                                         ,'Z=',AVAIL(i,1,ICLA)        !debug
!     &                                         ,'F=',M2T(ICLA,i)            !debug
!     &                                         ,'V=',VOLU2D%R(i)            !debug
!     &                                         ,'M=',MASS_SAND(ICLA,1,i)    !debug
!            ENDIF                                                           !debug
!          ENDDO                                                             !debug
!        ENDDO                                                               !debug
!       
!
          CALL INTERFACERUNNESTOR(  NPOIN      !  Number of POINts (nodes)
     &                            , NSICLA     !  Number of SIze CLAsses
     &                            , LT         !  Telemac time step
     &                            , DT         !  duration of Sisyphe time step
     &                            , AT0        !  time
     &                            , ES(1:NPOIN,1)   !(non const.) thickness of active laver [m]
     &                            , ZF%R       !  bottom [m+NN]
     &                            , ZFCL_C     !  evolution per class per time step [m]
     &                            , AVAIL(1:NPOIN,1,1:NSICLA)  !fraction of each class for layer 1
     &                            , MESH%KNOLG%I    ! index list: Local to Global node index
     &                            , HN%R       !  water depth [m]                               ! Itera
     &                           )
        ENDIF
!
!       !==== conversion of thickness to mass  ========================! 
        DO ICLA = 1,NSICLA  ! convert for each class thickness evolution to mass evolution
          DO i = 1,NPOIN
            EVCL_MB%ADR(ICLA)%P%R(i)
     &         =  ZFCL_C%ADR(ICLA)%P%R(i) / MpA2T(ICLA,i)
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