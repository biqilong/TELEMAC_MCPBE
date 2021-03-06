!                       ******************************
                        SUBROUTINE NESTOR_INTERFACE
!                       ******************************
!
     &(OPTION)
!
!***********************************************************************
! SISYPHE VERSION
!
!
!***********************************************************************
!
!  FUNCTION: THIS IS THE INTERFACE TO NESTOR, CONTAINING ALL
!            DEPENDENCIES TO NESTOR LIBRARIES
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   OPTION       | -->| 1 : INITIALISATION (CALLED IN SISYPHE)
! |                |    | 2 : CALLED EVERY TIME STEP (FROM
! |                |    |     BEDLOAD_MAIN)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! PROGRAMME APPELANT : SISYPHE, BEDLOAD_MAIN
! PROGRAMMES APPELES :
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NESTOR,NPOIN,IELMH_SIS
     &                                ,SIS_FILES,SINACT,SINPOL
     &                                ,SINREF,SINRST
     &                                ,ZF, HN, T13, T14
     &                                ,MESH, MSK, MASKEL
     &                                ,LT, DT, AT0, LEOPR
     &                                ,MARDAT, MARTIM
     &                                ,ZFCL_C,AVAIL
     &                                ,ZRL
     &                                ,NSICLA
     &                                ,MOFAC, DT, NSOUS, ES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: OPTION
      DOUBLE PRECISION    :: DTS
      LOGICAL             :: CALLEDBY_T2D
      DOUBLE PRECISION, ALLOCATABLE :: TMP_AVAIL(:,:,:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IF(OPTION.EQ.1) THEN
!
!     INITIALISATION
!
      CALLEDBY_T2D = .FALSE.
!
!      _____________________________________________________________________
!     /________ calculation of Node-areas for all nodes of grid ___________/
      ! calculation of Node-areas in T13      !  for parallel: here the interface-nodes
      !                                          node-area is allready reduced
      CALL VECTOR(T13,'=','MASBAS          '
     &      ,IELMH_SIS,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)
!
!
      CALL INTERFACEINITNESTOR(  NCSIZE, IPID, NPOIN
     &                         , NSICLA
     &                         , MARDAT, MARTIM ! Sis start: date , time
     &                         , MOFAC    ! morphological factor
     &                         , LEOPR  ! period of graphical outputs
     &                         , MESH%X%R
     &                         , MESH%Y%R
     &                         , T13%R
     &                         , MAXVAL( MESH%KNOLG%I(:) )
     &                         , ZF%R       !  bottom at time 0 [m+NN]    Itera
     &                         , LU                       ! logical unit for standard output
     &                         , SIS_FILES(SINACT)%LU     ! logical unit to NESTOR ACTION FILE
     &                         , SIS_FILES(SINPOL)%LU     ! logical unit to NESTOR POLYGON FILE
     &                         , SIS_FILES(SINREF)%LU     ! logical unit to NESTOR SURFACE REFERENCE FILE
     &                         , SIS_FILES(SINRST)%LU     ! logical unit to NESTOR RESTART FILE
     &                         , CALLEDBY_T2D
     &                         , ZRL%R                    ! reference level [m+NN]
     &                        )
!
!
! LEOPRD aus T2D, GRAFCOUNT in sisyphe.f
! LEOPR = GRAFCOUNT in sisyphe.F
!
!
! NO QUASI BUBBLE SUPPORTED
        IF(IELMH_SIS.NE.11) THEN
          WRITE(*,*)'ONLY LINEAR 2D ELEMENTS ARE SUPPORTED'
          WRITE(*,*)'NESTOR -STOP, PLEASE START WITHOUT'
          WRITE(*,*)'NESTOR OR USE ANOTHER ELEMENT TYPE'
          WRITE(*,*)'ACTUAL ELEMENT TYPE OF ZF: ',IELMH_SIS
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
!     CALL FROM WITHIN BEDLOAD_MAIN
!
        IF (NESTOR.EQV..TRUE.) THEN
          DTS = DT/NSOUS
          ALLOCATE(TMP_AVAIL(NPOIN,1,NSICLA))
          TMP_AVAIL = AVAIL(1:NPOIN,1:1,1:NSICLA)
          CALL INTERFACERUNNESTOR(  NPOIN      !  NUMBER OF POINTS (NODES)
     &                            , NSICLA     !  number of SIze CLAsses
     &                            , LT         !  Telemac time step
     &                            , DTS        !  duration of Sisyphe time step
     &                            , AT0        !  time
     &                            , ES(1:NPOIN,1)   !(non const.) thickness of active laver [m]
     &                            , ZF%R       !  bottom [m+NN]
     &                            , ZFCL_C     !  evolution per class per time step [m]
     &                            , TMP_AVAIL
     &                            , MESH%KNOLG%I    ! index list: Local to Global node index
     &                            , HN%R       !  water depth [m]                               ! Itera
     &                           )
          DEALLOCATE(TMP_AVAIL)
        ENDIF
!
!
      ELSE
!
!     ERROR
!
        WRITE(LU,*) 'BAD OPTION FOR NESTOR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
