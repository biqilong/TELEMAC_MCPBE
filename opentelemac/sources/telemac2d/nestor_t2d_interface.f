!                       ******************************
                        SUBROUTINE NESTOR_T2D_INTERFACE
!                       ******************************
!
     &(OPTION)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    COUPLING WITH NESTOR
!
!+  CALL THE NESTOR INTEFACE SUBROUTINE
!
!history  B. GLANDER (BAW)
!+        28/11/2017
!+        V7P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|             |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY :  NPOIN, IELMH
     &                                  , T2D_FILES,T2NACT,T2NPOL
     &                                  , T2NREF,T2NRST
     &                                  , ZF, HN, T13, T14
     &                                  , MESH, MSK, MASKEL
     &                                  , LT, DT, AT, LEOPRD
     &                                  , MARDAT, MARTIM
     &                                  , NES_DZ, AVAIL
     &                                  , ZRL

      USE DECLARATIONS_SPECIAL
!
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: OPTION
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ::  NSICLA         ! Number of grain SIze CLAsses (dummy)

      DOUBLE PRECISION :: MOFAC  ! MOrphological FACtor (dummy)
      DOUBLE PRECISION :: ES1_SIS
      LOGICAL          :: CALLEDBY_T2D
!-----------------------------------------------------------------------
!
      NSICLA       = 1        ! number of classes    in context with t2d always 1
      MOFAC        = 1.0D0    ! morphological factor in context with t2d always 1
! 
      CALLEDBY_T2D = .TRUE.
!
      IF(OPTION.EQ.1) THEN

!       NO QUASI BUBBLE SUPPORTED
        IF(IELMH.NE.11) THEN
          WRITE(LU,*)'ONLY LINEAR 2D ELEMENTS ARE SUPPORTED'
          WRITE(LU,*)'NESTOR -STOP, PLEASE START WITHOUT'
          WRITE(LU,*)'NESTOR OR USE ANOTHER ELEMENT TYPE'
          WRITE(LU,*)'ACTUAL ELEMENT TYPE OF ZF: ',IELMH
          CALL PLANTE(1)
          STOP
        ENDIF

        WRITE(LU,*)' -------- initialisation of Nestor ----------'

        !  generate the bief objekt NES_DZ which contains the
        !  bottom modification (per time step) caused by NESTOR
        CALL ALLBLO(NES_DZ,'NES_DZ')
        CALL BIEF_ALLVEC_IN_BLOCK
     &           (NES_DZ,NSICLA,1,'NES_DZ',IELMH,1,2,MESH)
        CALL OS('X=0     ', X=NES_DZ )

        ALLOCATE(AVAIL(NPOIN,1,NSICLA)) ! here a dummy,  in Sisyhe: fraction of each class for each layer (npoin,nomblay,nsicla)
!
!       ======= INITIALISATION =============
!
        AVAIL(:,:,:) = 1.0D0
!
!        _____________________________________________________________________
!       /________ calculation of Node-areas for all nodes of grid ___________/
        ! calculation of Node-areas in T13        for parallel: here the
        !                                         node-areas of the interface-nodes
        !                                         are accordingly reduced
        CALL VECTOR(T13,'=','MASBAS          '
     &        ,IELMH,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)


        CALL INTERFACEINITNESTOR(    NCSIZE, IPID, NPOIN
     &                             , NSICLA                   ! Number of SIze CLAsses (dummy)
     &                             , MARDAT, MARTIM           ! start time: DATe , TIMe
     &                             , MOFAC                    ! MOrphological FACtor (dummy)
     &                             , LEOPRD                   ! period of graphical outputs
     &                             , MESH%X%R, MESH%Y%R
     &                             , T13%R                    ! node area [m**2]
     &                             , MAXVAL(MESH%KNOLG%I(:))  ! max index
     &                             , ZF%R                     ! bottom at time 0 [m+NN]
     &                             , LU                       ! logical unit for standard output
     &                             , T2D_FILES(T2NACT)%LU     ! logical unit to NESTOR ACTION FILE
     &                             , T2D_FILES(T2NPOL)%LU     ! logical unit to NESTOR POLYGON FILE
     &                             , T2D_FILES(T2NREF)%LU     ! logical unit to NESTOR SURFACE REFERENCE FILE
     &                             , T2D_FILES(T2NRST)%LU     ! logical unit to NESTOR RESTART FILE
     &                             , CALLEDBY_T2D
     &                             , ZRL%R                    ! reference level [m+NN]
     &                            )
!
!
      ELSEIF(OPTION.EQ.2) THEN
        !#########################################
        !
        ! Run Nestor   CALL FROM WITHIN TELEMAC2D
        !
        !#########################################
!
        ES1_SIS = 0.1D0     !> [m] This value delimits the vertical movement of the bottom per time step
                            !  In case of exceedance Nestor will stop the computation.
        T14%R(:) = ES1_SIS  !> In case of pure hydrodynamic computation a single value would be enough
                            !  but due to compatibility reasons an array is needed at this point.
                            !  ( When Nestor is coupled with Sisyphe    ES1_sis(:)   represents 
                            !    the   active layer thickness   at each node.)
!
        CALL INTERFACERUNNESTOR(   NPOIN           !  Number of POINts (NODES)
     &                           , NSICLA          !  Number of SIze CLAsses (dummy)
     &                           , LT              !  Telemac time step
     &                           , DT              !  Duration of Telemac time step
     &                           , AT              !  time
     &                           , T14%R           !  limit of the vertical movement of the bottom per time step
     &                           , ZF%R            !  bottom [m+NN]
     &                           , NES_DZ          !  bottom cange per time step by nestor [m]
     &                           , AVAIL(1:NPOIN,1,1:NSICLA)    !
     &                           , MESH%KNOLG%I    !  index list: Local to Global node index
     &                           , HN%R            !  water depth [m]
     &                          )
!
        CALL OS('X=X+Y   ', X=ZF, Y=NES_DZ%ADR(1)%P)   ! well  it happens here
!
        CALL OS('X=0     ', X=NES_DZ )
!
!
      ELSE
!
!       ------ ERROR: the subroutine is called with a bad value for the parameter "OPTION"
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
