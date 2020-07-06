!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dig_by_Time                    !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(   A, F, dt_ts, z_sis, dzCL_sis
     &  , AVAIL, ES1_sis, time, KNOLG, m   )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :   ParallelComputing, nGrainClass, ipid
!
      USE INTERFACE_PARALLEL, ONLY : P_DSUM,P_ISUM
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  InfoMessage
     &                               , Dump_by_Rate
     &                               , Dump_by_Time
     &                               , ErrMsgAndStop
     &                               , Dealloc_Dump_Field
     &                               , Dump_by_Time_Planar
     &                               , Dump_by_Rate_Planar
#endif
!
      IMPLICIT NONE
!
      TYPE(t_Action),INTENT(INOUT)   :: A
      TYPE(t_Field) ,INTENT(INOUT)   :: F            ! Dig Field
      REAL (KIND=R8),INTENT(IN)      :: dt_ts
      REAL (KIND=R8),INTENT(IN)      :: z_sis(:)     ! bottom [m+NN] at time (assumed-shape array)
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT)   :: dzCL_sis(:)
      REAL (KIND=R8),INTENT(IN)      :: AVAIL(:,:,:) ! debug test!  assumed-shape array
      REAL (KIND=R8),INTENT(IN)      :: ES1_sis(:)   ! (non const.) thickness of active laver  [m]
      REAL (KIND=R8),INTENT(IN)      :: time         !  time [s]
      INTEGER       ,INTENT(IN)      :: KNOLG(*)     ! index list: Local to Global node index
      INTEGER       ,INTENT(IN)      :: m            ! number of Action
!
#ifndef  NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
!
!
      INTEGER            :: i, iCL
      INTEGER            :: iM  !  index Mesh
!     INTEGER            :: nodeIndex, status
!     INTEGER            :: nLessNodesToDig = 0
!     REAL (KIND=R8)     :: dzDig_ts
      REAL (KIND=R8)     :: dzDig
      REAL (KIND=R8)     :: dzEvo_sis
      REAL (KIND=R8)     :: sumInput_ts
      !REAL (KIND=R8)     :: x, y
!     REAL (KIND=R8)     :: remainingDepthToDig
!     REAL (KIND=R8)     :: maxDump_dz_ts
!     INTEGER, SAVE      :: countDig = 0

!
      REAL (KIND=R8)                      :: heap    !> total dug volume of all FieldNodes
      REAL (KIND=R8),ALLOCATABLE                     !  during the current timestep
     &             ,SAVE ,DIMENSION   (:) :: heapCL  !> total dug volume per grain class of all
      REAL (KIND=R8),ALLOCATABLE                     !  FieldNodes during the current timestep
     &             ,SAVE ,DIMENSION   (:) :: layCL
!
      CHARACTER  (16) :: S1,S2,S3                    ! to convert num. values to string
      CHARACTER (128) :: S4,S5,S6,S7,S8              ! to create the error message
!
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
!      dbug WRITE(6,*)'?>-------  SR Dig_by_Time ------------------'
      SRname%s = "Dig_by_Time"         ! subroutine name
!
      IF( .NOT. ALLOCATED(layCL) ) ALLOCATE(  layCL( nGrainClass ))
      IF( .NOT. ALLOCATED(heapCL)) ALLOCATE( heapCL( nGrainClass ))
!
      !DO i=1, F%nNodes                                                        ! debug
      !    iM = F%Node(i)                                                   ! debug
      !    WRITE(*,*)'?>    dzCL_sis(1)%R(i)=', dzCL_sis(1)%R(iM)           ! debug
      !    WRITE(*,*)'?>                     ', dzCL_sis(2)%R(iM)           ! debug
      !    WRITE(*,*)'?>                     ', dzCL_sis(3)%R(iM)           ! debug
      !ENDDO                                                                   ! debug
!
!      __________________________________________________________
      !                                                        __|
      !                                                     __|
      IF( A%FirstTimeActive )  THEN  !_____________________|
        A%State = 1     ! 1 = Action currently active
!
        A%nts = INT( (A%TimeEnd - time) / dt_ts ) !> calculate number
                                 !  of time steps (nts) to fulfill the Action
!
        IF( A%nts < 1 ) Call ErrMsgAndStop( " "
     &    ,"reason:  period for this action is too short "
     &    ," ","occured in action number:", m, SRname, ipid )
!
        A%dzTot = A%DigVolume / F%Area         ! change of z to fulfil the Action
        A%dz_ts = A%dzTot / DBLE( A%nts )      ! change of z per time step
        IF(.NOT.A%DigPlanar) A%MaxDig_dz_ts = A%dz_ts
!
        A%DumpVolume  =   0.0D0
        A%MovedVolume =   0.0D0
        A%SaveTime    = -11.1D0
        F%nNodeToDig  = F%nNodes
        A%nNodeToDig  = F%nNodeToDig
        IF( ParallelComputing ) A%nNodeToDig = P_ISUM( A%nNodeToDig )
!
        SELECT CASE( A%DumpMode )
          CASE( 10 ) !> Dump_by_Time
            CALL Dump_by_Time(   A, dt_ts, dzCL_sis, time, m )
!
          CASE( 11 ) !> Dump_by_Time_Planar
            CALL Dump_by_Time_Planar(   A, dt_ts, z_sis
     &                                , dzCL_sis, time, m )
          CASE( 20 ) !> Dump_by_Rate
            CALL Dump_by_Rate( A, dt_ts, dzCL_sis )
          CASE( 21 ) !> Dump_by_Rate_Planar
            CALL Dump_by_Rate_Planar( A, dt_ts, z_sis, dzCL_sis, m )
          CASE DEFAULT
        END SELECT
!
        CALL InfoMessage( A, m, time )
!
        A%tsCount         = 0
        A%FirstTimeActive = .FALSE.
        A%InputDigField   = 0.0D0
!
!             _____________________________________________
      ENDIF  !(IF A%FirstTimeActive )                      |__
      !                                                       |__
      !__________________________________________________________|
!
!
      ! The point where Nestor is linked in the Sisyphe time loop we have
      ! the following situation:
      ! 1.) The active layer respectively the AVAIL array
      !     has the state of the previous time step.
      ! 2.) The bottom level (z_sis) has the state of the previous time step.
      ! 3.) The evolution per class (dzCL_sis) is already calculated
      !     for the current time step. But it's not mixed into the
      !     active layer yet.
      ! To know what sediment mixture the digger will grab, we combine
      ! the active layer and the evolution.
!
      sumInput_ts = 0.0D0
!
      dzDig = A%dz_ts
!
!
      heapCL(:) = 0.0D0 ! new time step => reset heap
      IF( A%tsCount < A%nts  ) THEN
!
!       !> Before digging we calc. the amount of sediment that was
        !  transported by morphodynamic during the last time step (ts)
        !  into (or out of) the dig nodes WHILE THEY WERE ACTIVE.
        !  I case there is a further action operating at the same time
        !  on this field and it is carried out already (what depends on the
        !  internal order of execution), then it will
        !  appear here as sumInput_ts too.
        DO iCL=1, nGrainClass
          DO i=1, F%nNodes
            iM = F%Node(i)    !> mesh index of field node
            sumInput_ts =    sumInput_ts
     &                     + dzCL_sis(iCL)%R(iM) * F%NodeArea(i)
          ENDDO
        ENDDO
!
!
!
        DO i=1, F%nNodes  ! loop over FieldNodes
          iM = F%Node(i)
!
          !IF( .NOT. F%NodeToDig(i) ) CYCLE
          dzEvo_sis  =  0.0D0
          DO iCL=1, nGrainClass
            dzEvo_sis  =   dzEvo_sis + dzCL_sis(iCL)%R(iM)     !> Sum of evolution of all classes
                                                               !  at node at current time step
            layCL(iCL) =   dzCL_sis(iCL)%R(iM)           !>  Thickness of evolution per class
     &                   + AVAIL(iM,1,iCL) * ES1_sis(iM) ! + thickness of class in active layer
                           !...........................!----- convert fraction to thickness
          ENDDO
!
          IF( dzDig + dzEvo_sis <= -ES1_sis(iM) ) THEN  !Digger exceeds active layer thickness
            WRITE(S2,'(E12.4)') ES1_sis(iM)        !> convert real value to string
            WRITE(S3,'(E12.4)') dzDig + dzEvo_sis  !> convert real value to string
            IF( ParallelComputing ) iM = KNOLG(iM) !> get the global mesh index
            WRITE(S1,'(I9)') iM                    !> convert integer value to string
            S1 = adjustl(S1)
            S2 = adjustl(S2)
            S3 = adjustl(S3)
            WRITE(S4,*)"At node "//TRIM(S1)//" dzDig+dzEvol exceeds the"
            WRITE(S5,*)"active layer thickness (="//TRIM(S2)//"[m])."
            WRITE(S6,*)"dzDig+dzEvol = "//TRIM(S3)//"[m] is too much."
            WRITE(S7,*)"==> increase the time slot or active layer"
            WRITE(S8,*)"thickness or reduce the time step or ..."
            Call ErrMsgAndStop(
     &                  "reason:"//TRIM(S4)//S5
     &                 ,"       "//TRIM(S6)
     &                 ,"       "//TRIM(S7)//S8
     &                 ,"occured in Action: ", m, SRname, ipid)
          ENDIF
!
!
          layCL(:) = layCL(:) / (dzEvo_sis + ES1_sis(iM)) !> Convert thickness to fraction
!
!
          DO iCL=1, nGrainClass                                    !> Digging happens here
            dzCL_sis(iCL)%R(iM) =   dzCL_sis(iCL)%R(iM)            !> We calculate the mixture which
     &                               + layCL(iCL) * dzDig          !  is left in the ground
          ENDDO
!
!
          heapCL(:) = heapCL(:) + layCL(:) *(-dzDig) * F%NodeArea(i) !> During the current time step of all nodes
                                                                     !  we collect for each class the dug material
                                                                     !  and put it in heaps per class
        ENDDO  !  loop over FieldNodes
!
      ENDIF !( A%tsCount <= A%nts  )
!
      A%tsCount = A%tsCount + 1
!
!
      !> Collect the total dug volume since the Action started
      !  and calculate the grain composition
!
      IF( ParallelComputing ) THEN
        DO iCL=1, nGrainClass
          heapCL(iCL) = P_DSUM( heapCL(iCL) )
        ENDDO
        sumInput_ts = P_DSUM( sumInput_ts )
      ENDIF
!
      A%InputDigField = A%InputDigField + sumInput_ts
!
      heap = SUM( heapCL(:) )  !> Total volume of dug material at current time step
!
      !> Add per class   volume of dug material and DumpVolume
      heapCL(:) = heapCL(:) + A%GrainClass(:) * A%DumpVolume
                              !............................!--- convert fraction to volume
!
      A%DumpVolume  = A%DumpVolume  + heap
!
!
      A%MovedVolume = A%MovedVolume + heap
!
!
!
      IF( A%DumpVolume  <=  0.0D0  ) THEN
        A%GrainClass(:) = heapCL(:) / heap
      ELSE                !..............!------------- convert thickness to fraction
        A%GrainClass(:) = heapCL(:) / A%DumpVolume
      ENDIF               !......................!----- convert thickness to fraction
!
       !IF(abs(A%DumpVolume - SUM(heapCL(:)))  >=  0.00000001D0 ) THEN ! debug
       !  WRITE(*,*)'?>  A%DumpVolume    =', A%DumpVolume              ! debug
       !  WRITE(*,*)'?>  heap            =', heap                      ! debug
       !  WRITE(*,*)'?>  A%GrainClass(:) =', A%GrainClass(:)           ! debug
       !  !STOP                                                        ! debug
       !ENDIF                                                          ! debug
!
!
      SELECT CASE( A%DumpMode )
        CASE( 10 ) !> Dump_by_Time
          CALL Dump_by_Time(   A, dt_ts, dzCL_sis, time, m )
        CASE( 11 ) !> Dump_by_Time_Planar
          IF( A%tsCount <= A%nts )
     &      CALL Dump_by_Time_Planar(   A, dt_ts, z_sis
     &                                , dzCL_sis, time, m )
        CASE( 20 ) !> Dump_by_Rate
          IF( A%DumpVolume  >  0.0D0  )
     &      CALL Dump_by_Rate( A, dt_ts, dzCL_sis )
        CASE( 21 ) !> Dump_by_Rate_Planar
          CALL Dump_by_Rate_Planar( A, dt_ts, z_sis, dzCL_sis, m )
        CASE DEFAULT
      END SELECT
!
      IF( A%tsCount  >=  A%nts ) THEN      !>  when all is dug
!
        F%nNodeToDig  = 0
        A%nNodeToDig  = 0
!
        IF( A%SaveTime < 0.D0 ) A%SaveTime = time
!
        SELECT CASE( A%DumpMode )
          CASE ( 10 )         !> Dump_by_Time
            A%State = 9                          !> 9 = for ever inactive
            CALL InfoMessage( A, m, time )
          CASE ( 11 )         !> Dump_by_Time_Planar
            A%State = 9                          !> 9 = for ever inactive
            CALL InfoMessage( A, m, time )
          CASE ( 20 )         !> Dump_by_Rate
            IF( A%DumpVolume <= 0.0D0  ) THEN
              A%State = 9                        !> 9 = for ever inactive
              CALL InfoMessage( A, m, time )
            ENDIF
          CASE ( 21 )         !> Dump_by_Rate_Planar
            IF( A%DumpVolume <= 0.0D0  ) THEN
              A%State = 9                        !> 9 = for ever inactive
              CALL InfoMessage( A, m, time )
            ENDIF
          CASE ( -1 )        !> only digging no dumping
            A%State = 9                          !> 9 = for ever inactive
            CALL InfoMessage( A, m, time )
        END SELECT
      ENDIF !( A%tsCount  >=  A%nts )
!
!
      IF( A%State == 2 .OR. A%State == 9  ) THEN   !> 2 = temporary inactive
        DEALLOCATE( layCL )                        !  9 = for ever inactive
        DEALLOCATE( heapCL )
        CALL Dealloc_Dump_Field( A )
      ENDIF
!
!      dbug WRITE(6,*)'?>-------  SR Dig_by_Time END --------------'
      RETURN
#endif
      END SUBROUTINE Dig_by_Time                 !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
