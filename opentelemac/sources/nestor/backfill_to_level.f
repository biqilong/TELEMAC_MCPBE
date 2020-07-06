!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Backfill_to_level              !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(   A, F, dt_ts, z_sis, dzCL_sis, time, m   )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  ParallelComputing, nGrainClass, ipid
     &                      , lim_dzts
!
      USE INTERFACE_PARALLEL, ONLY : P_ISUM, P_MAX
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  InfoMessage
     &                               , ErrMsgAndStop
     &                               , Dealloc_Dump_Field
     &                               , Set_RefLevel_by_Profiles
#endif
!
      IMPLICIT NONE
!
      TYPE(t_Action),INTENT(INOUT)   :: A
      TYPE(t_Field) ,INTENT(INOUT)   :: F
      REAL (KIND=R8),INTENT(IN)      :: dt_ts        ! time-step-duration  [ s ]
      REAL (KIND=R8),INTENT(IN)      :: z_sis(:)     ! bottom [m+NN] at time (assumed-shape array)
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT)   :: dzCL_sis(:)
      REAL (KIND=R8),INTENT(IN)      :: time         !  time [s]
      INTEGER       ,INTENT(IN)      :: m            ! number of Action
!
#ifndef NESTOR_INTERFACES
!
      !------- local variables ---------------
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
      INTEGER            :: i, iCL, iMesh, status
      REAL (KIND=R8)     :: dz_ts, dzFill, remainingHightToFill
      CHARACTER  (16)    :: rCh              ! to store a real value as string
!
!      dbug WRITE(6,*)'?>-------  SR Backfill_to_level ------------'
      SRname%s = "Backfill_to_level"   !> subroutine name
!
!
      !_____________________________________________________________
      !                                                           __|
      !                                                        __|
      IF( A%FirstTimeActive )  THEN  !________________________|
!
        A%State = 1     !> 1 = Action currently active
!
        IF(  A%ReferenceLevel(1:8) == 'WATERLVL') THEN
          ALLOCATE( F%refZ( F%nNodes ), stat=status)
          F%refZ(:) = 9999.9D0
          CALL Set_RefLevel_by_Waterlevel( F, A, m )   ! the result is F%refZ(:)
        ELSE IF(  A%ReferenceLevel(1:8) == 'SECTIONS') THEN
          ALLOCATE( F%refZ( F%nNodes ), stat=status)
          ALLOCATE( F%km(   F%nNodes ), stat=status)
          F%refZ(:) = 9999.9D0
          F%km(:)   = 9999.9D0
          CALL Set_RefLevel_by_Profiles( F )   ! the result is F%refZ(:)
        ENDIF
!
        ALLOCATE( F%NodeToDump( F%nNodes ), stat=status)
        F%NodeToDump(:) = .FALSE. ! initialisation
!
        F%nNodeToDump   = 0
        DO i=1, F%nNodes         ! mark nodes to dig
          iMesh = F%Node(i)      ! mesh index of field node
          IF( z_sis(iMesh) < (F%refZ(i) - A%CritDepth) ) THEN
            F%NodeToDump(i) = .TRUE.
            F%nNodeToDump   = F%nNodeToDump + 1
          ENDIF
        ENDDO
!                                           ______________________________________________________
        IF( A%DumpRate < 0.0D0 ) THEN      ! The backfilling is controlled by the period of time  |
!
          A%nts = INT( (A%TimeEnd - time) / dt_ts ) !> calculate number of time
                                                    !  steps (nts) to fulfil the Action
          IF( A%nts < 1 ) Call ErrMsgAndStop( " "
     &    ,"reason:  period for this action is too short "
     &    ," ","occured in action number:", m, SRname, ipid )
!
          ALLOCATE( F%dz( F%nNodes ), stat=status)
          F%dz(:) = 9999.9D0
          F%dz(:) = (F%refZ(:) - A%CritDepth) - z_sis(F%Node(:))
          F%dz(:) = F%dz(:) / DBLE( A%nts )  ! convert total F(n)%dz to F(n)%dz per time step
!
          A%MaxDump_dz_ts =  MAXVAL( F%dz(:) )
          IF( ParallelComputing )
     &    A%MaxDump_dz_ts = P_MAX( A%MaxDump_dz_ts )
!
          A%tsCount = 0
!
        ENDIF ! The backfilling is controlled by the period of time
!
!                                           ______________________________________________________
        IF( A%DumpRate > 0.0D0 ) THEN      ! The backfilling is controlled by the DumpRate        |
          A%MaxDump_dz_ts = dt_ts * A%DumpRate         !> max hight to fill during one time step
        ENDIF ! The backfilling is controlled by the DumpRate
!
        CALL InfoMessage( A, m, time )
!
        IF( A%MaxDump_dz_ts > lim_dzts ) THEN
          WRITE(rCh,'(F16.8)') A%MaxDump_dz_ts !> convert real value to string and then
          WRITE(rCh,'(16A)') adjustl(rCh)      !  convert string to left-aligned string
          Call ErrMsgAndStop(
     &     "reason: Change of bottom level per time step "
     &    ,"        dzts = "//rCh//"[m] is too big.      "
     &    ,"        lim_dzts = 0.1[m]                    "
     &    ,"occured in Action: ", m, SRname, ipid      )
        ENDIF
!
!
!
        A%FirstTimeActive = .FALSE.
!             ________________________________________________
      ENDIF  ! FirstTimeActive                                |__
      !                                                          |__
      !_____________________________________________________________|
!
!                                        +-------------------------------------------------+
      IF( A%DumpRate > 0.0D0 ) THEN      !  The backfilling is controlled by the DumpRate  |
                                         !                                                 |
                                         !                                                 |
!                                        +-------------------------------------------------+
      IF( F%nNodeToDump > 0 ) THEN
!
        dz_ts  = dt_ts * A%DumpRate         !> Hight to fill during one time step
        DO i=1, F%nNodes
          IF( .NOT. F%NodeToDump(i) ) CYCLE
!
          iMesh  = F%Node(i)  ! mesh index of field node
          dzFill = dz_ts
          remainingHightToFill = F%refZ(i)-A%CritDepth - z_sis(iMesh)
!
          IF( remainingHightToFill < dzFill ) THEN
            dzFill          = remainingHightToFill
            F%NodeToDump(i) = .FALSE.                !> No more digging for this node.
            F%nNodeToDump   = F%nNodeToDump - 1
          ENDIF
!
!         ----- dump one time step -----
          DO iCL=1, nGrainClass
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)     !> =   change of z per time step per class by morphodynamic
     &                               + dzFill * A%GrainClass(iCL) !    + change of z per time step per class by Backfill_to_level
          ENDDO
!
        ENDDO  ! i=1, nNodes
      ENDIF  ! IF F%nNodeToDump > 0
!                                        +-------------------------------------------------+
                                         !                                                 |
                                         !                                                 |
      ENDIF ! IF(A%DumpRate > 0.0D0 )    !  The backfilling is controlled by the DumpRate  |
!                                        +-------------------------------------------------+
!
!                                        +------------------------------------------------------+
      IF( A%DumpRate < 0.0D0 ) THEN      !  The backfilling is controlled by the period of time |
                                         !                                                      |
                                         !                                                      |
!                                        +------------------------------------------------------+
        A%tsCount = A%tsCount + 1
!
!       ----- dump one time step -----
        DO i=1, F%nNodes
          IF( .NOT. F%NodeToDump(i) ) CYCLE
          DO iCL=1, nGrainClass
            iMesh = F%Node(i)     !> iMesh = mesh index of field node
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                               + A%GrainClass(iCL) * F%dz(i)
          ENDDO
        ENDDO !-----------------------------------------------
!
        IF( A%tsCount == A%nts ) THEN
          F%nNodeToDump = 0
        ENDIF
!                                        +------------------------------------------------------+
                                         !                                                      |
                                         !                                                      |
      ENDIF ! IF(A%DumpRate < 0.0D0 )    ! The backfilling is controlled by the period of time  |
!                                        +------------------------------------------------------+
!
!
!     ============== finalise action ===================================
!
      A%nNodeToDump = F%nNodeToDump
!
      IF( ParallelComputing ) A%nNodeToDump = P_ISUM( A%nNodeToDump )
!
      IF(      time >= A%TimeEnd
!     &   .AND. TotalNumNodeToDig > 0  ) Call ErrMsgAndStop(
     &   .AND. A%nNodeToDump > 0  ) Call ErrMsgAndStop(
     &   "reason:  The period specified is too short."
     &  ,"repair:  Increase the period of time or the DumpRate!"
     &  ," ","occured in Action number : ", m, SRname, ipid      )
!
!
!      IF( TotalNumNodeToDig  <=  0  ) THEN !> backfilling is accomplished
      IF( A%nNodeToDump  <=  0  ) THEN !> backfilling is accomplished
        A%State = 9                    !  9 = for ever inactive
        CALL Dealloc_Dump_Field( A )
        CALL InfoMessage( A, m, time )
      ENDIF
!
!      dbug WRITE(6,*)'?>-------  SR Backfill_to_level END --------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Backfill_to_level           !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
