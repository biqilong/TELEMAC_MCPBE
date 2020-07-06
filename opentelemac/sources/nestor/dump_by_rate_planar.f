!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dump_by_Rate_Planar            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A, dt_ts, z_sis, dzCL_sis, m )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  F, ParallelComputing, nGrainClass
      USE INTERFACE_PARALLEL, ONLY : P_DSUM, P_ISUM
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  Calculate_PlanarLevel
     &                               , Set_RefLevel_by_Profiles
     &                               , Set_RefLevel_by_Waterlevel
#endif
!     set nNTDu to 0
      IMPLICIT NONE
!
      TYPE(t_Action),INTENT(INOUT) :: A
      REAL (KIND=R8),INTENT(IN)    :: dt_ts        ! time-step-duration  [ s ]
      REAL (KIND=R8),INTENT(IN)    :: z_sis(:)     ! bottom [m+NN] at time (assumed-shape array)
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_sis(:)
      INTEGER       ,INTENT(IN)    :: m            ! number of Action

#ifndef NESTOR_INTERFACES
!
      !------- local variables ---------------
      INTEGER            :: i, iCL, iMesh, n, status
      INTEGER            :: nLessNodesToDump

      REAL (KIND=R8)     :: dz_ts, dzts
      REAL (KIND=R8)     :: sumDump_ts, sumInput_ts
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Rate_Planar ----------'
      SRname%s = "Dump_by_Rate_Planar" ! subroutine name
!
      n = A%FieldDumpID
      !_____________________________________________________________
      !                                                           __|
      !                                  Dump_by_Rate_Planar   __|
      IF( A%FirstTimeActive )  THEN  !________________________|
!
        ALLOCATE( F(n)%Z( F(n)%nNodes ), stat=status)
        F(n)%Z(:) = z_sis( F(n)%Node(:) ) !>  F(n)%Node(i)= mesh index of field node
!
        ALLOCATE( F(n)%dZ( F(n)%nNodes ), stat=status)
        F(n)%dZ(:) = 1234.567890D0

        ALLOCATE( F(n)%NodeToDump( F(n)%nNodes ), stat=status)
        F(n)%NodeToDump(:) = .FALSE.
!
        IF(      A%ReferenceLevel(1:8) == 'WATERLVL') THEN
          ALLOCATE( F(n)%refZ( F(n)%nNodes ), stat=status)
          F(n)%refZ = 1234.567890D0
          CALL Set_RefLevel_by_Waterlevel( F(n), A, m )   ! the result is F(n)%refZ(:)
        ELSE IF( A%ReferenceLevel(1:8) == 'SECTIONS') THEN
          ALLOCATE( F(n)%refZ( F(n)%nNodes ), stat=status)
          ALLOCATE( F(n)%km( F(n)%nNodes )  , stat=status)
          F(n)%refZ(:) = 1234.567890D0
          F(n)%km(:)   = 1234.567890D0
          CALL Set_RefLevel_by_Profiles( F(n) )           ! the result is F(n)%refZ(:)
        ENDIF
!
        CALL Calculate_PlanarLevel( F(n), -A%DigVolume, 1 ) !> 1 => dump; The result is
                                                            !  total F%dz(:)and F%NodeToDump(:).
                                                            !  We use A%DigVolume because we
                                                            !  want to dump the DigVolume.
                                                            !  DigVolume is based upon the current shape
                                                            !  of the bottom. The volume which will be
                                                            !  dug in the end will be changed due to
                                                            !  morphodynamic processes while digging.
!
        F(n)%nNodeToDump = COUNT( F(n)%NodeToDump(:) )
!
        A%MaxDump_dz_ts = A%DumpRate * dt_ts
!
        A%fillArea = SUM( F(n)%NodeArea(:), MASK=F(n)%NodeToDump(:) )
!
        IF(ParallelComputing) THEN
          A%fillArea       = P_DSUM( A%fillArea )
          F(n)%nNodeToDump = P_ISUM( F(n)%nNodeToDump )
        ENDIF
!
        A%SumDumped      = 0.0D0
        A%InputDumpField = 0.0D0
!
        !IF( A%Solo ) A%FirstTimeActive = .FALSE. is set by the calling dig action (Dump_by_Rate_Planar is never solo)
!
!             ________________________________________________
      ENDIF  ! FirstTimeActive                                |__
      !                                Dump_by_Rate_Planar       |__
      !_____________________________________________________________|
!
!
!
!
      IF( .NOT. A%FirstTimeActive )  THEN
!
        !> Before dumping we calc. the amount of sediment that was
        !  transported by morphodynamic during the last time step
        !  into or out of the dump nodes WHILE THEY WERE ACTIVE.
        !  I case there is a further action operating at the same time
        !  on this field and it is carried out already (depends on the
        !  internal order of execution), then it will
        !  appear here as sumInput_ts too.
        sumInput_ts = 0.0D0
        DO iCL=1, nGrainClass       !  Only nodes below the planar
          DO i=1, F(n)%nNodes       !  level (NodeToDump is TRUE) are included for it.
            iMesh = F(n)%Node(i)    !> mesh index of field node
            IF( F(n)%NodeToDump(i) ) THEN
              sumInput_ts =   sumInput_ts
     &                      + dzCL_sis(iCL)%R(iMesh) * F(n)%NodeArea(i)
            ENDIF
          ENDDO
        ENDDO
!
!
        !> --- calc hight to dump during current time step ----
        dz_ts = dt_ts * A%DumpRate
!        WRITE(6,*)' ?>     dz_ts 0 ',dz_ts  ! debug
        IF( A%DumpVolume  <  dz_ts * A%fillArea ) THEN
           dz_ts = A%DumpVolume / A%fillArea
!           WRITE(6,*)' ?>-----dz_ts 01   ',dz_ts  ! debug
        ENDIF
!
!
!
        dzts             = dz_ts
        sumDump_ts       = 0.0D0
        nLessNodesToDump = 0
!
        DO i=1, F(n)%nNodes  !----- dump one time step -----------------
!
          IF( .NOT. F(n)%NodeToDump(i) ) CYCLE
!
          iMesh = F(n)%Node(i)     !> mesh index of field node
          dz_ts = dzts
!
          IF( F(n)%dz(i) <  dz_ts ) THEN
            dz_ts = F(n)%dz(i)
            F(n)%NodeToDump(i) = .FALSE.
            nLessNodesToDump = nLessNodesToDump + 1
!         WRITE(6,*)' ?>-----dz_ts XX ',dz_ts,'nNTDu',F(n)%NodeToDump(i)! debug
          ENDIF
!
          DO iCL=1, nGrainClass          !< dumping happens here
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                               + A%GrainClass(iCL) * dz_ts
          ENDDO
!
          F(n)%dz(i) = F(n)%dz(i) - dz_ts
!
          sumDump_ts = sumDump_ts + dz_ts * F(n)%NodeArea(i)
!
        ENDDO !-- i=1, F(n)%nNodes--------------------------------------
!
!
!
        A%fillArea = SUM( F(n)%NodeArea(:), MASK=F(n)%NodeToDump(:) )
!
        IF(ParallelComputing) THEN
          A%fillArea       = P_DSUM( A%fillArea )
          sumDump_ts       = P_DSUM( sumDump_ts )
          nLessNodesToDump = P_ISUM( nLessNodesToDump )
          sumInput_ts      = P_DSUM( sumInput_ts )
        ENDIF
!
        A%InputDumpField = A%InputDumpField + sumInput_ts
        A%SumDumped      = A%SumDumped + sumDump_ts
!
        F(n)%nNodeToDump = F(n)%nNodeToDump - nLessNodesToDump
!
!           WRITE(6,*)' ?>  1 DumpVolume = ', A%DumpVolume    ! debug
!           WRITE(6,*)' ?>  1 sumDump_ts = ', sumDump_ts      ! debug
!
        A%DumpVolume = A%DumpVolume - sumDump_ts
!
        IF(       ABS( A%DumpVolume ) <  1.0D-32
     &      .AND. A%nNodeToDig        == 0       ) THEN
!           WRITE(6,*)' ?>  2 Set nNTDu to O  '               ! debug
          F(n)%nNodeToDump   = 0
          F(n)%NodeToDump(:) = .FALSE.
        ENDIF
!
!           WRITE(6,*)' ?>  2 DumpVolume = ',A%DumpVolume     ! debug
!           WRITE(6,*)' ?>  2  NTDu',F(n)%nNodeToDump         ! debug
!     &                    ,'  NTDi',F(k)%nNodeToDig         ! debug
!     &                    ,'  NTDi',A%nNodeToDig             ! debug
!
        IF(       F(n)%nNodeToDump == 0
     &      .AND. A%nNodeToDig     == 0
     &      .AND. A%DumpVolume     >  1.0D-9 ) THEN
          F(n)%Z(:) = z_sis( F(n)%Node(:) ) !>  F(n)%Node(i)= mesh index of field node
          F(n)%NodeToDump(:) = .FALSE.
          CALL Calculate_PlanarLevel( F(n), A%DumpVolume, 1 ) !> 1 => dump;  The result is
          F(n)%nNodeToDump = COUNT( F(n)%NodeToDump(:) )      !  total F%dz(:)and F%NodeToDump(:).
          A%fillArea = SUM( F(n)%NodeArea(:), MASK=F(n)%NodeToDump(:) )
!
          IF(ParallelComputing) THEN
            A%fillArea       = P_DSUM( A%fillArea )
            F(n)%nNodeToDump = P_ISUM( F(n)%nNodeToDump )
          ENDIF
!
!           WRITE(6,*)' ?>xx2 DumpVolume = ',A%DumpVolume  ! debug
!     &              ,'NTDu',F(n)%nNodeToDump              ! debug
!
        ENDIF
!
!
        IF(       F(n)%nNodeToDump == 0
     &      .AND. A%nNodeToDig     == 0
     &      .AND. A%DumpVolume     <= 1.0D-9 ) THEN
          A%DumpVolume = -123.4567890D0
        ENDIF
!
!
      ENDIF !( .NOT. A%FirstTimeActive )
!
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Rate_Planar END-------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Dump_by_Rate_Planar         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
