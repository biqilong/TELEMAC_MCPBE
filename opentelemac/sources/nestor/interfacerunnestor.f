!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  InterFaceRunNestor             !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (  NPOIN, NSICLA, LT, DTS, AT0
     &  , ES1, ZF, ZFCL_C, AVAIL, KNOLG
     &  , HN                                !  water depth [m]
     & )
!
      USE BIEF
      USE m_TypeDefs_InterFace
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  MainNestor
#endif
!
!
      IMPLICIT NONE
!
      INTEGER       ,INTENT(IN)    ::   NPOIN     ! Number of points (Nodes)
     &                                , NSICLA    ! Number of SIze CLAsses (=nGrainClass)
      INTEGER       ,INTENT(IN)    ::   LT        ! time-step Telemac
      REAL (KIND=R8),INTENT(IN)    ::   DTS, AT0  ! time-step-duration , time
      REAL (KIND=R8),INTENT(IN)
     &          ,DIMENSION (NPOIN) ::   ES1       ! (non const.) thickness of active laver  [m]
      REAL (KIND=R8),INTENT(IN)
     &          ,DIMENSION (NPOIN) ::   ZF        ! current bottom (at time AT0)
      TYPE(BIEF_OBJ),INTENT(INOUT) ::   ZFCL_C    ! bedload evolution for each sediment class
!
      REAL (KIND=R8),INTENT(IN)
     &              ,DIMENSION (NPOIN,1,NSICLA) :: AVAIL
!
      INTEGER       ,INTENT(IN)    :: KNOLG(*)    ! index list: Local to Global node index
!
      REAL (KIND=R8),INTENT(IN)
     &          ,DIMENSION (NPOIN) ::   HN        ! water depth (at time AT0)
!
!
!
#ifndef NESTOR_INTERFACES
!
!
      TYPE( t_PointerToArrayOfReals )
     & , ALLOCATABLE, SAVE , DIMENSION (:)      :: dzCL_sis
!
!
      !---------- local variables ---------------------
      INTEGER          :: i, status
!
      LOGICAL , SAVE   ::  firstTime_InterFaceRunNestor = .TRUE.
!
!      dbug WRITE(6,*)'?>-------  SR InterFaceRunNestor ----------'
!
      IF( firstTime_InterFaceRunNestor ) THEN
        !WRITE(6,*)'?> firstTime_InterFaceRunNestor'  ! debug/test output
        ALLOCATE( dzCL_sis( NSICLA ), stat=status )
        DO i=1, NSICLA                             !> Set pointers to a sub-ranges of an BIEF-object.
          dzCL_sis(i)%R => ZFCL_C%ADR(i)%P%R       !  Now we have access to parts of the BIEF-object
        ENDDO                                      !  without using BIEF stuff for further calls
        firstTime_InterFaceRunNestor = .FALSE.     !  and without copying data
      ENDIF
!
!
!
      CALL MainNestor(   LT, DTS, AT0, ES1, ZF
     &                  , AVAIL, KNOLG, dzCL_sis, HN )
!
!
!      dbug WRITE(6,*)'?>-------  SR InterFaceRunNestor End ------'
!
      RETURN
!**                                               ********************************************
!**                                               ********************************************
#endif                                                     
      END SUBROUTINE InterFaceRunNestor          !********************************************
!**                                               ********************************************
!**                                               ********************************************
!*********************************************************************************************
!*********************************************************************************************
