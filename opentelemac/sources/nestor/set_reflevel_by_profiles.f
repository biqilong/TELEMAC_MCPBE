!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Set_RefLevel_by_Profiles       !********************************************
!***                                              ********************************************
!***                                              ********************************************
!     & ( F, fileNameRefLev )
     & ( F )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : ParallelComputing, ipid, LuRefF
!
!
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
     &                               , Intpol_Z_parallel_Profils
     &                               , Intpol_Z_angular_Profils
#endif
!
      IMPLICIT NONE
!
!      CHARACTER(128),INTENT(IN)    :: fileNameRefLev ! file containing profiles for reference level
      TYPE(t_Field) ,INTENT(INOUT) :: F
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      TYPE(t_Leg_3D), ALLOCATABLE, DIMENSION(:) :: P
!
      INTEGER         :: stat, ip, nProfiles, linecount, dataLine
!
      LOGICAL         :: no_END
!
      REAL (KIND=R8)  :: xA,yA,zA,xB,yB,zB,xC,yC,zC,xD,yD,zD  ! points:  A,B,C,D
      REAL (KIND=R8)  :: xAB,yAB , xCD,yCD                    ! vetkors: AB, CD
      REAL (KIND=R8)  :: absAB, absCD                         ! length of vetkors AB,CD
      REAL (KIND=R8)  :: skaABCD, skaCDCD                     ! skalar produkts
      REAL (KIND=R8)  :: r, angleABCD                         ! relation, angle(AB,CD), angle(SA,SN)
      !REAL (KIND=R8):: r2d = 565.48667764602D0 ! = 360/2*Pi conversion factor: radian to degree ! debug only
!
      CHARACTER (256) ::  line
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
!
!      dbug WRITE(6,*)'?>-------  SR Set_RefLevel_by_Profiles -------'
      SRname%s = "Set_RefLevel_by_Profiles"                     ! subroutine name
!
      F%refZ(:) = -9999.99D0
!
      no_END = .TRUE.
      REWIND LuRefF   ! LuRefF = Logical Unit REFerence level File
!
      linecount = 0
      dataLine = 0
      DO
        linecount = linecount + 1
        READ(LuRefF, '(A)', IOSTAT = stat) line
        !WRITE(*, '(A)') line
        IF( stat           /=  0    ) EXIT
        IF( line(1:1)      == '#'   ) CYCLE
        IF( LEN_TRIM(line) ==  0    ) CYCLE
        IF( line(1:3)      == 'END' ) THEN
          no_END = .FALSE.
          EXIT
        ENDIF
        dataLine = dataLine + 1
      ENDDO
!
      IF(no_END) CALL ErrMsgAndStop(
     &    "while read ReferezLevel file                    "
     &   ,"reason: The reference level must be terminated  "
     &   ,"        with a line which starts with   END     "
     &   ,"        occured in line: ", linecount, SRname, ipid  )
!
      IF(dataLine <= 0) Call ErrMsgAndStop(
     &    "while read ReferezLevel file  "
     &   ,"check content of file !"," "," ", -1, SRname, ipid  )
!
      nProfiles = dataLine
!      WRITE(6,*) '?> number of Profiles = ',nProfiles                  ! debug
!
      ALLOCATE( P(nProfiles) )
!      WRITE(6,'(" ?>",7(1x,a15))')                                     ! debug
!     & 'xL---', 'yL---','zL---','xR---','yR---','zR---','km--- '       ! debug
      REWIND LuRefF
      linecount = 0
      ip = 0
      DO
        linecount = linecount + 1
        READ(LuRefF,'(A)') line
        IF( line(1:1)      == '#'   ) CYCLE
        IF( LEN_TRIM(line) ==  0    ) CYCLE
        IF( line(1:3)      == 'END' ) EXIT
        ip = ip + 1
        READ(line,*, IOSTAT = stat)             ! read profiles
     &  P(ip)%x1,P(ip)%y1,P(ip)%z1,P(ip)%x2,P(ip)%y2,P(ip)%z2,P(ip)%km
!
        IF(stat /= 0) CALL ErrMsgAndStop(
     &    "while read ReferezLevel file      "
     &   ,"reason: 7 real values are expected"
     &   ,"        or   END       is expected"
     &   ,"        occured in line: ", linecount, SRname, ipid  )
!
!        WRITE(6,'(" ?>     ",7(1x,g15.6))')                            ! debug
!     &  P(ip)%x1,P(ip)%y1,P(ip)%z1,P(ip)%x2,P(ip)%y2,P(ip)%z2,P(ip)%km ! debug
!
      ENDDO
!
!      _________________________________________________________________
      ! 1)  calculate angle between two lines each given by two points  |
      !                                                                 |
      ! 2)  distinguish cases profiles are angular or parallel          |
      !                                                                 |
      DO ip=1, nProfiles - 1 !__________________________________________|
        !---------------------------------------------------------------
        !   angle between two vectors:
        !          skalarProd(AB,CD) = |AB|*|CD|*cos(angleABCD)
        !---------------------------------------------------------------
        !   orthogonal projection of vector AB on vector CD:
        !          vector  Ag = skalarProdukt(AB,CD) / |CD| * CD
        !---------------------------------------------------------------
!
        !                     D                vector AB:  xAB = xB - xA  ,   yAB = yB - yA
        !                     *                vector CD:  xCD = xD - xC  ,   yCD = yD - yC
        !                     :\
        !                     : \              r = relation Ag to CD
        !                     :  \             r < 0 ==> angle AB,CD > Pi/2
        !                     :   \            r > 0 ==> angle AB,CD < Pi/2
        !                     :    \
        !              N      :     \                naming of points:
        !               *     :      \ C               B :  left  point of profile 1
        !                     :       *                A :  right point of profile 1
        !                     :                        D :  left  point of profile 2
        !         *-----------+-------*                C :  right point of profile 2
        !         B           g       A                N :  the Field node to interpolate a z-value
        !                     |-- r --|                g :  dropped perpendicular foot
        !
        xA = P(ip  )%x2
        yA = P(ip  )%y2
        zA = P(ip  )%z2
        xB = P(ip  )%x1
        yB = P(ip  )%y1
        zB = P(ip  )%z1
        xC = P(ip+1)%x2
        yC = P(ip+1)%y2
        zC = P(ip+1)%z2
        xD = P(ip+1)%x1
        yD = P(ip+1)%y1
        zD = P(ip+1)%z1
!
        xAB = xB - xA             ! vector AB: x component
        yAB = yB - yA             !     "      y component
        xCD = xD - xC             ! vector CD: x component
        yCD = yD - yC             !     "      y component
!
        !---------------------------------------------------------------
        !   angle between two vectors:
        !          skalarProd(AB,CD) = |AB|*|CD|*cos(angleABCD)
        !---------------------------------------------------------------
        skaABCD = xAB*xCD + yAB*yCD   ! skalarProduct(AB,CD)
        skaCDCD = xCD*xCD + yCD*yCD   ! skalarProduct(CD,CD)
        absCD   = SQRT( skaCDCD )
        absAB   = SQRT( xAB**2 + yAB**2 )
!
        r =  skaABCD / skaCDCD   ! skalProd(CD,AB) / skalProd(CD,CD)
!
!        ______________________________________________________________
        !  check for angle bigger or equal  90deg                      |
        !  calculate the angle ________________________________________|
        IF( r <= 1.D-5    )Call ErrMsgAndStop( "      "
     &    ,"reason: Angle between profiles is to big !"
     &    ,"        max < 90deg                       "
     &    ,"occured at profile: ", ip, SRname, ipid      )
!
        angleABCD = ACOS( skaABCD / (absAB * absCD) )
!
!        ______________________________________________________________
        !  check for parallelism                                       |
        !  calculate intersection of lines ____________________________|
        IF( angleABCD < 1.D-5    ) THEN         ! profiles are almost parallel
           CALL Intpol_Z_parallel_Profils
     &       ( F, xA,yA,zA,  xB,yB,zB,  xC,yC,zC,  xD,yD,zD
     &          , ip, P(ip)%km, P(ip+1)%km                 )
        ELSE                                    !  profiles are angular
          CALL Intpol_Z_angular_Profils
     &       ( F, xA,yA,zA,  xB,yB,zB,  xC,yC,zC,  xD,yD,zD
     &          , ip, P(ip)%km, P(ip+1)%km,  angleABCD     )
        ENDIF
!
      ENDDO    !  ip=1, nProfiles - 1
!
      IF( MINVAL(F%refZ) < -1000.0D0 ) Call ErrMsgAndStop( "  "
     &,"reason: Some field nodes are not overlaped by profiles"
     &,"        Fit profils or field polygon !                "
     &," ", -1, SRname, ipid      )
!
!
      IF( ParallelComputing ) CALL P_SYNC()
!      dbug WRITE(6,*)'?>-------  SR Set_RefLevel_by_Profiles End ---'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Set_RefLevel_by_Profiles    !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
