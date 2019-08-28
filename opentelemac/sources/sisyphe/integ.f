!                    ****************
                     SUBROUTINE INTEG
!                    ****************
!
     &( A , B , IEIN , NPOIN)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE EINSTEIN INTEGRAL
!+                FOR SUSPENDED TRANSPORT.
!code
!+                              A-1     /1       A
!+                             B       |  / 1-Y  \     33Y
!+           I = 0.216*1.83 * ------ * | | ----- | LN --- DY
!+                                 A   |  \  Y   /      B
!+                            (1-B)    /B
!+
!+                          WS                    KR
!+      WITH    A = -------------------  AND  B = --
!+                  KAPPA * BETA * U*CW           H
!
!note     VALUES - CALCULATED BY SIMPSON - ARE TABULATED
!+         AND LINEARLY INTERPOLATED.
!
!history
!+        2000
!+        V5P5
!+   MODIFICATIONS EBS/TB
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| ROUSE NUMBER
!| B              |-->| RATIO BEDLOAD LAYER/WATER DEPTH
!| IEIN           |<--| INTEGRAL VALUE I
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: A(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: IEIN(NPOIN), B(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, PARAMETER :: LNA = 15, LNB = 42
      SAVE EINJ3,AEIN,BEIN
!
      DOUBLE PRECISION EINJ3(LNB,LNA)
      DOUBLE PRECISION AEIN(LNA),BEIN(LNB)
!
      DOUBLE PRECISION A1, A2, B1, B2, INT1, INT2
!
      INTEGER I, IT, JT, INF, SUP, MIL
!
! **********************************************************************
!        TABLES
! **********************************************************************
!
! RATIO QSS/QSC
! COMPUTED USING EINSTEIN INTEGRALS
! AND GENERATED BY MAPLE V
!
! DATA FOR A=  .0
!
      EINJ3(:,1) = (/
     & .0000D+00, .4806D+00, .9944D+00, .1535D+01, .2386D+01, .3889D+01,
     & .7128D+01, .9708D+01, .1422D+02, .1798D+02, .2055D+02, .2383D+02,
     & .2813D+02, .3399D+02, .4243D+02, .5549D+02, .7811D+02, .1257D+03,
     & .1628D+03, .2269D+03, .2797D+03, .3155D+03, .3609D+03, .4202D+03,
     & .5005D+03, .6152D+03, .7913D+03, .1093D+04, .1721D+04, .2206D+04,
     & .3037D+04, .3716D+04, .4176D+04, .4756D+04, .5511D+04, .6531D+04,
     & .7982D+04, .1020D+05, .1398D+05, .4627D+05, .9803D+05, .5538D+06/
     & )
!
! DATA FOR A=  .1
!
      EINJ3(:,2) = (/
     & .0000D+00, .4298D+00, .8772D+00, .1338D+01, .2048D+01, .3271D+01,
     & .5813D+01, .7778D+01, .1113D+02, .1386D+02, .1569D+02, .1801D+02,
     & .2101D+02, .2505D+02, .3077D+02, .3944D+02, .5407D+02, .8379D+02,
     & .1063D+03, .1442D+03, .1747D+03, .1951D+03, .2207D+03, .2536D+03,
     & .2977D+03, .3595D+03, .4526D+03, .6082D+03, .9202D+03, .1154D+04,
     & .1545D+04, .1857D+04, .2066D+04, .2326D+04, .2660D+04, .3105D+04,
     & .3727D+04, .4659D+04, .6207D+04, .1844D+05, .3649D+05, .1758D+06/
     & )
!
! DATA FOR A=  .2
!
      EINJ3(:,3) = (/
     & .0000D+00, .3881D+00, .7823D+00, .1180D+01, .1782D+01, .2792D+01,
     & .4821D+01, .6344D+01, .8874D+01, .1089D+02, .1223D+02, .1390D+02,
     & .1604D+02, .1888D+02, .2282D+02, .2869D+02, .3835D+02, .5729D+02,
     & .7124D+02, .9408D+02, .1121D+03, .1239D+03, .1386D+03, .1573D+03,
     & .1820D+03, .2160D+03, .2663D+03, .3481D+03, .5065D+03, .6219D+03,
     & .8095D+03, .9561D+03, .1052D+04, .1171D+04, .1322D+04, .1521D+04,
     & .1793D+04, .2193D+04, .2841D+04, .7579D+04, .1400D+05, .5756D+05/
     & )
!
! DATA FOR A=  .4
!
      EINJ3(:,4) = (/
     & .0000D+00, .3241D+00, .6390D+00, .9459D+00, .1393D+01, .2110D+01,
     & .3460D+01, .4418D+01, .5934D+01, .7092D+01, .7840D+01, .8753D+01,
     & .9895D+01, .1137D+02, .1335D+02, .1619D+02, .2063D+02, .2877D+02,
     & .3441D+02, .4320D+02, .4981D+02, .5405D+02, .5919D+02, .6557D+02,
     & .7375D+02, .8468D+02, .1001D+03, .1241D+03, .1674D+03, .1970D+03,
     & .2429D+03, .2771D+03, .2989D+03, .3253D+03, .3580D+03, .3997D+03,
     & .4551D+03, .5332D+03, .6534D+03, .1408D+04, .2273D+04, .6813D+04/
     & )
!
! DATA FOR A=  .6
!
      EINJ3(:,5) = (/
     & .0000D+00, .2773D+00, .5367D+00, .7818D+00, .1128D+01, .1659D+01,
     & .2603D+01, .3238D+01, .4199D+01, .4903D+01, .5346D+01, .5877D+01,
     & .6525D+01, .7340D+01, .8403D+01, .9866D+01, .1205D+02, .1579D+02,
     & .1823D+02, .2186D+02, .2446D+02, .2609D+02, .2802D+02, .3036D+02,
     & .3327D+02, .3703D+02, .4215D+02, .4969D+02, .6239D+02, .7059D+02,
     & .8262D+02, .9120D+02, .9653D+02, .1028D+03, .1104D+03, .1199D+03,
     & .1320D+03, .1484D+03, .1724D+03, .3023D+03, .4277D+03, .9411D+03/
     & )
!
! DATA FOR A=  .8
!
      EINJ3(:,6) = (/
     & .0000D+00, .2419D+00, .4607D+00, .6618D+00, .9376D+00, .1346D+01,
     & .2033D+01, .2474D+01, .3114D+01, .3564D+01, .3841D+01, .4166D+01,
     & .4555D+01, .5031D+01, .5635D+01, .6435D+01, .7576D+01, .9407D+01,
     & .1054D+02, .1213D+02, .1323D+02, .1390D+02, .1467D+02, .1559D+02,
     & .1670D+02, .1808D+02, .1990D+02, .2244D+02, .2644D+02, .2887D+02,
     & .3226D+02, .3457D+02, .3597D+02, .3758D+02, .3948D+02, .4177D+02,
     & .4462D+02, .4832D+02, .5346D+02, .7757D+02, .9715D+02, .1601D+03/
     & )
!
! DATA FOR A= 1.0
!
      EINJ3(:,7) = (/
     & .0000D+00, .2142D+00, .4023D+00, .5711D+00, .7968D+00, .1120D+01,
     & .1638D+01, .1956D+01, .2400D+01, .2702D+01, .2884D+01, .3093D+01,
     & .3338D+01, .3631D+01, .3992D+01, .4455D+01, .5086D+01, .6039D+01,
     & .6595D+01, .7344D+01, .7837D+01, .8129D+01, .8461D+01, .8844D+01,
     & .9296D+01, .9844D+01, .1053D+02, .1145D+02, .1281D+02, .1358D+02,
     & .1461D+02, .1528D+02, .1567D+02, .1611D+02, .1662D+02, .1722D+02,
     & .1794D+02, .1884D+02, .2003D+02, .2487D+02, .2817D+02, .3657D+02/
     & )
!
! DATA FOR A= 1.2
!
      EINJ3(:,8) = (/
     & .0000D+00, .1919D+00, .3563D+00, .5006D+00, .6892D+00, .9512D+00,
     & .1354D+01, .1592D+01, .1912D+01, .2122D+01, .2246D+01, .2387D+01,
     & .2549D+01, .2738D+01, .2965D+01, .3247D+01, .3615D+01, .4141D+01,
     & .4432D+01, .4807D+01, .5044D+01, .5180D+01, .5332D+01, .5504D+01,
     & .5701D+01, .5933D+01, .6215D+01, .6572D+01, .7066D+01, .7332D+01,
     & .7668D+01, .7877D+01, .7997D+01, .8129D+01, .8277D+01, .8447D+01,
     & .8644D+01, .8880D+01, .9178D+01, .1024D+02, .1084D+02, .1207D+02/
     & )
!
! DATA FOR A= 1.5
!
      EINJ3(:,9) = (/
     & .0000D+00, .1658D+00, .3032D+00, .4205D+00, .5694D+00, .7685D+00,
     & .1058D+01, .1220D+01, .1428D+01, .1560D+01, .1635D+01, .1718D+01,
     & .1812D+01, .1918D+01, .2041D+01, .2188D+01, .2369D+01, .2609D+01,
     & .2732D+01, .2881D+01, .2970D+01, .3020D+01, .3074D+01, .3133D+01,
     & .3198D+01, .3271D+01, .3356D+01, .3456D+01, .3583D+01, .3645D+01,
     & .3719D+01, .3763D+01, .3786D+01, .3812D+01, .3839D+01, .3869D+01,
     & .3903D+01, .3941D+01, .3986D+01, .4117D+01, .4174D+01, .4259D+01/
     & )
!
! DATA FOR A= 2.0
!
      EINJ3(:,10) = (/
     & .0000D+00, .1349D+00, .2418D+00, .3298D+00, .4372D+00, .5737D+00,
     & .7582D+00, .8542D+00, .9708D+00, .1040D+01, .1078D+01, .1120D+01,
     & .1164D+01, .1213D+01, .1268D+01, .1329D+01, .1399D+01, .1483D+01,
     & .1522D+01, .1566D+01, .1590D+01, .1603D+01, .1616D+01, .1630D+01,
     & .1645D+01, .1661D+01, .1678D+01, .1697D+01, .1718D+01, .1727D+01,
     & .1737D+01, .1742D+01, .1745D+01, .1748D+01, .1751D+01, .1754D+01,
     & .1757D+01, .1760D+01, .1764D+01, .1772D+01, .1774D+01, .1777D+01/
     & )
!
! DATA FOR A= 2.5
!
      EINJ3(:,11) = (/
     & .0000D+00, .1135D+00, .2004D+00, .2699D+00, .3524D+00, .4531D+00,
     & .5821D+00, .6459D+00, .7199D+00, .7621D+00, .7849D+00, .8089D+00,
     & .8344D+00, .8615D+00, .8907D+00, .9222D+00, .9568D+00, .9955D+00,
     & .1012D+01, .1031D+01, .1040D+01, .1045D+01, .1050D+01, .1055D+01,
     & .1060D+01, .1066D+01, .1072D+01, .1077D+01, .1084D+01, .1086D+01,
     & .1089D+01, .1090D+01, .1091D+01, .1091D+01, .1092D+01, .1093D+01,
     & .1093D+01, .1094D+01, .1095D+01, .1096D+01, .1097D+01, .1097D+01/
     & )
!
! DATA FOR A= 3.0
!
      EINJ3(:,12) = (/
     & .0000D+00, .9783D-01, .1708D+00, .2278D+00, .2939D+00, .3724D+00,
     & .4689D+00, .5148D+00, .5664D+00, .5950D+00, .6101D+00, .6259D+00,
     & .6424D+00, .6596D+00, .6777D+00, .6969D+00, .7173D+00, .7392D+00,
     & .7484D+00, .7580D+00, .7630D+00, .7655D+00, .7681D+00, .7707D+00,
     & .7733D+00, .7759D+00, .7786D+00, .7813D+00, .7841D+00, .7853D+00,
     & .7864D+00, .7870D+00, .7872D+00, .7875D+00, .7878D+00, .7881D+00,
     & .7884D+00, .7887D+00, .7890D+00, .7896D+00, .7897D+00, .7898D+00/
     & )
!
! DATA FOR A= 4.0
!
      EINJ3(:,13) = (/
     & .0000D+00, .7657D-01, .1314D+00, .1730D+00, .2196D+00, .2727D+00,
     & .3345D+00, .3624D+00, .3927D+00, .4088D+00, .4172D+00, .4258D+00,
     & .4346D+00, .4437D+00, .4530D+00, .4626D+00, .4725D+00, .4828D+00,
     & .4870D+00, .4913D+00, .4935D+00, .4946D+00, .4957D+00, .4968D+00,
     & .4979D+00, .4990D+00, .5001D+00, .5012D+00, .5023D+00, .5028D+00,
     & .5033D+00, .5035D+00, .5036D+00, .5037D+00, .5038D+00, .5039D+00,
     & .5041D+00, .5042D+00, .5043D+00, .5045D+00, .5046D+00, .5046D+00/
     & )
!
! DATA FOR A= 7.0
!
      EINJ3(:,14) = (/
     & .0000D+00, .4618D-01, .7721D-01, .9958D-01, .1235D+00, .1493D+00,
     & .1771D+00, .1890D+00, .2013D+00, .2076D+00, .2108D+00, .2141D+00,
     & .2174D+00, .2207D+00, .2240D+00, .2274D+00, .2308D+00, .2343D+00,
     & .2357D+00, .2371D+00, .2378D+00, .2381D+00, .2385D+00, .2388D+00,
     & .2392D+00, .2396D+00, .2399D+00, .2403D+00, .2406D+00, .2408D+00,
     & .2409D+00, .2410D+00, .2410D+00, .2410D+00, .2411D+00, .2411D+00,
     & .2412D+00, .2412D+00, .2412D+00, .2413D+00, .2413D+00, .2413D+00/
     & )
!
! DATA FOR A=15.0
!
      EINJ3(:,15) = (/
     & .0000D+00, .2236D-01, .3656D-01, .4639D-01, .5654D-01, .6702D-01,
     & .7786D-01, .8230D-01, .8681D-01, .8909D-01, .9023D-01, .9138D-01,
     & .9254D-01, .9369D-01, .9486D-01, .9602D-01, .9720D-01, .9837D-01,
     & .9884D-01, .9932D-01, .9955D-01, .9967D-01, .9979D-01, .9991D-01,
     & .1000D+00, .1001D+00, .1003D+00, .1004D+00, .1005D+00, .1005D+00,
     & .1006D+00, .1006D+00, .1006D+00, .1006D+00, .1007D+00, .1007D+00,
     & .1007D+00, .1007D+00, .1007D+00, .1007D+00, .1007D+00, .1007D+00/
     & )
!
! VALUES OF TABLE BEIN
!
      BEIN = (/
     &      1.D0  ,.75D0  ,.6D0   ,.5D0   ,.4D0    ,.3D0    ,
     &     .2D0   ,.16D0  ,.12D0  ,.1D0   ,.09D0   ,.08D0   ,
     &     .07D0  ,.06D0  ,.05D0  ,.04D0  ,.03D0   ,.02D0   ,
     &     .016D0 ,.012D0 ,.01D0  ,.009D0 ,.008D0  ,.007D0  ,
     &     .006D0 ,.005D0 ,.004D0 ,.003D0 ,.002D0  ,.0016D0 ,
     &     .0012D0,.001D0 ,.0009D0,.0008D0,.0007D0 ,.0006D0 ,
     &     .0005D0,.0004D0,.0003D0,.0001D0,.00005D0,.00001D0 /
     & )
!
! VALUES OF TABLE AEIN
!
      AEIN =   (/ 0.D0,.1D0,.2D0,.4D0,.6D0,.8D0,1.D0,1.2D0,1.5D0,
     &            2.D0 ,2.5D0,3.D0,4.D0,7.D0,15.D0 /)
!
      DO I=1,NPOIN
!
!
        IF (B(I).LT.BEIN(LNB)) B(I)=BEIN(LNB)
! TREATS B AND A
! =======================
!
! CASE WHERE SUSPENSION IS NEGLIGIBLE
!
!
        IF (A(I).GT.AEIN(LNA).OR.(B(I).GE.1.D0)) THEN
          IEIN(I) = 0.D0
!
! VALUES OUTSIDE OF THE TABLE
!
        ELSEIF ((B(I).GT.1.D0).OR.(B(I).LT.BEIN(LNB)).OR.
     &          (A(I).LT.AEIN(1))) THEN
!
 100      FORMAT(/,1X,'TBIJFR : B EST SUPERIEUR A 1',/,
     &             3X,'B     = ',G10.4,/,
     &             3X,'NOEUD = ',I5,/,
     &             3X,'VERIFIER LA PERTINENCE DE LA FORMULE DE BIJKER'
     &               ,' DANS VOTRE CAS',/)
 101      FORMAT(/,1X,'TBIJFR : B EST INFERIEUR A ',G10.4,
     &                ' LIMITE DE LA ZONE TABULEE',/,
     &             3X,'B     = ',G10.4,/,
     &             3X,'NOEUD = ',I5,/,
     &             3X,'VERIFIER LA PERTINENCE DE LA FORMULE DE BIJKER'
     &               ,' DANS VOTRE CAS',/)
 102      FORMAT(/,1X,'TBIJFR : A EST INFERIEUR A ',G10.4,
     &                ' LIMITE DE LA ZONE TABULEE',/,
     &             3X,'A     = ',G10.4,/,
     &             3X,'NOEUD = ',I5,/,
     &             3X,'VERIFIER LA PERTINENCE DE LA FORMULE DE BIJKER'
     &               ,' DANS VOTRE CAS',/)
!
          IF (B(I).GT.1.D0) THEN
             WRITE(LU,100) B(I),I
          ELSEIF (B(I).LT.BEIN(LNB)) THEN
             WRITE(LU,101) BEIN(LNB),B(I),I
          ELSEIF (A(I).LT.AEIN(1)) THEN
             WRITE(LU,102) AEIN(1),A(I),I
          ENDIF
          CALL PLANTE(0)
          STOP
!
! VALUES IN THE TABLE
!
        ELSE
!
! LOOKS FOR THE BOUNDS FOR B
! ===============================
!
          INF = 1
          SUP = LNB
!----START OF THE DO WHILE LOOP----
 51       MIL = (INF + SUP ) / 2
            IF (BEIN(MIL).GT.B(I)) THEN
              INF = MIL
            ELSE
              SUP = MIL
            ENDIF
          IF ((SUP - INF).NE.1)  GOTO 51
!----END OF THE DO WHILE LOOP----
          JT=SUP
          B1 = BEIN(INF)
          B2 = BEIN(SUP)
!
! LOOKS FOR THE BOUNDS FOR A
! ===============================
!
          INF = 1
          SUP = LNA
!----START OF THE DO WHILE LOOP----
          DO
          MIL = (INF + SUP ) / 2
            IF (AEIN(MIL).LT.A(I)) THEN
              INF = MIL
            ELSE
              SUP = MIL
            ENDIF
          IF ((SUP - INF).EQ.1) EXIT
          ENDDO
!----END OF THE DO WHILE LOOP----
          IT=SUP
          A1 = AEIN(INF)
          A2 = AEIN(SUP)
!
! COMPUTES THE IEIN INTEGRAL
! ==========================
!
! INTERPOLATION WITH CONSTANT A1
          INT1 = (EINJ3(JT-1,IT-1)-EINJ3(JT,IT-1))/(B1-B2)*
     &           (B(I)-B1) + EINJ3(JT-1,IT-1)
! INTERPOLATION WITH CONSTANT A2
          INT2 = (EINJ3(JT-1,IT)  -EINJ3(JT,IT)  )/(B1-B2)*
     &           (B(I)-B1) + EINJ3(JT-1,IT)
! RE-INTERPOLATION BETWEEN THE TWO PRECEDING VALUES
          IEIN(I) = (INT1-INT2)/(A1-A2)*(A(I)-A1) + INT1
!
        ENDIF
!
      ENDDO !I
!
      RETURN
      END SUBROUTINE INTEG
