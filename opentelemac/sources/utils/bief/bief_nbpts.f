!                    ***************************
                     INTEGER FUNCTION BIEF_NBPTS
!                    ***************************
!
     &(IELM,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE NUMBER OF NODES IN THE MESH
!+                FOR A TYPE OF ELEMENT.
!
!history  J-M HERVOUET (LNH)
!+        08/04/04
!+        V5P5
!+
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
!| IELM           |-->| TYPE OF ELEMENT
!| MESH           |-->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)         :: IELM
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(IELM.LT.0.OR.IELM.GT.81) THEN
        WRITE(LU,201) IELM
 201    FORMAT(1X,'BIEF_NBPTS: WRONG ARGUMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      BIEF_NBPTS = MESH%NDS(IELM,1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
