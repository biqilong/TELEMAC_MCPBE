!== Copyright (C) 2000-2017 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

module M_ERREUR_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : 8.1.4              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
use M_PRECISION

TYPE ERREUR_T
  sequence
  integer        :: Numero        ! numero de l'erreur
  character(400) :: ft        ! format d'erreur pour message long
  character(400) :: ft_c      ! format d'erreur pour message court
  character(400) :: Message       ! Message court
  character(132) :: arbredappel   ! Arbre d'appel
END TYPE ERREUR_T

end module M_ERREUR_T
