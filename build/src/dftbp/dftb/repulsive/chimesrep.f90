!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Implements a repulsive correction using the ChIMES force field
module dftbp_dftb_repulsive_chimesrep
  use dftbp_common_accuracy, only : dp
  use dftbp_common_constants, only : AA__Bohr, Bohr__AA, kcal_mol__Hartree, Hartree__kcal_mol
  use dftbp_dftb_periodic, only : TNeighbourList
  use dftbp_dftb_repulsive_repulsive, only : TRepulsive
  use dftbp_extlibs_chimes, only : TChimesCalc, TChimesCalc_init
  implicit none

  private
  public :: TChimesRepInp, TChimesRep, TChimesRep_init



  type :: TChimesRepInp
  end type TChimesRepInp

  type :: TChimesRep
  end type TChimesRep



contains


  !> Dummy initializer in case code was compiled without ChIMES
  subroutine TChimesRep_init()
  end subroutine TChimesRep_init



end module dftbp_dftb_repulsive_chimesrep
