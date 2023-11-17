!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Exports the entities used from the ChIMES calculator library
module dftbp_extlibs_chimes
  implicit none

  private
  public :: withChimes
  public :: TChimesCalc, TChimesCalc_init


  !> Whether the code was built with ChIMES support
  logical, parameter :: withChimes = .false.



  !> Dummy placeholder type
  type :: TChimesCalc
  end type TChimesCalc


contains


  subroutine TChimesCalc_init()
  end subroutine TChimesCalc_init


end module dftbp_extlibs_chimes
