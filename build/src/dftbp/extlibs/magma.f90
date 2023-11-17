!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> MAGMA GPU interface library
module dftbp_extlibs_magma
  use, intrinsic :: iso_c_binding, only : c_int
  implicit none

  private
  public :: withGpu

  !> Whether code was built with GPU support
  logical, parameter :: withGpu = .false.


end module dftbp_extlibs_magma
