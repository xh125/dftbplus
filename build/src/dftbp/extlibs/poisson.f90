!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!



!> Interface to libPoisson routines
!>
!> NOTE: THIS MODULE IS NOT MULTI-INSTANCE SAFE
!>
module dftbp_extlibs_poisson
  use dftbp_common_accuracy, only : dp
  use dftbp_common_constants, only : pi
  use dftbp_common_environment, only : TEnvironment, globalTimers
  use dftbp_common_globalenv, only : stdOut
  use dftbp_io_message, only : error
  use dftbp_type_commontypes, only : TOrbitals
  use libmpifx_module, only : mpifx_barrier, mpifx_bcast
  implicit none

  private
  public :: withPoisson
  public :: TPoissonInfo, TPoissonStructure
  public :: TPoissonInput, TPoisson, TPoisson_init

  logical, parameter :: withPoisson = .false.



  type :: TPoisson
  end type TPoisson




  type :: TPoissonStructure
  end type TPoissonStructure




  type :: TPoissonInfo
  end type TPoissonInfo




  type :: TPoissonInput
  end type TPoissonInput





contains


  subroutine TPoisson_init()
  end subroutine TPoisson_init




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Private routines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end module dftbp_extlibs_poisson
