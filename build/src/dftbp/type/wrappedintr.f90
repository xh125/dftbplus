!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Implements various wrapped data types for use in creating ragged multi-dimensional arrays.
module dftbp_type_wrappedintr
  use dftbp_common_accuracy, only : dp
  implicit none

  private

  public :: TwrappedLogical1


  public :: TwrappedLogical2


  public :: TwrappedInt1


  public :: TwrappedInt2


  public :: TwrappedReal1


  public :: TwrappedReal2


  public :: TwrappedCmplx1


  public :: TwrappedCmplx2



  !> 1 dimensional logical
  type :: TwrappedLogical1
    logical, allocatable :: data(:)
  end type TwrappedLogical1


  !> 2 dimensional logical
  type :: TwrappedLogical2
    logical, allocatable :: data(:,:)
  end type TwrappedLogical2


  !> 1 dimensional integer
  type :: TwrappedInt1
    integer, allocatable :: data(:)
  end type TwrappedInt1


  !> 2 dimensional integer
  type :: TwrappedInt2
    integer, allocatable :: data(:,:)
  end type TwrappedInt2


  !> 1 dimensional real
  type :: TwrappedReal1
    real(dp), allocatable :: data(:)
  end type TwrappedReal1


  !> 2 dimensional real
  type :: TwrappedReal2
    real(dp), allocatable :: data(:,:)
  end type TwrappedReal2


  !> 1 dimensional complex
  type :: TwrappedCmplx1
    complex(dp), allocatable :: data(:)
  end type TwrappedCmplx1


  !> 2 dimensional complex
  type :: TwrappedCmplx2
    complex(dp), allocatable :: data(:,:)
  end type TwrappedCmplx2


end module dftbp_type_wrappedintr
