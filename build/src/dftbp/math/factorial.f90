!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Contains routines relating to evaluating factorials
module dftbp_math_factorial
  use dftbp_common_accuracy, only : dp

  implicit none


  !> Calculate factorals up to a given order
  interface fact
     module procedure int_fact
     module procedure real_fact
  end interface fact

contains


  !> integer factorials of values 0 .. n
  subroutine int_fact(nbang,n)

    !> nbang factorials
    integer, intent(inout) :: nbang(0:)

    !> n calculate factorials from 0 to n
    integer, intent(in) :: n

    integer i


    nbang(0)=1
    do i=1,n-1
      nbang(i) = nbang(i-1) * i

    end do
    nbang(n)=nbang(n-1)*n

  end subroutine int_fact


  !> real factorials of values 0 .. n
  subroutine real_fact(nbang,n)

    !> nbang factorials
    real(dp), intent(inout) :: nbang(0:)

    !> n calculate factorials from 0 to n
    integer, intent(in) :: n

    integer i


    nbang(0)=1.0_dp
    do i=1,n-1
      nbang(i)=nbang(i-1)*real(i,dp)

    end do
    nbang(n)=nbang(n-1)*real(n,dp)

  end subroutine real_fact

end module dftbp_math_factorial
