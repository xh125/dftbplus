!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Contains replacements for some mathematical routines introduced in Fortran 2008.
!>
!> If the compiler does not provide these routines, preprocess the module with the
!> -DEMULATE_F08_MATH option.
!>
module dftbp_math_f08math
  use dftbp_common_accuracy, only : dp
  implicit none

  private

end module dftbp_math_f08math
