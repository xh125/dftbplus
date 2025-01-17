!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Common mathematical operations built out of multiple scalapack calls
module dftbp_math_scalafxext
  use dftbp_common_accuracy, only : lc, dp
  use dftbp_io_message, only : error
  use dftbp_common_status, only : TStatus
  use dftbp_extlibs_scalapackfx, only : DLEN_, scalafx_ppotrf, scalafx_ppotri
  implicit none

  private
  public :: psymmatinv, phermatinv

contains


  !> Inversion of a symmetric matrix
  subroutine psymmatinv(desc, aa, status, uplo)

    !> Matrix descriptor
    integer, intent(in) :: desc(DLEN_)

    !> Matrix to invert on entry, inverted matrix on exit
    real(dp), intent(inout) :: aa(:,:)

    !> Status flag
    type(TStatus), intent(out) :: status

    !> Whether upper or lower triangle is specified in the matrix ("U" or "L", default: "L")
    character, intent(in), optional :: uplo

    integer :: info

    call scalafx_ppotrf(aa, desc, uplo=uplo, info=info)
    if (info /= 0) then
  block
    character(1024) :: message
    write(message, "('scalafx_ppotrf failed in psymmatinv (info: ',I0,')')") info
  call status%setError(-1, trim(message), "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/math/scalafxext.F90", 47)
  end block
  return
    end if
    call scalafx_ppotri(aa, desc, uplo=uplo, info=info)
    if (info /= 0) then
  block
    character(1024) :: message
    write(message, "('scalafx_ppotri failed in psymmatinv (info: ',I0,')')") info
  call status%setError(-1, trim(message), "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/math/scalafxext.F90", 52)
  end block
  return
    end if

  end subroutine psymmatinv


  !> Inversion of a hermitian matrix
  subroutine phermatinv(desc, aa, status, uplo)

    !> Matrix descriptor
    integer, intent(in) :: desc(DLEN_)

    !> Matrix to invert on entry, inverted matrix on exit
    complex(dp), intent(inout) :: aa(:,:)

    !> Status flag
    type(TStatus), intent(out) :: status

    !> Whether upper or lower triangle is specified in the matrix ("U" or "L", default: "L")
    character, intent(in), optional :: uplo

    integer :: info

    call scalafx_ppotrf(aa, desc, uplo=uplo, info=info)
    if (info /= 0) then
  block
    character(1024) :: message
    write(message, "('scalafx_ppotrf failed in phermatinv (info: ',I0,')')") info
  call status%setError(-1, trim(message), "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/math/scalafxext.F90", 78)
  end block
  return
    end if
    call scalafx_ppotri(aa, desc, uplo=uplo, info=info)
    if (info /= 0) then
  block
    character(1024) :: message
    write(message, "('scalafx_ppotri failed in phermatinv (info: ',I0,')')") info
  call status%setError(-1, trim(message), "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/math/scalafxext.F90", 83)
  end block
  return
    end if

  end subroutine phermatinv


end module dftbp_math_scalafxext
