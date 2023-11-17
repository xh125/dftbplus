
!> Exports constants and helper routine(s).
!! \cond HIDDEN
module mpifx_helper_module
  use mpi
  use, intrinsic :: iso_fortran_env, only : stderr => error_unit
  use mpifx_constants_module
  implicit none
  private

  public :: default_tag, sp, dp
  public :: handle_errorflag, assert_failed
  public :: getoptarg, setoptarg

  !> Default tag
  integer, parameter :: default_tag = 0

  !> Single precision kind.
  integer, parameter :: sp = kind(1.0)

  !> Double precision kind.
  integer, parameter :: dp = kind(1.0d0)


  interface getoptarg
    module procedure getoptarg_i0
    module procedure getoptarg_s0
    module procedure getoptarg_d0
    module procedure getoptarg_c0
    module procedure getoptarg_z0
    module procedure getoptarg_l0
    module procedure getoptarg_h0
    module procedure getoptarg_i1
    module procedure getoptarg_s1
    module procedure getoptarg_d1
    module procedure getoptarg_c1
    module procedure getoptarg_z1
    module procedure getoptarg_l1
    module procedure getoptarg_h1
  end interface getoptarg


  interface setoptarg
    module procedure setoptarg_i0
    module procedure setoptarg_s0
    module procedure setoptarg_d0
    module procedure setoptarg_c0
    module procedure setoptarg_z0
    module procedure setoptarg_l0
    module procedure setoptarg_h0
    module procedure setoptarg_i1
    module procedure setoptarg_s1
    module procedure setoptarg_d1
    module procedure setoptarg_c1
    module procedure setoptarg_z1
    module procedure setoptarg_l1
    module procedure setoptarg_h1
  end interface setoptarg


contains

  !> Handles optional error flag.
  !!
  subroutine handle_errorflag(error0, msg, error)

    !> Error flag as returned by some routine.
    integer, intent(in) :: error0

    !>  Msg to print out, if program is stopped.
    character(*), intent(in) :: msg

    !> Optional error flag.
    !!
    !! If present, error0 is passed to it, otherwise if error0 was not zero, the
    !! error message in msg is printed and the program is stopped.
    !!
    integer, intent(out), optional :: error

    integer :: aborterror

    if (present(error)) then
      error = error0
    elseif (error0 /= 0) then
      write(stderr, "(A)") "Operation failed!"
      write(stderr, "(A)") msg
      write(stderr, "(A,I0)") "Error: ", error0
      call mpi_abort(MPI_COMM_WORLD, MPIFX_UNHANDLED_ERROR, aborterror)
      if (aborterror /= 0) then
        write(stderr, "(A)") "Stopping code with 'mpi_abort' did not succeed, trying 'stop' instead"
        stop 1
      end if
    end if

  end subroutine handle_errorflag


  !> Stops code signalizing a failed assert condition
  !!
  subroutine assert_failed(file, line)
    character(*), intent(in) :: file
    integer, intent(in) :: line

    integer :: aborterror

    write(stderr, "(A)") "Assertion failed"
    write(stderr, "(A,A)") "File:", file
    write(stderr, "(A,I0)") "Line:", line
    call mpi_abort(MPI_COMM_WORLD, MPIFX_ASSERT_FAILED, aborterror)
    if (aborterror /= 0) then
        write(stderr, "(A)") "Stopping code with 'mpi_abort' did not succeed, trying 'stop' instead"
        stop 1
    end if

  end subroutine assert_failed










  subroutine getoptarg_i0(defarg, arg, optarg)
    integer, intent(in) :: defarg
    integer, intent(out) :: arg
    integer, intent(in), optional :: optarg

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_i0



  subroutine setoptarg_i0(curval, optval)
    integer, intent(in) :: curval
    integer, intent(out), optional :: optval

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_i0






  subroutine getoptarg_i1(defarg, arg, optarg)
    integer, intent(in) :: defarg(:)
    integer, intent(out) :: arg(:)
    integer, intent(in), optional :: optarg(:)

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_i1



  subroutine setoptarg_i1(curval, optval)
    integer, intent(in) :: curval(:)
    integer, intent(out), optional :: optval(:)

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_i1






  subroutine getoptarg_s0(defarg, arg, optarg)
    real(sp), intent(in) :: defarg
    real(sp), intent(out) :: arg
    real(sp), intent(in), optional :: optarg

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_s0



  subroutine setoptarg_s0(curval, optval)
    real(sp), intent(in) :: curval
    real(sp), intent(out), optional :: optval

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_s0






  subroutine getoptarg_s1(defarg, arg, optarg)
    real(sp), intent(in) :: defarg(:)
    real(sp), intent(out) :: arg(:)
    real(sp), intent(in), optional :: optarg(:)

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_s1



  subroutine setoptarg_s1(curval, optval)
    real(sp), intent(in) :: curval(:)
    real(sp), intent(out), optional :: optval(:)

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_s1






  subroutine getoptarg_d0(defarg, arg, optarg)
    real(dp), intent(in) :: defarg
    real(dp), intent(out) :: arg
    real(dp), intent(in), optional :: optarg

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_d0



  subroutine setoptarg_d0(curval, optval)
    real(dp), intent(in) :: curval
    real(dp), intent(out), optional :: optval

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_d0






  subroutine getoptarg_d1(defarg, arg, optarg)
    real(dp), intent(in) :: defarg(:)
    real(dp), intent(out) :: arg(:)
    real(dp), intent(in), optional :: optarg(:)

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_d1



  subroutine setoptarg_d1(curval, optval)
    real(dp), intent(in) :: curval(:)
    real(dp), intent(out), optional :: optval(:)

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_d1






  subroutine getoptarg_c0(defarg, arg, optarg)
    complex(sp), intent(in) :: defarg
    complex(sp), intent(out) :: arg
    complex(sp), intent(in), optional :: optarg

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_c0



  subroutine setoptarg_c0(curval, optval)
    complex(sp), intent(in) :: curval
    complex(sp), intent(out), optional :: optval

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_c0






  subroutine getoptarg_c1(defarg, arg, optarg)
    complex(sp), intent(in) :: defarg(:)
    complex(sp), intent(out) :: arg(:)
    complex(sp), intent(in), optional :: optarg(:)

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_c1



  subroutine setoptarg_c1(curval, optval)
    complex(sp), intent(in) :: curval(:)
    complex(sp), intent(out), optional :: optval(:)

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_c1






  subroutine getoptarg_z0(defarg, arg, optarg)
    complex(dp), intent(in) :: defarg
    complex(dp), intent(out) :: arg
    complex(dp), intent(in), optional :: optarg

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_z0



  subroutine setoptarg_z0(curval, optval)
    complex(dp), intent(in) :: curval
    complex(dp), intent(out), optional :: optval

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_z0






  subroutine getoptarg_z1(defarg, arg, optarg)
    complex(dp), intent(in) :: defarg(:)
    complex(dp), intent(out) :: arg(:)
    complex(dp), intent(in), optional :: optarg(:)

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_z1



  subroutine setoptarg_z1(curval, optval)
    complex(dp), intent(in) :: curval(:)
    complex(dp), intent(out), optional :: optval(:)

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_z1






  subroutine getoptarg_l0(defarg, arg, optarg)
    logical, intent(in) :: defarg
    logical, intent(out) :: arg
    logical, intent(in), optional :: optarg

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_l0



  subroutine setoptarg_l0(curval, optval)
    logical, intent(in) :: curval
    logical, intent(out), optional :: optval

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_l0






  subroutine getoptarg_l1(defarg, arg, optarg)
    logical, intent(in) :: defarg(:)
    logical, intent(out) :: arg(:)
    logical, intent(in), optional :: optarg(:)

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_l1



  subroutine setoptarg_l1(curval, optval)
    logical, intent(in) :: curval(:)
    logical, intent(out), optional :: optval(:)

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_l1






  subroutine getoptarg_h0(defarg, arg, optarg)
    character(len=*), intent(in) :: defarg
    character(len=*), intent(out) :: arg
    character(len=*), intent(in), optional :: optarg

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_h0



  subroutine setoptarg_h0(curval, optval)
    character(len=*), intent(in) :: curval
    character(len=*), intent(out), optional :: optval

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_h0






  subroutine getoptarg_h1(defarg, arg, optarg)
    character(len=*), intent(in) :: defarg(:)
    character(len=*), intent(out) :: arg(:)
    character(len=*), intent(in), optional :: optarg(:)

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_h1



  subroutine setoptarg_h1(curval, optval)
    character(len=*), intent(in) :: curval(:)
    character(len=*), intent(out), optional :: optval(:)

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_h1



end module mpifx_helper_module

!> \endcond
