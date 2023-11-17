!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!





!> Contains MPI coherence tests across a comm world
module dftbp_common_coherence
  use dftbp_common_accuracy, only : dp, lc
  use dftbp_common_environment, only : TEnvironment
  use dftbp_extlibs_mpifx, only : MPI_LAND, mpifx_bcast, mpifx_allreduce
  implicit none

  private
  public :: exactCoherence, toleranceCoherence, checkExactCoherence, checkToleranceCoherence

  !> Check for coherence of data across processor(s)
  interface exactCoherence
    module procedure coherenceR0
    module procedure coherenceR1
    module procedure coherenceR2
    module procedure coherenceR3
    module procedure coherenceC1
    module procedure coherenceI0
    module procedure coherenceI1
    module procedure coherenceL0
    module procedure coherenceL1
    module procedure coherenceS0
  end interface exactCoherence

  !> Check for coherence of data to a tolerance across processor(s)
  interface toleranceCoherence
    module procedure approxCoherenceR0
    module procedure approxCoherenceR1
    module procedure approxCoherenceR2
    module procedure approxCoherenceR3
    module procedure approxCoherenceC0
    module procedure approxCoherenceC1
  end interface toleranceCoherence

  !> Check exact coherence of data across processor(s) with error handling
  interface checkExactCoherence
    module procedure coherenceWithErrorR0
    module procedure coherenceWithErrorR1
    module procedure coherenceWithErrorR2
    module procedure coherenceWithErrorR3
    module procedure coherenceWithErrorC1
    module procedure coherenceWithErrorI0
    module procedure coherenceWithErrorI1
    module procedure coherenceWithErrorL0
    module procedure coherenceWithErrorL1
    module procedure coherenceWithErrorS0
  end interface checkExactCoherence

  !> Check coherence of data across processor(s) to a tolerance, with error handling
  interface checkToleranceCoherence
    module procedure approxCoherenceWithErrorR0
    module procedure approxCoherenceWithErrorR1
    module procedure approxCoherenceWithErrorR2
    module procedure approxCoherenceWithErrorR3
    module procedure approxCoherenceWithErrorC0
    module procedure approxCoherenceWithErrorC1
  end interface checkToleranceCoherence


contains


  !> Comparison of data in global comm world
  function coherenceR0(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data

      real(dp) :: dataLocal

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (dataLocal == data) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceR0


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorR0(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceR0(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorR0


  !> Comparison of data in global comm world
  function coherenceR1(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:)

      real(dp), allocatable :: dataLocal(:)

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (all(dataLocal == data)) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceR1


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorR1(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceR1(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorR1


  !> Comparison of data in global comm world
  function coherenceR2(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:,:)

      real(dp), allocatable :: dataLocal(:,:)

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (all(dataLocal == data)) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceR2


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorR2(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:,:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceR2(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorR2


  !> Comparison of data in global comm world
  function coherenceR3(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:,:,:)

      real(dp), allocatable :: dataLocal(:,:,:)

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (all(dataLocal == data)) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceR3


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorR3(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:,:,:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceR3(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorR3


  !> Comparison of data in global comm world
  function coherenceC1(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    complex(dp), intent(in) :: data(:)

      complex(dp), allocatable :: dataLocal(:)

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (all(dataLocal == data)) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceC1


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorC1(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    complex(dp), intent(in) :: data(:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceC1(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorC1


  !> Comparison of data in global comm world
  function coherenceI0(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    integer, intent(in) :: data

      integer :: dataLocal

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (dataLocal == data) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceI0


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorI0(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    integer, intent(in) :: data

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceI0(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorI0


  !> Comparison of data in global comm world
  function coherenceI1(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    integer, intent(in) :: data(:)

      integer, allocatable :: dataLocal(:)

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (all(dataLocal == data)) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceI1


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorI1(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    integer, intent(in) :: data(:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceI1(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorI1


  !> Comparison of data in global comm world
  function coherenceL0(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    logical, intent(in) :: data

      logical :: dataLocal

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (dataLocal .eqv. data) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceL0


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorL0(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    logical, intent(in) :: data

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceL0(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorL0


  !> Comparison of data in global comm world
  function coherenceL1(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    logical, intent(in) :: data(:)

      logical, allocatable :: dataLocal(:)

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (all(dataLocal .eqv. data)) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceL1


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorL1(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    logical, intent(in) :: data(:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceL1(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorL1


  !> Comparison of data in global comm world
  function coherenceS0(env, data) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    character(len=*), intent(in) :: data

      character(len=len(data)) :: dataLocal

      !> Is the local data the same as the lead version?
      logical :: res

      logical :: resLocal

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (dataLocal == data) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function coherenceS0


  !> Wrapper for exact coherence with error handling
  subroutine coherenceWithErrorS0(env, data, message, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    character(len=*), intent(in) :: data

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    if (env%tAPICalculation) then
       if (.not. coherenceS0(env, data)) then
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in " //trim(adjustl(message))// " across nodes"
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
       end if
    end if

  end subroutine coherenceWithErrorS0



  !> Comparison of data in global comm world
  function approxCoherenceR0(env, data, tol) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Is the local data the same as the lead version?
    logical :: res

    logical :: resLocal

      real(dp) :: dataLocal

      real(dp) :: tol_

      if (present(tol)) then
        tol_ = tol
      else
        tol_ = epsilon(0.0_dp)
      end if

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (abs(dataLocal - data) <= tol_) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function approxCoherenceR0


  !> Wrapper for coherence within a specified tolerance, with error handling
  subroutine approxCoherenceWithErrorR0(env, data, message, tol, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    real(dp) :: tol_
    character(len=15) :: tol_str

    if (present(tol)) then
       tol_ = tol
    else
       tol_ = epsilon(0._dp)
    endif

    if (env%tAPICalculation) then
      if (.not. approxCoherenceR0(env, data, tol_)) then
        Write(tol_str, '(E12.5)') tol_
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in "//trim(adjustl(message))//" across nodes for a tolerance of:&
        & "//trim(adjustl(tol_str))
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
      end if
    end if

  end subroutine approxCoherenceWithErrorR0


  !> Comparison of data in global comm world
  function approxCoherenceR1(env, data, tol) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:)

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Is the local data the same as the lead version?
    logical :: res

    logical :: resLocal

      real(dp), allocatable :: dataLocal(:)

      real(dp) :: tol_

      if (present(tol)) then
        tol_ = tol
      else
        tol_ = epsilon(0.0_dp)
      end if

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (maxval(abs(dataLocal - data)) <= tol_) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function approxCoherenceR1


  !> Wrapper for coherence within a specified tolerance, with error handling
  subroutine approxCoherenceWithErrorR1(env, data, message, tol, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    real(dp) :: tol_
    character(len=15) :: tol_str

    if (present(tol)) then
       tol_ = tol
    else
       tol_ = epsilon(0._dp)
    endif

    if (env%tAPICalculation) then
      if (.not. approxCoherenceR1(env, data, tol_)) then
        Write(tol_str, '(E12.5)') tol_
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in "//trim(adjustl(message))//" across nodes for a tolerance of:&
        & "//trim(adjustl(tol_str))
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
      end if
    end if

  end subroutine approxCoherenceWithErrorR1


  !> Comparison of data in global comm world
  function approxCoherenceR2(env, data, tol) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:,:)

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Is the local data the same as the lead version?
    logical :: res

    logical :: resLocal

      real(dp), allocatable :: dataLocal(:,:)

      real(dp) :: tol_

      if (present(tol)) then
        tol_ = tol
      else
        tol_ = epsilon(0.0_dp)
      end if

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (maxval(abs(dataLocal - data)) <= tol_) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function approxCoherenceR2


  !> Wrapper for coherence within a specified tolerance, with error handling
  subroutine approxCoherenceWithErrorR2(env, data, message, tol, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:,:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    real(dp) :: tol_
    character(len=15) :: tol_str

    if (present(tol)) then
       tol_ = tol
    else
       tol_ = epsilon(0._dp)
    endif

    if (env%tAPICalculation) then
      if (.not. approxCoherenceR2(env, data, tol_)) then
        Write(tol_str, '(E12.5)') tol_
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in "//trim(adjustl(message))//" across nodes for a tolerance of:&
        & "//trim(adjustl(tol_str))
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
      end if
    end if

  end subroutine approxCoherenceWithErrorR2


  !> Comparison of data in global comm world
  function approxCoherenceR3(env, data, tol) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:,:,:)

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Is the local data the same as the lead version?
    logical :: res

    logical :: resLocal

      real(dp), allocatable :: dataLocal(:,:,:)

      real(dp) :: tol_

      if (present(tol)) then
        tol_ = tol
      else
        tol_ = epsilon(0.0_dp)
      end if

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (maxval(abs(dataLocal - data)) <= tol_) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function approxCoherenceR3


  !> Wrapper for coherence within a specified tolerance, with error handling
  subroutine approxCoherenceWithErrorR3(env, data, message, tol, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    real(dp), intent(in) :: data(:,:,:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    real(dp) :: tol_
    character(len=15) :: tol_str

    if (present(tol)) then
       tol_ = tol
    else
       tol_ = epsilon(0._dp)
    endif

    if (env%tAPICalculation) then
      if (.not. approxCoherenceR3(env, data, tol_)) then
        Write(tol_str, '(E12.5)') tol_
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in "//trim(adjustl(message))//" across nodes for a tolerance of:&
        & "//trim(adjustl(tol_str))
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
      end if
    end if

  end subroutine approxCoherenceWithErrorR3


  !> Comparison of data in global comm world
  function approxCoherenceC0(env, data, tol) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    complex(dp), intent(in) :: data

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Is the local data the same as the lead version?
    logical :: res

    logical :: resLocal

      complex(dp) :: dataLocal

      real(dp) :: tol_

      if (present(tol)) then
        tol_ = tol
      else
        tol_ = epsilon(0.0_dp)
      end if

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (abs(dataLocal - data) <= tol_) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function approxCoherenceC0


  !> Wrapper for coherence within a specified tolerance, with error handling
  subroutine approxCoherenceWithErrorC0(env, data, message, tol, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    complex(dp), intent(in) :: data

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    real(dp) :: tol_
    character(len=15) :: tol_str

    if (present(tol)) then
       tol_ = tol
    else
       tol_ = epsilon(0._dp)
    endif

    if (env%tAPICalculation) then
      if (.not. approxCoherenceC0(env, data, tol_)) then
        Write(tol_str, '(E12.5)') tol_
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in "//trim(adjustl(message))//" across nodes for a tolerance of:&
        & "//trim(adjustl(tol_str))
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
      end if
    end if

  end subroutine approxCoherenceWithErrorC0


  !> Comparison of data in global comm world
  function approxCoherenceC1(env, data, tol) result(res)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    complex(dp), intent(in) :: data(:)

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Is the local data the same as the lead version?
    logical :: res

    logical :: resLocal

      complex(dp), allocatable :: dataLocal(:)

      real(dp) :: tol_

      if (present(tol)) then
        tol_ = tol
      else
        tol_ = epsilon(0.0_dp)
      end if

      resLocal = .false.
      dataLocal = data
      call mpifx_bcast(env%mpi%globalComm, dataLocal)
      if (maxval(abs(dataLocal - data)) <= tol_) then
        resLocal = .true.
      end if
      call mpifx_allreduce(env%mpi%globalComm, resLocal, res, MPI_LAND)

  end function approxCoherenceC1


  !> Wrapper for coherence within a specified tolerance, with error handling
  subroutine approxCoherenceWithErrorC1(env, data, message, tol, err)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Data to check for coherence
    complex(dp), intent(in) :: data(:)

    !> string detailing data
    character(len=*), intent(in) :: message

    !> Tolerance for comparison, if absent use eps
    real(dp), intent(in), optional :: tol

    !> Error code return, 0 if no problems
    integer, intent(out), optional :: err

    real(dp) :: tol_
    character(len=15) :: tol_str

    if (present(tol)) then
       tol_ = tol
    else
       tol_ = epsilon(0._dp)
    endif

    if (env%tAPICalculation) then
      if (.not. approxCoherenceC1(env, data, tol_)) then
        Write(tol_str, '(E12.5)') tol_
  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")"Coherence failure in "//trim(adjustl(message))//" across nodes for a tolerance of:&
        & "//trim(adjustl(tol_str))
    if (present(err)) then
      err = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block
      end if
    end if

  end subroutine approxCoherenceWithErrorC1


end module dftbp_common_coherence
