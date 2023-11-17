
!> Contains wrapper for \c MPI_RECV
module mpifx_recv_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_recv


  !> Receives a message from a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of
  !! type integer (i), real (s), double precision (d), complex (c), 
  !! double complex (z), logical (b) and character (h). Its rank can vary from
  !! zero (scalar) up to the maximum rank.
  !!
  !! \see MPI documentation (\c MPI_RECV)
  !!
  !! Example:
  !!
  !!     program hello
  !!     use libmpifx_module
  !!     implicit none
  !!
  !!     character(100) :: msg
  !!     type(mpifx) :: mycomm
  !!     integer :: source
  !!
  !!     call mpifx_init()
  !!     call mycomm%init()
  !!     if (.not. mycomm%lead) then
  !!       write(msg, "(A,I0,A)") "Hello from process ", mycomm%rank, "!"
  !!       call mpifx_send(mycomm, msg, mycomm%leadrank)
  !!     else
  !!       write(*, "(A)") "Lead node:"
  !!       do source = 1, mycomm%size - 1
  !!         call mpifx_recv(mycomm, msg, source)
  !!         write(*,"(A,A)") "Message received: ", trim(msg)
  !!       end do
  !!     end if
  !!     call mpifx_finalize()
  !!
  !!   end program hello
  !!
  interface mpifx_recv
    module procedure mpifx_recv_i0
    module procedure mpifx_recv_i1
    module procedure mpifx_recv_i2
    module procedure mpifx_recv_i3
    module procedure mpifx_recv_i4
    module procedure mpifx_recv_i5
    module procedure mpifx_recv_i6
    module procedure mpifx_recv_s0
    module procedure mpifx_recv_s1
    module procedure mpifx_recv_s2
    module procedure mpifx_recv_s3
    module procedure mpifx_recv_s4
    module procedure mpifx_recv_s5
    module procedure mpifx_recv_s6
    module procedure mpifx_recv_d0
    module procedure mpifx_recv_d1
    module procedure mpifx_recv_d2
    module procedure mpifx_recv_d3
    module procedure mpifx_recv_d4
    module procedure mpifx_recv_d5
    module procedure mpifx_recv_d6
    module procedure mpifx_recv_c0
    module procedure mpifx_recv_c1
    module procedure mpifx_recv_c2
    module procedure mpifx_recv_c3
    module procedure mpifx_recv_c4
    module procedure mpifx_recv_c5
    module procedure mpifx_recv_c6
    module procedure mpifx_recv_z0
    module procedure mpifx_recv_z1
    module procedure mpifx_recv_z2
    module procedure mpifx_recv_z3
    module procedure mpifx_recv_z4
    module procedure mpifx_recv_z5
    module procedure mpifx_recv_z6
    module procedure mpifx_recv_l0
    module procedure mpifx_recv_l1
    module procedure mpifx_recv_l2
    module procedure mpifx_recv_l3
    module procedure mpifx_recv_l4
    module procedure mpifx_recv_l5
    module procedure mpifx_recv_l6
    module procedure mpifx_recv_h0
    module procedure mpifx_recv_h1
    module procedure mpifx_recv_h2
    module procedure mpifx_recv_h3
    module procedure mpifx_recv_h4
    module procedure mpifx_recv_h5
    module procedure mpifx_recv_h6
  end interface mpifx_recv

contains






  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_i0(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(out) :: msg
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, 1, MPI_INTEGER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_i0", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_i0





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_i1(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(out) :: msg(:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_INTEGER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_i1", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_i1





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_i2(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(out) :: msg(:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_INTEGER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_i2", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_i2





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_i3(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(out) :: msg(:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_INTEGER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_i3", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_i3





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_i4(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(out) :: msg(:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_INTEGER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_i4", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_i4





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_i5(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(out) :: msg(:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_INTEGER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_i5", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_i5





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_i6(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(out) :: msg(:,:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_INTEGER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_i6", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_i6





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_s0(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(out) :: msg
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, 1, MPI_REAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_s0", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_s0





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_s1(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(out) :: msg(:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_REAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_s1", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_s1





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_s2(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(out) :: msg(:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_REAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_s2", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_s2





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_s3(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(out) :: msg(:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_REAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_s3", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_s3





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_s4(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(out) :: msg(:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_REAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_s4", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_s4





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_s5(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(out) :: msg(:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_REAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_s5", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_s5





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_s6(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(out) :: msg(:,:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_REAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_s6", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_s6





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_d0(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(out) :: msg
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, 1, MPI_DOUBLE_PRECISION, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_d0", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_d0





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_d1(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(out) :: msg(:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_PRECISION, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_d1", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_d1





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_d2(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(out) :: msg(:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_PRECISION, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_d2", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_d2





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_d3(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(out) :: msg(:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_PRECISION, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_d3", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_d3





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_d4(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(out) :: msg(:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_PRECISION, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_d4", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_d4





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_d5(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(out) :: msg(:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_PRECISION, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_d5", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_d5





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_d6(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(out) :: msg(:,:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_PRECISION, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_d6", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_d6





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_c0(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(out) :: msg
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, 1, MPI_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_c0", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_c0





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_c1(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(out) :: msg(:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_c1", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_c1





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_c2(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(out) :: msg(:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_c2", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_c2





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_c3(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(out) :: msg(:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_c3", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_c3





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_c4(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(out) :: msg(:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_c4", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_c4





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_c5(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(out) :: msg(:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_c5", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_c5





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_c6(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(out) :: msg(:,:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_c6", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_c6





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_z0(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(out) :: msg
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, 1, MPI_DOUBLE_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_z0", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_z0





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_z1(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(out) :: msg(:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_z1", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_z1





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_z2(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(out) :: msg(:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_z2", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_z2





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_z3(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(out) :: msg(:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_z3", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_z3





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_z4(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(out) :: msg(:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_z4", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_z4





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_z5(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(out) :: msg(:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_z5", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_z5





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_z6(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(out) :: msg(:,:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_DOUBLE_COMPLEX, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_z6", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_z6





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_l0(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(out) :: msg
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, 1, MPI_LOGICAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_l0", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_l0





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_l1(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(out) :: msg(:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_LOGICAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_l1", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_l1





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_l2(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(out) :: msg(:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_LOGICAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_l2", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_l2





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_l3(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(out) :: msg(:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_LOGICAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_l3", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_l3





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_l4(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(out) :: msg(:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_LOGICAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_l4", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_l4





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_l5(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(out) :: msg(:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_LOGICAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_l5", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_l5





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_l6(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(out) :: msg(:,:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, size(msg), MPI_LOGICAL, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_l6", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_l6





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_h0(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(out) :: msg
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, len(msg) * 1, MPI_CHARACTER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_h0", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_h0





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_h1(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(out) :: msg(:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, len(msg) * size(msg), MPI_CHARACTER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_h1", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_h1





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_h2(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(out) :: msg(:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, len(msg) * size(msg), MPI_CHARACTER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_h2", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_h2





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_h3(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(out) :: msg(:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, len(msg) * size(msg), MPI_CHARACTER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_h3", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_h3





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_h4(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(out) :: msg(:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, len(msg) * size(msg), MPI_CHARACTER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_h4", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_h4





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_h5(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(out) :: msg(:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, len(msg) * size(msg), MPI_CHARACTER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_h5", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_h5





  
  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_h6(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(out) :: msg(:,:,:,:,:,:)
    integer, intent(in), optional :: source, tag
    integer, intent(out), optional :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    integer :: status0(MPI_STATUS_SIZE)

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)


    call mpi_recv(msg, len(msg) * size(msg), MPI_CHARACTER, source0, tag0, mycomm%id, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_h6", error)
    call setoptarg(status0, status)

  end subroutine mpifx_recv_h6


  
end module mpifx_recv_module
