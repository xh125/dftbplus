
!> Contains wrapper for \c MPI_SEND
module mpifx_send_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_send


  !> Sends a message to a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of
  !! type integer (i), real (s), double precision (d), complex (c), 
  !! double complex (z), logical (b) and character (h). Its rank can vary from
  !! zero (scalar) up to the maximum rank.
  !!
  !! \see MPI documentation (\c MPI_SEND)
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
  interface mpifx_send
    module procedure mpifx_send_i0
    module procedure mpifx_send_i1
    module procedure mpifx_send_i2
    module procedure mpifx_send_i3
    module procedure mpifx_send_i4
    module procedure mpifx_send_i5
    module procedure mpifx_send_i6
    module procedure mpifx_send_s0
    module procedure mpifx_send_s1
    module procedure mpifx_send_s2
    module procedure mpifx_send_s3
    module procedure mpifx_send_s4
    module procedure mpifx_send_s5
    module procedure mpifx_send_s6
    module procedure mpifx_send_d0
    module procedure mpifx_send_d1
    module procedure mpifx_send_d2
    module procedure mpifx_send_d3
    module procedure mpifx_send_d4
    module procedure mpifx_send_d5
    module procedure mpifx_send_d6
    module procedure mpifx_send_c0
    module procedure mpifx_send_c1
    module procedure mpifx_send_c2
    module procedure mpifx_send_c3
    module procedure mpifx_send_c4
    module procedure mpifx_send_c5
    module procedure mpifx_send_c6
    module procedure mpifx_send_z0
    module procedure mpifx_send_z1
    module procedure mpifx_send_z2
    module procedure mpifx_send_z3
    module procedure mpifx_send_z4
    module procedure mpifx_send_z5
    module procedure mpifx_send_z6
    module procedure mpifx_send_l0
    module procedure mpifx_send_l1
    module procedure mpifx_send_l2
    module procedure mpifx_send_l3
    module procedure mpifx_send_l4
    module procedure mpifx_send_l5
    module procedure mpifx_send_l6
    module procedure mpifx_send_h0
    module procedure mpifx_send_h1
    module procedure mpifx_send_h2
    module procedure mpifx_send_h3
    module procedure mpifx_send_h4
    module procedure mpifx_send_h5
    module procedure mpifx_send_h6
  end interface mpifx_send

contains






  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_i0(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: msg
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, 1, MPI_INTEGER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_i0", error)

  end subroutine mpifx_send_i0





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_i1(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: msg(:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_INTEGER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_i1", error)

  end subroutine mpifx_send_i1





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_i2(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: msg(:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_INTEGER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_i2", error)

  end subroutine mpifx_send_i2





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_i3(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: msg(:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_INTEGER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_i3", error)

  end subroutine mpifx_send_i3





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_i4(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: msg(:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_INTEGER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_i4", error)

  end subroutine mpifx_send_i4





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_i5(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: msg(:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_INTEGER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_i5", error)

  end subroutine mpifx_send_i5





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_i6(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: msg(:,:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_INTEGER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_i6", error)

  end subroutine mpifx_send_i6





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_s0(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: msg
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, 1, MPI_REAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_s0", error)

  end subroutine mpifx_send_s0





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_s1(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: msg(:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_REAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_s1", error)

  end subroutine mpifx_send_s1





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_s2(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: msg(:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_REAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_s2", error)

  end subroutine mpifx_send_s2





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_s3(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: msg(:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_REAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_s3", error)

  end subroutine mpifx_send_s3





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_s4(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: msg(:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_REAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_s4", error)

  end subroutine mpifx_send_s4





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_s5(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: msg(:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_REAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_s5", error)

  end subroutine mpifx_send_s5





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_s6(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: msg(:,:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_REAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_s6", error)

  end subroutine mpifx_send_s6





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_d0(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: msg
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, 1, MPI_DOUBLE_PRECISION, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_d0", error)

  end subroutine mpifx_send_d0





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_d1(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: msg(:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_PRECISION, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_d1", error)

  end subroutine mpifx_send_d1





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_d2(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: msg(:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_PRECISION, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_d2", error)

  end subroutine mpifx_send_d2





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_d3(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: msg(:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_PRECISION, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_d3", error)

  end subroutine mpifx_send_d3





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_d4(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: msg(:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_PRECISION, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_d4", error)

  end subroutine mpifx_send_d4





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_d5(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: msg(:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_PRECISION, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_d5", error)

  end subroutine mpifx_send_d5





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_d6(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: msg(:,:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_PRECISION, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_d6", error)

  end subroutine mpifx_send_d6





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_c0(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: msg
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, 1, MPI_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_c0", error)

  end subroutine mpifx_send_c0





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_c1(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: msg(:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_c1", error)

  end subroutine mpifx_send_c1





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_c2(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: msg(:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_c2", error)

  end subroutine mpifx_send_c2





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_c3(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: msg(:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_c3", error)

  end subroutine mpifx_send_c3





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_c4(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: msg(:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_c4", error)

  end subroutine mpifx_send_c4





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_c5(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: msg(:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_c5", error)

  end subroutine mpifx_send_c5





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_c6(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: msg(:,:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_c6", error)

  end subroutine mpifx_send_c6





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_z0(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: msg
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, 1, MPI_DOUBLE_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_z0", error)

  end subroutine mpifx_send_z0





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_z1(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: msg(:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_z1", error)

  end subroutine mpifx_send_z1





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_z2(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: msg(:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_z2", error)

  end subroutine mpifx_send_z2





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_z3(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: msg(:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_z3", error)

  end subroutine mpifx_send_z3





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_z4(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: msg(:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_z4", error)

  end subroutine mpifx_send_z4





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_z5(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: msg(:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_z5", error)

  end subroutine mpifx_send_z5





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_z6(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: msg(:,:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_DOUBLE_COMPLEX, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_z6", error)

  end subroutine mpifx_send_z6





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_l0(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: msg
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, 1, MPI_LOGICAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_l0", error)

  end subroutine mpifx_send_l0





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_l1(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: msg(:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_LOGICAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_l1", error)

  end subroutine mpifx_send_l1





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_l2(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: msg(:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_LOGICAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_l2", error)

  end subroutine mpifx_send_l2





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_l3(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: msg(:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_LOGICAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_l3", error)

  end subroutine mpifx_send_l3





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_l4(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: msg(:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_LOGICAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_l4", error)

  end subroutine mpifx_send_l4





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_l5(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: msg(:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_LOGICAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_l5", error)

  end subroutine mpifx_send_l5





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_l6(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: msg(:,:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, size(msg), MPI_LOGICAL, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_l6", error)

  end subroutine mpifx_send_l6





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_h0(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: msg
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, len(msg) * 1, MPI_CHARACTER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_h0", error)

  end subroutine mpifx_send_h0





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_h1(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: msg(:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, len(msg) * size(msg), MPI_CHARACTER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_h1", error)

  end subroutine mpifx_send_h1





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_h2(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: msg(:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, len(msg) * size(msg), MPI_CHARACTER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_h2", error)

  end subroutine mpifx_send_h2





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_h3(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: msg(:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, len(msg) * size(msg), MPI_CHARACTER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_h3", error)

  end subroutine mpifx_send_h3





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_h4(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: msg(:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, len(msg) * size(msg), MPI_CHARACTER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_h4", error)

  end subroutine mpifx_send_h4





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_h5(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: msg(:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, len(msg) * size(msg), MPI_CHARACTER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_h5", error)

  end subroutine mpifx_send_h5





  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_h6(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: msg(:,:,:,:,:,:)
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0


    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, len(msg) * size(msg), MPI_CHARACTER, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_h6", error)

  end subroutine mpifx_send_h6





end module mpifx_send_module
