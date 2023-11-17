
!> Contains wrapper for \c MPI_BCAST.
module mpifx_bcast_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_bcast

  !> Broadcasts an MPI message to all nodes.
  !!
  !! \details All functions have the same argument list only differing in the type and rank of the
  !! second argument. The second argument can be of type integer, real, double precision, complex,
  !! double complex, logical and character. Its rank can vary from zero (scalar) up to the maximum
  !! rank.
  !!
  !! \see MPI documentation (\c MPI_BCAST)
  !!
  !! Example:
  !!
  !!     program test_bcast
  !!       use libmpifx_module
  !!
  !!       type(mpifx) :: mycomm
  !!       integer :: buffer(3)
  !!
  !!       call mycomm%init()
  !!       if (mycomm%lead) then
  !!         buffer(:) = [ 1, 2, 3 ]
  !!       end if
  !!       call mpifx_bcast(mycomm, buffer)
  !!       print "(A,I2.2,A,3I5)", "BUFFER:", mycomm%rank, ":", buffer
  !!       call mycomm%destruct()
  !!
  !!     end program test_bcast
  !!
  interface mpifx_bcast
    module procedure mpifx_bcast_i0
    module procedure mpifx_bcast_i1
    module procedure mpifx_bcast_i2
    module procedure mpifx_bcast_i3
    module procedure mpifx_bcast_i4
    module procedure mpifx_bcast_i5
    module procedure mpifx_bcast_i6
    module procedure mpifx_bcast_s0
    module procedure mpifx_bcast_s1
    module procedure mpifx_bcast_s2
    module procedure mpifx_bcast_s3
    module procedure mpifx_bcast_s4
    module procedure mpifx_bcast_s5
    module procedure mpifx_bcast_s6
    module procedure mpifx_bcast_d0
    module procedure mpifx_bcast_d1
    module procedure mpifx_bcast_d2
    module procedure mpifx_bcast_d3
    module procedure mpifx_bcast_d4
    module procedure mpifx_bcast_d5
    module procedure mpifx_bcast_d6
    module procedure mpifx_bcast_c0
    module procedure mpifx_bcast_c1
    module procedure mpifx_bcast_c2
    module procedure mpifx_bcast_c3
    module procedure mpifx_bcast_c4
    module procedure mpifx_bcast_c5
    module procedure mpifx_bcast_c6
    module procedure mpifx_bcast_z0
    module procedure mpifx_bcast_z1
    module procedure mpifx_bcast_z2
    module procedure mpifx_bcast_z3
    module procedure mpifx_bcast_z4
    module procedure mpifx_bcast_z5
    module procedure mpifx_bcast_z6
    module procedure mpifx_bcast_l0
    module procedure mpifx_bcast_l1
    module procedure mpifx_bcast_l2
    module procedure mpifx_bcast_l3
    module procedure mpifx_bcast_l4
    module procedure mpifx_bcast_l5
    module procedure mpifx_bcast_l6
    module procedure mpifx_bcast_h0
    module procedure mpifx_bcast_h1
    module procedure mpifx_bcast_h2
    module procedure mpifx_bcast_h3
    module procedure mpifx_bcast_h4
    module procedure mpifx_bcast_h5
    module procedure mpifx_bcast_h6
  end interface

contains


  




  !> Broadcasts an MPI message to all nodes (type i0).
  !!
  subroutine mpifx_bcast_i0(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    integer :: msg

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, 1, MPI_INTEGER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_i0", error)

  end subroutine mpifx_bcast_i0






  !> Broadcasts an MPI message to all nodes (type i1).
  !!
  subroutine mpifx_bcast_i1(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    integer :: msg(:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_INTEGER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_i1", error)

  end subroutine mpifx_bcast_i1






  !> Broadcasts an MPI message to all nodes (type i2).
  !!
  subroutine mpifx_bcast_i2(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    integer :: msg(:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_INTEGER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_i2", error)

  end subroutine mpifx_bcast_i2






  !> Broadcasts an MPI message to all nodes (type i3).
  !!
  subroutine mpifx_bcast_i3(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    integer :: msg(:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_INTEGER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_i3", error)

  end subroutine mpifx_bcast_i3






  !> Broadcasts an MPI message to all nodes (type i4).
  !!
  subroutine mpifx_bcast_i4(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    integer :: msg(:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_INTEGER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_i4", error)

  end subroutine mpifx_bcast_i4






  !> Broadcasts an MPI message to all nodes (type i5).
  !!
  subroutine mpifx_bcast_i5(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    integer :: msg(:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_INTEGER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_i5", error)

  end subroutine mpifx_bcast_i5






  !> Broadcasts an MPI message to all nodes (type i6).
  !!
  subroutine mpifx_bcast_i6(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    integer :: msg(:,:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_INTEGER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_i6", error)

  end subroutine mpifx_bcast_i6






  !> Broadcasts an MPI message to all nodes (type s0).
  !!
  subroutine mpifx_bcast_s0(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(sp) :: msg

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, 1, MPI_REAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_s0", error)

  end subroutine mpifx_bcast_s0






  !> Broadcasts an MPI message to all nodes (type s1).
  !!
  subroutine mpifx_bcast_s1(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(sp) :: msg(:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_REAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_s1", error)

  end subroutine mpifx_bcast_s1






  !> Broadcasts an MPI message to all nodes (type s2).
  !!
  subroutine mpifx_bcast_s2(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(sp) :: msg(:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_REAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_s2", error)

  end subroutine mpifx_bcast_s2






  !> Broadcasts an MPI message to all nodes (type s3).
  !!
  subroutine mpifx_bcast_s3(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(sp) :: msg(:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_REAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_s3", error)

  end subroutine mpifx_bcast_s3






  !> Broadcasts an MPI message to all nodes (type s4).
  !!
  subroutine mpifx_bcast_s4(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(sp) :: msg(:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_REAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_s4", error)

  end subroutine mpifx_bcast_s4






  !> Broadcasts an MPI message to all nodes (type s5).
  !!
  subroutine mpifx_bcast_s5(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(sp) :: msg(:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_REAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_s5", error)

  end subroutine mpifx_bcast_s5






  !> Broadcasts an MPI message to all nodes (type s6).
  !!
  subroutine mpifx_bcast_s6(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(sp) :: msg(:,:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_REAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_s6", error)

  end subroutine mpifx_bcast_s6






  !> Broadcasts an MPI message to all nodes (type d0).
  !!
  subroutine mpifx_bcast_d0(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(dp) :: msg

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, 1, MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_d0", error)

  end subroutine mpifx_bcast_d0






  !> Broadcasts an MPI message to all nodes (type d1).
  !!
  subroutine mpifx_bcast_d1(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(dp) :: msg(:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_d1", error)

  end subroutine mpifx_bcast_d1






  !> Broadcasts an MPI message to all nodes (type d2).
  !!
  subroutine mpifx_bcast_d2(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(dp) :: msg(:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_d2", error)

  end subroutine mpifx_bcast_d2






  !> Broadcasts an MPI message to all nodes (type d3).
  !!
  subroutine mpifx_bcast_d3(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(dp) :: msg(:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_d3", error)

  end subroutine mpifx_bcast_d3






  !> Broadcasts an MPI message to all nodes (type d4).
  !!
  subroutine mpifx_bcast_d4(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(dp) :: msg(:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_d4", error)

  end subroutine mpifx_bcast_d4






  !> Broadcasts an MPI message to all nodes (type d5).
  !!
  subroutine mpifx_bcast_d5(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(dp) :: msg(:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_d5", error)

  end subroutine mpifx_bcast_d5






  !> Broadcasts an MPI message to all nodes (type d6).
  !!
  subroutine mpifx_bcast_d6(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    real(dp) :: msg(:,:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_d6", error)

  end subroutine mpifx_bcast_d6






  !> Broadcasts an MPI message to all nodes (type c0).
  !!
  subroutine mpifx_bcast_c0(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(sp) :: msg

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, 1, MPI_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_c0", error)

  end subroutine mpifx_bcast_c0






  !> Broadcasts an MPI message to all nodes (type c1).
  !!
  subroutine mpifx_bcast_c1(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(sp) :: msg(:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_c1", error)

  end subroutine mpifx_bcast_c1






  !> Broadcasts an MPI message to all nodes (type c2).
  !!
  subroutine mpifx_bcast_c2(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(sp) :: msg(:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_c2", error)

  end subroutine mpifx_bcast_c2






  !> Broadcasts an MPI message to all nodes (type c3).
  !!
  subroutine mpifx_bcast_c3(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(sp) :: msg(:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_c3", error)

  end subroutine mpifx_bcast_c3






  !> Broadcasts an MPI message to all nodes (type c4).
  !!
  subroutine mpifx_bcast_c4(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(sp) :: msg(:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_c4", error)

  end subroutine mpifx_bcast_c4






  !> Broadcasts an MPI message to all nodes (type c5).
  !!
  subroutine mpifx_bcast_c5(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(sp) :: msg(:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_c5", error)

  end subroutine mpifx_bcast_c5






  !> Broadcasts an MPI message to all nodes (type c6).
  !!
  subroutine mpifx_bcast_c6(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(sp) :: msg(:,:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_c6", error)

  end subroutine mpifx_bcast_c6






  !> Broadcasts an MPI message to all nodes (type z0).
  !!
  subroutine mpifx_bcast_z0(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(dp) :: msg

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, 1, MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_z0", error)

  end subroutine mpifx_bcast_z0






  !> Broadcasts an MPI message to all nodes (type z1).
  !!
  subroutine mpifx_bcast_z1(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(dp) :: msg(:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_z1", error)

  end subroutine mpifx_bcast_z1






  !> Broadcasts an MPI message to all nodes (type z2).
  !!
  subroutine mpifx_bcast_z2(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(dp) :: msg(:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_z2", error)

  end subroutine mpifx_bcast_z2






  !> Broadcasts an MPI message to all nodes (type z3).
  !!
  subroutine mpifx_bcast_z3(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(dp) :: msg(:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_z3", error)

  end subroutine mpifx_bcast_z3






  !> Broadcasts an MPI message to all nodes (type z4).
  !!
  subroutine mpifx_bcast_z4(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(dp) :: msg(:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_z4", error)

  end subroutine mpifx_bcast_z4






  !> Broadcasts an MPI message to all nodes (type z5).
  !!
  subroutine mpifx_bcast_z5(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(dp) :: msg(:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_z5", error)

  end subroutine mpifx_bcast_z5






  !> Broadcasts an MPI message to all nodes (type z6).
  !!
  subroutine mpifx_bcast_z6(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    complex(dp) :: msg(:,:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_z6", error)

  end subroutine mpifx_bcast_z6






  !> Broadcasts an MPI message to all nodes (type l0).
  !!
  subroutine mpifx_bcast_l0(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    logical :: msg

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, 1, MPI_LOGICAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_l0", error)

  end subroutine mpifx_bcast_l0






  !> Broadcasts an MPI message to all nodes (type l1).
  !!
  subroutine mpifx_bcast_l1(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    logical :: msg(:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_LOGICAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_l1", error)

  end subroutine mpifx_bcast_l1






  !> Broadcasts an MPI message to all nodes (type l2).
  !!
  subroutine mpifx_bcast_l2(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    logical :: msg(:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_LOGICAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_l2", error)

  end subroutine mpifx_bcast_l2






  !> Broadcasts an MPI message to all nodes (type l3).
  !!
  subroutine mpifx_bcast_l3(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    logical :: msg(:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_LOGICAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_l3", error)

  end subroutine mpifx_bcast_l3






  !> Broadcasts an MPI message to all nodes (type l4).
  !!
  subroutine mpifx_bcast_l4(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    logical :: msg(:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_LOGICAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_l4", error)

  end subroutine mpifx_bcast_l4






  !> Broadcasts an MPI message to all nodes (type l5).
  !!
  subroutine mpifx_bcast_l5(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    logical :: msg(:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_LOGICAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_l5", error)

  end subroutine mpifx_bcast_l5






  !> Broadcasts an MPI message to all nodes (type l6).
  !!
  subroutine mpifx_bcast_l6(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    logical :: msg(:,:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, size(msg), MPI_LOGICAL, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_l6", error)

  end subroutine mpifx_bcast_l6






  !> Broadcasts an MPI message to all nodes (type h0).
  !!
  subroutine mpifx_bcast_h0(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    character(len=*) :: msg

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, len(msg) * 1, MPI_CHARACTER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_h0", error)

  end subroutine mpifx_bcast_h0






  !> Broadcasts an MPI message to all nodes (type h1).
  !!
  subroutine mpifx_bcast_h1(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    character(len=*) :: msg(:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, len(msg) * size(msg), MPI_CHARACTER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_h1", error)

  end subroutine mpifx_bcast_h1






  !> Broadcasts an MPI message to all nodes (type h2).
  !!
  subroutine mpifx_bcast_h2(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    character(len=*) :: msg(:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, len(msg) * size(msg), MPI_CHARACTER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_h2", error)

  end subroutine mpifx_bcast_h2






  !> Broadcasts an MPI message to all nodes (type h3).
  !!
  subroutine mpifx_bcast_h3(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    character(len=*) :: msg(:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, len(msg) * size(msg), MPI_CHARACTER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_h3", error)

  end subroutine mpifx_bcast_h3






  !> Broadcasts an MPI message to all nodes (type h4).
  !!
  subroutine mpifx_bcast_h4(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    character(len=*) :: msg(:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, len(msg) * size(msg), MPI_CHARACTER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_h4", error)

  end subroutine mpifx_bcast_h4






  !> Broadcasts an MPI message to all nodes (type h5).
  !!
  subroutine mpifx_bcast_h5(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    character(len=*) :: msg(:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, len(msg) * size(msg), MPI_CHARACTER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_h5", error)

  end subroutine mpifx_bcast_h5






  !> Broadcasts an MPI message to all nodes (type h6).
  !!
  subroutine mpifx_bcast_h6(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    character(len=*) :: msg(:,:,:,:,:,:)

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0


    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, len(msg) * size(msg), MPI_CHARACTER, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_h6", error)

  end subroutine mpifx_bcast_h6



end module mpifx_bcast_module
