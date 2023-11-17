
!> Contains wrapper for \c MPI_ALLGATHER
module mpifx_allgather_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_allgather

  !> Gathers scalars/arrays on all nodes.
  !!
  !! All functions have the same argument list only differing in the type and
  !! rank of the second and third arguments. The second and third arguments can
  !! be of type integer, real, double precision, complex, double complex and
  !! logical. Their rank can vary from zero (scalars) up to the maximum
  !! rank. Both arguments must be of same type. The third argument must have the
  !! size of the second times the number of processes taking part in the
  !! gathering. The third argument must have either the same rank as the second
  !! one or one rank more. In latter case its last dimension must be of the size
  !! of the number of processes participating in the gathering operation.
  !!
  !! See MPI documentation (mpi_allgather()) for further details.
  !!
  !! Example:
  !!
  !!     program test_gather
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       integer :: send0
  !!       integer, allocatable :: send1(:)
  !!       integer, allocatable :: recv1(:), recv2(:,:)
  !!       character(100) :: formstr
  !!       character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!
  !!       ! I0 -> I1
  !!       send0 = mycomm%rank * 2
  !!       allocate(recv1(1 * mycomm%size))
  !!       recv1(:) = 0
  !!       write(*, *) mycomm%rank, "Send0 buffer:", send0
  !!       call mpifx_gather(mycomm, send0, recv1)
  !!       write(*, *) mycomm%rank, "Recv1 buffer:", recv1(:)
  !!       deallocate(recv1)
  !!
  !!       ! I1 -> I1
  !!       allocate(send1(2))
  !!       allocate(recv1(size(send1) * mycomm%size))
  !!       recv1(:) = 0
  !!       send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
  !!       write(*, *) "Send1 buffer:", send1(:)
  !!       call mpifx_gather(mycomm, send1, recv1)
  !!       write(*, *) "Recv1 buffer:", recv1
  !!
  !!       ! I1 -> I2
  !!       allocate(recv2(size(send1), mycomm%size))
  !!       recv2(:,:) = 0
  !!       send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
  !!       write(*, *) "Send1 buffer:", send1(:)
  !!       call mpifx_gather(mycomm, send1, recv2)
  !!       write(*, *) "Recv2 buffer:", recv2
  !!
  !!       call mpifx_finalize()
  !!
  !!     end program test_gather
  !!
  interface mpifx_allgather


      module procedure mpifx_allgather_i0i1


      module procedure mpifx_allgather_i1i1

      module procedure mpifx_allgather_i1i2


      module procedure mpifx_allgather_i2i2

      module procedure mpifx_allgather_i2i3


      module procedure mpifx_allgather_i3i3

      module procedure mpifx_allgather_i3i4


      module procedure mpifx_allgather_i4i4

      module procedure mpifx_allgather_i4i5


      module procedure mpifx_allgather_i5i5

      module procedure mpifx_allgather_i5i6


      module procedure mpifx_allgather_i6i6




      module procedure mpifx_allgather_s0s1


      module procedure mpifx_allgather_s1s1

      module procedure mpifx_allgather_s1s2


      module procedure mpifx_allgather_s2s2

      module procedure mpifx_allgather_s2s3


      module procedure mpifx_allgather_s3s3

      module procedure mpifx_allgather_s3s4


      module procedure mpifx_allgather_s4s4

      module procedure mpifx_allgather_s4s5


      module procedure mpifx_allgather_s5s5

      module procedure mpifx_allgather_s5s6


      module procedure mpifx_allgather_s6s6




      module procedure mpifx_allgather_d0d1


      module procedure mpifx_allgather_d1d1

      module procedure mpifx_allgather_d1d2


      module procedure mpifx_allgather_d2d2

      module procedure mpifx_allgather_d2d3


      module procedure mpifx_allgather_d3d3

      module procedure mpifx_allgather_d3d4


      module procedure mpifx_allgather_d4d4

      module procedure mpifx_allgather_d4d5


      module procedure mpifx_allgather_d5d5

      module procedure mpifx_allgather_d5d6


      module procedure mpifx_allgather_d6d6




      module procedure mpifx_allgather_c0c1


      module procedure mpifx_allgather_c1c1

      module procedure mpifx_allgather_c1c2


      module procedure mpifx_allgather_c2c2

      module procedure mpifx_allgather_c2c3


      module procedure mpifx_allgather_c3c3

      module procedure mpifx_allgather_c3c4


      module procedure mpifx_allgather_c4c4

      module procedure mpifx_allgather_c4c5


      module procedure mpifx_allgather_c5c5

      module procedure mpifx_allgather_c5c6


      module procedure mpifx_allgather_c6c6




      module procedure mpifx_allgather_z0z1


      module procedure mpifx_allgather_z1z1

      module procedure mpifx_allgather_z1z2


      module procedure mpifx_allgather_z2z2

      module procedure mpifx_allgather_z2z3


      module procedure mpifx_allgather_z3z3

      module procedure mpifx_allgather_z3z4


      module procedure mpifx_allgather_z4z4

      module procedure mpifx_allgather_z4z5


      module procedure mpifx_allgather_z5z5

      module procedure mpifx_allgather_z5z6


      module procedure mpifx_allgather_z6z6




      module procedure mpifx_allgather_l0l1


      module procedure mpifx_allgather_l1l1

      module procedure mpifx_allgather_l1l2


      module procedure mpifx_allgather_l2l2

      module procedure mpifx_allgather_l2l3


      module procedure mpifx_allgather_l3l3

      module procedure mpifx_allgather_l3l4


      module procedure mpifx_allgather_l4l4

      module procedure mpifx_allgather_l4l5


      module procedure mpifx_allgather_l5l5

      module procedure mpifx_allgather_l5l6


      module procedure mpifx_allgather_l6l6




      module procedure mpifx_allgather_h0h1


      module procedure mpifx_allgather_h1h1

      module procedure mpifx_allgather_h1h2


      module procedure mpifx_allgather_h2h2

      module procedure mpifx_allgather_h2h3


      module procedure mpifx_allgather_h3h3

      module procedure mpifx_allgather_h3h4


      module procedure mpifx_allgather_h4h4

      module procedure mpifx_allgather_h4h5


      module procedure mpifx_allgather_h5h5

      module procedure mpifx_allgather_h5h6


      module procedure mpifx_allgather_h6h6


  end interface mpifx_allgather

contains











  !> Gathers results on all processes (type i0i1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i0i1(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    integer, intent(in) :: send

    !>  Received data.
    integer, intent(out) :: recv(:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, 1, MPI_INTEGER, recv, 1, MPI_INTEGER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i0i1', error)

  end subroutine mpifx_allgather_i0i1






  !> Gathers results on all processes (type i1i1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i1i1(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    integer, intent(in) :: send(:)

    !>  Received data.
    integer, intent(out) :: recv(:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i1i1', error)

  end subroutine mpifx_allgather_i1i1




  !> Gathers results on all processes (type i1i2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i1i2(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    integer, intent(in) :: send(:)

    !>  Received data.
    integer, intent(out) :: recv(:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i1i2', error)

  end subroutine mpifx_allgather_i1i2






  !> Gathers results on all processes (type i2i2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i2i2(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i2i2', error)

  end subroutine mpifx_allgather_i2i2




  !> Gathers results on all processes (type i2i3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i2i3(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i2i3', error)

  end subroutine mpifx_allgather_i2i3






  !> Gathers results on all processes (type i3i3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i3i3(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i3i3', error)

  end subroutine mpifx_allgather_i3i3




  !> Gathers results on all processes (type i3i4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i3i4(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i3i4', error)

  end subroutine mpifx_allgather_i3i4






  !> Gathers results on all processes (type i4i4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i4i4(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:,:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i4i4', error)

  end subroutine mpifx_allgather_i4i4




  !> Gathers results on all processes (type i4i5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i4i5(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:,:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i4i5', error)

  end subroutine mpifx_allgather_i4i5






  !> Gathers results on all processes (type i5i5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i5i5(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i5i5', error)

  end subroutine mpifx_allgather_i5i5




  !> Gathers results on all processes (type i5i6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i5i6(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i5i6', error)

  end subroutine mpifx_allgather_i5i6






  !> Gathers results on all processes (type i6i6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_i6i6(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    integer, intent(in) :: send(:,:,:,:,:,:)

    !>  Received data.
    integer, intent(out) :: recv(:,:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_i6i6', error)

  end subroutine mpifx_allgather_i6i6








  !> Gathers results on all processes (type s0s1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s0s1(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(sp), intent(in) :: send

    !>  Received data.
    real(sp), intent(out) :: recv(:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, 1, MPI_REAL, recv, 1, MPI_REAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s0s1', error)

  end subroutine mpifx_allgather_s0s1






  !> Gathers results on all processes (type s1s1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s1s1(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:)

    !>  Received data.
    real(sp), intent(out) :: recv(:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s1s1', error)

  end subroutine mpifx_allgather_s1s1




  !> Gathers results on all processes (type s1s2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s1s2(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s1s2', error)

  end subroutine mpifx_allgather_s1s2






  !> Gathers results on all processes (type s2s2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s2s2(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s2s2', error)

  end subroutine mpifx_allgather_s2s2




  !> Gathers results on all processes (type s2s3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s2s3(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s2s3', error)

  end subroutine mpifx_allgather_s2s3






  !> Gathers results on all processes (type s3s3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s3s3(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s3s3', error)

  end subroutine mpifx_allgather_s3s3




  !> Gathers results on all processes (type s3s4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s3s4(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s3s4', error)

  end subroutine mpifx_allgather_s3s4






  !> Gathers results on all processes (type s4s4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s4s4(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:,:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s4s4', error)

  end subroutine mpifx_allgather_s4s4




  !> Gathers results on all processes (type s4s5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s4s5(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:,:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s4s5', error)

  end subroutine mpifx_allgather_s4s5






  !> Gathers results on all processes (type s5s5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s5s5(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s5s5', error)

  end subroutine mpifx_allgather_s5s5




  !> Gathers results on all processes (type s5s6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s5s6(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s5s6', error)

  end subroutine mpifx_allgather_s5s6






  !> Gathers results on all processes (type s6s6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_s6s6(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(sp), intent(in) :: send(:,:,:,:,:,:)

    !>  Received data.
    real(sp), intent(out) :: recv(:,:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_s6s6', error)

  end subroutine mpifx_allgather_s6s6








  !> Gathers results on all processes (type d0d1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d0d1(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(dp), intent(in) :: send

    !>  Received data.
    real(dp), intent(out) :: recv(:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, 1, MPI_DOUBLE_PRECISION, recv, 1, MPI_DOUBLE_PRECISION,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d0d1', error)

  end subroutine mpifx_allgather_d0d1






  !> Gathers results on all processes (type d1d1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d1d1(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:)

    !>  Received data.
    real(dp), intent(out) :: recv(:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d1d1', error)

  end subroutine mpifx_allgather_d1d1




  !> Gathers results on all processes (type d1d2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d1d2(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d1d2', error)

  end subroutine mpifx_allgather_d1d2






  !> Gathers results on all processes (type d2d2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d2d2(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d2d2', error)

  end subroutine mpifx_allgather_d2d2




  !> Gathers results on all processes (type d2d3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d2d3(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d2d3', error)

  end subroutine mpifx_allgather_d2d3






  !> Gathers results on all processes (type d3d3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d3d3(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d3d3', error)

  end subroutine mpifx_allgather_d3d3




  !> Gathers results on all processes (type d3d4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d3d4(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d3d4', error)

  end subroutine mpifx_allgather_d3d4






  !> Gathers results on all processes (type d4d4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d4d4(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:,:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d4d4', error)

  end subroutine mpifx_allgather_d4d4




  !> Gathers results on all processes (type d4d5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d4d5(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:,:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d4d5', error)

  end subroutine mpifx_allgather_d4d5






  !> Gathers results on all processes (type d5d5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d5d5(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d5d5', error)

  end subroutine mpifx_allgather_d5d5




  !> Gathers results on all processes (type d5d6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d5d6(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d5d6', error)

  end subroutine mpifx_allgather_d5d6






  !> Gathers results on all processes (type d6d6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_d6d6(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    real(dp), intent(in) :: send(:,:,:,:,:,:)

    !>  Received data.
    real(dp), intent(out) :: recv(:,:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_d6d6', error)

  end subroutine mpifx_allgather_d6d6








  !> Gathers results on all processes (type c0c1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c0c1(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(sp), intent(in) :: send

    !>  Received data.
    complex(sp), intent(out) :: recv(:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, 1, MPI_COMPLEX, recv, 1, MPI_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c0c1', error)

  end subroutine mpifx_allgather_c0c1






  !> Gathers results on all processes (type c1c1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c1c1(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c1c1', error)

  end subroutine mpifx_allgather_c1c1




  !> Gathers results on all processes (type c1c2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c1c2(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c1c2', error)

  end subroutine mpifx_allgather_c1c2






  !> Gathers results on all processes (type c2c2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c2c2(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c2c2', error)

  end subroutine mpifx_allgather_c2c2




  !> Gathers results on all processes (type c2c3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c2c3(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c2c3', error)

  end subroutine mpifx_allgather_c2c3






  !> Gathers results on all processes (type c3c3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c3c3(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c3c3', error)

  end subroutine mpifx_allgather_c3c3




  !> Gathers results on all processes (type c3c4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c3c4(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c3c4', error)

  end subroutine mpifx_allgather_c3c4






  !> Gathers results on all processes (type c4c4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c4c4(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:,:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c4c4', error)

  end subroutine mpifx_allgather_c4c4




  !> Gathers results on all processes (type c4c5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c4c5(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:,:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c4c5', error)

  end subroutine mpifx_allgather_c4c5






  !> Gathers results on all processes (type c5c5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c5c5(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c5c5', error)

  end subroutine mpifx_allgather_c5c5




  !> Gathers results on all processes (type c5c6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c5c6(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c5c6', error)

  end subroutine mpifx_allgather_c5c6






  !> Gathers results on all processes (type c6c6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_c6c6(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(sp), intent(in) :: send(:,:,:,:,:,:)

    !>  Received data.
    complex(sp), intent(out) :: recv(:,:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_c6c6', error)

  end subroutine mpifx_allgather_c6c6








  !> Gathers results on all processes (type z0z1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z0z1(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(dp), intent(in) :: send

    !>  Received data.
    complex(dp), intent(out) :: recv(:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, 1, MPI_DOUBLE_COMPLEX, recv, 1, MPI_DOUBLE_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z0z1', error)

  end subroutine mpifx_allgather_z0z1






  !> Gathers results on all processes (type z1z1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z1z1(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z1z1', error)

  end subroutine mpifx_allgather_z1z1




  !> Gathers results on all processes (type z1z2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z1z2(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z1z2', error)

  end subroutine mpifx_allgather_z1z2






  !> Gathers results on all processes (type z2z2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z2z2(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z2z2', error)

  end subroutine mpifx_allgather_z2z2




  !> Gathers results on all processes (type z2z3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z2z3(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z2z3', error)

  end subroutine mpifx_allgather_z2z3






  !> Gathers results on all processes (type z3z3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z3z3(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z3z3', error)

  end subroutine mpifx_allgather_z3z3




  !> Gathers results on all processes (type z3z4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z3z4(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z3z4', error)

  end subroutine mpifx_allgather_z3z4






  !> Gathers results on all processes (type z4z4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z4z4(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:,:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z4z4', error)

  end subroutine mpifx_allgather_z4z4




  !> Gathers results on all processes (type z4z5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z4z5(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:,:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z4z5', error)

  end subroutine mpifx_allgather_z4z5






  !> Gathers results on all processes (type z5z5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z5z5(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z5z5', error)

  end subroutine mpifx_allgather_z5z5




  !> Gathers results on all processes (type z5z6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z5z6(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z5z6', error)

  end subroutine mpifx_allgather_z5z6






  !> Gathers results on all processes (type z6z6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_z6z6(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    complex(dp), intent(in) :: send(:,:,:,:,:,:)

    !>  Received data.
    complex(dp), intent(out) :: recv(:,:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_z6z6', error)

  end subroutine mpifx_allgather_z6z6








  !> Gathers results on all processes (type l0l1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l0l1(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    logical, intent(in) :: send

    !>  Received data.
    logical, intent(out) :: recv(:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, 1, MPI_LOGICAL, recv, 1, MPI_LOGICAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l0l1', error)

  end subroutine mpifx_allgather_l0l1






  !> Gathers results on all processes (type l1l1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l1l1(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    logical, intent(in) :: send(:)

    !>  Received data.
    logical, intent(out) :: recv(:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l1l1', error)

  end subroutine mpifx_allgather_l1l1




  !> Gathers results on all processes (type l1l2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l1l2(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    logical, intent(in) :: send(:)

    !>  Received data.
    logical, intent(out) :: recv(:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l1l2', error)

  end subroutine mpifx_allgather_l1l2






  !> Gathers results on all processes (type l2l2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l2l2(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l2l2', error)

  end subroutine mpifx_allgather_l2l2




  !> Gathers results on all processes (type l2l3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l2l3(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l2l3', error)

  end subroutine mpifx_allgather_l2l3






  !> Gathers results on all processes (type l3l3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l3l3(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l3l3', error)

  end subroutine mpifx_allgather_l3l3




  !> Gathers results on all processes (type l3l4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l3l4(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l3l4', error)

  end subroutine mpifx_allgather_l3l4






  !> Gathers results on all processes (type l4l4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l4l4(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:,:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l4l4', error)

  end subroutine mpifx_allgather_l4l4




  !> Gathers results on all processes (type l4l5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l4l5(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:,:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l4l5', error)

  end subroutine mpifx_allgather_l4l5






  !> Gathers results on all processes (type l5l5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l5l5(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l5l5', error)

  end subroutine mpifx_allgather_l5l5




  !> Gathers results on all processes (type l5l6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l5l6(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l5l6', error)

  end subroutine mpifx_allgather_l5l6






  !> Gathers results on all processes (type l6l6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_l6l6(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    logical, intent(in) :: send(:,:,:,:,:,:)

    !>  Received data.
    logical, intent(out) :: recv(:,:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_l6l6', error)

  end subroutine mpifx_allgather_l6l6








  !> Gathers results on all processes (type h0h1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h0h1(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    character(len=*), intent(in) :: send

    !>  Received data.
    character(len=*), intent(out) :: recv(:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * 1, MPI_CHARACTER, recv, len(send) * 1, MPI_CHARACTER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h0h1', error)

  end subroutine mpifx_allgather_h0h1






  !> Gathers results on all processes (type h1h1).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h1h1(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h1h1', error)

  end subroutine mpifx_allgather_h1h1




  !> Gathers results on all processes (type h1h2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h1h2(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h1h2', error)

  end subroutine mpifx_allgather_h1h2






  !> Gathers results on all processes (type h2h2).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h2h2(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h2h2', error)

  end subroutine mpifx_allgather_h2h2




  !> Gathers results on all processes (type h2h3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h2h3(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h2h3', error)

  end subroutine mpifx_allgather_h2h3






  !> Gathers results on all processes (type h3h3).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h3h3(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h3h3', error)

  end subroutine mpifx_allgather_h3h3




  !> Gathers results on all processes (type h3h4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h3h4(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h3h4', error)

  end subroutine mpifx_allgather_h3h4






  !> Gathers results on all processes (type h4h4).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h4h4(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:,:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h4h4', error)

  end subroutine mpifx_allgather_h4h4




  !> Gathers results on all processes (type h4h5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h4h5(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:,:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h4h5', error)

  end subroutine mpifx_allgather_h4h5






  !> Gathers results on all processes (type h5h5).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h5h5(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h5h5', error)

  end subroutine mpifx_allgather_h5h5




  !> Gathers results on all processes (type h5h6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h5h6(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:,:,:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:,:,:,:,:)

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h5h6', error)

  end subroutine mpifx_allgather_h5h6






  !> Gathers results on all processes (type h6h6).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_h6h6(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    character(len=*), intent(in) :: send(:,:,:,:,:,:)

    !>  Received data.
    character(len=*), intent(out) :: recv(:,:,:,:,:,:)

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0





    call mpi_allgather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_h6h6', error)

  end subroutine mpifx_allgather_h6h6




end module mpifx_allgather_module
