
!> Contains wrapper for \c MPI_GATHER
module mpifx_gather_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_gather

  !> Gathers scalars/arrays on a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type. The third argument must have the size of the second times the number
  !! of processes taking part in the gathering. The third argument must have
  !! either the same rank as the second one or one rank more. In latter case
  !! the last dimension of it must be of the size of the number of processes
  !! in the gathering.
  !!
  !! \see MPI documentation (\c MPI_GATHER)
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
  !!       send0 = mycomm%rank * 2    ! Arbitrary number to send
  !!       if (mycomm%lead) then
  !!         allocate(recv1(1 * mycomm%size))
  !!         recv1(:) = 0
  !!       else
  !!         allocate(recv1(0))
  !!       end if
  !!       write(*, *) mycomm%rank, "Send0 buffer:", send0
  !!       call mpifx_gather(mycomm, send0, recv1)
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Recv1 buffer:", recv1(:)
  !!       end if
  !!       deallocate(recv1)
  !!     
  !!       ! I1 -> I1
  !!       allocate(send1(2))
  !!       send1(:) = [ mycomm%rank, mycomm%rank + 1 ]  ! Arbitrary numbers
  !!       if (mycomm%lead) then
  !!         allocate(recv1(size(send1) * mycomm%size))
  !!         recv1(:) = 0
  !!       else
  !!         allocate(recv1(0))
  !!       end if
  !!       write(*, *) mycomm%rank, "Send1 buffer:", send1(:)
  !!       call mpifx_gather(mycomm, send1, recv1)
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!       end if
  !!     
  !!       ! I1 -> I2
  !!       send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
  !!       if (mycomm%lead) then
  !!         allocate(recv2(size(send1), mycomm%size))
  !!         recv2(:,:) = 0
  !!       end if
  !!       write(*, *) mycomm%rank, "Send1 buffer:", send1(:)
  !!       call mpifx_gather(mycomm, send1, recv2)
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Recv2 buffer:", recv2
  !!       end if
  !!       
  !!       call mpifx_finalize()
  !!       
  !!     end program test_gather
  !!
  interface mpifx_gather
      module procedure mpifx_gather_i0i1
      module procedure mpifx_gather_i1i1
      module procedure mpifx_gather_i1i2
      module procedure mpifx_gather_i2i2
      module procedure mpifx_gather_i2i3
      module procedure mpifx_gather_i3i3
      module procedure mpifx_gather_i3i4
      module procedure mpifx_gather_i4i4
      module procedure mpifx_gather_i4i5
      module procedure mpifx_gather_i5i5
      module procedure mpifx_gather_i5i6
      module procedure mpifx_gather_i6i6
      module procedure mpifx_gather_s0s1
      module procedure mpifx_gather_s1s1
      module procedure mpifx_gather_s1s2
      module procedure mpifx_gather_s2s2
      module procedure mpifx_gather_s2s3
      module procedure mpifx_gather_s3s3
      module procedure mpifx_gather_s3s4
      module procedure mpifx_gather_s4s4
      module procedure mpifx_gather_s4s5
      module procedure mpifx_gather_s5s5
      module procedure mpifx_gather_s5s6
      module procedure mpifx_gather_s6s6
      module procedure mpifx_gather_d0d1
      module procedure mpifx_gather_d1d1
      module procedure mpifx_gather_d1d2
      module procedure mpifx_gather_d2d2
      module procedure mpifx_gather_d2d3
      module procedure mpifx_gather_d3d3
      module procedure mpifx_gather_d3d4
      module procedure mpifx_gather_d4d4
      module procedure mpifx_gather_d4d5
      module procedure mpifx_gather_d5d5
      module procedure mpifx_gather_d5d6
      module procedure mpifx_gather_d6d6
      module procedure mpifx_gather_c0c1
      module procedure mpifx_gather_c1c1
      module procedure mpifx_gather_c1c2
      module procedure mpifx_gather_c2c2
      module procedure mpifx_gather_c2c3
      module procedure mpifx_gather_c3c3
      module procedure mpifx_gather_c3c4
      module procedure mpifx_gather_c4c4
      module procedure mpifx_gather_c4c5
      module procedure mpifx_gather_c5c5
      module procedure mpifx_gather_c5c6
      module procedure mpifx_gather_c6c6
      module procedure mpifx_gather_z0z1
      module procedure mpifx_gather_z1z1
      module procedure mpifx_gather_z1z2
      module procedure mpifx_gather_z2z2
      module procedure mpifx_gather_z2z3
      module procedure mpifx_gather_z3z3
      module procedure mpifx_gather_z3z4
      module procedure mpifx_gather_z4z4
      module procedure mpifx_gather_z4z5
      module procedure mpifx_gather_z5z5
      module procedure mpifx_gather_z5z6
      module procedure mpifx_gather_z6z6
      module procedure mpifx_gather_l0l1
      module procedure mpifx_gather_l1l1
      module procedure mpifx_gather_l1l2
      module procedure mpifx_gather_l2l2
      module procedure mpifx_gather_l2l3
      module procedure mpifx_gather_l3l3
      module procedure mpifx_gather_l3l4
      module procedure mpifx_gather_l4l4
      module procedure mpifx_gather_l4l5
      module procedure mpifx_gather_l5l5
      module procedure mpifx_gather_l5l6
      module procedure mpifx_gather_l6l6
      module procedure mpifx_gather_h0h1
      module procedure mpifx_gather_h1h1
      module procedure mpifx_gather_h1h2
      module procedure mpifx_gather_h2h2
      module procedure mpifx_gather_h2h3
      module procedure mpifx_gather_h3h3
      module procedure mpifx_gather_h3h4
      module procedure mpifx_gather_h4h4
      module procedure mpifx_gather_h4h5
      module procedure mpifx_gather_h5h5
      module procedure mpifx_gather_h5h6
      module procedure mpifx_gather_h6h6
  end interface mpifx_gather
  
contains










  !> Gathers results on one process (type i0i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i0i1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send
    integer, intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i0i1", error)

  end subroutine mpifx_gather_i0i1






  !> Gathers results on one process (type i1i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i1i1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:)
    integer, intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i1i1", error)
    
  end subroutine mpifx_gather_i1i1




  !> Gathers results on one process (type i1i2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i1i2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:)
    integer, intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i1i2", error)

  end subroutine mpifx_gather_i1i2






  !> Gathers results on one process (type i2i2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i2i2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:)
    integer, intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i2i2", error)
    
  end subroutine mpifx_gather_i2i2




  !> Gathers results on one process (type i2i3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i2i3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:)
    integer, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i2i3", error)

  end subroutine mpifx_gather_i2i3






  !> Gathers results on one process (type i3i3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i3i3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:)
    integer, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i3i3", error)
    
  end subroutine mpifx_gather_i3i3




  !> Gathers results on one process (type i3i4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i3i4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:)
    integer, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i3i4", error)

  end subroutine mpifx_gather_i3i4






  !> Gathers results on one process (type i4i4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i4i4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i4i4", error)
    
  end subroutine mpifx_gather_i4i4




  !> Gathers results on one process (type i4i5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i4i5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i4i5", error)

  end subroutine mpifx_gather_i4i5






  !> Gathers results on one process (type i5i5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i5i5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i5i5", error)
    
  end subroutine mpifx_gather_i5i5




  !> Gathers results on one process (type i5i6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i5i6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i5i6", error)

  end subroutine mpifx_gather_i5i6






  !> Gathers results on one process (type i6i6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_i6i6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_INTEGER, recv, size(send), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_i6i6", error)
    
  end subroutine mpifx_gather_i6i6








  !> Gathers results on one process (type s0s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s0s1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send
    real(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, 1, MPI_REAL, recv, 1, MPI_REAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s0s1", error)

  end subroutine mpifx_gather_s0s1






  !> Gathers results on one process (type s1s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s1s1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:)
    real(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s1s1", error)
    
  end subroutine mpifx_gather_s1s1




  !> Gathers results on one process (type s1s2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s1s2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:)
    real(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s1s2", error)

  end subroutine mpifx_gather_s1s2






  !> Gathers results on one process (type s2s2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s2s2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:)
    real(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s2s2", error)
    
  end subroutine mpifx_gather_s2s2




  !> Gathers results on one process (type s2s3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s2s3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:)
    real(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s2s3", error)

  end subroutine mpifx_gather_s2s3






  !> Gathers results on one process (type s3s3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s3s3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:)
    real(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s3s3", error)
    
  end subroutine mpifx_gather_s3s3




  !> Gathers results on one process (type s3s4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s3s4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s3s4", error)

  end subroutine mpifx_gather_s3s4






  !> Gathers results on one process (type s4s4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s4s4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s4s4", error)
    
  end subroutine mpifx_gather_s4s4




  !> Gathers results on one process (type s4s5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s4s5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s4s5", error)

  end subroutine mpifx_gather_s4s5






  !> Gathers results on one process (type s5s5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s5s5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s5s5", error)
    
  end subroutine mpifx_gather_s5s5




  !> Gathers results on one process (type s5s6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s5s6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s5s6", error)

  end subroutine mpifx_gather_s5s6






  !> Gathers results on one process (type s6s6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_s6s6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_REAL, recv, size(send), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_s6s6", error)
    
  end subroutine mpifx_gather_s6s6








  !> Gathers results on one process (type d0d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d0d1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send
    real(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, 1, MPI_DOUBLE_PRECISION, recv, 1, MPI_DOUBLE_PRECISION, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d0d1", error)

  end subroutine mpifx_gather_d0d1






  !> Gathers results on one process (type d1d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d1d1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:)
    real(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d1d1", error)
    
  end subroutine mpifx_gather_d1d1




  !> Gathers results on one process (type d1d2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d1d2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:)
    real(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d1d2", error)

  end subroutine mpifx_gather_d1d2






  !> Gathers results on one process (type d2d2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d2d2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:)
    real(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d2d2", error)
    
  end subroutine mpifx_gather_d2d2




  !> Gathers results on one process (type d2d3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d2d3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:)
    real(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d2d3", error)

  end subroutine mpifx_gather_d2d3






  !> Gathers results on one process (type d3d3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d3d3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:)
    real(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d3d3", error)
    
  end subroutine mpifx_gather_d3d3




  !> Gathers results on one process (type d3d4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d3d4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d3d4", error)

  end subroutine mpifx_gather_d3d4






  !> Gathers results on one process (type d4d4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d4d4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d4d4", error)
    
  end subroutine mpifx_gather_d4d4




  !> Gathers results on one process (type d4d5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d4d5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d4d5", error)

  end subroutine mpifx_gather_d4d5






  !> Gathers results on one process (type d5d5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d5d5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d5d5", error)
    
  end subroutine mpifx_gather_d5d5




  !> Gathers results on one process (type d5d6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d5d6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d5d6", error)

  end subroutine mpifx_gather_d5d6






  !> Gathers results on one process (type d6d6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_d6d6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_PRECISION, recv, size(send), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_d6d6", error)
    
  end subroutine mpifx_gather_d6d6








  !> Gathers results on one process (type c0c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c0c1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send
    complex(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, 1, MPI_COMPLEX, recv, 1, MPI_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c0c1", error)

  end subroutine mpifx_gather_c0c1






  !> Gathers results on one process (type c1c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c1c1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:)
    complex(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c1c1", error)
    
  end subroutine mpifx_gather_c1c1




  !> Gathers results on one process (type c1c2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c1c2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:)
    complex(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c1c2", error)

  end subroutine mpifx_gather_c1c2






  !> Gathers results on one process (type c2c2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c2c2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:)
    complex(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c2c2", error)
    
  end subroutine mpifx_gather_c2c2




  !> Gathers results on one process (type c2c3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c2c3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:)
    complex(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c2c3", error)

  end subroutine mpifx_gather_c2c3






  !> Gathers results on one process (type c3c3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c3c3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:)
    complex(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c3c3", error)
    
  end subroutine mpifx_gather_c3c3




  !> Gathers results on one process (type c3c4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c3c4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c3c4", error)

  end subroutine mpifx_gather_c3c4






  !> Gathers results on one process (type c4c4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c4c4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c4c4", error)
    
  end subroutine mpifx_gather_c4c4




  !> Gathers results on one process (type c4c5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c4c5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c4c5", error)

  end subroutine mpifx_gather_c4c5






  !> Gathers results on one process (type c5c5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c5c5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c5c5", error)
    
  end subroutine mpifx_gather_c5c5




  !> Gathers results on one process (type c5c6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c5c6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c5c6", error)

  end subroutine mpifx_gather_c5c6






  !> Gathers results on one process (type c6c6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_c6c6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_COMPLEX, recv, size(send), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_c6c6", error)
    
  end subroutine mpifx_gather_c6c6








  !> Gathers results on one process (type z0z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z0z1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send
    complex(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, 1, MPI_DOUBLE_COMPLEX, recv, 1, MPI_DOUBLE_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z0z1", error)

  end subroutine mpifx_gather_z0z1






  !> Gathers results on one process (type z1z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z1z1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:)
    complex(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z1z1", error)
    
  end subroutine mpifx_gather_z1z1




  !> Gathers results on one process (type z1z2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z1z2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:)
    complex(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z1z2", error)

  end subroutine mpifx_gather_z1z2






  !> Gathers results on one process (type z2z2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z2z2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:)
    complex(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z2z2", error)
    
  end subroutine mpifx_gather_z2z2




  !> Gathers results on one process (type z2z3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z2z3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:)
    complex(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z2z3", error)

  end subroutine mpifx_gather_z2z3






  !> Gathers results on one process (type z3z3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z3z3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:)
    complex(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z3z3", error)
    
  end subroutine mpifx_gather_z3z3




  !> Gathers results on one process (type z3z4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z3z4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z3z4", error)

  end subroutine mpifx_gather_z3z4






  !> Gathers results on one process (type z4z4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z4z4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z4z4", error)
    
  end subroutine mpifx_gather_z4z4




  !> Gathers results on one process (type z4z5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z4z5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z4z5", error)

  end subroutine mpifx_gather_z4z5






  !> Gathers results on one process (type z5z5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z5z5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z5z5", error)
    
  end subroutine mpifx_gather_z5z5




  !> Gathers results on one process (type z5z6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z5z6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z5z6", error)

  end subroutine mpifx_gather_z5z6






  !> Gathers results on one process (type z6z6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_z6z6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_DOUBLE_COMPLEX, recv, size(send), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_z6z6", error)
    
  end subroutine mpifx_gather_z6z6








  !> Gathers results on one process (type l0l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l0l1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send
    logical, intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, 1, MPI_LOGICAL, recv, 1, MPI_LOGICAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l0l1", error)

  end subroutine mpifx_gather_l0l1






  !> Gathers results on one process (type l1l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l1l1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:)
    logical, intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l1l1", error)
    
  end subroutine mpifx_gather_l1l1




  !> Gathers results on one process (type l1l2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l1l2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:)
    logical, intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l1l2", error)

  end subroutine mpifx_gather_l1l2






  !> Gathers results on one process (type l2l2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l2l2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:)
    logical, intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l2l2", error)
    
  end subroutine mpifx_gather_l2l2




  !> Gathers results on one process (type l2l3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l2l3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:)
    logical, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l2l3", error)

  end subroutine mpifx_gather_l2l3






  !> Gathers results on one process (type l3l3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l3l3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:)
    logical, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l3l3", error)
    
  end subroutine mpifx_gather_l3l3




  !> Gathers results on one process (type l3l4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l3l4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:)
    logical, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l3l4", error)

  end subroutine mpifx_gather_l3l4






  !> Gathers results on one process (type l4l4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l4l4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l4l4", error)
    
  end subroutine mpifx_gather_l4l4




  !> Gathers results on one process (type l4l5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l4l5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l4l5", error)

  end subroutine mpifx_gather_l4l5






  !> Gathers results on one process (type l5l5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l5l5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l5l5", error)
    
  end subroutine mpifx_gather_l5l5




  !> Gathers results on one process (type l5l6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l5l6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l5l6", error)

  end subroutine mpifx_gather_l5l6






  !> Gathers results on one process (type l6l6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_l6l6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_LOGICAL, recv, size(send), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_l6l6", error)
    
  end subroutine mpifx_gather_l6l6








  !> Gathers results on one process (type h0h1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h0h1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send
    character(len=*), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, 1, MPI_CHARACTER, recv, 1, MPI_CHARACTER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h0h1", error)

  end subroutine mpifx_gather_h0h1






  !> Gathers results on one process (type h1h1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h1h1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:)
    character(len=*), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h1h1", error)
    
  end subroutine mpifx_gather_h1h1




  !> Gathers results on one process (type h1h2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h1h2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:)
    character(len=*), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_CHARACTER, recv, size(send), MPI_CHARACTER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h1h2", error)

  end subroutine mpifx_gather_h1h2






  !> Gathers results on one process (type h2h2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h2h2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:)
    character(len=*), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h2h2", error)
    
  end subroutine mpifx_gather_h2h2




  !> Gathers results on one process (type h2h3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h2h3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:)
    character(len=*), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_CHARACTER, recv, size(send), MPI_CHARACTER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h2h3", error)

  end subroutine mpifx_gather_h2h3






  !> Gathers results on one process (type h3h3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h3h3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:)
    character(len=*), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h3h3", error)
    
  end subroutine mpifx_gather_h3h3




  !> Gathers results on one process (type h3h4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h3h4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_CHARACTER, recv, size(send), MPI_CHARACTER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h3h4", error)

  end subroutine mpifx_gather_h3h4






  !> Gathers results on one process (type h4h4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h4h4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h4h4", error)
    
  end subroutine mpifx_gather_h4h4




  !> Gathers results on one process (type h4h5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h4h5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_CHARACTER, recv, size(send), MPI_CHARACTER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h4h5", error)

  end subroutine mpifx_gather_h4h5






  !> Gathers results on one process (type h5h5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h5h5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h5h5", error)
    
  end subroutine mpifx_gather_h5h5




  !> Gathers results on one process (type h5h6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h5h6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, size(send), MPI_CHARACTER, recv, size(send), MPI_CHARACTER, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h5h6", error)

  end subroutine mpifx_gather_h5h6






  !> Gathers results on one process (type h6h6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_h6h6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, len(send) * size(send), MPI_CHARACTER, recv, len(send) * size(send), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_h6h6", error)
    
  end subroutine mpifx_gather_h6h6





end module mpifx_gather_module
