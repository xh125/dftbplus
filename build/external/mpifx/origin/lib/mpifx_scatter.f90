
!> Contains wrapper for \c MPI_SCATTER
module mpifx_scatter_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_scatter

  !> Scatters scalars/arrays on a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type. The second argument must have the size of the third times the number
  !! of processes taking part in the scattering. The second argument must have
  !! either the same rank as the third one or one rank more. In latter case
  !! the last dimension of it must be of the size of the number of processes
  !! in the scattering.
  !!
  !! \see MPI documentation (\c MPI_SCATTER)
  !!
  !! Example:
  !!
  !!     program test_scatter
  !!       use libmpifx_module
  !!       implicit none
  !!     
  !!       type(mpifx_comm) :: mycomm
  !!       integer, allocatable :: send1(:), send2(:,:)
  !!       integer :: recv0
  !!       integer, allocatable :: recv1(:)
  !!       integer :: ii
  !!     
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!     
  !!       ! I1 -> I0
  !!       if (mycomm%lead) then
  !!         allocate(send1(mycomm%size))
  !!         send1(:) = [ (ii, ii = 1, size(send1)) ]
  !!         write(*, *) mycomm%rank, "Send1 buffer:", send1
  !!       else
  !!         allocate(send1(0))
  !!       end if
  !!       recv0 = 0
  !!       call mpifx_scatter(mycomm, send1, recv0)
  !!       write(*, *) mycomm%rank, "Recv0 buffer:", recv0
  !!     
  !!       ! I1 -> I1
  !!       if (mycomm%lead) then
  !!         deallocate(send1)
  !!         allocate(send1(2 * mycomm%size))
  !!         send1(:) = [ (ii, ii = 1, size(send1)) ]
  !!         write(*, *)  mycomm%rank, "Send1 buffer:", send1
  !!       end if
  !!       allocate(recv1(2))
  !!       recv1(:) = 0
  !!       call mpifx_scatter(mycomm, send1, recv1)
  !!       write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!     
  !!       ! I2 -> I1
  !!       if (mycomm%lead) then
  !!         allocate(send2(2, mycomm%size))
  !!         send2(:,:) = reshape(send1,  [ 2, mycomm%size ])
  !!         write(*, *) mycomm%rank, "Send2 buffer:", send2
  !!       else
  !!         allocate(send2(0,0))
  !!       end if
  !!       recv1(:) = 0
  !!       call mpifx_scatter(mycomm, send2, recv1)
  !!       write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!       
  !!       call mpifx_finalize()
  !!       
  !!     end program test_scatter
  !!
  interface mpifx_scatter
    module procedure mpifx_scatter_i1i1
    module procedure mpifx_scatter_i1i0
    module procedure mpifx_scatter_i2i2
    module procedure mpifx_scatter_i2i1
    module procedure mpifx_scatter_i3i3
    module procedure mpifx_scatter_i3i2
    module procedure mpifx_scatter_i4i4
    module procedure mpifx_scatter_i4i3
    module procedure mpifx_scatter_i5i5
    module procedure mpifx_scatter_i5i4
    module procedure mpifx_scatter_i6i6
    module procedure mpifx_scatter_i6i5
    module procedure mpifx_scatter_s1s1
    module procedure mpifx_scatter_s1s0
    module procedure mpifx_scatter_s2s2
    module procedure mpifx_scatter_s2s1
    module procedure mpifx_scatter_s3s3
    module procedure mpifx_scatter_s3s2
    module procedure mpifx_scatter_s4s4
    module procedure mpifx_scatter_s4s3
    module procedure mpifx_scatter_s5s5
    module procedure mpifx_scatter_s5s4
    module procedure mpifx_scatter_s6s6
    module procedure mpifx_scatter_s6s5
    module procedure mpifx_scatter_d1d1
    module procedure mpifx_scatter_d1d0
    module procedure mpifx_scatter_d2d2
    module procedure mpifx_scatter_d2d1
    module procedure mpifx_scatter_d3d3
    module procedure mpifx_scatter_d3d2
    module procedure mpifx_scatter_d4d4
    module procedure mpifx_scatter_d4d3
    module procedure mpifx_scatter_d5d5
    module procedure mpifx_scatter_d5d4
    module procedure mpifx_scatter_d6d6
    module procedure mpifx_scatter_d6d5
    module procedure mpifx_scatter_c1c1
    module procedure mpifx_scatter_c1c0
    module procedure mpifx_scatter_c2c2
    module procedure mpifx_scatter_c2c1
    module procedure mpifx_scatter_c3c3
    module procedure mpifx_scatter_c3c2
    module procedure mpifx_scatter_c4c4
    module procedure mpifx_scatter_c4c3
    module procedure mpifx_scatter_c5c5
    module procedure mpifx_scatter_c5c4
    module procedure mpifx_scatter_c6c6
    module procedure mpifx_scatter_c6c5
    module procedure mpifx_scatter_z1z1
    module procedure mpifx_scatter_z1z0
    module procedure mpifx_scatter_z2z2
    module procedure mpifx_scatter_z2z1
    module procedure mpifx_scatter_z3z3
    module procedure mpifx_scatter_z3z2
    module procedure mpifx_scatter_z4z4
    module procedure mpifx_scatter_z4z3
    module procedure mpifx_scatter_z5z5
    module procedure mpifx_scatter_z5z4
    module procedure mpifx_scatter_z6z6
    module procedure mpifx_scatter_z6z5
    module procedure mpifx_scatter_l1l1
    module procedure mpifx_scatter_l1l0
    module procedure mpifx_scatter_l2l2
    module procedure mpifx_scatter_l2l1
    module procedure mpifx_scatter_l3l3
    module procedure mpifx_scatter_l3l2
    module procedure mpifx_scatter_l4l4
    module procedure mpifx_scatter_l4l3
    module procedure mpifx_scatter_l5l5
    module procedure mpifx_scatter_l5l4
    module procedure mpifx_scatter_l6l6
    module procedure mpifx_scatter_l6l5
    module procedure mpifx_scatter_h1h1
    module procedure mpifx_scatter_h1h0
    module procedure mpifx_scatter_h2h2
    module procedure mpifx_scatter_h2h1
    module procedure mpifx_scatter_h3h3
    module procedure mpifx_scatter_h3h2
    module procedure mpifx_scatter_h4h4
    module procedure mpifx_scatter_h4h3
    module procedure mpifx_scatter_h5h5
    module procedure mpifx_scatter_h5h4
    module procedure mpifx_scatter_h6h6
    module procedure mpifx_scatter_h6h5
  end interface mpifx_scatter

contains









  !> Scatters object from one process (type i1i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i1i1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:)
    integer, intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i1i1", error)
      
  end subroutine mpifx_scatter_i1i1



  
  !> Scatters results on one process (type i1i0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i1i0(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:)
    integer, intent(out) :: recv
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i1i0", error)

  end subroutine mpifx_scatter_i1i0






  !> Scatters object from one process (type i2i2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i2i2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:)
    integer, intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i2i2", error)
      
  end subroutine mpifx_scatter_i2i2



  
  !> Scatters results on one process (type i2i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i2i1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:)
    integer, intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i2i1", error)

  end subroutine mpifx_scatter_i2i1






  !> Scatters object from one process (type i3i3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i3i3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:)
    integer, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i3i3", error)
      
  end subroutine mpifx_scatter_i3i3



  
  !> Scatters results on one process (type i3i2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i3i2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:)
    integer, intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i3i2", error)

  end subroutine mpifx_scatter_i3i2






  !> Scatters object from one process (type i4i4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i4i4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i4i4", error)
      
  end subroutine mpifx_scatter_i4i4



  
  !> Scatters results on one process (type i4i3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i4i3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:)
    integer, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i4i3", error)

  end subroutine mpifx_scatter_i4i3






  !> Scatters object from one process (type i5i5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i5i5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i5i5", error)
      
  end subroutine mpifx_scatter_i5i5



  
  !> Scatters results on one process (type i5i4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i5i4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i5i4", error)

  end subroutine mpifx_scatter_i5i4






  !> Scatters object from one process (type i6i6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i6i6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i6i6", error)
      
  end subroutine mpifx_scatter_i6i6



  
  !> Scatters results on one process (type i6i5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_i6i5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_i6i5", error)

  end subroutine mpifx_scatter_i6i5






  !> Scatters object from one process (type s1s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s1s1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:)
    real(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s1s1", error)
      
  end subroutine mpifx_scatter_s1s1



  
  !> Scatters results on one process (type s1s0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s1s0(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:)
    real(sp), intent(out) :: recv
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, 1, MPI_REAL, recv, 1, MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s1s0", error)

  end subroutine mpifx_scatter_s1s0






  !> Scatters object from one process (type s2s2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s2s2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:)
    real(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s2s2", error)
      
  end subroutine mpifx_scatter_s2s2



  
  !> Scatters results on one process (type s2s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s2s1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:)
    real(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s2s1", error)

  end subroutine mpifx_scatter_s2s1






  !> Scatters object from one process (type s3s3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s3s3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:)
    real(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s3s3", error)
      
  end subroutine mpifx_scatter_s3s3



  
  !> Scatters results on one process (type s3s2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s3s2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:)
    real(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s3s2", error)

  end subroutine mpifx_scatter_s3s2






  !> Scatters object from one process (type s4s4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s4s4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s4s4", error)
      
  end subroutine mpifx_scatter_s4s4



  
  !> Scatters results on one process (type s4s3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s4s3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s4s3", error)

  end subroutine mpifx_scatter_s4s3






  !> Scatters object from one process (type s5s5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s5s5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s5s5", error)
      
  end subroutine mpifx_scatter_s5s5



  
  !> Scatters results on one process (type s5s4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s5s4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s5s4", error)

  end subroutine mpifx_scatter_s5s4






  !> Scatters object from one process (type s6s6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s6s6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s6s6", error)
      
  end subroutine mpifx_scatter_s6s6



  
  !> Scatters results on one process (type s6s5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_s6s5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_s6s5", error)

  end subroutine mpifx_scatter_s6s5






  !> Scatters object from one process (type d1d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d1d1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:)
    real(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d1d1", error)
      
  end subroutine mpifx_scatter_d1d1



  
  !> Scatters results on one process (type d1d0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d1d0(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:)
    real(dp), intent(out) :: recv
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, 1, MPI_DOUBLE_PRECISION, recv, 1, MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d1d0", error)

  end subroutine mpifx_scatter_d1d0






  !> Scatters object from one process (type d2d2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d2d2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:)
    real(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d2d2", error)
      
  end subroutine mpifx_scatter_d2d2



  
  !> Scatters results on one process (type d2d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d2d1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:)
    real(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d2d1", error)

  end subroutine mpifx_scatter_d2d1






  !> Scatters object from one process (type d3d3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d3d3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:)
    real(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d3d3", error)
      
  end subroutine mpifx_scatter_d3d3



  
  !> Scatters results on one process (type d3d2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d3d2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:)
    real(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d3d2", error)

  end subroutine mpifx_scatter_d3d2






  !> Scatters object from one process (type d4d4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d4d4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d4d4", error)
      
  end subroutine mpifx_scatter_d4d4



  
  !> Scatters results on one process (type d4d3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d4d3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d4d3", error)

  end subroutine mpifx_scatter_d4d3






  !> Scatters object from one process (type d5d5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d5d5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d5d5", error)
      
  end subroutine mpifx_scatter_d5d5



  
  !> Scatters results on one process (type d5d4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d5d4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d5d4", error)

  end subroutine mpifx_scatter_d5d4






  !> Scatters object from one process (type d6d6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d6d6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d6d6", error)
      
  end subroutine mpifx_scatter_d6d6



  
  !> Scatters results on one process (type d6d5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_d6d5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_d6d5", error)

  end subroutine mpifx_scatter_d6d5






  !> Scatters object from one process (type c1c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c1c1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:)
    complex(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c1c1", error)
      
  end subroutine mpifx_scatter_c1c1



  
  !> Scatters results on one process (type c1c0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c1c0(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:)
    complex(sp), intent(out) :: recv
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, 1, MPI_COMPLEX, recv, 1, MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c1c0", error)

  end subroutine mpifx_scatter_c1c0






  !> Scatters object from one process (type c2c2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c2c2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:)
    complex(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c2c2", error)
      
  end subroutine mpifx_scatter_c2c2



  
  !> Scatters results on one process (type c2c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c2c1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:)
    complex(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c2c1", error)

  end subroutine mpifx_scatter_c2c1






  !> Scatters object from one process (type c3c3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c3c3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:)
    complex(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c3c3", error)
      
  end subroutine mpifx_scatter_c3c3



  
  !> Scatters results on one process (type c3c2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c3c2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:)
    complex(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c3c2", error)

  end subroutine mpifx_scatter_c3c2






  !> Scatters object from one process (type c4c4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c4c4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c4c4", error)
      
  end subroutine mpifx_scatter_c4c4



  
  !> Scatters results on one process (type c4c3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c4c3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c4c3", error)

  end subroutine mpifx_scatter_c4c3






  !> Scatters object from one process (type c5c5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c5c5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c5c5", error)
      
  end subroutine mpifx_scatter_c5c5



  
  !> Scatters results on one process (type c5c4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c5c4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c5c4", error)

  end subroutine mpifx_scatter_c5c4






  !> Scatters object from one process (type c6c6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c6c6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c6c6", error)
      
  end subroutine mpifx_scatter_c6c6



  
  !> Scatters results on one process (type c6c5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_c6c5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_c6c5", error)

  end subroutine mpifx_scatter_c6c5






  !> Scatters object from one process (type z1z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z1z1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:)
    complex(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z1z1", error)
      
  end subroutine mpifx_scatter_z1z1



  
  !> Scatters results on one process (type z1z0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z1z0(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:)
    complex(dp), intent(out) :: recv
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, 1, MPI_DOUBLE_COMPLEX, recv, 1, MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z1z0", error)

  end subroutine mpifx_scatter_z1z0






  !> Scatters object from one process (type z2z2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z2z2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:)
    complex(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z2z2", error)
      
  end subroutine mpifx_scatter_z2z2



  
  !> Scatters results on one process (type z2z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z2z1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:)
    complex(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z2z1", error)

  end subroutine mpifx_scatter_z2z1






  !> Scatters object from one process (type z3z3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z3z3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:)
    complex(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z3z3", error)
      
  end subroutine mpifx_scatter_z3z3



  
  !> Scatters results on one process (type z3z2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z3z2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:)
    complex(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z3z2", error)

  end subroutine mpifx_scatter_z3z2






  !> Scatters object from one process (type z4z4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z4z4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z4z4", error)
      
  end subroutine mpifx_scatter_z4z4



  
  !> Scatters results on one process (type z4z3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z4z3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z4z3", error)

  end subroutine mpifx_scatter_z4z3






  !> Scatters object from one process (type z5z5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z5z5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z5z5", error)
      
  end subroutine mpifx_scatter_z5z5



  
  !> Scatters results on one process (type z5z4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z5z4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z5z4", error)

  end subroutine mpifx_scatter_z5z4






  !> Scatters object from one process (type z6z6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z6z6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z6z6", error)
      
  end subroutine mpifx_scatter_z6z6



  
  !> Scatters results on one process (type z6z5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_z6z5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_z6z5", error)

  end subroutine mpifx_scatter_z6z5






  !> Scatters object from one process (type l1l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l1l1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:)
    logical, intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l1l1", error)
      
  end subroutine mpifx_scatter_l1l1



  
  !> Scatters results on one process (type l1l0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l1l0(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:)
    logical, intent(out) :: recv
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, 1, MPI_LOGICAL, recv, 1, MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l1l0", error)

  end subroutine mpifx_scatter_l1l0






  !> Scatters object from one process (type l2l2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l2l2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:)
    logical, intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l2l2", error)
      
  end subroutine mpifx_scatter_l2l2



  
  !> Scatters results on one process (type l2l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l2l1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:)
    logical, intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l2l1", error)

  end subroutine mpifx_scatter_l2l1






  !> Scatters object from one process (type l3l3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l3l3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:)
    logical, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l3l3", error)
      
  end subroutine mpifx_scatter_l3l3



  
  !> Scatters results on one process (type l3l2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l3l2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:)
    logical, intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l3l2", error)

  end subroutine mpifx_scatter_l3l2






  !> Scatters object from one process (type l4l4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l4l4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l4l4", error)
      
  end subroutine mpifx_scatter_l4l4



  
  !> Scatters results on one process (type l4l3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l4l3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:)
    logical, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l4l3", error)

  end subroutine mpifx_scatter_l4l3






  !> Scatters object from one process (type l5l5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l5l5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l5l5", error)
      
  end subroutine mpifx_scatter_l5l5



  
  !> Scatters results on one process (type l5l4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l5l4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l5l4", error)

  end subroutine mpifx_scatter_l5l4






  !> Scatters object from one process (type l6l6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l6l6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l6l6", error)
      
  end subroutine mpifx_scatter_l6l6



  
  !> Scatters results on one process (type l6l5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_l6l5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0





    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, size(recv), MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_l6l5", error)

  end subroutine mpifx_scatter_l6l5






  !> Scatters object from one process (type h1h1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h1h1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:)
    character(len=*), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h1h1", error)
      
  end subroutine mpifx_scatter_h1h1



  
  !> Scatters results on one process (type h1h0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h1h0(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:)
    character(len=*), intent(out) :: recv
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0






    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * 1, MPI_CHARACTER, recv, len(recv) * 1, MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h1h0", error)

  end subroutine mpifx_scatter_h1h0






  !> Scatters object from one process (type h2h2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h2h2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:)
    character(len=*), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h2h2", error)
      
  end subroutine mpifx_scatter_h2h2



  
  !> Scatters results on one process (type h2h1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h2h1(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:)
    character(len=*), intent(out) :: recv(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0






    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h2h1", error)

  end subroutine mpifx_scatter_h2h1






  !> Scatters object from one process (type h3h3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h3h3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:)
    character(len=*), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h3h3", error)
      
  end subroutine mpifx_scatter_h3h3



  
  !> Scatters results on one process (type h3h2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h3h2(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:)
    character(len=*), intent(out) :: recv(:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0






    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h3h2", error)

  end subroutine mpifx_scatter_h3h2






  !> Scatters object from one process (type h4h4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h4h4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h4h4", error)
      
  end subroutine mpifx_scatter_h4h4



  
  !> Scatters results on one process (type h4h3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h4h3(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0






    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h4h3", error)

  end subroutine mpifx_scatter_h4h3






  !> Scatters object from one process (type h5h5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h5h5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h5h5", error)
      
  end subroutine mpifx_scatter_h5h5



  
  !> Scatters results on one process (type h5h4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h5h4(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0






    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h5h4", error)

  end subroutine mpifx_scatter_h5h4






  !> Scatters object from one process (type h6h6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h6h6(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0




    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h6h6", error)
      
  end subroutine mpifx_scatter_h6h6



  
  !> Scatters results on one process (type h6h5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_h6h5(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0






    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, len(recv) * size(recv), MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_h6h5", error)

  end subroutine mpifx_scatter_h6h5



end module mpifx_scatter_module
