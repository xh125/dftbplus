
!> Contains wrapper for \c MPI_SCATTER
module mpifx_scatterv_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_scatterv

  !> scatters scalars/arrays of different lengths from a given node.
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
  !! in the scatterving.
  !!
  !! \see MPI documentation (\c MPI_scatterv)
  !!
  !! Example:
  !!
  !!     program test_scatterv
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       integer, allocatable :: send1(:)
  !!       integer, allocatable :: recv1(:)
  !!       integer, allocatable :: sendcounts(:)
  !!       integer :: ii, nsend
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!
  !!       ! I1 -> I1
  !!       allocate(recv1(mycomm%rank+1))
  !!       recv1 = 0
  !!       if (mycomm%lead) then
  !!         ! send1 size is 1+2+3+...+mycomm%size
  !!         nsend = mycomm%size*(mycomm%size+1)/2
  !!         allocate(send1(nsend))
  !!         do ii = 1, nsend
  !!           send1(ii) = ii
  !!         end do
  !!         allocate(sendcounts(mycomm%size))
  !!         do ii = 1, mycomm%size
  !!           sendcounts(ii) = ii
  !!         end do
  !!       else
  !!         allocate(send1(0))
  !!       end if
  !!
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Send1 buffer:", send1(:)
  !!       end if
  !!       call mpifx_scatterv(mycomm, send1, sendcounts, recv1)
  !!       write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!
  !!       call mpifx_finalize()
  !!
  !!     end program test_scatterv
  !!
  interface mpifx_scatterv
    module procedure mpifx_scatterv_i1i1
    module procedure mpifx_scatterv_i1i0
    module procedure mpifx_scatterv_i2i2
    module procedure mpifx_scatterv_i2i1
    module procedure mpifx_scatterv_i3i3
    module procedure mpifx_scatterv_i3i2
    module procedure mpifx_scatterv_i4i4
    module procedure mpifx_scatterv_i4i3
    module procedure mpifx_scatterv_i5i5
    module procedure mpifx_scatterv_i5i4
    module procedure mpifx_scatterv_i6i6
    module procedure mpifx_scatterv_i6i5
    module procedure mpifx_scatterv_s1s1
    module procedure mpifx_scatterv_s1s0
    module procedure mpifx_scatterv_s2s2
    module procedure mpifx_scatterv_s2s1
    module procedure mpifx_scatterv_s3s3
    module procedure mpifx_scatterv_s3s2
    module procedure mpifx_scatterv_s4s4
    module procedure mpifx_scatterv_s4s3
    module procedure mpifx_scatterv_s5s5
    module procedure mpifx_scatterv_s5s4
    module procedure mpifx_scatterv_s6s6
    module procedure mpifx_scatterv_s6s5
    module procedure mpifx_scatterv_d1d1
    module procedure mpifx_scatterv_d1d0
    module procedure mpifx_scatterv_d2d2
    module procedure mpifx_scatterv_d2d1
    module procedure mpifx_scatterv_d3d3
    module procedure mpifx_scatterv_d3d2
    module procedure mpifx_scatterv_d4d4
    module procedure mpifx_scatterv_d4d3
    module procedure mpifx_scatterv_d5d5
    module procedure mpifx_scatterv_d5d4
    module procedure mpifx_scatterv_d6d6
    module procedure mpifx_scatterv_d6d5
    module procedure mpifx_scatterv_c1c1
    module procedure mpifx_scatterv_c1c0
    module procedure mpifx_scatterv_c2c2
    module procedure mpifx_scatterv_c2c1
    module procedure mpifx_scatterv_c3c3
    module procedure mpifx_scatterv_c3c2
    module procedure mpifx_scatterv_c4c4
    module procedure mpifx_scatterv_c4c3
    module procedure mpifx_scatterv_c5c5
    module procedure mpifx_scatterv_c5c4
    module procedure mpifx_scatterv_c6c6
    module procedure mpifx_scatterv_c6c5
    module procedure mpifx_scatterv_z1z1
    module procedure mpifx_scatterv_z1z0
    module procedure mpifx_scatterv_z2z2
    module procedure mpifx_scatterv_z2z1
    module procedure mpifx_scatterv_z3z3
    module procedure mpifx_scatterv_z3z2
    module procedure mpifx_scatterv_z4z4
    module procedure mpifx_scatterv_z4z3
    module procedure mpifx_scatterv_z5z5
    module procedure mpifx_scatterv_z5z4
    module procedure mpifx_scatterv_z6z6
    module procedure mpifx_scatterv_z6z5
    module procedure mpifx_scatterv_l1l1
    module procedure mpifx_scatterv_l1l0
    module procedure mpifx_scatterv_l2l2
    module procedure mpifx_scatterv_l2l1
    module procedure mpifx_scatterv_l3l3
    module procedure mpifx_scatterv_l3l2
    module procedure mpifx_scatterv_l4l4
    module procedure mpifx_scatterv_l4l3
    module procedure mpifx_scatterv_l5l5
    module procedure mpifx_scatterv_l5l4
    module procedure mpifx_scatterv_l6l6
    module procedure mpifx_scatterv_l6l5
    module procedure mpifx_scatterv_h1h1
    module procedure mpifx_scatterv_h1h0
    module procedure mpifx_scatterv_h2h2
    module procedure mpifx_scatterv_h2h1
    module procedure mpifx_scatterv_h3h3
    module procedure mpifx_scatterv_h3h2
    module procedure mpifx_scatterv_h4h4
    module procedure mpifx_scatterv_h4h3
    module procedure mpifx_scatterv_h5h5
    module procedure mpifx_scatterv_h5h4
    module procedure mpifx_scatterv_h6h6
    module procedure mpifx_scatterv_h6h5
  end interface mpifx_scatterv

contains









  !> scatters object of variable length from one process (type i1i1).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_i1i1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i1i1", error)
      
  end subroutine mpifx_scatterv_i1i1



  
  !> Scatter results from one process (type i1i0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_i1i0(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, 1, MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i1i0", error)

  end subroutine mpifx_scatterv_i1i0






  !> scatters object of variable length from one process (type i2i2).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_i2i2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i2i2", error)
      
  end subroutine mpifx_scatterv_i2i2



  
  !> Scatter results from one process (type i2i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_i2i1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i2i1", error)

  end subroutine mpifx_scatterv_i2i1






  !> scatters object of variable length from one process (type i3i3).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_i3i3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i3i3", error)
      
  end subroutine mpifx_scatterv_i3i3



  
  !> Scatter results from one process (type i3i2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_i3i2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i3i2", error)

  end subroutine mpifx_scatterv_i3i2






  !> scatters object of variable length from one process (type i4i4).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_i4i4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i4i4", error)
      
  end subroutine mpifx_scatterv_i4i4



  
  !> Scatter results from one process (type i4i3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_i4i3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i4i3", error)

  end subroutine mpifx_scatterv_i4i3






  !> scatters object of variable length from one process (type i5i5).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_i5i5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i5i5", error)
      
  end subroutine mpifx_scatterv_i5i5



  
  !> Scatter results from one process (type i5i4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_i5i4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i5i4", error)

  end subroutine mpifx_scatterv_i5i4






  !> scatters object of variable length from one process (type i6i6).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_i6i6(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i6i6", error)
      
  end subroutine mpifx_scatterv_i6i6



  
  !> Scatter results from one process (type i6i5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_i6i5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    integer, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_INTEGER, recv, size(recv), MPI_INTEGER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_i6i5", error)

  end subroutine mpifx_scatterv_i6i5






  !> scatters object of variable length from one process (type s1s1).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_s1s1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s1s1", error)
      
  end subroutine mpifx_scatterv_s1s1



  
  !> Scatter results from one process (type s1s0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_s1s0(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, 1, MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s1s0", error)

  end subroutine mpifx_scatterv_s1s0






  !> scatters object of variable length from one process (type s2s2).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_s2s2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s2s2", error)
      
  end subroutine mpifx_scatterv_s2s2



  
  !> Scatter results from one process (type s2s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_s2s1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s2s1", error)

  end subroutine mpifx_scatterv_s2s1






  !> scatters object of variable length from one process (type s3s3).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_s3s3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s3s3", error)
      
  end subroutine mpifx_scatterv_s3s3



  
  !> Scatter results from one process (type s3s2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_s3s2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s3s2", error)

  end subroutine mpifx_scatterv_s3s2






  !> scatters object of variable length from one process (type s4s4).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_s4s4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s4s4", error)
      
  end subroutine mpifx_scatterv_s4s4



  
  !> Scatter results from one process (type s4s3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_s4s3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s4s3", error)

  end subroutine mpifx_scatterv_s4s3






  !> scatters object of variable length from one process (type s5s5).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_s5s5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s5s5", error)
      
  end subroutine mpifx_scatterv_s5s5



  
  !> Scatter results from one process (type s5s4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_s5s4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s5s4", error)

  end subroutine mpifx_scatterv_s5s4






  !> scatters object of variable length from one process (type s6s6).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_s6s6(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s6s6", error)
      
  end subroutine mpifx_scatterv_s6s6



  
  !> Scatter results from one process (type s6s5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_s6s5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_REAL, recv, size(recv), MPI_REAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_s6s5", error)

  end subroutine mpifx_scatterv_s6s5






  !> scatters object of variable length from one process (type d1d1).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_d1d1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d1d1", error)
      
  end subroutine mpifx_scatterv_d1d1



  
  !> Scatter results from one process (type d1d0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_d1d0(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, 1, MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d1d0", error)

  end subroutine mpifx_scatterv_d1d0






  !> scatters object of variable length from one process (type d2d2).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_d2d2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d2d2", error)
      
  end subroutine mpifx_scatterv_d2d2



  
  !> Scatter results from one process (type d2d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_d2d1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d2d1", error)

  end subroutine mpifx_scatterv_d2d1






  !> scatters object of variable length from one process (type d3d3).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_d3d3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d3d3", error)
      
  end subroutine mpifx_scatterv_d3d3



  
  !> Scatter results from one process (type d3d2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_d3d2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d3d2", error)

  end subroutine mpifx_scatterv_d3d2






  !> scatters object of variable length from one process (type d4d4).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_d4d4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d4d4", error)
      
  end subroutine mpifx_scatterv_d4d4



  
  !> Scatter results from one process (type d4d3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_d4d3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d4d3", error)

  end subroutine mpifx_scatterv_d4d3






  !> scatters object of variable length from one process (type d5d5).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_d5d5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d5d5", error)
      
  end subroutine mpifx_scatterv_d5d5



  
  !> Scatter results from one process (type d5d4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_d5d4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d5d4", error)

  end subroutine mpifx_scatterv_d5d4






  !> scatters object of variable length from one process (type d6d6).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_d6d6(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d6d6", error)
      
  end subroutine mpifx_scatterv_d6d6



  
  !> Scatter results from one process (type d6d5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_d6d5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    real(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_PRECISION, recv, size(recv), MPI_DOUBLE_PRECISION, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_d6d5", error)

  end subroutine mpifx_scatterv_d6d5






  !> scatters object of variable length from one process (type c1c1).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_c1c1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c1c1", error)
      
  end subroutine mpifx_scatterv_c1c1



  
  !> Scatter results from one process (type c1c0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_c1c0(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, 1, MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c1c0", error)

  end subroutine mpifx_scatterv_c1c0






  !> scatters object of variable length from one process (type c2c2).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_c2c2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c2c2", error)
      
  end subroutine mpifx_scatterv_c2c2



  
  !> Scatter results from one process (type c2c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_c2c1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c2c1", error)

  end subroutine mpifx_scatterv_c2c1






  !> scatters object of variable length from one process (type c3c3).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_c3c3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c3c3", error)
      
  end subroutine mpifx_scatterv_c3c3



  
  !> Scatter results from one process (type c3c2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_c3c2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c3c2", error)

  end subroutine mpifx_scatterv_c3c2






  !> scatters object of variable length from one process (type c4c4).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_c4c4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c4c4", error)
      
  end subroutine mpifx_scatterv_c4c4



  
  !> Scatter results from one process (type c4c3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_c4c3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c4c3", error)

  end subroutine mpifx_scatterv_c4c3






  !> scatters object of variable length from one process (type c5c5).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_c5c5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c5c5", error)
      
  end subroutine mpifx_scatterv_c5c5



  
  !> Scatter results from one process (type c5c4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_c5c4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c5c4", error)

  end subroutine mpifx_scatterv_c5c4






  !> scatters object of variable length from one process (type c6c6).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_c6c6(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c6c6", error)
      
  end subroutine mpifx_scatterv_c6c6



  
  !> Scatter results from one process (type c6c5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_c6c5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_COMPLEX, recv, size(recv), MPI_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_c6c5", error)

  end subroutine mpifx_scatterv_c6c5






  !> scatters object of variable length from one process (type z1z1).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_z1z1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z1z1", error)
      
  end subroutine mpifx_scatterv_z1z1



  
  !> Scatter results from one process (type z1z0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_z1z0(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, 1, MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z1z0", error)

  end subroutine mpifx_scatterv_z1z0






  !> scatters object of variable length from one process (type z2z2).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_z2z2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z2z2", error)
      
  end subroutine mpifx_scatterv_z2z2



  
  !> Scatter results from one process (type z2z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_z2z1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z2z1", error)

  end subroutine mpifx_scatterv_z2z1






  !> scatters object of variable length from one process (type z3z3).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_z3z3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z3z3", error)
      
  end subroutine mpifx_scatterv_z3z3



  
  !> Scatter results from one process (type z3z2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_z3z2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z3z2", error)

  end subroutine mpifx_scatterv_z3z2






  !> scatters object of variable length from one process (type z4z4).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_z4z4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z4z4", error)
      
  end subroutine mpifx_scatterv_z4z4



  
  !> Scatter results from one process (type z4z3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_z4z3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z4z3", error)

  end subroutine mpifx_scatterv_z4z3






  !> scatters object of variable length from one process (type z5z5).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_z5z5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z5z5", error)
      
  end subroutine mpifx_scatterv_z5z5



  
  !> Scatter results from one process (type z5z4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_z5z4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z5z4", error)

  end subroutine mpifx_scatterv_z5z4






  !> scatters object of variable length from one process (type z6z6).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_z6z6(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z6z6", error)
      
  end subroutine mpifx_scatterv_z6z6



  
  !> Scatter results from one process (type z6z5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_z6z5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    complex(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_DOUBLE_COMPLEX, recv, size(recv), MPI_DOUBLE_COMPLEX, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_z6z5", error)

  end subroutine mpifx_scatterv_z6z5






  !> scatters object of variable length from one process (type l1l1).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_l1l1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l1l1", error)
      
  end subroutine mpifx_scatterv_l1l1



  
  !> Scatter results from one process (type l1l0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_l1l0(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, 1, MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l1l0", error)

  end subroutine mpifx_scatterv_l1l0






  !> scatters object of variable length from one process (type l2l2).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_l2l2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l2l2", error)
      
  end subroutine mpifx_scatterv_l2l2



  
  !> Scatter results from one process (type l2l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_l2l1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l2l1", error)

  end subroutine mpifx_scatterv_l2l1






  !> scatters object of variable length from one process (type l3l3).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_l3l3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l3l3", error)
      
  end subroutine mpifx_scatterv_l3l3



  
  !> Scatter results from one process (type l3l2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_l3l2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l3l2", error)

  end subroutine mpifx_scatterv_l3l2






  !> scatters object of variable length from one process (type l4l4).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_l4l4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l4l4", error)
      
  end subroutine mpifx_scatterv_l4l4



  
  !> Scatter results from one process (type l4l3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_l4l3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l4l3", error)

  end subroutine mpifx_scatterv_l4l3






  !> scatters object of variable length from one process (type l5l5).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_l5l5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l5l5", error)
      
  end subroutine mpifx_scatterv_l5l5



  
  !> Scatter results from one process (type l5l4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_l5l4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l5l4", error)

  end subroutine mpifx_scatterv_l5l4






  !> scatters object of variable length from one process (type l6l6).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_l6l6(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l6l6", error)
      
  end subroutine mpifx_scatterv_l6l6



  
  !> Scatter results from one process (type l6l5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_l6l5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    logical, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)





    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_LOGICAL, recv, size(recv), MPI_LOGICAL, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_l6l5", error)

  end subroutine mpifx_scatterv_l6l5






  !> scatters object of variable length from one process (type h1h1).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_h1h1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h1h1", error)
      
  end subroutine mpifx_scatterv_h1h1



  
  !> Scatter results from one process (type h1h0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_h1h0(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)






    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, len(recv) * 1, MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h1h0", error)

  end subroutine mpifx_scatterv_h1h0






  !> scatters object of variable length from one process (type h2h2).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_h2h2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h2h2", error)
      
  end subroutine mpifx_scatterv_h2h2



  
  !> Scatter results from one process (type h2h1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_h2h1(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)






    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h2h1", error)

  end subroutine mpifx_scatterv_h2h1






  !> scatters object of variable length from one process (type h3h3).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_h3h3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h3h3", error)
      
  end subroutine mpifx_scatterv_h3h3



  
  !> Scatter results from one process (type h3h2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_h3h2(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)






    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h3h2", error)

  end subroutine mpifx_scatterv_h3h2






  !> scatters object of variable length from one process (type h4h4).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_h4h4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h4h4", error)
      
  end subroutine mpifx_scatterv_h4h4



  
  !> Scatter results from one process (type h4h3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_h4h3(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)






    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h4h3", error)

  end subroutine mpifx_scatterv_h4h3






  !> scatters object of variable length from one process (type h5h5).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_h5h5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h5h5", error)
      
  end subroutine mpifx_scatterv_h5h5



  
  !> Scatter results from one process (type h5h4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_h5h4(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)






    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h5h4", error)

  end subroutine mpifx_scatterv_h5h4






  !> scatters object of variable length from one process (type h6h6).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_h6h6(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)




    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h6h6", error)
      
  end subroutine mpifx_scatterv_h6h6



  
  !> Scatter results from one process (type h6h5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_h6h5(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:,:)
    integer, intent(in) :: sendcounts(:)
    character(len=*), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)






    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then

        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, MPI_CHARACTER, recv, len(recv) * size(recv), MPI_CHARACTER, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_h6h5", error)

  end subroutine mpifx_scatterv_h6h5



end module mpifx_scatterv_module
