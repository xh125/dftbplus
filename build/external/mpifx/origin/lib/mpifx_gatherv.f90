







!> Contains wrapper for \c MPI_gatherv
module mpifx_gatherv_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_gatherv

  !> Gathers scalars/arrays of different lengths on a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type. The third argument must have the size of the second times the number
  !! of processes taking part in the gathering. The fourth argument must be
  !! an array of integers corresponding to the array sizes received from each
  !! processor. The displacements at which to place the incoming data can be
  !! given as an optional argument. By default they are computed from recvcounts,
  !! assuming ordering with processor rank.
  !!
  !! \see MPI documentation (\c MPI_gatherv)
  !!
  !! Example:
  !!
  !!     program test_gatherv
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       real, allocatable :: send1(:)
  !!       real, allocatable :: recv1(:)
  !!       integer, allocatable :: recvcounts(:)
  !!       integer :: ii, nrecv
  !!       character(100) :: formstr
  !!       character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!
  !!       ! I1 -> I1
  !!       allocate(send1(mycomm%rank+1))
  !!       send1 = 1.0*mycomm%rank
  !!       if (mycomm%lead) then
  !!         ! recv1 size is 1+2+3+...+mycomm%size
  !!         nrecv = mycomm%size*(mycomm%size+1)/2
  !!         allocate(recv1(nrecv))
  !!         recv1(:) = 0
  !!         allocate(recvcounts(mycomm%size))
  !!         do ii = 1, mycomm%size
  !!           recvcounts(ii) = ii
  !!         end do
  !!       else
  !!         allocate(recv1(0))
  !!       end if
  !!
  !!       write(*, *) mycomm%rank, "Send1 buffer:", send1(:)
  !!       call mpifx_gatherv(mycomm, send1, recv1, recvcounts)
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!       end if
  !!
  !!       call mpifx_finalize()
  !!
  !!     end program test_gatherv
  !!
  interface mpifx_gatherv
        module procedure mpifx_gatherv_i1i1
        module procedure mpifx_gatherv_i2i2
        module procedure mpifx_gatherv_i3i3
        module procedure mpifx_gatherv_i4i4
        module procedure mpifx_gatherv_i5i5
        module procedure mpifx_gatherv_i6i6
      module procedure mpifx_gatherv_i0i1
        module procedure mpifx_gatherv_s1s1
        module procedure mpifx_gatherv_s2s2
        module procedure mpifx_gatherv_s3s3
        module procedure mpifx_gatherv_s4s4
        module procedure mpifx_gatherv_s5s5
        module procedure mpifx_gatherv_s6s6
      module procedure mpifx_gatherv_s0s1
        module procedure mpifx_gatherv_d1d1
        module procedure mpifx_gatherv_d2d2
        module procedure mpifx_gatherv_d3d3
        module procedure mpifx_gatherv_d4d4
        module procedure mpifx_gatherv_d5d5
        module procedure mpifx_gatherv_d6d6
      module procedure mpifx_gatherv_d0d1
        module procedure mpifx_gatherv_c1c1
        module procedure mpifx_gatherv_c2c2
        module procedure mpifx_gatherv_c3c3
        module procedure mpifx_gatherv_c4c4
        module procedure mpifx_gatherv_c5c5
        module procedure mpifx_gatherv_c6c6
      module procedure mpifx_gatherv_c0c1
        module procedure mpifx_gatherv_z1z1
        module procedure mpifx_gatherv_z2z2
        module procedure mpifx_gatherv_z3z3
        module procedure mpifx_gatherv_z4z4
        module procedure mpifx_gatherv_z5z5
        module procedure mpifx_gatherv_z6z6
      module procedure mpifx_gatherv_z0z1
        module procedure mpifx_gatherv_l1l1
        module procedure mpifx_gatherv_l2l2
        module procedure mpifx_gatherv_l3l3
        module procedure mpifx_gatherv_l4l4
        module procedure mpifx_gatherv_l5l5
        module procedure mpifx_gatherv_l6l6
      module procedure mpifx_gatherv_l0l1
        module procedure mpifx_gatherv_h1h1
        module procedure mpifx_gatherv_h2h2
        module procedure mpifx_gatherv_h3h3
        module procedure mpifx_gatherv_h4h4
        module procedure mpifx_gatherv_h5h5
        module procedure mpifx_gatherv_h6h6
      module procedure mpifx_gatherv_h0h1
  end interface mpifx_gatherv


contains




  !> Gathers results of variable length on one process (type i1i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_i1i1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:)
    integer, intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_INTEGER, recv, recvcounts, displs0, &
        & MPI_INTEGER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_i1i1", error)

  end subroutine mpifx_gatherv_i1i1


  !> Gathers results of variable length on one process (type i2i2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_i2i2(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:)
    integer, intent(out) :: recv(:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_INTEGER, recv, recvcounts, displs0, &
        & MPI_INTEGER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_i2i2", error)

  end subroutine mpifx_gatherv_i2i2


  !> Gathers results of variable length on one process (type i3i3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_i3i3(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:)
    integer, intent(out) :: recv(:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_INTEGER, recv, recvcounts, displs0, &
        & MPI_INTEGER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_i3i3", error)

  end subroutine mpifx_gatherv_i3i3


  !> Gathers results of variable length on one process (type i4i4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_i4i4(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_INTEGER, recv, recvcounts, displs0, &
        & MPI_INTEGER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_i4i4", error)

  end subroutine mpifx_gatherv_i4i4


  !> Gathers results of variable length on one process (type i5i5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_i5i5(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_INTEGER, recv, recvcounts, displs0, &
        & MPI_INTEGER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_i5i5", error)

  end subroutine mpifx_gatherv_i5i5


  !> Gathers results of variable length on one process (type i6i6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_i6i6(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send(:,:,:,:,:,:)
    integer, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_INTEGER, recv, recvcounts, displs0, &
        & MPI_INTEGER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_i6i6", error)

  end subroutine mpifx_gatherv_i6i6



  !> Gathers results on one process (type i0i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_i0i1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: send
    integer, intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then


      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, 1, MPI_INTEGER, recv, recvcounts, displs0, &
         & MPI_INTEGER,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_i0i1", error)

  end subroutine mpifx_gatherv_i0i1





  !> Gathers results of variable length on one process (type s1s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_s1s1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:)
    real(sp), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_REAL, recv, recvcounts, displs0, &
        & MPI_REAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_s1s1", error)

  end subroutine mpifx_gatherv_s1s1


  !> Gathers results of variable length on one process (type s2s2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_s2s2(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:)
    real(sp), intent(out) :: recv(:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_REAL, recv, recvcounts, displs0, &
        & MPI_REAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_s2s2", error)

  end subroutine mpifx_gatherv_s2s2


  !> Gathers results of variable length on one process (type s3s3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_s3s3(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:)
    real(sp), intent(out) :: recv(:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_REAL, recv, recvcounts, displs0, &
        & MPI_REAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_s3s3", error)

  end subroutine mpifx_gatherv_s3s3


  !> Gathers results of variable length on one process (type s4s4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_s4s4(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_REAL, recv, recvcounts, displs0, &
        & MPI_REAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_s4s4", error)

  end subroutine mpifx_gatherv_s4s4


  !> Gathers results of variable length on one process (type s5s5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_s5s5(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_REAL, recv, recvcounts, displs0, &
        & MPI_REAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_s5s5", error)

  end subroutine mpifx_gatherv_s5s5


  !> Gathers results of variable length on one process (type s6s6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_s6s6(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send(:,:,:,:,:,:)
    real(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_REAL, recv, recvcounts, displs0, &
        & MPI_REAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_s6s6", error)

  end subroutine mpifx_gatherv_s6s6



  !> Gathers results on one process (type s0s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_s0s1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: send
    real(sp), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then


      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, 1, MPI_REAL, recv, recvcounts, displs0, &
         & MPI_REAL,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_s0s1", error)

  end subroutine mpifx_gatherv_s0s1





  !> Gathers results of variable length on one process (type d1d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_d1d1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:)
    real(dp), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_PRECISION, recv, recvcounts, displs0, &
        & MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_d1d1", error)

  end subroutine mpifx_gatherv_d1d1


  !> Gathers results of variable length on one process (type d2d2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_d2d2(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:)
    real(dp), intent(out) :: recv(:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_PRECISION, recv, recvcounts, displs0, &
        & MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_d2d2", error)

  end subroutine mpifx_gatherv_d2d2


  !> Gathers results of variable length on one process (type d3d3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_d3d3(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:)
    real(dp), intent(out) :: recv(:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_PRECISION, recv, recvcounts, displs0, &
        & MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_d3d3", error)

  end subroutine mpifx_gatherv_d3d3


  !> Gathers results of variable length on one process (type d4d4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_d4d4(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_PRECISION, recv, recvcounts, displs0, &
        & MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_d4d4", error)

  end subroutine mpifx_gatherv_d4d4


  !> Gathers results of variable length on one process (type d5d5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_d5d5(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_PRECISION, recv, recvcounts, displs0, &
        & MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_d5d5", error)

  end subroutine mpifx_gatherv_d5d5


  !> Gathers results of variable length on one process (type d6d6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_d6d6(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send(:,:,:,:,:,:)
    real(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_PRECISION, recv, recvcounts, displs0, &
        & MPI_DOUBLE_PRECISION, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_d6d6", error)

  end subroutine mpifx_gatherv_d6d6



  !> Gathers results on one process (type d0d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_d0d1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: send
    real(dp), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then


      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, 1, MPI_DOUBLE_PRECISION, recv, recvcounts, displs0, &
         & MPI_DOUBLE_PRECISION,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_d0d1", error)

  end subroutine mpifx_gatherv_d0d1





  !> Gathers results of variable length on one process (type c1c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_c1c1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:)
    complex(sp), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_COMPLEX, recv, recvcounts, displs0, &
        & MPI_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_c1c1", error)

  end subroutine mpifx_gatherv_c1c1


  !> Gathers results of variable length on one process (type c2c2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_c2c2(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:)
    complex(sp), intent(out) :: recv(:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_COMPLEX, recv, recvcounts, displs0, &
        & MPI_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_c2c2", error)

  end subroutine mpifx_gatherv_c2c2


  !> Gathers results of variable length on one process (type c3c3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_c3c3(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:)
    complex(sp), intent(out) :: recv(:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_COMPLEX, recv, recvcounts, displs0, &
        & MPI_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_c3c3", error)

  end subroutine mpifx_gatherv_c3c3


  !> Gathers results of variable length on one process (type c4c4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_c4c4(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_COMPLEX, recv, recvcounts, displs0, &
        & MPI_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_c4c4", error)

  end subroutine mpifx_gatherv_c4c4


  !> Gathers results of variable length on one process (type c5c5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_c5c5(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_COMPLEX, recv, recvcounts, displs0, &
        & MPI_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_c5c5", error)

  end subroutine mpifx_gatherv_c5c5


  !> Gathers results of variable length on one process (type c6c6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_c6c6(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send(:,:,:,:,:,:)
    complex(sp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_COMPLEX, recv, recvcounts, displs0, &
        & MPI_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_c6c6", error)

  end subroutine mpifx_gatherv_c6c6



  !> Gathers results on one process (type c0c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_c0c1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: send
    complex(sp), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then


      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, 1, MPI_COMPLEX, recv, recvcounts, displs0, &
         & MPI_COMPLEX,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_c0c1", error)

  end subroutine mpifx_gatherv_c0c1





  !> Gathers results of variable length on one process (type z1z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_z1z1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:)
    complex(dp), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_COMPLEX, recv, recvcounts, displs0, &
        & MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_z1z1", error)

  end subroutine mpifx_gatherv_z1z1


  !> Gathers results of variable length on one process (type z2z2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_z2z2(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:)
    complex(dp), intent(out) :: recv(:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_COMPLEX, recv, recvcounts, displs0, &
        & MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_z2z2", error)

  end subroutine mpifx_gatherv_z2z2


  !> Gathers results of variable length on one process (type z3z3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_z3z3(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:)
    complex(dp), intent(out) :: recv(:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_COMPLEX, recv, recvcounts, displs0, &
        & MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_z3z3", error)

  end subroutine mpifx_gatherv_z3z3


  !> Gathers results of variable length on one process (type z4z4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_z4z4(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_COMPLEX, recv, recvcounts, displs0, &
        & MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_z4z4", error)

  end subroutine mpifx_gatherv_z4z4


  !> Gathers results of variable length on one process (type z5z5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_z5z5(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_COMPLEX, recv, recvcounts, displs0, &
        & MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_z5z5", error)

  end subroutine mpifx_gatherv_z5z5


  !> Gathers results of variable length on one process (type z6z6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_z6z6(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send(:,:,:,:,:,:)
    complex(dp), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_DOUBLE_COMPLEX, recv, recvcounts, displs0, &
        & MPI_DOUBLE_COMPLEX, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_z6z6", error)

  end subroutine mpifx_gatherv_z6z6



  !> Gathers results on one process (type z0z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_z0z1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: send
    complex(dp), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then


      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, 1, MPI_DOUBLE_COMPLEX, recv, recvcounts, displs0, &
         & MPI_DOUBLE_COMPLEX,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_z0z1", error)

  end subroutine mpifx_gatherv_z0z1





  !> Gathers results of variable length on one process (type l1l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_l1l1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:)
    logical, intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_LOGICAL, recv, recvcounts, displs0, &
        & MPI_LOGICAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_l1l1", error)

  end subroutine mpifx_gatherv_l1l1


  !> Gathers results of variable length on one process (type l2l2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_l2l2(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:)
    logical, intent(out) :: recv(:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_LOGICAL, recv, recvcounts, displs0, &
        & MPI_LOGICAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_l2l2", error)

  end subroutine mpifx_gatherv_l2l2


  !> Gathers results of variable length on one process (type l3l3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_l3l3(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:)
    logical, intent(out) :: recv(:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_LOGICAL, recv, recvcounts, displs0, &
        & MPI_LOGICAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_l3l3", error)

  end subroutine mpifx_gatherv_l3l3


  !> Gathers results of variable length on one process (type l4l4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_l4l4(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_LOGICAL, recv, recvcounts, displs0, &
        & MPI_LOGICAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_l4l4", error)

  end subroutine mpifx_gatherv_l4l4


  !> Gathers results of variable length on one process (type l5l5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_l5l5(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_LOGICAL, recv, recvcounts, displs0, &
        & MPI_LOGICAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_l5l5", error)

  end subroutine mpifx_gatherv_l5l5


  !> Gathers results of variable length on one process (type l6l6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_l6l6(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send(:,:,:,:,:,:)
    logical, intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_LOGICAL, recv, recvcounts, displs0, &
        & MPI_LOGICAL, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_l6l6", error)

  end subroutine mpifx_gatherv_l6l6



  !> Gathers results on one process (type l0l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_l0l1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: send
    logical, intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then


      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, 1, MPI_LOGICAL, recv, recvcounts, displs0, &
         & MPI_LOGICAL,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_l0l1", error)

  end subroutine mpifx_gatherv_l0l1





  !> Gathers results of variable length on one process (type h1h1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_h1h1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:)
    character(len=*), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_CHARACTER, recv, recvcounts, displs0, &
        & MPI_CHARACTER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_h1h1", error)

  end subroutine mpifx_gatherv_h1h1


  !> Gathers results of variable length on one process (type h2h2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_h2h2(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:)
    character(len=*), intent(out) :: recv(:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_CHARACTER, recv, recvcounts, displs0, &
        & MPI_CHARACTER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_h2h2", error)

  end subroutine mpifx_gatherv_h2h2


  !> Gathers results of variable length on one process (type h3h3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_h3h3(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:)
    character(len=*), intent(out) :: recv(:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_CHARACTER, recv, recvcounts, displs0, &
        & MPI_CHARACTER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_h3h3", error)

  end subroutine mpifx_gatherv_h3h3


  !> Gathers results of variable length on one process (type h4h4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_h4h4(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_CHARACTER, recv, recvcounts, displs0, &
        & MPI_CHARACTER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_h4h4", error)

  end subroutine mpifx_gatherv_h4h4


  !> Gathers results of variable length on one process (type h5h5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_h5h5(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_CHARACTER, recv, recvcounts, displs0, &
        & MPI_CHARACTER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_h5h5", error)

  end subroutine mpifx_gatherv_h5h5


  !> Gathers results of variable length on one process (type h6h6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_h6h6(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send(:,:,:,:,:,:)
    character(len=*), intent(out) :: recv(:,:,:,:,:,:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
        locLast = maxloc(displs0)

        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do

      end if
    end if

    call mpi_gatherv(send, size(send), MPI_CHARACTER, recv, recvcounts, displs0, &
        & MPI_CHARACTER, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_h6h6", error)

  end subroutine mpifx_gatherv_h6h6



  !> Gathers results on one process (type h0h1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_h0h1(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    character(len=*), intent(in) :: send
    character(len=*), intent(out) :: recv(:)
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

  if (present(root)) then
    root0 = root
  else
    root0 = mycomm%leadrank
  end if

    if (mycomm%rank == root0) then


      allocate(displs0(mycomm%size))
      if (present(displs)) then

        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, 1, MPI_CHARACTER, recv, recvcounts, displs0, &
         & MPI_CHARACTER,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_h0h1", error)

  end subroutine mpifx_gatherv_h0h1



end module mpifx_gatherv_module
