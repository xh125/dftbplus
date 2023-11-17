
!> Contains wrapper for \c MPI_REDUCE.
module mpifx_reduce_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_reduce, mpifx_reduceip

  !> Reduces a scalar/array on a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third 
  !! arguments can be of type integer (i), real (s), double precision (d), 
  !! complex (c), double complex (z) or logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type and rank.
  !!
  !! \see MPI documentation (\c MPI_REDUCE)
  !!
  !! Example:
  !!
  !!     program test_reduce
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       integer, parameter :: dp = kind(1.0d0)
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       real(dp) :: valr(3), resvalr(3)
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!       valr(:) = [ (mycomm%rank + 1) * 1.2_dp, &
  !!           & (mycomm%rank + 1) * 4.3_dp, (mycomm%rank + 1) * 3.8_dp ]
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 3, mycomm%rank, &
  !!           & "Value to be operated on:", valr(:)
  !!       call mpifx_reduce(mycomm, valr, resvalr, MPI_PROD)
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
  !!           & "Obtained result (prod):", resvalr(:)
  !!       call mpifx_finalize()
  !!       
  !!     end program test_reduce
  !!
  interface mpifx_reduce
    module procedure mpifx_reduce_i0
    module procedure mpifx_reduce_i1
    module procedure mpifx_reduce_i2
    module procedure mpifx_reduce_i3
    module procedure mpifx_reduce_i4
    module procedure mpifx_reduce_i5
    module procedure mpifx_reduce_i6
    module procedure mpifx_reduce_s0
    module procedure mpifx_reduce_s1
    module procedure mpifx_reduce_s2
    module procedure mpifx_reduce_s3
    module procedure mpifx_reduce_s4
    module procedure mpifx_reduce_s5
    module procedure mpifx_reduce_s6
    module procedure mpifx_reduce_d0
    module procedure mpifx_reduce_d1
    module procedure mpifx_reduce_d2
    module procedure mpifx_reduce_d3
    module procedure mpifx_reduce_d4
    module procedure mpifx_reduce_d5
    module procedure mpifx_reduce_d6
    module procedure mpifx_reduce_c0
    module procedure mpifx_reduce_c1
    module procedure mpifx_reduce_c2
    module procedure mpifx_reduce_c3
    module procedure mpifx_reduce_c4
    module procedure mpifx_reduce_c5
    module procedure mpifx_reduce_c6
    module procedure mpifx_reduce_z0
    module procedure mpifx_reduce_z1
    module procedure mpifx_reduce_z2
    module procedure mpifx_reduce_z3
    module procedure mpifx_reduce_z4
    module procedure mpifx_reduce_z5
    module procedure mpifx_reduce_z6
    module procedure mpifx_reduce_l0
    module procedure mpifx_reduce_l1
    module procedure mpifx_reduce_l2
    module procedure mpifx_reduce_l3
    module procedure mpifx_reduce_l4
    module procedure mpifx_reduce_l5
    module procedure mpifx_reduce_l6
  end interface mpifx_reduce


  !> Reduces a scalar/array on a given node in place.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of type
  !! integer (i), real (s), double precision (d), complex (c), double complex
  !! (z) or logical (l). Its rank can vary from zero (scalar) up to the
  !! maximum rank.
  !!
  !! \see MPI documentation (\c MPI_REDUCE)
  !!
  !!
  !! Example:
  !!
  !!     program test_reduceip
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       integer, parameter :: dp = kind(1.0d0)
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       real(dp) :: resvalr(3)
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!       resvalr(:) = [ (mycomm%rank + 1) * 1.2_dp, &
  !!           & (mycomm%rank + 1) * 4.3_dp, (mycomm%rank + 1) * 3.8_dp ]
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 3, mycomm%rank, &
  !!           & "Value to be operated on:", resvalr(:)
  !!       call mpifx_reduceip(mycomm, resvalr, MPI_PROD)
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
  !!           & "Obtained result (prod):", resvalr(:)
  !!       call mpifx_finalize()
  !!       
  !!     end program test_reduceip
  !!  
  interface mpifx_reduceip
    module procedure mpifx_reduceip_i0
    module procedure mpifx_reduceip_i1
    module procedure mpifx_reduceip_i2
    module procedure mpifx_reduceip_i3
    module procedure mpifx_reduceip_i4
    module procedure mpifx_reduceip_i5
    module procedure mpifx_reduceip_i6
    module procedure mpifx_reduceip_s0
    module procedure mpifx_reduceip_s1
    module procedure mpifx_reduceip_s2
    module procedure mpifx_reduceip_s3
    module procedure mpifx_reduceip_s4
    module procedure mpifx_reduceip_s5
    module procedure mpifx_reduceip_s6
    module procedure mpifx_reduceip_d0
    module procedure mpifx_reduceip_d1
    module procedure mpifx_reduceip_d2
    module procedure mpifx_reduceip_d3
    module procedure mpifx_reduceip_d4
    module procedure mpifx_reduceip_d5
    module procedure mpifx_reduceip_d6
    module procedure mpifx_reduceip_c0
    module procedure mpifx_reduceip_c1
    module procedure mpifx_reduceip_c2
    module procedure mpifx_reduceip_c3
    module procedure mpifx_reduceip_c4
    module procedure mpifx_reduceip_c5
    module procedure mpifx_reduceip_c6
    module procedure mpifx_reduceip_z0
    module procedure mpifx_reduceip_z1
    module procedure mpifx_reduceip_z2
    module procedure mpifx_reduceip_z3
    module procedure mpifx_reduceip_z4
    module procedure mpifx_reduceip_z5
    module procedure mpifx_reduceip_z6
    module procedure mpifx_reduceip_l0
    module procedure mpifx_reduceip_l1
    module procedure mpifx_reduceip_l2
    module procedure mpifx_reduceip_l3
    module procedure mpifx_reduceip_l4
    module procedure mpifx_reduceip_l5
    module procedure mpifx_reduceip_l6
  end interface mpifx_reduceip

contains


  

  




  !> Reduces on one process (type i0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_i0(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: orig
    integer, intent(inout) :: reduced
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, 1, MPI_INTEGER, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i0", error)
      
  end subroutine mpifx_reduce_i0


  
  !> Reduces results on one process (type i0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_i0(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(inout) :: origred
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    integer :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, 1, MPI_INTEGER, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, 1, MPI_INTEGER, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i0", error)
      
  end subroutine mpifx_reduceip_i0






  !> Reduces on one process (type i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_i1(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: orig(:)
    integer, intent(inout) :: reduced(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_INTEGER, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i1", error)
      
  end subroutine mpifx_reduce_i1


  
  !> Reduces results on one process (type i1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_i1(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(inout) :: origred(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    integer :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i1", error)
      
  end subroutine mpifx_reduceip_i1






  !> Reduces on one process (type i2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_i2(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: orig(:,:)
    integer, intent(inout) :: reduced(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_INTEGER, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i2", error)
      
  end subroutine mpifx_reduce_i2


  
  !> Reduces results on one process (type i2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_i2(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(inout) :: origred(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    integer :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i2", error)
      
  end subroutine mpifx_reduceip_i2






  !> Reduces on one process (type i3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_i3(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: orig(:,:,:)
    integer, intent(inout) :: reduced(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_INTEGER, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i3", error)
      
  end subroutine mpifx_reduce_i3


  
  !> Reduces results on one process (type i3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_i3(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(inout) :: origred(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    integer :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i3", error)
      
  end subroutine mpifx_reduceip_i3






  !> Reduces on one process (type i4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_i4(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: orig(:,:,:,:)
    integer, intent(inout) :: reduced(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_INTEGER, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i4", error)
      
  end subroutine mpifx_reduce_i4


  
  !> Reduces results on one process (type i4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_i4(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(inout) :: origred(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    integer :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i4", error)
      
  end subroutine mpifx_reduceip_i4






  !> Reduces on one process (type i5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_i5(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: orig(:,:,:,:,:)
    integer, intent(inout) :: reduced(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_INTEGER, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i5", error)
      
  end subroutine mpifx_reduce_i5


  
  !> Reduces results on one process (type i5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_i5(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(inout) :: origred(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    integer :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i5", error)
      
  end subroutine mpifx_reduceip_i5






  !> Reduces on one process (type i6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_i6(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: orig(:,:,:,:,:,:)
    integer, intent(inout) :: reduced(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_INTEGER, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i6", error)
      
  end subroutine mpifx_reduce_i6


  
  !> Reduces results on one process (type i6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_i6(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(inout) :: origred(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    integer :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_INTEGER, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_i6", error)
      
  end subroutine mpifx_reduceip_i6






  !> Reduces on one process (type s0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_s0(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: orig
    real(sp), intent(inout) :: reduced
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, 1, MPI_REAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s0", error)
      
  end subroutine mpifx_reduce_s0


  
  !> Reduces results on one process (type s0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_s0(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(inout) :: origred
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, 1, MPI_REAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, 1, MPI_REAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s0", error)
      
  end subroutine mpifx_reduceip_s0






  !> Reduces on one process (type s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_s1(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: orig(:)
    real(sp), intent(inout) :: reduced(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_REAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s1", error)
      
  end subroutine mpifx_reduce_s1


  
  !> Reduces results on one process (type s1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_s1(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(inout) :: origred(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_REAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_REAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s1", error)
      
  end subroutine mpifx_reduceip_s1






  !> Reduces on one process (type s2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_s2(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: orig(:,:)
    real(sp), intent(inout) :: reduced(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_REAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s2", error)
      
  end subroutine mpifx_reduce_s2


  
  !> Reduces results on one process (type s2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_s2(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(inout) :: origred(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_REAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_REAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s2", error)
      
  end subroutine mpifx_reduceip_s2






  !> Reduces on one process (type s3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_s3(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: orig(:,:,:)
    real(sp), intent(inout) :: reduced(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_REAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s3", error)
      
  end subroutine mpifx_reduce_s3


  
  !> Reduces results on one process (type s3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_s3(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(inout) :: origred(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_REAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_REAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s3", error)
      
  end subroutine mpifx_reduceip_s3






  !> Reduces on one process (type s4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_s4(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: orig(:,:,:,:)
    real(sp), intent(inout) :: reduced(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_REAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s4", error)
      
  end subroutine mpifx_reduce_s4


  
  !> Reduces results on one process (type s4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_s4(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(inout) :: origred(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_REAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_REAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s4", error)
      
  end subroutine mpifx_reduceip_s4






  !> Reduces on one process (type s5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_s5(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: orig(:,:,:,:,:)
    real(sp), intent(inout) :: reduced(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_REAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s5", error)
      
  end subroutine mpifx_reduce_s5


  
  !> Reduces results on one process (type s5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_s5(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(inout) :: origred(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_REAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_REAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s5", error)
      
  end subroutine mpifx_reduceip_s5






  !> Reduces on one process (type s6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_s6(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(in) :: orig(:,:,:,:,:,:)
    real(sp), intent(inout) :: reduced(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_REAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s6", error)
      
  end subroutine mpifx_reduce_s6


  
  !> Reduces results on one process (type s6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_s6(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(sp), intent(inout) :: origred(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_REAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_REAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_s6", error)
      
  end subroutine mpifx_reduceip_s6






  !> Reduces on one process (type d0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_d0(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: orig
    real(dp), intent(inout) :: reduced
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, 1, MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d0", error)
      
  end subroutine mpifx_reduce_d0


  
  !> Reduces results on one process (type d0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_d0(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(inout) :: origred
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, 1, MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, 1, MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d0", error)
      
  end subroutine mpifx_reduceip_d0






  !> Reduces on one process (type d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_d1(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: orig(:)
    real(dp), intent(inout) :: reduced(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d1", error)
      
  end subroutine mpifx_reduce_d1


  
  !> Reduces results on one process (type d1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_d1(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(inout) :: origred(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d1", error)
      
  end subroutine mpifx_reduceip_d1






  !> Reduces on one process (type d2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_d2(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: orig(:,:)
    real(dp), intent(inout) :: reduced(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d2", error)
      
  end subroutine mpifx_reduce_d2


  
  !> Reduces results on one process (type d2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_d2(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(inout) :: origred(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d2", error)
      
  end subroutine mpifx_reduceip_d2






  !> Reduces on one process (type d3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_d3(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: orig(:,:,:)
    real(dp), intent(inout) :: reduced(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d3", error)
      
  end subroutine mpifx_reduce_d3


  
  !> Reduces results on one process (type d3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_d3(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(inout) :: origred(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d3", error)
      
  end subroutine mpifx_reduceip_d3






  !> Reduces on one process (type d4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_d4(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: orig(:,:,:,:)
    real(dp), intent(inout) :: reduced(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d4", error)
      
  end subroutine mpifx_reduce_d4


  
  !> Reduces results on one process (type d4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_d4(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(inout) :: origred(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d4", error)
      
  end subroutine mpifx_reduceip_d4






  !> Reduces on one process (type d5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_d5(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: orig(:,:,:,:,:)
    real(dp), intent(inout) :: reduced(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d5", error)
      
  end subroutine mpifx_reduce_d5


  
  !> Reduces results on one process (type d5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_d5(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(inout) :: origred(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d5", error)
      
  end subroutine mpifx_reduceip_d5






  !> Reduces on one process (type d6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_d6(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(in) :: orig(:,:,:,:,:,:)
    real(dp), intent(inout) :: reduced(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d6", error)
      
  end subroutine mpifx_reduce_d6


  
  !> Reduces results on one process (type d6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_d6(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    real(dp), intent(inout) :: origred(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    real(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_PRECISION, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_d6", error)
      
  end subroutine mpifx_reduceip_d6






  !> Reduces on one process (type c0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_c0(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: orig
    complex(sp), intent(inout) :: reduced
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, 1, MPI_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c0", error)
      
  end subroutine mpifx_reduce_c0


  
  !> Reduces results on one process (type c0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_c0(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(inout) :: origred
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, 1, MPI_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, 1, MPI_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c0", error)
      
  end subroutine mpifx_reduceip_c0






  !> Reduces on one process (type c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_c1(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: orig(:)
    complex(sp), intent(inout) :: reduced(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c1", error)
      
  end subroutine mpifx_reduce_c1


  
  !> Reduces results on one process (type c1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_c1(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(inout) :: origred(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c1", error)
      
  end subroutine mpifx_reduceip_c1






  !> Reduces on one process (type c2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_c2(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: orig(:,:)
    complex(sp), intent(inout) :: reduced(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c2", error)
      
  end subroutine mpifx_reduce_c2


  
  !> Reduces results on one process (type c2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_c2(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(inout) :: origred(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c2", error)
      
  end subroutine mpifx_reduceip_c2






  !> Reduces on one process (type c3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_c3(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: orig(:,:,:)
    complex(sp), intent(inout) :: reduced(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c3", error)
      
  end subroutine mpifx_reduce_c3


  
  !> Reduces results on one process (type c3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_c3(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(inout) :: origred(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c3", error)
      
  end subroutine mpifx_reduceip_c3






  !> Reduces on one process (type c4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_c4(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: orig(:,:,:,:)
    complex(sp), intent(inout) :: reduced(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c4", error)
      
  end subroutine mpifx_reduce_c4


  
  !> Reduces results on one process (type c4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_c4(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(inout) :: origred(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c4", error)
      
  end subroutine mpifx_reduceip_c4






  !> Reduces on one process (type c5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_c5(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: orig(:,:,:,:,:)
    complex(sp), intent(inout) :: reduced(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c5", error)
      
  end subroutine mpifx_reduce_c5


  
  !> Reduces results on one process (type c5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_c5(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(inout) :: origred(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c5", error)
      
  end subroutine mpifx_reduceip_c5






  !> Reduces on one process (type c6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_c6(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(in) :: orig(:,:,:,:,:,:)
    complex(sp), intent(inout) :: reduced(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c6", error)
      
  end subroutine mpifx_reduce_c6


  
  !> Reduces results on one process (type c6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_c6(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(sp), intent(inout) :: origred(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(sp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_c6", error)
      
  end subroutine mpifx_reduceip_c6






  !> Reduces on one process (type z0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_z0(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: orig
    complex(dp), intent(inout) :: reduced
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, 1, MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z0", error)
      
  end subroutine mpifx_reduce_z0


  
  !> Reduces results on one process (type z0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_z0(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(inout) :: origred
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, 1, MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, 1, MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z0", error)
      
  end subroutine mpifx_reduceip_z0






  !> Reduces on one process (type z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_z1(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: orig(:)
    complex(dp), intent(inout) :: reduced(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z1", error)
      
  end subroutine mpifx_reduce_z1


  
  !> Reduces results on one process (type z1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_z1(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(inout) :: origred(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z1", error)
      
  end subroutine mpifx_reduceip_z1






  !> Reduces on one process (type z2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_z2(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: orig(:,:)
    complex(dp), intent(inout) :: reduced(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z2", error)
      
  end subroutine mpifx_reduce_z2


  
  !> Reduces results on one process (type z2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_z2(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(inout) :: origred(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z2", error)
      
  end subroutine mpifx_reduceip_z2






  !> Reduces on one process (type z3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_z3(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: orig(:,:,:)
    complex(dp), intent(inout) :: reduced(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z3", error)
      
  end subroutine mpifx_reduce_z3


  
  !> Reduces results on one process (type z3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_z3(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(inout) :: origred(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z3", error)
      
  end subroutine mpifx_reduceip_z3






  !> Reduces on one process (type z4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_z4(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: orig(:,:,:,:)
    complex(dp), intent(inout) :: reduced(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z4", error)
      
  end subroutine mpifx_reduce_z4


  
  !> Reduces results on one process (type z4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_z4(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(inout) :: origred(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z4", error)
      
  end subroutine mpifx_reduceip_z4






  !> Reduces on one process (type z5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_z5(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: orig(:,:,:,:,:)
    complex(dp), intent(inout) :: reduced(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z5", error)
      
  end subroutine mpifx_reduce_z5


  
  !> Reduces results on one process (type z5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_z5(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(inout) :: origred(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z5", error)
      
  end subroutine mpifx_reduceip_z5






  !> Reduces on one process (type z6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_z6(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(in) :: orig(:,:,:,:,:,:)
    complex(dp), intent(inout) :: reduced(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z6", error)
      
  end subroutine mpifx_reduce_z6


  
  !> Reduces results on one process (type z6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_z6(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    complex(dp), intent(inout) :: origred(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    complex(dp) :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_DOUBLE_COMPLEX, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_z6", error)
      
  end subroutine mpifx_reduceip_z6






  !> Reduces on one process (type l0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_l0(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: orig
    logical, intent(inout) :: reduced
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, 1, MPI_LOGICAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l0", error)
      
  end subroutine mpifx_reduce_l0


  
  !> Reduces results on one process (type l0).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_l0(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(inout) :: origred
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    logical :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, 1, MPI_LOGICAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, 1, MPI_LOGICAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l0", error)
      
  end subroutine mpifx_reduceip_l0






  !> Reduces on one process (type l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_l1(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: orig(:)
    logical, intent(inout) :: reduced(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_LOGICAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l1", error)
      
  end subroutine mpifx_reduce_l1


  
  !> Reduces results on one process (type l1).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_l1(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(inout) :: origred(:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    logical :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l1", error)
      
  end subroutine mpifx_reduceip_l1






  !> Reduces on one process (type l2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_l2(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: orig(:,:)
    logical, intent(inout) :: reduced(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_LOGICAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l2", error)
      
  end subroutine mpifx_reduce_l2


  
  !> Reduces results on one process (type l2).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_l2(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(inout) :: origred(:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    logical :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l2", error)
      
  end subroutine mpifx_reduceip_l2






  !> Reduces on one process (type l3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_l3(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: orig(:,:,:)
    logical, intent(inout) :: reduced(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_LOGICAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l3", error)
      
  end subroutine mpifx_reduce_l3


  
  !> Reduces results on one process (type l3).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_l3(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(inout) :: origred(:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    logical :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l3", error)
      
  end subroutine mpifx_reduceip_l3






  !> Reduces on one process (type l4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_l4(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: orig(:,:,:,:)
    logical, intent(inout) :: reduced(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_LOGICAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l4", error)
      
  end subroutine mpifx_reduce_l4


  
  !> Reduces results on one process (type l4).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_l4(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(inout) :: origred(:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    logical :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l4", error)
      
  end subroutine mpifx_reduceip_l4






  !> Reduces on one process (type l5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_l5(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: orig(:,:,:,:,:)
    logical, intent(inout) :: reduced(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_LOGICAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l5", error)
      
  end subroutine mpifx_reduce_l5


  
  !> Reduces results on one process (type l5).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_l5(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(inout) :: origred(:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    logical :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l5", error)
      
  end subroutine mpifx_reduceip_l5






  !> Reduces on one process (type l6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_l6(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(in) :: orig(:,:,:,:,:,:)
    logical, intent(inout) :: reduced(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)


    call mpi_reduce(orig, reduced, size(orig), MPI_LOGICAL, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l6", error)
      
  end subroutine mpifx_reduce_l6


  
  !> Reduces results on one process (type l6).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_l6(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    logical, intent(inout) :: origred(:,:,:,:,:,:)
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    logical :: dummy

    call getoptarg(mycomm%leadrank, root0, root)


    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, size(origred), MPI_LOGICAL, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_l6", error)
      
  end subroutine mpifx_reduceip_l6



end module mpifx_reduce_module
