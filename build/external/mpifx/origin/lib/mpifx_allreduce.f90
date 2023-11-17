
!> Contains wrapper for \c MPI_ALLREDUCE.
module mpifx_allreduce_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_allreduce, mpifx_allreduceip

  !> Reduces a scalar/array on all nodes.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type and rank.
  !!
  !! \see MPI documentation (\c MPI_ALLREDUCE)
  !!
  !! Example:
  !!
  !!     program test_allreduce
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
  !!       call mpifx_allreduce(mycomm, valr, resvalr, MPI_PROD)
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
  !!           & "Obtained result (prod):", resvalr(:)
  !!       call mpifx_finalize()
  !!
  !!     end program test_allreduce
  !!
  interface mpifx_allreduce
    module procedure mpifx_allreduce_i0
    module procedure mpifx_allreduce_i1
    module procedure mpifx_allreduce_i2
    module procedure mpifx_allreduce_i3
    module procedure mpifx_allreduce_i4
    module procedure mpifx_allreduce_i5
    module procedure mpifx_allreduce_i6
    module procedure mpifx_allreduce_s0
    module procedure mpifx_allreduce_s1
    module procedure mpifx_allreduce_s2
    module procedure mpifx_allreduce_s3
    module procedure mpifx_allreduce_s4
    module procedure mpifx_allreduce_s5
    module procedure mpifx_allreduce_s6
    module procedure mpifx_allreduce_d0
    module procedure mpifx_allreduce_d1
    module procedure mpifx_allreduce_d2
    module procedure mpifx_allreduce_d3
    module procedure mpifx_allreduce_d4
    module procedure mpifx_allreduce_d5
    module procedure mpifx_allreduce_d6
    module procedure mpifx_allreduce_c0
    module procedure mpifx_allreduce_c1
    module procedure mpifx_allreduce_c2
    module procedure mpifx_allreduce_c3
    module procedure mpifx_allreduce_c4
    module procedure mpifx_allreduce_c5
    module procedure mpifx_allreduce_c6
    module procedure mpifx_allreduce_z0
    module procedure mpifx_allreduce_z1
    module procedure mpifx_allreduce_z2
    module procedure mpifx_allreduce_z3
    module procedure mpifx_allreduce_z4
    module procedure mpifx_allreduce_z5
    module procedure mpifx_allreduce_z6
    module procedure mpifx_allreduce_l0
    module procedure mpifx_allreduce_l1
    module procedure mpifx_allreduce_l2
    module procedure mpifx_allreduce_l3
    module procedure mpifx_allreduce_l4
    module procedure mpifx_allreduce_l5
    module procedure mpifx_allreduce_l6
  end interface mpifx_allreduce


  !> Reduces a scalar/array on all nodes in place.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of type
  !! integer (i), real (s), double precision (d), complex (c), double complex
  !! (z) or logical (l). Its rank can vary from zero (scalar) up to the
  !! maximum rank.
  !!
  !! \see MPI documentation (\c MPI_ALLREDUCE)
  !!
  !!
  !! Example:
  !!
  !!     program test_allreduceip
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
  !!       call mpifx_allreduceip(mycomm, resvalr, MPI_PROD)
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
  !!           & "Obtained result (prod):", resvalr(:)
  !!       call mpifx_finalize()
  !!       
  !!     end program test_allreduceip
  !!
  interface mpifx_allreduceip
    module procedure mpifx_allreduceip_i0
    module procedure mpifx_allreduceip_i1
    module procedure mpifx_allreduceip_i2
    module procedure mpifx_allreduceip_i3
    module procedure mpifx_allreduceip_i4
    module procedure mpifx_allreduceip_i5
    module procedure mpifx_allreduceip_i6
    module procedure mpifx_allreduceip_s0
    module procedure mpifx_allreduceip_s1
    module procedure mpifx_allreduceip_s2
    module procedure mpifx_allreduceip_s3
    module procedure mpifx_allreduceip_s4
    module procedure mpifx_allreduceip_s5
    module procedure mpifx_allreduceip_s6
    module procedure mpifx_allreduceip_d0
    module procedure mpifx_allreduceip_d1
    module procedure mpifx_allreduceip_d2
    module procedure mpifx_allreduceip_d3
    module procedure mpifx_allreduceip_d4
    module procedure mpifx_allreduceip_d5
    module procedure mpifx_allreduceip_d6
    module procedure mpifx_allreduceip_c0
    module procedure mpifx_allreduceip_c1
    module procedure mpifx_allreduceip_c2
    module procedure mpifx_allreduceip_c3
    module procedure mpifx_allreduceip_c4
    module procedure mpifx_allreduceip_c5
    module procedure mpifx_allreduceip_c6
    module procedure mpifx_allreduceip_z0
    module procedure mpifx_allreduceip_z1
    module procedure mpifx_allreduceip_z2
    module procedure mpifx_allreduceip_z3
    module procedure mpifx_allreduceip_z4
    module procedure mpifx_allreduceip_z5
    module procedure mpifx_allreduceip_z6
    module procedure mpifx_allreduceip_l0
    module procedure mpifx_allreduceip_l1
    module procedure mpifx_allreduceip_l2
    module procedure mpifx_allreduceip_l3
    module procedure mpifx_allreduceip_l4
    module procedure mpifx_allreduceip_l5
    module procedure mpifx_allreduceip_l6
  end interface mpifx_allreduceip

contains









  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_i0(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    integer, intent(in) :: orig

    !> Contains result on exit.
    integer, intent(inout) :: reduced

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0



    call mpi_allreduce(orig, reduced, 1, MPI_INTEGER, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_i0', error)

  end subroutine mpifx_allreduce_i0



  !> Reduces operand on all processes (type i0).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_i0(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    integer, intent(inout) :: origreduced

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, 1, MPI_INTEGER, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_i0", error)

  end subroutine mpifx_allreduceip_i0






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_i1(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    integer, intent(in) :: orig(:)

    !> Contains result on exit.
    integer, intent(inout) :: reduced(:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_INTEGER, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_i1', error)

  end subroutine mpifx_allreduce_i1



  !> Reduces operand on all processes (type i1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_i1(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    integer, intent(inout) :: origreduced(:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_INTEGER, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_i1", error)

  end subroutine mpifx_allreduceip_i1






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_i2(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    integer, intent(in) :: orig(:,:)

    !> Contains result on exit.
    integer, intent(inout) :: reduced(:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_INTEGER, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_i2', error)

  end subroutine mpifx_allreduce_i2



  !> Reduces operand on all processes (type i2).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_i2(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    integer, intent(inout) :: origreduced(:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_INTEGER, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_i2", error)

  end subroutine mpifx_allreduceip_i2






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_i3(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    integer, intent(in) :: orig(:,:,:)

    !> Contains result on exit.
    integer, intent(inout) :: reduced(:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_INTEGER, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_i3', error)

  end subroutine mpifx_allreduce_i3



  !> Reduces operand on all processes (type i3).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_i3(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    integer, intent(inout) :: origreduced(:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_INTEGER, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_i3", error)

  end subroutine mpifx_allreduceip_i3






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_i4(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    integer, intent(in) :: orig(:,:,:,:)

    !> Contains result on exit.
    integer, intent(inout) :: reduced(:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_INTEGER, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_i4', error)

  end subroutine mpifx_allreduce_i4



  !> Reduces operand on all processes (type i4).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_i4(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    integer, intent(inout) :: origreduced(:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_INTEGER, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_i4", error)

  end subroutine mpifx_allreduceip_i4






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_i5(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    integer, intent(in) :: orig(:,:,:,:,:)

    !> Contains result on exit.
    integer, intent(inout) :: reduced(:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_INTEGER, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_i5', error)

  end subroutine mpifx_allreduce_i5



  !> Reduces operand on all processes (type i5).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_i5(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    integer, intent(inout) :: origreduced(:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_INTEGER, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_i5", error)

  end subroutine mpifx_allreduceip_i5






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_i6(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    integer, intent(in) :: orig(:,:,:,:,:,:)

    !> Contains result on exit.
    integer, intent(inout) :: reduced(:,:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_INTEGER, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_i6', error)

  end subroutine mpifx_allreduce_i6



  !> Reduces operand on all processes (type i6).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_i6(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    integer, intent(inout) :: origreduced(:,:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_INTEGER, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_i6", error)

  end subroutine mpifx_allreduceip_i6






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_s0(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(sp), intent(in) :: orig

    !> Contains result on exit.
    real(sp), intent(inout) :: reduced

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0



    call mpi_allreduce(orig, reduced, 1, MPI_REAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_s0', error)

  end subroutine mpifx_allreduce_s0



  !> Reduces operand on all processes (type s0).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_s0(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(sp), intent(inout) :: origreduced

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, 1, MPI_REAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_s0", error)

  end subroutine mpifx_allreduceip_s0






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_s1(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(sp), intent(in) :: orig(:)

    !> Contains result on exit.
    real(sp), intent(inout) :: reduced(:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_REAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_s1', error)

  end subroutine mpifx_allreduce_s1



  !> Reduces operand on all processes (type s1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_s1(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(sp), intent(inout) :: origreduced(:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_REAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_s1", error)

  end subroutine mpifx_allreduceip_s1






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_s2(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(sp), intent(in) :: orig(:,:)

    !> Contains result on exit.
    real(sp), intent(inout) :: reduced(:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_REAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_s2', error)

  end subroutine mpifx_allreduce_s2



  !> Reduces operand on all processes (type s2).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_s2(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(sp), intent(inout) :: origreduced(:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_REAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_s2", error)

  end subroutine mpifx_allreduceip_s2






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_s3(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(sp), intent(in) :: orig(:,:,:)

    !> Contains result on exit.
    real(sp), intent(inout) :: reduced(:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_REAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_s3', error)

  end subroutine mpifx_allreduce_s3



  !> Reduces operand on all processes (type s3).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_s3(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(sp), intent(inout) :: origreduced(:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_REAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_s3", error)

  end subroutine mpifx_allreduceip_s3






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_s4(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(sp), intent(in) :: orig(:,:,:,:)

    !> Contains result on exit.
    real(sp), intent(inout) :: reduced(:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_REAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_s4', error)

  end subroutine mpifx_allreduce_s4



  !> Reduces operand on all processes (type s4).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_s4(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(sp), intent(inout) :: origreduced(:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_REAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_s4", error)

  end subroutine mpifx_allreduceip_s4






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_s5(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(sp), intent(in) :: orig(:,:,:,:,:)

    !> Contains result on exit.
    real(sp), intent(inout) :: reduced(:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_REAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_s5', error)

  end subroutine mpifx_allreduce_s5



  !> Reduces operand on all processes (type s5).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_s5(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(sp), intent(inout) :: origreduced(:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_REAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_s5", error)

  end subroutine mpifx_allreduceip_s5






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_s6(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(sp), intent(in) :: orig(:,:,:,:,:,:)

    !> Contains result on exit.
    real(sp), intent(inout) :: reduced(:,:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_REAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_s6', error)

  end subroutine mpifx_allreduce_s6



  !> Reduces operand on all processes (type s6).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_s6(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(sp), intent(inout) :: origreduced(:,:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_REAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_s6", error)

  end subroutine mpifx_allreduceip_s6






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_d0(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(dp), intent(in) :: orig

    !> Contains result on exit.
    real(dp), intent(inout) :: reduced

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0



    call mpi_allreduce(orig, reduced, 1, MPI_DOUBLE_PRECISION, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_d0', error)

  end subroutine mpifx_allreduce_d0



  !> Reduces operand on all processes (type d0).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_d0(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(dp), intent(inout) :: origreduced

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, 1, MPI_DOUBLE_PRECISION, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_d0", error)

  end subroutine mpifx_allreduceip_d0






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_d1(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(dp), intent(in) :: orig(:)

    !> Contains result on exit.
    real(dp), intent(inout) :: reduced(:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_d1', error)

  end subroutine mpifx_allreduce_d1



  !> Reduces operand on all processes (type d1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_d1(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(dp), intent(inout) :: origreduced(:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_PRECISION, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_d1", error)

  end subroutine mpifx_allreduceip_d1






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_d2(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(dp), intent(in) :: orig(:,:)

    !> Contains result on exit.
    real(dp), intent(inout) :: reduced(:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_d2', error)

  end subroutine mpifx_allreduce_d2



  !> Reduces operand on all processes (type d2).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_d2(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(dp), intent(inout) :: origreduced(:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_PRECISION, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_d2", error)

  end subroutine mpifx_allreduceip_d2






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_d3(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(dp), intent(in) :: orig(:,:,:)

    !> Contains result on exit.
    real(dp), intent(inout) :: reduced(:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_d3', error)

  end subroutine mpifx_allreduce_d3



  !> Reduces operand on all processes (type d3).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_d3(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(dp), intent(inout) :: origreduced(:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_PRECISION, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_d3", error)

  end subroutine mpifx_allreduceip_d3






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_d4(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(dp), intent(in) :: orig(:,:,:,:)

    !> Contains result on exit.
    real(dp), intent(inout) :: reduced(:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_d4', error)

  end subroutine mpifx_allreduce_d4



  !> Reduces operand on all processes (type d4).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_d4(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(dp), intent(inout) :: origreduced(:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_PRECISION, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_d4", error)

  end subroutine mpifx_allreduceip_d4






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_d5(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(dp), intent(in) :: orig(:,:,:,:,:)

    !> Contains result on exit.
    real(dp), intent(inout) :: reduced(:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_d5', error)

  end subroutine mpifx_allreduce_d5



  !> Reduces operand on all processes (type d5).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_d5(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(dp), intent(inout) :: origreduced(:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_PRECISION, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_d5", error)

  end subroutine mpifx_allreduceip_d5






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_d6(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    real(dp), intent(in) :: orig(:,:,:,:,:,:)

    !> Contains result on exit.
    real(dp), intent(inout) :: reduced(:,:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_PRECISION, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_d6', error)

  end subroutine mpifx_allreduce_d6



  !> Reduces operand on all processes (type d6).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_d6(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    real(dp), intent(inout) :: origreduced(:,:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_PRECISION, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_d6", error)

  end subroutine mpifx_allreduceip_d6






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_c0(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(sp), intent(in) :: orig

    !> Contains result on exit.
    complex(sp), intent(inout) :: reduced

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0



    call mpi_allreduce(orig, reduced, 1, MPI_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_c0', error)

  end subroutine mpifx_allreduce_c0



  !> Reduces operand on all processes (type c0).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_c0(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(sp), intent(inout) :: origreduced

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, 1, MPI_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_c0", error)

  end subroutine mpifx_allreduceip_c0






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_c1(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(sp), intent(in) :: orig(:)

    !> Contains result on exit.
    complex(sp), intent(inout) :: reduced(:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_c1', error)

  end subroutine mpifx_allreduce_c1



  !> Reduces operand on all processes (type c1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_c1(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(sp), intent(inout) :: origreduced(:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_c1", error)

  end subroutine mpifx_allreduceip_c1






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_c2(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(sp), intent(in) :: orig(:,:)

    !> Contains result on exit.
    complex(sp), intent(inout) :: reduced(:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_c2', error)

  end subroutine mpifx_allreduce_c2



  !> Reduces operand on all processes (type c2).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_c2(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(sp), intent(inout) :: origreduced(:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_c2", error)

  end subroutine mpifx_allreduceip_c2






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_c3(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(sp), intent(in) :: orig(:,:,:)

    !> Contains result on exit.
    complex(sp), intent(inout) :: reduced(:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_c3', error)

  end subroutine mpifx_allreduce_c3



  !> Reduces operand on all processes (type c3).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_c3(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(sp), intent(inout) :: origreduced(:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_c3", error)

  end subroutine mpifx_allreduceip_c3






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_c4(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(sp), intent(in) :: orig(:,:,:,:)

    !> Contains result on exit.
    complex(sp), intent(inout) :: reduced(:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_c4', error)

  end subroutine mpifx_allreduce_c4



  !> Reduces operand on all processes (type c4).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_c4(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(sp), intent(inout) :: origreduced(:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_c4", error)

  end subroutine mpifx_allreduceip_c4






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_c5(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(sp), intent(in) :: orig(:,:,:,:,:)

    !> Contains result on exit.
    complex(sp), intent(inout) :: reduced(:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_c5', error)

  end subroutine mpifx_allreduce_c5



  !> Reduces operand on all processes (type c5).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_c5(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(sp), intent(inout) :: origreduced(:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_c5", error)

  end subroutine mpifx_allreduceip_c5






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_c6(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(sp), intent(in) :: orig(:,:,:,:,:,:)

    !> Contains result on exit.
    complex(sp), intent(inout) :: reduced(:,:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_c6', error)

  end subroutine mpifx_allreduce_c6



  !> Reduces operand on all processes (type c6).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_c6(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(sp), intent(inout) :: origreduced(:,:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_c6", error)

  end subroutine mpifx_allreduceip_c6






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_z0(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(dp), intent(in) :: orig

    !> Contains result on exit.
    complex(dp), intent(inout) :: reduced

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0



    call mpi_allreduce(orig, reduced, 1, MPI_DOUBLE_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_z0', error)

  end subroutine mpifx_allreduce_z0



  !> Reduces operand on all processes (type z0).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_z0(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(dp), intent(inout) :: origreduced

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, 1, MPI_DOUBLE_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_z0", error)

  end subroutine mpifx_allreduceip_z0






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_z1(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(dp), intent(in) :: orig(:)

    !> Contains result on exit.
    complex(dp), intent(inout) :: reduced(:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_z1', error)

  end subroutine mpifx_allreduce_z1



  !> Reduces operand on all processes (type z1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_z1(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(dp), intent(inout) :: origreduced(:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_z1", error)

  end subroutine mpifx_allreduceip_z1






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_z2(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(dp), intent(in) :: orig(:,:)

    !> Contains result on exit.
    complex(dp), intent(inout) :: reduced(:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_z2', error)

  end subroutine mpifx_allreduce_z2



  !> Reduces operand on all processes (type z2).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_z2(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(dp), intent(inout) :: origreduced(:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_z2", error)

  end subroutine mpifx_allreduceip_z2






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_z3(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(dp), intent(in) :: orig(:,:,:)

    !> Contains result on exit.
    complex(dp), intent(inout) :: reduced(:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_z3', error)

  end subroutine mpifx_allreduce_z3



  !> Reduces operand on all processes (type z3).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_z3(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(dp), intent(inout) :: origreduced(:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_z3", error)

  end subroutine mpifx_allreduceip_z3






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_z4(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(dp), intent(in) :: orig(:,:,:,:)

    !> Contains result on exit.
    complex(dp), intent(inout) :: reduced(:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_z4', error)

  end subroutine mpifx_allreduce_z4



  !> Reduces operand on all processes (type z4).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_z4(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(dp), intent(inout) :: origreduced(:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_z4", error)

  end subroutine mpifx_allreduceip_z4






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_z5(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(dp), intent(in) :: orig(:,:,:,:,:)

    !> Contains result on exit.
    complex(dp), intent(inout) :: reduced(:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_z5', error)

  end subroutine mpifx_allreduce_z5



  !> Reduces operand on all processes (type z5).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_z5(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(dp), intent(inout) :: origreduced(:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_z5", error)

  end subroutine mpifx_allreduceip_z5






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_z6(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    complex(dp), intent(in) :: orig(:,:,:,:,:,:)

    !> Contains result on exit.
    complex(dp), intent(inout) :: reduced(:,:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_z6', error)

  end subroutine mpifx_allreduce_z6



  !> Reduces operand on all processes (type z6).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_z6(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    complex(dp), intent(inout) :: origreduced(:,:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_DOUBLE_COMPLEX, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_z6", error)

  end subroutine mpifx_allreduceip_z6






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_l0(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    logical, intent(in) :: orig

    !> Contains result on exit.
    logical, intent(inout) :: reduced

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0



    call mpi_allreduce(orig, reduced, 1, MPI_LOGICAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_l0', error)

  end subroutine mpifx_allreduce_l0



  !> Reduces operand on all processes (type l0).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_l0(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    logical, intent(inout) :: origreduced

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, 1, MPI_LOGICAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_l0", error)

  end subroutine mpifx_allreduceip_l0






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_l1(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    logical, intent(in) :: orig(:)

    !> Contains result on exit.
    logical, intent(inout) :: reduced(:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_LOGICAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_l1', error)

  end subroutine mpifx_allreduce_l1



  !> Reduces operand on all processes (type l1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_l1(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    logical, intent(inout) :: origreduced(:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_LOGICAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_l1", error)

  end subroutine mpifx_allreduceip_l1






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_l2(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    logical, intent(in) :: orig(:,:)

    !> Contains result on exit.
    logical, intent(inout) :: reduced(:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_LOGICAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_l2', error)

  end subroutine mpifx_allreduce_l2



  !> Reduces operand on all processes (type l2).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_l2(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    logical, intent(inout) :: origreduced(:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_LOGICAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_l2", error)

  end subroutine mpifx_allreduceip_l2






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_l3(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    logical, intent(in) :: orig(:,:,:)

    !> Contains result on exit.
    logical, intent(inout) :: reduced(:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_LOGICAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_l3', error)

  end subroutine mpifx_allreduce_l3



  !> Reduces operand on all processes (type l3).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_l3(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    logical, intent(inout) :: origreduced(:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_LOGICAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_l3", error)

  end subroutine mpifx_allreduceip_l3






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_l4(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    logical, intent(in) :: orig(:,:,:,:)

    !> Contains result on exit.
    logical, intent(inout) :: reduced(:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_LOGICAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_l4', error)

  end subroutine mpifx_allreduce_l4



  !> Reduces operand on all processes (type l4).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_l4(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    logical, intent(inout) :: origreduced(:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_LOGICAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_l4", error)

  end subroutine mpifx_allreduceip_l4






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_l5(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    logical, intent(in) :: orig(:,:,:,:,:)

    !> Contains result on exit.
    logical, intent(inout) :: reduced(:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_LOGICAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_l5', error)

  end subroutine mpifx_allreduce_l5



  !> Reduces operand on all processes (type l5).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_l5(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    logical, intent(inout) :: origreduced(:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_LOGICAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_l5", error)

  end subroutine mpifx_allreduceip_l5






  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_l6(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    logical, intent(in) :: orig(:,:,:,:,:,:)

    !> Contains result on exit.
    logical, intent(inout) :: reduced(:,:,:,:,:,:)

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0




    call mpi_allreduce(orig, reduced, size(orig), MPI_LOGICAL, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_l6', error)

  end subroutine mpifx_allreduce_l6



  !> Reduces operand on all processes (type l6).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_l6(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    logical, intent(inout) :: origreduced(:,:,:,:,:,:)

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0


    call mpi_allreduce(MPI_IN_PLACE, origreduced, size(origreduced), MPI_LOGICAL, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_l6", error)

  end subroutine mpifx_allreduceip_l6



end module mpifx_allreduce_module
