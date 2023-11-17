!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!



!> Contains routines helpful for mpi-parallelisation.
module dftbp_common_schedule
  use dftbp_common_accuracy, only : dp
  use dftbp_common_environment, only : TEnvironment
  use dftbp_extlibs_mpifx, only : MPI_SUM, mpifx_allreduceip
  implicit none

  private
  public :: distributeRangeInChunks, distributeRangeInChunks2, distributeRangeWithWorkload
  public :: assembleChunks, getChunkRanges, getIndicesWithWorkload

  interface assembleChunks
    module procedure assembleR1Chunks
  end interface assembleChunks
  interface assembleChunks
    module procedure assembleR2Chunks
  end interface assembleChunks
  interface assembleChunks
    module procedure assembleR3Chunks
  end interface assembleChunks
  interface assembleChunks
    module procedure assembleR4Chunks
  end interface assembleChunks
  interface assembleChunks
    module procedure assembleC1Chunks
  end interface assembleChunks
  interface assembleChunks
    module procedure assembleC2Chunks
  end interface assembleChunks
  interface assembleChunks
    module procedure assembleC3Chunks
  end interface assembleChunks
  interface assembleChunks
    module procedure assembleC4Chunks
  end interface assembleChunks
  interface assembleChunks
    module procedure assembleI1Chunks
  end interface assembleChunks

contains


  !> Distributes a range in chunks over processes within a process group.
  subroutine distributeRangeInChunks(env, globalFirst, globalLast, localFirst, localLast)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> First element of the range
    integer, intent(in) :: globalFirst

    !> Last element of the range
    integer, intent(in) :: globalLast

    !> First element to process locally
    integer, intent(out) :: localFirst

    !> Last element to process locally
    integer, intent(out) :: localLast

    call getChunkRanges(env%mpi%groupComm%size, env%mpi%groupComm%rank, globalFirst, globalLast,&
        & localFirst, localLast)

  end subroutine distributeRangeInChunks


  !> Distributes a ranges in a double loop in chunks over processes within a process group.
  !>
  !> It will chop the loop with the wider range into chunks and leave the other intact.
  !>
  subroutine distributeRangeInChunks2(env, globalFirst1, globalLast1, globalFirst2, globalLast2,&
      & localFirst1, localLast1, localFirst2, localLast2)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> First element of the range for the outer loop
    integer, intent(in) :: globalFirst1

    !> Last element of the range for the outer loop
    integer, intent(in) :: globalLast1

    !> First element of the range for the inner loop
    integer, intent(in) :: globalFirst2

    !> Last element of the range for the inner loop
    integer, intent(in) :: globalLast2

    !> First element to process locally
    integer, intent(out) :: localFirst1

    !> Last element to process locally
    integer, intent(out) :: localLast1

    !> First element to process locally
    integer, intent(out) :: localFirst2

    !> Last element to process locally
    integer, intent(out) :: localLast2

    if (globalLast1 - globalFirst1 >= globalLast2 - globalFirst2) then
      call getChunkRanges(env%mpi%groupComm%size, env%mpi%groupComm%rank, globalFirst1,&
          & globalLast1, localFirst1, localLast1)
      localFirst2 = globalFirst2
      localLast2 = globalLast2
    else
      localFirst1 = globalFirst1
      localLast1 = globalLast1
      call getChunkRanges(env%mpi%groupComm%size, env%mpi%groupComm%rank, globalFirst2,&
          & globalLast2, localFirst2, localLast2)
    end if

  end subroutine distributeRangeInChunks2


  !> Distributes a range among processes within a process group
  !> and take into account that each item may have a different workload
  subroutine distributeRangeWithWorkload(env, globalFirst, globalLast, workload, indices)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> First element of the range
    integer, intent(in) :: globalFirst

    !> Last element of the range
    integer, intent(in) :: globalLast

    !> Number of elements each item has to process
    integer, intent(in) :: workload(:)

    !> Index array to be iterated over
    integer, allocatable, intent(out) :: indices(:)

    integer :: ii

    call getIndicesWithWorkload(env%mpi%groupComm%size, env%mpi%groupComm%rank, globalFirst,&
        & globalLast, workload, indices)

  end subroutine distributeRangeWithWorkload



  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleR1Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    real(dp), intent(inout) :: chunks(:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleR1Chunks


  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleR2Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    real(dp), intent(inout) :: chunks(:,:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleR2Chunks


  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleR3Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    real(dp), intent(inout) :: chunks(:,:,:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleR3Chunks


  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleR4Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    real(dp), intent(inout) :: chunks(:,:,:,:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleR4Chunks


  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleC1Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    complex(dp), intent(inout) :: chunks(:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleC1Chunks


  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleC2Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    complex(dp), intent(inout) :: chunks(:,:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleC2Chunks


  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleC3Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    complex(dp), intent(inout) :: chunks(:,:,:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleC3Chunks


  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleC4Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    complex(dp), intent(inout) :: chunks(:,:,:,:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleC4Chunks


  !> Assembles the chunks by summing up contributions within a process group.
  subroutine assembleI1Chunks(env,chunks)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> array to assemble
    integer, intent(inout) :: chunks(:)

    call mpifx_allreduceip(env%mpi%groupComm, chunks, MPI_SUM)

  end subroutine assembleI1Chunks



  !> Calculate the chunk ranges for a given MPI-communicator.
  subroutine getChunkRanges(groupSize, myRank, globalFirst, globalLast, localFirst, localLast)

    !> Size of the group over which the chunks should be distributed
    integer, intent(in) :: groupSize

    !> Rank of the current process
    integer, intent(in) :: myRank

    !> First element of the range
    integer, intent(in) :: globalFirst

    !> Last element of the range
    integer, intent(in) :: globalLast

    !> First element to process locally
    integer, intent(out) :: localFirst

    !> Last element to process locally
    integer, intent(out) :: localLast

    integer :: rangeLength, nLocal, remainder

    rangeLength = globalLast - globalFirst + 1
    nLocal = rangeLength / groupSize
    remainder = mod(rangeLength, groupSize)
    if (myRank < remainder) then
      nLocal = nLocal + 1
      localFirst = globalFirst + myRank * nLocal
    else
      localFirst = globalFirst + remainder * (nLocal + 1) + (myRank - remainder) * nLocal
    end if
    localLast = min(localFirst + nLocal - 1, globalLast)

  end subroutine getChunkRanges


  !> Calculate the indices for a given MPI-communicator considerung different workload
  subroutine getIndicesWithWorkload(groupSize, myRank, globalFirst, globalLast, workload, indices)

    !> Size of the group over which the chunks should be distributed
    integer, intent(in) :: groupSize

    !> Rank of the current process
    integer, intent(in) :: myRank

    !> First element of the range
    integer, intent(in) :: globalFirst

    !> Last element of the range
    integer, intent(in) :: globalLast

    !> Workload for each item
    integer, intent(in) :: workload(:)

    !> Index array to be iterated over
    integer, allocatable, intent(out) :: indices(:)

    integer :: numIndices, rank, ii
    integer, allocatable :: rankWorkload(:), indices_(:)

    allocate(indices_(globalLast - globalFirst + 1))
    allocate(rankWorkload(groupSize))

    rankWorkload(:) = 0
    indices_(:) = 0
    numIndices = 0

    do ii = globalFirst, globalLast
      rank = minloc(rankWorkload, dim=1)
      rankWorkload(rank) = rankWorkload(rank) + max(1, workload(ii))
      if (rank == myRank + 1) then
        numIndices = numIndices + 1
        indices_(numIndices) = ii
      end if
    end do

    allocate(indices(numIndices))
    indices(1:numIndices) = indices_(1:numIndices)

  end subroutine getIndicesWithWorkload


end module dftbp_common_schedule
