!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Module containing routines to make linear combinations of orbitals for degenerate perturbation
!> from a hermitian/symmetric matrix
module dftbp_derivs_rotatedegen
  use dftbp_common_accuracy, only : dp
  use dftbp_common_status, only : TStatus
  use dftbp_math_eigensolver, only : heev
  use dftbp_math_qm, only : makeSimilarityTrans
  use dftbp_type_wrappedintr, only : TwrappedReal2, TwrappedCmplx2
  use dftbp_common_environment, only : TEnvironment
  use dftbp_type_densedescr, only: TDenseDescr
  use linecomm_module, only : linecomm
  implicit none

  private
  public :: TRotateDegen, TRotateDegen_init


  !> Data type for resolving degenerate orbitals in perturbation expressions
  type :: TRotateDegen
    private

    !> Dense unitary matrix to transform orbitals
    complex(dp), allocatable :: CmplxU(:,:)

    !> Individual sub-blocks of unitary matrices to transform orbitals, if much smaller than U
    type(TwrappedCmplx2), allocatable :: CmplxUBlock(:)


    !> Dense unitary matrix to transform orbitals
    real(dp), allocatable :: RealU(:,:)

    !> Individual sub-blocks of unitary matrices to transform orbitals, if much smaller than U
    type(TwrappedReal2), allocatable :: RealUBlock(:)


    !> Block ranges as needed in UBlock, deallocated again by time of return if dense U case
    integer, allocatable :: blockRange(:,:)

    !> To which group of states any particular one belongs
    integer, allocatable :: degenerateGroup(:)

    !> numerical tolerance for deciding degeneracy
    real(dp) :: tolerance

    !> Sub-range of states if needed
    integer :: eiRange(2)

    !> Denominator for fraction of states in a degenerate group before the using dense transforms
    integer :: minDegenerateFraction

    !> Minimum number of states in a degenerate group before the using dense transforms
    integer :: minDegenerateStates

    !> Should the order of states (derivatives) be reversed compared to the eigensolver return
    logical :: tReverseOrder = .false.

    ! redundant variables (could test from allocation status and size), but makes code simpler to
    ! read in a few places:

    !> Number of groups of degenerate orbitals
    integer :: nGrp = -1

    !> Stores data structure for a real unitary transform
    logical :: tReal = .false.

    !> Stores data structure for a complex unitary transform
    logical :: tComplx = .false.

    !> Is any storage allocated
    logical :: tAllocateStorage = .false.

    !> Order of matrix
    integer :: nOrb = -1

    logical :: tFullUMatrix = .false.

  contains

    !> Generate unitary matrix for degenerate perturbation
    procedure :: generateCmplxUnitary
    procedure :: generateRealUnitary
    generic :: generateUnitary => generateCmplxUnitary, generateRealUnitary


    !> Are a pair of states in the same degenerate group
    procedure :: degenerate

    !> Release memory and cleans up
    procedure :: destroy

  end type TRotateDegen



contains

  !> Initialises instance and set some optional parameters
  subroutine TRotateDegen_init(self, tolerance, smallestBlock, smallestFraction, eiRange)

    !> Instance
    type(TRotateDegen), intent(inout) :: self

    !> Tolerance for degeneracy testing
    real(dp), intent(in), optional :: tolerance

    !> Smallest number of degenerate orbitals at which the dense algorithm should be used
    integer, intent(in), optional :: smallestBlock

    !> Smallest fraction of the matrix at which the dense algorithm should be used
    integer, intent(in), optional :: smallestFraction

    !> Sub-range of states if needed, for example for metallic finite temperature in parallel gauge
    integer, intent(in), optional :: eiRange(2)

    if (present(smallestBlock)) then
      self%minDegenerateStates = smallestBlock
    else
      self%minDegenerateStates = 500
    end if

    if (present(smallestFraction)) then
      self%minDegenerateFraction = smallestFraction
    else
      self%minDegenerateFraction = 4 ! a quarter of the matrix
    end if

    ! Tolerance for degeneracy detection
    self%tolerance = tolerance

    if (present(eiRange)) then
      self%eiRange(:) = eiRange
    else
      self%eiRange(:) = -1
    end if

  end subroutine TRotateDegen_init




  !> Set up unitary transformation of matrix for degenerate states, producing combinations that are
  !> orthogonal under the action of the matrix. This is the complex case.
  subroutine generateCmplxUnitary(self, env, matrixToProcess, ei, eigVecs, denseDesc,&
      & tTransformed, errStatus)

    !> Instance
    class(TRotateDegen), intent(inout) :: self

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Matrix elements between (potentially degenerate) orbitals
    complex(dp), intent(in) :: matrixToProcess(:,:)

    !> Eigenvalues of local block of degenerate matrix
    real(dp), intent(in) :: ei(:)

    !> Eigenvectors for rotation
    complex(dp), intent(inout) :: eigVecs(:,:)

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Are and vectors from degenerate eigenvalues, so transformed
    logical, intent(out) :: tTransformed

    !> Status of routine
    type(TStatus), intent(out) :: errStatus

    integer :: eiRange(2), maxRange, nInBlock, iGrp, iEnd, iStart, iGet
    type(linecomm) :: communicator
    complex(dp), allocatable :: localMatrix(:,:), localMatrixCols(:,:)
    real(dp), allocatable :: eigenvals(:)

    real(dp), parameter :: one = (1.0_dp, 0.0_dp)
    real(dp), parameter :: zero = (0.0_dp, 0.0_dp)

    self%nOrb = size(ei)
    if (all(self%eiRange == [-1,-1] )) then
      eiRange(:) = [1, self%nOrb]
    else
      eiRange(:) = self%eiRange
    end if

    if (allocated(self%blockRange)) then
      if (any(shape(self%blockRange) /= [2, self%nOrb])) then
        deallocate(self%blockRange)
      end if
    end if
    if (.not.allocated(self%blockRange)) then
      allocate(self%blockRange(2, self%nOrb))
    end if
    self%blockRange(:,:) = 0
    if (allocated(self%degenerateGroup)) then
      if (size(self%degenerateGroup) /= self%nOrb) then
        deallocate(self%degenerateGroup)
      end if
    end if
    if (.not.allocated(self%degenerateGroup)) then
      allocate(self%degenerateGroup(self%nOrb))
    end if

    call degeneracyRanges(self%blockRange, self%nGrp, Ei, errStatus, self%tolerance, eiRange,&
        & self%degenerateGroup)
  if (errStatus%hasError()) then
    return
  end if

    maxRange = maxval(self%blockRange(2,:self%nGrp) - self%blockRange(1,:self%nGrp)) + 1

    if (maxRange > self%minDegenerateStates) then
  call errStatus%setError(-1, "Degenerate group exceeds reasonable size for one node to process",&
      & "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/derivs/rotatedegen.F90", 242)
  return
      ! should add a dense case to cope with this -- blank out non-degenerate elements, diagonalise
      ! whole matrix and then use pgemm with eigenvectors
    end if

    allocate(eigenvals(maxRange))

    if (maxRange == 1) then
      tTransformed = .false.
      return
    end if
    tTransformed = .true.

    call communicator%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")

    allocate(localMatrixCols(self%nOrb, maxRange))
    allocate(localMatrix(maxRange, maxRange))
    localMatrixCols(:,:) = 0.0_dp
    localMatrix(:,:) = 0.0_dp

    do iGrp = 1, self%nGrp
      iStart = self%blockRange(1,iGrp)
      iEnd = self%blockRange(2,iGrp)
      nInBlock = iEnd - iStart + 1
      if (nInBlock == 1) then
        cycle
      end if
      localMatrix(:,:) = 0.0_dp
      do iGet = iStart, iEnd

        if (env%mpi%tGroupLead) then

          call communicator%getline_lead(env%blacs%orbitalGrid, iGet, matrixToProcess,&
              & localMatrixCols(:,iGet-iStart+1))

          localMatrix(:iEnd-iStart+1,iGet-iStart+1) = localMatrixCols(iStart:iEnd,iGet-iStart+1)

          ! now get eigenvectors into this structure
          call communicator%getline_lead(env%blacs%orbitalGrid, iGet, eigvecs,&
              & localMatrixCols(:,iGet-iStart+1))

        else

          ! send matrix rows
          call communicator%getline_follow(env%blacs%orbitalGrid, iGet, matrixToProcess)

          ! send eigenvectors
          call communicator%getline_follow(env%blacs%orbitalGrid, iGet, eigvecs)

        end if

      end do

      if (env%mpi%tGroupLead) then
        call heev(localMatrix(:nInBlock, :nInBlock), eigenvals(:nInBlock), 'L', 'V')

        if (self%tReverseOrder) then
          localMatrix(:nInBlock, :nInBlock) = localMatrix(:nInBlock, nInBlock:1:-1)
        end if

        localMatrixCols(:,:nInBlock) = matmul(localMatrixCols(:,:nInBlock),&
            & localMatrix(:nInBlock, :nInBlock))
      end if



      do iGet = iStart, iEnd

        if (env%mpi%tGroupLead) then

          ! now send transformed eigenvectors back
          call communicator%setline_lead(env%blacs%orbitalGrid, iGet,&
              & localMatrixCols(:,iGet-iStart+1), eigvecs)

        else

          ! set relevant eigenvectors to the transformed ones
          call communicator%setline_follow(env%blacs%orbitalGrid, iGet, eigvecs)

        end if

      end do

    end do

  end subroutine generateCmplxUnitary


  !> Set up unitary transformation of matrix for degenerate states, producing combinations that are
  !> orthogonal under the action of the matrix. This is the real case.
  subroutine generateRealUnitary(self, env, matrixToProcess, ei, eigVecs, denseDesc,&
      & tTransformed, errStatus)

    !> Instance
    class(TRotateDegen), intent(inout) :: self

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Matrix elements between (potentially degenerate) orbitals
    real(dp), intent(in) :: matrixToProcess(:,:)

    !> Eigenvalues of local block of degenerate matrix
    real(dp), intent(in) :: ei(:)

    !> Eigenvectors for rotation
    real(dp), intent(inout) :: eigVecs(:,:)

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Are and vectors from degenerate eigenvalues, so transformed
    logical, intent(out) :: tTransformed

    !> Status of routine
    type(TStatus), intent(out) :: errStatus

    integer :: eiRange(2), maxRange, nInBlock, iGrp, iEnd, iStart, iGet
    type(linecomm) :: communicator
    real(dp), allocatable :: localMatrix(:,:), localMatrixCols(:,:)
    real(dp), allocatable :: eigenvals(:)

    real(dp), parameter :: one = 1.0_dp
    real(dp), parameter :: zero = 0.0_dp

    self%nOrb = size(ei)
    if (all(self%eiRange == [-1,-1] )) then
      eiRange(:) = [1, self%nOrb]
    else
      eiRange(:) = self%eiRange
    end if

    if (allocated(self%blockRange)) then
      if (any(shape(self%blockRange) /= [2, self%nOrb])) then
        deallocate(self%blockRange)
      end if
    end if
    if (.not.allocated(self%blockRange)) then
      allocate(self%blockRange(2, self%nOrb))
    end if
    self%blockRange(:,:) = 0
    if (allocated(self%degenerateGroup)) then
      if (size(self%degenerateGroup) /= self%nOrb) then
        deallocate(self%degenerateGroup)
      end if
    end if
    if (.not.allocated(self%degenerateGroup)) then
      allocate(self%degenerateGroup(self%nOrb))
    end if

    call degeneracyRanges(self%blockRange, self%nGrp, Ei, errStatus, self%tolerance, eiRange,&
        & self%degenerateGroup)
  if (errStatus%hasError()) then
    return
  end if

    maxRange = maxval(self%blockRange(2,:self%nGrp) - self%blockRange(1,:self%nGrp)) + 1

    if (maxRange > self%minDegenerateStates) then
  call errStatus%setError(-1, "Degenerate group exceeds reasonable size for one node to process",&
      & "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/derivs/rotatedegen.F90", 242)
  return
      ! should add a dense case to cope with this -- blank out non-degenerate elements, diagonalise
      ! whole matrix and then use pgemm with eigenvectors
    end if

    allocate(eigenvals(maxRange))

    if (maxRange == 1) then
      tTransformed = .false.
      return
    end if
    tTransformed = .true.

    call communicator%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")

    allocate(localMatrixCols(self%nOrb, maxRange))
    allocate(localMatrix(maxRange, maxRange))
    localMatrixCols(:,:) = 0.0_dp
    localMatrix(:,:) = 0.0_dp

    do iGrp = 1, self%nGrp
      iStart = self%blockRange(1,iGrp)
      iEnd = self%blockRange(2,iGrp)
      nInBlock = iEnd - iStart + 1
      if (nInBlock == 1) then
        cycle
      end if
      localMatrix(:,:) = 0.0_dp
      do iGet = iStart, iEnd

        if (env%mpi%tGroupLead) then

          call communicator%getline_lead(env%blacs%orbitalGrid, iGet, matrixToProcess,&
              & localMatrixCols(:,iGet-iStart+1))

          localMatrix(:iEnd-iStart+1,iGet-iStart+1) = localMatrixCols(iStart:iEnd,iGet-iStart+1)

          ! now get eigenvectors into this structure
          call communicator%getline_lead(env%blacs%orbitalGrid, iGet, eigvecs,&
              & localMatrixCols(:,iGet-iStart+1))

        else

          ! send matrix rows
          call communicator%getline_follow(env%blacs%orbitalGrid, iGet, matrixToProcess)

          ! send eigenvectors
          call communicator%getline_follow(env%blacs%orbitalGrid, iGet, eigvecs)

        end if

      end do

      if (env%mpi%tGroupLead) then
        call heev(localMatrix(:nInBlock, :nInBlock), eigenvals(:nInBlock), 'L', 'V')

        if (self%tReverseOrder) then
          localMatrix(:nInBlock, :nInBlock) = localMatrix(:nInBlock, nInBlock:1:-1)
        end if

        localMatrixCols(:,:nInBlock) = matmul(localMatrixCols(:,:nInBlock),&
            & localMatrix(:nInBlock, :nInBlock))
      end if



      do iGet = iStart, iEnd

        if (env%mpi%tGroupLead) then

          ! now send transformed eigenvectors back
          call communicator%setline_lead(env%blacs%orbitalGrid, iGet,&
              & localMatrixCols(:,iGet-iStart+1), eigvecs)

        else

          ! set relevant eigenvectors to the transformed ones
          call communicator%setline_follow(env%blacs%orbitalGrid, iGet, eigvecs)

        end if

      end do

    end do

  end subroutine generateRealUnitary



  !> Clean up structure
  subroutine destroy(self)

    !> Instance
    class(TRotateDegen), intent(inout) :: self

    integer :: iGrp

    if (allocated(self%blockRange)) then
      deallocate(self%blockRange)
    end if
    if (allocated(self%degenerateGroup)) then
      deallocate(self%degenerateGroup)
    end if

    if (self%tFullUMatrix) then

      if (allocated(self%CmplxU)) then
        deallocate(self%CmplxU)
      end if
      if (allocated(self%RealU)) then
        deallocate(self%RealU)
      end if

    else

      if (allocated(self%CmplxUBlock)) then
        do iGrp = 1, size(self%CmplxUBlock)
          deallocate(self%CmplxUBlock(iGrp)%data)
        end do
        deallocate(self%CmplxUBlock)
      end if
      if (allocated(self%RealUBlock)) then
        do iGrp = 1, size(self%RealUBlock)
          deallocate(self%RealUBlock(iGrp)%data)
        end do
        deallocate(self%RealUBlock)
      end if

    end if

  end subroutine destroy


  !> Returns whether states are in the same degenerate group
  pure function degenerate(self, ii, jj)

    !> Instance
    class(TRotateDegen), intent(in) :: self

    !> First state
    integer, intent(in) :: ii

    !> second state
    integer, intent(in) :: jj

    !> Resulting test
    logical :: degenerate

    degenerate = (self%degenerateGroup(ii) == self%degenerateGroup(jj))

  end function degenerate


  !> Find which groups of eigenvales are degenerate to within a tolerance
  !> Note, similar process is used in Casida excited state calculations, so should spin off as its
  !> own module at some point
  subroutine degeneracyRanges(blockRange, nGrp, Ei, errStatus, tol, eiRange, grpMembership)

    !> Index array for lower and upper states in degenerate group
    integer, intent(out) :: blockRange(:,:)

    !> Number of degenerate groups
    integer, intent(out) :: nGrp

    !> Eigenvalues for degeneracy testing
    real(dp), intent(in) :: Ei(:)

    !> Status of routine
    type(TStatus), intent(out) :: errStatus

    !> Tolerance for degeneracy testing
    real(dp), intent(in), optional :: tol

    !> sub range of eigenvalues to process
    integer, intent(in), optional :: eiRange(2)

    integer, intent(out), optional :: grpMembership(:)

    integer :: ii, jj, nOrb, iStart, iEnd
    real(dp) :: localTol

    if (present(tol)) then
      localTol = tol
    else
      ! a few times eps, just in case of minor symmetry breaking
      localTol = 128.0_dp * epsilon(0.0_dp)
    end if
    nOrb = size(ei)
    blockRange(:,:) = 0
    nGrp = 0

    if (present(eiRange)) then
      ! set states before group as not of interest
      iStart = eiRange(1)
      iEnd = eiRange(2)
      if (iStart == iEnd) then
  call errStatus%setError(-1, "Degeneracy range is itself degenerate, should not be here",&
      & "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/derivs/rotatedegen.F90", 705)
  return
      end if
      do ii = 1, iStart - 1
        blockRange(:, ii) = ii
      end do
    else
      iStart = 1
      iEnd = nOrb
    end if
    nGrp = nGrp + iStart - 1

    do ii = 1, nGrp
      grpMembership(ii) = ii
    end do

    ii = iStart
    do while (ii <= iEnd)
      nGrp = nGrp + 1
      blockRange(1, nGrp) = ii
      grpMembership(ii) = nGrp
      do jj = ii + 1, iEnd
        ! assumes sorted:
        if ( abs(ei(jj) - ei(jj-1)) > localTol) then
          exit
        end if
        grpMembership(jj) = nGrp
      end do
      ii = jj
      blockRange(2, nGrp) = jj - 1
    end do

    if (present(eiRange)) then
      ! set states after range as not of interest
      do ii = iEnd + 1, nOrb
        nGrp = nGrp + 1
        blockRange(:, nGrp) = ii
        grpMembership(ii) = nGrp
      end do
    end if

  end subroutine degeneracyRanges

end module dftbp_derivs_rotatedegen
