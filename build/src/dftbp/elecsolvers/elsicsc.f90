!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Contains routines for converting from and to ELSI CSC format.
module dftbp_elecsolvers_elsicsc
  use dftbp_common_accuracy, only : dp
  use dftbp_common_constants, only : pi
  use dftbp_common_environment, only : TEnvironment
  use dftbp_dftb_periodic, only : TNeighbourList
  use dftbp_io_message, only : error
  use dftbp_math_angmomentum, only : rotateZ
  use dftbp_type_commontypes, only : TOrbitals
  use dftbp_extlibs_mpifx, only : MPI_SUM, mpifx_allreduce
  implicit none

  private
  public :: TElsiCsc, TElsiCsc_init


  !> Data needed to convert between DFTB+ sparse format and ELSI CSC format.
  type :: TElsiCsc
    private

    !> number of non-zero matrix elements on this processor
    integer, public :: nnzLocal

    !>  number of non-zero matrix elements in the whole sparse matrix
    integer, public :: nnzGlobal

    !> Number of local columns on this processor
    integer, public :: numColLocal

    !> Local column pointer in the CSC format on this processor
    integer, public, allocatable :: colPtrLocal(:)

    !> Local row index for non-zero elements (note: initialized at first pack2elsi() call)
    integer, public, allocatable :: rowIndLocal(:)

    !> On which column does this processor start its matrix
    integer :: colStartLocal

    !> On which column does this processor end its matrix
    integer :: colEndLocal

    !> Index for starting row of blocks in nzValLocal
    integer, allocatable :: blockRow(:,:)

    !> List of atoms with elements in the columns held locally
    integer, allocatable :: atomsInColumns(:)

    !> Count of the atoms with elements in the columns held locally
    integer :: nAtomsInColumns

  contains

    procedure, private :: TElsiCsc_convertPackedToElsiReal
    procedure, private :: TElsiCsc_convertPackedHelicalToElsiReal
    procedure, private :: TElsiCsc_convertPackedToElsiCmplx
    procedure, private :: TElsiCsc_convertPackedHelicalToElsiCmplx
    procedure, private :: TElsiCsc_convertElsiToPackedReal
    procedure, private :: TElsiCsc_convertElsiToPackedHelicalReal
    procedure, private :: TElsiCsc_convertElsiToPackedCmplx
    procedure, private :: TElsiCsc_convertElsiToPackedHelicalCmplx
    generic :: convertPackedToElsiReal => TElsiCsc_convertPackedToElsiReal,&
        & TElsiCsc_convertPackedHelicalToElsiReal
    generic :: convertPackedToElsiCmplx => TElsiCsc_convertPackedToElsiCmplx,&
        & TElsiCsc_convertPackedHelicalToElsiCmplx
    generic :: convertElsiToPackedReal => TElsiCsc_convertElsiToPackedReal,&
        & TElsiCsc_convertElsiToPackedHelicalReal
    generic :: convertElsiToPackedCmplx => TElsiCsc_convertElsiToPackedCmplx,&
        & TElsiCsc_convertElsiToPackedHelicalCmplx

  end type TElsiCsc



contains

  !> Initializes the ELSI CSC converter.
  subroutine TElsiCsc_init(this, env, nBasis, csrBlockSize, neighList, nNeighbourSK, iAtomStart,&
      & iSparseStart, img2CentCell)

    !> Sparse conversion instance
    type(TElsiCsc), intent(out) :: this

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Number of basis functions
    integer, intent(in) :: nBasis

    !> Number of csr rows on a given process.
    integer, intent(in) :: csrBlockSize

    !> Neighbour list for the atoms (First index from 0!)
    type(TNeighbourList), intent(in) :: neighList

    !> Nr. of neighbours for the atoms.
    integer, intent(in) :: nNeighbourSK(:)

    !> Atom offset for the squared matrix
    integer, intent(in) :: iAtomStart(:)

    !> indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Mapping between image atoms and corresponding atom in the central cell.
    integer, intent(in) :: img2CentCell(:)


    call error("Internal error: TElsiCsc_init() called despite missing ELSI support")


  end subroutine TElsiCsc_init


  !> Convert sparse DFTB+ matrix to distributed CSC matrix format for ELSI calculations for real
  !> matrices
  !>
  !> NOTE: ELSI needs the full matrix (both triangles)
  !>
  subroutine TElsiCsc_convertPackedToElsiReal(this, orig, iNeighbour, nNeighbourSK,&
      & iAtomStart, iSparseStart, img2CentCell, nzValLocal, orb&
    &)

    !> Sparse conversion instance
    class(TElsiCsc), intent(inout) :: this

    !> Sparse Hamiltonian
    real(dp), intent(in) :: orig(:)

    !> Neighbour list for each atom (first index from 0!).
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom (incl. itthis).
    integer, intent(in) :: nNeighbourSK(:)

    !> Atom offset for the squared Hamiltonian
    integer, intent(in) :: iAtomStart(:)

    !> Indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Atomic mapping indexes.
    integer, intent(in) :: img2CentCell(:)

    !> Local non-zero elements
    real(dp), intent(out) :: nzValLocal(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb



    call error("Internal error: TElsiCsc_convertPackedToElsiReal() called despite missing ELSI&
        & support")


  end subroutine TElsiCsc_convertPackedToElsiReal


  !> Convert sparse DFTB+ matrix to distributed CSC matrix format for ELSI calculations for complex
  !> matrices
  !>
  !> NOTE: ELSI needs the full matrix (both triangles)
  !>
  subroutine TElsiCsc_convertPackedToElsiCmplx(this, orig, iNeighbour, nNeighbourSK,&
      & iAtomStart, iSparseStart, img2CentCell, kPoint, iCellVec, cellVec, nzValLocal, orb&
      &)

    !> Sparse conversion instance
    class(TElsiCsc), intent(inout) :: this

    !> Sparse Hamiltonian
    real(dp), intent(in) :: orig(:)

    !> Neighbour list for each atom (first index from 0!).
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom (incl. itthis).
    integer, intent(in) :: nNeighbourSK(:)

    !> Atom offset for the squared Hamiltonian
    integer, intent(in) :: iAtomStart(:)

    !> Indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Atomic mapping indexes.
    integer, intent(in) :: img2CentCell(:)

    !> Current k-point
    real(dp), intent(in) :: kPoint(:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Local non-zero elements
    complex(dp), intent(out) :: nzValLocal(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb



    call error("Internal error: TElsiCsc_convertPackedToElsiCmplx() called despite missing ELSI&
        & support")


  end subroutine TElsiCsc_convertPackedToElsiCmplx


  !> Convert CSC matrix format into DFTB+ sparse format
  !>
  !> Note: primitive will not be set to zero on startup, and values are added to enable addition of
  !> spin components. Make sure, you set it to zero before invoking this routine the first time.
  subroutine TElsiCsc_convertElsiToPackedReal(this, iNeighbour, nNeighbourSK, orb,&
      & iAtomStart, iSparseStart, img2CentCell, nzval, primitive)

    !> Sparse conversion instance
    class(TElsiCsc), intent(inout) :: this

    !> Neighbour list for each atom (first index from 0!).
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom (incl. itthis).
    integer, intent(in) :: nNeighbourSK(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb


    !> Atom offset for the squared Hamiltonian
    integer, intent(in) :: iAtomStart(:)

    !> Indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Atomic mapping indexes.
    integer, intent(in) :: img2CentCell(:)

    !> Local non-zero elements
    real(dp), intent(in) :: nzval(:)

    !> Sparse Hamiltonian
    real(dp), intent(inout) :: primitive(:)


    call error("Internal error: TElsiCsc_convertElsiToPackedReal() called despite missing ELSI&
        & support")


  end subroutine TElsiCsc_convertElsiToPackedReal


  !> Convert CSC matrix format into DFTB+ sparse format
  !>
  !> Note: primitive will not be set to zero on startup, and values are added to enable addition of
  !> spin components. Make sure, you set it to zero before invoking this routine the first time.
  subroutine TElsiCsc_convertElsiToPackedCmplx(this, iNeighbour, nNeighbourSK, orb,&
      & iAtomStart, iSparseStart, img2CentCell, kPoint, kWeight, iCellVec, cellVec, nzval,&
      & primitive)

    !> Sparse conversion instance
    class(TElsiCsc), intent(in) :: this

    !> Neighbour list for each atom (first index from 0!).
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom (incl. itthis).
    integer, intent(in) :: nNeighbourSK(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb


    !> Atom offset for the squared Hamiltonian
    integer, intent(in) :: iAtomStart(:)

    !> Indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Atomic mapping indexes.
    integer, intent(in) :: img2CentCell(:)

    !> Current k-point
    real(dp), intent(in) :: kPoint(:)

    !> Weight for current k-points
    real(dp), intent(in) :: kWeight

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Local non-zero elements
    complex(dp), intent(in) :: nzval(:)

    !> Sparse Hamiltonian
    real(dp), intent(inout) :: primitive(:)


    call error("Internal error: TElsiCsc_convertElsiToPackedCmplx() called despite missing ELSI&
        & support")


  end subroutine TElsiCsc_convertElsiToPackedCmplx


  !> Convert sparse DFTB+ matrix to distributed CSC matrix format for ELSI calculations for real
  !> matrices
  !>
  !> NOTE: ELSI needs the full matrix (both triangles)
  !>
  subroutine TElsiCsc_convertPackedHelicalToElsiReal(this, orig, iNeighbour, nNeighbourSK,&
      & iAtomStart, iSparseStart, img2CentCell, nzValLocal, orb&
      &, species, coord&
    &)

    !> Sparse conversion instance
    class(TElsiCsc), intent(inout) :: this

    !> Sparse Hamiltonian
    real(dp), intent(in) :: orig(:)

    !> Neighbour list for each atom (first index from 0!).
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom (incl. itthis).
    integer, intent(in) :: nNeighbourSK(:)

    !> Atom offset for the squared Hamiltonian
    integer, intent(in) :: iAtomStart(:)

    !> Indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Atomic mapping indexes.
    integer, intent(in) :: img2CentCell(:)

    !> Local non-zero elements
    real(dp), intent(out) :: nzValLocal(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb


    !> Species of each atom
    integer :: species(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coord(:,:)



    call error("Internal error: TElsiCsc_convertPackedToElsiReal() called despite missing ELSI&
        & support")


  end subroutine TElsiCsc_convertPackedHelicalToElsiReal


  !> Convert sparse DFTB+ matrix to distributed CSC matrix format for ELSI calculations for complex
  !> matrices
  !>
  !> NOTE: ELSI needs the full matrix (both triangles)
  !>
  subroutine TElsiCsc_convertPackedHelicalToElsiCmplx(this, orig, iNeighbour, nNeighbourSK,&
      & iAtomStart, iSparseStart, img2CentCell, kPoint, iCellVec, cellVec, nzValLocal, orb&
      &, species, coord&
      &)

    !> Sparse conversion instance
    class(TElsiCsc), intent(inout) :: this

    !> Sparse Hamiltonian
    real(dp), intent(in) :: orig(:)

    !> Neighbour list for each atom (first index from 0!).
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom (incl. itthis).
    integer, intent(in) :: nNeighbourSK(:)

    !> Atom offset for the squared Hamiltonian
    integer, intent(in) :: iAtomStart(:)

    !> Indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Atomic mapping indexes.
    integer, intent(in) :: img2CentCell(:)

    !> Current k-point
    real(dp), intent(in) :: kPoint(:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Local non-zero elements
    complex(dp), intent(out) :: nzValLocal(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb


    !> Species of each atom
    integer :: species(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coord(:,:)



    call error("Internal error: TElsiCsc_convertPackedToElsiCmplx() called despite missing ELSI&
        & support")


  end subroutine TElsiCsc_convertPackedHelicalToElsiCmplx


  !> Convert CSC matrix format into DFTB+ sparse format
  !>
  !> Note: primitive will not be set to zero on startup, and values are added to enable addition of
  !> spin components. Make sure, you set it to zero before invoking this routine the first time.
  subroutine TElsiCsc_convertElsiToPackedHelicalReal(this, iNeighbour, nNeighbourSK, orb,&
      & species, coord,&
      & iAtomStart, iSparseStart, img2CentCell, nzval, primitive)

    !> Sparse conversion instance
    class(TElsiCsc), intent(inout) :: this

    !> Neighbour list for each atom (first index from 0!).
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom (incl. itthis).
    integer, intent(in) :: nNeighbourSK(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb


    !> Species of each atom
    integer :: species(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coord(:,:)


    !> Atom offset for the squared Hamiltonian
    integer, intent(in) :: iAtomStart(:)

    !> Indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Atomic mapping indexes.
    integer, intent(in) :: img2CentCell(:)

    !> Local non-zero elements
    real(dp), intent(in) :: nzval(:)

    !> Sparse Hamiltonian
    real(dp), intent(inout) :: primitive(:)


    call error("Internal error: TElsiCsc_convertElsiToPackedReal() called despite missing ELSI&
        & support")


  end subroutine TElsiCsc_convertElsiToPackedHelicalReal


  !> Convert CSC matrix format into DFTB+ sparse format
  !>
  !> Note: primitive will not be set to zero on startup, and values are added to enable addition of
  !> spin components. Make sure, you set it to zero before invoking this routine the first time.
  subroutine TElsiCsc_convertElsiToPackedHelicalCmplx(this, iNeighbour, nNeighbourSK, orb,&
      & species, coord,&
      & iAtomStart, iSparseStart, img2CentCell, kPoint, kWeight, iCellVec, cellVec, nzval,&
      & primitive)

    !> Sparse conversion instance
    class(TElsiCsc), intent(in) :: this

    !> Neighbour list for each atom (first index from 0!).
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom (incl. itthis).
    integer, intent(in) :: nNeighbourSK(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb


    !> Species of each atom
    integer :: species(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coord(:,:)


    !> Atom offset for the squared Hamiltonian
    integer, intent(in) :: iAtomStart(:)

    !> Indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Atomic mapping indexes.
    integer, intent(in) :: img2CentCell(:)

    !> Current k-point
    real(dp), intent(in) :: kPoint(:)

    !> Weight for current k-points
    real(dp), intent(in) :: kWeight

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Local non-zero elements
    complex(dp), intent(in) :: nzval(:)

    !> Sparse Hamiltonian
    real(dp), intent(inout) :: primitive(:)


    call error("Internal error: TElsiCsc_convertElsiToPackedCmplx() called despite missing ELSI&
        & support")


  end subroutine TElsiCsc_convertElsiToPackedHelicalCmplx


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Private routines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end module dftbp_elecsolvers_elsicsc
