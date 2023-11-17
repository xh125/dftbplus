!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Contains the interface to the ELSI solvers
module dftbp_elecsolvers_elsisolver
  use dftbp_common_accuracy, only : dp, lc
  use dftbp_common_environment, only : TEnvironment, globalTimers
  use dftbp_common_globalenv, only : stdOut
  use dftbp_common_version, only : TVersion
  use dftbp_dftb_energytypes, only : TEnergies
  use dftbp_dftb_etemp, only : fillingTypes
  use dftbp_dftb_periodic, only : TNeighbourList
  use dftbp_dftb_potentials, only : TPotentials
  use dftbp_dftb_spin, only : ud2qm
  use dftbp_dftb_spinorbit, only : addOnsiteSpinOrbitHam, getOnsiteSpinOrbitEnergy
  use dftbp_elecsolvers_elecsolvertypes, only : electronicSolverTypes
  use dftbp_elecsolvers_elsicsc, only : TElsiCsc
  use dftbp_extlibs_elsiiface, only : elsi_rw_handle, elsi_handle
  use dftbp_io_message, only : error, warning, cleanshutdown
  use dftbp_math_angmomentum, only : getLOnsite
  use dftbp_type_commontypes, only : TParallelKS, TOrbitals
  use dftbp_type_densedescr, only : TDenseDescr
  use dftbp_dftb_sparse2dense, only : unpackHPauliBlacs,&
      & unpackHSHelicalRealBlacs, unpackHSRealBlacs, packRhoHelicalCplxBlacs, packRhoCplxBlacs,&
      & unpackSPauliBlacs, packRhoPauliBlacs, packRhoHelicalRealBlacs, packRhoRealBlacs,&
      & unpackHSHelicalCplxBlacs, unpackHSCplxBlacs, packRhoHelicalRealBlacs, packRhoRealBlacs,&
      & packERhoPauliBlacs
  use dftbp_elecsolvers_elsicsc, only : TElsiCsc_init
  use dftbp_extlibs_elsiiface, only : elsi_get_version, elsi_finalize, elsi_reinit, elsi_init,&
      & elsi_set_mpi_global, elsi_set_sing_check, elsi_set_mpi, elsi_set_csc_blk,&
      & elsi_set_zero_def, elsi_set_sparsity_mask, elsi_set_blacs, elsi_init_rw, elsi_set_rw_blacs,&
      & elsi_set_elpa_solver, elsi_set_elpa_autotune, elsi_set_elpa_gpu,&
      & elsi_set_omm_flavor, elsi_set_omm_n_elpa, elsi_set_omm_tol,&
      & elsi_set_pexsi_np_per_pole, elsi_set_pexsi_mu_min, elsi_set_pexsi_mu_max,&
      & elsi_set_pexsi_method, elsi_set_pexsi_n_pole, elsi_set_pexsi_n_mu, elsi_set_pexsi_np_symbo,&
      & elsi_set_pexsi_delta_e, elsi_set_ntpoly_tol, elsi_set_ntpoly_filter,&
      & elsi_set_ntpoly_method, elsi_set_spin, elsi_set_kpoint, elsi_set_output,&
      & elsi_set_output_log, elsi_get_entropy, elsi_get_mu, elsi_get_pexsi_mu_max,&
      & elsi_get_pexsi_mu_min, elsi_write_mat_real, elsi_finalize_rw, elsi_dm_complex,&
      & elsi_write_mat_complex, elsi_set_mu_broaden_width, elsi_set_mu_mp_order,&
      & elsi_set_mu_broaden_scheme, elsi_set_pexsi_temp, elsi_set_csc, elsi_set_rw_csc,&
      & elsi_write_mat_complex_sparse, elsi_dm_complex_sparse, elsi_get_edm_real_sparse,&
      & elsi_get_edm_real, elsi_set_rw_mpi, elsi_get_edm_complex, elsi_get_edm_complex_sparse,&
      & elsi_dm_real, elsi_write_mat_real_sparse, elsi_dm_real_sparse
  use dftbp_extlibs_mpifx, only : MPI_SUM, mpifx_allreduceip
  implicit none

  private
  public :: TElsiSolverInp
  public :: TElsiSolver, TElsiSolver_init, TElsiSolver_final


  !> Input data for the ELSI solvers
  type :: TElsiSolverInp

    !> Choice of the solver
    integer :: iSolver

    !> Choice of ELPA solver
    integer :: elpaSolver = 2

    !> Enable ELPA autotuning
    logical :: elpaAutotune = .false.

    !> Enable GPU usage in ELPA
    logical :: elpaGpu = .false.

    !> Iterations of ELPA solver before OMM minimization
    integer :: ommIterationsElpa = 5

    !> Halting tolerance for OMM iterations
    real(dp) :: ommTolerance = 1.0E-10_dp

    !> Should the overlap be factorized before minimization
    logical :: ommCholesky = .true.

    !> PEXSI pole expansion method
    integer :: pexsiMethod = 3

    !> number of poles for PEXSI expansion
    integer :: pexsiNPole = 30

    !> number of processors per pole for PEXSI
    integer :: pexsiNpPerPole = 1

    !> number of interpolation points for mu (Fermi) search
    integer :: pexsiNMu = 2

    !> number of processors for symbolic factorisation
    integer :: pexsiNpSymbo = 1

    !> spectral radius (range of eigenvalues) if available
    real(dp) :: pexsiDeltaE = 10.0_dp

    !> density matrix purification algorithm
    integer :: ntpolyMethod = 2

    !> truncation threshold for sparse matrix multiplication
    real(dp) :: ntpolyTruncation = 1.0E-10_dp

    !> convergence tolerance for density matrix purification
    real(dp) :: ntpolyTolerance = 1.0E-5_dp

    !> Use sparse CSR format
    logical :: elsiCsr = .false.

    !> Tolerance for converting from dense matrices to internal sparse storage for libOMM, PEXSI and
    !> NTPoly.
    real(dp) :: elsi_zero_def

  end type TElsiSolverInp


  !> Contains settings for the solvers of the ELSI library. See ELSI manual for detailed meaning of
  !> variables
  type :: TElsiSolver

    private

    !> should the code write matrices and stop
    logical, public :: tWriteHS

    !> handle for matrix IO
    type(elsi_rw_handle), public :: rwHandle

    !> Solver id (using the central DFTB+ solver type)
    integer :: iSolver

    !> Handle for the ELSI library
    type(elsi_handle), public :: handle

    !> Use sparse CSR format
    logical, public :: isSparse = .false.

    !> Tolerance for converting from dense matrices to internal sparse storage for libOMM, PEXSI and
    !> NTPoly.
    real(dp) :: elsi_zero_def

    !> ELSI Solver choice
    integer :: solver

    !> Level of solver information output
    integer :: outputLevel

    !> See ELSI manual - parallelisation strategy
    integer :: parallel

    !> See ELSI manual - type of parallel data decomposition
    integer :: denseBlacs

    !> Number of basis functions
    integer :: nBasis

    !> Number of electrons in the system
    real(dp) :: nElectron

    !> Maximum spin occupation for levels
    real(dp) :: spinDegeneracy

    !> Number of states to solve when relevant
    integer :: nState

    !> Global comm
    integer :: mpiCommWorld

    !> Group comm
    integer :: myCommWorld

    !> BLACS matrix context in use
    integer :: myBlacsCtx

    !> BLACS block sizes
    integer :: BlacsBlockSize

    !> CSC blocksize
    integer :: csrBlockSize

    !> Number of independent spins
    integer :: nSpin

    !> Number of independent k-points
    integer :: nKPoint

    !> Index for current spin processed here
    integer :: iSpin

    !> Index for current k-point processed here
    integer :: iKPoint

    !> Weighting for current k-point
    real(dp) :: kWeight

    !> Choice of broadening function
    integer :: muBroadenScheme

    !> If Meth-Paxton, order of scheme
    integer :: muMpOrder

    !> Whether solver had been already initialised
    logical :: tSolverInitialised = .false.

    !> Has overlap been factorized
    logical :: tCholeskyDecomposed

    !> Is this the first calls for this geometry
    logical :: tFirstCalc = .true.

    !> Sparse format data structure
    type(TElsiCsc), allocatable :: elsiCsc


    !! ELPA settings

    !> ELPA solver choice
    integer :: elpaSolverOption

    !> Whether ELPA autotuning is enabled (1=true)
    integer :: elpaAutotune

    !> Whether ELPA uses GPUs (1=true)
    integer :: elpaGpu

    !! OMM settings

    !> Starting iterations with ELPA
    integer :: ommIter

    !> Tolerance for minimisation
    real(dp) :: ommTolerance

    !> Whether to Cholesky factorize and transform or work with general
    logical :: ommCholesky

    !! PEXSI settings

    !> Minimum contour range
    real(dp) :: pexsiMuMin

    !> Maximum contour range
    real(dp) :: pexsiMuMax

    !> Most negative potential
    real(dp) :: pexsiDeltaVMin

    !> Most positive potential
    real(dp) :: pexsiDeltaVMax

    !> Previous potentials
    real(dp), allocatable :: pexsiVOld(:)

    !> PEXSI pole expansion method
    integer :: pexsiMethod

    !> Number of poles for expansion
    integer :: pexsiNPole

    !> Processors per pole
    integer :: pexsiNpPerPole

    !> Processes for chemical potential search
    integer :: pexsiNMu

    !> Processors used for symbolic factorization stage
    integer :: pexsiNpSymbo

    !> Spectral radius
    real(dp) :: pexsiDeltaE

    !! NTPoly settings

    !> Choice of minimisation method
    integer :: ntpolyMethod

    !> Truncation threshold for elements
    real(dp) :: ntpolyTruncation

    !> Convergence tolerance
    real(dp) :: ntpolyTolerance

  contains

    procedure :: reset => TElsiSolver_reset
    procedure :: updateGeometry => TElsiSolver_updateGeometry
    procedure :: updateElectronicTemp => TElsiSolver_updateElectronicTemp
    procedure :: getDensity => TElsiSolver_getDensity
    procedure :: getEDensity => TElsiSolver_getEDensity
    procedure :: initPexsiDeltaVRanges => TElsiSolver_initPexsiDeltaVRanges
    procedure :: updatePexsiDeltaVRanges => TElsiSolver_updatePexsiDeltaVRanges
    procedure :: getSolverName => TElsiSolver_getSolverName

  end type TElsiSolver


contains


  !> Initialise ELSI solver
  subroutine TElsiSolver_init(this, inp, env, nBasisFn, nEl, iDistribFn, nSpin, iSpin, nKPoint,&
      & iKPoint, kWeight, tWriteHS, providesElectronEntropy)

    !> control structure for solvers, including ELSI data
    class(TElsiSolver), intent(out) :: this

    !> input structure for ELSI
    type(TElsiSolverInp), intent(in) :: inp

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> number of orbitals in the system
    integer, intent(in) :: nBasisFn

    !> number of electrons
    real(dp), intent(in) :: nEl(:)

    !> filling function
    integer, intent(in) :: iDistribFn

    !> total number of spin channels.
    integer, intent(in) :: nSpin

    !> spin channel processed by current process
    integer, intent(in) :: iSpin

    !> total number of k-points
    integer, intent(in) :: nKPoint

    !> K-point processed by current process.
    integer, intent(in) :: iKPoint

    !> Weight of current k-point
    real(dp), intent(in) :: kWeight

    !> Should the matrices be written out
    logical, intent(in) :: tWriteHS

    !> Whether the solver provides the TS term for electrons
    logical, intent(inout) :: providesElectronEntropy


    call error("Internal error: TElsiSolver_init() called despite missing ELSI support")


  end subroutine TElsiSolver_init


  !> Checks for supported ELSI api version, ideally 2.6.2, but 2.5.0 can also be used with warnings.
  subroutine supportedVersionNumber(version)

    !> Version value components inside the structure
    type(TVersion), intent(in) :: version

    logical :: isSupported, isPartSupported

    isSupported = version >= TVersion(2, 6)
    isPartSupported = version >= TVersion(2, 5)

    if (.not. isPartSupported) then
      call error("Unsuported ELSI version for DFTB+ (requires release >= 2.5.0)")
    else if (.not. isSupported) then
      call warning("ELSI version 2.5 is only partially supported due to changes in default solver&
          & behaviour for PEXSI at 2.6.0")
    end if

    write(stdOut,"(A,T30,I0,'.',I0,'.',I0)") 'ELSI library version :', version%numbers

  end subroutine supportedVersionNumber


  !> Finalizes the ELSI solver.
  subroutine TELsiSolver_final(this)

    !> Instance
    type(TElsiSolver), intent(inout) :: this


    call error("Internal error: TELsiSolver_final() called despite missing ELSI&
        & support")


  end subroutine TELsiSolver_final


  !> reset the ELSI solver - safer to do this on geometry change, due to the lack of a Choleskii
  !> refactorization option
  subroutine TElsiSolver_reset(this)

    !> Instance
    class(TElsiSolver), intent(inout) :: this



    call error("Internal error: TElsiSolver_reset() called despite missing ELSI support")


  end subroutine TElsiSolver_reset


  !> Resets solver due to geometry changes
  subroutine TElsiSolver_updateGeometry(this, env, neighList, nNeighbourSK, iAtomStart,&
      & iSparseStart, img2CentCell)

    !> Instance
    class(TElsiSolver), intent(inout) :: this

    !> Environment settings
    type(TEnvironment), intent(inout) :: env

    !> Neighbour list
    type(TNeighbourList), intent(in) :: neighList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Atom offset for the squared matrix
    integer, intent(in) :: iAtomStart(:)

    !> indexing array for the sparse Hamiltonian
    integer, intent(in) :: iSparseStart(0:,:)

    !> Mapping between image atoms and corresponding atom in the central cell.
    integer, intent(in) :: img2CentCell(:)


    call error("Internal error: TELsiSolver_updateGeometry() called despite missing ELSI&
        & support")


  end subroutine TElsiSolver_updateGeometry


  !> Updated the electronic temperature
  subroutine TElsiSolver_updateElectronicTemp(this, tempElec)

    !> Instance
    class(TElsiSolver), intent(inout) :: this

    !> Electronic temperature
    real(dp), intent(in) :: tempElec


    call error("Internal error: TELsiSolver_updateElectronicTemp() called despite missing ELSI&
        & support")


  end subroutine TElsiSolver_updateElectronicTemp


  !> Returns the density matrix using ELSI non-diagonalisation routines.
  subroutine TElsiSolver_getDensity(this, env, denseDesc, ham, over, neighbourList, nNeighbourSK,&
      & iSparseStart, img2CentCell, iCellVec, cellVec, kPoint, kWeight, tHelical, orb, species,&
      & coord, tRealHS, tSpinSharedEf, tSpinOrbit, tDualSpinOrbit, tMulliken, parallelKS, Ef,&
      & energy, rhoPrim, Eband, TS, iHam, xi, orbitalL, HSqrReal, SSqrReal, iRhoPrim, HSqrCplx,&
      & SSqrCplx)

    !> Electronic solver information
    class(TElsiSolver), intent(inout) :: this

    !> Environment settings
    type(TEnvironment), intent(inout) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> hamiltonian in sparse storage
    real(dp), intent(in) :: ham(:,:)

    !> sparse overlap matrix
    real(dp), intent(in) :: over(:)

    !> list of neighbours for each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Index array for the start of atomic blocks in sparse arrays
    integer, intent(in) :: iSparseStart(:,:)

    !> map from image atoms to the original unique atom
    integer, intent(in) :: img2CentCell(:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> k-points
    real(dp), intent(in) :: kPoint(:,:)

    !> Weights for k-points
    real(dp), intent(in) :: kWeight(:)

    !> Is the geometry helical
    logical, intent(in) :: tHelical

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> species of all atoms in the system
    integer, intent(in) :: species(:)

    !> atomic coordinates
    real(dp), intent(in) :: coord(:,:)

    !> Is the hamiltonian real (no k-points/molecule/gamma point)?
    logical, intent(in) :: tRealHS

    !> Is the Fermi level common across spin channels?
    logical, intent(in) :: tSpinSharedEf

    !> Are spin orbit interactions present
    logical, intent(in) :: tSpinOrbit

    !> Are block population spin orbit interactions present
    logical, intent(in) :: tDualSpinOrbit

    !> Should Mulliken populations be generated/output
    logical, intent(in) :: tMulliken

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Fermi level(s)
    real(dp), intent(inout) :: Ef(:)

    !> Energy contributions and total
    type(TEnergies), intent(inout) :: energy

    !> sparse density matrix
    real(dp), intent(out) :: rhoPrim(:,:)

    !> band structure energy
    real(dp), intent(out) :: Eband(:)

    !> electronic entropy times temperature
    real(dp), intent(out) :: TS(:)

    !> imaginary part of hamiltonian
    real(dp), intent(in), allocatable :: iHam(:,:)

    !> spin orbit constants
    real(dp), intent(in), allocatable :: xi(:,:)

    !> orbital moments of atomic shells
    real(dp), intent(inout), allocatable :: orbitalL(:,:,:)

    !> imaginary part of density matrix
    real(dp), intent(inout), allocatable :: iRhoPrim(:,:)

    !> dense real hamiltonian storage
    real(dp), intent(inout), allocatable :: HSqrReal(:,:)

    !> dense real overlap storage
    real(dp), intent(inout), allocatable :: SSqrReal(:,:)

    !> dense complex (k-points) hamiltonian storage
    complex(dp), intent(inout), allocatable :: HSqrCplx(:,:)

    !> dense complex (k-points) overlap storage
    complex(dp), intent(inout), allocatable :: SSqrCplx(:,:)


    call error("Internal error: TELsiSolver_getDensity() called despite missing ELSI support")


  end subroutine TElsiSolver_getDensity


  ! Returns the energy weighted density matrix using ELSI non-diagonalisation routines.
  subroutine TElsiSolver_getEDensity(this, env, denseDesc, nSpin, kPoint, kWeight, neighbourList,&
      & nNeighbourSK, tHelical, orb, species, coord, iSparseStart, img2CentCell, iCellVec, cellVec,&
      & tRealHS, parallelKS, ERhoPrim, SSqrReal, SSqrCplx)

    !> Electronic solver information
    class(TElsiSolver), intent(inout) :: this

    !> Environment settings
    type(TEnvironment), intent(inout) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Number of spin channels
    integer, intent(in) :: nSpin

    !> K-points
    real(dp), intent(in) :: kPoint(:,:)

    !> Weights for k-points
    real(dp), intent(in) :: kWeight(:)

    !> list of neighbours for each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Is the geometry helical
    logical, intent(in) :: tHelical

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> species of all atoms in the system
    integer, intent(in) :: species(:)

    !> atomic coordinates
    real(dp), intent(in) :: coord(:,:)

    !> Index array for the start of atomic blocks in sparse arrays
    integer, intent(in) :: iSparseStart(:,:)

    !> map from image atoms to the original unique atom
    integer, intent(in) :: img2CentCell(:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Is the hamiltonian real (no k-points/molecule/gamma point)?
    logical, intent(in) :: tRealHS

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Energy weighted sparse matrix
    real(dp), intent(out) :: ERhoPrim(:)

    !> Storage for dense overlap matrix
    real(dp), intent(inout), allocatable :: SSqrReal(:,:)

    !> Storage for dense overlap matrix (complex case)
    complex(dp), intent(inout), allocatable :: SSqrCplx(:,:)


    call error("Internal error: TELsiSolver_getDensity() called despite missing ELSI support")


  end subroutine TElsiSolver_getEDensity


  !> Initializes the PEXSI potentials.
  subroutine TElsiSolver_initPexsiDeltaVRanges(this, tSccCalc, potential)

    !> Electronic solver information
    class(TElsiSolver), intent(inout) :: this

    !> Whether we have an SCC calculation
    logical, intent(in) :: tSccCalc

    !> Potentials acting
    type(TPotentials), intent(in) :: potential


    call error("Internal error: TELsiSolver_initPexsiDeltaVRanges() called despite missing ELSI&
        & support")


  end subroutine TElsiSolver_initPexsiDeltaVRanges


  !> Updates the PEXSI potentials.
  subroutine TElsiSolver_updatePexsiDeltaVRanges(this, potential)

    !> Electronic solver information
    class(TElsiSolver), intent(inout) :: this

    !> Potentials acting
    type(TPotentials), intent(in) :: potential


    call error("Internal error: TElsiSolver_updatePexsiDeltaVRanges() called despite missing ELSI&
        & support")


  end subroutine TElsiSolver_updatePexsiDeltaVRanges


  !> Returns the name of the solver
  function TElsiSolver_getSolverName(this) result(solverName)

    !> Instance.
    class(TElsiSolver), intent(in) :: this

    !> Name of the solver.
    character(:), allocatable :: solverName


    solverName = ""

    call error("Internal error: TElsiSolver_getSolverName() called despite missing ELSI support")


  end function TElsiSolver_getSolverName


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Private routines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end module dftbp_elecsolvers_elsisolver
