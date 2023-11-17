!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

!> Proxy module for interfacing with the tblite library.
!>
!> @note
!> The library calculates energies, gradients (∂E/∂R) and strain derivatives (∂E/∂ε = –V·σ),
!> while DFTB+ works with atom-resolved energies, gradients and the stress tensor.
!> The atom-resolved energy partitioning is archived by distributing the energy equivalently
!> over all atoms. The strain derivative is saved as such in the container and only
!> transformed to the stress tensor on demand using σ = –1/V·∂E/∂ε.
!>
!> @warning
!> This module has to account for changing between sign conventions between DFTB+ and tblite.
!> Generally, all intermediate quantities passed from DFTB+ to the library are using the
!> conventions of DFTB+, while all intermediate quantities passed from the library to DFTB+
!> will follow tblite's conventions (usually encapsulated in derived types already).
!>
!> Both tblite and DFTB+ use consistent ordering of spherical harmonics
!> in the standard sorting, *i.e.* [-l, ..., 0, ..., l].
module dftbp_extlibs_tblite
  use dftbp_common_accuracy, only : dp
  use dftbp_common_environment, only : TEnvironment
  use dftbp_common_schedule, only : distributeRangeInChunks, assembleChunks
  use dftbp_dftb_charges, only : getSummedCharges
  use dftbp_dftb_periodic, only : TNeighbourList
  use dftbp_io_message, only : error
  use dftbp_math_blasroutines, only : gemv
  use dftbp_math_simplealgebra, only : determinant33
  use dftbp_type_commontypes, only : TOrbitals
  use dftbp_type_integral, only : TIntegral
  implicit none
  private

  public :: TTBLite, TTBLiteInput, TTBLite_init, writeTBLiteInfo
  public :: tbliteMethod


  !> Possible methods available in this library
  type :: EnumMethod

    !> Selector for GFN2-xTB Hamiltonian
    integer :: gfn2xtb = 2

    !> Selector for GFN1-xTB Hamiltonian
    integer :: gfn1xtb = 1

    !> Selector for IPEA1-xTB Hamiltonian
    integer :: ipea1xtb = 11

  end type EnumMethod

  !> Actual numerated method selector
  type(EnumMethod), parameter :: tbliteMethod = EnumMethod()


  !> Information on the library setup
  type :: TBLiteInfo

    !> Parametrization name
    character(len=:), allocatable :: name

  end type TBLiteInfo


  !> Input for the library
  type :: TTBLiteInput


    !> Parametrization info
    type(TBLiteInfo) :: info

  contains

    !> Create geometry data for library
    procedure :: setupGeometry

    !> Create parametrization data for library
    generic :: setupCalculator => setupCalculatorFromEnum, setupCalculatorFromFile
    procedure :: setupCalculatorFromEnum
    procedure :: setupCalculatorFromFile

    !> Create orbital information from input data
    procedure :: setupOrbitals

  end type TTBLiteInput


  !> Library interface handler
  type :: TTBLite

    !> Parametrization info
    type(TBLiteInfo) :: info

    !> Mapping between species and identifiers
    integer, allocatable :: sp2id(:)

    !> Coordination number
    real(dp), allocatable :: cn(:)

    !> Derivative of the coordination number w.r.t. atomic displacements
    real(dp), allocatable :: dcndr(:, :, :)

    !> Derivative of the coordination number w.r.t. strain deformations
    real(dp), allocatable :: dcndL(:, :, :)

    !> Diagonal elements of the Hamiltonian
    real(dp), allocatable :: selfenergy(:)

    !> Derivatives of the diagonal elements w.r.t. the coordination number
    real(dp), allocatable :: dsedcn(:)

    !> Repulsion energy
    real(dp), allocatable :: erep(:)

    !> Halogen bonding energy
    real(dp), allocatable :: ehal(:)

    !> Non-self consistent dispersion energy
    real(dp), allocatable :: edisp(:)

    !> Self-consistent dispersion energy
    real(dp), allocatable :: escd(:)

    !> Electrostatic energy
    real(dp), allocatable :: ees(:)

    !> Contributions to the gradient
    real(dp), allocatable :: gradient(:, :)

    !> Contributions to the virial
    real(dp) :: sigma(3, 3)

  contains

    !> Update internal copy of coordinates
    procedure :: updateCoords

    !> Update internal copy of lattice vectors
    procedure :: updateLatVecs

    !> Get real space cutoff
    procedure :: getRCutoff

    !> Get energy contributions
    procedure :: getEnergies

    !> Get force contributions
    procedure :: addGradients

    !> Get stress tensor contributions
    procedure :: getStress

    !> Updates with changed charges for the instance.
    procedure :: updateCharges

    !> Returns shifts per atom
    procedure :: getShifts

    !> Get orbital information
    procedure :: getOrbitalInfo

    !> Get information about required multipolar contributions
    procedure :: getMultipoleInfo

    !> Get reference occupation
    procedure :: getReferenceN0

    !> Returns the equivalence to get the correct mixing of charge dependent contributions
    procedure :: getOrbitalEquiv

    !> Construct Hamiltonian and overlap related integrals
    procedure :: buildSH0

    !> Evaluate shift related derivatives from Hamiltonian and overlap related integrals
    procedure :: buildDerivativeShift

    !> Calculates nonadiabatic matrix: overlap gradient (Sprime) times velocities (Rdot)
    procedure :: buildRdotSprime

  end type TTBLite


  !> Number of dipole components used in tblite library (x, y, z)
  integer, parameter :: dimDipole = 3

  !> Number of quadrupole components used in tblite library (xx, xy, yy, xz, yz, zz)
  integer, parameter :: dimQuadrupole = 6


contains


  !> Setup geometry information for input data
  subroutine setupGeometry(this, nAtom, species0, coords0, speciesNames, latVecs)

    !> Input data
    class(TTBLiteInput), intent(inout) :: this

    !> Nr. of atoms in the system
    integer, intent(in) :: nAtom

    !> Species of every atom in the unit cell
    integer, intent(in) :: species0(:)

    !> Atomic coordinates in the unit cell
    real(dp), intent(in) :: coords0(:,:)

    !> Symbols of the species
    character(len=*), intent(in) :: speciesNames(:)

    !> Lattice vectors, if the system is periodic
    real(dp), intent(in), optional :: latVecs(:,:)

    call notImplementedError
  end subroutine setupGeometry


  !> Setup calculator for input data
  subroutine setupCalculatorFromEnum(this, method)

    !> Input data
    class(TTBLiteInput), intent(inout) :: this

    !> Selected method
    integer, intent(in) :: method

    call notImplementedError
  end subroutine setupCalculatorFromEnum


  !> Setup calculator for input data
  subroutine setupCalculatorFromFile(this, method)

    !> Input data
    class(TTBLiteInput), intent(inout) :: this

    !> Selected method
    character(len=*), intent(in) :: method

    call notImplementedError
  end subroutine setupCalculatorFromFile


  !> Setup orbital information from input data
  subroutine setupOrbitals(this, species0, orb)

    !> Input data
    class(TTBLiteInput), intent(in) :: this

    !> Species of every atom in the unit cell
    integer, intent(in) :: species0(:)

    !> Orbital information
    type(TOrbitals), intent(out) :: orb

    call notImplementedError
  end subroutine setupOrbitals


  !> Constructor for the library interface
  subroutine TTBLite_init(this, input, nAtom, species0, speciesNames, coords0, latVecs)

    !> Instance of the library interface
    type(TTBLite), intent(out) :: this

    !> Input to construct the library interface from
    type(TTBLiteInput), intent(in) :: input

    !> Nr. of atoms in the system
    integer, intent(in) :: nAtom

    ! Spin channels in the system
    integer, parameter :: nSpin = 1

    !> Species of every atom in the unit cell
    integer, intent(in) :: species0(:)

    !> Atomic coordinates in the unit cell
    real(dp), intent(in) :: coords0(:,:)

    !> Symbols of the species
    character(len=*), intent(in) :: speciesNames(:)

    !> Lattice vectors, if the system is periodic
    real(dp), intent(in), optional :: latVecs(:,:)

    call notImplementedError
  end subroutine TTBLite_init




  subroutine getSpeciesIdentifierMap(sp2id, species, id)

    !> Mapping from species to identifiers
    integer, intent(out) :: sp2id(:)

    !> Element species used in DFTB+
    integer, intent(in) :: species(:)

    !> Element identifiers used in tblite
    integer, intent(in) :: id(:)

    integer :: nSpecies, iAt, iSp, iId
    logical, allocatable :: done(:)

    nSpecies = maxval(species)
    allocate(done(nSpecies))
    done(:) = .false.
    do iAt = 1, size(species)
      iId = id(iAt)
      iSp = species(iAt)
      if (done(iSp)) cycle
      sp2id(iSp) = iId
      done(iSp) = .true.
    end do
  end subroutine getSpeciesIdentifierMap


  !> Write information about library setup
  subroutine writeTBLiteInfo(unit, this)

    !> Formatted unit for output
    integer, intent(in) :: unit

    !> Data structure
    type(TTBLite), intent(in) :: this

    call notImplementedError
  end subroutine writeTBLiteInfo


  !> Update internal stored coordinates
  subroutine updateCoords(this, env, neighList, img2CentCell, coords, species0)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> List of neighbours to atoms
    type(TNeighbourList), intent(in) :: neighList

    !> Image to central cell atom index
    integer, intent(in) :: img2CentCell(:)

    !> Atomic coordinates
    real(dp), intent(in) :: coords(:,:)

    !> Central cell chemical species
    integer, intent(in) :: species0(:)

    call notImplementedError
  end subroutine updateCoords


  !> Update internal copy of lattice vectors
  subroutine updateLatVecs(this, latVecs)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Lattice vectors
    real(dp), intent(in) :: latVecs(:,:)

    call notImplementedError
  end subroutine updateLatVecs


  !> Get energy contributions
  subroutine getEnergies(this, energies)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Energy contributions for each atom
    real(dp), intent(out) :: energies(:)

    call notImplementedError
  end subroutine getEnergies


  !> Get force contributions
  subroutine addGradients(this, env, neighList, species, coords, img2CentCell, gradients)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Neighbour list.
    type(TNeighbourList), intent(in) :: neighList

    !> Specie for each atom.
    integer, intent(in) :: species(:)

    !> Coordinate of each atom.
    real(dp), intent(in) :: coords(:,:)

    !> Mapping of atoms to cetnral cell.
    integer, intent(in) :: img2CentCell(:)

    !> Gradient contributions for each atom
    real(dp), intent(inout) :: gradients(:,:)

    call notImplementedError
  end subroutine addGradients


  !> Get stress tensor contributions, by converting the saved strain derivatives.
  !> Calculating the stress tensor includes a sign change from the strain derivatives
  !> and a normalization with the cell volume
  subroutine getStress(this, stress)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Stress tensor contributions
    real(dp), intent(out) :: stress(:,:)

    call notImplementedError
  end subroutine getStress


  !> Distance cut off for dispersion interactions
  function getRCutoff(this) result(cutoff)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Resulting cutoff
    real(dp) :: cutoff

    call notImplementedError
  end function getRCutoff


  !> Updates with changed charges for the instance.
  !>
  !> This routine will be called for both potential and energy calculations.
  !> In case of the energy calculations the orbital charges qq are the actual
  !> output charges, while in case of the potential calculation the orbital
  !> charges in qq are an incomplete output from the mixer and are only accurate
  !> up to the requested charges from the variable_info routine.
  !>
  !> Also the orbital charges have the opposite sign of the ones requested from
  !> the library.
  subroutine updateCharges(this, env, species, neighList, qq, q0, dipAtom, quadAtom, &
      & img2CentCell, orb)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Species, shape: [nAtom]
    integer, intent(in) :: species(:)

    !> Neighbour list.
    type(TNeighbourList), intent(in) :: neighList

    !> Orbital populations
    real(dp), intent(in) :: qq(:,:,:)

    !> Reference orbital populations
    real(dp), intent(in) :: q0(:,:,:)

    !> Cumulative atomic dipole populations
    real(dp), intent(in), optional :: dipAtom(:,:,:)

    !> Cumulative atomic quadrupole populations
    real(dp), intent(in), optional :: quadAtom(:,:,:)

    !> Mapping on atoms in central cell.
    integer, intent(in) :: img2CentCell(:)

    !> Orbital information
    type(TOrbitals), intent(in) :: orb

    call notImplementedError
  end subroutine updateCharges


  !> Returns atom-resolved and shell-resolved shifts from library.
  !>
  !> Potential shifts in tblite are calculated as derivatives w.r.t. the partial
  !> charges / multipole moments, while in DFTB+ they are calculated as derivative
  !> w.r.t. the population. This means we have to flip the sign here.
  subroutine getShifts(this, shiftPerAtom, shiftPerShell, dipShift, quadShift)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Shift per atom
    real(dp), intent(out) :: shiftPerAtom(:)

    !> Shift per shell
    real(dp), intent(out) :: shiftPerShell(:,:)

    !> Dipolar shift per atom
    real(dp), intent(out), optional :: dipShift(:,:)

    !> Quadrupolar shift per atom
    real(dp), intent(out), optional :: quadShift(:,:)

    call notImplementedError
  end subroutine getShifts


  !> Create orbital information. The orbital information is already generated in
  !> this%calc%bas, but might be incomplete w.r.t. the information required here.
  subroutine getOrbitalInfo(this, species0, orb)

    !> Data structure
    class(TTBLite), intent(in) :: this

    !> Species of each atom, shape: [nAtom]
    integer, intent(in) :: species0(:)

    !> Orbital information
    type(TOrbitals), intent(out) :: orb

    call notImplementedError
  end subroutine getOrbitalInfo




  !> Get information on required multipolar contributions
  subroutine getMultipoleInfo(this, nDipole, nQuadrupole)

    !> Data structure
    class(TTBLite), intent(in) :: this

    !> Number of dipole moment components
    integer, intent(out) :: nDipole

    !> Number of quadrupole moment components
    integer, intent(out) :: nQuadrupole

    call notImplementedError
  end subroutine getMultipoleInfo


  !> Get reference occupation numbers.
  subroutine getReferenceN0(this, species0, referenceN0)

    !> Data structure
    class(TTBLite), intent(in) :: this

    !> Species of each atom, shape: [nAtom]
    integer, intent(in) :: species0(:)

    !> Reference occupation numbers
    real(dp), intent(out) :: referenceN0(:, :)

    call notImplementedError
  end subroutine getReferenceN0


  !> Returns the equivalence to get the correct mixing of charge dependent contributions
  subroutine getOrbitalEquiv(this, equivOrb, equivDip, equivQuad, orb, species)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> The equivalence vector for orbital populations
    integer, intent(out) :: equivOrb(:,:,:)

    !> The equivalence vector for cumulative atomic dipole populations
    integer, intent(out) :: equivDip(:,:)

    !> The equivalence vector for cumulative atomic quadrupole populations
    integer, intent(out) :: equivQuad(:,:)

    !> Information about the orbitals and their angular momenta
    type(TOrbitals), intent(in) :: orb

    !> Species of each atom
    integer, intent(in) :: species(:)

    call notImplementedError
  end subroutine getOrbitalEquiv


  !> Build atomic block sparse compressed Hamiltonian and overlap related integrals
  subroutine buildSH0(this, env, species, coords, nNeighbour, iNeighbours, img2centCell, &
      & iPair, orb, hamiltonian, overlap, dpintBra, dpintKet, qpintBra, qpintKet)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Returns the non-self-consistent Hamiltonian
    real(dp), intent(out) :: hamiltonian(:)

    !> Returns the non-self-consistent Hamiltonian
    real(dp), intent(out) :: overlap(:)

    !> Dipole moment integral matrix, operator on the bra function
    real(dp), intent(inout) :: dpintBra(:, :)

    !> Dipole moment integral matrix, operator on the ket function
    real(dp), intent(inout) :: dpintKet(:, :)

    !> Quadrupole moment integral matrix, operator on the bra function
    real(dp), intent(inout) :: qpintBra(:, :)

    !> Quadrupole moment integral matrix, operator on the ket function
    real(dp), intent(inout) :: qpintKet(:, :)

    !> Atomic coordinates
    real(dp), intent(in) :: coords(:,:)

    !> Number of surrounding neighbours for each atom
    integer, intent(in) :: nNeighbour(:)

    !> List of surrounding neighbours for each atom
    integer, intent(in) :: iNeighbours(0:,:)

    !> Mapping of images back to atoms in the central cell
    integer, intent(in) :: img2centCell(:)

    !> Chemical species of each atom
    integer, intent(in) :: species(:)

    !> Shift vector, where the interaction between two atoms
    integer, intent(in) :: iPair(0:,:)

    !> Information about the orbitals in the system
    type(TOrbitals), intent(in) :: orb

    call notImplementedError
  end subroutine buildSH0




  !> Shift multipole operator from Ket function (center i) to Bra function (center j),
  !> the multipole operator on the Bra function can be assembled from the lower moments
  !> on the Ket function and the displacement vector using horizontal shift rules.
  !>
  !> This is usually done inside the tblite library, but since we want to have both
  !> Bra and Ket contributions at once and do not want to iterate over both triangles
  !> of the multipole integral matrix we perform the shift of the moment operator here.
  !>
  !> Candidate for (partial) upstreaming in tblite library.
  pure subroutine shiftOperator(vec, s, di, qi, dj, qj)

    !> Displacement vector of center i and j
    real(dp),intent(in) :: vec(:)

    !> Overlap integral between basis functions
    real(dp),intent(in) :: s

    !> Dipole integral with operator on Ket function (center i)
    real(dp),intent(in) :: di(:)

    !> Quadrupole integral with operator on Ket function (center i)
    real(dp),intent(in) :: qi(:)

    !> Dipole integral with operator on Bra function (center j)
    real(dp),intent(out) :: dj(:)

    !> Quadrupole integral with operator on Bra function (center j)
    real(dp),intent(out) :: qj(:)

    real(dp) :: tr

    ! Create dipole operator on Bra function from Ket function and shift contribution
    ! due to monopol displacement
    dj(1) = di(1) + vec(1)*s
    dj(2) = di(2) + vec(2)*s
    dj(3) = di(3) + vec(3)*s

    ! For the quadrupole operator on the Bra function we first construct the shift
    ! contribution from the dipole and monopol displacement, since we have to remove
    ! the trace contribution from the shift and the moment integral on the Ket function
    ! is already traceless
    qj(1) = 2*vec(1)*di(1) + vec(1)**2*s
    qj(3) = 2*vec(2)*di(2) + vec(2)**2*s
    qj(6) = 2*vec(3)*di(3) + vec(3)**2*s
    qj(2) = vec(1)*di(2) + vec(2)*di(1) + vec(1)*vec(2)*s
    qj(4) = vec(1)*di(3) + vec(3)*di(1) + vec(1)*vec(3)*s
    qj(5) = vec(2)*di(3) + vec(3)*di(2) + vec(2)*vec(3)*s
    ! Now collect the trace of the shift contribution
    tr = 0.5_dp * (qj(1) + qj(3) + qj(6))

    ! Finally, assemble the quadrupole operator on the Bra function from the operator
    ! on the Ket function and the traceless shift contribution
    qj(1) = qi(1) + 1.5_dp * qj(1) - tr
    qj(2) = qi(2) + 1.5_dp * qj(2)
    qj(3) = qi(3) + 1.5_dp * qj(3) - tr
    qj(4) = qi(4) + 1.5_dp * qj(4)
    qj(5) = qi(5) + 1.5_dp * qj(5)
    qj(6) = qi(6) + 1.5_dp * qj(6) - tr
  end subroutine shiftOperator


  !> Evaluate derivatives of potential shifts from Hamiltonian and overlap related
  !> integrals.
  subroutine buildDerivativeShift(this, env, DM, EDM, coords, species, &
      & nNeighbour, iNeighbours, img2CentCell, iPair, orb, shift, dipShift, quadShift)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> density matrix in packed format
    real(dp), intent(in) :: DM(:,:)

    !> energy-weighted density matrix in packed format
    real(dp), intent(in) :: EDM(:)

    !> list of all atomic coordinates
    real(dp), intent(in) :: coords(:,:)

    !> list of all atomic species
    integer, intent(in) :: species(:)

    !> number of neighbours of each atom
    integer, intent(in) :: nNeighbour(:)

    !> neighbour list for atoms
    integer, intent(in) :: iNeighbours(0:,:)

    !> indexing array for periodic image atoms
    integer, intent(in) :: img2CentCell(:)

    !> indexing array for the Hamiltonian
    integer, intent(in) :: iPair(0:,:)

    !> Information about the shells and orbitals in the system.
    type(TOrbitals), intent(in) :: orb

    !> block shift from the potential
    real(dp), intent(in) :: shift(:,:,:,:)

    !> Dipole potential shift, shape: [nDipole, nAtom]
    real(dp), intent(in) :: dipShift(:, :)

    !> Quadrupole potential shift, shape: [nQuadrupole, nAtom]
    real(dp), intent(in) :: quadShift(:, :)

    call notImplementedError
  end subroutine buildDerivativeShift




  !> Calculates nonadiabatic matrix: overlap gradient (Sprime) times velocities (Rdot)
  subroutine buildRdotSprime(this, env, RdotSprime, coords, dcoord, species, nNeighbour, &
      & iNeighbour, img2CentCell, iSquare, orb)

    !> Data structure
    class(TTBLite), intent(inout) :: this

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Nonadiabatic coupling matrix elements
    complex(dp), intent(out) :: RdotSprime(:,:)

    !> Coords of the atoms (3, nAllAtom)
    real(dp), intent(in) :: coords(:,:)

    !> Change in coords of the atoms (3, nAtom)
    real(dp), intent(in) :: dcoord(:,:)

    !> List of all atomic species
    integer, intent(in) :: species(:)

    !> Number of neighbours for atoms out to max interaction distance (excluding Ewald terms)
    integer, intent(in) :: nNeighbour(:)

    !> Neighbour list for atoms
    integer, intent(in) :: iNeighbour(0:,:)

    !> Image atoms to their equivalent in the central cell
    integer, intent(in) :: img2CentCell(:)

    !> Index array for start of atomic block in dense matrices
    integer, intent(in) :: iSquare(:)

    !> Data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    call error("Forces currently not available in Ehrenfest dynamic with this Hamiltonian")
  end subroutine buildRdotSprime


  subroutine notImplementedError

    call error("DFTB+ compiled without support for tblite library")
  end subroutine notImplementedError


end module dftbp_extlibs_tblite
