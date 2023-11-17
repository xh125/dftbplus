!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

!> Contains the C-API of DFTB+.
module dftbp_capi
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env
  use dftbp_common_accuracy, only : dp
  use dftbp_common_file, only : TFileDescr, openFile
  use dftbp_common_globalenv, only : instanceSafeBuild
  use dftbp_dftbplus_qdepextpotgenc, only :&
      & getExtPotIfaceC, getExtPotGradIfaceC, TQDepExtPotGenC, TQDepExtPotGenC_init
  use dftbp_mmapi, only :&
      & TDftbPlus, TDftbPlus_init, TDftbPlus_destruct, TDftbPlusInput, TDftbPlusAtomList
  use dftbp_type_linkedlist, only : TListString, append, init, destruct
  implicit none
  private


  !> DFTB+ input tree
  type, bind(C) :: c_DftbPlusInput
    type(c_ptr) :: pDftbPlusInput
  end type c_DftbPlusInput


  !> DFTB+ calculation
  type, bind(C) :: c_DftbPlus
    type(c_ptr) :: instance
  end type c_DftbPlus


  !> Simple extension around the TDftbPlus with some additional variables for the C-API.
  type, extends(TDftbPlus) :: TDftbPlusC
    private
    type(TFileDescr) :: outputFile
  end type TDftbPlusC


contains


  !> Returns the current API version
  subroutine c_DftbPlus_api(major, minor, patch) bind(C, name='dftbp_api')

    !> makor.minor.patch
    integer(c_int), intent(out) :: major, minor, patch

    major = 0
    minor = 4
    patch = 0

  end subroutine c_DftbPlus_api


  !> Queries, whether library is instance safe (allowing for concurrent DFTB+ instances)
  function c_DftbPlus_is_instance_safe() result(instSafe) bind(C, name='dftbp_is_instance_safe')

    !> Whether library was built ensuring instance safety (multiple instances supported)
    logical(c_bool) :: instSafe

    instSafe = instanceSafeBuild

  end function c_DftbPlus_is_instance_safe


  !> Initialises a DFTB+ calculation with output sent to to some location
  subroutine c_DftbPlus_init(handler, outputFileName) bind(C, name='dftbp_init')

    !> DFTB+ handler
    type(c_DftbPlus), intent(out) :: handler

    !> output location
    type(c_ptr), value, intent(in) :: outputFileName

    type(TDftbPlusC), pointer :: instance

    allocate(instance)
    call handleOutputFile_(outputFileName, instance%outputFile)
    call TDftbPlus_init(instance%TDftbPlus, outputUnit=instance%outputFile%unit)
    handler%instance = c_loc(instance)

  end subroutine c_DftbPlus_init


  !> Initialises a DFTB+ calculator (MPI-version)
  subroutine c_DftbPlus_init_mpi(handler, outputFileName, mpiComm) bind(C, name='dftbp_init_mpi')

    !> DFTB+ handler
    type(c_DftbPlus), intent(out) :: handler

    !> output location
    type(c_ptr), value, intent(in) :: outputFileName

    !> MPI-communicator id
    integer(c_int), value, intent(in) :: mpiComm

    type(TDftbPlusC), pointer :: instance

    allocate(instance)
    call handleOutputFile_(outputFileName, instance%outputFile)
    call TDftbPlus_init(instance%TDftbPlus, outputUnit=instance%outputFile%unit, mpiComm=mpiComm)
    handler%instance = c_loc(instance)

  end subroutine c_DftbPlus_init_mpi


  !> finalises a DFTB+ instance
  subroutine c_DftbPlus_final(handler) bind(C, name='dftbp_final')

    !> DFTB+ handler
    type(c_DftbPlus), intent(inout) :: handler

    !> the specific instance to be finalised
    type(TDftbPlusC), pointer :: instance

    call c_f_pointer(handler%instance, instance)
    call TDftbPlus_destruct(instance%TDftbPlus)
    deallocate(instance)
    handler%instance = c_null_ptr

  end subroutine c_DftbPlus_final


  !> Read input for DFTB+ from a specified file
  subroutine c_DftbPlus_getInputFromFile(handler, fileName, inputHandler)&
      & bind(C, name='dftbp_get_input_from_file')

    !> handler for the input
    type(c_DftbPlus), intent(inout) :: handler

    !> file to read
    character(c_char), intent(in) :: fileName(*)

    !> handler for the resulting input
    type(c_DftbPlusInput), intent(inout) :: inputHandler

    type(TDftbPlusC), pointer :: instance
    type(TDftbPlusInput), pointer :: pDftbPlusInput
    character(:), allocatable :: fortranFileName

    allocate(pDftbPlusInput)
    fortranFileName = fortranChar(fileName)
    call c_f_pointer(handler%instance, instance)
    call instance%getInputFromFile(fortranFileName, pDftbPlusInput)
    inputHandler%pDftbPlusInput = c_loc(pDftbPlusInput)

  end subroutine c_DftbPlus_getInputFromFile


  !> Process a document tree to get settings for the calculation
  subroutine c_DftbPlus_processInput(handler, inputHandler) bind(C, name='dftbp_process_input')

    !> handler for the calculation instance
    type(c_DftbPlus), intent(inout) :: handler

    !> input tree handler
    type(c_DftbPlusInput), intent(inout) :: inputHandler

    type(TDftbPlusC), pointer :: instance
    type(TDftbPlusInput), pointer :: pDftbPlusInput

    call c_f_pointer(handler%instance, instance)
    call c_f_pointer(inputHandler%pDftbPlusInput, pDftbPlusInput)
    call instance%setupCalculator(pDftbPlusInput)

  end subroutine c_DftbPlus_processInput


  !> Get electrostatic potential from DFTB+ at specified points
  subroutine c_DftbPlus_get_elstat_potential(handler, nLocations, pot, locations)&
      & bind(C, name='dftbp_get_elstat_potential')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> Number of requested points
    integer, value, intent(in) :: nLocations

    !> Resulting potentials
    real(dp), intent(out) :: pot(*)

    !> Sites to calculate potential
    real(c_double), intent(in) :: locations(3,*)

    type(TDftbPlusC), pointer :: instance

    call c_f_pointer(handler%instance, instance)

    call instance%getElStatPotential(pot(1:nLocations), locations(:, 1:nLocations))

  end subroutine c_DftbPlus_get_elstat_potential


  !> Set an external potential on the DFTB+ calculation
  subroutine c_DftbPlus_setExternalPotential(handler, extPot, extPotGrad)&
      & bind(C, name='dftbp_set_external_potential')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> externally set potential
    real(c_double), intent(in) :: extPot(*)

    !> gradient of the potential wrt to atom positions
    type(c_ptr), value, intent(in) :: extPotGrad

    type(TDftbPlusC), pointer :: instance
    real(c_double), pointer :: pExtPotGrad(:,:)
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()

    if (c_associated(extPotGrad)) then
      call c_f_pointer(extPotGrad, pExtPotGrad, [3, nAtom])
    else
      pExtPotGrad => null()
    end if
    call instance%setExternalPotential(extPot(1:nAtom), pExtPotGrad)

  end subroutine c_DftbPlus_setExternalPotential


  !> Register a generator for an external potential
  subroutine c_DftbPlus_registerExtPotGenerator(handler, refPtr, extPotFunc, extPotGradFunc)&
      & bind(C, name='dftbp_register_ext_pot_generator')

    !> handler for the potential
    type(c_DftbPlus), intent(inout) :: handler

    !> pointer to the C routine for the external potential
    type(c_ptr), value, intent(in) :: refPtr

    !> function for the external potential
    type(c_funptr), value, intent(in) :: extPotFunc

    !> function for the gradient of the potential
    type(c_funptr), value, intent(in) :: extPotGradFunc

    type(TDftbPlusC), pointer :: instance
    type(TQDepExtPotGenC) :: extPotGenC
    procedure(getExtPotIfaceC), pointer :: pExtPotFunc
    procedure(getExtPotGradIfaceC), pointer :: pExtPotGradFunc

    call c_f_procpointer(extPotFunc, pExtPotFunc)
    call c_f_procpointer(extPotGradFunc, pExtPotGradFunc)
    call c_f_pointer(handler%instance, instance)
    call TQDepExtPotGenC_init(extPotGenC, refPtr, pExtPotFunc, pExtPotGradFunc)
    call instance%setQDepExtPotGen(extPotGenC)

  end subroutine c_DftbPlus_registerExtPotGenerator


  !> Set/replace the coordinates in a DFTB+ calculation instance
  subroutine c_DftbPlus_setCoords(handler, coords) bind(C, name='dftbp_set_coords')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> coordinates, (xyz, :nAtom)
    real(c_double), intent(in) :: coords(3,*)

    type(TDftbPlusC), pointer :: instance
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()
    call instance%setGeometry(coords(:, 1:nAtom))

  end subroutine c_DftbPlus_setCoords


  !> Set both the coordinates and lattice vectors
  subroutine c_DftbPlus_setCoordsAndLatticeVecs(handler, coords, latVecs)&
      & bind(C, name='dftbp_set_coords_and_lattice_vecs')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> coordinates, row major format (xyz, :nAtom)
    real(c_double), intent(in) :: coords(3,*)

    !> lattice vectors, row major format
    real(c_double), intent(in) :: latvecs(3, *)

    type(TDftbPlusC), pointer :: instance
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()
    call instance%setGeometry(coords(:, 1:nAtom), latVecs(:, 1:3))

  end subroutine c_DftbPlus_setCoordsAndLatticeVecs


  !> Set the coordinates and lattice vectors with an origin
  subroutine c_DftbPlus_setCoordsLatticeVecsOrigin(handler, coords, latVecs, origin)&
      & bind(C, name='dftbp_set_coords_lattice_origin')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> coordinates, row major format (xyz, :nAtom)
    real(c_double), intent(in) :: coords(3,*)

    !> lattice vectors, row major format
    real(c_double), intent(in) :: latvecs(3, *)

    !> coordinate origin
    real(c_double), intent(in) :: origin(3)

    type(TDftbPlusC), pointer :: instance
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()
    call instance%setGeometry(coords(:, 1:nAtom), latVecs(:, 1:3), origin(1:3))

  end subroutine c_DftbPlus_setCoordsLatticeVecsOrigin


  !> Set the neighbour list instead of computing it in DFTB+
  subroutine c_DftbPlus_setNeighbourList(handler, nAllAtom, nMaxNeighbours, nNeighbour,&
      & iNeighbour, neighDist, cutOff, coordNeighbours, neighbour2CentCell)&
      & bind(C, name='dftbp_set_neighbour_list')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> total number of neighbour atoms
    integer(c_int), value, intent(in) :: nAllAtom

    !> maximum number of neighbours an atom in the central cell can have
    integer(c_int), value, intent(in) :: nMaxNeighbours

    !> number of neighbours for each atom in the central cell
    integer(c_int), intent(in) :: nNeighbour(*)

    !> references to the neighbour atoms for an atom in the central cell
    integer(c_int), intent(in) :: iNeighbour(nMaxNeighbours, *)

    !> distances to the neighbour atoms for an atom in the central cell
    real(c_double), intent(in) :: neighDist(nMaxNeighbours, *)

    !> cutoff distance used for this neighbour list
    real(c_double), value, intent(in) :: cutOff

    !> coordinates of all neighbours
    real(c_double), intent(in) :: coordNeighbours(3, *)

    !> mapping between neighbour reference and atom index in the central cell
    integer(c_int), intent(in) :: neighbour2CentCell(*)

    type(TDftbPlusC), pointer :: instance
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()
    call instance%setNeighbourList(nNeighbour(1:nAtom), iNeighbour(:,1:nAtom),&
        & neighDist(:,1:nAtom), cutOff, coordNeighbours(:,1:nAllAtom),&
        & neighbour2CentCell(1:nAllAtom))

  end subroutine c_DftbPlus_setNeighbourList


  !> Obtain nr. of atoms.
  function c_DftbPlus_nrOfAtoms(handler) result(nAtom) bind(C, name='dftbp_get_nr_atoms')
    type(c_DftbPlus), intent(inout) :: handler
    integer(c_int) :: nAtom

    type(TDftbPlusC), pointer :: instance

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()

  end function c_DftbPlus_nrOfAtoms


  !> Obtain nr. of k-points.
  function c_DftbPlus_nrOfKPoints(handler) result(nKPoints) bind(C, name='dftbp_nr_kpoints')
    type(c_DftbPlus), intent(inout) :: handler
    integer(c_int) :: nKPoints

    type(TDftbPlusC), pointer :: instance

    call c_f_pointer(handler%instance, instance)
    nKPoints = instance%nrOfKPoints()

  end function c_DftbPlus_nrOfKPoints


  !> Obtain masses of atoms in the DFTB+ calculation.
  subroutine c_DftbPlus_get_atom_masses(handler, masses) bind(C, name='dftbp_get_masses')
    type(c_DftbPlus), intent(inout) :: handler
    real(c_double), intent(out) :: masses(*)

    type(TDftbPlusC), pointer :: instance
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()
    call instance%getAtomicMasses(masses(:nAtom))

  end subroutine c_DftbPlus_get_atom_masses


  !> Obtain nr. basis functions at each atoms in the DFTB+ calculation.
  subroutine c_DftbPlus_get_atom_nr_basis(handler, nOrbitals) bind(C, name='dftbp_get_nr_orbitals')
    type(c_DftbPlus), intent(inout) :: handler
    integer(c_int), intent(out) :: nOrbitals(*)

    type(TDftbPlusC), pointer :: instance
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()
    call instance%getNOrbitalsOnAtoms(nOrbitals(:nAtom))

  end subroutine c_DftbPlus_get_atom_nr_basis


  !> Retrieve the cutoff distance that is being used for interactions
  function c_DftbPlus_getCutOff(handler) result(cutOff) bind(C, name='dftbp_get_cutoff')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> cutoff distance
    real(dp) :: cutOff

    type(TDftbPlusC), pointer :: instance

    call c_f_pointer(handler%instance, instance)
    cutOff = instance%getCutOff()

  end function c_DftbPlus_getCutOff


  !> Obtain the DFTB+ energy
  subroutine c_DftbPlus_getEnergy(handler, merminEnergy) bind(C, name='dftbp_get_energy')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> resulting energy
    real(c_double), intent(out) :: merminEnergy

    type(TDftbPlusC), pointer :: instance

    call c_f_pointer(handler%instance, instance)
    call instance%getEnergy(merminEnergy)

  end subroutine c_DftbPlus_getEnergy


  !> Obtain the gradients wrt DFTB atom positions
  subroutine c_DftbPlus_getGradients(handler, gradients) bind(C, name='dftbp_get_gradients')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> gradients, row major format
    real(c_double), intent(out) :: gradients(3, *)

    type(TDftbPlusC), pointer :: instance
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()
    call instance%getGradients(gradients(:, 1:nAtom))

  end subroutine c_DftbPlus_getGradients


  !> Obtain the stress tensor of the periodic system
  subroutine c_DftbPlus_getStressTensor(handler, stresstensor)&
      & bind(C, name='dftbp_get_stress_tensor')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> gradients, row major format
    real(c_double), intent(out) :: stresstensor(3, 3)

    type(TDftbPlusC), pointer :: instance

    call c_f_pointer(handler%instance, instance)

    call instance%getStressTensor(stresstensor(:, 1:3))

  end subroutine c_DftbPlus_getStressTensor


  !> Obtain gross (Mulliken) charges for atoms wrt to neutral references
  subroutine c_DftbPlus_getGrossCharges(handler, atomCharges)&
      & bind(C, name='dftbp_get_gross_charges')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> resulting atomic charges
    real(c_double), intent(out) :: atomCharges(*)

    type(TDftbPlusC), pointer :: instance
    integer :: nAtom

    call c_f_pointer(handler%instance, instance)
    nAtom = instance%nrOfAtoms()
    call instance%getGrossCharges(atomCharges(1:nAtom))

  end subroutine c_DftbPlus_getGrossCharges


  !> Obtain CM5 charges
  subroutine c_DftbPlus_getCM5Charges(handler, atomCharges)&
      & bind(C, name='dftbp_get_cm5_charges')

    !> handler for the calculation
    type(c_DftbPlus), intent(inout) :: handler

    !> resulting atomic charges
    real(c_double), intent(out) :: atomCharges(*)

    !> f pointer of input arguments
    type(TDftbPlusC), pointer :: instance

    integer :: nAtom

    !> translate c to f objects
    call c_f_pointer(handler%instance, instance)

    nAtom = instance%nrOfAtoms()
    call instance%getCM5Charges(atomCharges(1:nAtom))

  end subroutine c_DftbPlus_getCM5Charges


  !> Converts a 0-char terminated C-type string into a Fortran string.
  function fortranChar(cstring, maxlen)

    !> C-type string as array
    character(kind=c_char), intent(in) :: cstring(*)

    !> Maximal string length. If C-string is longer, it will be chopped.
    integer, intent(in), optional  :: maxlen

    !> Resulting Fortran string
    character(:, kind=c_char), allocatable :: fortranChar

    integer :: ii, maxlen0

    if (present(maxlen)) then
      maxlen0 = maxlen
    else
      maxlen0 = huge(maxlen0) - 1
    end if

    do ii = 1, maxlen0
      if (cstring(ii) == c_null_char) then
        exit
      end if
    end do
    allocate(character(ii - 1) :: fortranChar)
    fortranChar = transfer(cstring(1 : ii - 1), fortranChar)

  end function fortranChar


  ! Returns a unit for an opened output file.
  !
  ! If outputFileName is associated, a file with that name will be created (and returned), otherwise
  ! the output_unit is returned (and no file is created)
  !
  subroutine handleOutputFile_(outputFileName, outputFile)
    type(c_ptr), intent(in) :: outputFileName
    type(TFileDescr), intent(out) :: outputFile

    character(c_char), pointer :: pOutputFileName
    character(:), allocatable :: fortranFileName

    if (c_associated(outputFileName)) then
      call c_f_pointer(outputFileName, pOutputFileName)
      fortranFileName = fortranChar(pOutputFileName)
      call openFile(outputFile, fortranFileName, mode="w")
    else
      call outputFile%connectToUnit(output_unit)
    end if

  end subroutine handleOutputFile_


end module dftbp_capi
