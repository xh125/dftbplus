!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!





!> Contains routines to write out various data structures in a comprehensive tagged format.
module dftbp_io_taggedoutput
  use dftbp_common_accuracy, only : dp
  implicit none

  private
  public :: tagLabels
  public :: TTaggedWriter, TTaggedWriter_init


  !> Length of permissible tag labels. Tag names should be shorter than lenLabel!
  integer, parameter :: lenLabel = 20

  !> Max length of the format strings for individual items
  integer, parameter :: lenFormStr = 20


  !> Contains a writer to write data in tagged form.
  type :: TTaggedWriter
    private
    ! Format strings
    character(len=lenFormStr) :: formReal
    character(len=lenFormStr) :: formCmplx
    character(len=lenFormStr) :: formInt
    character(len=lenFormStr) :: formLogical
    logical :: initialized = .false.
  contains
        procedure, private :: writeReal0 => TTaggedWriter_writeReal0
        generic :: write => writeReal0
        procedure, private :: writeReal1 => TTaggedWriter_writeReal1
        generic :: write => writeReal1
        procedure, private :: writeReal2 => TTaggedWriter_writeReal2
        generic :: write => writeReal2
        procedure, private :: writeReal3 => TTaggedWriter_writeReal3
        generic :: write => writeReal3
        procedure, private :: writeReal4 => TTaggedWriter_writeReal4
        generic :: write => writeReal4
        procedure, private :: writeReal5 => TTaggedWriter_writeReal5
        generic :: write => writeReal5
        procedure, private :: writeCplx0 => TTaggedWriter_writeCplx0
        generic :: write => writeCplx0
        procedure, private :: writeCplx1 => TTaggedWriter_writeCplx1
        generic :: write => writeCplx1
        procedure, private :: writeCplx2 => TTaggedWriter_writeCplx2
        generic :: write => writeCplx2
        procedure, private :: writeCplx3 => TTaggedWriter_writeCplx3
        generic :: write => writeCplx3
        procedure, private :: writeCplx4 => TTaggedWriter_writeCplx4
        generic :: write => writeCplx4
        procedure, private :: writeCplx5 => TTaggedWriter_writeCplx5
        generic :: write => writeCplx5
        procedure, private :: writeInteger0 => TTaggedWriter_writeInteger0
        generic :: write => writeInteger0
        procedure, private :: writeInteger1 => TTaggedWriter_writeInteger1
        generic :: write => writeInteger1
        procedure, private :: writeInteger2 => TTaggedWriter_writeInteger2
        generic :: write => writeInteger2
        procedure, private :: writeInteger3 => TTaggedWriter_writeInteger3
        generic :: write => writeInteger3
        procedure, private :: writeInteger4 => TTaggedWriter_writeInteger4
        generic :: write => writeInteger4
        procedure, private :: writeInteger5 => TTaggedWriter_writeInteger5
        generic :: write => writeInteger5
        procedure, private :: writeLogical0 => TTaggedWriter_writeLogical0
        generic :: write => writeLogical0
        procedure, private :: writeLogical1 => TTaggedWriter_writeLogical1
        generic :: write => writeLogical1
        procedure, private :: writeLogical2 => TTaggedWriter_writeLogical2
        generic :: write => writeLogical2
        procedure, private :: writeLogical3 => TTaggedWriter_writeLogical3
        generic :: write => writeLogical3
        procedure, private :: writeLogical4 => TTaggedWriter_writeLogical4
        generic :: write => writeLogical4
        procedure, private :: writeLogical5 => TTaggedWriter_writeLogical5
        generic :: write => writeLogical5
  end type TTaggedWriter


  !> Enumeration of the possible tag labels
  type :: TTagLabelsEnum

    !> unit cell volume (periodic)
    character(lenLabel) :: volume = 'cell_volume'

    !> final geometry
    character(lenLabel) :: endCoord = 'end_coords'

    !> excitation energies in Casida formalism
    character(lenLabel) :: excEgy = 'exc_energies_sqr'

    !> excited state force contributions
    character(lenLabel) :: excForce = 'exc_forces'

    !> oscillator strength for excitations
    character(lenLabel) :: excOsc = 'exc_oscillator'

    !> Transition dipole moments for excitations
    character(lenLabel) :: excDipole = 'exc_transdip'

    !> nonadiabatic coupling vector, H
    character(lenLabel) :: nacH = 'coupling_vectors'

    !> nonadiabatic coupling vector TD-DFTB
    character(lenLabel) :: nacv = 'nac_vectors'

    !> ground state total forces
    character(lenLabel) :: forceTot = 'forces'

    !> forces on any external charges present
    character(lenLabel) :: chrgForces = 'forces_ext_charges'

    !> Fermi level(s)
    character(lenLabel) :: fermiLvl = 'fermi_level'

    !> number of electrons
    character(lenLabel) :: nElec = 'number_of_electrons'

    !> eigenvalues/single particle states
    character(lenLabel) :: eigvals = 'eigenvalues'

    !> filling of the eigenstates
    character(lenLabel) :: eigFill = 'filling'

    !> Gibbs free energy for finite pressure periodic systems
    character(lenLabel) :: gibbsFree = 'gibbs_energy'

    !> Gross atomic charges
    character(lenLabel) :: qOutAtGross  = 'gross_atomic_charges'

    !> Charge model 5 corrected atomic gross charges
    character(lenLabel) :: qOutAtCM5 = 'cm5_atomic_charges'

    !> Gross atomic spin polarizations
    character(lenLabel) :: spinOutAtGross  = 'gross_atomic_spins'

    !> numerically calculated second derivatives matrix
    character(lenLabel) :: hessianNum = 'hessian_numerical'

    !> numerically calculated Born charges
    character(lenLabel) :: BorndDipNum = 'born_mudrv_numerical'

    !> final energy components after real-time propagation
    character(lenLabel) :: tdenergy = 'final_energy'

    !> final dipole moment vector after real-time propagation
    character(lenLabel) :: tddipole = 'final_dipole_moment'

    !> final negative gross atomic Mulliken charges after real-time propagation
    character(lenLabel) :: tdcharges = 'final_td_charges'

    !> final forces components after real-time (Ehrenfest) propagation
    character(lenLabel) :: ehrenforces = 'final_ehrenfest_forc'

    !> final geometry after real-time (Ehrenfest) propagation
    character(lenLabel) :: ehrencoords = 'final_ehrenfest_geom'

    !> final velocities after real-time (Ehrenfest) propagation
    character(lenLabel) :: ehrenvelos = 'final_ehrenfest_velo'

    !> final molecular orbitals occupations after real-time (Ehrenfest) propagation
    character(lenLabel) :: tdprojocc = 'final_td_proj_occ'

    !> Sum of bond populaion values (should be number of electrons)
    character(lenLabel) :: sumBondPopul = 'sum_bond_pops'

    !> final atom-resolved energies
    character(lenLabel) :: atomenergies = 'atomic_energies'

    !> total energy including electron TS contribution
    character(lenLabel) :: freeEgy = 'mermin_energy'

    !> Mulliken charges
    character(lenLabel) :: qOutput = 'orbital_charges'

    !> Pipek-Mezey localisation score of single particle levels
    character(lenLabel) :: pmlocalise = 'pm_localisation'

    !> total stress tensor for periodic geometries
    character(lenLabel) :: stressTot = 'stress'

    !> total tunneling vector
    character(lenLabel) :: tunn = 'total_tunneling'

    !> total projected DOS vector
    character(lenLabel) :: ldos = 'total_localdos'

    !> total bond currents
    character(lenLabel) :: localCurrents = 'local_currents'

    !> total internal energy
    character(lenLabel) :: egyTotal   = 'total_energy'

    !> total internal energy extrapolated to 0 K
    character(lenLabel) :: egy0Total   = 'extrapolated0_energy'

    !> Energy, which if differentiated gives - force
    character(lenLabel) :: egyForceRelated = 'forcerelated_energy'

    !> Internal electric field
    character(lenLabel) :: internField = 'internal_efield'

    !> External electric field
    character(lenLabel) :: externField = 'external_efield'

    !> Static electric polarizability from linear response/perturbation
    character(lenLabel) :: dmudEPerturb = 'staticPolResponse'

    !> Static gross charge (Mulliken) response from linear response/perturbation
    character(lenLabel) :: dqdEPerturb = 'staticChargeReponse'

    !> Derivatives of ground state single particle eigenvalues wrt. k
    character(lenLabel) :: dEigenDE = 'dEidEfield'

    !> Number of electrons at the Fermi energy
    character(lenLabel) :: neFermi = 'neFermi'

    !> Derivative of the Fermi energy with respect to electric field
    character(lenLabel) :: dEfdE = 'dEfdE'

    !> Derivatives of ground state single particle eigenvalues wrt. onsite potentials
    character(lenLabel) :: dEigenDVons = 'dEidVons'

    !> Derivatives of ground state single particle eigenvalues wrt. potential at an atom
    character(lenLabel) :: dEigenDV = 'dEidV'

    !> Static gross charge (Mulliken) response with respect to potential at an atom
    character(lenLabel) :: dqdV = 'dqdV'

    !> Static net charge (onsite) response with respect to potential at an atom
    character(lenLabel) :: dqnetdV = 'dqnetdV'

    !> two-electron addition/removal energies in ppRPA formalism
    character(lenLabel) :: egyppRPA = '2e_add-rem_energies'

    !> atomic masses
    character(lenLabel) :: atomMass = 'atomic_masses'

    !> Total dipole moment
    character(lenLabel) :: dipoleMoment = 'dipole_moments'

    !> Rescaled dipole moment (for example if solvated)
    character(lenLabel) :: scaledDipole = 'scaled_dipole'

    !> Atomic dipole moments
    character(lenLabel) :: dipoleAtom = 'atomic_dipole_moments'

  end type TTagLabelsEnum


  !> Enum containing the tag labels used.
  type(TTagLabelsEnum), parameter :: tagLabels = TTagLabelsEnum()


contains


  !> initialise writer
  subroutine TTaggedWriter_init(this)

    !> Instance
    type(TTaggedWriter), intent(out) :: this

    integer :: nDecDigit, nExpDigit, nChar, nField

    if (this%initialized) then
      return
    end if

    !! "-3.1234567E-123 ": nDec = 7, nExpDigit = 3, nChar = 16
    nExpDigit = ceiling(log(maxexponent(1.0_dp) / log(10.0)) / log(10.0))
    nDecDigit = precision(1.0_dp)
    nChar = nDecDigit + nExpDigit + 6
    nField = 80 / nChar
    if (nField == 0) then
      nField = 1
    end if

    write (this%formReal, "('(', I2.2, 'E', I2.2, '.', I2.2, 'E', I3.3, ')')") nField, nChar,&
        & nDecDigit, nExpDigit

    write (this%formCmplx, "('(', I2.2, '(2E', I2.2, '.', I2.2, 'E', I3.3, '))')") nField / 2,&
        & nChar, nDecDigit, nExpDigit

    nChar = digits(1) + 2
    nField = 80 / nChar
    if (nField == 0) then
      nField = 1
    end if
    write (this%formInt, "('(', I2.2, 'I', I2.2, ')')") nField, nChar
    write (this%formLogical, "('(40L2)')")

    this%initialized = .true.

  end subroutine TTaggedWriter_init



  !> Write tagged data (data type: real(dp))
  subroutine TTaggedWriter_writeReal0(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    real(dp), intent(in) :: data

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formReal)
    end if
      call writeTaggedHeader(file, tag, 'real')
    write(file, form) data

  end subroutine TTaggedWriter_writeReal0


  !> Write tagged data (data type: real(dp))
  subroutine TTaggedWriter_writeReal1(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    real(dp), intent(in) :: data(:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formReal)
    end if
      call writeTaggedHeader(file, tag, 'real', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeReal1


  !> Write tagged data (data type: real(dp))
  subroutine TTaggedWriter_writeReal2(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    real(dp), intent(in) :: data(:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formReal)
    end if
      call writeTaggedHeader(file, tag, 'real', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeReal2


  !> Write tagged data (data type: real(dp))
  subroutine TTaggedWriter_writeReal3(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    real(dp), intent(in) :: data(:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formReal)
    end if
      call writeTaggedHeader(file, tag, 'real', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeReal3


  !> Write tagged data (data type: real(dp))
  subroutine TTaggedWriter_writeReal4(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    real(dp), intent(in) :: data(:,:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formReal)
    end if
      call writeTaggedHeader(file, tag, 'real', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeReal4


  !> Write tagged data (data type: real(dp))
  subroutine TTaggedWriter_writeReal5(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    real(dp), intent(in) :: data(:,:,:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formReal)
    end if
      call writeTaggedHeader(file, tag, 'real', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeReal5


  !> Write tagged data (data type: complex(dp))
  subroutine TTaggedWriter_writeCplx0(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    complex(dp), intent(in) :: data

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formCmplx)
    end if
      call writeTaggedHeader(file, tag, 'complex')
    write(file, form) data

  end subroutine TTaggedWriter_writeCplx0


  !> Write tagged data (data type: complex(dp))
  subroutine TTaggedWriter_writeCplx1(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    complex(dp), intent(in) :: data(:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formCmplx)
    end if
      call writeTaggedHeader(file, tag, 'complex', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeCplx1


  !> Write tagged data (data type: complex(dp))
  subroutine TTaggedWriter_writeCplx2(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    complex(dp), intent(in) :: data(:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formCmplx)
    end if
      call writeTaggedHeader(file, tag, 'complex', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeCplx2


  !> Write tagged data (data type: complex(dp))
  subroutine TTaggedWriter_writeCplx3(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    complex(dp), intent(in) :: data(:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formCmplx)
    end if
      call writeTaggedHeader(file, tag, 'complex', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeCplx3


  !> Write tagged data (data type: complex(dp))
  subroutine TTaggedWriter_writeCplx4(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    complex(dp), intent(in) :: data(:,:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formCmplx)
    end if
      call writeTaggedHeader(file, tag, 'complex', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeCplx4


  !> Write tagged data (data type: complex(dp))
  subroutine TTaggedWriter_writeCplx5(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    complex(dp), intent(in) :: data(:,:,:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formCmplx)
    end if
      call writeTaggedHeader(file, tag, 'complex', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeCplx5


  !> Write tagged data (data type: integer)
  subroutine TTaggedWriter_writeInteger0(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    integer, intent(in) :: data

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formInt)
    end if
      call writeTaggedHeader(file, tag, 'integer')
    write(file, form) data

  end subroutine TTaggedWriter_writeInteger0


  !> Write tagged data (data type: integer)
  subroutine TTaggedWriter_writeInteger1(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    integer, intent(in) :: data(:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formInt)
    end if
      call writeTaggedHeader(file, tag, 'integer', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeInteger1


  !> Write tagged data (data type: integer)
  subroutine TTaggedWriter_writeInteger2(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    integer, intent(in) :: data(:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formInt)
    end if
      call writeTaggedHeader(file, tag, 'integer', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeInteger2


  !> Write tagged data (data type: integer)
  subroutine TTaggedWriter_writeInteger3(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    integer, intent(in) :: data(:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formInt)
    end if
      call writeTaggedHeader(file, tag, 'integer', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeInteger3


  !> Write tagged data (data type: integer)
  subroutine TTaggedWriter_writeInteger4(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    integer, intent(in) :: data(:,:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formInt)
    end if
      call writeTaggedHeader(file, tag, 'integer', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeInteger4


  !> Write tagged data (data type: integer)
  subroutine TTaggedWriter_writeInteger5(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    integer, intent(in) :: data(:,:,:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formInt)
    end if
      call writeTaggedHeader(file, tag, 'integer', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeInteger5


  !> Write tagged data (data type: logical)
  subroutine TTaggedWriter_writeLogical0(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    logical, intent(in) :: data

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formLogical)
    end if
      call writeTaggedHeader(file, tag, 'logical')
    write(file, form) data

  end subroutine TTaggedWriter_writeLogical0


  !> Write tagged data (data type: logical)
  subroutine TTaggedWriter_writeLogical1(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    logical, intent(in) :: data(:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formLogical)
    end if
      call writeTaggedHeader(file, tag, 'logical', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeLogical1


  !> Write tagged data (data type: logical)
  subroutine TTaggedWriter_writeLogical2(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    logical, intent(in) :: data(:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formLogical)
    end if
      call writeTaggedHeader(file, tag, 'logical', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeLogical2


  !> Write tagged data (data type: logical)
  subroutine TTaggedWriter_writeLogical3(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    logical, intent(in) :: data(:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formLogical)
    end if
      call writeTaggedHeader(file, tag, 'logical', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeLogical3


  !> Write tagged data (data type: logical)
  subroutine TTaggedWriter_writeLogical4(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    logical, intent(in) :: data(:,:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formLogical)
    end if
      call writeTaggedHeader(file, tag, 'logical', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeLogical4


  !> Write tagged data (data type: logical)
  subroutine TTaggedWriter_writeLogical5(this, file, tag, data, optForm)

    !> Instance
    class(TTaggedWriter), intent(inout) :: this

    !> File ID
    integer, intent(in) :: file

    !> tag label
    character(len=*), intent(in) :: tag

    !> data to print
    logical, intent(in) :: data(:,:,:,:,:)

    !> optional formatting string
    character(len=*), optional, intent(in) :: optForm

    character(len=20) :: form



    if (present(optForm)) then
      form = getLabel(optForm)
    else
      form = getLabel(this%formLogical)
    end if
      call writeTaggedHeader(file, tag, 'logical', shape(data))
    write(file, form) data

  end subroutine TTaggedWriter_writeLogical5



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Private functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !> Writes the tagged header.
  subroutine writeTaggedHeader(file, tag, dataType, dataShape)

    !> File id to write to
    integer, intent(in) :: file

    !> Tag name
    character(*), intent(in) :: tag

    !> Form string to use
    character(*), intent(in) :: dataType

    !> Original shape of the data
    integer, intent(in), optional :: dataShape(:)

    character(100) :: buffer

    if (present(dataShape)) then
      if (size(dataShape) == 1) then
        write(buffer, "(5A,I0,4A)") '("', getLabel(tag), ":", trim(dataType), ":", size(dataShape),&
            & ":", '",', 'I0', ')'
      else
        write(buffer, "(5A,I0,3A,I0,2A)") '("', getLabel(tag), ":", trim(dataType), ":",&
            & size(dataShape), ":", '",', 'I0,', size(dataShape) - 1, '(",",I0)', ')'
      end if
      write(file, buffer) dataShape
    else
      write(file, "(4A,I0,A)") getLabel(tag), ":", trim(dataType), ":", 0, ":"
    end if

  end subroutine writeTaggedHeader


  !> Extracts the label for a tag
  function getLabel(tag)

    !> relevant tag
    character(len=*), intent(in) :: tag

    !> Label
    character(len=20) :: getLabel

    integer :: lentrim

    lentrim = len_trim(tag)
    if (lentrim >= lenLabel) then
      getLabel(:) = tag(1:lenLabel)
    else
      getLabel(1:lentrim) = tag(1:lentrim)
      getLabel(lentrim+1:lenLabel) = " "
    end if

  end function getLabel


end module dftbp_io_taggedoutput
