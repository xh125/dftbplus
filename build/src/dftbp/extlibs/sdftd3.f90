!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

!> Proxy module for interfacing with the s-dftd3 library.
module dftbp_extlibs_sdftd3
  use dftbp_common_accuracy, only : dp
  use dftbp_common_constants, only : kcal_mol__Hartree, AA__Bohr
  use dftbp_common_environment, only : TEnvironment
  use dftbp_common_status, only : TStatus
  use dftbp_dftb_dispiface, only : TDispersionIface
  use dftbp_dftb_periodic, only : TNeighbourList, getNrOfNeighboursForAll
  use dftbp_io_message, only : error
  use dftbp_math_simplealgebra, only : determinant33
  implicit none
  private

  public :: TSDFTD3, TSDFTD3Input, TSDFTD3_init, writeSDFTD3Info, dampingFunction


  !> Possible damping functions
  type :: EnumDampingFunction

    !> Rational damping function
    integer :: rational = 1

    !> Zero damping function
    integer :: zero = 2

    !> Modified zero damping
    integer :: mzero = 3

  end type EnumDampingFunction

  !> Actual enumerator for available damping functions
  type(EnumDampingFunction), parameter :: dampingFunction = EnumDampingFunction()


  !> Input for the library
  type :: TSDFTD3Input

    !> Selected damping function
    integer :: dampingFunction

    !> Scaling parameter for C6 contributions
    real(dp) :: s6

    !> Scaling parameter for C8 contributions
    real(dp) :: s8

    !> Scaling parameter for C9 contributions
    real(dp) :: s9

    !> Scaling paramter for critical radius in rational damping function
    real(dp) :: a1

    !> Offset parameter for critical radius in rational damping function
    real(dp) :: a2

    !> Range separation parameter for zero damping function
    real(dp) :: sr6

    !> Exponent for zero damping
    real(dp) :: alpha6

    !> Offset parameter for modified zero damping
    real(dp) :: beta

    !> Real-space cutoff for two-body dispersion contributions
    real(dp) :: cutoff

    !> Real-space cutoff for coordination number evaluation
    real(dp) :: cutoffCN

    !> Atomic numbers
    integer, allocatable :: izp(:)

    !> Add hydrogen-hydrogen repulsion contribution (Why is this here?)
    logical :: hhrepulsion

  end type TSDFTD3Input


  !> Library interface handler
  type, extends(TDispersionIFace) :: TSDFTD3


    !> Dispersion energy
    real(dp) :: energy

    !> Derivative of dispersion energy w. r. t. cartesian displacements
    real(dp), allocatable :: gradient(:, :)

    !> Derivative of dispersion energy w. r. t. strain deformations
    real(dp), allocatable :: sigma(:, :)

    !> Atom is hydrogen (This does not belong here)
    logical, allocatable :: hydrogen(:)

  contains

    !> Update internal store of coordinates
    procedure :: updateCoords

    !> Update internal store of lattice vectors
    procedure :: updateLatVecs

    !> Return energy contribution
    procedure :: getEnergies

    !> Return force contribution
    procedure :: addGradients

    !> Return stress tensor contribution
    procedure :: getStress

    !> Cutoff distance in real space for dispersion
    procedure :: getRCutoff

  end type TSDFTD3


  ! Cut-off distance in Bohr for the H-H repulsion term dropping below 1E-10
  real(dp), parameter :: hhRepCutOff = 10.0_dp


contains


  !> Constructor for the library interface
  subroutine TSDFTD3_init(this, input, nAtom, species0, speciesNames, coords0, latVecs)

    !> Instance of the library interface
    type(TSDFTD3), intent(out) :: this

    !> Input to construct the library interface
    type(TSDFTD3Input), intent(in) :: input

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
  end subroutine TSDFTD3_init


  !> Notifies the objects about changed coordinates.
  subroutine updateCoords(this, env, neigh, img2CentCell, coords, species0, stat)

    !> Instance of library handler
    class(TSDFTD3), intent(inout) :: this

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> Updated neighbour list.
    type(TNeighbourList), intent(in) :: neigh

    !> Updated mapping to central cell.
    integer, intent(in) :: img2CentCell(:)

    !> Updated coordinates.
    real(dp), intent(in) :: coords(:,:)

    !> Species of the atoms in the unit cell.
    integer, intent(in) :: species0(:)

    !> Status of operation
    type(TStatus), intent(out) :: stat

    call notImplementedError

  end subroutine updateCoords


  !> Notifies the object about updated lattice vectors.
  subroutine updateLatVecs(this, latVecs)

    !> Instance of DFTD3 data
    class(TSDFTD3), intent(inout) :: this

    !> New lattice vectors
    real(dp), intent(in) :: latVecs(:,:)

    call notImplementedError
  end subroutine updatelatVecs


  !> Returns the atomic resolved energies due to the dispersion.
  subroutine getEnergies(this, energies)

    !> Instance of DFTD3 data
    class(TSDFTD3), intent(inout) :: this

    !> Contains the atomic energy contributions on exit.
    real(dp), intent(out) :: energies(:)

    call notImplementedError
  end subroutine getEnergies


  !> Adds the atomic gradients to the provided vector.
  subroutine addGradients(this, env, neigh, img2CentCell, coords, species0, &
      & gradients, stat)

    !> Instance of DFTD3 data
    class(TSDFTD3), intent(inout) :: this

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> list of neighbours to atoms
    type(TNeighbourList), intent(in) :: neigh

    !> image to central cell atom index
    integer, intent(in) :: img2CentCell(:)

    !> atomic coordinates
    real(dp), intent(in) :: coords(:,:)

    !> central cell chemical species
    integer, intent(in) :: species0(:)

    !> The vector to increase by the gradients.
    real(dp), intent(inout) :: gradients(:,:)

    !> Status of operation
    integer, intent(out), optional :: stat

    call notImplementedError
  end subroutine addGradients


  !> Get stress tensor contributions, by converting the saved strain derivatives.
  !> Calculating the stress tensor includes a sign change from the strain derivatives
  !> and a normalization with the cell volume
  subroutine getStress(this, stress)

    !> Instance of DFTD3 data
    class(TSDFTD3), intent(inout) :: this

    !> stress tensor from the dispersion
    real(dp), intent(out) :: stress(:,:)

    call notImplementedError
  end subroutine getStress


  !> Estimates the real space cutoff of the dispersion interaction.
  function getRCutoff(this) result(cutoff)

    !> Instance of DFTD3 data
    class(TSDFTD3), intent(inout) :: this

    !> Resulting cutoff
    real(dp) :: cutoff

    cutoff = merge(hhRepCutOff, 0.0_dp, allocated(this%hydrogen))
  end function getRCutoff


  !> Add the additional H-H repulsion for D3H5 (Note: this routine does not belong here...)
  subroutine addHHRepulsion(this, coords, neigh, img2CentCell)

    !> Instance of DFTD3 data
    class(TSDFTD3), intent(inout) :: this

    !> Current coordinates
    real(dp), intent(in) :: coords(:,:)

    !> Neighbour list
    type(TNeighbourList), intent(in) :: neigh

    !> Updated mapping to central cell
    integer, intent(in) :: img2CentCell(:)

    ! Parameters (as published in dx.doi.org/10.1021/ct200751e )
    real(dp), parameter :: kk = 0.30_dp * kcal_mol__Hartree  ! s_HH
    real(dp), parameter :: ee = 14.31_dp  ! e_HH
    real(dp), parameter :: r0 = 2.35_dp * AA__Bohr  ! r_0,HH

    integer :: iAt1, iNeigh, iAt2, iAt2f, ii, nAtom
    real(dp) :: rr, repE, dEdR, dCdR(3), vec(3), prefac
    integer, allocatable :: nNeigh(:)

    nAtom = size(neigh%nNeighbour)
    allocate(nNeigh(nAtom))
    call getNrOfNeighboursForAll(nNeigh, neigh, HHRepCutOff)

    repE = 0.0_dp
    do iAt1 = 1, nAtom
      if (.not.this%hydrogen(iAt1)) cycle
      do iNeigh = 1, nNeigh(iAt1)
        iAt2 = neigh%iNeighbour(iNeigh, iAt1)
        iAt2f = img2CentCell(iAt2)
        if (.not.this%hydrogen(iAt2f)) cycle
        prefac = merge(0.5_dp, 1.0_dp, iAt1 == iAt2f)
        vec(:) = coords(:,iAt1) - coords(:,iAt2)
        rr = norm2(vec)
        repE = repE + kk * (1.0_dp - 1.0_dp / (1.0_dp + exp(-ee * (rr / r0 - 1.0_dp))))
        dEdR = -kk * ee * exp(-ee * (rr / r0 -1.0_dp))&
            & / (r0 * (1.0_dp + exp(-ee * (rr / r0 - 1.0_dp)))**2)
        dCdR(:) = vec / rr
        this%gradient(:,iAt1) = this%gradient(:,iAt1) + dEdR * dCdR
        this%gradient(:,iAt2f) = this%gradient(:,iAt2f) - dEdR * dCdR

        do ii = 1, 3
          this%sigma(:, ii) = this%sigma(:, ii) + prefac * vec(ii) * dEdR * dCdR
        end do
      end do
    end do

    this%energy = this%energy + repE

  end subroutine addHHRepulsion


  !> Write information about library setup
  subroutine writeSDFTD3Info(unit, this)

    !> Formatted unit for output
    integer, intent(in) :: unit

    !> Data structure
    type(TSDFTD3), intent(in) :: this

    call notImplementedError
  end subroutine writeSDFTD3Info


  subroutine notImplementedError

    call error("DFTB+ compiled without support for s-dftd3 library")
  end subroutine notImplementedError


end module dftbp_extlibs_sdftd3
