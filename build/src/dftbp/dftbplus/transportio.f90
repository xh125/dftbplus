!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


module dftbp_dftbplus_transportio
  use dftbp_common_accuracy, only : dp, lc
  use dftbp_common_constants, only : Hartree__eV
  use dftbp_common_file, only : TFileDescr, openFile, closeFile, fileExists
  use dftbp_common_globalenv, only : stdOut
  use dftbp_io_message, only : error
  use dftbp_type_orbitals, only : TOrbitals
  implicit none

  private
  public :: writeShifts, readShifts, writeContactShifts
  public :: readContactShifts

  integer, parameter :: contactFormatVersion = 2

  character(len=*), parameter :: formatFermiWrite = "(1X,A,T20,F22.18,1X,A,F22.18,1X,A)"
  character(len=*), parameter :: formatFermiRead = "(T20, F22.18)"

contains

  !> Write the Hamiltonian self consistent shifts to file
  subroutine writeShifts(fShifts, orb, shiftPerL)

    !> filename where shifts are stored
    character(*), intent(in) :: fShifts

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> shifts organized per (shell , atom,  spin)
    real(dp), intent(in) :: shiftPerL(:,:,:)

    type(TFileDescr) :: fdHS
    integer :: nSpin, nAtom, ii, jj

    nSpin = size(shiftPerL, dim=3)
    nAtom = size(shiftPerL, dim=2)

    if (size(shiftPerL, dim=1) /= orb%mShell ) then
      call error("Internal error in writeshift: size(shiftPerL,1)")
    endif

    if (size(shiftPerL, dim=2) /= size(orb%nOrbAtom) ) then
      call error("Internal error in writeshift size(shiftPerL,2)")
    endif

    call openFile(fdHS, fShifts, mode="w")
    write(fdHS%unit, *) nAtom, orb%mShell, orb%mOrb, nSpin
    do ii = 1, nAtom
      write(fdHS%unit, *) orb%nOrbAtom(ii), (shiftPerL(:,ii,jj), jj = 1, nSpin)
    end do
    call closeFile(fdHS)

    write(stdOut,*) ">> Shifts saved for restart in shifts.dat"

  end subroutine writeShifts


  !> Read the Hamiltonian potential shifts from file
  subroutine readShifts(fShifts, orb, nAtom, nSpin, shiftPerL)

    !> filename where shifts are stored
    character(*), intent(in) :: fShifts

    !> orbital information
    type(TOrbitals), intent(in) :: orb

    !> number of atoms and spin blocks
    integer, intent(in) :: nAtom, nSpin

    !> potential shifts (shell,atom,spin) charge/mag is used
    real(dp), intent(inout) :: shiftPerL(:,:,:)

    type(TFileDescr) :: fdH
    integer :: nAtomSt, nSpinSt, mOrbSt, mShellSt, ii, jj
    integer, allocatable :: nOrbAtom(:)

    shiftPerL(:,:,:) = 0.0_dp

    call openFile(fdH, fShifts, mode="r")
    read(fdH%unit, *) nAtomSt, mShellSt, mOrbSt, nSpinSt

    if (nAtomSt /= nAtom .or. mShellSt /= orb%mShell .or. mOrbSt /= orb%mOrb) then
      call error("Shift upload error: Mismatch in number of atoms or max shell per atom.")
    end if
    if (nSpin /= nSpinSt) then
      call error("Shift upload error: Mismatch in number of spin channels.")
    end if

    allocate(nOrbAtom(nAtomSt))
    do ii = 1, nAtom
      read(fdH%unit, *) nOrbAtom(ii), (shiftPerL(:,ii,jj), jj = 1, nSpin)
    end do

    call closeFile(fdH)

    if (any(nOrbAtom /= orb%nOrbAtom)) then
      call error("Incompatible orbitals in the upload file!")
    end if

  end subroutine readShifts


  !> Writes the contact potential shifts per shell (for transport)
  subroutine writeContactShifts(filename, orb, shiftPerL, charges, Ef, blockCharges, tWriteAscii)

    !> filename where shifts are written
    character(*), intent(in) :: filename

    !> orbital structure
    type(TOrbitals), intent(in) :: orb

    !> array of shifts per shell and spin, only the charge related part is written to disc
    real(dp), intent(in) :: shiftPerL(:,:,:)

    !> array of charges per shell and spin
    real(dp), intent(in) :: charges(:,:,:)

    !> Fermi level
    real(dp), intent(in) :: Ef(:)

    !> block charge populations
    real(dp), allocatable, intent(in) :: blockCharges(:,:,:,:)

    !> Should a text or binary file be saved
    logical, intent(in), optional :: tWriteAscii

    type(TFileDescr) :: fdHS
    integer :: nAtom, nSpin, iAt, iSp
    logical :: tAsciiFile

    nSpin = size(charges, dim=3)
    nAtom = size(charges, dim=2)

    tAsciiFile = .true.
    if (present(tWriteAscii)) then
      tAsciiFile = tWriteAscii
    end if

    if (tAsciiFile) then

      call openFile(fdHS, file="shiftcont_" // trim(filename) // ".dat", mode="w")

      ! now with a version number on the top of the file:
      write(fdHS%unit, *) contactFormatVersion

      write(fdHS%unit, *) nAtom, orb%mShell, orb%mOrb, nSpin, allocated(blockCharges)
      write(fdHS%unit, *) orb%nOrbAtom
      write(fdHS%unit, *) shiftPerL(:,:,1)
      write(fdHS%unit, *) charges

      if (allocated(blockCharges)) then
        do iSp = 1, nSpin
          do iAt = 1, nAtom
            write(fdHS%unit, *) blockCharges(:orb%nOrbAtom(iAt), :orb%nOrbAtom(iAt), iAt, iSp)
          end do
        end do
      end if

      if (nSpin == 2) then
        write(fdHS%unit, formatFermiWrite) 'Fermi level (up):', Ef(1), "H", Hartree__eV * Ef(1),&
            & 'eV'
        write(fdHS%unit, formatFermiWrite) 'Fermi level (down):', Ef(2), "H", Hartree__eV * Ef(2),&
            & 'eV'
      else
        write(fdHS%unit, formatFermiWrite) 'Fermi level :', Ef(1), "H", Hartree__eV * Ef(1), 'eV'
      end if

      write(stdOut,*) 'shiftcont_' // trim(filename) // '.dat written to file'

    else

      call openFile(fdHS, "shiftcont_" // trim(filename) // ".bin", mode="wb")

      ! now with a version number on the top of the file:
      write(fdHS%unit) contactFormatVersion

      write(fdHS%unit) nAtom, orb%mShell, orb%mOrb, nSpin, allocated(blockCharges)
      write(fdHS%unit) orb%nOrbAtom
      write(fdHS%unit) shiftPerL(:,:,1)
      write(fdHS%unit) charges

      if (allocated(blockCharges)) then
        do iSp = 1, nSpin
          do iAt = 1, nAtom
            write(fdHS%unit) blockCharges(:orb%nOrbAtom(iAt), :orb%nOrbAtom(iAt), iAt, iSp)
          end do
        end do
      end if

      write(fdHS%unit) Ef(:)

      write(stdOut,*) 'shiftcont_' // trim(filename) // '.bin written to file'

    end if

    call closeFile(fdHS)


  end subroutine writeContactShifts



  subroutine readContactShifts()
  end subroutine readContactShifts


end module dftbp_dftbplus_transportio
