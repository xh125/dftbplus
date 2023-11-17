!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Contains routines to calculate contributions to typical DFTB Hamiltonian parts using various
!> generalisations of H_mu,nu = 0.5*S_mu,nu*(V_mu + V_nu)
module dftbp_dftb_shift
  use dftbp_common_accuracy, only : dp
  use dftbp_common_environment, only : TEnvironment
  use dftbp_common_schedule, only : distributeRangeWithWorkload, assembleChunks
  use dftbp_type_commontypes, only : TOrbitals

  implicit none

  private
  public :: addShift, totalShift, addOnsiteShift, addAtomicMultipoleShift


  !> add shifts to a given Hamiltonian
  interface addShift
    module procedure add_shift_atom
    module procedure add_shift_lshell
    module procedure add_shift_block
  end interface addShift


  !> Totals together shifts to get composites
  interface totalShift
    module procedure addatom_shell
    module procedure addshell_block
  end interface totalShift

contains


  !> Regular atomic shift (potential is only dependent on number of atom)
  subroutine add_shift_atom(env,ham,over,nNeighbour,iNeighbour,species,orb,iPair, nAtom,img2CentCell, &
      & shift,isInputZero)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> The resulting Hamiltonian contribution.
    real(dp), intent(inout) :: ham(:,:)

    !> The overlap matrix.
    real(dp), intent(in) :: over(:)

    !> Number of neighbours surrounding each atom.
    integer, intent(in) :: nNeighbour(:)

    !> List of neighbours for each atom.
    integer, intent(in) :: iNeighbour(0:,:)

    !> List of the species of each atom.
    integer, intent(in) :: species(:)

    !> Contains Information about the atomic orbitals in the system
    type(TOrbitals), intent(in) :: orb

    !> Indexing array for the Hamiltonian.
    integer, intent(in) :: iPair(0:,:)

    !> Index mapping atoms onto the central cell atoms.
    integer, intent(in) :: nAtom

    !> Mapping from image atom to central cell
    integer, intent(in) :: img2CentCell(:)

    !> Shift to add at atom sites
    real(dp), intent(in) :: shift(:,:)

    !> Whether array 'ham' is zero everywhere on input
    logical, intent(in) :: isInputZero

    integer :: iAt1, iAt2, iAt2f, iOrig, iSp1, iSp2, nOrb1, nOrb2
    integer :: iIter, iNeigh, iSpin, nSpin
    integer, allocatable :: iterIndices(:)












    nSpin = size(ham,dim=2)


    call distributeRangeWithWorkload(env, 1, nAtom, nNeighbour, iterIndices)

    do iSpin = 1, nSpin
      do iIter = 1, size(iterIndices)
        iAt1 = iterIndices(iIter)
        iSp1 = species(iAt1)
        nOrb1 = orb%nOrbSpecies(iSp1)
        do iNeigh = 0, nNeighbour(iAt1)
          iAt2 = iNeighbour(iNeigh, iAt1)
          iAt2f = img2CentCell(iAt2)
          iSp2 = species(iAt2f)
          nOrb2 = orb%nOrbSpecies(iSp2)
          iOrig = iPair(iNeigh, iAt1)
          ham(iOrig+1:iOrig+nOrb2*nOrb1,iSpin) = &
              & ham(iOrig+1:iOrig+nOrb2*nOrb1,iSpin) + &
              & over(iOrig+1:iOrig+nOrb2*nOrb1) * 0.5_dp * &
              & ( shift(iAt1,iSpin) + shift(iAt2f,iSpin) )
        end do
      end do
    end do

    call assembleChunks(env, ham)

  end subroutine add_shift_atom


  !> l-dependent shift (potential is dependent on number of atom and l-shell)
  subroutine add_shift_lshell(env,ham,over,nNeighbour,iNeighbour,species,orb,iPair,nAtom,img2CentCell,&
      & shift,isInputZero)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> The resulting Hamiltonian contribution.
    real(dp), intent(inout) :: ham(:,:)

    !> The overlap matrix.
    real(dp), intent(in) :: over(:)

    !> Number of neighbours surrounding each atom.
    integer, intent(in) :: nNeighbour(:)

    !> List of neighbours for each atom.
    integer, intent(in) :: iNeighbour(0:,:)

    !> List of the species of each atom.
    integer, intent(in) :: species(:)

    !> Contains Information about the atomic orbitals in the system
    type(TOrbitals), intent(in) :: orb

    !> Indexing array for the Hamiltonian.
    integer, intent(in) :: iPair(0:,:)

    !> Index mapping atoms onto the central cell atoms.
    integer, intent(in) :: nAtom

    !> Mapping from image atom to central cell
    integer, intent(in) :: img2CentCell(:)

    !> Shift to add for each l-shell on all atom sites, (0:lmax,1:nAtom)
    real(dp), intent(in) :: shift(:,:,:)

    !> Whether array 'ham' is zero everywhere on input
    logical, intent(in) :: isInputZero

    integer :: iAt1, iAt2f, iOrig, iSp1, iSp2, nOrb1, nOrb2
    integer :: iSh1, iSh2, iIter, iNeigh, iSpin, nSpin
    real(dp) :: tmpH(orb%mOrb,orb%mOrb), rTmp
    integer, allocatable :: iterIndices(:)












    nSpin = size(ham,dim=2)

    call distributeRangeWithWorkload(env, 1, nAtom, nNeighbour, iterIndices)

    do iSpin = 1, nSpin
      do iIter = 1, size(iterIndices)
        iAt1 = iterIndices(iIter)
        iSp1 = species(iAt1)
        nOrb1 = orb%nOrbSpecies(iSp1)
        do iNeigh = 0, nNeighbour(iAt1)
          iAt2f = img2CentCell(iNeighbour(iNeigh, iAt1))
          iSp2 = species(iAt2f)
          nOrb2 = orb%nOrbSpecies(iSp2)
          iOrig = iPair(iNeigh, iAt1)
          do iSh1 = 1, orb%nShell(iSp1)
            rTmp = shift(iSh1, iAt1, iSpin)
            do iSh2 = 1, orb%nShell(iSp2)
              tmpH(orb%posShell(iSh2,iSp2):orb%posShell(iSh2+1,iSp2)-1, &
                  & orb%posShell(iSh1,iSp1):orb%posShell(iSh1+1,iSp1)-1) = &
                  & rTmp + shift(iSh2, iAt2f,iSpin)
            end do
          end do
          ham(iOrig+1:iOrig+nOrb2*nOrb1,iSpin) = &
              & ham(iOrig+1:iOrig+nOrb2*nOrb1,ispin) + &
              & 0.5_dp * over(iOrig+1:iOrig+nOrb2*nOrb1) &
              & * reshape(tmpH(1:nOrb2, 1:nOrb1), (/nOrb2*nOrb1/))
        end do
      end do
    end do

    call assembleChunks(env, ham)

  end subroutine add_shift_lshell


  !> shift depending on occupation-matrix like potentials. To use this for lm-dependent potentials,
  !> use a diagonal shift matrix
  subroutine add_shift_block(env,ham,over,nNeighbour,iNeighbour,species,orb,iPair,nAtom,img2CentCell, &
      & shift,isInputZero)

    !> Computational environment settings
    type(TEnvironment), intent(in) :: env

    !> The resulting Hamiltonian contribution.
    real(dp), intent(inout) :: ham(:,:)

    !> The overlap matrix.
    real(dp), intent(in) :: over(:)

    !> Number of neighbours surrounding each atom.
    integer, intent(in) :: nNeighbour(:)

    !> List of neighbours for each atom.
    integer, intent(in) :: iNeighbour(0:,:)

    !> List of the species of each atom.
    integer, intent(in) :: species(:)

    !> Contains Information about the atomic orbitals in the system
    type(TOrbitals), intent(in) :: orb

    !> Indexing array for the Hamiltonian.
    integer, intent(in) :: iPair(0:,:)

    !> Index mapping atoms onto the central cell atoms.
    integer, intent(in) :: nAtom

    !> Mapping from image atom to central cell
    integer, intent(in) :: img2CentCell(:)

    !> Shift to add at atom sites, listed as (0:nOrb,0:nOrb,1:nAtom)
    real(dp), intent(in) :: shift(:,:,:,:)

    !> Whether array 'ham' is zero everywhere on input
    logical, intent(in) :: isInputZero

    integer :: iAt1, iAt2, iAt2f, iOrig, iSp1, iSp2, nOrb1, nOrb2
    integer :: iIter, iNeigh, iSpin, nSpin
    real(dp) :: tmpH(orb%mOrb,orb%mOrb), tmpS(orb%mOrb,orb%mOrb)
    integer, allocatable :: iterIndices(:)












    nSpin = size(ham,dim=2)

    if (isInputZero) then
      call distributeRangeWithWorkload(env, 1, nAtom, nNeighbour, iterIndices)
    else
      !> If input is not zero everywhere, we have to compute in serial here, otherwise
      !> the call of 'assembleChunks' will mess up the array.
      allocate(iterIndices(nAtom))
      iterIndices(:) = [(iIter, iIter = 1, nAtom)]
    end if

    do iSpin = 1, nSpin
      do iIter = 1, size(iterIndices)
        iAt1 = iterIndices(iIter)
        iSp1 = species(iAt1)
        nOrb1 = orb%nOrbSpecies(iSp1)
        do iNeigh = 0, nNeighbour(iAt1)
          iAt2 = iNeighbour(iNeigh, iAt1)
          iAt2f = img2CentCell(iAt2)
          iSp2 = species(iAt2f)
          nOrb2 = orb%nOrbSpecies(iSp2)
          iOrig = iPair(iNeigh, iAt1)
          tmpS(1:nOrb2,1:nOrb1) = reshape( &
              & over(iOrig+1:iOrig+nOrb2*nOrb1),(/nOrb2,nOrb1/) )
          tmpH(1:nOrb2,1:nOrb1) = 0.5_dp * ( &
              & matmul(tmpS(1:nOrb2,1:nOrb1), &
              & shift(1:nOrb1,1:nOrb1,iAt1,iSpin)) + &
              & matmul(shift(1:nOrb2,1:nOrb2,iAt2f,iSpin), &
              & tmpS(1:nOrb2,1:nOrb1)) )
          ham(iOrig+1:iOrig+nOrb2*nOrb1,iSpin) = &
              & ham(iOrig+1:iOrig+nOrb2*nOrb1,iSpin) + &
              & reshape(tmpH(1:nOrb2, 1:nOrb1), (/nOrb2*nOrb1/))
        end do
      end do
    end do

    if (isInputZero) then
      call assembleChunks(env, ham)
    end if

  end subroutine add_shift_block


  !> Add a shift for atom resolved potetial to shell resolved potential
  subroutine addatom_shell(shiftshell, atom, orb, species)

    !> Shift to add at atomic shells
    real(dp), intent(inout) :: shiftshell(:,:,:)

    !> Atomic part of shift
    real(dp), intent(in) :: atom(:,:)

    !> Contains Information about the atomic orbitals in the system
    type(TOrbitals), intent(in) :: orb

    !> List of the species of each atom.
    integer, intent(in) :: species(:)

    integer iAtom, iSpin, nAtom, nSpin

    nAtom = size(atom, dim=1)
    nSpin = size(atom, dim=2)






    do iSpin = 1, nSpin
      do iAtom = 1, nAtom
        shiftshell(1:orb%nShell(species(iAtom)),iAtom,iSpin) = &
            & shiftshell(1:orb%nShell(species(iAtom)),iAtom,iSpin) &
            & + atom(iAtom,iSpin)
      end do
    end do

  end subroutine addatom_shell


  !> Add a shift for shell resolved potetial to block resolved potential
  subroutine addshell_block(shiftblock, shell, orb, species)

    !> block resolved shift
    real(dp), intent(inout) :: shiftblock(:,:,:,:)

    !> Shell shift to add in
    real(dp), intent(in) :: shell(:,:,:)

    !> Contains Information about the atomic orbitals in the system
    type(TOrbitals), intent(in) :: orb

    !> List of the species of each atom.
    integer, intent(in) :: species(:)

    integer iAt, iSpin, nAtom, nSpin, iSh, iSp, iOrb

    nAtom = size(shiftblock, dim=3)
    nSpin = size(shiftblock, dim=4)








    do iSpin = 1, nSpin
      do iAt = 1, nAtom
        iSp = species(iAt)
        do iSh = 1, orb%nShell(iSp)
          do iOrb = orb%posShell(iSh,iSp),orb%posShell(iSh+1,iSp)-1
            shiftblock(iOrb,iOrb,iAt,iSpin) = shiftblock(iOrb,iOrb,iAt,iSpin)&
                & + shell(iSh,iAt,iSpin)
          end do
        end do
      end do
    end do

  end subroutine addshell_block


  !> Add on-site only atomic shift (potential is not only dependent on overlap, only the number of
  !> each atom in the structure)
  subroutine addOnsiteShift(ham, over, species, orb, iPair, nAtom, shift)

    !> The Hamiltonian to add the contribution
    real(dp), intent(inout) :: ham(:,:)

    !> The overlap matrix
    real(dp), intent(in) :: over(:)

    !> List of the species of each atom
    integer, intent(in) :: species(:)

    !> Contains Information about the atomic orbitals in the system
    type(TOrbitals), intent(in) :: orb

    !> Indexing array for the Hamiltonian
    integer, intent(in) :: iPair(0:,:)

    !> Index mapping atoms onto the central cell atoms
    integer, intent(in) :: nAtom

    !> Atom resolved potential
    real(dp), intent(in) :: shift(:,:)

    integer :: iAt, iOrig, iSp, nOrb, iSpin

    do iSpin = 1, size(ham,dim=2)
      do iAt = 1, nAtom
        iSp = species(iAt)
        nOrb = orb%nOrbSpecies(iSp)
        iOrig = iPair(0, iAt)
        ham(iOrig+1:iOrig+nOrb*nOrb,iSpin) = ham(iOrig+1:iOrig+nOrb*nOrb,iSpin)&
            & + over(iOrig+1:iOrig+nOrb*nOrb) * shift(iAt,iSpin)
      end do
    end do

  end subroutine addOnsiteShift


  subroutine addAtomicMultipoleShift(ham, mpintBra, mpintKet, nNeighbour, iNeighbour, &
      & species, orb, iPair, nAtom, img2CentCell, shift)

    !> The resulting Hamiltonian contribution.
    real(dp), intent(inout) :: ham(:,:)

    !> Multipole integrals <|
    real(dp), intent(in) :: mpintBra(:, :)

    !> Multipole integrals |>
    real(dp), intent(in) :: mpintKet(:, :)

    !> Number of neighbours surrounding each atom.
    integer, intent(in) :: nNeighbour(:)

    !> List of neighbours for each atom.
    integer, intent(in) :: iNeighbour(0:,:)

    !> List of the species of each atom.
    integer, intent(in) :: species(:)

    !> Contains Information about the atomic orbitals in the system
    type(TOrbitals), intent(in) :: orb

    !> Indexing array for the Hamiltonian.
    integer, intent(in) :: iPair(0:,:)

    !> Number of atoms
    integer, intent(in) :: nAtom

    !> Index mapping atoms onto the central cell atoms.
    integer, intent(in) :: img2CentCell(:)

    !> Shift to add at atom sites, listed as (,1:nAtom)
    real(dp), intent(in) :: shift(:,:)

    integer :: iAt1, iAt2, img, ind, nBlk, iBlk, iSp1, iSp2, iOrb1, iOrb2, iNeigh











    do iAt1 = 1, nAtom
      iSp1 = species(iAt1)
      do iNeigh = 0, nNeighbour(iAt1)
        img = iNeighbour(iNeigh, iAt1)
        iAt2 = img2CentCell(img)
        iSp2 = species(iAt2)
        ind = iPair(iNeigh, iAt1)
        nBlk = orb%nOrbSpecies(iSp2)

        do iOrb1 = 1, orb%nOrbSpecies(iSp1)
          do iOrb2 = 1, nBlk
            iBlk = ind + iOrb2 + nBlk*(iOrb1-1)
            ham(iBlk, 1) = ham(iBlk, 1) &
              & + 0.5_dp * dot_product(mpintKet(:, iBlk), shift(:, iAt1)) &
              & + 0.5_dp * dot_product(mpintBra(:, iBlk), shift(:, iAt2))
          end do
        end do
      end do
    end do
  end subroutine addAtomicMultipoleShift


end module dftbp_dftb_shift
