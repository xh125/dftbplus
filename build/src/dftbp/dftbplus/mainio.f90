!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!



!> Various I/O routines for the main program.
module dftbp_dftbplus_mainio
  use dftbp_common_accuracy, only : dp, mc, sc, lc
  use dftbp_common_constants, only : Hartree__eV, Bohr__AA, au__pascal, au__V_m, au__fs, au__Debye,&
      & Boltzmann, gfac, spinName, quaternionName
  use dftbp_common_environment, only : TEnvironment
  use dftbp_common_file, only : TFileDescr, openFile, closeFile
  use dftbp_common_globalenv, only : stdOut, destructGlobalEnv, abortProgram
  use dftbp_common_status, only : TStatus
  use dftbp_dftb_determinants, only : TDftbDeterminants
  use dftbp_dftb_dispersions, only : TDispersionIface
  use dftbp_dftb_elstatpot, only : TElStatPotentials
  use dftbp_dftb_energytypes, only : TEnergies
  use dftbp_dftb_extfields, only : TEField
  use dftbp_dftb_periodic, only : TNeighbourList
  use dftbp_dftb_sccinit, only : writeQToFile
  use dftbp_dftb_sparse2dense, only : unpackHS, unpackSPauli
  use dftbp_dftb_spin, only : qm2ud
  use dftbp_elecsolvers_elecsolvers, only : TElectronicSolver, electronicSolverTypes
  use dftbp_extlibs_xmlf90, only : xmlf_t, xml_OpenFile, xml_ADDXMLDeclaration, xml_NewElement,&
      & xml_EndElement, xml_Close
  use dftbp_io_charmanip, only : i2c
  use dftbp_io_commonformats, only : formatHessian, formatBorn, formatdBorn, formatGeoOut,&
      & format1U, format2U, format1Ue, format2Ue, format1U1e
  use dftbp_io_formatout, only : writeXYZFormat, writeGenFormat, writeSparse, writeSparseAsSquare
  use dftbp_io_hsdutils, only : writeChildValue
  use dftbp_io_message, only : error, warning
  use dftbp_io_taggedoutput, only : TTaggedWriter, tagLabels
  use dftbp_math_blasroutines, only : hemv
  use dftbp_math_eigensolver, only : heev
  use dftbp_md_mdintegrator, only : TMdIntegrator, state
  use dftbp_reks_reks, only : TReksCalc, reksTypes, setReksTargetEnergy
  use dftbp_solvation_cm5, only : TChargeModel5
  use dftbp_solvation_cosmo, only : TCosmo
  use dftbp_solvation_fieldscaling, only : TScaleExtEField
  use dftbp_solvation_solvation, only : TSolvation
  use dftbp_type_commontypes, only : TOrbitals, TParallelKS
  use dftbp_type_densedescr, only : TDenseDescr
  use dftbp_type_linkedlist, only : TListCharLc, TListIntR1, len, get, elemShape, intoArray
  use dftbp_type_multipole, only : TMultipole
  use dftbp_type_orbitals, only : orbitalNames, getShellNames
  use dftbp_extlibs_mpifx, only : mpifx_recv, mpifx_send, mpifx_bcast
  use dftbp_dftb_sparse2dense, only :unpackSPauliBlacs, unpackHSCplxBlacs, unpackHSRealBlacs
  use dftbp_extlibs_scalapackfx, only : linecomm, pblasfx_phemm, pblasfx_psymm
  implicit none

  private
  public :: writeEigenvectors, writeRealEigvecs, writeCplxEigvecs
  public :: writeRealEigvecsBinBlacs, writeRealEigvecsTxtBlacs
  public :: writeCplxEigvecsBinBlacs, writeCplxEigvecsTxtBlacs
  public :: writeProjectedEigenvectors
  public :: writeAutotestTag, writeResultsTag, writeDetailedXml, writeBandOut
  public :: writeDerivBandOut, writeHessianOut, writeBornChargesOut, writeBornDerivs
  public :: openOutputFile
  public :: writeDetailedOut1, writeDetailedOut2, writeDetailedOut2Dets, writeDetailedOut3
  public :: writeDetailedOut4, writeDetailedOut5, writeDetailedOut6, writeDetailedOut7
  public :: writeDetailedOut8, writeDetailedOut9, writeDetailedOut10
  public :: writeMdOut1, writeMdOut2
  public :: writeCharges
  public :: writeEsp
  public :: writeCurrentGeometry, writeFinalDriverStatus
  public :: writeHSAndStop, writeHS
  public :: printGeoStepInfo, printSccHeader, printSccInfo, printEnergies, printVolume
  public :: printPressureAndFreeEnergy, printMaxForce, printMaxLatticeForce
  public :: printForceNorm, printLatticeForceNorm
  public :: printMdInfo, printBlankLine
  public :: printReksSccHeader, printReksSccInfo
  public :: writeReksDetailedOut1
  public :: readEigenvecs
  public :: writeCosmoFile

  !> Ground state eigenvectors in text format
  character(*), parameter :: eigvecOut = "eigenvec.out"

  !> Ground state eigenvectors in binary format
  character(*), parameter :: eigvecBin = "eigenvec.bin"

  !> Cosmo file name
  character(len=*), parameter :: cosmoFile = "dftbp.cosmo"


  interface readEigenvecs
    module procedure readRealEigenvecs
    module procedure readCplxEigenvecs
  end interface readEigenvecs

  !> Derivatives of eigenvalues with respect to perturbations
  interface writeDerivBandOut
    module procedure writeDBandCart
    module procedure writeDBand
  end interface writeDerivBandOut

contains

  !> Writes the eigenvectors to disc.
  subroutine writeEigenvectors(env, runId, neighbourList, nNeighbourSK, cellVec, iCellVec,&
      & denseDesc, iPair, img2CentCell, species, speciesName, orb, kPoint, over, parallelKS,&
      & tPrintEigvecsTxt, eigvecsReal, SSqrReal, eigvecsCplx, SSqrCplx)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Job ID for future identification
    integer, intent(in) :: runId

    !> list of neighbours for each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> map from image atoms to the original unique atom
    integer, intent(in) :: img2CentCell(:)

    !> Index of start of atom blocks in dense matrices
    type(TDenseDescr), intent(in) :: denseDesc

    !> Index array for the start of atomic blocks in sparse arrays
    integer, intent(in) :: iPair(:,:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> species of all atoms in the system
    integer, intent(in) :: species(:)

    !> label for each atomic chemical species
    character(*), intent(in) :: speciesName(:)

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> k-points
    real(dp), intent(in) :: kPoint(:,:)

    !> sparse overlap matrix
    real(dp), intent(in) :: over(:)

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Whether eigenvectors should be also written in text form
    logical, intent(in) :: tPrintEigvecsTxt

    !> Real eigenvectors (will be overwritten)
    real(dp), intent(inout), allocatable :: eigvecsReal(:,:,:)

    !> Storage for dense real overlap matrix
    real(dp), intent(inout), allocatable :: SSqrReal(:,:)

    !> Complex eigenvectors (will be overwritten)
    complex(dp), intent(inout), allocatable :: eigvecsCplx(:,:,:)

    !> Storage for dense complex overlap matrix
    complex(dp), intent(inout), allocatable :: SSqrCplx(:,:)




    if (allocated(eigvecsCplx)) then
      call writeCplxEigvecs(env, runId, neighbourList, nNeighbourSK, cellVec, iCellVec, denseDesc,&
          & iPair, img2CentCell, species, speciesName, orb, kPoint, over, parallelKS,&
          & tPrintEigvecsTxt, eigvecsCplx, SSqrCplx)
    else
      call writeRealEigvecs(env, runId, neighbourList, nNeighbourSK, denseDesc, iPair,&
          & img2CentCell, species, speciesName, orb, over, parallelKS, tPrintEigvecsTxt,&
          & eigvecsReal, SSqrReal)
    end if

  end subroutine writeEigenvectors


  !> Writes real eigenvectors
  subroutine writeRealEigvecs(env, runId, neighbourList, nNeighbourSK, denseDesc, iPair,&
      & img2CentCell, species, speciesName, orb, over, parallelKS, tPrintEigvecsTxt, eigvecsReal,&
      & SSqrReal, fileName)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Job ID for future identification
    integer, intent(in) :: runId

    !> list of neighbours for each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Index of start of atom blocks in dense matrices
    type(TDenseDescr), intent(in) :: denseDesc

    !> Index array for the start of atomic blocks in sparse arrays
    integer, intent(in) :: iPair(:,:)

    !> map from image atoms to the original unique atom
    integer, intent(in) :: img2CentCell(:)

    !> species of all atoms in the system
    integer, intent(in) :: species(:)

    !> label for each atomic chemical species
    character(*), intent(in) :: speciesName(:)

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> sparse overlap matrix
    real(dp), intent(in) :: over(:)

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Whether eigenvectors should be also written in text form
    logical, intent(in) :: tPrintEigvecsTxt

    !> Real eigenvectors (will be overwritten)
    real(dp), intent(inout) :: eigvecsReal(:,:,:)

    !> Storage for dense real overlap matrix
    real(dp), intent(inout) :: SSqrReal(:,:)

    !> optional alternative file prefix, to appear as "fileName".bin or "fileName".out
    character(len=*), intent(in), optional :: fileName

    call writeRealEigvecsBinBlacs(env, denseDesc, eigvecsReal, runId, parallelKS, fileName=fileName)
    if (tPrintEigvecsTxt) then
      call writeRealEigvecsTxtBlacs(env, denseDesc, eigvecsReal, parallelKS, orb, over,&
          & neighbourList%iNeighbour, nNeighbourSK, iPair, img2CentCell, species, speciesName,&
          & fileName=fileName)
    end if

  end subroutine writeRealEigvecs


  !> Writes complex eigenvectors.
  subroutine writeCplxEigvecs(env, runId, neighbourList, nNeighbourSK, cellVec, iCellVec,&
      & denseDesc, iPair, img2CentCell, species, speciesName, orb, kPoint, over, parallelKS,&
      & tPrintEigvecsTxt, eigvecsCplx, SSqrCplx, fileName)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Job ID for future identification
    integer, intent(in) :: runId

    !> list of neighbours for each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Index of start of atom blocks in dense matrices
    type(TDenseDescr), intent(in) :: denseDesc

    !> Index array for the start of atomic blocks in sparse arrays
    integer, intent(in) :: iPair(:,:)

    !> map from image atoms to the original unique atom
    integer, intent(in) :: img2CentCell(:)

    !> species of all atoms in the system
    integer, intent(in) :: species(:)

    !> label for each atomic chemical species
    character(*), intent(in) :: speciesName(:)

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> k-points
    real(dp), intent(in) :: kPoint(:,:)

    !> sparse overlap matrix
    real(dp), intent(in) :: over(:)

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Whether eigenvectors should be also written in text form
    logical, intent(in) :: tPrintEigvecsTxt

    !> Complex eigenvectors (will be overwritten)
    complex(dp), intent(inout) :: eigvecsCplx(:,:,:)

    !> Storage for dense complex overlap matrix
    complex(dp), intent(inout) :: SSqrCplx(:,:)

    !> optional alternative file prefix, to appear as "fileName".bin or "fileName".out
    character(len=*), intent(in), optional :: fileName

    call writeCplxEigvecsBinBlacs(env, denseDesc, eigvecsCplx, runId, parallelKS, fileName=fileName)
    if (tPrintEigvecsTxt) then
      if (denseDesc%t2Component) then
        call writePauliEigvecsTxtBlacs(env, denseDesc, eigvecsCplx, parallelKS, orb, over, kPoint,&
            & neighbourList%iNeighbour, nNeighbourSK, iCellVec, cellVec, iPair, img2CentCell,&
            & species, speciesName, fileName=fileName)
      else
        call writeCplxEigvecsTxtBlacs(env, denseDesc, eigvecsCplx, parallelKS, orb, over, kPoint,&
            & neighbourList%iNeighbour, nNeighbourSK, iCellVec, cellVec, iPair, img2CentCell,&
            & species, speciesName, fileName=fileName)
      end if
    end if

  end subroutine writeCplxEigvecs




  !> Write the real eigvectors into binary output file (BLACS version).
  subroutine writeCplxEigvecsBinBlacs(env, denseDesc, eigvecs, runId, parallelKS, fileName)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Square Hamiltonian (or work array)
    complex(dp), intent(in) :: eigvecs(:,:,:)

    !> Id of the current program run.
    integer, intent(in) :: runId

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> optional alternative file prefix, to appear as "fileName".bin
    character(len=*), intent(in), optional :: fileName

    type(linecomm) :: collector
    type(TFileDescr) :: fd
    complex(dp), allocatable :: localEigvec(:)
    integer :: nOrb
    integer :: iKS, iGroup, iEig

    nOrb = denseDesc%fullSize
    allocate(localEigvec(nOrb))

    if (env%mpi%tGlobalLead) then
      call createEigvecFileBin(fd, runId, fileName)
    end if
    call collector%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")

    ! The lead process collects in the first run (iGroup = 0) the columns of the matrix in its own
    ! process group (as process group lead) via the collector. In the subsequent runs it just
    ! receives the columns collected by the respective group leaders. The number of available
    ! matrices (possible k and s indices) may differ for various process groups. Also note, that the
    ! (k, s) pairs are round-robin distributed between the process groups.

    leadOrFollow: if (env%mpi%tGlobalLead) then
      do iKS = 1, parallelKS%maxGroupKS
        group: do iGroup = 0, env%mpi%nGroup - 1
          if (iKS > parallelKS%nGroupKS(iGroup)) then
            cycle group
          end if
          do iEig = 1, nOrb
            if (iGroup == 0) then
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                  & localEigvec)
            else
              call mpifx_recv(env%mpi%interGroupComm, localEigvec, iGroup)
            end if
            write(fd%unit) localEigvec
          end do
        end do group
      end do
      call closeFile(fd)
    else
      do iKS = 1, parallelKS%nLocalKS
        do iEig = 1, nOrb
          if (env%mpi%tGroupLead) then
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                & localEigvec)
            call mpifx_send(env%mpi%interGroupComm, localEigvec, env%mpi%interGroupComm%leadrank)
          else
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS))
          end if
        end do
      end do
    end if leadOrFollow

  end subroutine writeCplxEigvecsBinBlacs




  !> Write the real eigvectors into binary output file (BLACS version).
  subroutine writeRealEigvecsBinBlacs(env, denseDesc, eigvecs, runId, parallelKS, fileName)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Square Hamiltonian (or work array)
    real(dp), intent(in) :: eigvecs(:,:,:)

    !> Id of the current program run.
    integer, intent(in) :: runId

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> optional alternative file prefix, to appear as "fileName".bin
    character(len=*), intent(in), optional :: fileName

    type(linecomm) :: collector
    type(TFileDescr) :: fd
    real(dp), allocatable :: localEigvec(:)
    integer :: nOrb
    integer :: iKS, iGroup, iEig

    nOrb = denseDesc%fullSize
    allocate(localEigvec(nOrb))

    if (env%mpi%tGlobalLead) then
      call createEigvecFileBin(fd, runId, fileName)
    end if
    call collector%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")

    ! The lead process collects in the first run (iGroup = 0) the columns of the matrix in its own
    ! process group (as process group lead) via the collector. In the subsequent runs it just
    ! receives the columns collected by the respective group leaders. The number of available
    ! matrices (possible k and s indices) may differ for various process groups. Also note, that the
    ! (k, s) pairs are round-robin distributed between the process groups.

    leadOrFollow: if (env%mpi%tGlobalLead) then
      do iKS = 1, parallelKS%maxGroupKS
        group: do iGroup = 0, env%mpi%nGroup - 1
          if (iKS > parallelKS%nGroupKS(iGroup)) then
            cycle group
          end if
          do iEig = 1, nOrb
            if (iGroup == 0) then
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                  & localEigvec)
            else
              call mpifx_recv(env%mpi%interGroupComm, localEigvec, iGroup)
            end if
            write(fd%unit) localEigvec
          end do
        end do group
      end do
      call closeFile(fd)
    else
      do iKS = 1, parallelKS%nLocalKS
        do iEig = 1, nOrb
          if (env%mpi%tGroupLead) then
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                & localEigvec)
            call mpifx_send(env%mpi%interGroupComm, localEigvec, env%mpi%interGroupComm%leadrank)
          else
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS))
          end if
        end do
      end do
    end if leadOrFollow

  end subroutine writeRealEigvecsBinBlacs





  !> Write the real eigvectors into human readible output file (BLACS version).
  subroutine writeRealEigvecsTxtBlacs(env, denseDesc, eigvecs, parallelKS, orb, over, iNeighbour,&
      & nNeighbourSK, iSparseStart, img2CentCell, species, speciesName, fileName)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Square Hamiltonian (or work array)
    real(dp), intent(in) :: eigvecs(:,:,:)

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Orbital information
    type(TOrbitals), intent(in) :: orb

    !> Sparse overlap
    real(dp), intent(in) :: over(:)

    !> Neighbours of each atom
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom
    integer, intent(in) :: nNeighbourSK(:)

    !> Index array for sparse matrices
    integer, intent(in) :: iSparseStart(:,:)

    !> Mapping of atoms into the central cell.
    integer, intent(in) :: img2CentCell(:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> Name of each species
    character(*), intent(in) :: speciesName(:)

    !> optional alternative file prefix, to appear as "fileName".bin
    character(len=*), intent(in), optional :: fileName

    type(linecomm) :: collector
    real(dp), allocatable :: localEigvec(:), localFrac(:)
    real(dp), allocatable :: globalS(:,:), globalFrac(:,:)
    integer :: nOrb, nAtom
    integer :: iKS, iS, iGroup, iEig
    type(TFileDescr) :: fd

    nOrb = denseDesc%fullSize
    nAtom = size(nNeighbourSK)
    allocate(globalS(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    allocate(globalFrac(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    if (env%mpi%tGroupLead) then
      allocate(localEigvec(nOrb))
      allocate(localFrac(nOrb))
    end if

    call collector%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")
    if (env%mpi%tGlobalLead) then
      call prepareEigvecFileTxt(fd, .false., fileName)
    end if

    ! See comment about algorithm in routine writeRealEigvecsBinBlacs

    leadOrFollow: if (env%mpi%tGlobalLead) then
      ! Global lead process
      do iKS = 1, parallelKS%maxGroupKS
        group: do iGroup = 0, env%mpi%nGroup - 1
          if (iKS > parallelKS%nGroupKS(iGroup)) then
            cycle group
          end if
          iS = parallelKS%groupKS(2, iKS, iGroup)
          if (iGroup == 0) then
            call unpackHSRealBlacs(env%blacs, over, iNeighbour, nNeighbourSK, iSparseStart,&
                & img2CentCell, denseDesc, globalS)
            call pblasfx_psymm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
                & denseDesc%blacsOrbSqr, globalFrac, denseDesc%blacsOrbSqr)
            globalFrac(:,:) = globalFrac * eigvecs(:,:,iKS)
          end if
          do iEig = 1, nOrb
            if (iGroup == 0) then
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                  & localEigvec)
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalFrac, localFrac)
            else
              call mpifx_recv(env%mpi%interGroupComm, localEigvec, iGroup)
              call mpifx_recv(env%mpi%interGroupComm, localFrac, iGroup)
            end if
            call writeSingleRealEigvecTxt(fd, localEigvec, localFrac, iS, iEig, orb, species,&
                & speciesName, nAtom)
          end do
        end do group
      end do
    else
      ! All processes except the global lead process
      do iKS = 1, parallelKS%nLocalKS
        call unpackHSRealBlacs(env%blacs, over, iNeighbour, nNeighbourSK, iSparseStart,&
            & img2CentCell, denseDesc, globalS)
        call pblasfx_psymm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS), denseDesc%blacsOrbSqr,&
            & globalFrac, denseDesc%blacsOrbSqr)
        globalFrac(:,:) = globalFrac * eigvecs(:,:,iKS)
        do iEig = 1, nOrb
          if (env%mpi%tGroupLead) then
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                & localEigvec)
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalFrac, localFrac)
            call mpifx_send(env%mpi%interGroupComm, localEigvec, env%mpi%interGroupComm%leadrank)
            call mpifx_send(env%mpi%interGroupComm, localFrac, env%mpi%interGroupComm%leadrank)
          else
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS))
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, globalFrac)
          end if
        end do
      end do
    end if leadOrFollow

    call closeFile(fd)

  end subroutine writeRealEigvecsTxtBlacs




  !> Write the complex eigvectors into human readible output file (BLACS version).
  subroutine writeCplxEigvecsTxtBlacs(env, denseDesc, eigvecs, parallelKS, orb, over, kPoints,&
      & iNeighbour, nNeighbourSK, iCellVec, cellVec, iSparseStart, img2CentCell, species,&
      & speciesName, fileName)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Square Hamiltonian (or work array)
    complex(dp), intent(in) :: eigvecs(:,:,:)

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Orbital information
    type(TOrbitals), intent(in) :: orb

    !> Sparse overlap
    real(dp), intent(in) :: over(:)

    !> Kpoints
    real(dp), intent(in) :: kPoints(:,:)

    !> Neighbours of each atom
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom
    integer, intent(in) :: nNeighbourSK(:)

    !> Cell vector index for each atom
    integer, intent(in) :: iCellVec(:)

    !> Cell vectors
    real(dp), intent(in) :: cellVec(:,:)

    !> Index array for sparse matrices
    integer, intent(in) :: iSparseStart(:,:)

    !> Mapping of atoms into the central cell.
    integer, intent(in) :: img2CentCell(:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> Name of each species
    character(*), intent(in) :: speciesName(:)

    !> optional alternative file prefix, to appear as "fileName".bin
    character(len=*), intent(in), optional :: fileName

    type(linecomm) :: collector
    type(TFileDescr) :: fd
    complex(dp), allocatable :: localEigvec(:)
    complex(dp), allocatable :: globalS(:,:), globalSDotC(:,:)
    real(dp), allocatable :: localFrac(:), globalFrac(:,:)
    integer :: nEigvec, nAtom
    integer :: iKS, iK, iS, iGroup, iEig

    nEigvec = denseDesc%nOrb
    nAtom = size(nNeighbourSK)
    allocate(globalS(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    allocate(globalSDotC(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    allocate(globalFrac(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    if (env%mpi%tGroupLead) then
      allocate(localEigvec(denseDesc%nOrb))
      allocate(localFrac(denseDesc%nOrb))
    end if

    call collector%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")
    if (env%mpi%tGlobalLead) then
      call prepareEigvecFileTxt(fd, .false., fileName)
    end if

    if (env%mpi%tGlobalLead) then
      do iKS = 1, parallelKS%maxGroupKS
        group: do iGroup = 0, env%mpi%nGroup - 1
          if (iKS > parallelKS%nGroupKS(iGroup)) then
            cycle group
          end if
          iK = parallelKS%groupKS(1, iKS, iGroup)
          iS = parallelKS%groupKS(2, iKS, iGroup)
          if (iGroup == 0) then
            call unpackHSCplxBlacs(env%blacs, over, kPoints(:,iK), iNeighbour, nNeighbourSK,&
                & iCellVec, cellVec, iSparseStart, img2CentCell, denseDesc, globalS)
            call pblasfx_phemm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
                & denseDesc%blacsOrbSqr, globalSDotC, denseDesc%blacsOrbSqr)
            globalFrac(:,:) = real(conjg(eigvecs(:,:,iKS)) * globalSDotC)
          end if
          do iEig = 1, nEigvec
            if (iGroup == 0) then
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                  & localEigvec)
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalFrac, localFrac)
            else
              call mpifx_recv(env%mpi%interGroupComm, localEigvec, iGroup)
              call mpifx_recv(env%mpi%interGroupComm, localFrac, iGroup)
            end if
            call writeSingleCplxEigvecTxt(fd, localEigvec, localFrac, iS, iK, iEig, orb, species,&
                & speciesName, nAtom)
          end do
        end do group
      end do
    else
      do iKS = 1, parallelKS%nLocalKS
        iK = parallelKS%localKS(1, iKS)
        call unpackHSCplxBlacs(env%blacs, over, kPoints(:,iK), iNeighbour, nNeighbourSK, iCellVec,&
            & cellVec, iSparseStart, img2CentCell, denseDesc, globalS)
        call pblasfx_phemm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
            & denseDesc%blacsOrbSqr, globalSDotC, denseDesc%blacsOrbSqr)
        globalFrac(:,:) = real(conjg(eigvecs(:,:,iKS)) * globalSDotC)
        do iEig = 1, nEigvec
          if (env%mpi%tGroupLead) then
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                & localEigvec)
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalFrac, localFrac)
            call mpifx_send(env%mpi%interGroupComm, localEigvec, env%mpi%interGroupComm%leadrank)
            call mpifx_send(env%mpi%interGroupComm, localFrac, env%mpi%interGroupComm%leadrank)
          else
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS))
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, globalFrac)
          end if
        end do
      end do
    end if

    call closeFile(fd)

  end subroutine writeCplxEigvecsTxtBlacs




  !> Write the complex eigvectors into human readible output file (BLACS version).
  subroutine writePauliEigvecsTxtBlacs(env, denseDesc, eigvecs, parallelKS, orb, over, kPoints,&
      & iNeighbour, nNeighbourSK, iCellVec, cellVec, iSparseStart, img2CentCell, species,&
      & speciesName, fileName)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Square Hamiltonian (or work array)
    complex(dp), intent(in) :: eigvecs(:,:,:)

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Orbital information
    type(TOrbitals), intent(in) :: orb

    !> Sparse overlap
    real(dp), intent(in) :: over(:)

    !> Kpoints
    real(dp), intent(in) :: kPoints(:,:)

    !> Neighbours of each atom
    integer, intent(in) :: iNeighbour(0:,:)

    !> Nr. of neighbours for each atom
    integer, intent(in) :: nNeighbourSK(:)

    !> Cell vector index for each atom
    integer, intent(in) :: iCellVec(:)

    !> Cell vectors
    real(dp), intent(in) :: cellVec(:,:)

    !> Index array for sparse matrices
    integer, intent(in) :: iSparseStart(:,:)

    !> Mapping of atoms into the central cell.
    integer, intent(in) :: img2CentCell(:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> Name of each species
    character(*), intent(in) :: speciesName(:)

    !> optional alternative file prefix, to appear as "fileName".bin
    character(len=*), intent(in), optional :: fileName

    type(linecomm) :: collector
    real(dp), allocatable :: fracs(:,:)
    complex(dp), allocatable :: localEigvec(:), localSDotC(:)
    complex(dp), allocatable :: globalS(:,:), globalSDotC(:,:)
    integer :: nAtom, nOrb
    integer :: iKS, iK, iGroup, iEig
    type(TFileDescr) :: fd

    nOrb = denseDesc%fullSize
    nAtom = size(nNeighbourSK)
    allocate(globalS(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    allocate(globalSDotC(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    if (env%mpi%tGroupLead) then
      allocate(localEigvec(denseDesc%fullSize))
      allocate(localSDotC(denseDesc%fullSize))
      if (env%mpi%tGlobalLead) then
        allocate(fracs(4, denseDesc%nOrb))
      end if
    end if

    call collector%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")
    if (env%mpi%tGlobalLead) then
      call prepareEigvecFileTxt(fd, .true., fileName)
    end if

    ! See comment about algorithm in routine writeRealEigvecsBinBlacs

    leadOrFollow: if (env%mpi%tGlobalLead) then
      ! Global lead process
      do iKS = 1, parallelKS%maxGroupKS
        group: do iGroup = 0, env%mpi%nGroup - 1
          if (iKS > parallelKS%nGroupKS(iGroup)) then
            cycle group
          end if
          iK = parallelKS%groupKS(1, iKS, iGroup)
          if (iGroup == 0) then
            call unpackSPauliBlacs(env%blacs, over, kPoints(:,iK), iNeighbour, nNeighbourSK,&
                & iCellVec, cellVec, iSparseStart, img2CentCell, orb%mOrb, denseDesc, globalS)
            call pblasfx_phemm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
                & denseDesc%blacsOrbSqr, globalSDotC, denseDesc%blacsOrbSqr)
          end if
          do iEig = 1, nOrb
            if (iGroup == 0) then
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                  & localEigvec)
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalSDotC, localSDotC)
            else
              call mpifx_recv(env%mpi%interGroupComm, localEigvec, iGroup)
              call mpifx_recv(env%mpi%interGroupComm, localSDotC, iGroup)
            end if
            call getPauliFractions(localEigvec, localSDotC, fracs)
            call writeSinglePauliEigvecTxt(fd, localEigvec, fracs, iK, iEig, orb, species,&
                & speciesName, nAtom, denseDesc%nOrb)
          end do
        end do group
      end do
    else
      ! All processes except the global lead process
      do iKS = 1, parallelKS%nLocalKS
        iK = parallelKS%localKS(1, iKS)
        call unpackSPauliBlacs(env%blacs, over, kPoints(:,iK), iNeighbour, nNeighbourSK, iCellVec,&
            & cellVec, iSparseStart, img2CentCell, orb%mOrb, denseDesc, globalS)
        call pblasfx_phemm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
            & denseDesc%blacsOrbSqr, globalSDotC, denseDesc%blacsOrbSqr)
        do iEig = 1, nOrb
          if (env%mpi%tGroupLead) then
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                & localEigvec)
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalSDotC, localSDotC)
            call mpifx_send(env%mpi%interGroupComm, localEigvec, env%mpi%interGroupComm%leadrank)
            call mpifx_send(env%mpi%interGroupComm, localSDotC, env%mpi%interGroupComm%leadrank)
          else
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS))
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, globalSDotC)
          end if
        end do
      end do
    end if leadOrFollow

    call closeFile(fd)

  end subroutine writePauliEigvecsTxtBlacs



  !> Write projected eigenvectors.
  subroutine writeProjectedEigenvectors(env, regionLabels, eigen, neighbourList, nNeighbourSK,&
      & cellVec, iCellVec, denseDesc, iPair, img2CentCell, orb, over, kPoint, kWeight, iOrbRegion,&
      & parallelKS, eigvecsReal, workReal, eigvecsCplx, workCplx)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> File name prefix for each region
    type(TListCharLc), intent(inout) :: regionLabels

    !> Eigenvalues
    real(dp), intent(in) :: eigen(:,:,:)

    !> list of neighbours for each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Dense matrix descriptor for H and S
    type(TDenseDescr), intent(in) :: denseDesc

    !> Index array for the start of atomic blocks in sparse arrays
    integer, intent(in) :: iPair(:,:)

    !> map from image atoms to the original unique atom
    integer, intent(in) :: img2CentCell(:)

    !> Orbital information
    type(TOrbitals), intent(in) :: orb

    !> sparse overlap matrix
    real(dp), intent(in) :: over(:)

    !> k-points
    real(dp), intent(in) :: kPoint(:,:)

    !> Weights for k-points
    real(dp), intent(in) :: kWeight(:)

    !> Orbital regions to project
    type(TListIntR1), intent(inout) :: iOrbRegion

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Storage for eigenvectors (real)
    real(dp), intent(inout), allocatable :: eigvecsReal(:,:,:)

    !> Work space (real)
    real(dp), intent(inout), allocatable :: workReal(:,:)

    !> Storage for eigenvectors (complex)
    complex(dp), intent(inout), allocatable :: eigvecsCplx(:,:,:)

    !> Work space (complex)
    complex(dp), intent(inout), allocatable :: workCplx(:,:)




    if (allocated(eigvecsCplx)) then
      if (denseDesc%t2Component) then
        call writeProjPauliEigvecsBlacs(env, denseDesc, regionLabels, iOrbRegion, eigen,&
            & eigvecsCplx, orb, parallelKS, kPoint, kWeight, over, neighbourList, nNeighbourSK,&
            & iPair, img2CentCell, iCellVec, cellVec)
      else
        call writeProjCplxEigvecsBlacs(env, denseDesc, regionLabels, iOrbRegion, eigen,&
            & eigvecsCplx, parallelKS, kPoint, kWeight, over, neighbourList, nNeighbourSK, iPair,&
            & img2CentCell, iCellVec, cellVec)
      end if
    else
      call writeProjRealEigvecsBlacs(env, denseDesc, regionLabels, iOrbRegion, eigen, eigvecsReal,&
          & parallelKS, over, neighbourList, nNeighbourSK, iPair, img2CentCell)
    end if

  end subroutine writeProjectedEigenvectors



  !> Write the real eigvectors into human readible output file (BLACS version).
  subroutine writeProjRealEigvecsBlacs(env, denseDesc, fileNames, iOrbRegion, eigvals, eigvecs,&
      & parallelKS, over, neighbourList, nNeighbourSK, iSparseStart, img2CentCell)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> List of region file names
    type(TListCharLc), intent(inout) :: fileNames

    !> orbital number in each region
    type(TListIntR1), intent(inout) :: iOrbRegion

    !> Eigenvalues
    real(dp), intent(in) :: eigvals(:,:,:)

    !> Eigenvectors
    real(dp), intent(in) :: eigvecs(:,:,:)

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Sparse overlap
    real(dp), intent(in) :: over(:)

    !> Neighbours of each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Nr. of neighbours for each atom
    integer, intent(in) :: nNeighbourSK(:)

    !> Index array for sparse matrices
    integer, intent(in) :: iSparseStart(:,:)

    !> Mapping of atoms into the central cell.
    integer, intent(in) :: img2CentCell(:)

    type(linecomm) :: collector
    real(dp), allocatable :: globalS(:,:), globalFrac(:,:), localFrac(:)
    integer :: nOrb, nReg
    integer :: iKS, iS, iGroup, iEig
    type(TFileDescr), allocatable :: fd(:)

    nReg = len(iOrbRegion)
    allocate(fd(nReg))
    nOrb = denseDesc%fullSize
    allocate(globalS(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    allocate(globalFrac(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    if (env%mpi%tGroupLead) then
      allocate(localFrac(nOrb))
    end if

    if (env%mpi%tGlobalLead) then
      call prepareProjEigvecFiles(fd, fileNames)
    end if
    call collector%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")

    ! See comment about algorithm in routine writeRealEigvecsBinBlacs

    leadOrFollow: if (env%mpi%tGlobalLead) then
      ! Global lead process
      do iKS = 1, parallelKS%maxGroupKS
        group: do iGroup = 0, env%mpi%nGroup - 1
          if (iKS > parallelKS%nGroupKS(iGroup)) then
            cycle group
          end if
          iS = parallelKS%groupKS(2, iKS, iGroup)
          if (iGroup == 0) then
            call unpackHSRealBlacs(env%blacs, over, neighbourList%iNeighbour, nNeighbourSK,&
                & iSparseStart, img2CentCell, denseDesc, globalS)
            call pblasfx_psymm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
                & denseDesc%blacsOrbSqr, globalFrac, denseDesc%blacsOrbSqr)
            globalFrac(:,:) = eigvecs(:,:,iKS) * globalFrac
          end if
          call writeProjEigvecHeader(fd, iS)
          do iEig = 1, nOrb
            if (iGroup == 0) then
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalFrac, localFrac)
            else
              call mpifx_recv(env%mpi%interGroupComm, localFrac, iGroup)
            end if
            call writeProjEigvecData(fd, iOrbRegion, eigvals(iEig, 1, iS), localFrac)
          end do
          call writeProjEigvecFooter(fd)
        end do group
      end do
    else
      ! All processes except the global lead process
      do iKS = 1, parallelKS%nLocalKS
        call unpackHSRealBlacs(env%blacs, over, neighbourList%iNeighbour, nNeighbourSK,&
            & iSparseStart, img2CentCell, denseDesc, globalS)
        call pblasfx_psymm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS), denseDesc%blacsOrbSqr,&
            & globalFrac, denseDesc%blacsOrbSqr)
        globalFrac(:,:) = eigvecs(:,:,iKS) * globalFrac
        do iEig = 1, nOrb
          if (env%mpi%tGroupLead) then
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalFrac, localFrac)
            call mpifx_send(env%mpi%interGroupComm, localFrac, env%mpi%interGroupComm%leadrank)
          else
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, globalFrac)
          end if
        end do
      end do
    end if leadOrFollow

    call closeFile(fd)

  end subroutine writeProjRealEigvecsBlacs




  !> Write the complex eigvectors into human readible output file (BLACS version).
  subroutine writeProjCplxEigvecsBlacs(env, denseDesc, fileNames, iOrbRegion, eigvals, eigvecs,&
      & parallelKS, kPoints, kWeights, over, neighbourList, nNeighbourSK, iSparseStart,&
      & img2CentCell, iCellVec, cellVec)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> List of region file names
    type(TListCharLc), intent(inout) :: fileNames

    !> orbital number in each region
    type(TListIntR1), intent(inout) :: iOrbRegion

    !> Eigenvalues
    real(dp), intent(in) :: eigvals(:,:,:)

    !> Eigenvectors
    complex(dp), intent(in) :: eigvecs(:,:,:)

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> K-points
    real(dp), intent(in) :: kPoints(:,:)

    !> Weights of the k-points
    real(dp), intent(in) :: kWeights(:)

    !> Sparse overlap
    real(dp), intent(in) :: over(:)

    !> Neighbours of each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Nr. of neighbours for each atom
    integer, intent(in) :: nNeighbourSK(:)

    !> Index array for sparse matrices
    integer, intent(in) :: iSparseStart(:,:)

    !> Mapping of atoms into the central cell.
    integer, intent(in) :: img2CentCell(:)

    !> Cell vector index for each atom
    integer, intent(in) :: iCellVec(:)

    !> Cell vectors
    real(dp), intent(in) :: cellVec(:,:)

    type(linecomm) :: collector
    real(dp), allocatable :: globalFrac(:,:), localFrac(:)
    complex(dp), allocatable :: globalS(:,:), globalSDotC(:,:)
    integer :: nOrb, nReg
    integer :: iKS, iK, iS, iGroup, iEig
    type(TFileDescr), allocatable :: fd(:)

    nReg = len(iOrbRegion)
    allocate(fd(nReg))
    nOrb = denseDesc%fullSize
    allocate(globalS(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    allocate(globalSDotC(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    allocate(globalFrac(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    if (env%mpi%tGroupLead) then
      allocate(localFrac(nOrb))
    end if

    if (env%mpi%tGlobalLead) then
      call prepareProjEigvecFiles(fd, fileNames)
    end if
    call collector%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")

    ! See comment about algorithm in routine writeRealEigvecsBinBlacs

    leadOrFollow: if (env%mpi%tGlobalLead) then
      ! Global lead process
      do iKS = 1, parallelKS%maxGroupKS
        group: do iGroup = 0, env%mpi%nGroup - 1
          if (iKS > parallelKS%nGroupKS(iGroup)) then
            cycle group
          end if
          iK = parallelKS%groupKS(1, iKS, iGroup)
          iS = parallelKS%groupKS(2, iKS, iGroup)
          if (iGroup == 0) then
            call unpackHSCplxBlacs(env%blacs, over, kPoints(:,iK), neighbourList%iNeighbour,&
                & nNeighbourSK, iCellVec, cellVec, iSparseStart, img2CentCell, denseDesc, globalS)
            call pblasfx_phemm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
                & denseDesc%blacsOrbSqr, globalSDotC, denseDesc%blacsOrbSqr)
            globalFrac(:,:) = real(globalSDotC * conjg(eigvecs(:,:,iKS)))
          end if
          call writeProjEigvecHeader(fd, iS, iK, kWeights(iK))
          do iEig = 1, nOrb
            if (iGroup == 0) then
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalFrac, localFrac)
            else
              call mpifx_recv(env%mpi%interGroupComm, localFrac, iGroup)
            end if
            call writeProjEigvecData(fd, iOrbRegion, eigvals(iEig, iK, iS), localFrac)
          end do
        end do group
        call writeProjEigvecFooter(fd)
      end do
    else
      ! All processes except the global lead process
      do iKS = 1, parallelKS%nLocalKS
        iK = parallelKS%localKS(1, iKS)
        call unpackHSCplxBlacs(env%blacs, over, kPoints(:,iK), neighbourList%iNeighbour,&
            & nNeighbourSK, iCellVec, cellVec, iSparseStart, img2CentCell, denseDesc, globalS)
        call pblasfx_phemm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
            & denseDesc%blacsOrbSqr, globalSDotC, denseDesc%blacsOrbSqr)
        globalFrac(:,:) = real(conjg(eigvecs(:,:,iKS)) * globalSDotC)
        do iEig = 1, nOrb
          if (env%mpi%tGroupLead) then
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalFrac, localFrac)
            call mpifx_send(env%mpi%interGroupComm, localFrac, env%mpi%interGroupComm%leadrank)
          else
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, globalFrac)
          end if
        end do
      end do
    end if leadOrFollow

    call closeFile(fd)

  end subroutine writeProjCplxEigvecsBlacs




  !> Write the complex eigvectors into human readible output file (BLACS version).
  subroutine writeProjPauliEigvecsBlacs(env, denseDesc, fileNames, iOrbRegion, eigvals, eigvecs,&
      & orb, parallelKS, kPoints, kWeights, over, neighbourList, nNeighbourSK, iSparseStart,&
      & img2CentCell, iCellVec, cellVec)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> List of region file names
    type(TListCharLc), intent(inout) :: fileNames

    !> orbital number in each region
    type(TListIntR1), intent(inout) :: iOrbRegion

    !> Eigenvalues
    real(dp), intent(in) :: eigvals(:,:,:)

    !> Eigenvectors
    complex(dp), intent(in) :: eigvecs(:,:,:)

    !> Basis orbital information
    type(TOrbitals), intent(in) :: orb

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> K-points
    real(dp), intent(in) :: kPoints(:,:)

    !> Weights of the k-points
    real(dp), intent(in) :: kWeights(:)

    !> Sparse overlap
    real(dp), intent(in) :: over(:)

    !> Neighbours of each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Nr. of neighbours for each atom
    integer, intent(in) :: nNeighbourSK(:)

    !> Index array for sparse matrices
    integer, intent(in) :: iSparseStart(:,:)

    !> Mapping of atoms into the central cell.
    integer, intent(in) :: img2CentCell(:)

    !> Cell vector index for each atom
    integer, intent(in) :: iCellVec(:)

    !> Cell vectors
    real(dp), intent(in) :: cellVec(:,:)

    type(linecomm) :: collector
    complex(dp), allocatable :: localSDotC(:), localEigvec(:)
    complex(dp), allocatable :: globalS(:,:), globalSDotC(:,:)
    real(dp), allocatable :: fracs(:,:)
    integer :: nOrb
    integer :: iKS, iK, iGroup, iEig
    type(TFileDescr), allocatable :: fd(:)

    allocate(fd(len(iOrbRegion)))
    nOrb = denseDesc%fullSize
    allocate(globalS(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    allocate(globalSDotC(size(eigvecs, dim=1), size(eigvecs, dim=2)))
    if (env%mpi%tGroupLead) then
      allocate(localEigvec(nOrb))
      allocate(localSDotC(nOrb))
      if (env%mpi%tGlobalLead) then
        allocate(fracs(4, nOrb / 2))
      end if
    end if

    if (env%mpi%tGlobalLead) then
      call prepareProjEigvecFiles(fd, fileNames)
    end if
    call collector%init(env%blacs%orbitalGrid, denseDesc%blacsOrbSqr, "c")

    ! See comment about algorithm in routine writeRealEigvecsBinBlacs

    leadOrFollow: if (env%mpi%tGlobalLead) then
      ! Global lead process
      do iKS = 1, parallelKS%maxGroupKS
        group: do iGroup = 0, env%mpi%nGroup - 1
          if (iKS > parallelKS%nGroupKS(iGroup)) then
            cycle group
          end if
          iK = parallelKS%groupKS(1, iKS, iGroup)
          if (iGroup == 0) then
            call unpackSPauliBlacs(env%blacs, over, kPoints(:,iK), neighbourList%iNeighbour,&
                & nNeighbourSK, iCellVec, cellVec, iSparseStart, img2CentCell, orb%mOrb, denseDesc,&
                & globalS)
            call pblasfx_phemm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
                & denseDesc%blacsOrbSqr, globalSDotC, denseDesc%blacsOrbSqr)
          end if
          call writeProjEigvecHeader(fd, 1, iK, kWeights(iK))
          do iEig = 1, nOrb
            if (iGroup == 0) then
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                  & localEigvec)
              call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalSDotC, localSDotC)
            else
              call mpifx_recv(env%mpi%interGroupComm, localEigvec, iGroup)
              call mpifx_recv(env%mpi%interGroupComm, localSDotC, iGroup)
            end if
            call getPauliFractions(localEigvec, localSDotC, fracs)
            call writeProjPauliEigvecData(fd, iOrbRegion, eigvals(iEig, iK, 1), fracs)
          end do
          call writeProjEigvecFooter(fd)
        end do group
      end do
    else
      ! All processes except the global lead process
      do iKS = 1, parallelKS%nLocalKS
        iK = parallelKS%localKS(1, iKS)
        call unpackSPauliBlacs(env%blacs, over, kPoints(:,iK), neighbourList%iNeighbour,&
            & nNeighbourSK, iCellVec, cellVec, iSparseStart, img2CentCell, orb%mOrb, denseDesc,&
            & globalS)
        call pblasfx_phemm(globalS, denseDesc%blacsOrbSqr, eigvecs(:,:,iKS),&
            & denseDesc%blacsOrbSqr, globalSDotC, denseDesc%blacsOrbSqr)
        do iEig = 1, nOrb
          if (env%blacs%orbitalGrid%lead) then
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS),&
                & localEigvec)
            call collector%getline_lead(env%blacs%orbitalGrid, iEig, globalSDotC, localSDotC)
            call mpifx_send(env%mpi%interGroupComm, localEigvec, env%mpi%interGroupComm%leadrank)
            call mpifx_send(env%mpi%interGroupComm, localSDotC, env%mpi%interGroupComm%leadrank)
          else
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, eigvecs(:,:,iKS))
            call collector%getline_follow(env%blacs%orbitalGrid, iEig, globalSDotC)
          end if
        end do
      end do
    end if leadOrFollow

    call closeFile(fd)

  end subroutine writeProjPauliEigvecsBlacs



  !> Open an output file and clear it
  subroutine initOutputFile(fileName)

    !> File name
    character(*), intent(in) :: fileName

    type(TFileDescr) :: fd

    call openFile(fd, fileName, mode="w")
    call closeFile(fd)

  end subroutine initOutputFile


  !> Write tagged output of data from the code at the end of the DFTB+ run, data being then used for
  !> regression testing
  subroutine writeAutotestTag(fileName, electronicSolver, tPeriodic, cellVol, tMulliken, qOutput,&
      & derivs, chrgForces, excitedDerivs, tStress, totalStress, pDynMatrix, energy, pressure,&
      & endCoords, tLocalise, localisation, esp, taggedWriter, tunneling, ldos, lCurrArray,&
      & polarisability, dEidE, dipoleMoment, eFieldScaling)

    !> Name of output file
    character(*), intent(in) :: fileName

    !> Electronic solver information
    type(TElectronicSolver), intent(in) :: electronicSolver

    !> Is the geometry periodic
    logical, intent(in) :: tPeriodic

    !> Unit cell volume if periodic (unreferenced otherwise)
    real(dp), intent(in) :: cellVol

    !> Are Mulliken charges to be output
    logical, intent(in) :: tMulliken

    !> Output Mulliken charges
    real(dp), intent(in) :: qOutput(:,:,:)

    !> Atomic derivatives (allocation status used as a flag)
    real(dp), allocatable, intent(in) :: derivs(:,:)

    !> Forces on external charges (allocation status used as a flag)
    real(dp), allocatable, intent(in) :: chrgForces(:,:)

    !> Excited state forces on atoms (allocation status used as a flag)
    real(dp), allocatable, intent(in) :: excitedDerivs(:,:)

    !> Should stresses be printed (assumes periodic)
    logical, intent(in) :: tStress

    !> Stress tensor
    real(dp), intent(in) :: totalStress(:,:)

    !> Hessian (dynamical) matrix
    real(dp), pointer, intent(in) ::  pDynMatrix(:,:)

    !> Energy contributions and total
    type(TEnergies), intent(in) :: energy

    !> External pressure
    real(dp), intent(in) :: pressure

    !> Final atomic coordinates
    real(dp), intent(in) :: endCoords(:,:)

    !> Has localisation of single particle states been applied
    logical, intent(in) :: tLocalise

    !> Localisation measure, if relevant
    real(dp), intent(in) :: localisation

    !> Object holding the potentials and their locations
    type(TElStatPotentials), allocatable, intent(in) :: esp

    !> Tagged writer object
    type(TTaggedWriter), intent(inout) :: taggedWriter

    !> tunneling array
    real(dp), allocatable, intent(in) :: tunneling(:,:)

    !> local projected DOS array
    real(dp), allocatable, intent(in) :: ldos(:,:)

    !> Array containing bond currents as (Jvalues, atom)
    !> This array is for testing only since it misses info
    real(dp), allocatable, intent(in) :: lCurrArray(:,:)

    !> Static electric polarisability
    real(dp), intent(in), allocatable :: polarisability(:,:,:)

    !> Derivatives of eigenvalues wrt to electric field, if required
    real(dp), allocatable, intent(in) :: dEidE(:,:,:,:)

    !> Overall dipole moment
    real(dp), intent(in), allocatable :: dipoleMoment(:,:)

    !> Any dielectric environment scaling
    class(TScaleExtEField), intent(in) :: eFieldScaling

    type(TFileDescr) :: fd
    real(dp), allocatable :: qOutputUpDown(:,:,:)

    call openFile(fd, fileName, mode="a")
    if (tPeriodic) then
      call taggedWriter%write(fd%unit, tagLabels%volume, cellVol)
    end if
    if (tMulliken) then
      qOutputUpDown = qOutput
      call qm2ud(qOutputUpDown)
      call taggedWriter%write(fd%unit, tagLabels%qOutput, qOutputUpDown(:,:,1))
    end if
    if (allocated(derivs)) then
      call taggedWriter%write(fd%unit, tagLabels%forceTot, -derivs)
    end if
    if (allocated(chrgForces)) then
      call taggedWriter%write(fd%unit, tagLabels%chrgForces, -chrgForces)
    end if
    if (allocated(excitedDerivs)) then
      if (size(excitedDerivs) > 0) then
        call taggedWriter%write(fd%unit, tagLabels%excForce, -excitedDerivs)
      end if
    end if
    if (tStress) then
      call taggedWriter%write(fd%unit, tagLabels%stressTot, totalStress)
    end if
    if (associated(pDynMatrix)) then
      call taggedWriter%write(fd%unit, tagLabels%HessianNum, pDynMatrix)
    end if
    if (electronicSolver%providesElectronEntropy) then
      ! Mermin electronic free energy
      call taggedWriter%write(fd%unit, tagLabels%freeEgy, energy%EMermin)
    else
      call taggedWriter%write(fd%unit, tagLabels%egyTotal, energy%ETotal)
    end if
    if (pressure /= 0.0_dp) then
      ! Gibbs free energy
      call taggedWriter%write(fd%unit, tagLabels%gibbsfree, energy%EGibbs)
    end if
    call taggedWriter%write(fd%unit, tagLabels%endCoord, endCoords)
    if (tLocalise) then
      call taggedWriter%write(fd%unit, tagLabels%pmlocalise, localisation)
    end if

    if (allocated(esp)) then
      call taggedWriter%write(fd%unit, tagLabels%internfield, -esp%intPotential)
      if (allocated(esp%extPotential)) then
        call taggedWriter%write(fd%unit, tagLabels%externfield, -esp%extPotential)
      end if
    end if

    if (allocated(tunneling)) then
      if (size(tunneling, dim=1) > 0) then
        call taggedWriter%write(fd%unit, tagLabels%tunn, tunneling)
      end if
    end if

    if (allocated(ldos)) then
      if (size(ldos,1) > 0) then
        call taggedWriter%write(fd%unit, tagLabels%ldos, ldos)
      end if
    end if

    if (allocated(lCurrArray)) then
      call taggedWriter%write(fd%unit, tagLabels%localCurrents, lCurrArray)
    end if

    if (allocated(polarisability)) then
      call taggedWriter%write(fd%unit, tagLabels%dmudEPerturb, polarisability)
    end if

    if (allocated(dEidE)) then
      call taggedWriter%write(fd%unit, tagLabels%dEigenDE, dEidE)
    end if

    if (allocated(dipoleMoment)) then
      call taggedWriter%write(fd%unit, tagLabels%dipoleMoment, dipoleMoment)
      call taggedWriter%write(fd%unit, tagLabels%scaledDipole,&
          & eFieldScaling%scaledSoluteDipole(dipoleMoment))
    end if
    call closeFile(fd)

  end subroutine writeAutotestTag


  !> Writes out machine readable data
  subroutine writeResultsTag(fileName, energy, derivs, chrgForces, nEl, Ef, eigen, filling,&
      & electronicSolver, tStress, totalStress, pDynMatrix, pBornMatrix, tPeriodic, cellVol,&
      & tMulliken, qOutput, q0, taggedWriter, cm5Cont, polarisability, dEidE, dqOut, neFermi,&
      & dEfdE, dipoleMoment, multipole, eFieldScaling)

    !> Name of output file
    character(len=*), intent(in) :: fileName

    !> Energy contributions and total
    type(TEnergies), intent(in) :: energy

    !> Atomic derivatives (allocation status used as a flag)
    real(dp), allocatable, intent(in) :: derivs(:,:)

    !> Forces on external charges
    real(dp), allocatable, intent(in) :: chrgForces(:,:)

    !> Number of electrons
    real(dp), intent(in) :: nEl(:)

    !> Fermi level(s)
    real(dp), intent(inout) :: Ef(:)

    !> Eigenvalues/single particle states (level, kpoint, spin)
    real(dp), intent(in) :: eigen(:,:,:)

    !> Filling of the eigenstates
    real(dp), intent(in) :: filling(:,:,:)

    !> Electronic solver information
    type(TElectronicSolver), intent(in) :: electronicSolver

    !> Should stresses be printed (assumes periodic)
    logical, intent(in) :: tStress

    !> Stress tensor
    real(dp), intent(in) :: totalStress(:,:)

    !> Hessian (dynamical) matrix
    real(dp), pointer, intent(in) :: pDynMatrix(:,:)

    !> Born charge matrix
    real(dp), pointer, intent(in) :: pBornMatrix(:,:)

    !> Is the geometry periodic
    logical, intent(in) :: tPeriodic

    !> Unit cell volume if periodic (unreferenced otherwise)
    real(dp), intent(in) :: cellVol

    !> Are Mulliken charges to be output
    logical, intent(in) :: tMulliken

    !> Output Mulliken charges
    real(dp), intent(in) :: qOutput(:,:,:)

    !> Reference atomic charges
    real(dp), intent(in) :: q0(:,:,:)

    !> Tagged writer object
    type(TTaggedWriter), intent(inout) :: taggedWriter

    !> Charge model 5 to correct atomic gross charges
    type(TChargeModel5), allocatable, intent(in) :: cm5Cont

    !> Static electric polarisability
    real(dp), intent(in), allocatable :: polarisability(:,:,:)

    !> Derivatives of eigenvalues wrt to electric field, if required
    real(dp), allocatable, intent(in) :: dEidE(:,:,:,:)

    !> Derivative of Mulliken charges wrt to electric field, if required
    real(dp), allocatable, intent(in) :: dqOut(:,:,:,:)

    !> Electrons at the Fermi energy (if metallic and evaluated)
    real(dp), allocatable, intent(in) :: neFermi(:)

    !> Derivative of the Fermi energy with respect to electric field
    real(dp), allocatable, intent(in) :: dEfdE(:,:)

    !> Overall dipole moment
    real(dp), intent(in), allocatable :: dipoleMoment(:,:)

    !> Multipole moments
    type(TMultipole), intent(in) :: multipole

    !> Any dielectric environment scaling
    class(TScaleExtEField), intent(in) :: eFieldScaling

    real(dp), allocatable :: qOutputUpDown(:,:,:), qDiff(:,:,:)
    type(TFileDescr) :: fd

    call openFile(fd, fileName, mode="a")

    call taggedWriter%write(fd%unit, tagLabels%egyTotal, energy%ETotal)
    if (electronicSolver%elecChemPotAvailable) then
      call taggedWriter%write(fd%unit, tagLabels%fermiLvl, Ef)
    end if
    call taggedWriter%write(fd%unit, tagLabels%nElec, nEl)

    if (electronicSolver%providesFreeEnergy) then
      call taggedWriter%write(fd%unit, tagLabels%freeEgy, energy%EForceRelated)
    elseif (electronicSolver%providesElectronEntropy) then
      call taggedWriter%write(fd%unit, tagLabels%freeEgy, energy%EMermin)
    else
      call taggedWriter%write(fd%unit, tagLabels%egyTotal, energy%ETotal)
    end if

    if (electronicSolver%providesFreeEnergy .or. electronicSolver%providesElectronEntropy) then
      ! extrapolated zero temperature energy (the chemical potential and electron number are assumed
      ! to be temperature independent, as just extrapolates the Mermin energy)
      call taggedWriter%write(fd%unit, tagLabels%egy0Total, energy%Ezero)
    end if

    if (electronicSolver%providesEigenvals) then
      call taggedWriter%write(fd%unit, tagLabels%eigvals, eigen)
      call taggedWriter%write(fd%unit, tagLabels%eigFill, filling)
    end if

    if (electronicSolver%providesFreeEnergy) then
      ! energy connected to the evaluated force/stress (differs for various free energies)
      call taggedWriter%write(fd%unit, tagLabels%egyForceRelated, energy%EForceRelated)
    end if

    if (allocated(derivs)) then
      call taggedWriter%write(fd%unit, tagLabels%forceTot, -derivs)
    end if
    if (allocated(chrgForces)) then
      call taggedWriter%write(fd%unit, tagLabels%chrgForces, -chrgForces)
    end if
    if (tStress) then
      call taggedWriter%write(fd%unit, tagLabels%stressTot, totalStress)
    end if
    if (associated(pDynMatrix)) then
      call taggedWriter%write(fd%unit, tagLabels%HessianNum, pDynMatrix)
    end if
    if (associated(pBornMatrix)) then
      call taggedWriter%write(fd%unit, tagLabels%BorndDipNum,&
          & eFieldScaling%scaledSoluteDipole(pBornMatrix))
    end if
    if (tPeriodic) then
      call taggedWriter%write(fd%unit, tagLabels%volume, cellVol)
    end if

    if (tMulliken) then
      qOutputUpDown = qOutput
      call qm2ud(qOutputUpDown)
      qDiff = qOutput - q0
      call taggedWriter%write(fd%unit, tagLabels%qOutput, qOutputUpDown)
      call taggedWriter%write(fd%unit, tagLabels%qOutAtGross, -sum(qDiff(:,:,1), dim=1))
      if (size(qDiff, dim=3) > 1) then
        call taggedWriter%write(fd%unit, tagLabels%spinOutAtGross, sum(qDiff(:, :, 2:), dim=1))
      end if
      if (allocated(cm5Cont)) then
        call taggedWriter%write(fd%unit, tagLabels%qOutAtCM5, cm5Cont%cm5 - sum(qDiff(:,:,1),dim=1))
      end if
    end if

    if (allocated(dipoleMoment)) then
      call taggedWriter%write(fd%unit, tagLabels%dipoleMoment, dipoleMoment)
      call taggedWriter%write(fd%unit, tagLabels%scaledDipole,&
          & eFieldScaling%scaledSoluteDipole(dipoleMoment))
    end if

    if (allocated(multipole%dipoleAtom)) then
      call taggedWriter%write(fd%unit, tagLabels%dipoleAtom,&
          & eFieldScaling%scaledSoluteDipole(multipole%dipoleAtom))
    end if

    if (allocated(polarisability)) then
      call taggedWriter%write(fd%unit, tagLabels%dmudEPerturb, polarisability)
    end if
    if (allocated(dEidE)) then
      call taggedWriter%write(fd%unit, tagLabels%dEigenDE, dEidE)
    end if
    if (allocated(dqOut)) then
      call taggedWriter%write(fd%unit, tagLabels%dqdEPerturb, sum(dqOut, dim = 1))
    end if

    if (allocated(neFermi)) then
      call taggedWriter%write(fd%unit, tagLabels%neFermi, neFermi)
    end if

    if (allocated(dEfdE)) then
      call taggedWriter%write(fd%unit, tagLabels%dEfdE, dEfdE)
    end if

    call closeFile(fd)

  end subroutine writeResultsTag


  !> Write XML format of derived results
  subroutine writeDetailedXml(runId, speciesName, species0, coord0Out, tPeriodic, tHelical, latVec,&
      & origin, tRealHS, nKPoint, nSpin, nStates, nOrb, kPoint, kWeight, filling, occNatural)

    !> Identifier for the run
    integer, intent(in) :: runId

    !> Labels for the atomic species
    character(*), intent(in) :: speciesName(:)

    !> Species numbers for central cell atoms
    integer, intent(in) :: species0(:)

    !> coordinates of atoms
    real(dp), intent(in) :: coord0Out(:,:)

    !> Periodic boundary conditions
    logical, intent(in) :: tPeriodic

    !> Is the geometry helical?
    logical, intent(in) :: tHelical

    !> Lattice vectors if periodic or helical
    real(dp), intent(in) :: latVec(:,:)

    !> Origin for periodic/helical coordinates
    real(dp), intent(in) :: origin(:)

    !> Real Hamiltonian
    logical, intent(in) :: tRealHS

    !> Number of k-points present
    integer, intent(in) :: nKPoint

    !> Number of spin channels present
    integer, intent(in) :: nSpin

    !> Number of eigen states in the system / dimension of the Hamiltonian
    integer, intent(in) :: nStates

    !> Number of atomic orbitals (may not match nStates if non-collinear)
    integer, intent(in) :: nOrb

    !> k-points in the system
    real(dp), intent(in) :: kPoint(:,:)

    !> Weights of the k-points
    real(dp), intent(in) :: kWeight(:)

    !> Filling of the eigenstates
    real(dp), intent(in) :: filling(:,:,:)

    !> Occupation numbers for natural orbitals
    real(dp), allocatable, target, intent(in) :: occNatural(:)

    type(xmlf_t) :: xf
    real(dp), allocatable :: bufferRealR2(:,:)
    integer :: ii, jj, ll
    real(dp), pointer :: pOccNatural(:,:)



    call xml_OpenFile("detailed.xml", xf, indent=.true.)
    call xml_ADDXMLDeclaration(xf)
    call xml_NewElement(xf, "detailedout")
    call writeChildValue(xf, "identity", runId)
    call xml_NewElement(xf, "geometry")
    call writeChildValue(xf, "typenames", speciesName)
    if (tPeriodic .or. tHelical) then
      call writeChildValue(xf, "typesandcoordinates", reshape(species0, [ 1, size(species0) ]),&
          & coord0Out + spread(origin, 2, size(coord0Out, dim=2)))
    else
      call writeChildValue(xf, "typesandcoordinates", reshape(species0, [ 1, size(species0) ]),&
          & coord0Out)
    end if
    call writeChildValue(xf, "periodic", tPeriodic)
    call writeChildValue(xf, "helical", tHelical)
    if (tPeriodic .or. tHelical) then
      call writeChildValue(xf, "latticevectors", latVec)
      call writeChildValue(xf, "coordinateorigin", origin)
    end if
    call xml_EndElement(xf, "geometry")
    call writeChildValue(xf, "real", tRealHS)
    call writeChildValue(xf, "nrofkpoints", nKPoint)
    call writeChildValue(xf, "nrofspins", nSpin)
    call writeChildValue(xf, "nrofstates", nStates)
    call writeChildValue(xf, "nroforbitals", nOrb)
    allocate(bufferRealR2(4, nKPoint))
    bufferRealR2(1:3, :) = kPoint
    bufferRealR2(4,:) = kWeight
    call writeChildValue(xf, "kpointsandweights", bufferRealR2)
    call xml_NewElement(xf, "occupations")
    do ii = 1, nSpin
      call xml_NewElement(xf, "spin" // i2c(ii))
      do jj = 1, nKpoint
        call writeChildValue(xf, "k" // i2c(jj), filling(:, jj, mod(ii,3)))
      end do
      call xml_EndElement(xf, "spin" // i2c(ii))
    end do
    call xml_EndElement(xf, "occupations")
    if (allocated(occNatural)) then
      call xml_NewElement(xf, "excitedoccupations")
      call xml_NewElement(xf, "spin" // i2c(1))
      !pOccNatural(1:size(occNatural), 1:1) => occNatural
      ll = size(occNatural)
      pOccNatural(1:ll, 1:1) => occNatural
      call writeChildValue(xf, "k" // i2c(1), pOccNatural)
      call xml_EndElement(xf, "spin" // i2c(1))
      call xml_EndElement(xf, "excitedoccupations")
    end if
    call xml_EndElement(xf, "detailedout")
    call xml_Close(xf)
  end subroutine writeDetailedXml


  !> Write the band structure data out
  subroutine writeBandOut(fileName, eigen, filling, kWeight)

    !> Name of file to write to
    character(*), intent(in) :: fileName

    !> Eigenvalues for states, k-points and spin indices
    real(dp), intent(in) :: eigen(:,:,:)

    !> Fillings of the states
    real(dp), intent(in) :: filling(:,:,:)

    !> Weights of the k-points
    real(dp), intent(in) :: kWeight(:)

    type(TFileDescr) :: fd
    integer :: iSpin, iK, iEgy

    call openFile(fd, fileName, mode="w")
    do iSpin = 1, size(eigen, dim=3)
      do iK = 1, size(eigen, dim=2)
        write(fd%unit, *) 'KPT ', iK, ' SPIN ', iSpin, ' KWEIGHT ', kWeight(iK)
        do iEgy = 1, size(eigen, dim=1)
          ! meV accuracy for eigenvalues
          write(fd%unit, "(I6, F10.3, F9.5)") iEgy, Hartree__eV * eigen(iEgy, iK, iSpin),&
              & filling(iEgy, iK, iSpin)
        end do
        write(fd%unit, *)
      end do
    end do
    call closeFile(fd)

  end subroutine writeBandOut


  !> Write the derivative band structure data out
  subroutine writeDBandCart(fileName, dEigen, kWeight, isFileAppended)

    !> Name of file to write to
    character(*), intent(in) :: fileName

    !> Derivative of eigenvalues for states, k-points, spin indices and directions
    real(dp), intent(in) :: dEigen(:,:,:,:)

    !> Weights of the k-points
    real(dp), intent(in) :: kWeight(:)

    !> If true, append onto the file, if false replace the file
    logical, intent(in), optional :: isFileAppended

    type(TFileDescr) :: fd
    character(1) :: fileMode
    integer :: iSpin, iK, iEgy, iCart
    logical :: isFileAppended_



    if (present(isFileAppended)) then
      isFileAppended_ = isFileAppended
    else
      isFileAppended_ = .false.
    end if

    if (isFileAppended_) then
      fileMode = "a"
    else
      fileMode = "w"
    end if
    call openFile(fd, fileName, mode=fileMode)
    do iCart = 1, 3
      do iSpin = 1, size(dEigen, dim=3)
        do iK = 1, size(dEigen, dim=2)
          write(fd%unit, *) 'DIR ', quaternionName(iCart+1), ' KPT ', iK, ' SPIN ', iSpin,&
              & ' KWEIGHT ', kWeight(iK)
          do iEgy = 1, size(dEigen, dim=1)
            write(fd%unit, "(I6, E16.6)") iEgy, Hartree__eV * dEigen(iEgy, iK, iSpin, iCart)
          end do
          write(fd%unit,*)
        end do
      end do
    end do
    call closeFile(fd)

  end subroutine writeDBandCart


  !> Write the derivative band structure data out
  subroutine writeDBand(fileName, dEigen, kWeight, isFileAppended, preLabel)

    !> Name of file to write to
    character(*), intent(in) :: fileName

    !> Derivatives of eigenvalues for states, k-points and spin indices
    real(dp), intent(in) :: dEigen(:,:,:)

    !> Weights of the k-points
    real(dp), intent(in) :: kWeight(:)

    !> If true, append onto the file, if false replace the file
    logical, intent(in), optional :: isFileAppended

    !> Extra text to print at start of data
    character(*), intent(in), optional :: preLabel

    type(TFileDescr) :: fd
    character(1) :: fileMode
    integer :: iSpin, iK, iEgy
    logical :: isFileAppended_

    if (present(isFileAppended)) then
      isFileAppended_ = isFileAppended
    else
      isFileAppended_ = .false.
    end if

    if (isFileAppended_) then
      fileMode = "a"
    else
      fileMode = "w"
    end if
    call openFile(fd, fileName, mode=fileMode)
    do iSpin = 1, size(dEigen, dim=3)
      do iK = 1, size(dEigen, dim=2)
        if (present(preLabel)) then
          write(fd%unit, "(A)", advance="NO")trim(preLabel) // " "
        end if
        write(fd%unit, *) 'KPT ', iK, ' SPIN ', iSpin, ' KWEIGHT ', kWeight(iK)
        do iEgy = 1, size(dEigen, dim=1)
          write(fd%unit, "(I6, E16.6)") iEgy, Hartree__eV * dEigen(iEgy, iK, iSpin)
        end do
        write(fd%unit,*)
      end do
    end do
    call closeFile(fd)

  end subroutine writeDBand


  !> Write the energy second derivative matrix
  subroutine writeHessianOut(fileName, pDynMatrix, indMovedAtoms, errStatus)

    !> File name
    character(*), intent(in) :: fileName

    !> Dynamical (Hessian) matrix
    real(dp), intent(in) :: pDynMatrix(:,:)

    !> Indices of moved atoms
    integer, intent(in) :: indMovedAtoms(:)

    !> Status of operation
    type(TStatus), intent(out) :: errStatus

    type(TFileDescr) :: fd
    integer :: ii
    character(10) :: suffix1, suffix2
    logical :: tPartialHessian = .false.

    ! Sanity check in case some bug is introduced
    if (size(pDynMatrix, dim=2) /= 3*size(indMovedAtoms)) then
  call errStatus%setError(-1, "Internal error: incorrect number of rows of dynamical Matrix",&
      & "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/dftbplus/mainio.F90", 2514)
  return
    end if
    ! It is a partial Hessian Calculation if DynMatrix is not squared
    if (size(pDynMatrix, dim=1) > size(pDynMatrix, dim=2)) then
      tPartialHessian = .true.
    end if

    if (tPartialHessian) then
      write(suffix1,'(I10)') indMovedAtoms(1)
      write(suffix2,'(I10)') indMovedAtoms(size(indMovedAtoms))
      call openFile(fd,&
          & fileName // "." // trim(adjustl(suffix1)) // "-" // trim(adjustl(suffix2)),&
          & mode="w")
    else
      call openFile(fd, fileName, mode="w")
    end if

    do ii = 1, size(pDynMatrix, dim=2)
      write(fd%unit, formatHessian) pDynMatrix(:, ii)
    end do

    if (tPartialHessian) then
      write(stdOut, "(2A)") 'Hessian matrix written to ',&
          & fileName//"."//trim(adjustl(suffix1))//"-"//trim(adjustl(suffix2))
    else
      write(stdOut, "(2A)") 'Hessian matrix written to ', fileName
    end if

    call closeFile(fd)

  end subroutine writeHessianOut


  !> Write the dipole derivative wrt.coordinates matrix/Born charges
  subroutine writeBornChargesOut(fileName, pBornMatrix, indMovedAtoms, nDerivAtoms, errStatus)

    !> File name
    character(*), intent(in) :: fileName

    !> Born (dipole derivatives or force wrt electric field)
    real(dp), intent(in) :: pBornMatrix(:,:)

    !> Indices of moved atoms
    integer, intent(in) :: indMovedAtoms(:)

    !> Number of atoms for which derivatives should be calculated (>= size(indMovedAtoms))
    integer, intent(in) :: nDerivAtoms

    !> Status of operation
    type(TStatus), intent(out) :: errStatus

    type(TFileDescr) :: fd
    integer :: ii
    character(10) :: suffix1, suffix2
    logical :: tPartialMatrix

    ! Sanity check in case some bug is introduced
    if (any(shape(pBornMatrix) /= [3,3*size(indMovedAtoms)])) then
  call errStatus%setError(-1, "Internal error: incorrectly shaped Born Matrix", "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/df&
      &tbplus/mainio.F90", 2572)
  return
    end if

    tPartialMatrix = size(pBornMatrix, dim=2) < 3 * nDerivAtoms
    if (tPartialMatrix) then
      write(suffix1,'(I10)') indMovedAtoms(1)
      write(suffix2,'(I10)') indMovedAtoms(size(indMovedAtoms))
      call openFile(fd, fileName // "." // trim(adjustl(suffix1)) // "-" // trim(adjustl(suffix2)),&
          & mode="w")
    else
      call openFile(fd, fileName, mode="w")
    end if

    do ii = 1, size(pBornMatrix, dim=2)
      write(fd%unit, formatBorn) pBornMatrix(:, ii)
    end do

    call closeFile(fd)

    if (tPartialMatrix) then
      write(stdOut, "(2A)") 'Born charges matrix written to ',&
          & fileName//"."//trim(adjustl(suffix1))//"-"//trim(adjustl(suffix2))
    else
      write(stdOut, "(2A)") 'Born charges matrix written to ', fileName
    end if

  end subroutine writeBornChargesOut


  !> Write the Derivatives of the polarizability
  subroutine writeBornDerivs(fileName, pdBornMatrix, indMovedAtoms, nDerivAtoms, errStatus)

    !> File name
    character(*), intent(in) :: fileName

    !> Born (dipole derivatives or force wrt electric field)
    real(dp), intent(in) :: pdBornMatrix(:, :, :)

    !> Indices of moved atoms
    integer, intent(in) :: indMovedAtoms(:)

    !> Number of atoms for which derivatives should be calculated (>= size(indMovedAtoms))
    integer, intent(in) :: nDerivAtoms

    !> Status of operation
    type(TStatus), intent(out) :: errStatus

    type(TFileDescr) :: fd
    character(:), allocatable :: file
    integer :: ii
    character(10) :: suffix1, suffix2
    logical :: tPartialMatrix

    ! Sanity check in case some bug is introduced
    if (any(shape(pdBornMatrix) /= [3, 3, 3*size(indMovedAtoms)])) then
  call errStatus%setError(-1, "Internal error: incorrectly shaped Born Matrix", "/home/xh/packages/DFTB+/dftbplus-23.1/src/dftbp/df&
      &tbplus/mainio.F90", 2627)
  return
    end if

    tPartialMatrix = size(pdBornMatrix, dim=3) < 3 * nDerivAtoms
    if (tPartialMatrix) then
      write(suffix1,'(I10)') indMovedAtoms(1)
      write(suffix2,'(I10)') indMovedAtoms(size(indMovedAtoms))
      file = fileName // "." // trim(adjustl(suffix1)) // "-" // trim(adjustl(suffix2))
    else
      file = fileName
    end if
    call openFile(fd, file, mode="w")

    do ii = 1, size(pdBornMatrix, dim=3)
      write(fd%unit, formatdBorn) pdBornMatrix(:, :, ii)
    end do

    call closeFile(fd)
    write(stdOut, "(2A)") 'Born charge derivative matrix written to ', file

  end subroutine writeBornDerivs


  !> Opens an output file or uses the its current unit number, if the file is already open.
  subroutine openOutputFile(fileName, append, fd)

    !> Name of the output file
    character(*), intent(in) :: fileName

    !> Whether apppend to the end of the file or overwrite
    logical, intent(in) :: append

    !> File descriptor
    type(TFileDescr), intent(inout) :: fd

    if (fd%isConnected() .and. .not. append) then
      call closeFile(fd)
    end if
    if (.not. fd%isConnected()) then
      call openFile(fd, fileName, mode="w")
    end if

  end subroutine openOutputFile


  !> Optimization and geometry data to go to detailed.out
  subroutine writeDetailedOut1(fd, iDistribFn, nGeoSteps, iGeoStep, tMD, tDerivs, tCoordOpt,&
      & tLatOpt, iLatGeoStep, iSccIter, energy, diffElec, sccErrorQ, indMovedAtom, coord0Out,&
      & tPeriodic, tScc, tNegf, invLatVec, kPoints)

    !> File ID
    integer, intent(in) :: fd

    !> Electron distribution choice
    integer, intent(in) :: iDistribFn

    !> Total number of geometry steps
    integer, intent(in) :: nGeoSteps

    !> Current geometry step
    integer, intent(in) :: iGeoStep

    !> Is this a molecular dynamics run
    logical, intent(in) :: tMD

    !> Is this a finite difference derivative calculation
    logical, intent(in) :: tDerivs

    !> Are atomic coordinates being optimised?
    logical, intent(in) :: tCoordOpt

    !> Is the lattice being optimised?
    logical, intent(in) :: tLatOpt

    !> Which step of lattice optimisation is occurring
    integer, intent(in) :: iLatGeoStep

    !> Which scc step is occurring
    integer, intent(in) :: iSccIter

    !> Energy terms in the system
    type(TEnergies), intent(in) :: energy

    !> Change in energy from previous SCC iteration
    real(dp), intent(in) :: diffElec

    !> Input/output charge error for SCC
    real(dp), intent(in) :: sccErrorQ

    !> Moving atoms
    integer, intent(in) :: indMovedAtom(:)

    !> Output atomic coordinates
    real(dp), intent(in) :: coord0Out(:,:)

    !> Is the system periodic
    logical, intent(in) :: tPeriodic

    !> Is this a self consistent charge calculation
    logical, intent(in) :: tScc

    !> whether we solve NEGF
    logical, intent(in) :: tNegf

    !> Reciprocal lattice vectors if periodic
    real(dp), intent(in) :: invLatVec(:,:)

    !> K-points if periodic
    real(dp), intent(in) :: kPoints(:,:)

    integer :: nKPoint, nMovedAtom, iAt, iK
    character(lc) :: strTmp

    nKPoint = size(kPoints, dim=2)
    nMovedAtom = size(indMovedAtom)

    if (.not. tNegf) then
      ! depends on the contact calculations
      select case(iDistribFn)
      case(0)
        write(fd,*) 'Fermi distribution function'
      case(1)
        write(fd,*) 'Gaussian distribution function'
      case default
        write(fd,*) 'Methfessel-Paxton distribution function order', iDistribFn
      end select
      write(fd,*)
    end if

    if (nGeoSteps > 0) then
      if (tMD) then
        write(fd, "(A, I0)") "MD step: ", iGeoStep
      elseif (tDerivs) then
        write(fd, "(A, I0)") 'Difference derivative step: ', iGeoStep
      else
        if (tCoordOpt .and. tLatOpt) then
          write(fd, "(A, I0, A, I0)") "Geometry optimization step: ", iGeoStep,&
              & ", Lattice step: ", iLatGeoStep
        else
          write(fd, "(A, I0)") "Geometry optimization step: ", iGeoStep
        end if
      end if
    elseif (tScc) then
      ! Only written if scc is on, to be compatible with old output
      write(fd, "(A)") "Calculation with static geometry"
    end if
    write(fd, *)

    if (tSCC) then
      write(fd, "(/, A)") repeat("*", 80)
      write(fd, "(A5, A18, A18, A18)") "iSCC", " Total electronic ", "  Diff electronic ",&
          & "     SCC error    "
      write(fd, "(I5, E18.8, E18.8, E18.8, E18.8)") iSCCIter, energy%Eelec, diffElec, sccErrorQ
      write(fd, "(A)") repeat("*", 80)
      write(fd, *)
    end if

    if (tPeriodic .and. tLatOpt) then
      do iK = 1, nKPoint
        if (iK == 1) then
          write(strTmp, "(A,':')") "K-points in absolute space"
        else
          write(strTmp, "(A)") ""
        end if
        write(fd, "(A,T28,I6,':',3F10.6)") trim(strTmp), iK, matmul(invLatVec,kPoints(:,iK))
      end do
      write(fd, *)
    end if

    if (nMovedAtom > 0 .and. .not. tDerivs) then
      write(fd, "(A)") "Coordinates of moved atoms (au):"
      do iAt = 1, nMovedAtom
        write(fd, formatGeoOut) indMovedAtom(iAt), coord0Out(:, indMovedAtom(iAt))
      end do
      write(fd, *)
    end if

  end subroutine writeDetailedOut1


  !> Charge data to go to detailed.out
  subroutine writeDetailedOut2(fd, q0, qOutput, orb, species, tDFTBU, tImHam, tPrintMulliken,&
      & orbitalL, qBlockOut, nSpin, tOnSite, iAtInCentralRegion, cm5Cont, qNetAtom)

    !> File ID
    integer, intent(in) :: fd

    !> Reference atomic charges
    real(dp), intent(in) :: q0(:,:,:)

    !> Output atomic charges (if SCC)
    real(dp), intent(in) :: qOutput(:,:,:)

    !> Type containing atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Chemical species of atoms
    integer, intent(in) :: species(:)

    !> Are orbital potentials being used
    logical, intent(in) :: tDFTBU

    !> Does the Hamiltonian have an imaginary component (spin-orbit, magnetic field, ...)
    logical, intent(in) :: tImHam

    !> Should Mulliken populations be printed
    logical, intent(in) :: tPrintMulliken

    !> Orbital angular momentum (if available)
    real(dp), allocatable, intent(in) :: orbitalL(:,:,:)

    !> Output block (dual) Mulliken charges
    real(dp), allocatable, intent(in) :: qBlockOut(:,:,:,:)

    !> Number of spin channels
    integer, intent(in) :: nSpin

    !> Are on-site corrections being used?
    logical, intent(in) :: tOnSite

    !> atoms in the central cell (or device region if transport)
    integer, intent(in) :: iAtInCentralRegion(:)

    !> Charge model 5 for correcting atomic gross charges
    type(TChargeModel5), allocatable, intent(in) :: cm5Cont

    !> Onsite mulliken population per atom
    real(dp), intent(in), optional :: qNetAtom(:)

    real(dp), allocatable :: qOutputUpDown(:,:,:), qBlockOutUpDown(:,:,:,:), ev(:,:), ei(:)
    real(dp) :: angularMomentum(3)
    integer :: ang
    integer :: nAtom
    integer :: iAt, iSpin, iSp, iSh, iOrb, ii, kk
    character(sc), allocatable :: shellNamesTmp(:)
    character(lc) :: strTmp

    nAtom = size(q0, dim=2)

    qOutputUpDown = qOutput
    call qm2ud(qOutputUpDown)
    if (allocated(qBlockOut)) then
      qBlockOutUpDown = qBlockOut
      call qm2ud(qBlockOutUpDown)
    end if

    ! Write out atomic charges
    if (tPrintMulliken) then
      write(fd, "(A, F14.8)") " Total charge: ", sum(q0(:, iAtInCentralRegion(:), 1)&
          & - qOutput(:, iAtInCentralRegion(:), 1))
      write(fd, "(/,A)") " Atomic gross charges (e)"
      write(fd, "(A5, 1X, A16)")" Atom", " Charge"
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, 1X, F16.8)") iAt, sum(q0(:, iAt, 1) - qOutput(:, iAt, 1))
      end do
      write(fd, *)

      if (present(qNetAtom)) then
        write(fd, "(/,A)") " Atomic net (on-site) populations and hybridisation ratios"
        write(fd, "(A5, 1X, A16, A16)")" Atom", " Population", "Hybrid."
        do ii = 1, size(iAtInCentralRegion)
          iAt = iAtInCentralRegion(ii)
          write(fd, "(I5, 1X, F16.8, F16.8)") iAt, qNetAtom(iAt),&
              & (1.0_dp - qNetAtom(iAt) / sum(q0(:, iAt, 1)))
        end do
        write(fd, *)
      end if

      if (allocated(cm5Cont)) then
         write(fd, "(A)") " CM5 corrected atomic gross charges (e)"
         write(fd, "(A5, 1X, A16)")" Atom", " Charge"
         do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            write(fd, "(I5, 1X, F16.8)") iAt, sum(q0(:, iAt, 1) - qOutput(:, iAt, 1))&
                & + cm5Cont%cm5(iAt)
         end do
         write(fd, *)
      end if
    end if

    if (nSpin == 4) then
      if (tPrintMulliken) then
        do iSpin = 1, 4

          write(fd,"(3A, F16.8)") 'Nr. of electrons (', quaternionName(iSpin), '):',&
              & sum(qOutput(:, iAtInCentralRegion(:), iSpin))
          write(fd, *)
          write(fd, "(/, 3A)") 'Atom populations (', quaternionName(iSpin), ')'
          write(fd, "(A5, 1X, A16)") " Atom", " Population"
          do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            write(fd, "(1X, I5, 1X, F16.8)") iAt, sum(qOutput(:, iAt, iSpin))
          end do
          write(fd, "(/, 3A)") 'l-shell populations (', quaternionName(iSpin), ')'
          write(fd, "(A5, 1X, A3, 1X, A3, 1X, A16)") " Atom", "Sh.", "  l", " Population"
          do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            iSp = species(iAt)
            do iSh = 1, orb%nShell(iSp)
              write(fd, "(I5, 1X, I3, 1X, I3, 1X, F16.8)") iAt, iSh, orb%angShell(iSh, iSp),&
                  & sum(qOutput(orb%posShell(iSh,iSp):orb%posShell(iSh+1, iSp) - 1, iAt, iSpin))
            end do
          end do
          write(fd,*)
          write(fd, "(/, 3A)") 'Orbital populations (', quaternionName(iSpin) ,')'
          write(fd, "(A5, 1X, A3, 1X, A3, 1X, A3, 1X, A16, 1X, A6)") " Atom", "Sh.","  l","  m",&
              & " Population", " Label"
          do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            iSp = species(iAt)
            call getShellNames(iSp, orb, shellNamesTmp)
            do iSh = 1, orb%nShell(iSp)
              ang = orb%angShell(iSh, iSp)
              if (ang > 0) then
                write(strtmp,"(A)")trim(shellNamesTmp(iSh))//'_'
              else
                write(strtmp,"(A)")trim(shellNamesTmp(iSh))
              end if
              do kk = 0, 2 * ang
                write(fd, "(I5, 1X, I3, 1X, I3, 1X, I3, 1X, F16.8, 2X, A)") iAt, iSh, ang,&
                    & kk - ang, qOutput(orb%posShell(iSh, iSp) + kk, iAt, iSpin),&
                    & trim(strTmp)//trim(orbitalNames(kk-ang,ang))
              end do
            end do
            deallocate(shellNamesTmp)
          end do
          write(fd, *)
        end do
      end if

      if (tDFTBU) then
        do iSpin = 1, 4
          write(fd, "(3A)") 'Block populations (', quaternionName(iSpin), ')'
          do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            iSp = species(iAt)
            write(fd, "(A, 1X, I0)") 'Atom', iAt
            do iOrb = 1, orb%nOrbSpecies(iSp)
              write(fd, "(16F8.4)") qBlockOut(1:orb%nOrbSpecies(iSp), iOrb, iAt, iSpin)
            end do
            if (orb%nOrbSpecies(iSp) > 1) then
              allocate(ei(orb%nOrbSpecies(iSp)))
              ev = qBlockOut(:orb%nOrbSpecies(iSp), :orb%nOrbSpecies(iSp), iAt, iSpin)
              call heev(ev, ei, 'l', 'v')
              write(fd,*)'Eigen-decomposition'
              do iOrb = 1, orb%nOrbSpecies(iSp)
                write(fd, "(F8.4,A,16F8.4)") ei(iOrb),':',ev(:, iOrb)
              end do
              deallocate(ev, ei)
            end if
            write(fd, *)
          end do
        end do
      end if

      if (tImHam .and. tPrintMulliken) then
        write(fd, "(/, A)") 'Electron angular momentum (mu_B/hbar)'
        write(fd, "(2X, A5, T9, A3, T13, A1, T19, A1, T34, A9)")&
            & "Atom", "Sh.", "l", "S", "Momentum"
        do ii = 1, size(iAtInCentralRegion)
          iAt = iAtInCentralRegion(ii)
          iSp = species(iAt)
          do iSh = 1, orb%nShell(iSp)
            write(fd, "(I5, 1X, I3, 1X, I3, 1X, F14.8, ' :', 3F14.8)") iAt, iSh,&
                & orb%angShell(iSh, iSp), 0.5_dp * sqrt(sum(sum(qOutput(orb%posShell(iSh, iSp)&
                & :orb%posShell(iSh + 1, iSp) - 1, iAt, 2:4), dim=1)**2)),&
                & -gfac * 0.25_dp * sum(qOutput(orb%posShell(iSh, iSp)&
                & :orb%posShell(iSh + 1, iSp) - 1, iAt, 2:4), dim=1)
          end do
        end do
        write(fd, "(/, A)") 'Orbital angular momentum (mu_B/hbar)'
        write(fd, "(2X, A5, T9, A3, T13, A1, T19, A1, T34, A9)")&
            & "Atom", "Sh.", "l", "L", "Momentum"
        do ii = 1, size(iAtInCentralRegion)
          iAt = iAtInCentralRegion(ii)
          iSp = species(iAt)
          do iSh = 1, orb%nShell(iSp)
            write(fd, "(I5, 1X, I3, 1X, I3, 1X, F14.8, ' :', 3F14.8)") iAt, iSh,&
                & orb%angShell(iSh, iSp), sqrt(sum(orbitalL(1:3, iSh, iAt)**2)),&
                & -orbitalL(1:3, iSh, iAt)
          end do
        end do

        write(fd, *)
        write(fd, "(A)") 'Total angular momentum (mu_B/hbar)'
        write(fd, "(2X, A5, T9, A3, T13, A1, T19, A1, T34, A9)")&
            & "Atom", "Sh.", "l", "J", "Momentum"
        angularMomentum(:) = 0.0_dp
        do ii = 1, size(iAtInCentralRegion)
          iAt = iAtInCentralRegion(ii)
          iSp = species(iAt)
          do iSh = 1, orb%nShell(iSp)
            write(fd, "(I5, 1X, I3, 1X, I3, 1X, F14.8, ' :', 3F14.8)") iAt, iSh,&
                & orb%angShell(iSh, iSp), sqrt(sum((orbitalL(1:3, iSh, iAt)&
                & + sum(0.5_dp * qOutput(orb%posShell(iSh, iSp)&
                & :orb%posShell(iSh + 1, iSp) - 1, iAt, 2:4), dim=1))**2)),&
                & -orbitalL(1:3, iSh, iAt)&
                & -gfac * 0.25_dp * sum(qOutput(orb%posShell(iSh, iSp)&
                & :orb%posShell(iSh + 1, iSp) - 1, iAt, 2:4), dim=1)
            angularMomentum(1:3) = angularMomentum(1:3) -orbitalL(1:3, iSh, iAt)&
                & -gfac * 0.25_dp * sum(qOutput(orb%posShell(iSh, iSp)&
                & :orb%posShell(iSh + 1, iSp) - 1, iAt, 2:4), dim=1)
          end do
        end do
        write(fd, *)
      end if
    else
      lpSpinPrint2: do iSpin = 1, nSpin
        if (tPrintMulliken) then
          write(fd, "(3A, F16.8)") 'Nr. of electrons (', trim(spinName(iSpin)), '):',&
              & sum(qOutputUpDown(:, iAtInCentralRegion(:), iSpin))
          write(fd, "(3A)") 'Atom populations (', trim(spinName(iSpin)), ')'
          write(fd, "(A5, 1X, A16)") " Atom", " Population"
          do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            write(fd, "(I5, 1X, F16.8)") iAt, sum(qOutputUpDown(:, iAt, iSpin))
          end do
          write(fd, *)
          write(fd, "(3A)") 'l-shell populations (', trim(spinName(iSpin)), ')'
          write(fd, "(A5, 1X, A3, 1X, A3, 1X, A16)")" Atom", "Sh.", "  l", " Population"
          do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            iSp = species(iAt)
            do iSh = 1, orb%nShell(iSp)
              write(fd, "(I5, 1X, I3, 1X, I3, 1X, F16.8)") iAt, iSh, orb%angShell(iSh, iSp),&
                  & sum(qOutputUpDown(orb%posShell(iSh, iSp):orb%posShell(iSh + 1, iSp)-1, iAt,&
                  & iSpin))
            end do
          end do
          write(fd, *)
          write(fd, "(3A)") 'Orbital populations (', trim(spinName(iSpin)), ')'
          write(fd, "(A5, 1X, A3, 1X, A3, 1X, A3, 1X, A16, 1X, A6)")&
              & " Atom", "Sh.", "  l", "  m", " Population", " Label"
          do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            iSp = species(iAt)
            call getShellNames(iSp, orb, shellNamesTmp)
            do iSh = 1, orb%nShell(iSp)
              ang = orb%angShell(iSh, iSp)
              if (ang > 0) then
                write(strtmp,"(A)")trim(shellNamesTmp(iSh))//'_'
              else
                write(strTmp,"(A)")trim(shellNamesTmp(iSh))
              end if
              do kk = 0, 2 * ang
                write(fd, "(I5, 1X, I3, 1X, I3, 1X, I3, 1X, F16.8, 2X, A)") iAt, iSh, ang,&
                    & kk - ang, qOutputUpDown(orb%posShell(iSh, iSp) + kk, iAt, iSpin),&
                    & trim(strTmp)//trim(orbitalNames(kk-ang,ang))
              end do
            end do
            deallocate(shellNamesTmp)
          end do
          write(fd, *)
        end if
        if (tDFTBU .or. tOnSite) then
          write(fd, "(3A)") 'Block populations (', trim(spinName(iSpin)), ')'
          do ii = 1, size(iAtInCentralRegion)
            iAt = iAtInCentralRegion(ii)
            iSp = species(iAt)
            write(fd, "(A, 1X, I0)") 'Atom', iAt
            do iOrb = 1, orb%nOrbSpecies(iSp)
              write(fd, "(16F8.4)") qBlockOutUpDown(1:orb%nOrbSpecies(iSp), iOrb, iAt, iSpin)
            end do
            if (orb%nOrbSpecies(iSp) > 1) then
              allocate(ei(orb%nOrbSpecies(iSp)))
              ev = qBlockOutUpDown(:orb%nOrbSpecies(iSp), :orb%nOrbSpecies(iSp), iAt, iSpin)
              call heev(ev, ei, 'l', 'v')
              write(fd,*)'Eigen-decomposition'
              do iOrb = 1, orb%nOrbSpecies(iSp)
                write(fd, "(F8.4,A,16F8.4)") ei(iOrb),':',ev(:, iOrb)
              end do
              deallocate(ev, ei)
            end if
          end do
          write(fd, *)
        end if
      end do lpSpinPrint2
    end if

  end subroutine writeDetailedOut2


  !> Wrapped call for detailedout2 and print energies, which can process multiple determinants,
  !> currently only for two spin channels
  subroutine writeDetailedOut2Dets(fdDetailedOut, userOut, tAppendDetailedOut, dftbEnergy,&
      & electronicSolver, deltaDftb, q0, orb, qOutput, qDets, qBlockDets, species,&
      & iAtInCentralRegion, tPrintMulliken, cm5Cont)

    !> File ID
    type(TFileDescr), intent(inout) :: fdDetailedOut

    !> File name for output
    character(*), intent(in) :: userOut

    !> Append to the end of the file or overwrite
    logical, intent(in) :: tAppendDetailedOut

    !> Energy contributions and total
    type(TEnergies), intent(in) :: dftbEnergy(:)

    !> Electronic solver information
    type(TElectronicSolver), intent(in) :: electronicSolver

    !> type for DFTB determinants
    type(TDftbDeterminants), intent(in) :: deltaDftb

    !> Reference atomic charges
    real(dp), intent(in) :: q0(:,:,:)

    !> Type containing atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Charges for final state
    real(dp), intent(in), allocatable :: qOutput(:,:,:)

    !> Charges for each determinant this is present
    real(dp), intent(in), allocatable :: qDets(:,:,:,:)

    !> block populations for each determinant present
    real(dp), intent(in), allocatable :: qBlockDets(:,:,:,:,:)

    !> Chemical species of atoms
    integer, intent(in) :: species(:)

    !> Should Mulliken populations be printed
    logical, intent(in) :: tPrintMulliken

    !> atoms in the central cell (or device region if transport)
    integer, intent(in) :: iAtInCentralRegion(:)

    !> Charge model 5 for correcting atomic gross charges
    type(TChargeModel5), allocatable, intent(in) :: cm5Cont

    real(dp), allocatable :: blockTmp(:,:,:,:), orbitalL(:,:,:)



    call openOutputFile(userOut, tAppendDetailedOut, fdDetailedOut)

    if (deltaDftb%iGround > 0) then
      write(fdDetailedOut%unit, *)'S0 state'
      if (allocated(qBlockDets)) then
        blockTmp = qBlockDets(:,:,:,:,deltaDftb%iGround)
      end if
      call writeDetailedOut2(fdDetailedOut%unit, q0, qDets(:,:,:,deltaDftb%iGround),&
          & orb, species, allocated(blockTmp), .false., tPrintMulliken, orbitalL, blockTmp, 2,&
          & allocated(blockTmp), iAtInCentralRegion, cm5Cont)
    end if
    if (deltaDftb%iTriplet > 0) then
      write(fdDetailedOut%unit, *)'T1 state'
      if (allocated(qBlockDets)) then
        blockTmp = qBlockDets(:,:,:,:,deltaDftb%iTriplet)
      end if
      call writeDetailedOut2(fdDetailedOut%unit, q0, qDets(:,:,:,deltaDftb%iTriplet),&
          & orb, species, allocated(blockTmp), .false., tPrintMulliken, orbitalL, blockTmp, 2,&
          & allocated(blockTmp), iAtInCentralRegion, cm5Cont)
    end if
    if (deltaDftb%isSpinPurify) then
      write(fdDetailedOut%unit, *)'S1 state'
      if (allocated(qBlockDets)) then
        blockTmp = 2.0_dp*qBlockDets(:,:,:,:,deltaDftb%iMixed)&
            & - qBlockDets(:,:,:,:,deltaDftb%iTriplet)
      end if
    else
      write(fdDetailedOut%unit, *)'Mixed state'
      if (allocated(qBlockDets)) then
        blockTmp = qBlockDets(:,:,:,:,deltaDftb%iMixed)
      end if
    end if

    call writeDetailedOut2(fdDetailedOut%unit, q0, qOutput, orb, species, allocated(blockTmp),&
        & .false., tPrintMulliken, orbitalL, blockTmp, 2, allocated(blockTmp), iAtInCentralRegion,&
        & cm5Cont)

    call printEnergies(dftbEnergy, electronicSolver, deltaDftb, fdDetailedOut%unit)

  end subroutine writeDetailedOut2Dets


  !> Third group of data to go to detailed.out
  subroutine writeDetailedOut3(fd, qInput, qOutput, energy, species, tDFTBU, tPrintMulliken, Ef,&
      & pressure, cellVol, tAtomicEnergy, dispersion, isExtField, tPeriodic, nSpin, tSpin,&
      & tSpinOrbit, tScc, tOnSite, iAtInCentralRegion, electronicSolver, tHalogenX,&
      & tRangeSep, t3rd, tSolv)

    !> File ID
    integer, intent(in) :: fd

    !> Input atomic charges (if SCC)
    real(dp), intent(in) :: qInput(:,:,:)

    !> Output atomic charges (if SCC)
    real(dp), intent(in) :: qOutput(:,:,:)

    !> Energy terms in the system
    type(TEnergies), intent(in) :: energy

    !> Chemical species of atoms
    integer, intent(in) :: species(:)

    !> Are orbital potentials being used
    logical, intent(in) :: tDFTBU

    !> Should Mulliken populations be printed
    logical, intent(in) :: tPrintMulliken

    !> Fermi level
    real(dp), intent(in) :: Ef(:)

    !> External pressure
    real(dp), intent(in) :: pressure

    !> Unit cell volume
    real(dp), intent(in) :: cellVol

    !> Are atom resolved energies required
    logical, intent(in) :: tAtomicEnergy

    !> Dispersion interactions object
    class(TDispersionIface), allocatable, intent(inout) :: dispersion

    !> Is there an external field present
    logical, intent(in) :: isExtField

    !> Is the system periodic
    logical, intent(in) :: tPeriodic

    !> Number of spin channels
    integer, intent(in) :: nSpin

    !> is this a spin polarized calculation?
    logical :: tSpin

    !> Are spin orbit interactions present
    logical, intent(in) :: tSpinOrbit

    !> Is this a self consistent charge calculation
    logical, intent(in) :: tScc

    !> Are on-site corrections being used?
    logical, intent(in) :: tOnSite

    !> atoms in the central cell (or device region if transport)
    integer, intent(in) :: iAtInCentralRegion(:)

    !> Electronic solver information
    type(TElectronicSolver), intent(in) :: electronicSolver

    !> Is there a halogen bond correction present?
    logical, intent(in) :: tHalogenX

    !> Is this a range separation calculation?
    logical, intent(in) :: tRangeSep

    !> Is this a 3rd order scc calculation?
    logical, intent(in) :: t3rd

    !> Is this a solvation model used?
    logical, intent(in) :: tSolv

    real(dp), allocatable :: qInputUpDown(:,:,:), qOutputUpDown(:,:,:)
    integer :: nSpinHams
    integer :: iAt, iSpin, ii

    nSpinHams = size(Ef)

    qInputUpDown = qInput
    call qm2ud(qInputUpDown)
    qOutputUpDown = qOutput
    call qm2ud(qOutputUpDown)

    lpSpinPrint3: do iSpin = 1, nSpinHams
      if (nSpin == 2) then
        write(fd, "(A, 1X, A)") 'Spin ', trim(spinName(iSpin))
      end if
      if (electronicSolver%elecChemPotAvailable) then
        write(fd, format2U) 'Fermi level', Ef(iSpin), "H", Hartree__eV * Ef(iSpin), 'eV'
      end if
      if (electronicSolver%providesBandEnergy) then
        write(fd, format2U) 'Band energy', energy%Eband(iSpin), "H",&
            & Hartree__eV * energy%Eband(iSpin), 'eV'
      end if
      if (electronicSolver%providesElectronEntropy) then
        write(fd, format2U)'TS', energy%TS(iSpin), "H", Hartree__eV * energy%TS(iSpin), 'eV'
      end if
      if (electronicSolver%providesFreeEnergy) then
        if (electronicSolver%providesBandEnergy) then
          write(fd, format2U) 'Band free energy (E-TS)', energy%Eband(iSpin)-energy%TS(iSpin), "H",&
              & Hartree__eV * (energy%Eband(iSpin) - energy%TS(iSpin)), 'eV'
        end if
        write(fd, format2U) 'Extrapolated E(0K)', energy%E0(iSpin), "H",&
            & Hartree__eV * (energy%E0(iSpin)), 'eV'
      end if
      if (tPrintMulliken) then
        if (nSpin == 2) then
          write(fd, "(3A, 2F18.10)") 'Input / Output electrons (', trim(spinName(iSpin)), '):',&
              & sum(qInputUpDown(:, iAtInCentralRegion(:), iSpin)),&
              & sum(qOutputUpDown(:, iAtInCentralRegion(:), iSpin))
        else
          if (tSCC) then
            write(fd, "(3A, 2F18.10)") 'Input / Output electrons (', quaternionName(iSpin), '):',&
                & sum(qInputUpDown(:, iAtInCentralRegion(:), iSpin)),&
                & sum(qOutputUpDown(:, iAtInCentralRegion(:), iSpin))
          else
            write(fd, "(3A, F18.10)") 'Output electrons (', quaternionName(iSpin), '):',&
                & sum(qOutputUpDown(:, iAtInCentralRegion(:), iSpin))
          end if
        end if
      end if
      write(fd, *)
    end do lpSpinPrint3

    write(fd, format2U) 'Energy H0', energy%EnonSCC, 'H', energy%EnonSCC * Hartree__eV, 'eV'

    if (tSCC) then
      write(fd, format2U) 'Energy SCC', energy%ESCC, 'H', energy%ESCC * Hartree__eV, 'eV'
      if (tSpin) then
        write(fd, format2U) 'Energy SPIN', energy%Espin, 'H', energy%Espin * Hartree__eV, 'eV'
      end if
      if (t3rd) then
        write(fd, format2U) 'Energy 3rd', energy%e3rd, 'H', energy%e3rd * Hartree__eV, 'eV'
      end if
      if (tRangeSep) then
        write(fd, format2U) 'Energy Fock', energy%Efock, 'H', energy%Efock * Hartree__eV, 'eV'
      end if
      if (tDFTBU) then
        write(fd, format2U) 'Energy DFTB+U', energy%Edftbu, 'H', energy%Edftbu * Hartree__eV, 'eV'
      end if
      if (tOnSite) then
        write (fd,format2U) 'Energy onsite', energy%eOnSite, 'H', energy%eOnSite*Hartree__eV, 'eV'
      end if
    end if

    if (tSpinOrbit) then
      write(fd, format2U) 'Energy L.S', energy%ELS, 'H', energy%ELS * Hartree__eV, 'eV'
    end if

    if (isExtField) then
      write(fd, format2U) 'Energy ext. field', energy%Eext, 'H', energy%Eext * Hartree__eV, 'eV'
    end if

    if (tSolv) then
      write(fd, format2U) 'Solvation energy', energy%ESolv, 'H', energy%ESolv * Hartree__eV, 'eV'
    end if

    write(fd, format2U) 'Total Electronic energy', energy%Eelec, 'H', energy%Eelec * Hartree__eV,&
        & 'eV'
    write(fd, format2U) 'Repulsive energy', energy%Erep, 'H', energy%Erep * Hartree__eV, 'eV'

    if (allocated(dispersion)) then
      if (dispersion%energyAvailable()) then
        write(fd, format2U) 'Dispersion energy', energy%eDisp, 'H', energy%eDisp * Hartree__eV, 'eV'
      else
        write(fd, "(A)") 'Dispersion energy not yet evaluated, so also missing from other energies'
      end if
    end if

    if (tHalogenX) then
      write(fd, format2U) 'Halogen correction energy', energy%eHalogenX, 'H',&
          & energy%eHalogenX * Hartree__eV, 'eV'
    end if

    write(fd, format2U) 'Total energy', energy%Etotal, 'H', energy%Etotal * Hartree__eV, 'eV'
    if (electronicSolver%providesElectronEntropy) then
      write(fd, format2U) 'Extrapolated to 0', energy%Ezero, 'H', energy%Ezero * Hartree__eV, 'eV'
      write(fd, format2U) 'Total Mermin free energy', energy%Etotal - sum(energy%TS), 'H',&
          & (energy%Etotal - sum(energy%TS)) * Hartree__eV, 'eV'
    end if
    if (electronicSolver%providesFreeEnergy) then
      write(fd, format2U) 'Force related energy', energy%EForceRelated, 'H',&
          & energy%EForceRelated * Hartree__eV, 'eV'
    end if
    if (tPeriodic .and. pressure /= 0.0_dp) then
      write(fd, format2U) 'Gibbs free energy', energy%Etotal - sum(energy%TS) + cellVol * pressure,&
          & 'H', Hartree__eV * (energy%Etotal - sum(energy%TS) + cellVol * pressure), 'eV'
    end if
    write(fd, *)

    if (tAtomicEnergy) then
      write(fd, "(A)") 'Atom resolved electronic energies '
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, F16.8, A, F16.6, A)") iAt, energy%atomElec(iAt), ' H',&
            & Hartree__eV * energy%atomElec(iAt), ' eV'
      end do
      write(fd, *)

      write(fd, "(A)") 'Atom resolved repulsive energies '
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, F16.8, A, F16.6, A)") iAt, energy%atomRep(iAt), ' H',&
            & Hartree__eV * energy%atomRep(iAt), ' eV'
      end do
      write(fd, *)
      write(fd, "(A)") 'Atom resolved total energies '
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, F16.8, A, F16.6, A)") iAt, energy%atomTotal(iAt), ' H',&
            & Hartree__eV * energy%atomTotal(iAt), ' eV'
      end do
      write(fd, *)
    end if

  end subroutine writeDetailedOut3


  !> Fourth group of data for detailed.out
  subroutine writeDetailedOut4(fd, tScc, tConverged, tXlbomd, isLinResp, tGeoOpt, tMd,&
      & tPrintForces, tStress, tPeriodic, energy, totalStress, totalLatDeriv, derivs, chrgForces,&
      & indMovedAtom, cellVol, cellPressure, geoOutFile, iAtInCentralRegion)

    !> File ID
    integer, intent(in) :: fd

    !> Charge self consistent?
    logical, intent(in) :: tScc

    !> Has the SCC cycle converged?
    logical, intent(in) :: tConverged

    !> Is the extended Lagrangian in use for MD
    logical, intent(in) :: tXlbomd

    !> Is the Casida excited state in use?
    logical, intent(in) :: isLinResp

    !> Is the geometry being optimised
    logical, intent(in) :: tGeoOpt

    !> Is this a molcular dynamics run
    logical, intent(in) :: tMd

    !> Should forces be printed out?
    logical, intent(in) :: tPrintForces

    !> Is the stress tensor to be printed?
    logical, intent(in) :: tStress

    !> Is the geometry periodic
    logical, intent(in) :: tPeriodic

    !> Structure containing energy contributions
    type(TEnergies), intent(in) :: energy

    !> Stress tensor
    real(dp), intent(in) :: totalStress(:,:)

    !> Derivative with respect to lattice vectors
    real(dp), intent(in) :: totalLatDeriv(:,:)

    !> Energy derivative with respect to atomic coordinates
    real(dp), intent(in), allocatable :: derivs(:,:)

    !> Forces on external charges
    real(dp), intent(in), allocatable :: chrgForces(:,:)

    !> Index of moving atoms
    integer, intent(in) :: indMovedAtom(:)

    !> Unit cell volume
    real(dp), intent(in) :: cellVol

    !> Internal pressure in the unit cell
    real(dp), intent(in) :: cellPressure

    !> File for geometry output
    character(*), intent(in) :: geoOutFile

    !> atoms in the central cell (or device region if transport)
    integer, intent(in) :: iAtInCentralRegion(:)

    integer :: iAt, ii

    if (tScc) then
      if (tConverged) then
        write(fd, "(A)") "SCC converged"
        write(fd, *)
      else
        if (.not. tXlbomd) then
          write(fd, "(A)") "SCC is NOT converged, maximal SCC iterations exceeded"
          write(fd, *)
        end if
      end if
    else
      write(fd, "(A)") "Non-SCC calculation"
      write(fd, *)
    end if

    ! only print excitation energy if 1) its been calculated and 2) its avaialable for a single
    ! state
    if (isLinResp .and. energy%Eexcited /= 0.0_dp) then
      write(fd, format2U) "Excitation Energy", energy%Eexcited, "H", Hartree__eV * energy%Eexcited,&
          & "eV"
      write(fd, *)
    end if

    if (tGeoOpt .or. tMd) then
      write(fd, "(3A)") "Full geometry written in ", trim(geoOutFile), ".{xyz|gen}"
      write(fd, *)
    end if

    if (tPrintForces) then
      write(fd, "(A)") 'Total Forces'
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, 3F20.12)")iAt, -derivs(:, iAt)
      end do
      write(fd, *)
      if (tStress .and. .not. tMd) then
        write(fd, "(A)") 'Total stress tensor'
        do ii = 1, 3
          write(fd, "(3F20.12)") totalStress(:, ii)
        end do
        write(fd, *)
        write(fd, "(A)") 'Total lattice derivs'
        do ii = 1, 3
          write(fd, "(3F20.12)") totalLatDeriv(:, ii)
        end do
        write(fd, *)
      end if

      write(fd, format1Ue) "Maximal derivative component",&
          & maxval(abs(derivs(:,iAtInCentralRegion(:)))), 'au'
      if (size(indMovedAtom) > 0) then
        write(fd, format1Ue) "Max force for moved atoms:",&
            & maxval(abs(derivs(:, indMovedAtom))), 'au'
      end if
      write(fd, *)

      if (allocated(chrgForces)) then
        write(fd, "(A)") "Forces on external charges"
        do ii = 1, size(chrgForces, dim=2)
          write(fd, "(3F20.12)") -chrgForces(:, ii)
        end do
        write(fd, *)
      end if

      if (tPeriodic .and. .not. tMd) then
        write(fd, format1Ue) 'Volume', cellVol, 'au^3'
        if (tStress) then
          write(fd, format2Ue)'Pressure', cellPressure, 'au', cellPressure * au__pascal, 'Pa'
        end if
        write(fd, *)
      end if
    end if

  end subroutine writeDetailedOut4


  !> Fifth group of data for detailed.out
  subroutine writeDetailedOut5(fd, tPrintForces, tSetFillingTemp, tPeriodic, tStress, totalStress,&
      & totalLatDeriv, energy, tempElec, pressure, cellPressure, tempIon)

    !> File ID
    integer, intent(in) :: fd

    !> Print forces on atoms
    logical, intent(in) :: tPrintForces

    !> If the electronic temperature is being set during the run
    logical, intent(in) :: tSetFillingTemp

    !> Is this a periodic geometry
    logical, intent(in) :: tPeriodic

    !> Should the stress tensor/lattice derivatives be printed?
    logical, intent(in) :: tStress

    !> Stress tensor
    real(dp), intent(in) :: totalStress(:,:)

    !> Energy derivatives with respect to lattice vectors
    real(dp), intent(in) :: totalLatDeriv(:,:)

    !> Data structure for energy components
    type(TEnergies), intent(in) :: energy

    !> electron temperature
    real(dp), intent(in) :: tempElec

    !> External pressure
    real(dp), intent(in) :: pressure

    !> Internal pressure in the unit cell
    real(dp), intent(in) :: cellPressure

    !> Atomic kinetic temperature
    real(dp), intent(in) :: tempIon

    integer :: ii

    if (tStress .and. tPrintForces) then
      write(fd, "(A)") 'Total stress tensor'
      do ii = 1, 3
        write(fd, "(3F20.12)") totalStress(:, ii)
      end do
      write(fd, *)
      write(fd, "(A)") 'Total lattice derivs'
      do ii = 1, 3
        write(fd, "(3F20.12)") totalLatDeriv(:, ii)
      end do
      write(fd, *)
    end if

    if (tSetFillingTemp) then
      write(fd, format2U) "Electronic Temperature", tempElec, 'au', tempElec * Hartree__eV,&
          & 'eV'
    end if
    write(fd, format1U) "MD Kinetic Energy", energy%EKin, "H"
    write(fd, format1U) "Total MD Energy", energy%EKin + energy%EMermin, "H"
    if (tPeriodic) then
      write(fd, format2Ue) 'Pressure', cellPressure, 'au', cellPressure * au__pascal, 'Pa'
      if (pressure /= 0.0_dp) then
        write(fd, format2U) 'Gibbs free energy including KE', energy%EGibbsKin, 'H',&
            & Hartree__eV * energy%EGibbsKin, 'eV'
      end if
    end if
    write(fd, format2U) "MD Temperature", tempIon, "H", tempIon / Boltzmann, "K"

  end subroutine writeDetailedOut5


  !> Sixth group of data for detailed.out
  subroutine writeDetailedOut6(fd, energy, tempIon)

    !> File ID
    integer, intent(in) :: fd

    !> Energy contributions
    type(TEnergies), intent(in) :: energy

    !> Atomic kinetic energy
    real(dp), intent(in) :: tempIon

    write(fd, format1U) "MD Kinetic Energy", energy%Ekin, "H"
    write(fd, format2U) "Total MD Energy", energy%EMerminKin, "H",&
        & Hartree__eV * energy%EMerminKin, "eV"
    write(fd, format2U) "MD Temperature", tempIon, "H", tempIon / Boltzmann, "K"
    write(fd, *)

  end subroutine writeDetailedOut6


  !> Seventh group of data for detailed.out
  subroutine writeDetailedOut7(fd, tGeoOpt, tGeomEnd, tMd, tDerivs, eField, dipoleMoment,&
      & deltaDftb, eFieldScaling, dipoleMessage)

    !> File ID
    integer, intent(in) :: fd

    !> Is the geometry changing during the run
    logical, intent(in) :: tGeoOpt

    !> Did the geometry changes successfully complete
    logical, intent(in) :: tGeomEnd

    !> Is this a molecular dynamics run
    logical, intent(in) :: tMd

    !> Are finite difference derivatives being computed
    logical, intent(in) :: tDerivs

    !> External electric field (if allocated)
    type(TEField), intent(in), allocatable :: eField

    !> dipole moment
    real(dp), intent(inout), allocatable :: dipoleMoment(:,:)

    !> type for DFTB determinants
    type(TDftbDeterminants), intent(in) :: deltaDftb

    !> Any dielectric environment scaling
    class(TScaleExtEField), intent(in) :: eFieldScaling

    !> Optional extra message about dipole moments
    character(*), intent(in) :: dipoleMessage

    if (allocated(dipoleMoment)) then
      if (len(trim(dipoleMessage))>0) then
        write(fd, "(A)")trim(dipoleMessage)
      end if
      if (deltaDftb%isNonAufbau) then
        if (deltaDftb%iGround > 0) then
          write(fd, "(A, 3F14.8, A)")'S0 Dipole moment:',&
              & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iGround)), ' au'
          write(fd, "(A, 3F14.8, A)")'S0 Dipole moment:',&
              & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iGround)) * au__Debye,&
              & ' Debye'
          write(fd, *)
        end if
        if (deltaDftb%iTriplet > 0) then
          write(fd, "(A, 3F14.8, A)")'T1 Dipole moment:',&
              & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iTriplet)), ' au'
          write(fd, "(A, 3F14.8, A)")'T1 Dipole moment:',&
              & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iTriplet)) * au__Debye,&
              & ' Debye'
          write(fd, *)
        end if
        if (deltaDftb%isSpinPurify) then
          write(fd, "(A, 3F14.8, A)")'S1 Dipole moment:',&
              & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iFinal)), ' au'
          write(fd, "(A, 3F14.8, A)")'S1 Dipole moment:',&
              & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iFinal)) * au__Debye,&
              & ' Debye'
          write(fd, *)
          if (deltaDftb%isSpinPurify .and. deltaDftb%iGround > 0) then
            write(fd, "(A, 3F14.8, A)")'S0 -> S1 transition dipole:',&
                & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iFinal))&
                & -eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iGround)), ' au'
          end if
        else
          write(fd, "(A, 3F14.8, A)")'Mixed state Dipole moment:',&
              & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iMixed)), ' au'
          write(fd, "(A, 3F14.8, A)")'Mixed state Dipole moment:',&
              & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iMixed))&
              & * au__Debye, ' Debye'
          write(fd, *)
        end if
      else
        write(fd, "(A, 3F14.8, A)")'Dipole moment:',&
            & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iGround)), ' au'
        write(fd, "(A, 3F14.8, A)")'Dipole moment:',&
            & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,deltaDftb%iGround)) * au__Debye,&
            & ' Debye'
        write(fd, *)
      end if
    end if

    if (allocated(eField)) then
      if (allocated(eField%EFieldStrength)) then
        if (eFieldScaling%isRescaled) then
          write(fd, format1U1e) 'Effective external E field', eField%absEField, 'au',&
              & eField%absEField * au__V_m, 'V/m'
        else
          write(fd, format1U1e) 'External E field', eField%absEField, 'au',&
              & eField%absEField * au__V_m, 'V/m'
        end if
      end if
    end if

    if (tGeoOpt) then
      if (tGeomEnd) then
        write(fd, "(A)") "Geometry converged"
      else
        write(fd, "(A)") "!!! Geometry did NOT converge!"
      end if
    elseif (tMD) then
      if (tGeomEnd) then
        write(fd, "(A)") "Molecular dynamics completed"
      else
        write(fd, "(A)") "!!! Molecular dynamics terminated abnormally!"
      end if
    elseif (tDerivs) then
      if (tGeomEnd) then
        write(fd, "(A)") "Second derivatives completed"
      else
        write(fd, "(A)") "!!! Second derivatives terminated abnormally!"
      end if
    end if
    write(fd,*)

  end subroutine writeDetailedOut7

  !> Eighth group of data for detailed.out (Born effective charges)
  subroutine writeDetailedOut8(fd, born)

    !> File ID
    integer, intent(in) :: fd

    !> Born charges
    real(dp), intent(in) :: born(:,:)

    integer :: ii

    write(fd,*)'Born charges/dipole derivatives wrt. atom positions (e)'
    do ii = 1, size(born,dim=2), 3
      write(fd,"(A,1X,I0)")'Atom',ii/3+1
      write(fd,"(3F12.6)")born(:,ii:ii+2)
    end do

  end subroutine writeDetailedOut8


  !> Nineth group of data for detailed.out (density of states at Fermi energy)
  subroutine writeDetailedOut9(fd, neFermi)

    !> File ID
    integer, intent(in) :: fd

    !> Electrons at the Fermi energy (if metallic and evaluated)
    real(dp), allocatable, intent(in) :: neFermi(:)

    if (allocated(neFermi)) then
      write(fd,"(A)", advance='no')'Density of states at the Fermi energy (a.u.): '
      if (size(neFermi)==2) then
        write(fd,"(E12.6,A,E12.6,A)")neFermi(1), ' (up) ', neFermi(2), ' (down)'
      else
        write(fd,"(E12.6)")neFermi
      end if
    end if

  end subroutine writeDetailedOut9


  !> Tenth group of data for detailed.out (derivatives with respect to an external electric field)
  subroutine writeDetailedOut10(fd, orb, polarisability, dqOut, dEfdE)

    !> File ID
    integer, intent(in) :: fd

    !> Type containing atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Static electric polarisability
    real(dp), intent(in), allocatable :: polarisability(:,:,:)

    !> Derivative of Mulliken charges wrt to electric field, if required
    real(dp), allocatable, intent(in) :: dqOut(:,:,:,:)

    !> Derivative of the Fermi energy with respect to electric field
    real(dp), allocatable, intent(in) :: dEfdE(:,:)

    integer :: iCart, iAt, nAtom, iS, nSpin, iOmega

    if (allocated(dqOut)) then
      nAtom = size(dqOut, dim=2)
      nSpin = size(dqOut, dim=3)
      do iCart = 1, 3
        write(fd,"(A)")'Atomic charge derivatives (a.u.), d q / d E_' //&
            & trim(quaternionName(iCart+1)) //':'
        select case(nSpin)
        case(1)
          do iAt = 1, nAtom
            write(fd,"(I4,1X,4E20.12)")iAt, sum(dqOut(:orb%nOrbAtom(iAt), iAt, 1, iCart))
          end do
        case(2)
          do iAt = 1, nAtom
            write(fd,"(I4,1X,A,4E20.12)")iAt, 'u',&
                & 0.5_dp*(sum(dqOut(:orb%nOrbAtom(iAt), iAt, 1, iCart))&
                & + sum(dqOut(:orb%nOrbAtom(iAt), iAt, 2, iCart)))
            write(fd,"(5X,A,4E20.12)")'d',&
                & 0.5_dp*(sum(dqOut(:orb%nOrbAtom(iAt), iAt, 1, iCart))&
                & - sum(dqOut(:orb%nOrbAtom(iAt), iAt, 2, iCart)))
          end do
        case(4)
          do iAt = 1, nAtom
            do iS = 1, nSpin
              if (iS == 1) then
                write(fd,"(I4,1X,A,4E20.12)")iAt, quaternionName(iS),&
                    & sum(dqOut(:orb%nOrbAtom(iAt), iAt, iS, iCart))
              else
                write(fd,"(5X,A,4E20.12)")quaternionName(iS),&
                    & sum(dqOut(:orb%nOrbAtom(iAt), iAt, iS, iCart))
              end if
            end do
          end do
        end select
        write(fd,*)
      end do
    end if

    if (allocated(dEfdE)) then
      write(fd,"(A)")'Derivative of Fermi energy with respect to electric field'
      do iCart = 1, 3
        write(fd,"(1X,A,2E20.12)")'d E_f / d E_'//trim(quaternionName(iCart+1))//':',&
            & dEfdE(:,iCart)
      end do
    end if

    if (allocated(polarisability)) then
      write(fd,*)
      write(fd,"(A)")'Electric polarisability (a.u.)'
      do iOmega = 1, size(polarisability, dim=3)
        do iCart = 1, 3
          write(fd,"(3E20.12)")polarisability(:, iCart, iOmega)
        end do
      end do
      write(fd,*)
    end if

  end subroutine writeDetailedOut10


  !> First group of output data during molecular dynamics
  subroutine writeMdOut1(fd, iGeoStep, pMdIntegrator)

    !> File ID
    integer, intent(in) :: fd

    !> Number of the current geometry step
    integer, intent(in) :: iGeoStep

    !> Molecular dynamics integrator
    type(TMdIntegrator), intent(in) :: pMdIntegrator

    write(fd, "(A, 1X, I0)") "MD step:", iGeoStep
    call state(pMdIntegrator, fd)

  end subroutine writeMdOut1

  !> Second group of output data during molecular dynamics
  subroutine writeMdOut2(fd, tStress, tPeriodic, tBarostat, isLinResp, eField, tFixEf,&
      & tPrintMulliken, energy, energiesCasida, latVec, cellVol, cellPressure, pressure, tempIon,&
      & qOutput, q0, dipoleMoment, eFieldScaling, dipoleMessage)

    !> File ID
    integer, intent(in) :: fd

    !> Is the stress tensor to be printed?
    logical, intent(in) :: tStress

    !> Is this a periodic geometry
    logical, intent(in) :: tPeriodic

    !> Is a barostat in use
    logical, intent(in) :: tBarostat

    !> Is linear response excitation being used
    logical, intent(in) :: isLinResp

    !> External electric field (if allocated)
    type(TEField), intent(in), allocatable :: eField

    !> Is the  Fermi level fixed
    logical, intent(in) :: tFixEf

    !> Should Mulliken charges be printed, hence total charge here
    logical, intent(in) :: tPrintMulliken

    !> energy contributions
    type(TEnergies), intent(in) :: energy

    !> excitation energies, if allocated
    real(dp), allocatable, intent(inout) :: energiesCasida(:)

    !> Lattice vectors if periodic
    real(dp), intent(in) :: latVec(:,:)

    !> Unit cell volume
    real(dp), intent(in) :: cellVol

    !> Internal cell pressure
    real(dp), intent(in) :: cellPressure

    !> External applied pressure
    real(dp), intent(in) :: pressure

    !> Atomic kinetic energy
    real(dp), intent(in) :: tempIon

    !> Output atomic charges (if SCC)
    real(dp), intent(in) :: qOutput(:,:,:)

    !> Reference atomic charges
    real(dp), intent(in) :: q0(:,:,:)

    !> dipole moment if available
    real(dp), intent(inout), allocatable :: dipoleMoment(:,:)

    !> Any dielectric environment scaling
    class(TScaleExtEField), intent(in) :: eFieldScaling

    !> Optional extra message about dipole moments
    character(*), intent(in) :: dipoleMessage

    integer :: ii
    character(lc) :: strTmp

    if (tStress) then
      if (tBarostat) then
        write(fd, "(A)") 'Lattice vectors (A)'
        do ii = 1, 3
          write(fd, "(3E24.8)") latVec(:,ii) * Bohr__AA
        end do
        write(fd, format2Ue) 'Volume', cellVol, 'au^3', (Bohr__AA**3) * cellVol, 'A^3'
      end if
      if (tPeriodic) then
        write(fd, format2Ue) 'Pressure', cellPressure, 'au', cellPressure * au__pascal, 'Pa'
        if (pressure /= 0.0_dp) then
          write(fd, format2U) 'Gibbs free energy', energy%EGibbs, 'H',&
              & Hartree__eV * energy%EGibbs,'eV'
          write(fd, format2U) 'Gibbs free energy including KE', energy%EGibbsKin, 'H',&
              & Hartree__eV * energy%EGibbsKin, 'eV'
        end if
      end if
    end if
    if (isLinResp) then
      if (energy%Eexcited /= 0.0_dp) then
        write(fd, format2U) "Excitation Energy", energy%Eexcited, "H",&
            & Hartree__eV * energy%Eexcited, "eV"
      end if
      if (allocated(energiesCasida)) then
        do ii = 1, size(energiesCasida)
          write(strTmp,"('Excitation ',I0)")ii
          write(fd, format2U) trim(strTmp), energiesCasida(ii), "H",&
              & Hartree__eV * energiesCasida(ii), "eV"
        end do
      end if
    end if
    write(fd, format2U) 'Potential Energy', energy%EMermin,'H', energy%EMermin * Hartree__eV, 'eV'
    write(fd, format2U) 'MD Kinetic Energy', energy%Ekin, 'H', energy%Ekin * Hartree__eV, 'eV'
    write(fd, format2U) 'Total MD Energy', energy%EMerminKin, 'H',&
        & energy%EMerminKin * Hartree__eV, 'eV'
    write(fd, format2U) 'MD Temperature', tempIon, 'au', tempIon / Boltzmann, 'K'
    if (allocated(eField)) then
      if (allocated(eField%EFieldStrength)) then
        write(fd, format1U1e) 'External E field', eField%absEField, 'au',&
            & eField%absEField * au__V_m, 'V/m'
      end if
    end if
    if (tFixEf .and. tPrintMulliken) then
      write(fd, "(A, F14.8)") 'Net charge: ', sum(q0(:, :, 1) - qOutput(:, :, 1))
    end if
    if (allocated(dipoleMoment)) then
      if (len(trim(dipoleMessage))>0) then
        write(fd, "(A)")trim(dipoleMessage)
      end if
      ii = size(dipoleMoment, dim=2)
      write(fd, "(A, 3F14.8, 1X,A)") 'Dipole moment:',&
          & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,ii)),  'au'
      write(fd, "(A, 3F14.8, 1X, A)") 'Dipole moment:',&
          & eFieldScaling%scaledSoluteDipole(dipoleMoment(:,ii)) * au__Debye,  'Debye'
    end if

  end subroutine writeMdOut2


  !> Write out charges.
  subroutine writeCharges(fCharges, tWriteAscii, orb, qInput, qBlockIn, qiBlockIn, deltaRhoIn,&
      & nAtInCentralRegion, multipoles)

    !> File name for charges to be written to
    character(*), intent(in) :: fCharges

    !> Charges should be output in ascii (T) or binary (F)
    logical, intent(in) :: tWriteAscii

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> input charges
    real(dp), intent(in) :: qInput(:,:,:)

    !> Block populations if present
    real(dp), intent(in), allocatable :: qBlockIn(:,:,:,:)

    !> Imaginary part of block populations if present
    real(dp), intent(in), allocatable :: qiBlockIn(:,:,:,:)

    !> Full density matrix with on-diagonal adjustment
    real(dp), intent(in), allocatable :: deltaRhoIn(:)

    !> Number of atoms in central region (atoms outside this will have charges suplied from
    !> elsewhere)
    integer, intent(in) :: nAtInCentralRegion

    !> Atomic multipoles, if relevant
    type(TMultipole), intent(in), optional :: multipoles

    call writeQToFile(qInput, fCharges, tWriteAscii, orb, qBlockIn, qiBlockIn, deltaRhoIn,&
        & nAtInCentralRegion, multipoles)
    if (tWriteAscii) then
      write(stdOut, "(A,A)") '>> Charges saved for restart in ', trim(fCharges)//'.dat'
    else
      write(stdOut, "(A,A)") '>> Charges saved for restart in ', trim(fCharges)//'.bin'
    end if

  end subroutine writeCharges


  !> Writes Hamiltonian and overlap matrices and stops program execution.
  subroutine writeHSAndStop(env, tWriteHS, tWriteRealHS, tRealHS, over, neighbourList,&
      & nNeighbourSK, iAtomStart, iPair, img2CentCell, kPoint, iCellVec, cellVec, ham, iHam)

    !> Environment settings
    type(TEnvironment), intent(inout) :: env

    !> Write dense hamiltonian and overlap matrices
    logical, intent(in) :: tWriteHS

    !> write sparse hamiltonian and overlap matrices
    logical, intent(in) :: tWriteRealHS

    !> Is the hamiltonian real?
    logical, intent(in) :: tRealHS

    !> overlap in sparse storage
    real(dp), intent(in) :: over(:)

    !> atomic neighbours
    type(TNeighbourList), intent(in) :: neighbourList

    !> number of neighbours for each central cell atom
    integer, intent(in) :: nNeighbourSK(:)

    !> Dense matrix indexing for atomic blocks
    integer, intent(in) :: iAtomStart(:)

    !> sparse matrix indexing for atomic blocks
    integer, intent(in) :: iPair(:,:)

    !> Image atoms to central cell
    integer, intent(in) :: img2CentCell(:)

    !> k-points
    real(dp), intent(in) :: kPoint(:,:)

    !> index  for which unit cell an atom is in
    integer, intent(in) :: iCellVec(:)

    !> vectors to unit cells, in lattice constant units
    real(dp), intent(in) :: cellVec(:,:)

    !> sparse hamiltonian
    real(dp), intent(in) :: ham(:,:)

    !> imaginary part of hamiltonian (used if allocated)
    real(dp), allocatable, intent(in) :: iHam(:,:)

    real(dp), allocatable :: hamUpDown(:,:)
    integer :: nSpin

    nSpin = size(ham, dim=2)

    ! Sanity check, although this should have been caught in initprogram already.
    if (nSpin == 4) then
      call error('Internal error: Hamiltonian writing for Pauli-Hamiltoninan not implemented')
    end if

    hamUpDown = ham
    call qm2ud(hamUpDown)

    ! Write out matrices if necessary and quit.
    call writeHS(env, tWriteHS, tWriteRealHS, tRealHS, hamUpDown, over, neighbourList%iNeighbour,&
        & nNeighbourSK, iAtomStart, iPair, img2CentCell, kPoint, iCellVec, cellVec, iHam)
    write(stdOut, "(A)") "Hamilton/Overlap written, exiting program."
    call env%destruct()
    call destructGlobalEnv()
    call abortProgram()

  end subroutine writeHSAndStop


  !> Invokes the writing routines for the Hamiltonian and overlap matrices.
  subroutine writeHS(env, tWriteHS, tWriteRealHS, tRealHS, ham, over, iNeighbour, nNeighbourSK,&
      & iAtomStart, iPair, img2CentCell, kPoint, iCellVec, cellVec, iHam)

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Should the hamiltonian and overlap be written out as dense matrices
    logical, intent(in) :: tWriteHS

    !> Should the (sparse) real space storage hamiltonian and overlap
    logical, intent(in) :: tWriteRealHS

    !> Is the hamiltonian real?
    logical, intent(in) :: tRealHS

    !> sparse hamiltonian matrix
    real(dp), intent(in) :: ham(:,:)

    !> sparse overlap matrix
    real(dp), intent(in) :: over(:)

    !> Atomic neighbour data
    integer, intent(in) :: iNeighbour(0:,:)

    !> number of atomic neighbours for each atom
    integer, intent(in) :: nNeighbourSK(:)

    !> Index array for start of atomic block in dense matrices
    integer, intent(in) :: iAtomStart(:)

    !> Index array for start of atomic block in sparse matrices
    integer, intent(in) :: iPair(0:,:)

    !> Index array for images of atoms
    integer, intent(in) :: img2CentCell(:)

    !> The kpoints in the system
    real(dp), intent(in) :: kPoint(:,:)

    !> Index from atom to which unit cell it belongs
    integer, intent(in) :: iCellVec(:)

    !> Vectors to specific unit cells
    real(dp), intent(in) :: cellVec(:,:)

    !> Imaginary part of the hamiltonian if present
    real(dp), intent(in), allocatable :: iHam(:,:)

    integer :: iS, nSpin

    nSpin = size(ham, dim=2)

    if (tWriteRealHS) then
      do iS = 1, nSpin
        call writeSparse("hamreal" // i2c(iS) // ".dat", ham(:,iS), iNeighbour, nNeighbourSK,&
            & iAtomStart, iPair, img2CentCell, iCellVec, cellVec)
        if (allocated(iHam)) then
          call writeSparse("hamimag" // i2c(iS) // ".dat", iHam(:,iS), iNeighbour, nNeighbourSK,&
              & iAtomStart, iPair, img2CentCell, iCellVec, cellVec)
        end if
      end do
      call writeSparse("overreal.dat", over, iNeighbour, nNeighbourSK, iAtomStart, iPair,&
          & img2CentCell, iCellVec, cellVec)
    end if
    if (tWriteHS) then
      if (tRealHS) then
        do iS = 1, nSpin
          call writeSparseAsSquare(env, "hamsqr" // i2c(iS) // ".dat", ham(:,iS), iNeighbour,&
              & nNeighbourSK, iAtomStart, iPair, img2CentCell)
        end do
        call writeSparseAsSquare(env, "oversqr.dat", over, iNeighbour, nNeighbourSK, iAtomStart,&
            & iPair, img2CentCell)
      else
        do iS = 1, nSpin
          call writeSparseAsSquare(env, "hamsqr" // i2c(iS) // ".dat", ham(:,iS), kPoint,&
              & iNeighbour, nNeighbourSK, iAtomStart, iPair, img2CentCell, iCellVec, cellVec)
        end do
        call writeSparseAsSquare(env, "oversqr.dat", over, kPoint, iNeighbour, nNeighbourSK,&
            & iAtomStart, iPair, img2CentCell, iCellVec, cellVec)
      end if
    end if

  end subroutine writeHS


  !> Write current geometry to disc
  subroutine writeCurrentGeometry(geoOutFile, pCoord0Out, tLatOpt, tMd, tAppendGeo, tFracCoord,&
      & tPeriodic, tHelical, tPrintMulliken, species0, speciesName, latVec, origin, iGeoStep,&
      & iLatGeoStep, nSpin, qOutput, velocities)

    !>  file for geometry output
    character(*), intent(in) :: geoOutFile

    !> How central cell atoms are represented
    real(dp), intent(in) :: pCoord0Out(:,:)

    !> is the lattice being optimised?
    logical, intent(in) :: tLatOpt

    !> Is this a molecular dynamics calculation?
    logical, intent(in) :: tMd

    !> should the geometry be added to the end, or the file cleared first
    logical, intent(in) :: tAppendGeo

    !> are fractional GEN files expected
    logical, intent(in) :: tFracCoord

    !> Is the geometry periodic?
    logical, intent(in) :: tPeriodic

    !> Is the geometry helical?
    logical, intent(in) :: tHelical

    !> should Mulliken charges be printed
    logical, intent(in) :: tPrintMulliken

    !> species of atoms in the central cell
    integer, intent(in) :: species0(:)

    !> label for each atomic chemical species
    character(*), intent(in) :: speciesName(:)

    !> lattice vectors
    real(dp), intent(in) :: latVec(:,:)

    !> Origin for periodic coordinates
    real(dp), intent(in) :: origin(:)

    !> current geometry step
    integer, intent(in) :: iGeoStep

    !> current lattice step
    integer, intent(in) :: iLatGeoStep

    !> Number of spin channels
    integer, intent(in) :: nSpin

    !> charges
    real(dp), intent(in), allocatable :: qOutput(:,:,:)

    !> atomic velocities
    real(dp), intent(in), allocatable :: velocities(:,:)

    real(dp), allocatable :: tmpMatrix(:,:)
    integer :: nAtom
    integer :: ii, jj
    character(lc) :: comment, fname

    nAtom = size(pCoord0Out, dim=2)

    fname = trim(geoOutFile) // ".gen"
    if (tPeriodic .or. tHelical) then
      call writeGenFormat(fname, pCoord0Out, species0, speciesName, latVec, origin, tFracCoord)
    else
      call writeGenFormat(fname, pCoord0Out, species0, speciesName)
    end if

    fname = trim(geoOutFile) // ".xyz"
    if (tLatOpt) then
      write(comment, "(A, I0, A, I0)") '** Geometry step: ', iGeoStep, ', Lattice step: ',&
          & iLatGeoStep
    elseif (tMD) then
      write(comment, "(A, I0)") 'MD iter: ', iGeoStep
    else
      write(comment,"(A, I0)") 'Geometry Step: ', iGeoStep
    end if

    if (tPrintMulliken) then
      ! For non-colinear spin without velocities write magnetisation into the velocity field
      if (nSpin == 4 .and. .not. allocated(velocities)) then
        allocate(tmpMatrix(3, nAtom))
        do jj = 1, nAtom
          do ii = 1, 3
            tmpMatrix(ii,jj) = sum(qOutput(:, jj, ii + 1))
          end do
        end do
        ! convert by the inverse of the scaling used in writeXYZFormat
        tmpMatrix(:,:) = tmpMatrix * au__fs / (1000_dp * Bohr__AA)
        call writeXYZFormat(fname, pCoord0Out, species0, speciesName,&
            & charges=sum(qOutput(:,:,1), dim=1), velocities=tmpMatrix, comment=comment,&
            & append=tAppendGeo)
      else if (allocated(velocities)) then
        call writeXYZFormat(fname, pCoord0Out, species0, speciesName,&
            & charges=sum(qOutput(:,:,1),dim=1), velocities=velocities, comment=comment,&
            & append=tAppendGeo)
      else
        call writeXYZFormat(fname, pCoord0Out, species0, speciesName,&
            & charges=sum(qOutput(:,:,1),dim=1), comment=comment, append=tAppendGeo)
      end if
    else if (allocated(velocities)) then
      call writeXYZFormat(fname, pCoord0Out, species0, speciesName, velocities=velocities,&
          & comment=comment, append=tAppendGeo)
    else
      call writeXYZFormat(fname, pCoord0Out, species0, speciesName, comment=comment,&
          & append=tAppendGeo)
    end if

  end subroutine writeCurrentGeometry


  !> Write out final status of the geometry driver.
  subroutine writeFinalDriverStatus(tGeoOpt, tGeomEnd, tMd, tDerivs)

    !> Is the geometry being optimised?
    logical, intent(in) :: tGeoOpt

    !> Has the optimisation terminated?
    logical, intent(in) :: tGeomEnd

    !> Is this a molecular dynamics calculation?
    logical, intent(in) :: tMd

    !> Are finite difference derivatives being calculated?
    logical, intent(in) :: tDerivs

    if (tGeoOpt) then
      if (tGeomEnd) then
        write(stdOut, "(/, A)") "Geometry converged"
      else
        call warning("!!! Geometry did NOT converge!")
      end if
    elseif (tMD) then
      if (tGeomEnd) then
        write(stdOut, "(/, A)") "Molecular dynamics completed"
      else
        call warning("!!! Molecular dynamics terminated abnormally!")
      end if
    elseif (tDerivs) then
      if (tGeomEnd) then
        write(stdOut, "(/, A)") "Second derivatives completed"
      else
        call warning("!!! Second derivatives terminated abnormally!")
      end if
    end if

  end subroutine writeFinalDriverStatus


  !> Prints geometry step information to standard out
  subroutine printGeoStepInfo(tCoordOpt, tLatOpt, iLatGeoStep, iGeoStep)

    !> Are coordinates being optimised
    logical, intent(in) :: tCoordOpt

    !> Is the lattice being optimised
    logical, intent(in) :: tLatOpt

    !> Which geometry step is this
    integer, intent(in) :: iGeoStep

    !> How many lattice optimisation steps have occurred
    integer, intent(in) :: iLatGeoStep

    write(stdOut, '(/, A)') repeat('-', 80)
    if (tCoordOpt .and. tLatOpt) then
      write(stdOut, "(/, A, I0, A, I0,/)") '***  Geometry step: ', iGeoStep, ', Lattice step: ',&
          & iLatGeoStep
    else
      write(stdOut, "(/, A, I0, /)") '***  Geometry step: ', iGeoStep
    end if

  end subroutine printGeoStepInfo


  !> Prints the line above the start of the SCC cycle data
  subroutine printSccHeader()

    write(stdOut, "(A5, A18, A18, A18)") "iSCC", " Total electronic ", "  Diff electronic ",&
        & "     SCC error    "

  end subroutine printSccHeader

  !> Prints the line above the start of the REKS SCC cycle data
  subroutine printReksSccHeader(reks)

    !> data type for REKS
    type(TReksCalc), intent(in) :: reks

    select case (reks%reksAlg)
    case (reksTypes%noReks)
    case (reksTypes%ssr22)
      write(stdOut,"(1X,A5,A20,A20,A13,A15)") "iSCC", "       reks energy  ", &
          & "      Diff energy   ", "      x_a    ", "   SCC error   "
    case (reksTypes%ssr44)
      call error("SSR(4,4) is not implemented yet")
    end select

  end subroutine printReksSccHeader

  subroutine printBlankLine()
    write(stdOut,*)
  end subroutine printBlankLine

  !> Prints info about scc convergence.
  subroutine printSccInfo(tDftbU, iSccIter, Eelec, diffElec, sccErrorQ)

    !> Are orbital potentials being used
    logical, intent(in) :: tDftbU

    !> Iteration count
    integer, intent(in) :: iSccIter

    !> electronic energy
    real(dp), intent(in) :: Eelec

    !> Difference in electronic energy between this iteration and the last
    real(dp), intent(in) :: diffElec

    !> Maximum charge difference between input and output
    real(dp), intent(in) :: sccErrorQ

    if (tDFTBU) then
      write(stdOut, "(I5,E18.8,E18.8,E18.8)") iSCCIter, Eelec, diffElec, sccErrorQ
    else
      write(stdOut, "(I5,E18.8,E18.8,E18.8)") iSCCIter, Eelec, diffElec, sccErrorQ
    end if

  end subroutine printSccInfo


  !> Prints info about scc convergence.
  subroutine printReksSccInfo(iSccIter, Eavg, diffTotal, sccErrorQ, reks)

    !> Iteration count
    integer, intent(in) :: iSccIter

    !> Total energy for averaged state in REKS
    real(dp), intent(in) :: Eavg

    !> Difference in total energy between this iteration and the last
    real(dp), intent(in) :: diffTotal

    !> Maximum charge difference between input and output
    real(dp), intent(in) :: sccErrorQ

    !> data type for REKS
    type(TReksCalc), intent(in) :: reks

    ! print out the iteration information
    select case (reks%reksAlg)
    case (reksTypes%noReks)
    case (reksTypes%ssr22)
      write(stdOut,"(I5,4x,F16.10,3x,F16.10,3x,F10.6,3x,F11.8)") iSCCIter, Eavg,&
          & diffTotal, reks%FONs(1,1) * 0.5_dp, sccErrorQ
    case (reksTypes%ssr44)
      call error("SSR(4,4) is not implemented yet")
    end select

  end subroutine printReksSccInfo


  !> Prints current total energies
  subroutine printEnergies(energy, electronicSolver, deltaDftb, outUnit)

    !> energy components, potentially from multiple determinants
    type(TEnergies), intent(in) :: energy(:)

    !> Electronic solver information
    type(TElectronicSolver), intent(in) :: electronicSolver

    !> type for DFTB determinants
    type(TDftbDeterminants), intent(in) :: deltaDftb

    !> Optional unit to print out the results
    integer, intent(in), optional :: outUnit

    integer :: iUnit

    if (present(outUnit)) then
      iUnit = outUnit
    else
      iUnit = stdOut
    end if

    write(iUnit, *)

    if (deltaDftb%iGround > 0) then

      if (deltaDftb%isNonAufbau) then
        write(iUnit, format2U) "Ground State Total Energy", energy(deltaDftb%iGround)%Etotal,"H",&
            & Hartree__eV * energy(deltaDftb%iGround)%Etotal,"eV"
        if (electronicSolver%providesEigenvals) then
          write(iUnit, format2U) "Ground State Extrapolated to 0K",&
              & energy(deltaDftb%iGround)%Ezero, "H",&
              & Hartree__eV * energy(deltaDftb%iGround)%Ezero, "eV"
        end if
        if (electronicSolver%providesElectronEntropy) then
          write(iUnit, format2U) "Total Ground State Mermin egy",&
              & energy(deltaDftb%iGround)%EMermin, "H",&
              & Hartree__eV * energy(deltaDftb%iGround)%EMermin, "eV"
        end if
        if (electronicSolver%providesFreeEnergy) then
          write(iUnit, format2U) 'Ground State Force related egy',&
              & energy(deltaDftb%iGround)%EForceRelated, 'H',&
              & energy(deltaDftb%iGround)%EForceRelated * Hartree__eV, 'eV'
        end if
      else
        write(iUnit, format2U) "Total Energy", energy(deltaDftb%iGround)%Etotal,"H",&
            & Hartree__eV * energy(deltaDftb%iGround)%Etotal,"eV"
        if (electronicSolver%providesEigenvals) then
          write(iUnit, format2U) "Extrapolated to 0K", energy(deltaDftb%iGround)%Ezero,&
              & "H", Hartree__eV * energy(deltaDftb%iGround)%Ezero, "eV"
        end if
        if (electronicSolver%providesElectronEntropy) then
          write(iUnit, format2U) "Total Mermin free energy", energy(deltaDftb%iGround)%EMermin,&
              & "H", Hartree__eV * energy(deltaDftb%iGround)%EMermin, "eV"
        end if
        if (electronicSolver%providesFreeEnergy) then
          write(iUnit, format2U) 'Force related energy', energy(deltaDftb%iGround)%EForceRelated,&
              & 'H', energy(deltaDftb%iGround)%EForceRelated * Hartree__eV, 'eV'
        end if
      end if
      write(iUnit,*)
    end if

    if (deltaDftb%iTriplet > 0) then

      write(iUnit, format2U) "Triplet State Total Energy", energy(deltaDftb%iTriplet)%Etotal,"H",&
          & Hartree__eV * energy(deltaDftb%iTriplet)%Etotal,"eV"
      if (electronicSolver%providesEigenvals) then
        write(iUnit, format2U) "Triplet State Extrapolated to 0K",&
            & energy(deltaDftb%iTriplet)%Ezero, "H",&
            & Hartree__eV * energy(deltaDftb%iTriplet)%Ezero, "eV"
      end if
      if (electronicSolver%providesElectronEntropy) then
        write(iUnit, format2U) "Triplet State Mermin free egy",&
            & energy(deltaDftb%iTriplet)%EMermin, "H",&
            & Hartree__eV * energy(deltaDftb%iTriplet)%EMermin, "eV"
      end if
      if (electronicSolver%providesFreeEnergy) then
        write(iUnit, format2U) 'Triplet State Force related egy',&
            & energy(deltaDftb%iTriplet)%EForceRelated, 'H',&
            & energy(deltaDftb%iTriplet)%EForceRelated * Hartree__eV, 'eV'
      end if
      write(iUnit,*)
    end if

    if (deltaDftb%iMixed > 0) then

      if (deltaDftb%isSpinPurify) then

        write(iUnit, format2U) "Purified State Total Energy", energy(deltaDftb%iFinal)%Etotal,"H",&
            & Hartree__eV * energy(deltaDftb%iFinal)%Etotal,"eV"
        if (electronicSolver%providesEigenvals) then
          write(iUnit, format2U) "Purified Extrapolated 0K",&
              & energy(deltaDftb%iFinal)%Ezero, "H",&
              & Hartree__eV * energy(deltaDftb%iFinal)%Ezero, "eV"
        end if
        if (electronicSolver%providesElectronEntropy) then
          write(iUnit, format2U) "Purified State Mermin free egy",&
              & energy(deltaDftb%iFinal)%EMermin, "H",&
              & Hartree__eV * energy(deltaDftb%iFinal)%EMermin, "eV"
        end if
        if (electronicSolver%providesFreeEnergy) then
          write(iUnit, format2U) 'Purified Force related egy',&
              & energy(deltaDftb%iFinal)%EForceRelated, 'H',&
              & energy(deltaDftb%iFinal)%EForceRelated * Hartree__eV, 'eV'
        end if

        if (deltaDftb%iGround > 0) then
          if (electronicSolver%providesFreeEnergy) then
            write(iUnit, *)
            write(iUnit, format2U) 'S0 -> T1',&
                & energy(deltaDftb%iTriplet)%EForceRelated&
                & - energy(deltaDftb%iGround)%EForceRelated, 'H',&
                & (energy(deltaDftb%iTriplet)%EForceRelated&
                & - energy(deltaDftb%iGround)%EForceRelated) * Hartree__eV, 'eV'
            write(iUnit, format2U) 'S0 -> S1',&
                & energy(deltaDftb%iFinal)%EForceRelated-energy(deltaDftb%iGround)%EForceRelated,&
                & 'H',&
                & (energy(deltaDftb%iFinal)%EForceRelated-energy(deltaDftb%iGround)%EForceRelated)&
                & * Hartree__eV, 'eV'
          end if
        end if

      else

        write(iUnit, format2U) "Mixed State Total Energy", energy(deltaDftb%iMixed)%Etotal,"H",&
            & Hartree__eV * energy(deltaDftb%iMixed)%Etotal,"eV"
        if (electronicSolver%providesEigenvals) then
          write(iUnit, format2U) "Mixed Extrapolated to 0K",&
              & energy(deltaDftb%iMixed)%Ezero, "H",&
              & Hartree__eV * energy(deltaDftb%iMixed)%Ezero, "eV"
        end if
        if (electronicSolver%providesElectronEntropy) then
          write(iUnit, format2U) "Mixed State Mermin free egy",&
              & energy(deltaDftb%iMixed)%EMermin, "H",&
              & Hartree__eV * energy(deltaDftb%iMixed)%EMermin, "eV"
        end if
        if (electronicSolver%providesFreeEnergy) then
          write(iUnit, format2U) 'Mixed State Force related egy',&
              & energy(deltaDftb%iMixed)%EForceRelated, 'H',&
              & energy(deltaDftb%iMixed)%EForceRelated * Hartree__eV, 'eV'
        end if

      end if

      write(iUnit,*)

    end if

  end subroutine printEnergies


  !> Prints cell volume.
  subroutine printVolume(cellVol)

    !> unit cell volume
    real(dp), intent(in) :: cellVol

    write(stdOut, format2Ue) 'Volume', cellVol, 'au^3', (Bohr__AA**3) * cellVol, 'A^3'

  end subroutine printVolume


  !> Prints pressure and free energy.
  subroutine printPressureAndFreeEnergy(pressure, cellPressure, EGibbs)

    !> applied external pressure
    real(dp), intent(in) :: pressure

    !> internal cell pressure
    real(dp), intent(in) :: cellPressure

    !> Gibbs free energy (E -TS_elec +pV)
    real(dp), intent(in) :: EGibbs

    write(stdOut, format2Ue) 'Pressure', cellPressure, 'au', cellPressure * au__pascal, 'Pa'
    if (abs(pressure) > epsilon(1.0_dp)) then
      write(stdOut, format2U) "Gibbs free energy", EGibbs, 'H', Hartree__eV * EGibbs, 'eV'
    end if

  end subroutine printPressureAndFreeEnergy


  !> Writes maximal force component.
  subroutine printMaxForce(maxForce)

    !> maximum of the atomic forces
    real(dp), intent(in) :: maxForce

    write(stdOut, "(A, ':', T30, E20.6)") "Maximal force component", maxForce

  end subroutine printMaxForce


  !> Writes norm of the force
  subroutine printForceNorm(forceNorm)

    !> Norm of the force
    real(dp), intent(in) :: forceNorm

    write(stdOut, "(A, ':', T30, E20.6)") "Averaged force norm", forceNorm

  end subroutine printForceNorm


  !> Print maximal lattice force component
  subroutine printMaxLatticeForce(maxLattForce)

    !> Maximum energy derivative with respect to lattice vectors
    real(dp), intent(in) :: maxLattForce

    write(stdOut, format1Ue) "Maximal Lattice force component", maxLattForce, 'au'

  end subroutine printMaxLatticeForce


  !> Print norm of lattice force
  subroutine printLatticeForceNorm(lattForceNorm)

    !> Norm of the lattice force
    real(dp), intent(in) :: lattForceNorm

    write(stdOut, format1Ue) "Averaged lattice force norm", lattForceNorm, 'au'

  end subroutine printLatticeForceNorm


  !> Prints out info about current MD step.
  subroutine printMdInfo(tSetFillingTemp, eField, tPeriodic, tempElec, tempIon, cellPressure,&
      & pressure, energy)

    !> Is the electronic temperature set by the thermostat method?
    logical, intent(in) :: tSetFillingTemp

    !> External electric field (if allocated)
    type(TEField), intent(in), allocatable :: eField

    !> Is the geometry periodic?
    logical, intent(in) :: tPeriodic

    !> Electronic temperature
    real(dp), intent(in) :: tempElec

    !> Atomic kinetic energy
    real(dp), intent(in) :: tempIon

    !> Internal pressure
    real(dp), intent(in) :: cellPressure

    !> External pressure (applied)
    real(dp), intent(in) :: pressure

    !> data type for energy components and total
    type(TEnergies), intent(in) :: energy

    if (tSetFillingTemp) then
      write(stdOut, format2U) 'Electronic Temperature', tempElec, 'H', tempElec / Boltzmann, 'K'
    end if
    if (allocated(eField)) then
      if (allocated(eField%EFieldStrength)) then
        write(stdOut, format1U1e) 'External E field', eField%absEField, 'au',&
            & eField%absEField * au__V_m, 'V/m'
      end if
    end if
    write(stdOut, format2U) "MD Temperature", tempIon, "H", tempIon / Boltzmann, "K"
    write(stdOut, format2U) "MD Kinetic Energy", energy%Ekin, "H", Hartree__eV * energy%Ekin, "eV"
    write(stdOut, format2U) "Total MD Energy", energy%EMerminKin, "H",&
        & Hartree__eV * energy%EMerminKin, "eV"
    if (tPeriodic) then
      write(stdOut, format2Ue) 'Pressure', cellPressure, 'au', cellPressure * au__pascal, 'Pa'
      if (abs(pressure) < epsilon(1.0_dp)) then
        write(stdOut, format2U) 'Gibbs free energy including KE', energy%EGibbsKin, 'H',&
            & Hartree__eV * energy%EGibbsKin, 'eV'
      end if
    end if

  end subroutine printMdInfo




  !> Creates and prepares binary eigenvector file for writing.
  subroutine createEigvecFileBin(file, runId, fileName)

    !> New file ID for the results
    type(TFileDescr), intent(out) :: file

    !> Run id to write into the file header
    integer, intent(in) :: runId

    !> Name of the file
    character(*), intent(in), optional :: fileName

    character(:), allocatable :: fileName_

    if (present(fileName)) then
      fileName_ = trim(fileName) // ".bin"
    else
      fileName_ = eigvecBin
    end if
    call openFile(file, fileName_, mode="wb")
    write(file%unit) runId

  end subroutine createEigvecFileBin


  !> Prepares text eigenvector file for writing.
  subroutine prepareEigvecFileTxt(fd, t2Component, fileName)

    !> New file ID for the results
    type(TFileDescr), intent(out) :: fd

    !> Whether eigenvectors present 2-component Pauli vectors
    logical, intent(in) :: t2Component

    !> Name of the file
    character(*), intent(in), optional :: fileName

    character(lc) :: tmpStr

    if (present(fileName)) then
      write(tmpStr, "(A,A)") trim(fileName), ".out"
      call openFile(fd, tmpStr, mode="w")
    else
      call openFile(fd, eigvecOut, mode="w")
    end if
    write(fd%unit, "(A/)") "Coefficients and Mulliken populations of the atomic orbitals"
    if (t2Component) then
      write(fd%unit, "(A/)")"   Atom   Orb         up spin coefficients   down spin coefficients  &
          &  charge    x         y         z"
    end if

  end subroutine prepareEigvecFileTxt


  !> Writes a single real eigenvector into a file
  subroutine writeSingleRealEigvecTxt(fd, eigvec, fracs, iS, iEigvec, orb, species, speciesName,&
      & nAtom)

    !> File descriptor of open file
    type(TFileDescr), intent(in) :: fd

    !> Eigenvector to write
    real(dp), intent(in) :: eigvec(:)

    !> Fraction of each component in the eigenvector (c.S.c)
    real(dp), intent(in) :: fracs(:)

    !> Spin index of the eigenvector
    integer, intent(in) :: iS

    !> Index of the eigenvector
    integer, intent(in) :: iEigvec

    !> Orbital information
    type(TOrbitals), intent(in) :: orb

    !> Species for each atom
    integer, intent(in) :: species(:)

    !> Name of each species
    character(*), intent(in) :: speciesName(:)

    !> Number of atoms
    integer, intent(in) :: nAtom

    character(sc), allocatable :: shellNamesTmp(:)
    character(lc) :: tmpStr, strTmp
    integer :: ind, ang
    integer :: iAt, iSp, iSh, iOrb

    write(fd%unit, "('Eigenvector:',I4,4X,'(',A,')'/)") iEigvec, trim(spinName(iS))
    ind = 0
    do iAt = 1, nAtom
      iSp = species(iAt)
      call getShellNames(iSp, orb, shellNamesTmp)
      do iSh = 1, orb%nShell(iSp)
        ang = orb%angShell(iSh, iSp)
        if (ang > 0) then
          write(strTmp,"(A)")trim(shellNamesTmp(iSh))//'_'
        else
          write(strTmp,"(A)")trim(shellNamesTmp(iSh))
        end if
        if (iSh == 1) then
          write(tmpStr, "(I5,1X,A2,2X,A)") iAt, speciesName(iSp), trim(strTmp)
        else
          write(tmpStr, "(10X,A)") trim(strTmp)
        end if
        do iOrb = 1, 2 * ang + 1
          ind = ind + 1
          write(fd%unit , "(A,T22,1X,F10.6,1X,F10.6)")&
              & trim(tmpStr) // trim(orbitalNames(iOrb-ang-1, ang)),&
              & eigvec(ind), fracs(ind)
        end do
      end do
      deallocate(shellNamesTmp)
      write(fd%unit,*)
    end do

  end subroutine writeSingleRealEigvecTxt


  !> Writes a single complex eigenvector into a file
  subroutine writeSingleCplxEigvecTxt(fd, eigvec, fracs, iS, iK, iEigvec, orb, species,&
      & speciesName, nAtom)

    !> File descriptor of open file
    type(TFileDescr), intent(in) :: fd

    !> Eigenvector to write
    complex(dp), intent(in) :: eigvec(:)

    !> Fraction of each basis function in the eigenvector (c.S.c)
    real(dp), intent(in) :: fracs(:)

    !> Spin index of the eigenvector
    integer, intent(in) :: iS

    !> K-point index of the eigenvector
    integer, intent(in) :: iK

    !> Index of the eigenvector
    integer, intent(in) :: iEigvec

    !> Orbital information
    type(TOrbitals), intent(in) :: orb

    !> Species for each atom
    integer, intent(in) :: species(:)

    !> Name of each species
    character(*), intent(in) :: speciesName(:)

    !> Number of atoms
    integer, intent(in) :: nAtom

    character(sc), allocatable :: shellNamesTmp(:)
    character(lc) :: tmpStr, strTmp
    integer :: ind, ang
    integer :: iAt, iSp, iSh, iOrb

    write(fd%unit, "(A,I4,4X,A,I4,4X,'(',A,')'/)") "K-point: ", iK, "Eigenvector: ", iEigvec,&
        & trim(spinName(iS))
    ind = 0
    do iAt = 1, nAtom
      iSp = species(iAt)
      call getShellNames(iSp, orb, shellNamesTmp)
      do iSh = 1, orb%nShell(iSp)
        ang = orb%angShell(iSh, iSp)
        if (ang > 0) then
          write(strTmp,"(A)")trim(shellNamesTmp(iSh))//'_'
        else
          write(strTmp,"(A)")trim(shellNamesTmp(iSh))
        end if
        if (iSh == 1) then
          write(tmpStr, "(I5,1X,A2,2X,A)") iAt, speciesName(iSp), trim(strTmp)
        else
          write(tmpStr, "(10X,A)")  trim(strTmp)
        end if
        do iOrb = 1, 2 * ang + 1
          ind = ind + 1
          write(fd%unit, "(A,T22,1X,'(',F10.6,',',F10.6,1X,')',1X,F10.6)")&
              & trim(tmpStr) // trim(orbitalNames(iOrb-ang-1, ang)),&
              & real(eigvec(ind)), aimag(eigvec(ind)), fracs(ind)
        end do
      end do
      deallocate(shellNamesTmp)
      write(fd%unit,*)
    end do

  end subroutine writeSingleCplxEigvecTxt


  !> Writes a single Pauli two-component eigenvector into a file
  subroutine writeSinglePauliEigvecTxt(fd, eigvec, fracs, iK, iEigvec, orb, species, speciesName,&
      & nAtom, nOrb)

    !> File descriptor of open file
    type(TFileDescr), intent(in) :: fd

    !> Eigenvector to write
    complex(dp), intent(in) :: eigvec(:)

    !> Fraction of each orbital in the eigenvector, decomposed into 4 components.
    real(dp), intent(in) :: fracs(:,:)

    !> K-point index of the eigenvector
    integer, intent(in) :: iK

    !> Index of the eigenvector
    integer, intent(in) :: iEigvec

    !> Orbital information
    type(TOrbitals), intent(in) :: orb

    !> Species for each atom
    integer, intent(in) :: species(:)

    !> Name of each species
    character(*), intent(in) :: speciesName(:)

    !> Number of atoms
    integer, intent(in) :: nAtom

    !> Number of orbitals
    integer, intent(in) :: nOrb

    character(sc), allocatable :: shellNamesTmp(:)
    character(lc) :: tmpStr, strTmp
    integer :: ind, ang
    integer :: iAt, iSp, iSh, iOrb

    write(fd%unit, "(A,I4,4X,A,I4)") "K-point: ", ik, "Eigenvector: ", iEigvec
    ind = 0
    do iAt = 1, nAtom
      iSp = species(iAt)
      call getShellNames(iSp, orb, shellNamesTmp)
      do iSh = 1, orb%nShell(iSp)
        ang = orb%angShell(iSh,iSp)
        if (ang > 0) then
          write(strTmp,"(A)")trim(shellNamesTmp(iSh))//'_'
        else
          write(strTmp,"(A)")trim(shellNamesTmp(iSh))
        end if
        if (iSh == 1) then
          write(tmpStr, "(I5,1X,A2,2X,A)") iAt, speciesName(iSp), trim(strTmp)
        else
          write(tmpStr, "(10X,A)") trim(strTmp)
        end if
        do iOrb = 1, 2 * ang + 1
          ind = ind + 1
          write(fd%unit, "(A,T22,1X,'(',F10.6,',',F10.6,')','(',F10.6,',',F10.6,')',1X,4F10.6)")&
              & trim(tmpStr) // trim(orbitalNames(iOrb-ang-1, ang)), real(eigvec(ind)),&
              & aimag(eigvec(ind)), real(eigvec(ind + nOrb)), aimag(eigvec(ind + nOrb)),&
              & fracs(:, ind)
        end do
      end do
      deallocate(shellNamesTmp)
      write(fd%unit, *)
    end do

  end subroutine writeSinglePauliEigvecTxt


  !> Write projected real eigenvector data to disc
  subroutine writeProjEigvecData(fd, iOrbRegion, eigval, fracs)

    !> File descriptor for each region
    type(TFileDescr), intent(in) :: fd(:)

    !> List of orbital for each region
    type(TListIntR1), intent(inout) :: iOrbRegion

    !> Eigenvalue for current eigenvector
    real(dp), intent(in) :: eigval

    !> Fraction of each orbital in the current eigenvector (c.S.c)
    real(dp), intent(in) :: fracs(:)

    integer, allocatable :: iOrbs(:)
    integer :: valShape(1)
    integer :: iReg, dummy

    do iReg = 1, size(fd)
      call elemShape(iOrbRegion, valshape, iReg)
      allocate(iOrbs(valshape(1)))
      call intoArray(iOrbRegion, iOrbs, dummy, iReg)
      write(fd(iReg)%unit, "(f13.6,f10.6)") Hartree__eV * eigval, sum(fracs(iOrbs))
      deallocate(iOrbs)
    end do

  end subroutine writeProjEigvecData


  !> Write projected real eigenvector data to disc (complex)
  subroutine writeProjPauliEigvecData(fd, iOrbRegion, eigval, fracs)

    !> File descriptor for each region
    type(TFileDescr), intent(in) :: fd(:)

    !> List of orbital for each region
    type(TListIntR1), intent(inout) :: iOrbRegion

    !> Eigenvalue for current eigenvector
    real(dp), intent(in) :: eigval

    !> Fraction of each orbital in the eigenvector for the four Pauli components
    real(dp), intent(in) :: fracs(:,:)

    integer, allocatable :: iOrbs(:)
    integer :: valShape(1)
    integer :: iReg, dummy

    do iReg = 1, size(fd)
      call elemShape(iOrbRegion, valshape, iReg)
      allocate(iOrbs(valshape(1)))
      call intoArray(iOrbRegion, iOrbs, dummy, iReg)
      write(fd(iReg)%unit, "(f13.6,4f10.6)") Hartree__eV * eigval, sum(fracs(:,iOrbs), dim=2)
      deallocate(iOrbs)
    end do

  end subroutine writeProjPauliEigvecData


  !> Writes header for projected eigenvectors
  subroutine writeProjEigvecHeader(fd, iS, iK, kWeight)

    !> File descriptor for each region
    type(TFileDescr), intent(in) :: fd(:)

    !> Index fo current spin
    integer, intent(in) :: iS

    !> Index of current k-point
    integer, intent(in), optional :: iK

    !> Weight of current k-point
    real(dp), intent(in), optional :: kWeight

    integer :: iReg
    character(len=*), parameter :: formatHeader = "(2(A,1X,I0,1X),A,1X,F12.8)"



    do iReg = 1, size(fd)
      if (present(iK)) then
        write(fd(iReg)%unit, formatHeader) 'KPT', iK, 'SPIN', iS, 'KWEIGHT', kWeight
      else
        write(fd(iReg)%unit, formatHeader) 'KPT', 1, 'SPIN', iS, 'KWEIGHT', 1.0_dp
      end if
    end do

  end subroutine writeProjEigvecHeader


  !> Writes footer for projected eigenvectors
  subroutine writeProjEigvecFooter(fd)

    !> File descriptor for each region
    type(TFileDescr), intent(in) :: fd(:)

    integer :: iReg

    do iReg = 1, size(fd)
      write(fd(iReg)%unit, "(A)") ""
    end do

  end subroutine writeProjEigvecFooter


  !> Returns the fraction of each orbital in a 2-component Pauli-vector.
  subroutine getPauliFractions(eigvec, overDotEigvec, fracs)

    !> Pauli eigenvector
    complex(dp), intent(in) :: eigvec(:)

    !> Overlap times eigenvector
    complex(dp), intent(in) :: overDotEigvec(:)

    !> Fractions along the 4 (total, x, y and z) component. Shape (4, size(eigvec) / 2)
    real(dp), intent(out) :: fracs(:,:)

    integer :: nOrb
    integer :: iOrb

    nOrb = size(eigvec) / 2
    do iOrb = 1, nOrb
      fracs(1, iOrb) =  real(conjg(eigvec(iOrb)) * overDotEigvec(iOrb)&
          & + conjg(eigvec(iOrb + nOrb)) * overDotEigvec(iOrb + nOrb))
      fracs(2, iOrb) = real(conjg(eigvec(iOrb + nOrb)) * overDotEigvec(iOrb)&
          & + conjg(eigvec(iOrb)) * overDotEigvec(iOrb + nOrb))
      fracs(3, iOrb) = aimag(conjg(eigvec(iOrb)) * overDotEigvec(iOrb + nOrb)&
          & - conjg(eigvec(iOrb + nOrb)) * overDotEigvec(iOrb))
      fracs(4, iOrb) = real(conjg(eigvec(iOrb)) * overDotEigvec(iOrb)&
          & - conjg(eigvec(iOrb + nOrb)) * overDotEigvec(iOrb + nOrb))
    end do

  end subroutine getPauliFractions


  !> Prepare projected eigenvector file for each region
  subroutine prepareProjEigvecFiles(fileItems, fileNames)

    !> File descriptor for a not yet opened file for each region
    type(TFileDescr), intent(inout) :: fileItems(:)

    !> List of region file names
    type(TListCharLc), intent(inout) :: fileNames

    integer :: iReg
    character(lc) :: tmpStr

    do iReg = 1, size(fileItems)
      call get(fileNames, tmpStr, iReg)
      call openFile(fileItems(iReg), tmpStr, mode="w")
    end do

  end subroutine prepareProjEigvecFiles


  !> Electrostatic potential at specified points
  subroutine writeEsp(esp, env, iGeoStep, nGeoSteps)

    !> Object holding the potentials and their locations
    type(TElStatPotentials), intent(in) :: esp

    !> Environment settings
    type(TEnvironment), intent(in) :: env

    !> Step of the geometry driver
    integer, intent(in) :: iGeoStep

    !> Number of geometry steps
    integer, intent(in) :: nGeoSteps

    type(TFileDescr) :: fdEsp
    integer :: ii
    character(lc) :: tmpStr

    if (env%tGlobalLead) then
      if (esp%tAppendEsp) then
        call openFile(fdEsp, trim(esp%EspOutFile), mode="a")
      else
        call openFile(fdEsp, trim(esp%EspOutFile), mode="w")
      end if
      ! Header with presence of external field and regular grid size
      write(tmpStr, "('# ', L2, 3I6, 1x, I0)")allocated(esp%extPotential),&
          & esp%gridDimensioning, size(esp%intPotential)
      if (.not.esp%tAppendEsp .or. iGeoStep == 0) then
        write(fdEsp%unit, "(A)") trim(tmpStr)
        if (all(esp%gridDimensioning > 0)) then
          write(fdEsp%unit, "(A,3E20.12)") '#', esp%origin * Bohr__AA
          do ii = 1, 3
            write(fdEsp%unit, "(A,3E20.12)") '#', esp%axes(:,ii) * Bohr__AA
          end do
        end if
      end if

      if (nGeoSteps > 0) then
        write(tmpStr, "(' Geo ', I0)")iGeoStep
      else
        write(tmpStr,*)
      end if

      ! actually print the potentials, note the sign changes, as inside DFTB+ potentials are defined
      ! as though the charge on electrons is positive.
      if (all(esp%gridDimensioning > 0)) then
        ! Regular point distribution, do not print positions
        if (allocated(esp%extPotential)) then
          write(fdEsp%unit, "(A,A)") '# Internal (V)        External (V)', trim(tmpStr)
          do ii = 1, size(esp%espGrid,dim=2)
            write(fdEsp%unit, "(2E20.12)") -esp%intPotential(ii) * Hartree__eV,&
                & -esp%extPotential(ii) * Hartree__eV
          end do
        else
          write(fdEsp%unit, "(A,A)") '# Internal (V)', trim(tmpStr)
          do ii = 1, size(esp%espGrid,dim=2)
            write(fdEsp%unit, "(E20.12)") -esp%intPotential(ii) * Hartree__eV
          end do
        end if
      else
        ! Scattered points, print locations
        if (allocated(esp%extPotential)) then
          write(fdEsp%unit ,"(A,A)")&
              & '#           Location (AA)             Internal (V)        External (V)',&
              & trim(tmpStr)
          do ii = 1, size(esp%espGrid,dim=2)
            write(fdEsp%unit, "(3E12.4,2E20.12)") esp%espGrid(:,ii) * Bohr__AA,&
                & -esp%intPotential(ii) * Hartree__eV, -esp%extPotential(ii) * Hartree__eV
          end do
        else
          write(fdEsp%unit, "(A,A)") '#           Location (AA)             Internal (V)',&
              & trim(tmpStr)
          do ii = 1, size(esp%espGrid,dim=2)
            write(fdEsp%unit, "(3E12.4,E20.12)") esp%espGrid(:,ii) * Bohr__AA,&
                & -esp%intPotential(ii) * Hartree__eV
          end do
        end if
      end if
    end if

    call closeFile(fdEsp)

  end subroutine writeEsp



  !> Read external eigenvector file (eigenvec.bin)
  subroutine readCplxEigenvecs(eigenvecs, jobId)

    !> Resulting eigenvectors read from file
    complex(dp), intent(out) :: eigenvecs(:,:)

    !> ID of the calculation which produced the file
    integer, intent(out), optional :: jobId


    call error("Eigenvector reading not currently supported for ScaLAPACK enabled builds")


  end subroutine readCplxEigenvecs


  !> Read external eigenvector file (eigenvec.bin)
  subroutine readRealEigenvecs(eigenvecs, jobId)

    !> Resulting eigenvectors read from file
    real(dp), intent(out) :: eigenvecs(:,:)

    !> ID of the calculation which produced the file
    integer, intent(out), optional :: jobId


    call error("Eigenvector reading not currently supported for ScaLAPACK enabled builds")


  end subroutine readRealEigenvecs



  !> First group of data to go to detailed.out
  subroutine writeReksDetailedOut1(fd, nGeoSteps, iGeoStep, tMD, tDerivs,&
      & tCoordOpt, tLatOpt, iLatGeoStep, iSccIter, energy, diffElec, sccErrorQ,&
      & indMovedAtom, coord0Out, q0, qOutput, orb, species, tPrintMulliken, pressure,&
      & cellVol, tAtomicEnergy, dispersion, tPeriodic, tScc, invLatVec, kPoints,&
      & iAtInCentralRegion, electronicSolver, reks, t3rd, isRangeSep, qNetAtom)

    !> File ID
    integer, intent(in) :: fd

    !> Total number of geometry steps
    integer, intent(in) :: nGeoSteps

    !> Current geometry step
    integer, intent(in) :: iGeoStep

    !> Is this a molecular dynamics run
    logical, intent(in) :: tMD

    !> Is this a finite difference derivative calculation
    logical, intent(in) :: tDerivs

    !> Are atomic coordinates being optimised?
    logical, intent(in) :: tCoordOpt

    !> Is the lattice being optimised?
    logical, intent(in) :: tLatOpt

    !> Which step of lattice optimisation is occurring
    integer, intent(in) :: iLatGeoStep

    !> Which scc step is occurring
    integer, intent(in) :: iSccIter

    !> Energy terms in the system
    type(TEnergies), intent(inout) :: energy

    !> Change in energy from previous SCC iteration
    real(dp), intent(in) :: diffElec

    !> Input/output charge error for SCC
    real(dp), intent(in) :: sccErrorQ

    !> Moving atoms
    integer, intent(in) :: indMovedAtom(:)

    !> Output atomic coordinates
    real(dp), intent(in) :: coord0Out(:,:)

    !> Reference atomic charges
    real(dp), intent(in) :: q0(:,:,:)

    !> Output atomic charges (if SCC)
    real(dp), intent(in) :: qOutput(:,:,:)

    !> Type containing atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Chemical species of atoms
    integer, intent(in) :: species(:)

    !> Should Mulliken populations be printed
    logical, intent(in) :: tPrintMulliken

    !> External pressure
    real(dp), intent(in) :: pressure

    !> Unit cell volume
    real(dp), intent(in) :: cellVol

    !> Are atom resolved energies required
    logical, intent(in) :: tAtomicEnergy

    !> Dispersion interactions object
    class(TDispersionIface), allocatable, intent(inout) :: dispersion

    !> Is the system periodic
    logical, intent(in) :: tPeriodic

    !> Is this a self consistent charge calculation
    logical, intent(in) :: tScc

    !> Reciprocal lattice vectors if periodic
    real(dp), intent(in) :: invLatVec(:,:)

    !> K-points if periodic
    real(dp), intent(in) :: kPoints(:,:)

    !> atoms in the central cell (or device region if transport)
    integer, intent(in) :: iAtInCentralRegion(:)

    !> Electronic solver information
    type(TElectronicSolver), intent(in) :: electronicSolver

    !> Third order DFTB
    logical, intent(in) :: t3rd

    !> Whether to run a range separated calculation
    logical, intent(in) :: isRangeSep

    !> Onsite mulliken population per atom
    real(dp), intent(in), optional :: qNetAtom(:)

    !> data type for REKS
    type(TReksCalc), intent(in) :: reks

    integer :: nAtom, nKPoint, nMovedAtom
    integer :: ang, iAt, iSpin, iK, iSp, iSh, ii, kk
    character(sc), allocatable :: shellNamesTmp(:)
    character(lc) :: strTmp

    nAtom = size(q0, dim=2)
    nKPoint = size(kPoints, dim=2)
    nMovedAtom = size(indMovedAtom)

    write(fd, "(A)") "REKS do not use any electronic distribution function"
    write(fd,*)

    if (nGeoSteps > 0) then
      if (tMD) then
        write(fd, "(A, I0)") "MD step: ", iGeoStep
      elseif (tDerivs) then
        write(fd, "(A, I0)") 'Difference derivative step: ', iGeoStep
      else
        if (tCoordOpt .and. tLatOpt) then
          write(fd, "(A, I0, A, I0)") "Geometry optimization step: ", &
              & iGeoStep, ", Lattice step: ", iLatGeoStep
        else
          write(fd, "(A, I0)") "Geometry optimization step: ", iGeoStep
        end if
      end if
    elseif (tScc) then
      ! Only written if scc is on, to be compatible with old output
      write(fd, "(A)") "Calculation with static geometry"
    end if
    write(fd, *)

    if (tSCC) then
      select case (reks%reksAlg)
      case (reksTypes%noReks)
      case (reksTypes%ssr22)
        write(fd, "(A)") repeat("*", 92)
        write(fd,"(1X,A5,A20,A20,A13,A15)") "iSCC", "       reks energy  ", &
            & "      Diff energy   ", "      x_a    ", "   SCC error   "
        write(fd,"(I5,4x,F16.10,3x,F16.10,3x,F10.6,3x,F11.8)") &
            & iSCCIter, energy%Etotal, diffElec, reks%FONs(1,1)*0.5_dp, sccErrorQ
        write(fd, "(A)") repeat("*", 92)
      case (reksTypes%ssr44)
        call error("SSR(4,4) is not implemented yet")
      end select
      write(fd, *)
    end if

    if (tPeriodic .and. tLatOpt) then
      do iK = 1, nKPoint
        if (iK == 1) then
          write(strTmp, "(A,':')") "K-points in absolute space"
        else
          write(strTmp, "(A)") ""
        end if
        write(fd, "(A,T28,I6,':',3F10.6)") trim(strTmp), iK, matmul(invLatVec,kPoints(:,iK))
      end do
      write(fd, *)
    end if

    if (nMovedAtom > 0 .and. .not. tDerivs) then
      write(fd, "(A)") "Coordinates of moved atoms (au):"
      do iAt = 1, nMovedAtom
        write(fd, formatGeoOut) indMovedAtom(iAt), coord0Out(:, indMovedAtom(iAt))
      end do
      write(fd, *)
    end if

    ! Write out atomic charges
    if (tPrintMulliken) then

      if (reks%nstates > 1) then
        write(fd, "(1X,A)") "SA-REKS optimizes the averaged state, not individual states."
        write(fd, "(1X,A)") "These charges are not from individual states."
        write(fd, "(1X,A)") "Similarly, the values in band.out file indicate"
        write(fd, "(1X,A)") "the band energies and occupations for the averaged state."
        if (.not.reks%tRD) then
          write(fd, "(1X,A)") "If you want to compute the relaxed density,"
          write(fd, "(1X,A)") "please, set 'RelaxedDensity = Yes' option"
        else
          write(fd, "(1X,A)") "Check the file relaxed_charge.dat for the relaxed density."
        end if
        write(fd, *)
      end if

      write(fd, "(A, F14.8)") " Total charge: ", sum(q0(:, iAtInCentralRegion(:), 1)&
          & - qOutput(:, iAtInCentralRegion(:), 1))
      write(fd, "(/,A)") " Atomic gross charges (e)"
      write(fd, "(A5, 1X, A16)")" Atom", " Charge"
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, 1X, F16.8)") iAt, sum(q0(:, iAt, 1) - qOutput(:, iAt, 1))
      end do
      write(fd, *)

      if (present(qNetAtom)) then
        write(fd, "(/,A)") " Atomic net (on-site) populations and hybridisation ratios"
        write(fd, "(A5, 1X, A16, A16)")" Atom", " Population", "Hybrid."
        do ii = 1, size(iAtInCentralRegion)
          iAt = iAtInCentralRegion(ii)
          write(fd, "(I5, 1X, F16.8, F16.8)") iAt, qNetAtom(iAt),&
              & (1.0_dp - qNetAtom(iAt) / sum(q0(:, iAt, 1)))
        end do
        write(fd, *)
      end if

    end if

    lpSpinPrint2_REKS: do iSpin = 1, 1
      if (tPrintMulliken) then
        write(fd, "(3A, F16.8)") 'Nr. of electrons (', trim(spinName(iSpin)), '):',&
            & sum(qOutput(:, iAtInCentralRegion(:), iSpin))
        write(fd, "(3A)") 'Atom populations (', trim(spinName(iSpin)), ')'
        write(fd, "(A5, 1X, A16)") " Atom", " Population"
        do ii = 1, size(iAtInCentralRegion)
          iAt = iAtInCentralRegion(ii)
          write(fd, "(I5, 1X, F16.8)") iAt, sum(qOutput(:, iAt, iSpin))
        end do
        write(fd, *)
        write(fd, "(3A)") 'l-shell populations (', trim(spinName(iSpin)), ')'
        write(fd, "(A5, 1X, A3, 1X, A3, 1X, A16)")" Atom", "Sh.", "  l", " Population"
        do ii = 1, size(iAtInCentralRegion)
          iAt = iAtInCentralRegion(ii)
          iSp = species(iAt)
          do iSh = 1, orb%nShell(iSp)
            write(fd, "(I5, 1X, I3, 1X, I3, 1X, F16.8)") iAt, iSh, orb%angShell(iSh, iSp),&
                & sum(qOutput(orb%posShell(iSh, iSp):orb%posShell(iSh + 1, iSp)-1, iAt,&
                & iSpin))
          end do
        end do
        write(fd, *)
        write(fd, "(3A)") 'Orbital populations (', trim(spinName(iSpin)), ')'
        write(fd, "(A5, 1X, A3, 1X, A3, 1X, A3, 1X, A16, 1X, A6)")&
            & " Atom", "Sh.", "  l", "  m", " Population", " Label"
        do ii = 1, size(iAtInCentralRegion)
          iAt = iAtInCentralRegion(ii)
          iSp = species(iAt)
          call getShellNames(iSp, orb, shellNamesTmp)
          do iSh = 1, orb%nShell(iSp)
            ang = orb%angShell(iSh, iSp)
            if (ang > 0) then
              write(strtmp,"(A)")trim(shellNamesTmp(iSh))//'_'
            else
              write(strTmp,"(A)")trim(shellNamesTmp(iSh))
            end if
            do kk = 0, 2 * ang
              write(fd, "(I5, 1X, I3, 1X, I3, 1X, I3, 1X, F16.8, 2X, A)") iAt, iSh, ang,&
                  & kk - ang, qOutput(orb%posShell(iSh, iSp) + kk, iAt, iSpin),&
                  & trim(strTmp)//trim(orbitalNames(kk-ang,ang))
            end do
          end do
          deallocate(shellNamesTmp)
        end do
        write(fd, *)
      end if
    end do lpSpinPrint2_REKS

    lpSpinPrint3_REKS: do iSpin = 1, 1
      if (tPrintMulliken) then
        write(fd, "(3A, F18.10)") 'Input / Output electrons (', quaternionName(iSpin), '):',&
            & sum(qOutput(:, iAtInCentralRegion(:), iSpin))
      end if
      write(fd, *)
    end do lpSpinPrint3_REKS

    call setReksTargetEnergy(reks, energy, cellVol, pressure)

    write(fd, format2U) 'Energy H0', energy%EnonSCC, 'H', energy%EnonSCC * Hartree__eV, 'eV'
    if (tSCC) then
      write(fd, format2U) 'Energy SCC', energy%ESCC, 'H', energy%ESCC * Hartree__eV, 'eV'
      write(fd, format2U) 'Energy SPIN', energy%Espin, 'H', energy%Espin * Hartree__eV, 'eV'
      if (t3rd) then
        write (fd,format2U) 'Energy 3rd', energy%e3rd, 'H', energy%e3rd*Hartree__eV, 'eV'
      end if
      if (isRangeSep) then
        write(fd, format2U) 'Energy Fock', energy%Efock, 'H', energy%Efock * Hartree__eV, 'eV'
      end if
    end if

    write(fd, format2U) 'Total Electronic energy', energy%Eelec, 'H', &
        & energy%Eelec * Hartree__eV, 'eV'
    write(fd, format2U) 'Repulsive energy', energy%Erep, 'H', energy%Erep * Hartree__eV, 'eV'

    if (allocated(dispersion)) then
      if (dispersion%energyAvailable()) then
        write(fd, format2U) 'Dispersion energy', energy%eDisp, 'H', energy%eDisp * Hartree__eV, 'eV'
      else
        write(fd, "(A)") 'Dispersion energy not yet evaluated, so also missing from other energies'
      end if
    end if

    write(fd, *)
    if (reks%nstates > 1) then
      write(fd, format2U) "Excitation Energy", energy%Eexcited, "H", &
          & Hartree__eV * energy%Eexcited, "eV"
      write(fd, *)
    end if

    write(fd, format2U) 'Total energy', energy%Etotal, 'H', energy%Etotal * Hartree__eV, 'eV'
    if (electronicSolver%providesElectronEntropy) then
      write(fd, format2U) 'Extrapolated to 0', energy%Ezero, 'H', energy%Ezero * Hartree__eV, 'eV'
      write(fd, format2U) 'Total Mermin free energy', energy%Emermin, 'H',&
          & energy%Emermin * Hartree__eV, 'eV'
    end if
    if (electronicSolver%providesFreeEnergy) then
      write(fd, format2U) 'Force related energy', energy%EForceRelated, 'H',&
          & energy%EForceRelated * Hartree__eV, 'eV'
    end if
    if (tPeriodic .and. pressure /= 0.0_dp) then
      write(fd, format2U) 'Gibbs free energy', energy%EGibbs,&
          & 'H', Hartree__eV * energy%EGibbs, 'eV'
    end if
    write(fd, *)

    if (tAtomicEnergy) then
      write(fd, "(A)") 'Atom resolved electronic energies '
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, F16.8, A, F16.6, A)") iAt, energy%atomElec(iAt), ' H',&
            & Hartree__eV * energy%atomElec(iAt), ' eV'
      end do
      write(fd, *)

      write(fd, "(A)") 'Atom resolved repulsive energies '
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, F16.8, A, F16.6, A)") iAt, energy%atomRep(iAt), ' H',&
            & Hartree__eV * energy%atomRep(iAt), ' eV'
      end do
      write(fd, *)
      write(fd, "(A)") 'Atom resolved total energies '
      do ii = 1, size(iAtInCentralRegion)
        iAt = iAtInCentralRegion(ii)
        write(fd, "(I5, F16.8, A, F16.6, A)") iAt, energy%atomTotal(iAt), ' H',&
            & Hartree__eV * energy%atomTotal(iAt), ' eV'
      end do
      write(fd, *)
    end if

  end subroutine writeReksDetailedOut1


  !> Write cavity information as cosmo file
  subroutine writeCosmoFile(solvation, species0, speciesNames, coords0, energy)

    !> Instance of the solvation model
    class(TSolvation), intent(in) :: solvation

    !> Symbols of the species
    character(len=*), intent(in) :: speciesNames(:)

    !> Species of every atom in the unit cell
    integer, intent(in) :: species0(:)

    !> Atomic coordinates
    real(dp), intent(in) :: coords0(:,:)

    !> Total energy
    real(dp), intent(in) :: energy

    type(TFileDescr) :: file

    select type(solvation)
    class is (TCosmo)
      write(stdOut, '(*(a:, 1x))') "Cavity information written to", cosmoFile
      call openFile(file, cosmoFile, mode="w")
      call solvation%writeCosmoFile(file%unit, species0, speciesNames, coords0, energy)
      call closeFile(file)
    end select

  end subroutine writeCosmoFile

end module dftbp_dftbplus_mainio