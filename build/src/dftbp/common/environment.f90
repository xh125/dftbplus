!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Contains computer environment settings
module dftbp_common_environment
  use dftbp_common_globalenv, only : shutdown, stdOut
  use dftbp_common_status, only : TStatus
  use dftbp_common_timerarray, only : TTimerItem, TTimerArray, TTimerArray_init
  use dftbp_common_globalenv, only : globalMpiComm
  use dftbp_common_mpienv, only : TMpiEnv, TMpiEnv_init, TMpiEnv_final
  use dftbp_common_blacsenv, only : TBlacsEnv, TBlacsEnv_init, TBlacsEnv_final
  implicit none

  private
  public :: TEnvironment, TEnvironment_init
  public :: globalTimers


  !> Contains environment settings.
  type :: TEnvironment
    private

    !> Whether this process is the lead?
    logical, public :: tGlobalLead = .true.

    !> Nr. of groups in the system
    integer, public :: nGroup = 1

    !> Id of current group (starts with 0)
    integer, public :: myGroup = 0

    !> Global timers
    type(TTimerArray), public, allocatable :: globalTimer


    !> Global mpi settings
    type(TMpiEnv), public :: mpi

    !> Whether MPI environment had been initialised
    logical :: mpiInitialised = .false.



    !> Global scalapack settings
    type(TBlacsEnv), public :: blacs

    !> Whether BLACS environment had been initialised
    logical :: blacsInitialised = .false.


    !> Is this calculation called by the API?
    logical, public :: tAPICalculation = .false.

  contains
    procedure :: destruct => TEnvironment_destruct
    procedure :: shutdown => TEnvironment_shutdown
    procedure :: initGlobalTimer => TEnvironment_initGlobalTimer

    procedure :: initMpi => TEnvironment_initMpi

    procedure :: initBlacs => TEnvironment_initBlacs


  end type TEnvironment

  type(TTimerItem), parameter :: globalTimerItems(25) = [&
      & TTimerItem("Global initialisation", 1),&
      & TTimerItem("Pre-SCC initialisation", 1),&
      & TTimerItem("Sparse H0 and S build", 4),&
      & TTimerItem("SCC", 1),&
      & TTimerItem("Poisson", 2),&
      & TTimerItem("Poisson Ewald", 4),&
      & TTimerItem("Poisson bulk read", 4),&
      & TTimerItem("Poisson bulk compute", 4),&
      & TTimerItem("Poisson solution", 4),&
      & TTimerItem("Poisson shifts", 4),&
      & TTimerItem("Poisson charge density build", 4),&
      & TTimerItem("Diagonalisation", 2),&
      & TTimerItem("Sparse to dense", 4),&
      & TTimerItem("Dense to sparse", 4),&
      & TTimerItem("Range separated Hamiltonian", 4),&
      & TTimerItem("Density matrix creation", 2),&
      & TTimerItem("Energy evaluation", 2),&
      & TTimerItem("Post-SCC processing", 1),&
      & TTimerItem("Eigenvector writing", 2),&
      & TTimerItem("Energy-density matrix creation", 2),&
      & TTimerItem("Force calculation", 2),&
      & TTimerItem("Stress calculation", 2),&
      & TTimerItem("Post-geometry optimisation", 1),&
      & TTimerItem("Electron dynamics initialisation", 2),&
      & TTimerItem("Electron dynamics loop", 2)&
      & ]

  type :: TGlobalTimersHelper
    integer :: globalInit = 1
    integer :: preSccInit = 2
    integer :: sparseH0S = 3
    integer :: scc = 4
    integer :: poisson = 5
    integer :: poissonEwald = 6
    integer :: poissonBulkRead = 7
    integer :: poissonBulkCalc = 8
    integer :: poissonSoln = 9
    integer :: poissonShifts = 10
    integer :: poissonDensity = 11
    integer :: diagonalization = 12
    integer :: sparseToDense = 13
    integer :: denseToSparse = 14
    integer :: rangeSeparatedH = 15
    integer :: densityMatrix = 16
    integer :: energyEval = 17
    integer :: postScc = 18
    integer :: eigvecWriting = 19
    integer :: energyDensityMatrix = 20
    integer :: forceCalc = 21
    integer :: stressCalc = 22
    integer :: postGeoOpt = 23
    integer :: elecDynInit = 24
    integer :: elecDynLoop = 25

  end type TGlobalTimersHelper

  type(TGlobalTimersHelper), parameter :: globalTimers = TGlobalTimersHelper()


contains

  !> Returns an initialized instance.
  subroutine TEnvironment_init(this)

    !> Instance
    type(TEnvironment), intent(out) :: this

    continue

  end subroutine TEnvironment_init


  !> Finalizes the environment.
  subroutine TEnvironment_destruct(this)

    !> Instance
    class(TEnvironment), intent(inout) :: this

    if (allocated(this%globalTimer)) then
      call this%globalTimer%writeTimings()
    end if

      if (this%blacsInitialised) then
        call TBlacsEnv_final(this%blacs)
        this%blacsInitialised = .false.
      end if

      if (this%mpiInitialised) then
        call TMpiEnv_final(this%mpi)
        this%mpiInitialised = .false.
      end if

    flush(stdOut)

  end subroutine TEnvironment_destruct


  !> Gracefully cleans up and shuts down.
  !>
  !> Note: This routine must be collectively called by all processes.
  !>
  subroutine TEnvironment_shutdown(this)

    !> Instance
    class(TEnvironment), intent(inout) :: this

    call this%destruct()
    call shutdown()

  end subroutine TEnvironment_shutdown


  !> Initlaizes the global timer of the environment.
  subroutine TEnvironment_initGlobalTimer(this, timingLevel, header, unit)

    !> Instance
    class(TEnvironment), intent(inout) :: this

    !> Timing level up to which timings should be printed (-1 for all)
    integer, intent(in) :: timingLevel

    !> Header of the timing table
    character(*), intent(in) :: header

    !> File unit into which the table should be written
    integer, intent(in) :: unit

    allocate(this%globalTimer)
    call TTimerArray_init(this%globalTimer, globalTimerItems, maxLevel=timingLevel, header=header,&
        & unit=unit)

  end subroutine TEnvironment_initGlobalTimer




  !> Initializes MPI environment.
  subroutine TEnvironment_initMpi(this, nGroup)

    !> Instance
    class(TEnvironment), intent(inout) :: this

    !> Number of process groups to create
    integer, intent(in) :: nGroup

    ! MPI settings
    call TMpiEnv_init(this%mpi, globalMpiComm=globalMpiComm, nGroup=nGroup)
    this%tGlobalLead = this%mpi%tGlobalLead
    this%nGroup = this%mpi%nGroup
    this%myGroup = this%mpi%myGroup
    this%mpiInitialised = .true.

  end subroutine TEnvironment_initMpi




  !> Initializes BLACS environment
  subroutine TEnvironment_initBlacs(this, rowBlock, colBlock, nOrb, nAtom, errStatus)

    !> Instance
    class(TEnvironment), intent(inout) :: this

    !> Row block size
    integer, intent(in) :: rowBlock

    !> Column block size
    integer, intent(in) :: colBlock

    !> Nr. of orbitals
    integer, intent(in) :: nOrb

    !> Nr. of atoms
    integer, intent(in) :: nAtom

    !> Operation status, if an error needs to be returned
    type(TStatus), intent(inout) :: errStatus

    call TBlacsEnv_init(this%blacs, this%mpi, rowBlock, colBlock, nOrb, nAtom, errStatus)
  if (errStatus%hasError()) then
    return
  end if
    this%blacsInitialised = .true.

  end subroutine TEnvironment_initBlacs





end module dftbp_common_environment
