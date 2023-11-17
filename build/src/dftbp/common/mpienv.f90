!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!



!> Contains MPI related environment settings
module dftbp_common_mpienv
  use dftbp_common_accuracy, only : lc
  use dftbp_extlibs_mpifx, only : mpifx_comm, mpifx_allgather, MPI_COMM_TYPE_SHARED
  use dftbp_io_message, only : error
  implicit none

  private
  public :: TMpiEnv, TMpiEnv_init, TMpiEnv_final


  !> Contains MPI related environment settings
  type :: TMpiEnv

    !> Global MPI communicator
    type(mpifx_comm) :: globalComm

    !> Communicator to access processes within current group
    type(mpifx_comm) :: groupComm

    !> Communicator to access equivalent processes in other groups
    type(mpifx_comm) :: interGroupComm

    !> Communicator within the current node
    type(mpifx_comm) :: nodeComm

    !> Size of the process groups
    integer :: groupSize

    !> Number of processor groups
    integer :: nGroup

    !> Group index of the current process (starts with 0)
    integer :: myGroup

    !> Rank of the processes in the given group (with respect of globalComm)
    integer, allocatable :: groupMembersGlobal(:)

    !> Rank of the processes in the given group (with respect of MPI_COMM_WORLD)
    integer, allocatable :: groupMembersWorld(:)

    !> Whether current process is the global lead
    logical :: tGlobalLead

    !> Whether current process is the group lead
    logical :: tGroupLead

  contains

    procedure :: mpiSerialEnv

  end type TMpiEnv


contains

  !> Initializes MPI environment.
  ! ---------------------------------------------------------------
  ! Initializes global communicator and group communicators
  ! Example:
  ! globalSize = 10
  ! nGroup = 2
  ! groupSize = 5
  !                        rank
  ! globalComm:      0 1 2 3 4 5 6 7 8 9
  ! groupComm:       0 1 2 3 4 0 1 2 3 4
  ! interGroupComm:  0 0 0 0 0 1 1 1 1 1
  ! ---------------------------------------------------------------
  ! SCALAPACK
  ! Different groups handle different kpoints/spin (iKS)
  ! All procs within a group know eigenval(:,iKS)
  ! These are distributed to all other nodes using interGroupComm
  ! eigenvec(:,:,iKS) are used to build the density matrix, DM(:,:,iKS)
  ! DM(:,:,iKS) contains kWeight(iK) and occupation(iKS)
  ! total DM(:,:) is obtained by mpiallreduce with MPI_SUM
  ! ---------------------------------------------------------------
  ! LIBNEGF
  ! Different groups handle different kpoints/spin (iKS)
  ! All procs within a group know densMat(:,:,iKS)
  ! DM(:,:,iKS) contains kWeight(iK) and occupation(iKS)
  ! total DM(:,:) is obtained by mpiallreduce with MPI_SUM
  ! ---------------------------------------------------------------
  subroutine TMpiEnv_init(this, globalMpiComm, nGroup)

    !> Initialised instance on exit
    type(TMpiEnv), intent(out) :: this

    !> The global MPI communicator (assumed to be MPI_COMM_WORLD if not specified)
    type(mpifx_comm), optional, intent(in) :: globalMpiComm

    !> Number of process groups to create
    integer, intent(in), optional :: nGroup

    if (present(globalMpiComm)) then
      this%globalComm = globalMpiComm
    else
      call this%globalComm%init()
    end if
    if (present(nGroup)) then
      this%nGroup = nGroup
    else
      this%nGroup = 1
    end if

    call setup_inter_node_communicator(this)

      call setup_subgrids_common(this)

    this%tGlobalLead = this%globalComm%lead
    this%tGroupLead = this%groupComm%lead

    if (this%tGlobalLead .and. .not. this%tGroupLead) then
      call error("Internal error: Global lead process is not a group leading process")
    end if

  end subroutine TMpiEnv_init


  !> Finalises the communicators in the structure supplied here
  subroutine TMpiEnv_final(this)

    !>  Initialised instance.
    type(TMpiEnv), intent(inout) :: this

    call this%interGroupComm%free()
    call this%groupComm%free()

  end subroutine TMpiEnv_final


  !> Routine to check this is a single processor instance, stopping otherwise (useful to call in
  !> purely serial codes to avid multiple copies being invoked with mpirun)
  subroutine mpiSerialEnv(this, iErr)

    !> Instance
    class(TMpiEnv), intent(in) :: this

    !> Optional error flag
    integer, intent(out), optional :: iErr

    if (this%globalComm%size > 1) then

  block
    use dftbp_common_accuracy, only : lc
    use dftbp_io_message
    !> Error handling string
    character(lc) :: stringTmp

    write(stringTmp,"(A)")'This is serial code, but invoked on multiple processors'
    if (present(iErr)) then
      iErr = -1
      call warning(stringTmp)
      return
    else
      call error(stringTmp)
    end if
  end block

    end if

  end subroutine mpiSerialEnv


  !> Sets up subgrids and group communicators used with common (non-NEGF) solvers.
  subroutine setup_subgrids_common(this)

    !> Environment instance
    type(TMpiEnv), intent(inout) :: this

    integer :: myRank, myGroup
    character(lc) :: tmpStr
    type(mpifx_comm) :: mpiCommWorld

    this%groupSize = this%globalComm%size / this%nGroup
    if (this%nGroup * this%groupSize /= this%globalComm%size) then
      write(tmpStr, "(A,I0,A,I0,A)") "Number of groups (", this%nGroup,&
          & ") not compatible with number of processes (", this%globalComm%size, ")"
      call error(tmpStr)
    end if

    this%myGroup = this%globalComm%rank / this%groupSize
    myRank = mod(this%globalComm%rank, this%groupSize)
    call this%globalComm%split(this%myGroup, myRank, this%groupComm)
    allocate(this%groupMembersGlobal(this%groupSize))
    call mpifx_allgather(this%groupComm, this%globalComm%rank, this%groupMembersGlobal)

    ! Make a wrapper around MPI_COMM_WORLD and get group member ids within that descriptor
    call mpiCommWorld%init()
    allocate(this%groupMembersWorld(this%groupSize))
    call mpifx_allgather(this%groupComm, mpiCommWorld%rank, this%groupMembersWorld)

    myGroup = myRank
    myRank = this%myGroup
    call this%globalComm%split(myGroup, myRank, this%interGroupComm)

  end subroutine setup_subgrids_common



  !> Sets up a communicator within the node to use MPI shared memory
  subroutine setup_inter_node_communicator(this)

    !> Environment instance
    type(TMpiEnv), intent(inout) :: this

    call this%globalComm%split_type(MPI_COMM_TYPE_SHARED, this%globalComm%rank, this%nodeComm)

  end subroutine setup_inter_node_communicator

end module dftbp_common_mpienv
