
!> Contains the linecomm type.
module linecomm_module
  use blacsfx_module
  use scalapackfx_common_module
  use scalapackfx_module
  implicit none
  private

  public :: linecomm


  !> Type for communicating a row or a column of a distributed matrix.
  !!
  !! \details The type linecomm collects/distributes a line (row or column)
  !! of a distributed matrix into/from a buffer on the lead node.
  !! It communicate the entire line at once or blockwise, with the blocks
  !! having the size of the BLACS block size.
  !!
  !! The code below demonstrates how to write out a distributed matrix
  !! columnwise to disc with the help of linecomm:
  !!
  !!     type(linecomm) :: collector
  !!     real(dp), allocatable :: iobuffer(:)
  !!
  !!     allocate(iobuffer(desc(M_))
  !!     call collector%init(mygrid, desc, "c")
  !!     do icol = 1, desc(N_)
  !!       if (mygrid%lead) then
  !!         call collector%getline_lead(mygrid, icol, mtxloc, iobuffer)
  !!         write(fd, formatstr) iobuffer(:)
  !!       else
  !!         call collector%getline_follow(mygrid, icol, mtxloc)
  !!       end if
  !!     end do
  !!
  !! Similarly, to read from a file columnwise, you could do the following:
  !!
  !!     type(linecomm) :: distributor
  !!     real(dp), allocatable :: iobuffer(:)
  !!
  !!     allocate(iobuffer(desc(M_))
  !!     call distributor%init(desc, "c")
  !!     do icol = 1, ncol
  !!       if (mygrid%lead) then
  !!         read(fd, *) iobuffer(:)
  !!         call distributor%setline_lead(mygrid, icol, iobuffer, mtxloc)
  !!       else
  !!         call distributor%setline_follow(mygrid, icol, mtxloc)
  !!       end if
  !!     end do
  !!
  type :: linecomm
    private
    integer :: nn, nblock, blocksize, iorow, iocol
    logical :: rowcollect
    integer :: desc(DLEN_)
  contains
    procedure :: init
    procedure :: getnrblocks
    procedure :: getblocksize

        procedure :: getblock_lead_int
        procedure :: getblock_lead_real
        procedure :: getblock_lead_dreal
        procedure :: getblock_lead_complex
        procedure :: getblock_lead_dcomplex
        procedure :: getblock_follow_int
        procedure :: getblock_follow_real
        procedure :: getblock_follow_dreal
        procedure :: getblock_follow_complex
        procedure :: getblock_follow_dcomplex

    generic :: getblock_lead => getblock_lead_int, getblock_lead_real, &
        & getblock_lead_dreal, getblock_lead_complex, &
        & getblock_lead_dcomplex
    generic :: getblock_follow => getblock_follow_int, getblock_follow_real, &
        & getblock_follow_dreal, getblock_follow_complex, getblock_follow_dcomplex

        procedure :: getline_lead_int
        procedure :: getline_lead_real
        procedure :: getline_lead_dreal
        procedure :: getline_lead_complex
        procedure :: getline_lead_dcomplex
        procedure :: getline_follow_int
        procedure :: getline_follow_real
        procedure :: getline_follow_dreal
        procedure :: getline_follow_complex
        procedure :: getline_follow_dcomplex

    generic :: getline_lead => getline_lead_int, getline_lead_real, &
        & getline_lead_dreal, getline_lead_complex, getline_lead_dcomplex
    generic :: getline_follow => getline_follow_int, getline_follow_real, &
        & getline_follow_dreal, getline_follow_complex, getline_follow_dcomplex

        procedure :: setblock_lead_int
        procedure :: setblock_lead_real
        procedure :: setblock_lead_dreal
        procedure :: setblock_lead_complex
        procedure :: setblock_lead_dcomplex
        procedure :: setblock_follow_int
        procedure :: setblock_follow_real
        procedure :: setblock_follow_dreal
        procedure :: setblock_follow_complex
        procedure :: setblock_follow_dcomplex

    generic :: setblock_lead => setblock_lead_int, setblock_lead_real, &
        & setblock_lead_dreal, setblock_lead_complex, &
        & setblock_lead_dcomplex
    generic :: setblock_follow => setblock_follow_int, setblock_follow_real, &
        & setblock_follow_dreal, setblock_follow_complex, setblock_follow_dcomplex

        procedure :: setline_lead_int
        procedure :: setline_lead_real
        procedure :: setline_lead_dreal
        procedure :: setline_lead_complex
        procedure :: setline_lead_dcomplex
        procedure :: setline_follow_int
        procedure :: setline_follow_real
        procedure :: setline_follow_dreal
        procedure :: setline_follow_complex
        procedure :: setline_follow_dcomplex

    generic :: setline_lead => setline_lead_int, setline_lead_real, &
        & setline_lead_dreal, setline_lead_complex, setline_lead_dcomplex
    generic :: setline_follow => setline_follow_int, setline_follow_real, &
        & setline_follow_dreal, setline_follow_complex, setline_follow_dcomplex

    procedure, private :: getpositions
  end type linecomm


contains

  !> Initializes a linecomm instance.
  !!
  !! \param self  Initialized instance on exit.
  !! \param desc  Descriptor of distributed matrix.
  !! \param rowcol  If "r" or "R", a given row of the matrix is collected,
  !!     otherwise a given column.
  !! \param iorow  Row of process doing the io (default: row of lead).
  !! \param iocol  Column of process doing the io (default: column of lead).
  !!
  subroutine init(self, mygrid, desc, rowcol, iorow, iocol)
    class(linecomm), intent(out) :: self
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: desc(DLEN_)
    character, intent(in) :: rowcol
    integer, intent(in), optional :: iorow, iocol

  if (present(iorow)) then
    self%iorow = iorow
  else
    self%iorow = mygrid%leadrow
  end if
  if (present(iocol)) then
    self%iocol = iocol
  else
    self%iocol = mygrid%leadcol
  end if
    self%rowcollect = (rowcol == "R" .or. rowcol == "r")
    self%desc(:) = desc
    if (self%rowcollect) then
      self%nn = desc(M_)
      self%blocksize = desc(MB_)
    else
      self%nn = desc(N_)
      self%blocksize = desc(NB_)
    end if
    self%nblock = self%nn / self%blocksize
    if (mod(self%nn, self%blocksize) /= 0) then
      self%nblock = self%nblock + 1
    end if

  end subroutine init


  !> Returns the nr. of blocks along the given row or column.
  !!
  !! \param self  Instance.
  !!
  function getnrblocks(self) result(res)
    class(linecomm), intent(in) :: self
    integer :: res

    res = self%nblock

  end function getnrblocks


  !> Returns the size of a block with the given index.
  !!
  !! \param self Instance.
  !! \param ib  Block index.
  !! \return  Size of the given block.
  !!
  function getblocksize(self, ib) result(res)
    class(linecomm), intent(in) :: self
    integer, intent(in) :: ib
    integer :: res

    if (ib < self%nblock) then
      res = self%blocksize
    else
      res = mod(self%nn, self%blocksize)
      if (res == 0) then
        res = self%blocksize
      end if
    end if

  end function getblocksize






















  !> Returns the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the given piece of the distributed matrix on exit.
  !!    Its size should be greater than or equal to the BLACS block size
  !!    along that dimension. The actual number of elements for a given block
  !!    can be queried via the getblocksize() call.
  !!
  subroutine getblock_lead_int(self, mygrid, ii, ib, locmtx, &
      & buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    integer, intent(in) :: locmtx(:,:)
    integer, target, intent(out) :: buffer(:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    integer, pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work = locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1)
    else
      call blacsfx_gerv(mygrid, work, prow, pcol)
    end if

  end subroutine getblock_lead_int


  !> Returns the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getblock_follow_int(self, mygrid, ii, ib, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    integer, target, intent(in) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    integer, pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gesd(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine getblock_follow_int


  !> Returns an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the collected line on exit. Its size should be
  !!     big enough to contain the result (greater or equal to the size of
  !!     the distributed matrix along that direction).
  !!
  subroutine getline_lead_int(self, mygrid, ii, locmtx, buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    integer, intent(in) :: locmtx(:,:)
    integer, intent(out) :: buffer(:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%getblock_lead(mygrid, ii, ib, locmtx, buffer(istart:iend))
    end do

  end subroutine getline_lead_int


  !> Returns the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getline_follow_int(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    integer, intent(in) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%getblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine getline_follow_int


  !> Sets the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param buffer  Contains the given piece to be distributed. It should contain
  !!    the appropriate number of elements for the given block, as returned by
  !!    the getblocksize() call. Its size can be bigger than that.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_lead_int(self, mygrid, ii, ib, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    integer, target, intent(in) :: buffer(:)
    integer, intent(inout) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    integer, pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1) = work
    else
      call blacsfx_gesd(mygrid, work, prow, pcol)
    end if

  end subroutine setblock_lead_int


  !> Sets the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_follow_int(self, mygrid, ii, ib, locmtx)
    use iso_fortran_env
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    integer, intent(inout), target :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    integer, pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gerv(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine setblock_follow_int


  !> Sets an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param buffer  Contains the line to distribute. It should contain all
  !!     elements along the line. Its size can be bigger.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_lead_int(self, mygrid, ii, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    integer, intent(in) :: buffer(:)
    integer, intent(inout) :: locmtx(:,:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%setblock_lead(mygrid, ii, ib, buffer(istart:iend), locmtx)
    end do

  end subroutine setline_lead_int


  !> Sets the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_follow_int(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    integer, intent(inout) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%setblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine setline_follow_int


  !> Returns the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the given piece of the distributed matrix on exit.
  !!    Its size should be greater than or equal to the BLACS block size
  !!    along that dimension. The actual number of elements for a given block
  !!    can be queried via the getblocksize() call.
  !!
  subroutine getblock_lead_real(self, mygrid, ii, ib, locmtx, &
      & buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    real(sp), intent(in) :: locmtx(:,:)
    real(sp), target, intent(out) :: buffer(:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    real(sp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work = locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1)
    else
      call blacsfx_gerv(mygrid, work, prow, pcol)
    end if

  end subroutine getblock_lead_real


  !> Returns the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getblock_follow_real(self, mygrid, ii, ib, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    real(sp), target, intent(in) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    real(sp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gesd(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine getblock_follow_real


  !> Returns an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the collected line on exit. Its size should be
  !!     big enough to contain the result (greater or equal to the size of
  !!     the distributed matrix along that direction).
  !!
  subroutine getline_lead_real(self, mygrid, ii, locmtx, buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    real(sp), intent(in) :: locmtx(:,:)
    real(sp), intent(out) :: buffer(:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%getblock_lead(mygrid, ii, ib, locmtx, buffer(istart:iend))
    end do

  end subroutine getline_lead_real


  !> Returns the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getline_follow_real(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    real(sp), intent(in) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%getblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine getline_follow_real


  !> Sets the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param buffer  Contains the given piece to be distributed. It should contain
  !!    the appropriate number of elements for the given block, as returned by
  !!    the getblocksize() call. Its size can be bigger than that.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_lead_real(self, mygrid, ii, ib, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    real(sp), target, intent(in) :: buffer(:)
    real(sp), intent(inout) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    real(sp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1) = work
    else
      call blacsfx_gesd(mygrid, work, prow, pcol)
    end if

  end subroutine setblock_lead_real


  !> Sets the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_follow_real(self, mygrid, ii, ib, locmtx)
    use iso_fortran_env
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    real(sp), intent(inout), target :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    real(sp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gerv(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine setblock_follow_real


  !> Sets an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param buffer  Contains the line to distribute. It should contain all
  !!     elements along the line. Its size can be bigger.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_lead_real(self, mygrid, ii, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    real(sp), intent(in) :: buffer(:)
    real(sp), intent(inout) :: locmtx(:,:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%setblock_lead(mygrid, ii, ib, buffer(istart:iend), locmtx)
    end do

  end subroutine setline_lead_real


  !> Sets the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_follow_real(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    real(sp), intent(inout) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%setblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine setline_follow_real


  !> Returns the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the given piece of the distributed matrix on exit.
  !!    Its size should be greater than or equal to the BLACS block size
  !!    along that dimension. The actual number of elements for a given block
  !!    can be queried via the getblocksize() call.
  !!
  subroutine getblock_lead_dreal(self, mygrid, ii, ib, locmtx, &
      & buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    real(dp), intent(in) :: locmtx(:,:)
    real(dp), target, intent(out) :: buffer(:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    real(dp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work = locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1)
    else
      call blacsfx_gerv(mygrid, work, prow, pcol)
    end if

  end subroutine getblock_lead_dreal


  !> Returns the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getblock_follow_dreal(self, mygrid, ii, ib, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    real(dp), target, intent(in) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    real(dp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gesd(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine getblock_follow_dreal


  !> Returns an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the collected line on exit. Its size should be
  !!     big enough to contain the result (greater or equal to the size of
  !!     the distributed matrix along that direction).
  !!
  subroutine getline_lead_dreal(self, mygrid, ii, locmtx, buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    real(dp), intent(in) :: locmtx(:,:)
    real(dp), intent(out) :: buffer(:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%getblock_lead(mygrid, ii, ib, locmtx, buffer(istart:iend))
    end do

  end subroutine getline_lead_dreal


  !> Returns the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getline_follow_dreal(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    real(dp), intent(in) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%getblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine getline_follow_dreal


  !> Sets the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param buffer  Contains the given piece to be distributed. It should contain
  !!    the appropriate number of elements for the given block, as returned by
  !!    the getblocksize() call. Its size can be bigger than that.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_lead_dreal(self, mygrid, ii, ib, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    real(dp), target, intent(in) :: buffer(:)
    real(dp), intent(inout) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    real(dp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1) = work
    else
      call blacsfx_gesd(mygrid, work, prow, pcol)
    end if

  end subroutine setblock_lead_dreal


  !> Sets the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_follow_dreal(self, mygrid, ii, ib, locmtx)
    use iso_fortran_env
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    real(dp), intent(inout), target :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    real(dp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gerv(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine setblock_follow_dreal


  !> Sets an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param buffer  Contains the line to distribute. It should contain all
  !!     elements along the line. Its size can be bigger.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_lead_dreal(self, mygrid, ii, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    real(dp), intent(in) :: buffer(:)
    real(dp), intent(inout) :: locmtx(:,:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%setblock_lead(mygrid, ii, ib, buffer(istart:iend), locmtx)
    end do

  end subroutine setline_lead_dreal


  !> Sets the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_follow_dreal(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    real(dp), intent(inout) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%setblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine setline_follow_dreal


  !> Returns the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the given piece of the distributed matrix on exit.
  !!    Its size should be greater than or equal to the BLACS block size
  !!    along that dimension. The actual number of elements for a given block
  !!    can be queried via the getblocksize() call.
  !!
  subroutine getblock_lead_complex(self, mygrid, ii, ib, locmtx, &
      & buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    complex(sp), intent(in) :: locmtx(:,:)
    complex(sp), target, intent(out) :: buffer(:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    complex(sp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work = locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1)
    else
      call blacsfx_gerv(mygrid, work, prow, pcol)
    end if

  end subroutine getblock_lead_complex


  !> Returns the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getblock_follow_complex(self, mygrid, ii, ib, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    complex(sp), target, intent(in) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    complex(sp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gesd(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine getblock_follow_complex


  !> Returns an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the collected line on exit. Its size should be
  !!     big enough to contain the result (greater or equal to the size of
  !!     the distributed matrix along that direction).
  !!
  subroutine getline_lead_complex(self, mygrid, ii, locmtx, buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    complex(sp), intent(in) :: locmtx(:,:)
    complex(sp), intent(out) :: buffer(:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%getblock_lead(mygrid, ii, ib, locmtx, buffer(istart:iend))
    end do

  end subroutine getline_lead_complex


  !> Returns the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getline_follow_complex(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    complex(sp), intent(in) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%getblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine getline_follow_complex


  !> Sets the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param buffer  Contains the given piece to be distributed. It should contain
  !!    the appropriate number of elements for the given block, as returned by
  !!    the getblocksize() call. Its size can be bigger than that.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_lead_complex(self, mygrid, ii, ib, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    complex(sp), target, intent(in) :: buffer(:)
    complex(sp), intent(inout) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    complex(sp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1) = work
    else
      call blacsfx_gesd(mygrid, work, prow, pcol)
    end if

  end subroutine setblock_lead_complex


  !> Sets the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_follow_complex(self, mygrid, ii, ib, locmtx)
    use iso_fortran_env
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    complex(sp), intent(inout), target :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    complex(sp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gerv(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine setblock_follow_complex


  !> Sets an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param buffer  Contains the line to distribute. It should contain all
  !!     elements along the line. Its size can be bigger.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_lead_complex(self, mygrid, ii, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    complex(sp), intent(in) :: buffer(:)
    complex(sp), intent(inout) :: locmtx(:,:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%setblock_lead(mygrid, ii, ib, buffer(istart:iend), locmtx)
    end do

  end subroutine setline_lead_complex


  !> Sets the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_follow_complex(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    complex(sp), intent(inout) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%setblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine setline_follow_complex


  !> Returns the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the given piece of the distributed matrix on exit.
  !!    Its size should be greater than or equal to the BLACS block size
  !!    along that dimension. The actual number of elements for a given block
  !!    can be queried via the getblocksize() call.
  !!
  subroutine getblock_lead_dcomplex(self, mygrid, ii, ib, locmtx, &
      & buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    complex(dp), intent(in) :: locmtx(:,:)
    complex(dp), target, intent(out) :: buffer(:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    complex(dp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work = locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1)
    else
      call blacsfx_gerv(mygrid, work, prow, pcol)
    end if

  end subroutine getblock_lead_dcomplex


  !> Returns the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getblock_follow_dcomplex(self, mygrid, ii, ib, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    complex(dp), target, intent(in) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    complex(dp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gesd(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine getblock_follow_dcomplex


  !> Returns an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !! \param buffer  Contains the collected line on exit. Its size should be
  !!     big enough to contain the result (greater or equal to the size of
  !!     the distributed matrix along that direction).
  !!
  subroutine getline_lead_dcomplex(self, mygrid, ii, locmtx, buffer)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    complex(dp), intent(in) :: locmtx(:,:)
    complex(dp), intent(out) :: buffer(:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%getblock_lead(mygrid, ii, ib, locmtx, buffer(istart:iend))
    end do

  end subroutine getline_lead_dcomplex


  !> Returns the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to collect.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine getline_follow_dcomplex(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    complex(dp), intent(in) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%getblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine getline_follow_dcomplex


  !> Sets the given block of the distributed matrix (lead, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param buffer  Contains the given piece to be distributed. It should contain
  !!    the appropriate number of elements for the given block, as returned by
  !!    the getblocksize() call. Its size can be bigger than that.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_lead_dcomplex(self, mygrid, ii, ib, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    complex(dp), target, intent(in) :: buffer(:)
    complex(dp), intent(inout) :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    complex(dp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    work(1:nrow,1:ncol) => buffer(1:nrow*ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      locmtx(lrow:lrow+nrow-1,lcol:lcol+ncol-1) = work
    else
      call blacsfx_gesd(mygrid, work, prow, pcol)
    end if

  end subroutine setblock_lead_dcomplex


  !> Sets the given block of the distributed matrix (follow, $1).
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor.
  !! \param ii  Row/Column index.
  !! \param ib  Block index within given row/column.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setblock_follow_dcomplex(self, mygrid, ii, ib, locmtx)
    use iso_fortran_env
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    complex(dp), intent(inout), target :: locmtx(:,:)

    integer :: prow, pcol, lrow, lcol, nrow, ncol
    complex(dp), pointer :: work(:,:)

    call self%getpositions(mygrid, ii, ib, prow, pcol, lrow, lcol, nrow, &
        & ncol)
    if (prow == mygrid%myrow .and. pcol == mygrid%mycol) then
      work => locmtx(lrow:lrow+nrow-1, lcol:lcol+ncol-1)
      call blacsfx_gerv(mygrid, work, self%iorow, self%iocol)
    end if

  end subroutine setblock_follow_dcomplex


  !> Sets an entire row/column of a distributed matrix (lead, $1)
  !!
  !! \param self  Instance.
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param buffer  Contains the line to distribute. It should contain all
  !!     elements along the line. Its size can be bigger.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_lead_dcomplex(self, mygrid, ii, buffer, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    complex(dp), intent(in) :: buffer(:)
    complex(dp), intent(inout) :: locmtx(:,:)

    integer :: ib, istart, iend

    iend = 0
    do ib = 1, self%nblock
      istart = iend + 1
      iend = istart + self%getblocksize(ib) - 1
      call self%setblock_lead(mygrid, ii, ib, buffer(istart:iend), locmtx)
    end do

  end subroutine setline_lead_dcomplex


  !> Sets the entire row/column of a distributed matrix (follow)
  !!
  !! \param mygrid  BLACS descriptor
  !! \param ii  Number of the line (row or column) to set.
  !! \param locmtx  Local part of the global matrix.
  !!
  subroutine setline_follow_dcomplex(self, mygrid, ii, locmtx)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii
    complex(dp), intent(inout) :: locmtx(:,:)

    integer :: ib

    do ib = 1, self%nblock
      call self%setblock_follow(mygrid, ii, ib, locmtx)
    end do

  end subroutine setline_follow_dcomplex



  !! Helper routine to get global positions for given block.
  !!
  !! \param self  Instance
  !! \param mygrid BLACS descriptor
  !! \param ii  Index of the line (row/column).
  !! \param ib  Index of the block within the line.
  !! \param prow  Row of the processor owning the block.
  !! \param pcol  Column of the processor ownig the block.
  !! \param lrow  Starting row in the local matrix for the block.
  !! \param lcol  Starting column in the local matrix for the block.
  !! \param nrow  Nr. of rows corresponding to the block.
  !! \param ncol  Nr. of columns corresponding to the block.
  !!
  subroutine getpositions(self, mygrid, ii, ib, prow, pcol, lrow, lcol, &
      & nrow, ncol)
    class(linecomm), intent(in) :: self
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: ii, ib
    integer, intent(out) :: prow, pcol, lrow, lcol, nrow, ncol

    integer :: grow, gcol

    if (self%rowcollect) then
      grow = ii
      gcol = (ib - 1) * self%desc(NB_) + 1
      nrow = 1
      ncol = self%getblocksize(ib)
    else
      grow = (ib - 1) * self%desc(MB_) + 1
      gcol = ii
      nrow = self%getblocksize(ib)
      ncol = 1
    end if
    call scalafx_infog2l(mygrid, self%desc, grow, gcol, lrow, lcol, prow, pcol)

  end subroutine getpositions


end module linecomm_module
