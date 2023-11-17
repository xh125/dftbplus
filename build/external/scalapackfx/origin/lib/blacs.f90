
!> Interfaces to BLACS routines.
module blacs_module
  use scalapackfx_common_module
  implicit none
  private

  public :: blacs_pinfo, blacs_get, blacs_gridinfo, blacs_gridinit
  public :: blacs_barrier, blacs_exit, blacs_abort, blacs_pnum
  public :: gebs2d, gebr2d, gesd2d, gerv2d, gsum2d, gemr2d


  !> Performs 2d copy from data in matrix to another, potentially with different distribution
  !> patterns.
  !> See BLACS documentation for details.
  interface gemr2d
    module procedure igemr2d
    module procedure sgemr2d
    module procedure dgemr2d
    module procedure cgemr2d
    module procedure zgemr2d
  end interface gemr2d

  interface

    !> Returns the number of processes available for use.
    !! \see BLACS documentation for details.
    subroutine blacs_pinfo(id, nproc)
      integer, intent(out) :: id, nproc
    end subroutine blacs_pinfo

    !> Gets values that BLACS uses for internal defaults.
    !! \see BLACS documentation for details.
    subroutine blacs_get(ictxt, what, val)
      integer, intent(in) :: ictxt, what
      integer, intent(out) :: val
    end subroutine blacs_get

    !> Delivers information about the grid.
    !! \see BLACS documentation for details.
    subroutine blacs_gridinfo(ictxt, nprow, npcol, myrow, mycol)
      integer, intent(in) :: ictxt,nprow, npcol
      integer, intent(out) :: myrow, mycol
    end subroutine blacs_gridinfo

    !> Assigns available processes into BLACS process grid.
    !! \see BLACS documentation for details.
    subroutine blacs_gridinit(ictxt, order, nprow, npcol)
      integer, intent(inout) :: ictxt
      character, intent(in) :: order
      integer, intent(in) :: nprow, npcol
    end subroutine blacs_gridinit

    !> Creates a customized process grid.
    !! \see BLACS documentation for details.
    subroutine blacs_gridmap(ictxt, umap, ldumap, nprow, npcol)
      integer, intent(inout) :: ictxt
      integer, intent(in) :: ldumap
      integer, intent(in) :: umap(ldumap, *), nprow, npcol
    end subroutine blacs_gridmap

    !> Calls a barrier.
    !! \see BLACS documentation for details.
    subroutine blacs_barrier(ictxt, scope)
      integer, intent(in) :: ictxt
      character, intent(in) :: scope
    end subroutine blacs_barrier

    !> Exits blacs communicator.
    !! \see BLACS documentation for details.
    subroutine blacs_exit(cont)
      integer, intent(in) :: cont
    end subroutine blacs_exit

    !> Frees all BLACS contexts and releases all allocated memory.
    !! \see BLACS documentation for details.
    subroutine blacs_abort(ictxt, errornum)
      integer, intent(in) :: ictxt, errornum
    end subroutine blacs_abort

    !> Returns the system process number of the process in the process grid.
    !! \see BLACS documentation for details.
    function blacs_pnum(ictxt, prow, pcol) result(res)
      integer, intent(in) :: ictxt, prow, pcol
      integer :: res
    end function blacs_pnum

    !> Returns row and column of a process in the grid
    !! \see BLACS documentation for details.
    subroutine blacs_pcoord(ictxt, pnum, prow, pcol)
      integer, intent(in) :: ictxt, pnum
      integer, intent(out) :: prow, pcol
    end subroutine blacs_pcoord

  end interface

!##########################################################################
!##########################################################################
!##########################################################################











!##########################################################################
!##########################################################################
!##########################################################################

  !> Wrapper around type specific ?gebs2d subroutines.
  !! \see BLACS documentation for details.
  interface gebs2d

    !> Starts broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine igebs2d(ictxt, scope, top, mm, nn, aa, lda)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      integer, intent(in) :: aa(lda,*)
    end subroutine igebs2d


    !> Starts broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine sgebs2d(ictxt, scope, top, mm, nn, aa, lda)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      real(sp), intent(in) :: aa(lda,*)
    end subroutine sgebs2d


    !> Starts broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine dgebs2d(ictxt, scope, top, mm, nn, aa, lda)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      real(dp), intent(in) :: aa(lda,*)
    end subroutine dgebs2d


    !> Starts broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine cgebs2d(ictxt, scope, top, mm, nn, aa, lda)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      complex(sp), intent(in) :: aa(lda,*)
    end subroutine cgebs2d


    !> Starts broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine zgebs2d(ictxt, scope, top, mm, nn, aa, lda)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      complex(dp), intent(in) :: aa(lda,*)
    end subroutine zgebs2d

  end interface gebs2d

  !> Wrapper around type specific ?gebr2d subroutines.
  !! \see BLACS documentation for details.
  interface gebr2d

    !> Receives broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine igebr2d(ictxt, scope, top, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      integer, intent(out) :: aa(lda,*)
      integer, intent(in) :: rsrc, csrc
    end subroutine igebr2d


    !> Receives broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine sgebr2d(ictxt, scope, top, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      real(sp), intent(out) :: aa(lda,*)
      integer, intent(in) :: rsrc, csrc
    end subroutine sgebr2d


    !> Receives broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine dgebr2d(ictxt, scope, top, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      real(dp), intent(out) :: aa(lda,*)
      integer, intent(in) :: rsrc, csrc
    end subroutine dgebr2d


    !> Receives broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine cgebr2d(ictxt, scope, top, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      complex(sp), intent(out) :: aa(lda,*)
      integer, intent(in) :: rsrc, csrc
    end subroutine cgebr2d


    !> Receives broadcast for general rectangular matrix.
    !! \see BLACS documentation for details.
    subroutine zgebr2d(ictxt, scope, top, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      complex(dp), intent(out) :: aa(lda,*)
      integer, intent(in) :: rsrc, csrc
    end subroutine zgebr2d

  end interface gebr2d

  !> Wrapper around type specific ?gesd2d subroutines.
  !! \see BLACS documentation for details.
  interface gesd2d

    !> Sends general rectangular matrix to destination.
    !! \see BLACS documentation for details.
    subroutine igesd2d(ictxt, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rdest, cdest
      integer, intent(in) :: aa(lda,*)
    end subroutine igesd2d


    !> Sends general rectangular matrix to destination.
    !! \see BLACS documentation for details.
    subroutine sgesd2d(ictxt, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rdest, cdest
      real(sp), intent(in) :: aa(lda,*)
    end subroutine sgesd2d


    !> Sends general rectangular matrix to destination.
    !! \see BLACS documentation for details.
    subroutine dgesd2d(ictxt, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rdest, cdest
      real(dp), intent(in) :: aa(lda,*)
    end subroutine dgesd2d


    !> Sends general rectangular matrix to destination.
    !! \see BLACS documentation for details.
    subroutine cgesd2d(ictxt, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rdest, cdest
      complex(sp), intent(in) :: aa(lda,*)
    end subroutine cgesd2d


    !> Sends general rectangular matrix to destination.
    !! \see BLACS documentation for details.
    subroutine zgesd2d(ictxt, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rdest, cdest
      complex(dp), intent(in) :: aa(lda,*)
    end subroutine zgesd2d

  end interface gesd2d

  !> Wrapper around type specific ?gerv2d subroutines.
  !! \see BLACS documentation for details.
  interface gerv2d

    !> Receives general rectangular matrix from process ($1).
    !! \see BLACS documentation for details.
    subroutine igerv2d(ictxt, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rsrc, csrc
      integer, intent(out) :: aa(lda,*)
    end subroutine igerv2d


    !> Receives general rectangular matrix from process ($1).
    !! \see BLACS documentation for details.
    subroutine sgerv2d(ictxt, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rsrc, csrc
      real(sp), intent(out) :: aa(lda,*)
    end subroutine sgerv2d


    !> Receives general rectangular matrix from process ($1).
    !! \see BLACS documentation for details.
    subroutine dgerv2d(ictxt, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rsrc, csrc
      real(dp), intent(out) :: aa(lda,*)
    end subroutine dgerv2d


    !> Receives general rectangular matrix from process ($1).
    !! \see BLACS documentation for details.
    subroutine cgerv2d(ictxt, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rsrc, csrc
      complex(sp), intent(out) :: aa(lda,*)
    end subroutine cgerv2d


    !> Receives general rectangular matrix from process ($1).
    !! \see BLACS documentation for details.
    subroutine zgerv2d(ictxt, mm, nn, aa, lda, rsrc, csrc)
      import
      integer, intent(in) :: ictxt, mm, nn
      integer, intent(in) :: lda, rsrc, csrc
      complex(dp), intent(out) :: aa(lda,*)
    end subroutine zgerv2d

  end interface gerv2d

  !> Wrapper around type specific ?gsum2d subroutines.
  !! \see BLACS documentation for details.
  interface gsum2d

    !> Performs element-wise summation.
    !! \see BLACS documentation for details.
    subroutine igsum2d(ictxt, scope, top, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      integer, intent(inout) :: aa(lda,*)
      integer, intent(in) :: rdest, cdest
    end subroutine igsum2d


    !> Performs element-wise summation.
    !! \see BLACS documentation for details.
    subroutine sgsum2d(ictxt, scope, top, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      real(sp), intent(inout) :: aa(lda,*)
      integer, intent(in) :: rdest, cdest
    end subroutine sgsum2d


    !> Performs element-wise summation.
    !! \see BLACS documentation for details.
    subroutine dgsum2d(ictxt, scope, top, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      real(dp), intent(inout) :: aa(lda,*)
      integer, intent(in) :: rdest, cdest
    end subroutine dgsum2d


    !> Performs element-wise summation.
    !! \see BLACS documentation for details.
    subroutine cgsum2d(ictxt, scope, top, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      complex(sp), intent(inout) :: aa(lda,*)
      integer, intent(in) :: rdest, cdest
    end subroutine cgsum2d


    !> Performs element-wise summation.
    !! \see BLACS documentation for details.
    subroutine zgsum2d(ictxt, scope, top, mm, nn, aa, lda, rdest, cdest)
      import
      integer, intent(in) :: ictxt
      character, intent(in) :: scope, top
      integer, intent(in) :: mm, nn
      integer, intent(in) :: lda
      complex(dp), intent(inout) :: aa(lda,*)
      integer, intent(in) :: rdest, cdest
    end subroutine zgsum2d

  end interface gsum2d

contains

  !> Interface for i case of the p?gemr2d routine, explictly wrapped to work around the
  !> lack of assumed size in interfaces (Fortran2018)
  subroutine igemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb
    integer, intent(in) :: descA(DLEN_), descB(DLEN_)
    integer, intent(in) :: aa(:,:)
    integer, intent(inout) :: bb(:,:)
    integer, intent(in) :: ictxt
    external pigemr2d

    call pigemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine igemr2d

  !> Interface for s case of the p?gemr2d routine, explictly wrapped to work around the
  !> lack of assumed size in interfaces (Fortran2018)
  subroutine sgemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb
    integer, intent(in) :: descA(DLEN_), descB(DLEN_)
    real(sp), intent(in) :: aa(:,:)
    real(sp), intent(inout) :: bb(:,:)
    integer, intent(in) :: ictxt
    external psgemr2d

    call psgemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine sgemr2d

  !> Interface for d case of the p?gemr2d routine, explictly wrapped to work around the
  !> lack of assumed size in interfaces (Fortran2018)
  subroutine dgemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb
    integer, intent(in) :: descA(DLEN_), descB(DLEN_)
    real(dp), intent(in) :: aa(:,:)
    real(dp), intent(inout) :: bb(:,:)
    integer, intent(in) :: ictxt
    external pdgemr2d

    call pdgemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine dgemr2d

  !> Interface for c case of the p?gemr2d routine, explictly wrapped to work around the
  !> lack of assumed size in interfaces (Fortran2018)
  subroutine cgemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb
    integer, intent(in) :: descA(DLEN_), descB(DLEN_)
    complex(sp), intent(in) :: aa(:,:)
    complex(sp), intent(inout) :: bb(:,:)
    integer, intent(in) :: ictxt
    external pcgemr2d

    call pcgemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine cgemr2d

  !> Interface for z case of the p?gemr2d routine, explictly wrapped to work around the
  !> lack of assumed size in interfaces (Fortran2018)
  subroutine zgemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb
    integer, intent(in) :: descA(DLEN_), descB(DLEN_)
    complex(dp), intent(in) :: aa(:,:)
    complex(dp), intent(inout) :: bb(:,:)
    integer, intent(in) :: ictxt
    external pzgemr2d

    call pzgemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine zgemr2d


end module blacs_module
