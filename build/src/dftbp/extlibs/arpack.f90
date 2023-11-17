!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!



!> Interfaces for the ARPACK routines needed in DFTB+ (currently for the linear response excited
!> state calculations).
module dftbp_extlibs_arpack
  use dftbp_common_accuracy, only : rsp, rdp
  use dftbp_io_message
  implicit none
  private

  public :: withArpack, saupd, seupd


  !> Whether code was built with ARPACK support
  logical, parameter :: withArpack = .false.

  !> Dummy routines, as ARPACK library is not compiled in
  interface saupd
    module procedure ssaupd
    module procedure dsaupd
  end interface saupd

  !> Dummy routines, as ARPACK library is not compiled in
  interface seupd
    module procedure sseupd
    module procedure dseupd
  end interface

contains

  !> Generates error message, if a stub was called
  subroutine stubError(routineName)
    character(*), intent(in) :: routineName

    call error("Internal error: " // trim(routineName) // "() called in a build without ARPACK&
        & support")

  end subroutine stubError


    !> Dummy ARPACK routine
    !> single precision Arnoldi solver call
    subroutine ssaupd(ido, bmat, n, which, nev, tol, resid, ncv, v, ldv, iparam, ipntr,&
        & workd, workl, lworkl, info)
      integer, intent(inout) :: ido
      character, intent(in) :: bmat
      integer, intent(in) :: n
      character(2), intent(in) :: which
      integer, intent(in) :: nev
      real(rsp), intent(in) :: tol
      real(rsp), intent(inout) :: resid(n)
      integer, intent(in) :: ncv
      integer, intent(in) :: ldv
      real(rsp), intent(out) :: v(ldv, ncv)
      integer, intent(inout) :: iparam(11)
      integer, intent(out) :: ipntr(11)
      real(rsp), intent(inout) :: workd(3 * n)
      integer, intent(in) :: lworkl
      real(rsp), intent(inout) :: workl(lworkl)
      integer, intent(inout) :: info
      call stubError("ssaupd")
    end subroutine ssaupd
    !> Dummy ARPACK routine
    !> double precision Arnoldi solver call
    subroutine dsaupd(ido, bmat, n, which, nev, tol, resid, ncv, v, ldv, iparam, ipntr,&
        & workd, workl, lworkl, info)
      integer, intent(inout) :: ido
      character, intent(in) :: bmat
      integer, intent(in) :: n
      character(2), intent(in) :: which
      integer, intent(in) :: nev
      real(rdp), intent(in) :: tol
      real(rdp), intent(inout) :: resid(n)
      integer, intent(in) :: ncv
      integer, intent(in) :: ldv
      real(rdp), intent(out) :: v(ldv, ncv)
      integer, intent(inout) :: iparam(11)
      integer, intent(out) :: ipntr(11)
      real(rdp), intent(inout) :: workd(3 * n)
      integer, intent(in) :: lworkl
      real(rdp), intent(inout) :: workl(lworkl)
      integer, intent(inout) :: info
      call stubError("dsaupd")
    end subroutine dsaupd

    !> Dummy ARPACK routine
    !> single precision return from the results of the solver
    subroutine sseupd(rvec, howmny, sel, d, z, ldz, sigma, bmat, n, which, nev, tol, resid,&
        & ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, info)
      logical, intent(in) :: rvec
      character, intent(in) :: howmny
      integer, intent(in) :: ncv
      logical, intent(in) :: sel(ncv)
      integer, intent(in) :: nev
      real(rsp), intent(out) :: d(nev)
      integer, intent(in) :: ldz
      real(rsp), intent(out) :: z(ldz, nev)
      real(rsp), intent(in) :: sigma
      character, intent(in) :: bmat
      integer, intent(in) :: n
      character(2), intent(in) :: which
      real(rsp), intent(in) :: tol
      real(rsp), intent(in) :: resid(n)
      integer, intent(in) :: ldv
      real(rsp), intent(inout) :: v(ldv, ncv)
      integer, intent(in) :: iparam(7)
      integer, intent(inout) :: ipntr(11)
      real(rsp), intent(inout) :: workd(2 * n)
      integer, intent(in) :: lworkl
      real(rsp), intent(inout) :: workl(lworkl)
      integer, intent(inout) :: info
      call stubError("sseupd")
    end subroutine sseupd
    !> Dummy ARPACK routine
    !> double precision return from the results of the solver
    subroutine dseupd(rvec, howmny, sel, d, z, ldz, sigma, bmat, n, which, nev, tol, resid,&
        & ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, info)
      logical, intent(in) :: rvec
      character, intent(in) :: howmny
      integer, intent(in) :: ncv
      logical, intent(in) :: sel(ncv)
      integer, intent(in) :: nev
      real(rdp), intent(out) :: d(nev)
      integer, intent(in) :: ldz
      real(rdp), intent(out) :: z(ldz, nev)
      real(rdp), intent(in) :: sigma
      character, intent(in) :: bmat
      integer, intent(in) :: n
      character(2), intent(in) :: which
      real(rdp), intent(in) :: tol
      real(rdp), intent(in) :: resid(n)
      integer, intent(in) :: ldv
      real(rdp), intent(inout) :: v(ldv, ncv)
      integer, intent(in) :: iparam(7)
      integer, intent(inout) :: ipntr(11)
      real(rdp), intent(inout) :: workd(2 * n)
      integer, intent(in) :: lworkl
      real(rdp), intent(inout) :: workl(lworkl)
      integer, intent(inout) :: info
      call stubError("dseupd")
    end subroutine dseupd

end module dftbp_extlibs_arpack
