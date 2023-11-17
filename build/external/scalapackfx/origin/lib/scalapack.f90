
!> Wrapper functions for scalapack.
module scalapack_module
  use scalapackfx_common_module
  implicit none
  private

  public :: psygst, phegst, psyev, pheev, psyevd, pheevd, psyevr, pheevr
  public :: ptrsm, ppotrf, ppotri, ptrtri, pgetrf, pgesvd
  public :: sl_init, numroc, infog2l, indxl2g, descinit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! ppotrf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! ppotri
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! ptrtri
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! pgetrf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! psygst
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! phegst
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! psyev
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! pheev
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! psyevd
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! pheevd
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! psyevr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! pheevr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! prgesvd
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! pcgesvd
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! ptrsm
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! SCALAPACK CORE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Cholesky factorization of symmetric/Hermitian positive definite matrix.
  interface ppotrf
  !> Cholesky factorization of symmetric/Hermitian pos.def. matrix (real).
  subroutine pspotrf(uplo, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pspotrf
  !> Cholesky factorization of symmetric/Hermitian pos.def. matrix (dreal).
  subroutine pdpotrf(uplo, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pdpotrf
  !> Cholesky factorization of symmetric/Hermitian pos.def. matrix (complex).
  subroutine pcpotrf(uplo, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pcpotrf
  !> Cholesky factorization of symmetric/Hermitian pos.def. matrix (dcomplex).
  subroutine pzpotrf(uplo, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pzpotrf
  end interface ppotrf

  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix.
  interface ppotri
  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix (real).
  subroutine pspotri(uplo, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pspotri
  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix (dreal).
  subroutine pdpotri(uplo, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pdpotri
  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix (complex).
  subroutine pcpotri(uplo, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pcpotri
  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix (dcomplex).
  subroutine pzpotri(uplo, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pzpotri
  end interface ppotri

  !> Inversion of a triangular matrix.
  interface ptrtri
  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix (real).
  subroutine pstrtri(uplo, diag, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo, diag
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pstrtri
  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix (dreal).
  subroutine pdtrtri(uplo, diag, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo, diag
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pdtrtri
  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix (complex).
  subroutine pctrtri(uplo, diag, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo, diag
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pctrtri
  !> Inversion of a Cholesky decomposed symmetric/Hermitian matrix (dcomplex).
  subroutine pztrtri(uplo, diag, nn, aa, ia, ja, desca, info)
    import
    character, intent(in) :: uplo, diag
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: info
  end subroutine pztrtri
  end interface ptrtri

  !> LU decomposition of a general matrix with pivoting
  interface pgetrf
  !> LU factorization of a general matrix with pivoting (real).
  subroutine psgetrf(mm, nn, aa, ia, ja, desca, ipiv, info)
    import
    integer, intent(in) :: mm
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: ipiv(*)
    integer, intent(out) :: info
  end subroutine psgetrf
  !> LU factorization of a general matrix with pivoting (dreal).
  subroutine pdgetrf(mm, nn, aa, ia, ja, desca, ipiv, info)
    import
    integer, intent(in) :: mm
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: ipiv(*)
    integer, intent(out) :: info
  end subroutine pdgetrf
  !> LU factorization of a general matrix with pivoting (complex).
  subroutine pcgetrf(mm, nn, aa, ia, ja, desca, ipiv, info)
    import
    integer, intent(in) :: mm
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: ipiv(*)
    integer, intent(out) :: info
  end subroutine pcgetrf
  !> LU factorization of a general matrix with pivoting (dcomplex).
  subroutine pzgetrf(mm, nn, aa, ia, ja, desca, ipiv, info)
    import
    integer, intent(in) :: mm
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(out) :: ipiv(*)
    integer, intent(out) :: info
  end subroutine pzgetrf
  end interface pgetrf

  !> Reduces generalized symmetric eigenvalue problem to standard form.
  interface psygst
  !> Reduces generalized symmetric eigenvalue problem to standard form (real).
  subroutine pssygst(ibtype, uplo, nn, aa, ia, ja, desca, bb, ib,&
    & jb, descb, scale, info)
    import
    integer, intent(in) :: ibtype
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ib, jb, descb(*)
    real(sp), intent(in) :: bb(descb(LLD_), *)
    real(sp), intent(out) :: scale
    integer, intent(out) :: info
  end subroutine pssygst
  !> Reduces generalized symmetric eigenvalue problem to standard form (dreal).
  subroutine pdsygst(ibtype, uplo, nn, aa, ia, ja, desca, bb, ib,&
    & jb, descb, scale, info)
    import
    integer, intent(in) :: ibtype
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ib, jb, descb(*)
    real(dp), intent(in) :: bb(descb(LLD_), *)
    real(dp), intent(out) :: scale
    integer, intent(out) :: info
  end subroutine pdsygst
  end interface psygst

  !> Reduces generalized Hermitian eigenvalue problem to standard form.
  interface phegst
  !> Reduces generalized Hermitian eigenvalue problem to standard form (complex).
  subroutine pchegst(ibtype, uplo, nn, aa, ia, ja, desca, bb, ib,&
    & jb, descb, scale, info)
    import
    integer, intent(in) :: ibtype
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ib, jb, descb(*)
    complex(sp), intent(in) :: bb(descb(LLD_), *)
    real(sp), intent(out) :: scale
    integer, intent(out) :: info
  end subroutine pchegst
  !> Reduces generalized Hermitian eigenvalue problem to standard form (dcomplex).
  subroutine pzhegst(ibtype, uplo, nn, aa, ia, ja, desca, bb, ib,&
    & jb, descb, scale, info)
    import
    integer, intent(in) :: ibtype
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ib, jb, descb(*)
    complex(dp), intent(in) :: bb(descb(LLD_), *)
    real(dp), intent(out) :: scale
    integer, intent(out) :: info
  end subroutine pzhegst
  end interface phegst

  !> Solves the symmetric eigenvalue problem.
  interface psyev
  !> Eigenvalues and eigenvectors by divide and conquer algorithm (real)
  subroutine pssyev(jobz, uplo, nn, aa, ia, ja, desca, ww, zz,&
    & iz, jz, descz, work, lwork, info)
    import
    character, intent(in) :: jobz, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: iz, jz, descz(*)
    real(sp), intent(out) :: ww(nn), zz(descz(LLD_),*)
    real(sp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    integer, intent(out) :: info
  end subroutine pssyev
  !> Eigenvalues and eigenvectors by divide and conquer algorithm (dreal)
  subroutine pdsyev(jobz, uplo, nn, aa, ia, ja, desca, ww, zz,&
    & iz, jz, descz, work, lwork, info)
    import
    character, intent(in) :: jobz, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: iz, jz, descz(*)
    real(dp), intent(out) :: ww(nn), zz(descz(LLD_),*)
    real(dp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    integer, intent(out) :: info
  end subroutine pdsyev
  end interface psyev

  !> Solves the Hermitian eigenvalue problem.
  interface pheev
  !> Eigenvalues and eigenvectors by divide and conquer algorithm (complex)
  subroutine pcheev(jobz, uplo, nn, aa, ia, ja, desca, ww, zz, iz, jz,&
      & descz, work, lwork, rwork, lrwork, info)
    import
    character, intent(in) :: jobz, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: iz, jz, descz(*)
    real(sp), intent(out) :: ww(nn)
    complex(sp), intent(out) ::  zz(descz(LLD_),*)
    complex(sp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    real(sp), intent(inout) :: rwork(*)
    integer, intent(in) :: lrwork
    integer, intent(out) :: info
  end subroutine pcheev
  !> Eigenvalues and eigenvectors by divide and conquer algorithm (dcomplex)
  subroutine pzheev(jobz, uplo, nn, aa, ia, ja, desca, ww, zz, iz, jz,&
      & descz, work, lwork, rwork, lrwork, info)
    import
    character, intent(in) :: jobz, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: iz, jz, descz(*)
    real(dp), intent(out) :: ww(nn)
    complex(dp), intent(out) ::  zz(descz(LLD_),*)
    complex(dp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    real(dp), intent(inout) :: rwork(*)
    integer, intent(in) :: lrwork
    integer, intent(out) :: info
  end subroutine pzheev
  end interface pheev

  !> Solves the symmetric eigenvalue problem by divide and conquer algorithm.
  interface psyevd
  !> Eigenvalues and eigenvectors by divide and conquer algorithm (real)
  subroutine pssyevd(jobz, uplo, nn, aa, ia, ja, desca, ww, zz, iz, jz,&
      & descz, work, lwork, iwork, liwork, info)
    import
    character, intent(in) :: jobz, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: iz, jz, descz(*)
    real(sp), intent(out) :: ww(nn), zz(descz(LLD_),*)
    real(sp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liwork
    integer, intent(out) :: info
  end subroutine pssyevd
  !> Eigenvalues and eigenvectors by divide and conquer algorithm (dreal)
  subroutine pdsyevd(jobz, uplo, nn, aa, ia, ja, desca, ww, zz, iz, jz,&
      & descz, work, lwork, iwork, liwork, info)
    import
    character, intent(in) :: jobz, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: iz, jz, descz(*)
    real(dp), intent(out) :: ww(nn), zz(descz(LLD_),*)
    real(dp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liwork
    integer, intent(out) :: info
  end subroutine pdsyevd
  end interface psyevd

  !> Solves the Hermitian eigenvalue problem by divide and conquer algorithm.
  interface pheevd
  !> Eigenvalues and eigenvectors by divide and conquer algorithm (complex)
  subroutine pcheevd(jobz, uplo, nn, aa, ia, ja, desca, ww, zz, iz, jz,&
      & descz, work, lwork, rwork, lrwork, iwork, liwork, info)
    import
    character, intent(in) :: jobz, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: iz, jz, descz(*)
    real(sp), intent(out) :: ww(nn)
    complex(sp), intent(out) ::  zz(descz(LLD_),*)
    complex(sp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    real(sp), intent(inout) :: rwork(*)
    integer, intent(in) :: lrwork
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liwork
    integer, intent(out) :: info
  end subroutine pcheevd
  !> Eigenvalues and eigenvectors by divide and conquer algorithm (dcomplex)
  subroutine pzheevd(jobz, uplo, nn, aa, ia, ja, desca, ww, zz, iz, jz,&
      & descz, work, lwork, rwork, lrwork, iwork, liwork, info)
    import
    character, intent(in) :: jobz, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: iz, jz, descz(*)
    real(dp), intent(out) :: ww(nn)
    complex(dp), intent(out) ::  zz(descz(LLD_),*)
    complex(dp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    real(dp), intent(inout) :: rwork(*)
    integer, intent(in) :: lrwork
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liwork
    integer, intent(out) :: info
  end subroutine pzheevd
  end interface pheevd

  !> Solves the symmetric eigenvalue problem by the MRRR algorithm.
  interface psyevr
  !> Eigenvalues and eigenvectors by MRRR algorithm (real)
  subroutine pssyevr(jobz, range, uplo, nn, aa, ia, ja, desca, vl, vu,&
    & il, iu, mm, nz, ww, zz, iz, jz, descz, work, lwork, iwork, liwork, info)
    import
    character, intent(in) :: jobz, range, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(sp), intent(in) :: vl, vu
    integer, intent(in) :: il, iu
    integer, intent(out) :: mm, nz
    real(sp), intent(out) :: ww(nn)
    integer, intent(in) :: descz(*)
    real(sp), intent(out) :: zz(descz(LLD_),*)
    integer, intent(in) :: iz, jz
    real(sp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liwork
    integer, intent(out) :: info
  end subroutine pssyevr
  !> Eigenvalues and eigenvectors by MRRR algorithm (dreal)
  subroutine pdsyevr(jobz, range, uplo, nn, aa, ia, ja, desca, vl, vu,&
    & il, iu, mm, nz, ww, zz, iz, jz, descz, work, lwork, iwork, liwork, info)
    import
    character, intent(in) :: jobz, range, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(dp), intent(in) :: vl, vu
    integer, intent(in) :: il, iu
    integer, intent(out) :: mm, nz
    real(dp), intent(out) :: ww(nn)
    integer, intent(in) :: descz(*)
    real(dp), intent(out) :: zz(descz(LLD_),*)
    integer, intent(in) :: iz, jz
    real(dp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liwork
    integer, intent(out) :: info
  end subroutine pdsyevr
  end interface psyevr

  !> Solves the Hermitian eigenvalue problem by the MRRR algorithm.
  interface pheevr
  !> Eigenvalues and eigenvectors by MRRR algorithm (complex)
  subroutine pcheevr(jobz, range, uplo, nn, aa, ia, ja, desca, vl,&
    & vu, il, iu, mm, nz, ww, zz, iz, jz, descz, work, lwork, rwork, lrwork,&
    & iwork, liwork, info)
    import
    character, intent(in) :: jobz, range, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(sp), intent(in) :: vl, vu
    integer, intent(in) :: il, iu
    integer, intent(out) :: mm, nz
    real(sp), intent(out) :: ww(nn)
    integer, intent(in) :: descz(*)
    complex(sp), intent(out) ::  zz(descz(LLD_),*)
    integer, intent(in) :: iz, jz
    complex(sp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    real(sp), intent(inout) :: rwork(*)
    integer, intent(in) :: lrwork
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liwork
    integer, intent(out) :: info
  end subroutine pcheevr
  !> Eigenvalues and eigenvectors by MRRR algorithm (dcomplex)
  subroutine pzheevr(jobz, range, uplo, nn, aa, ia, ja, desca, vl,&
    & vu, il, iu, mm, nz, ww, zz, iz, jz, descz, work, lwork, rwork, lrwork,&
    & iwork, liwork, info)
    import
    character, intent(in) :: jobz, range, uplo
    integer, intent(in) :: nn
    integer, intent(in) :: desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(dp), intent(in) :: vl, vu
    integer, intent(in) :: il, iu
    integer, intent(out) :: mm, nz
    real(dp), intent(out) :: ww(nn)
    integer, intent(in) :: descz(*)
    complex(dp), intent(out) ::  zz(descz(LLD_),*)
    integer, intent(in) :: iz, jz
    complex(dp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    real(dp), intent(inout) :: rwork(*)
    integer, intent(in) :: lrwork
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liwork
    integer, intent(out) :: info
  end subroutine pzheevr
  end interface pheevr

  !> Singular value decomposition of a matrix
  interface pgesvd
  !> Singular values and vectors (real)
  subroutine psgesvd(jobu, jobvt, mm, nn, aa, ia, ja, desca, sigma,&
    & uu, iu, ju, descu, vt, ivt, jvt, descvt, work, lwork, info)
    import
    character, intent(in) :: jobu, jobvt
    integer, intent(in) :: mm, nn
    integer, intent(in) :: ia, ja, desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    real(sp), intent(out) :: sigma(*)
    integer, intent(in) :: iu, ju, descu(*)
    real(sp), intent(out) :: uu(descu(LLD_), *)
    integer, intent(in) :: ivt, jvt, descvt(*)
    real(sp), intent(out) :: vt(descvt(LLD_), *)
    real(sp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    integer, intent(out) :: info
  end subroutine psgesvd
  !> Singular values and vectors (dreal)
  subroutine pdgesvd(jobu, jobvt, mm, nn, aa, ia, ja, desca, sigma,&
    & uu, iu, ju, descu, vt, ivt, jvt, descvt, work, lwork, info)
    import
    character, intent(in) :: jobu, jobvt
    integer, intent(in) :: mm, nn
    integer, intent(in) :: ia, ja, desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    real(dp), intent(out) :: sigma(*)
    integer, intent(in) :: iu, ju, descu(*)
    real(dp), intent(out) :: uu(descu(LLD_), *)
    integer, intent(in) :: ivt, jvt, descvt(*)
    real(dp), intent(out) :: vt(descvt(LLD_), *)
    real(dp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    integer, intent(out) :: info
  end subroutine pdgesvd
  !> Singular values and vectors (complex)
  subroutine pcgesvd(jobu, jobvt, mm, nn, aa, ia, ja, desca, sigma,&
    & uu, iu, ju, descu, vt, ivt, jvt, descvt, work, lwork, rwork, info)
    import
    character, intent(in) :: jobu, jobvt
    integer, intent(in) :: mm, nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    real(sp), intent(out) :: sigma(*)
    integer, intent(in) :: iu, ju, descu(*)
    complex(sp), intent(out) :: uu(descu(LLD_), *)
    integer, intent(in) :: ivt, jvt, descvt(*)
    complex(sp), intent(out) :: vt(descvt(LLD_), *)
    complex(sp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    real(sp), intent(inout) :: rwork(*)
    integer, intent(out) :: info
  end subroutine pcgesvd
  !> Singular values and vectors (dcomplex)
  subroutine pzgesvd(jobu, jobvt, mm, nn, aa, ia, ja, desca, sigma,&
    & uu, iu, ju, descu, vt, ivt, jvt, descvt, work, lwork, rwork, info)
    import
    character, intent(in) :: jobu, jobvt
    integer, intent(in) :: mm, nn
    integer, intent(in) :: ia, ja, desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    real(dp), intent(out) :: sigma(*)
    integer, intent(in) :: iu, ju, descu(*)
    complex(dp), intent(out) :: uu(descu(LLD_), *)
    integer, intent(in) :: ivt, jvt, descvt(*)
    complex(dp), intent(out) :: vt(descvt(LLD_), *)
    complex(dp), intent(inout) :: work(*)
    integer, intent(in) :: lwork
    real(dp), intent(inout) :: rwork(*)
    integer, intent(out) :: info
  end subroutine pzgesvd
  end interface pgesvd

  !> Linear system of equation for triangular matrix.
  interface ptrsm
  !> Solves a triangular matrix equation (real).
  subroutine pstrsm(side, uplo, transa, diag, mm, nn, alpha, aa, ia, ja,&
      & desca, bb, ib, jb, descb)
    import
    character, intent(in) :: side, uplo, transa, diag
    integer, intent(in) :: mm, nn
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    real(sp), intent(inout) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
  end subroutine pstrsm
  !> Solves a triangular matrix equation (dreal).
  subroutine pdtrsm(side, uplo, transa, diag, mm, nn, alpha, aa, ia, ja,&
      & desca, bb, ib, jb, descb)
    import
    character, intent(in) :: side, uplo, transa, diag
    integer, intent(in) :: mm, nn
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    real(dp), intent(inout) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
  end subroutine pdtrsm
  !> Solves a triangular matrix equation (complex).
  subroutine pctrsm(side, uplo, transa, diag, mm, nn, alpha, aa, ia, ja,&
      & desca, bb, ib, jb, descb)
    import
    character, intent(in) :: side, uplo, transa, diag
    integer, intent(in) :: mm, nn
    complex(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    complex(sp), intent(inout) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
  end subroutine pctrsm
  !> Solves a triangular matrix equation (dcomplex).
  subroutine pztrsm(side, uplo, transa, diag, mm, nn, alpha, aa, ia, ja,&
      & desca, bb, ib, jb, descb)
    import
    character, intent(in) :: side, uplo, transa, diag
    integer, intent(in) :: mm, nn
    complex(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    complex(dp), intent(inout) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
  end subroutine pztrsm
  end interface ptrsm


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! SCALAPACK TOOLS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface

    !> Scalapack initialization routine.
    subroutine sl_init(ictxt, nprow, npcol)
      integer, intent(out) :: ictxt
      integer, intent(in) :: nprow, npcol
    end subroutine sl_init

    !> Number of rows or columns of distributed matrix owned by local process.
    function numroc(nn, nb, iproc, isrcproc, nproc)
      integer, intent(in) :: nn, nb, iproc, isrcproc, nproc
      integer :: numroc
    end function numroc

    !> Converts global matrix index into local.
    subroutine infog2l(grindx, gcindx, desc, nprow, npcol, myrow, mycol,&
        & lrindx, lcindx, rsrc, csrc)
      integer, intent(in) :: grindx, gcindx, desc(*)
      integer, intent(in) :: nprow, npcol, myrow, mycol
      integer, intent(out) :: lrindx, lcindx, rsrc, csrc
    end subroutine infog2l

    !> Converts local matrix index into global.
    function indxl2g(indxglob, nb, iproc, isrcproc, nprocs)
      integer :: indxl2g
      integer, intent(in) :: indxglob, nb, iproc, isrcproc, nprocs
    end function indxl2g

    !> Initializes a descriptor for a distributed array.
    subroutine descinit(desc, mm, nn, mb, nb, irsrc, icsrc, ictxt, lld, info)
      integer, intent(out) :: desc(*)
      integer, intent(in) :: mm, nn, mb, nb, irsrc, icsrc, ictxt, lld
      integer, intent(out) :: info
    end subroutine descinit

  end interface


end module scalapack_module

