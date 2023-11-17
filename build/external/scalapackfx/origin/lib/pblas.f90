
! ************************************************************************
! *** psyr / pher
! ************************************************************************





! ************************************************************************
! *** psyrk / pherk
! ************************************************************************





! ************************************************************************
! *** psyr2k / pher2k
! ************************************************************************





! ************************************************************************
! *** psymv / phemv
! ************************************************************************




! ************************************************************************
! *** psymm / phemm
! ************************************************************************



! ************************************************************************
! *** ptrmm
! ************************************************************************



! ************************************************************************
! *** pgemm
! ************************************************************************



! ************************************************************************
! *** ptran / ptranu / ptranc
! ************************************************************************




!> Interface to PBLAS routines
module pblas_module
  use scalapackfx_common_module
  implicit none
  private

  public :: psyr, pher
  public :: psyrk, pherk
  public :: psyr2k, pher2k
  public :: psymv, phemv
  public :: psymm, phemm
  public :: pgemm
  public :: ptrmm
  public :: ptran, ptranu
  public :: ptranc

  !> Symmetric rank one update.
  interface psyr

  !> Symmetric/hermitian rank one update (real).
  subroutine pssyr(uplo, nn, alpha, xx, ix, jx, descx, incx, aa, ia, ja, desca)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    real(sp), intent(in) :: alpha
    integer, intent(in) :: descx(*)
    real(sp), intent(in) :: xx(descx(LLD_), *)
    integer, intent(in) :: ix, jx
    integer, intent(in) :: incx
    integer, intent(in) :: desca(*)
    real(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
  end subroutine pssyr


  !> Symmetric/hermitian rank one update (dreal).
  subroutine pdsyr(uplo, nn, alpha, xx, ix, jx, descx, incx, aa, ia, ja, desca)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    real(dp), intent(in) :: alpha
    integer, intent(in) :: descx(*)
    real(dp), intent(in) :: xx(descx(LLD_), *)
    integer, intent(in) :: ix, jx
    integer, intent(in) :: incx
    integer, intent(in) :: desca(*)
    real(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
  end subroutine pdsyr

  end interface psyr

  !> Hermitian rank one update.
  interface pher

  !> Symmetric/hermitian rank one update (complex).
  subroutine pcher(uplo, nn, alpha, xx, ix, jx, descx, incx, aa, ia, ja, desca)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    real(sp), intent(in) :: alpha
    integer, intent(in) :: descx(*)
    complex(sp), intent(in) :: xx(descx(LLD_), *)
    integer, intent(in) :: ix, jx
    integer, intent(in) :: incx
    integer, intent(in) :: desca(*)
    complex(sp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
  end subroutine pcher


  !> Symmetric/hermitian rank one update (dcomplex).
  subroutine pzher(uplo, nn, alpha, xx, ix, jx, descx, incx, aa, ia, ja, desca)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    real(dp), intent(in) :: alpha
    integer, intent(in) :: descx(*)
    complex(dp), intent(in) :: xx(descx(LLD_), *)
    integer, intent(in) :: ix, jx
    integer, intent(in) :: incx
    integer, intent(in) :: desca(*)
    complex(dp), intent(inout) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
  end subroutine pzher

  end interface pher

  !> Symmetric rank-k update.
  interface psyrk

  !> Symmetric/hermitian rank-k update (real).
  subroutine pssyrk(uplo, trans, nn, kk, alpha, aa, ia, ja, desca, beta, cc,&
      & ic, jc, descc)
    import
    character, intent(in) :: uplo, trans
    integer, intent(in) :: nn, kk
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(sp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    real(sp), intent(inout) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pssyrk


  !> Symmetric/hermitian rank-k update (dreal).
  subroutine pdsyrk(uplo, trans, nn, kk, alpha, aa, ia, ja, desca, beta, cc,&
      & ic, jc, descc)
    import
    character, intent(in) :: uplo, trans
    integer, intent(in) :: nn, kk
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(dp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    real(dp), intent(inout) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pdsyrk

  end interface psyrk

  !> Hermitian rank-k update.
  interface pherk

  !> Symmetric/hermitian rank-k update (complex).
  subroutine pcherk(uplo, trans, nn, kk, alpha, aa, ia, ja, desca, beta, cc,&
      & ic, jc, descc)
    import
    character, intent(in) :: uplo, trans
    integer, intent(in) :: nn, kk
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(sp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    complex(sp), intent(inout) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pcherk


  !> Symmetric/hermitian rank-k update (dcomplex).
  subroutine pzherk(uplo, trans, nn, kk, alpha, aa, ia, ja, desca, beta, cc,&
      & ic, jc, descc)
    import
    character, intent(in) :: uplo, trans
    integer, intent(in) :: nn, kk
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(dp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    complex(dp), intent(inout) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pzherk

  end interface pherk

  !> Symmetric rank-k update.
  interface psyr2k

  !> Symmetric/hermitian rank-k update (real).
  subroutine pssyr2k(uplo, trans, nn, kk, alpha, aa, ia, ja, desca, bb, ib, jb, descb, beta, cc,&
      & ic, jc, descc)
    import
    character, intent(in) :: uplo, trans
    integer, intent(in) :: nn, kk
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    real(sp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    real(sp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    real(sp), intent(inout) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pssyr2k


  !> Symmetric/hermitian rank-k update (dreal).
  subroutine pdsyr2k(uplo, trans, nn, kk, alpha, aa, ia, ja, desca, bb, ib, jb, descb, beta, cc,&
      & ic, jc, descc)
    import
    character, intent(in) :: uplo, trans
    integer, intent(in) :: nn, kk
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    real(dp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    real(dp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    real(dp), intent(inout) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pdsyr2k

  end interface psyr2k

  !> Hermitian rank-k update.
  interface pher2k

  !> Symmetric/hermitian rank-k update (complex).
  subroutine pcher2k(uplo, trans, nn, kk, alpha, aa, ia, ja, desca, bb, ib, jb, descb, beta, cc,&
      & ic, jc, descc)
    import
    character, intent(in) :: uplo, trans
    integer, intent(in) :: nn, kk
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    complex(sp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    real(sp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    complex(sp), intent(inout) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pcher2k


  !> Symmetric/hermitian rank-k update (dcomplex).
  subroutine pzher2k(uplo, trans, nn, kk, alpha, aa, ia, ja, desca, bb, ib, jb, descb, beta, cc,&
      & ic, jc, descc)
    import
    character, intent(in) :: uplo, trans
    integer, intent(in) :: nn, kk
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    complex(dp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    real(dp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    complex(dp), intent(inout) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pzher2k

  end interface pher2k

  !> Symmetric matrix vector product
  interface psymv

  !> Symmetric/hermitian matrix vector product ($1).
  subroutine pssymv(uplo, nn, alpha, aa, ia, ja, desca, xx, ix, jx, descx, incx, &
      & beta, yy, iy, jy, descy, incy)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descx(*)
    real(sp), intent(in) :: xx(descx(LLD_), *)
    integer, intent(in) :: ix, jx, incx
    real(sp), intent(in) :: beta
    integer, intent(in) :: descy(*)
    real(sp), intent(inout) :: yy(descy(LLD_), *)
    integer, intent(in) :: iy, jy, incy
  end subroutine pssymv


  !> Symmetric/hermitian matrix vector product ($1).
  subroutine pdsymv(uplo, nn, alpha, aa, ia, ja, desca, xx, ix, jx, descx, incx, &
      & beta, yy, iy, jy, descy, incy)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descx(*)
    real(dp), intent(in) :: xx(descx(LLD_), *)
    integer, intent(in) :: ix, jx, incx
    real(dp), intent(in) :: beta
    integer, intent(in) :: descy(*)
    real(dp), intent(inout) :: yy(descy(LLD_), *)
    integer, intent(in) :: iy, jy, incy
  end subroutine pdsymv

  end interface psymv

  !> Hermitian matrix vector product
  interface phemv

  !> Symmetric/hermitian matrix vector product ($1).
  subroutine pchemv(uplo, nn, alpha, aa, ia, ja, desca, xx, ix, jx, descx, incx, &
      & beta, yy, iy, jy, descy, incy)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    complex(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descx(*)
    complex(sp), intent(in) :: xx(descx(LLD_), *)
    integer, intent(in) :: ix, jx, incx
    complex(sp), intent(in) :: beta
    integer, intent(in) :: descy(*)
    complex(sp), intent(inout) :: yy(descy(LLD_), *)
    integer, intent(in) :: iy, jy, incy
  end subroutine pchemv


  !> Symmetric/hermitian matrix vector product ($1).
  subroutine pzhemv(uplo, nn, alpha, aa, ia, ja, desca, xx, ix, jx, descx, incx, &
      & beta, yy, iy, jy, descy, incy)
    import
    character, intent(in) :: uplo
    integer, intent(in) :: nn
    complex(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descx(*)
    complex(dp), intent(in) :: xx(descx(LLD_), *)
    integer, intent(in) :: ix, jx, incx
    complex(dp), intent(in) :: beta
    integer, intent(in) :: descy(*)
    complex(dp), intent(inout) :: yy(descy(LLD_), *)
    integer, intent(in) :: iy, jy, incy
  end subroutine pzhemv

  end interface phemv

  !> Symmetric matrix general matrix product
  interface psymm

  !> Symmetric/hermitian matrix with general matrix product ($1).
  subroutine pssymm(side, uplo, mm, nn, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb, beta, cc, ic, jc, descc)
    import
    character, intent(in) :: side
    character, intent(in) :: uplo
    integer, intent(in) :: mm
    integer, intent(in) :: nn
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(sp), intent(in) :: beta
    integer, intent(in) :: descb(*)
    real(sp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    integer, intent(in) :: descc(*)
    real(sp), intent(in) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pssymm


  !> Symmetric/hermitian matrix with general matrix product ($1).
  subroutine pdsymm(side, uplo, mm, nn, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb, beta, cc, ic, jc, descc)
    import
    character, intent(in) :: side
    character, intent(in) :: uplo
    integer, intent(in) :: mm
    integer, intent(in) :: nn
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    real(dp), intent(in) :: beta
    integer, intent(in) :: descb(*)
    real(dp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    integer, intent(in) :: descc(*)
    real(dp), intent(in) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pdsymm

  end interface psymm

  !> Hermitian matrix general matrix product
  interface phemm

  !> Symmetric/hermitian matrix with general matrix product ($1).
  subroutine pchemm(side, uplo, mm, nn, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb, beta, cc, ic, jc, descc)
    import
    character, intent(in) :: side
    character, intent(in) :: uplo
    integer, intent(in) :: mm
    integer, intent(in) :: nn
    complex(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    complex(sp), intent(in) :: beta
    integer, intent(in) :: descb(*)
    complex(sp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    integer, intent(in) :: descc(*)
    complex(sp), intent(in) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pchemm


  !> Symmetric/hermitian matrix with general matrix product ($1).
  subroutine pzhemm(side, uplo, mm, nn, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb, beta, cc, ic, jc, descc)
    import
    character, intent(in) :: side
    character, intent(in) :: uplo
    integer, intent(in) :: mm
    integer, intent(in) :: nn
    complex(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    complex(dp), intent(in) :: beta
    integer, intent(in) :: descb(*)
    complex(dp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    integer, intent(in) :: descc(*)
    complex(dp), intent(in) :: cc(descc(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pzhemm

  end interface phemm

  !> Triangular matrix matrix product
  interface ptrmm

  !> Symmetric/hermitian matrix vector product (real).
  subroutine pstrmm(side, uplo, transa, diag, mm, nn, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb)
    import
    character, intent(in) :: side, uplo, transa, diag
    integer, intent(in) :: mm, nn
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    real(sp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
  end subroutine pstrmm


  !> Symmetric/hermitian matrix vector product (dreal).
  subroutine pdtrmm(side, uplo, transa, diag, mm, nn, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb)
    import
    character, intent(in) :: side, uplo, transa, diag
    integer, intent(in) :: mm, nn
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    real(dp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
  end subroutine pdtrmm


  !> Symmetric/hermitian matrix vector product (complex).
  subroutine pctrmm(side, uplo, transa, diag, mm, nn, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb)
    import
    character, intent(in) :: side, uplo, transa, diag
    integer, intent(in) :: mm, nn
    complex(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    complex(sp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
  end subroutine pctrmm


  !> Symmetric/hermitian matrix vector product (dcomplex).
  subroutine pztrmm(side, uplo, transa, diag, mm, nn, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb)
    import
    character, intent(in) :: side, uplo, transa, diag
    integer, intent(in) :: mm, nn
    complex(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    complex(dp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
  end subroutine pztrmm

  end interface ptrmm

  !> General matrix matrix product
  interface pgemm

  !> General matrix matrix product (real).
  subroutine psgemm(transa, transb, mm, nn, kk, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb, beta, cc, ic, jc, descc)
    import
    character, intent(in) :: transa, transb
    integer, intent(in) :: mm, nn, kk
    real(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    real(sp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    real(sp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    real(sp), intent(inout) :: cc(descb(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine psgemm


  !> General matrix matrix product (dreal).
  subroutine pdgemm(transa, transb, mm, nn, kk, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb, beta, cc, ic, jc, descc)
    import
    character, intent(in) :: transa, transb
    integer, intent(in) :: mm, nn, kk
    real(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    real(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    real(dp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    real(dp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    real(dp), intent(inout) :: cc(descb(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pdgemm


  !> General matrix matrix product (complex).
  subroutine pcgemm(transa, transb, mm, nn, kk, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb, beta, cc, ic, jc, descc)
    import
    character, intent(in) :: transa, transb
    integer, intent(in) :: mm, nn, kk
    complex(sp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(sp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    complex(sp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    complex(sp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    complex(sp), intent(inout) :: cc(descb(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pcgemm


  !> General matrix matrix product (dcomplex).
  subroutine pzgemm(transa, transb, mm, nn, kk, alpha, aa, ia, ja, desca, &
      & bb, ib, jb, descb, beta, cc, ic, jc, descc)
    import
    character, intent(in) :: transa, transb
    integer, intent(in) :: mm, nn, kk
    complex(dp), intent(in) :: alpha
    integer, intent(in) :: desca(*)
    complex(dp), intent(in) :: aa(desca(LLD_), *)
    integer, intent(in) :: ia, ja
    integer, intent(in) :: descb(*)
    complex(dp), intent(in) :: bb(descb(LLD_), *)
    integer, intent(in) :: ib, jb
    complex(dp), intent(in) :: beta
    integer, intent(in) :: descc(*)
    complex(dp), intent(inout) :: cc(descb(LLD_), *)
    integer, intent(in) :: ic, jc
  end subroutine pzgemm

  end interface pgemm

  !> Real matrix transpose.
  interface ptran

  !> Transpose of a distributed matrix ($COMMENT$).
  subroutine pstran(mm, nn, alpha, aa, ia, ja, desca, beta, cc, ic, jc, descc)
    import
    integer, intent(in)   :: mm, nn
    real(sp), intent(in)    :: alpha
    integer, intent(in)   :: ia, ja
    integer, intent(in)   :: desca(*)
    real(sp), intent(in)    :: aa(desca(LLD_), *)
    real(sp), intent(in)    :: beta
    integer, intent(in)   :: ic, jc
    integer, intent(in)   :: descc(*)
    real(sp), intent(inout) :: cc(descc(LLD_), *)
  end subroutine pstran


  !> Transpose of a distributed matrix ($COMMENT$).
  subroutine pdtran(mm, nn, alpha, aa, ia, ja, desca, beta, cc, ic, jc, descc)
    import
    integer, intent(in)   :: mm, nn
    real(dp), intent(in)    :: alpha
    integer, intent(in)   :: ia, ja
    integer, intent(in)   :: desca(*)
    real(dp), intent(in)    :: aa(desca(LLD_), *)
    real(dp), intent(in)    :: beta
    integer, intent(in)   :: ic, jc
    integer, intent(in)   :: descc(*)
    real(dp), intent(inout) :: cc(descc(LLD_), *)
  end subroutine pdtran

  end interface ptran

  !> Complex matrix transpose.
  interface ptranu

  !> Transpose of a distributed matrix ($COMMENT$).
  subroutine pctranu(mm, nn, alpha, aa, ia, ja, desca, beta, cc, ic, jc, descc)
    import
    integer, intent(in)   :: mm, nn
    complex(sp), intent(in)    :: alpha
    integer, intent(in)   :: ia, ja
    integer, intent(in)   :: desca(*)
    complex(sp), intent(in)    :: aa(desca(LLD_), *)
    complex(sp), intent(in)    :: beta
    integer, intent(in)   :: ic, jc
    integer, intent(in)   :: descc(*)
    complex(sp), intent(inout) :: cc(descc(LLD_), *)
  end subroutine pctranu


  !> Transpose of a distributed matrix ($COMMENT$).
  subroutine pztranu(mm, nn, alpha, aa, ia, ja, desca, beta, cc, ic, jc, descc)
    import
    integer, intent(in)   :: mm, nn
    complex(dp), intent(in)    :: alpha
    integer, intent(in)   :: ia, ja
    integer, intent(in)   :: desca(*)
    complex(dp), intent(in)    :: aa(desca(LLD_), *)
    complex(dp), intent(in)    :: beta
    integer, intent(in)   :: ic, jc
    integer, intent(in)   :: descc(*)
    complex(dp), intent(inout) :: cc(descc(LLD_), *)
  end subroutine pztranu

  end interface ptranu

  !> Complex hermitian matrix transpose.
  interface ptranc

  !> Transpose of a distributed matrix ($COMMENT$).
  subroutine pctranc(mm, nn, alpha, aa, ia, ja, desca, beta, cc, ic, jc, descc)
    import
    integer, intent(in)   :: mm, nn
    complex(sp), intent(in)    :: alpha
    integer, intent(in)   :: ia, ja
    integer, intent(in)   :: desca(*)
    complex(sp), intent(in)    :: aa(desca(LLD_), *)
    complex(sp), intent(in)    :: beta
    integer, intent(in)   :: ic, jc
    integer, intent(in)   :: descc(*)
    complex(sp), intent(inout) :: cc(descc(LLD_), *)
  end subroutine pctranc


  !> Transpose of a distributed matrix ($COMMENT$).
  subroutine pztranc(mm, nn, alpha, aa, ia, ja, desca, beta, cc, ic, jc, descc)
    import
    integer, intent(in)   :: mm, nn
    complex(dp), intent(in)    :: alpha
    integer, intent(in)   :: ia, ja
    integer, intent(in)   :: desca(*)
    complex(dp), intent(in)    :: aa(desca(LLD_), *)
    complex(dp), intent(in)    :: beta
    integer, intent(in)   :: ic, jc
    integer, intent(in)   :: descc(*)
    complex(dp), intent(inout) :: cc(descc(LLD_), *)
  end subroutine pztranc

  end interface ptranc

end module pblas_module
