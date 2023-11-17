!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!



!> Contains F90 wrapper functions for some commonly used blas calls needed in the code. The
!> interface of all BLAS calls must be defined in the module blas.
module dftbp_math_blasroutines
  use dftbp_common_accuracy, only : dp, elecTolMax, mc, lc, sc, rsp, rdp
  use dftbp_extlibs_blas, only : ssyr, cher, zher, sspmv, dspmv
  implicit none

  private
  public ::  gemv, gemm, ger, hemm, hemv, her, herk, her2k, sbmv, scal, spmv, symm, swap


  !> Rank 1 update of a matrix A := alpha*x*x' + A
  !> Wrapper for the level 2 blas routine xsyr to perform the rank 1 update of the chosen triangle
  interface her
    module procedure her_real
    module procedure her_cmplx
    module procedure her_dble
    module procedure her_dblecmplx
  end interface her


  !> Rank 1 update of a matrix A := alpha*x*y' + A
  !> Wrapper for the level 2 blas routine xger to perform the rank 1 update of a general matrix
  interface ger
    module procedure ger_real
    module procedure ger_cmplx
    module procedure ger_dble
    module procedure ger_dblecmplx
  end interface ger


  !> Symmetric matrix vector multiply y := alpha*A*x + beta*y
  !> Wrapper for the level 2 blas routine
  interface hemv
    module procedure symv_real
    module procedure symv_dble
    module procedure hemv_cmplx
    module procedure hemv_dblecmplx
  end interface hemv


  !> General matrix vector multiply y := alpha*a*x + beta*y
  !> Wrapper for the level 2 blas routine
  interface gemv
    module procedure gemv_real
    module procedure gemv_dble
      module procedure gemv231_real
      module procedure gemv231_dble
      module procedure gemv242_real
      module procedure gemv242_dble
  end interface gemv


  !> Banded symmetric matrix vector multiply y := alpha*A*x + beta*y
  !> Wrapper for the level 2 blas routine
  interface sbmv
    module procedure sbmv_real
    module procedure sbmv_dble
  end interface sbmv


  !> Interface to SYMM routines
  !> Wrapper for the level 3 blas routine
  interface symm
    module procedure symm_real
    module procedure symm_dble
  end interface symm


  !> Interface to GEMM routines evaluates C := alpha*op( A )*op( B ) + beta*C, where op( X ) is one
  !> of op( X ) = X or op( X ) = X'

  !> Wrapper for the level 3 blas routine
  interface gemm
    module procedure gemm_real
    module procedure gemm_dble
    module procedure gemm_cmplx
    module procedure gemm_dblecmplx
      module procedure gemm332_real
      module procedure gemm332_dble
  end interface gemm


  !> Wrapper for the level 3 blas routine syrk/herk to perform the rank k update of the chosen
  !> triangle of matrix C
  interface herk
    module procedure herk_real
    module procedure herk_cmplx
    module procedure herk_dble
    module procedure herk_dblecmplx
  end interface herk


  !> Wrapper for the level 3 blas routine syr2k/her2k to perform the rank 2k update of the chosen
  !> triangle of matrix C
  interface her2k
    module procedure her2k_real
    module procedure her2k_cmplx
    module procedure her2k_dble
    module procedure her2k_dblecmplx
  end interface her2k


  !> Interface to HEMM routines
  !> Wrapper for the level 3 blas routine
  interface hemm
    module procedure hemm_cmplx
    module procedure hemm_dblecmplx
  end interface hemm

 !> SCAL scales a vector or matrix x by a constant: x = alpha*x where alpha is a scalar
 !> Wrapper for the level 1 blas routine (only for matrices)
  interface scal
    module procedure scal_cmplx
    module procedure scal_dblecmplx
 end interface scal

 !> Given two vectors or matrices x and y, SWAP routines return vectors y and x swapped
 !> Wrapper for the level 1 blas routine (only for matrices)
 interface swap
    module procedure swap_cmplx
    module procedure swap_dblecmplx
 end interface swap


 !> Symmetric packed matrix-vector product
 interface spmv
   module procedure spmv_real
   module procedure spmv_dble
 end interface spmv


contains


  !> Real rank 1 update of a symmetric matrix
  subroutine her_real(a,alpha,x,uplo)

    !> contains the matrix for the update
    real(rsp), intent(inout) :: a(:,:)

    !> scaling value for the update contribution
    real(rsp), intent(in) :: alpha

    !> vector of values for the update
    real(rsp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    integer :: n
    character :: iUplo



    if (present(uplo)) then
      iuplo = uplo
    else
      iuplo = 'l'
    end if

    n = size(x)
    call ssyr(iuplo,n,alpha,x,1,a,n)
  end subroutine her_real


  !> Complex rank 1 update of a symmetric matrix
  subroutine her_cmplx(a,alpha,x,uplo)

    !> contains the matrix for the update
    complex(rsp), intent(inout) :: a(:,:)

    !> scaling value for the update contribution
    real(rsp), intent(in) :: alpha

    !> vector of values for the update
    complex(rsp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in),optional :: uplo

    integer :: n
    character :: iuplo



    if (present(uplo)) then
      iuplo = uplo
    else
      iuplo = 'l'
    end if

    n = size(x)
    call cher(iuplo,n,alpha,x,1,a,n)
  end subroutine her_cmplx


  !> Double precision rank 1 update of a symmetric matrix
  subroutine her_dble(a,alpha,x,uplo)

    !> contains the matrix for the update
    real(rdp), intent(inout) :: a(:,:)

    !> scaling value for the update contribution
    real(rdp), intent(in) :: alpha

    !> vector of values for the update
    real(rdp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    character :: iuplo
    integer :: n


    if (present(uplo)) then
      iuplo = uplo
    else
      iuplo = 'l'
    end if

    n = size(x)
    call dsyr(iuplo,n,alpha,x,1,a,n)
  end subroutine her_dble


  !> Double complex rank 1 update of a symmetric matrix
  subroutine her_dblecmplx(a,alpha,x,uplo)

    !> contains the matrix for the update
    complex(rdp), intent(inout) :: a(:,:)

    !> scaling value for the update contribution
    real(rdp), intent(in) :: alpha

    !> vector of values for the update
    complex(rdp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in),optional :: uplo

    integer :: n
    character :: iuplo


    if (present(uplo)) then
      iuplo = uplo
    else
      iuplo = 'l'
    end if

    n = size(x)
    call zher(iuplo,n,alpha,x,1,a,n)
  end subroutine her_dblecmplx


  !> Real rank 1 update of a general matrix
  subroutine ger_real(a,alpha,x,y)

    !> contains the matrix for the update
    real(rsp), intent(inout) :: a(:,:)

    !> scaling value for the update contribution
    real(rsp), intent(in) :: alpha

    !> vector of values for the update
    real(rsp), intent(in) :: x(:)

    !> vector of values for the update
    real(rsp), intent(in) :: y(:)

    integer :: n, m


    m = size(x)
    n = size(y)
    call sger(m,n,alpha,x,1,y,1,a,m)
  end subroutine ger_real


  !> complex rank 1 update of a general matrix
  subroutine ger_cmplx(a,alpha,x,y)

    !> contains the matrix for the update
    complex(rsp), intent(inout) :: a(:,:)

    !> scaling value for the update contribution
    complex(rsp), intent(in) :: alpha

    !> vector of values for the update
    complex(rsp), intent(in) :: x(:)

    !> vector of values for the update
    complex(rsp), intent(in) :: y(:)

    integer :: n, m


    m = size(x)
    n = size(y)
    call cgerc(m,n,alpha,x,1,y,1,a,m)
  end subroutine ger_cmplx


  !> Double precision rank 1 update of a general matrix
  subroutine ger_dble(a,alpha,x,y)

    !> contains the matrix for the update
    real(rdp), intent(inout) :: a(:,:)

    !> scaling value for the update contribution
    real(rdp), intent(in) :: alpha

    !> vector of values for the update
    real(rdp), intent(in) :: x(:)

    !> vector of values for the update
    real(rdp), intent(in) :: y(:)

    integer :: n, m


    m = size(x)
    n = size(y)
    call dger(m,n,alpha,x,1,y,1,a,m)
  end subroutine ger_dble


  !> complex rank 1 update of a general matrix
  subroutine ger_dblecmplx(a,alpha,x,y)

    !> contains the matrix for the update
    complex(rdp), intent(inout) :: a(:,:)

    !> scaling value for the update contribution
    complex(rdp), intent(in) :: alpha

    !> vector of values for the update
    complex(rdp), intent(in) :: x(:)

    !> vector of values for the update
    complex(rdp), intent(in) :: y(:)

    integer :: n, m


    m = size(x)
    n = size(y)
    call zgerc(m,n,alpha,x,1,y,1,a,m)
  end subroutine ger_dblecmplx


  !> real symmetric matrix*vector product
  subroutine symv_real(y,a,x,uplo,alpha,beta)

    !> vector
    real(rsp), intent(inout) :: y(:)

    !> symmetric matrix
    real(rsp), intent(in) :: a(:,:)

    !> vector
    real(rsp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle (defaults to lower)
    character, intent(in), optional :: uplo

    !> optional scaling factor (defaults to 1)
    real(rsp), intent(in), optional :: alpha

    !> optional scaling factor (defaults to 0)
    real(rsp), intent(in), optional :: beta

    integer :: n
    character :: iUplo
    real(rsp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if





    n = size(y)
    call ssymv( iUplo, n, iAlpha, a, n, x, 1, iBeta, y, 1 )

  end subroutine symv_real


  !> real symmetric matrix*vector product
  subroutine symv_dble(y,a,x,uplo,alpha,beta)

    !> vector
    real(rdp), intent(inout) :: y(:)

    !> symmetric matrix
    real(rdp), intent(in) :: a(:,:)

    !> vector
    real(rdp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle (defaults to lower)
    character, intent(in), optional :: uplo

    !> optional scaling factor (defaults to 1)
    real(rdp), intent(in), optional :: alpha

    !> optional scaling factor (defaults to 0)
    real(rdp), intent(in), optional :: beta

    integer :: n
    character :: iUplo
    real(rdp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if





    n = size(y)
    call dsymv( iUplo, n, iAlpha, a, n, x, 1, iBeta, y, 1 )

  end subroutine symv_dble


  !> complex hermitian matrix*vector product
  subroutine hemv_cmplx(y,a,x,uplo,alpha,beta)

    !> vector
    complex(rsp), intent(inout) :: y(:)

    !> symmetric matrix
    complex(rsp), intent(in) :: a(:,:)

    !> vector
    complex(rsp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle (defaults to lower)
    character, intent(in), optional :: uplo

    !> optional scaling factor (defaults to 1)
    complex(rsp), intent(in), optional :: alpha

    !> optional scaling factor (defaults to 0)
    complex(rsp), intent(in), optional :: beta

    integer :: n
    character :: iUplo
    complex(rsp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = cmplx(1.0,0.0,rsp)
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = cmplx(0.0,0.0,rsp)
    end if





    n = size(y)
    call chemv( iUplo, n, iAlpha, a, n, x, 1, iBeta, y, 1 )

  end subroutine hemv_cmplx


  !> double complex hermitian matrix*vector product
  subroutine hemv_dblecmplx(y,a,x,uplo,alpha,beta)

    !> vector
    complex(rdp), intent(inout) :: y(:)

    !> symmetric matrix
    complex(rdp), intent(in) :: a(:,:)

    !> vector
    complex(rdp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle (defaults to lower)
    character, intent(in), optional :: uplo

    !> optional scaling factor (defaults to 1)
    complex(rdp), intent(in), optional :: alpha

    !> optional scaling factor (defaults to 0)
    complex(rdp), intent(in), optional :: beta

    integer :: n
    character :: iUplo
    complex(rdp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = cmplx(1.0,0.0,rdp)
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = cmplx(0.0,0.0,rdp)
    end if





    n = size(y)
    call zhemv( iUplo, n, iAlpha, a, n, x, 1, iBeta, y, 1 )

  end subroutine hemv_dblecmplx


  !> real matrix*vector product
  subroutine gemv_real(y,a,x,alpha,beta,trans)

    !> vector
    real(rsp), intent(inout) :: y(:)

    !> matrix
    real(rsp), intent(in) :: a(:,:)

    !> vector
    real(rsp), intent(in) :: x(:)

    !> optional scaling factor (defaults to 1)
    real(rsp), intent(in), optional :: alpha

    !> optional scaling factor (defaults to 0)
    real(rsp), intent(in), optional :: beta

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c' and 'C'
    character, intent(in), optional :: trans

    integer :: n, m
    character :: iTrans
    real(rsp) :: iAlpha, iBeta

    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if





    m = size(a,dim=1)
    n = size(a,dim=2)

    call sgemv( iTrans, m, n, iAlpha, a, m, x, 1, iBeta, y, 1 )

  end subroutine gemv_real


  !> double precision matrix*vector product
  subroutine gemv_dble(y,a,x,alpha,beta,trans)

    !> vector
    real(rdp), intent(inout) :: y(:)

    !> matrix
    real(rdp), intent(in) :: a(:,:)

    !> vector
    real(rdp), intent(in) :: x(:)

    !> optional scaling factor (defaults to 1)
    real(rdp), intent(in), optional :: alpha

    !> optional scaling factor (defaults to 0)
    real(rdp), intent(in), optional :: beta

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c' and 'C'
    character, intent(in), optional :: trans

    integer :: n, m
    character :: iTrans
    real(rdp) :: iAlpha, iBeta

    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if





    m = size(a,dim=1)
    n = size(a,dim=2)

    call dgemv( iTrans, m, n, iAlpha, a, m, x, 1, iBeta, y, 1 )

  end subroutine gemv_dble



    !> Generalized matrix vector contraction Cij = Aijk * Bk
    subroutine gemv231_real(y, a, x, alpha, beta, trans)

      !> matrix
      real(rsp), intent(inout), contiguous, target :: y(:,:)

      !> matrix
      real(rsp), intent(in), contiguous, target :: a(:,:,:)

      !> vector
      real(rsp), intent(in) :: x(:)

      !> optional scaling factor (defaults to 1)
      real(rsp), intent(in), optional :: alpha

      !> optional scaling factor (defaults to 0)
      real(rsp), intent(in), optional :: beta

      !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c' and 'C'
      character, intent(in), optional :: trans

      real(rsp), pointer :: pY(:)
      real(rsp), pointer :: pA(:,:)

      pY(1 : size(y)) => y
      pA(1 : size(a, dim=1) * size(a, dim=2), 1 : size(a, dim=3)) => a
      call gemv(pY, pA, x, alpha, beta, trans)

    end subroutine gemv231_real


    !> Generalized matrix vector contraction Cij = Aijk * Bk
    subroutine gemv231_dble(y, a, x, alpha, beta, trans)

      !> matrix
      real(rdp), intent(inout), contiguous, target :: y(:,:)

      !> matrix
      real(rdp), intent(in), contiguous, target :: a(:,:,:)

      !> vector
      real(rdp), intent(in) :: x(:)

      !> optional scaling factor (defaults to 1)
      real(rdp), intent(in), optional :: alpha

      !> optional scaling factor (defaults to 0)
      real(rdp), intent(in), optional :: beta

      !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c' and 'C'
      character, intent(in), optional :: trans

      real(rdp), pointer :: pY(:)
      real(rdp), pointer :: pA(:,:)

      pY(1 : size(y)) => y
      pA(1 : size(a, dim=1) * size(a, dim=2), 1 : size(a, dim=3)) => a
      call gemv(pY, pA, x, alpha, beta, trans)

    end subroutine gemv231_dble



    !> Generalized matrix vector contraction Cij = Aijk * Bk
    subroutine gemv242_real(y, a, x, alpha, beta, trans)

      !> matrix
      real(rsp), intent(inout), contiguous, target :: y(:,:)

      !> matrix
      real(rsp), intent(in), contiguous, target :: a(:,:,:,:)

      !> matrix
      real(rsp), intent(in), contiguous, target :: x(:,:)

      !> optional scaling factor (defaults to 1)
      real(rsp), intent(in), optional :: alpha

      !> optional scaling factor (defaults to 0)
      real(rsp), intent(in), optional :: beta

      !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c' and 'C'
      character, intent(in), optional :: trans

      real(rsp), pointer :: pX(:)
      real(rsp), pointer :: pY(:)
      real(rsp), pointer :: pA(:,:)

      pY(1:size(y)) => y
      pA(1:size(a, dim=1)*size(a, dim=2), 1:size(a, dim=3)*size(a, dim=4)) => a
      pX(1:size(x)) => x
      call gemv(pY, pA, pX, alpha, beta, trans)

    end subroutine gemv242_real


    !> Generalized matrix vector contraction Cij = Aijk * Bk
    subroutine gemv242_dble(y, a, x, alpha, beta, trans)

      !> matrix
      real(rdp), intent(inout), contiguous, target :: y(:,:)

      !> matrix
      real(rdp), intent(in), contiguous, target :: a(:,:,:,:)

      !> matrix
      real(rdp), intent(in), contiguous, target :: x(:,:)

      !> optional scaling factor (defaults to 1)
      real(rdp), intent(in), optional :: alpha

      !> optional scaling factor (defaults to 0)
      real(rdp), intent(in), optional :: beta

      !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c' and 'C'
      character, intent(in), optional :: trans

      real(rdp), pointer :: pX(:)
      real(rdp), pointer :: pY(:)
      real(rdp), pointer :: pA(:,:)

      pY(1:size(y)) => y
      pA(1:size(a, dim=1)*size(a, dim=2), 1:size(a, dim=3)*size(a, dim=4)) => a
      pX(1:size(x)) => x
      call gemv(pY, pA, pX, alpha, beta, trans)

    end subroutine gemv242_dble


  !> real symmetric banded matrix*vector product
  subroutine sbmv_real(y,ba,x,uplo,alpha,beta)

    !> vector
    real(rsp), intent(inout) :: y(:)

    !> banded symmetric matrix
    real(rsp), intent(in) :: ba(:,:)

    !> vector
    real(rsp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle (defaults to lower)
    character, intent(in), optional :: uplo

    !> optional scaling factor (defaults to 1)
    real(rsp), intent(in), optional :: alpha

    !> optional scaling factor (defaults to 0)
    real(rsp), intent(in), optional :: beta

    integer :: n
    integer :: k
    character :: iUplo
    real(rsp) :: iAlpha, iBeta

    k = size(ba,dim=1) - 1

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if





    n = size(y)
    call ssbmv( iUplo, n, k, iAlpha, ba, k+1, x, 1, iBeta, y, 1 )
  end subroutine sbmv_real


  !> double precision symmetric banded matrix*vector product
  subroutine sbmv_dble(y,ba,x,uplo,alpha,beta)

    !> vector
    real(rdp), intent(inout) :: y(:)

    !> banded symmetric matrix
    real(rdp), intent(in) :: ba(:,:)

    !> vector
    real(rdp), intent(in) :: x(:)

    !> optional upper, 'U', or lower 'L' triangle (defaults to lower)
    character, intent(in), optional :: uplo

    !> optional scaling factor (defaults to 1)
    real(rdp), intent(in), optional :: alpha

    !> optional scaling factor (defaults to 0)
    real(rdp), intent(in), optional :: beta

    integer :: n
    integer :: k
    character :: iUplo
    real(rdp) :: iAlpha, iBeta

    k = size(ba,dim=1) - 1

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if





    n = size(y)
    call dsbmv( iUplo, n, k, iAlpha, ba, k+1, x, 1, iBeta, y, 1 )
  end subroutine sbmv_dble


  !> real precision symmetric matrix * general matrix multiply
  subroutine symm_real(C,side,A,B,uplo,alpha,beta,m,n)

    !> general matrix output
    real(rsp), intent(inout) :: C(:,:)

    !> symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and B is general SIDE = 'L' or
    !> 'l' C := alpha*A*B + beta*C, SIDE = 'R' or 'r' C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> symmetric matrix, size
    real(rsp), intent(in) :: A(:,:)

    !> general matrix
    real(rsp), intent(in) :: B(:,:)

    !> is an 'U'pper or 'L'ower triangle matrix, defaults to lower
    character, intent(in), optional :: uplo

    !> defaults to 1 if not set
    real(rsp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    real(rsp), intent(in), optional :: beta

    !> specifies the number of rows of the matrix C
    integer, intent(in), optional :: m

    !> specifies the number of columns of the matrix C
    integer, intent(in), optional :: n

    integer :: lda, ldb, ldc, ka, im, in
    character :: iUplo
    real(rsp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)




    if (present(n)) then
      in = n
    else
      in = size(C,dim=2)
    end if
    if (present(m)) then
      im = m
    else
      im = size(C,dim=1)
    end if
    if (iUplo=='l'.or.iUplo=='L') then
      ka = im
    else
      ka = in
    end if










    call ssymm ( side, iUplo, im, in, iAlpha, A, lda, B, ldb, iBeta, C, ldc )

  end subroutine symm_real


  !> double precision symmetric matrix * general matrix multiply
  subroutine symm_dble(C,side,A,B,uplo,alpha,beta,m,n)

    !> general matrix output
    real(rdp), intent(inout) :: C(:,:)

    !> symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and B is general SIDE = 'L' or
    !> 'l' C := alpha*A*B + beta*C, SIDE = 'R' or 'r' C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> symmetric matrix
    real(rdp), intent(in) :: A(:,:)

    !> general matrix
    real(rdp), intent(in) :: B(:,:)

    !> is an 'U'pper or 'L'ower triangle matrix, defaults to lower
    character, intent(in), optional :: uplo

    !> defaults to 1 if not set
    real(rdp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    real(rdp), intent(in), optional :: beta

    !> specifies the number of rows of the matrix C
    integer, intent(in), optional :: m

    !> specifies the number of columns of the matrix C
    integer, intent(in), optional :: n

    integer :: lda, ldb, ldc, ka, im, in
    character :: iUplo
    real(rdp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)




    if (present(n)) then
      in = n
    else
      in = size(C,dim=2)
    end if
    if (present(m)) then
      im = m
    else
      im = size(C,dim=1)
    end if
    if (iUplo=='l'.or.iUplo=='L') then
      ka = im
    else
      ka = in
    end if










    call dsymm ( side, iUplo, im, in, iAlpha, A, lda, B, ldb, iBeta, C, ldc )

  end subroutine symm_dble



  !> real matrix*matrix product
  subroutine gemm_real(C, A, B, alpha, beta, transA, transB, n, m, k, lda, ldb, ldc)

    !> general matrix output
    real(rsp), intent(inout) :: C(:,:)

    !> general matrix
    real(rsp), intent(in) :: A(:,:)

    !> general matrix
    real(rsp), intent(in) :: B(:,:)

    !> defaults to 1 if not set
    real(rsp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    real(rsp), intent(in), optional :: beta

    !> optional transpose of A matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c'
    !> and 'C'
    character, intent(in), optional :: transA

    !> optional transpose of B matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c'
    !> and 'C'
    character, intent(in), optional :: transB

    !> specifies the number of columns of the matrix C
    integer, intent(in), optional :: n

    !> specifies the number of rows of the matrix C
    integer, intent(in), optional :: m

    !> specifies the internal number of elements in Op(A)_ik Op(B)_kj
    integer, intent(in), optional :: k

    !> leading dimensions
    integer, intent(in), optional :: lda, ldb, ldc

    integer :: ilda, ildb, ildc
    integer :: in, im, ik
    character :: iTransA, iTransB
    real(rsp) :: iAlpha, iBeta

    if (present(transA)) then
      iTransA = transA
    else
      iTransA = 'n'
    end if
    if (present(transB)) then
      iTransB = transB
    else
      iTransB = 'n'
    end if




    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if

    if (present(lda)) then
      ilda = lda
    else
      ilda = size(a,dim=1)
    end if
    if (present(ldb)) then
      ildb = ldb
    else
      ildb = size(b,dim=1)
    end if
    if (present(ldc)) then
      ildc = ldc
    else
      ildc = size(c,dim=1)
    end if

    if (present(m)) then
      im = m
    else
      if (iTransA == 'n' .or. iTransA == 'N') then
        im = size(A,dim=1)
      else
        im = size(A,dim=2)
      end if
    end if
    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTransA == 'n' .or. iTransA == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if











    call sgemm(iTransA,iTransB,im,in,ik,iAlpha,A,ilda,B,ildb,iBeta,C,ildc)

  end subroutine gemm_real


  !> dble matrix*matrix product
  subroutine gemm_dble(C, A, B, alpha, beta, transA, transB, n, m, k, lda, ldb, ldc)

    !> general matrix output
    real(rdp), intent(inout) :: C(:,:)

    !> general matrix
    real(rdp), intent(in) :: A(:,:)

    !> general matrix
    real(rdp), intent(in) :: B(:,:)

    !> defaults to 1 if not set
    real(rdp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    real(rdp), intent(in), optional :: beta

    !> optional transpose of A matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c'
    !> and 'C'
    character, intent(in), optional :: transA

    !> optional transpose of B matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c'
    !> and 'C'
    character, intent(in), optional :: transB

    !> specifies the number of columns of the matrix C
    integer, intent(in), optional :: n

    !> specifies the number of rows of the matrix C
    integer, intent(in), optional :: m

    !> specifies the internal number of elements in Op(A)_ik Op(B)_kj
    integer, intent(in), optional :: k

    !> leading dimensions
    integer, intent(in), optional :: lda, ldb, ldc

    integer :: ilda, ildb, ildc
    integer :: in, im, ik
    character :: iTransA, iTransB
    real(rdp) :: iAlpha, iBeta

    if (present(transA)) then
      iTransA = transA
    else
      iTransA = 'n'
    end if
    if (present(transB)) then
      iTransB = transB
    else
      iTransB = 'n'
    end if




    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if

    if (present(lda)) then
      ilda = lda
    else
      ilda = size(a,dim=1)
    end if
    if (present(ldb)) then
      ildb = ldb
    else
      ildb = size(b,dim=1)
    end if
    if (present(ldc)) then
      ildc = ldc
    else
      ildc = size(c,dim=1)
    end if

    if (present(m)) then
      im = m
    else
      if (iTransA == 'n' .or. iTransA == 'N') then
        im = size(A,dim=1)
      else
        im = size(A,dim=2)
      end if
    end if
    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTransA == 'n' .or. iTransA == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if











    call dgemm(iTransA,iTransB,im,in,ik,iAlpha,A,ilda,B,ildb,iBeta,C,ildc)

  end subroutine gemm_dble



  !> complex matrix*matrix product
  subroutine gemm_cmplx(C,A,B,alpha,beta,transA,transB,n,m,k)

    !> general matrix output
    complex(rsp), intent(inout) :: C(:,:)

    !> general matrix
    complex(rsp), intent(in) :: A(:,:)

    !> general matrix
    complex(rsp), intent(in) :: B(:,:)

    !> defaults to 1 if not set
    complex(rsp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    complex(rsp), intent(in), optional :: beta

    !> optional transpose of A matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c'
    !> and 'C'
    character, intent(in), optional :: transA

    !> optional transpose of B matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c'
    !> and 'C'
    character, intent(in), optional :: transB

    !> specifies the number of columns of the matrix C
    integer, intent(in), optional :: n

    !> specifies the number of rows of the matrix C
    integer, intent(in), optional :: m

    !> specifies the internal number of elements in Op(A)_ik Op(B)_kj
    integer, intent(in), optional :: k

    integer :: lda, ldb, ldc
    integer :: in, im, ik
    character :: iTransA, iTransB
    complex(rsp) :: iAlpha, iBeta

    if (present(transA)) then
      iTransA = transA
    else
      iTransA = 'n'
    end if
    if (present(transB)) then
      iTransB = transB
    else
      iTransB = 'n'
    end if




    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = cmplx(1.0,0.0,rsp)
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = cmplx(0.0,0.0,rsp)
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)

    if (present(m)) then
      im = m
    else
      if (iTransA == 'n' .or. iTransA == 'N') then
        im = size(A,dim=1)
      else
        im = size(A,dim=2)
      end if
    end if
    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTransA == 'n' .or. iTransA == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if











    call cgemm(iTransA,iTransB,im,in,ik,iAlpha,A,lda,B,ldb,iBeta,C,ldc)

  end subroutine gemm_cmplx


  !> Double precision matrix*matrix product
  subroutine gemm_dblecmplx(C,A,B,alpha,beta,transA,transB,n,m,k)

    !> general matrix output
    complex(rdp), intent(inout) :: C(:,:)

    !> general matrix
    complex(rdp), intent(in) :: A(:,:)

    !> general matrix
    complex(rdp), intent(in) :: B(:,:)

    !> defaults to 1 if not set
    complex(rdp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    complex(rdp), intent(in), optional :: beta

    !> optional transpose of A matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c'
    !> and 'C'
    character, intent(in), optional :: transA

    !> optional transpose of B matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T', 'c'
    !> and 'C'
    character, intent(in), optional :: transB

    !> specifies the number of columns of the matrix C
    integer, intent(in), optional :: n

    !> specifies the number of rows of the matrix C
    integer, intent(in), optional :: m

    !> specifies the internal number of elements in Op(A)_ik Op(B)_kj
    integer, intent(in), optional :: k

    integer :: lda, ldb, ldc
    integer :: in, im, ik
    character :: iTransA, iTransB
    complex(rdp) :: iAlpha, iBeta

    if (present(transA)) then
      iTransA = transA
    else
      iTransA = 'n'
    end if
    if (present(transB)) then
      iTransB = transB
    else
      iTransB = 'n'
    end if




    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = cmplx(1.0,0.0,rdp)
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = cmplx(0.0,0.0,rdp)
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)

    if (present(m)) then
      im = m
    else
      if (iTransA == 'n' .or. iTransA == 'N') then
        im = size(A,dim=1)
      else
        im = size(A,dim=2)
      end if
    end if
    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTransA == 'n' .or. iTransA == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if











    call zgemm(iTransA,iTransB,im,in,ik,iAlpha,A,lda,B,ldb,iBeta,C,ldc)

  end subroutine gemm_dblecmplx



  !> Generalized real matrix matrix contraction (Cijl = Aijk * Bkl)
  subroutine gemm332_real(C, A, B, alpha, beta, transA, transB)

    !> general matrix output
    real(rsp), intent(inout), target, contiguous :: C(:,:,:)

    !> general matrix
    real(rsp), intent(in), target, contiguous :: A(:,:,:)

    !> general matrix
    real(rsp), intent(in) :: B(:,:)

    !> defaults to 1 if not set
    real(rsp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    real(rsp), intent(in), optional :: beta

    !> optional transpose of A matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T',
    !> 'c' and 'C'. Note this acts on the compound index ij
    character, intent(in), optional :: transA

    !> optional transpose of B matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T',
    !> 'c' and 'C'
    character, intent(in), optional :: transB

    real(rsp), pointer :: pA(:,:), pC(:,:)

    pA(1 : size(A, dim=1) * size(A, dim=2), 1 : size(A, dim=3)) => A
    pC(1 : size(C, dim=1) * size(C, dim=2), 1 : size(C, dim=3)) => C
    call gemm(pC, pA, B, alpha, beta, transA, transB)

  end subroutine gemm332_real


  !> Generalized real matrix matrix contraction (Cijl = Aijk * Bkl)
  subroutine gemm332_dble(C, A, B, alpha, beta, transA, transB)

    !> general matrix output
    real(rdp), intent(inout), target, contiguous :: C(:,:,:)

    !> general matrix
    real(rdp), intent(in), target, contiguous :: A(:,:,:)

    !> general matrix
    real(rdp), intent(in) :: B(:,:)

    !> defaults to 1 if not set
    real(rdp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    real(rdp), intent(in), optional :: beta

    !> optional transpose of A matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T',
    !> 'c' and 'C'. Note this acts on the compound index ij
    character, intent(in), optional :: transA

    !> optional transpose of B matrix (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T',
    !> 'c' and 'C'
    character, intent(in), optional :: transB

    real(rdp), pointer :: pA(:,:), pC(:,:)

    pA(1 : size(A, dim=1) * size(A, dim=2), 1 : size(A, dim=3)) => A
    pC(1 : size(C, dim=1) * size(C, dim=2), 1 : size(C, dim=3)) => C
    call gemm(pC, pA, B, alpha, beta, transA, transB)

  end subroutine gemm332_dble



  !> Rank-k update
  subroutine herk_real(C,A,alpha,beta,uplo,trans,n,k)

    !> contains the matrix to be updated
    real(rsp), intent(inout) :: C(:,:)

    !> contains the matrix to update
    real(rsp), intent(in) :: A(:,:)

    !> scaling value for the update contribution, defaults to 1
    real(rsp), intent(in), optional :: alpha

    !> scaling value for the original C, defaults to 0
    real(rsp), intent(in), optional :: beta

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T' (and 'C' or 'c'
    !> for the real cases)
    character, intent(in), optional :: trans

    !> order of the matrix C
    integer, intent(in), optional :: n

    !> internal order of A summation
    integer, intent(in), optional :: k

    integer :: lda, ldc
    integer :: in, ik
    character :: iTrans, iUplo
    real(rsp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if


    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if



    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if

    lda = size(a,dim=1)
    ldc = size(c,dim=1)

    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTrans == 'n' .or. iTrans == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if







    call ssyrk(iUplo, iTrans, in, ik, iAlpha, A, lda, iBeta, C, ldc )

  end subroutine herk_real

  !> Rank-k update
  subroutine herk_cmplx(C,A,alpha,beta,uplo,trans,n,k)

    !> contains the matrix to be updated
    complex(rsp), intent(inout) :: C(:,:)

    !> contains the matrix to update
    complex(rsp), intent(in) :: A(:,:)

    !> scaling value for the update contribution, defaults to 1
    real(rsp), intent(in), optional :: alpha

    !> scaling value for the original C, defaults to 0
    real(rsp), intent(in), optional :: beta

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T' (and 'C' or 'c'
    !> for the real cases)
    character, intent(in), optional :: trans

    !> order of the matrix C
    integer, intent(in), optional :: n

    !> internal order of A summation
    integer, intent(in), optional :: k

    integer :: lda, ldc
    integer :: in, ik
    character :: iTrans, iUplo
    real(rsp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if


    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if



    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if

    lda = size(a,dim=1)
    ldc = size(c,dim=1)

    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTrans == 'n' .or. iTrans == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if







    call cherk(iUplo, iTrans, in, ik, iAlpha, A, lda, iBeta, C, ldc )

  end subroutine herk_cmplx

  !> Rank-k update
  subroutine herk_dble(C,A,alpha,beta,uplo,trans,n,k)

    !> contains the matrix to be updated
    real(rdp), intent(inout) :: C(:,:)

    !> contains the matrix to update
    real(rdp), intent(in) :: A(:,:)

    !> scaling value for the update contribution, defaults to 1
    real(rdp), intent(in), optional :: alpha

    !> scaling value for the original C, defaults to 0
    real(rdp), intent(in), optional :: beta

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T' (and 'C' or 'c'
    !> for the real cases)
    character, intent(in), optional :: trans

    !> order of the matrix C
    integer, intent(in), optional :: n

    !> internal order of A summation
    integer, intent(in), optional :: k

    integer :: lda, ldc
    integer :: in, ik
    character :: iTrans, iUplo
    real(rdp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if


    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if



    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if

    lda = size(a,dim=1)
    ldc = size(c,dim=1)

    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTrans == 'n' .or. iTrans == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if







    call dsyrk(iUplo, iTrans, in, ik, iAlpha, A, lda, iBeta, C, ldc )

  end subroutine herk_dble

  !> Rank-k update
  subroutine herk_dblecmplx(C,A,alpha,beta,uplo,trans,n,k)

    !> contains the matrix to be updated
    complex(rdp), intent(inout) :: C(:,:)

    !> contains the matrix to update
    complex(rdp), intent(in) :: A(:,:)

    !> scaling value for the update contribution, defaults to 1
    real(rdp), intent(in), optional :: alpha

    !> scaling value for the original C, defaults to 0
    real(rdp), intent(in), optional :: beta

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T' (and 'C' or 'c'
    !> for the real cases)
    character, intent(in), optional :: trans

    !> order of the matrix C
    integer, intent(in), optional :: n

    !> internal order of A summation
    integer, intent(in), optional :: k

    integer :: lda, ldc
    integer :: in, ik
    character :: iTrans, iUplo
    real(rdp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if


    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if



    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if

    lda = size(a,dim=1)
    ldc = size(c,dim=1)

    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTrans == 'n' .or. iTrans == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if







    call zherk(iUplo, iTrans, in, ik, iAlpha, A, lda, iBeta, C, ldc )

  end subroutine herk_dblecmplx



  !> Rank-k update
  subroutine her2k_real(C,A,B,alpha,beta,uplo,trans,n,k)

    !> contains the matrix to be updated
    real(rsp), intent(inout) :: C(:,:)

    !> contains the first matrix to update with
    real(rsp), intent(in) :: A(:,:)

    !> contains the second matrix to update with
    real(rsp), intent(in) :: B(:,:)

    !> scaling value for the update contribution, defaults to 1
    real(rsp), intent(in), optional :: alpha

    !> scaling value for the original C, defaults to 0
    real(rsp), intent(in), optional :: beta

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T' (and 'C' or 'c'
    !> for the real cases)
    character, intent(in), optional :: trans

    !> order of the matrix C
    integer, intent(in), optional :: n

    !> internal order of A summation
    integer, intent(in), optional :: k

    integer :: lda, ldb, ldc
    integer :: in, ik
    character :: iTrans, iUplo
    real(rsp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if


    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if



    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)

    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTrans == 'n' .or. iTrans == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if








    call ssyr2k(iUplo, iTrans, in, ik, iAlpha, A, lda, B, ldb, iBeta, C, ldc )

  end subroutine her2k_real
  !> Rank-k update
  subroutine her2k_cmplx(C,A,B,alpha,beta,uplo,trans,n,k)

    !> contains the matrix to be updated
    complex(rsp), intent(inout) :: C(:,:)

    !> contains the first matrix to update with
    complex(rsp), intent(in) :: A(:,:)

    !> contains the second matrix to update with
    complex(rsp), intent(in) :: B(:,:)

    !> scaling value for the update contribution, defaults to 1
    complex(rsp), intent(in), optional :: alpha

    !> scaling value for the original C, defaults to 0
    complex(rsp), intent(in), optional :: beta

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T' (and 'C' or 'c'
    !> for the real cases)
    character, intent(in), optional :: trans

    !> order of the matrix C
    integer, intent(in), optional :: n

    !> internal order of A summation
    integer, intent(in), optional :: k

    integer :: lda, ldb, ldc
    integer :: in, ik
    character :: iTrans, iUplo
    complex(rsp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if


    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if



    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rsp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rsp
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)

    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTrans == 'n' .or. iTrans == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if








    call cher2k(iUplo, iTrans, in, ik, iAlpha, A, lda, B, ldb, iBeta, C, ldc )

  end subroutine her2k_cmplx
  !> Rank-k update
  subroutine her2k_dble(C,A,B,alpha,beta,uplo,trans,n,k)

    !> contains the matrix to be updated
    real(rdp), intent(inout) :: C(:,:)

    !> contains the first matrix to update with
    real(rdp), intent(in) :: A(:,:)

    !> contains the second matrix to update with
    real(rdp), intent(in) :: B(:,:)

    !> scaling value for the update contribution, defaults to 1
    real(rdp), intent(in), optional :: alpha

    !> scaling value for the original C, defaults to 0
    real(rdp), intent(in), optional :: beta

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T' (and 'C' or 'c'
    !> for the real cases)
    character, intent(in), optional :: trans

    !> order of the matrix C
    integer, intent(in), optional :: n

    !> internal order of A summation
    integer, intent(in), optional :: k

    integer :: lda, ldb, ldc
    integer :: in, ik
    character :: iTrans, iUplo
    real(rdp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if


    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if



    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)

    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTrans == 'n' .or. iTrans == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if








    call dsyr2k(iUplo, iTrans, in, ik, iAlpha, A, lda, B, ldb, iBeta, C, ldc )

  end subroutine her2k_dble
  !> Rank-k update
  subroutine her2k_dblecmplx(C,A,B,alpha,beta,uplo,trans,n,k)

    !> contains the matrix to be updated
    complex(rdp), intent(inout) :: C(:,:)

    !> contains the first matrix to update with
    complex(rdp), intent(in) :: A(:,:)

    !> contains the second matrix to update with
    complex(rdp), intent(in) :: B(:,:)

    !> scaling value for the update contribution, defaults to 1
    complex(rdp), intent(in), optional :: alpha

    !> scaling value for the original C, defaults to 0
    complex(rdp), intent(in), optional :: beta

    !> optional upper, 'U', or lower 'L' triangle, defaults to lower
    character, intent(in), optional :: uplo

    !> optional transpose (defaults to 'n'), allowed choices are 'n', 'N', 't', 'T' (and 'C' or 'c'
    !> for the real cases)
    character, intent(in), optional :: trans

    !> order of the matrix C
    integer, intent(in), optional :: n

    !> internal order of A summation
    integer, intent(in), optional :: k

    integer :: lda, ldb, ldc
    integer :: in, ik
    character :: iTrans, iUplo
    complex(rdp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if


    if (present(trans)) then
      iTrans = trans
    else
      iTrans = 'n'
    end if



    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = 1.0_rdp
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = 0.0_rdp
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)

    if (present(n)) then
      in = n
    else
      in = size(c,dim=2)
    end if
    if (present(k)) then
      ik = k
    else
      if (iTrans == 'n' .or. iTrans == 'N') then
        ik = size(A,dim=2)
      else
        ik = size(A,dim=1)
      end if
    end if








    call zher2k(iUplo, iTrans, in, ik, iAlpha, A, lda, B, ldb, iBeta, C, ldc )

  end subroutine her2k_dblecmplx


  !> single precision hermitian matrix * general matrix multiply
  subroutine hemm_cmplx(C, side, A, B, uplo, alpha, beta, m, n)

    !> general matrix output
    complex(rsp), intent(inout) :: C(:,:)

    !> symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and B is general SIDE = 'L' or
    !> 'l' C := alpha*A*B + beta*C, SIDE = 'R' or 'r' C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> hermitian matrix
    complex(rsp), intent(in) :: A(:,:)

    !> general matrix
    complex(rsp), intent(in) :: B(:,:)

    !> A is an 'U'pper or 'L'ower triangle matrix, defaults to lower
    character, intent(in), optional :: uplo

    !> defaults to 1 if not set
    complex(rsp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    complex(rsp), intent(in), optional :: beta

    !> specifies the number of rows of the matrix C
    integer, intent(in), optional :: m

    !> specifies the number of columns of the matrix C
    integer, intent(in), optional :: n

    integer :: lda, ldb, ldc, ka, im, in
    character :: iUplo
    complex(rsp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = (1.0_rsp, 0.0_rsp)
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = (0.0_rsp, 0.0_rsp)
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)




    if (present(n)) then
      in = n
    else
      in = size(C,dim=2)
    end if
    if (present(m)) then
      im = m
    else
      im = size(C,dim=1)
    end if
    if (iUplo=='l'.or.iUplo=='L') then
      ka = im
    else
      ka = in
    end if










    call chemm(side, iUplo, im, in, iAlpha, A, lda, B, ldb, iBeta, C, ldc)

  end subroutine hemm_cmplx


  !> double precision hermitian matrix * general matrix multiply
  subroutine hemm_dblecmplx(C, side, A, B, uplo, alpha, beta, m, n)

    !> general matrix output
    complex(rdp), intent(inout) :: C(:,:)

    !> symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and B is gen
    !> 'l' C := alpha*A*B + beta*C, SIDE = 'R' or 'r' C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> hermitian matrix
    complex(rdp), intent(in) :: A(:,:)

    !> general matrix
    complex(rdp), intent(in) :: B(:,:)

    !> A is an 'U'pper or 'L'ower triangle matrix, defaults to lower
    character, intent(in), optional :: uplo

    !> defaults to 1 if not set
    complex(rdp), intent(in), optional :: alpha

    !> defaults to 0 if not set
    complex(rdp), intent(in), optional :: beta

    !> specifies the number of rows of the matrix C
    integer, intent(in), optional :: m

    !> specifies the number of columns of the matrix C
    integer, intent(in), optional :: n

    integer :: lda, ldb, ldc, ka, im, in
    character :: iUplo
    complex(rdp) :: iAlpha, iBeta

    if (present(uplo)) then
      iUplo = uplo
    else
      iUplo = 'L'
    end if
    if (present(alpha)) then
      iAlpha = alpha
    else
      iAlpha = (1.0_rdp, 0.0_rdp)
    end if
    if (present(beta)) then
      iBeta = beta
    else
      iBeta = (0.0_rdp, 0.0_rdp)
    end if

    lda = size(a,dim=1)
    ldb = size(b,dim=1)
    ldc = size(c,dim=1)




    if (present(n)) then
      in = n
    else
      in = size(C,dim=2)
    end if
    if (present(m)) then
      im = m
    else
      im = size(C,dim=1)
    end if
    if (iUplo=='l'.or.iUplo=='L') then
      ka = im
    else
      ka = in
    end if










    call zhemm(side, iUplo, im, in, iAlpha, A, lda, B, ldb, iBeta, C, ldc)

  end subroutine hemm_dblecmplx



  !> single precision complex matrix scaling
  subroutine scal_cmplx(a,alpha)

    !> contains the matrix to be scaled
    complex(rsp), intent(inout) :: a(:,:)

    !> scaling factor
    complex(rsp), intent(in) :: alpha

    call cscal(product(shape(a)), alpha, a, 1)

  end subroutine scal_cmplx


  !> double precision complex matrix scaling
  subroutine scal_dblecmplx(a,alpha)

    !> contains the matrix to be scaled
    complex(rdp), intent(inout) :: a(:,:)

    !> scaling factor
    complex(rdp), intent(in) :: alpha

    call zscal(product(shape(a)), alpha, a, 1)

  end subroutine scal_dblecmplx




  !> single precision complex matrix swapping
  subroutine swap_cmplx(a,b)

    !> matrices to be swapped
    complex(rsp), intent(inout) :: a(:,:), b(:,:)




    call cswap(product(shape(a)), a, 1, b, 1)

  end subroutine swap_cmplx


  !> double precision complex matrix swapping
  subroutine swap_dblecmplx(a,b)

    !> matrices to be swapped
    complex(rdp), intent(inout) :: a(:,:), b(:,:)




    call zswap(product(shape(a)), a, 1, b, 1)

  end subroutine swap_dblecmplx




  !> Performs the matrix-vector operation
  !>
  !>    y := alpha*A*x + beta*y,
  !>
  !> where alpha and beta are scalars, x and y are n element vectors and
  !> A is an n by n symmetric matrix, supplied in packed form.
  !>
  pure subroutine spmv_real(amat, xvec, yvec, uplo, alpha, beta)

    !> Matrix A
    real(rsp), intent(in) :: amat(:)

    !> Vector x
    real(rsp), intent(in) :: xvec(:)

    !> Vector y
    real(rsp), intent(inout) :: yvec(:)

    !> Whether upper or lower matrix was provided ('u' or 'l', default 'l')
    character(len=1), intent(in), optional :: uplo

    !> Prefactor alpha (default 1.0)
    real(rsp), intent(in), optional :: alpha

    !> Prefactor beta (default 0.0)
    real(rsp), intent(in), optional :: beta

    character(len=1) :: ula
    real(rsp) :: a, b
    integer :: incx, incy, n

    if (present(alpha)) then
      a = alpha
    else
      a = 1.0_dp
    end if
    if (present(beta)) then
      b = beta
    else
      b = 0
    end if
    if (present(uplo)) then
      ula = uplo
    else
      ula = 'u'
    end if
    incx = 1
    incy = 1
    n = size(xvec)
    call sspmv(ula, n, a, amat, xvec, incx, b, yvec, incy)

  end subroutine spmv_real


  !> Performs the matrix-vector operation
  !>
  !>    y := alpha*A*x + beta*y,
  !>
  !> where alpha and beta are scalars, x and y are n element vectors and
  !> A is an n by n symmetric matrix, supplied in packed form.
  !>
  pure subroutine spmv_dble(amat, xvec, yvec, uplo, alpha, beta)

    !> Matrix A
    real(rdp), intent(in) :: amat(:)

    !> Vector x
    real(rdp), intent(in) :: xvec(:)

    !> Vector y
    real(rdp), intent(inout) :: yvec(:)

    !> Whether upper or lower matrix was provided ('u' or 'l', default 'l')
    character(len=1), intent(in), optional :: uplo

    !> Prefactor alpha (default 1.0)
    real(rdp), intent(in), optional :: alpha

    !> Prefactor beta (default 0.0)
    real(rdp), intent(in), optional :: beta

    character(len=1) :: ula
    real(rdp) :: a, b
    integer :: incx, incy, n

    if (present(alpha)) then
      a = alpha
    else
      a = 1.0_dp
    end if
    if (present(beta)) then
      b = beta
    else
      b = 0
    end if
    if (present(uplo)) then
      ula = uplo
    else
      ula = 'u'
    end if
    incx = 1
    incy = 1
    n = size(xvec)
    call dspmv(ula, n, a, amat, xvec, incx, b, yvec, incy)

  end subroutine spmv_dble



end module dftbp_math_blasroutines
