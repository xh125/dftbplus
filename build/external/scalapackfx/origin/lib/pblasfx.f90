








































!> High level Fortran wrappers for the PBLAS library.
module pblasfx_module
  use scalapackfx_common_module
  use pblas_module
  implicit none
  private

  public :: pblasfx_psyr, pblasfx_pher
  public :: pblasfx_psyrk, pblasfx_pherk
  public :: pblasfx_psyr2k, pblasfx_pher2k
  public :: pblasfx_psymv, pblasfx_phemv
  public :: pblasfx_psymm, pblasfx_phemm
  public :: pblasfx_pgemm
  public :: pblasfx_ptrmm
  public :: pblasfx_ptran, pblasfx_ptranu
  public :: pblasfx_ptranc

  interface pblasfx_psyr
    module procedure pblasfx_psyr_real, pblasfx_psyr_dreal
  end interface pblasfx_psyr

  interface pblasfx_pher
    module procedure pblasfx_pher_complex, pblasfx_pher_dcomplex
  end interface pblasfx_pher

  interface pblasfx_psyrk
    module procedure pblasfx_psyrk_real, pblasfx_psyrk_dreal
  end interface pblasfx_psyrk

  interface pblasfx_pherk
    module procedure pblasfx_pherk_complex, pblasfx_pherk_dcomplex
  end interface pblasfx_pherk

  interface pblasfx_psyr2k
    module procedure pblasfx_psyr2k_real, pblasfx_psyr2k_dreal
  end interface pblasfx_psyr2k

  interface pblasfx_pher2k
    module procedure pblasfx_pher2k_complex, pblasfx_pher2k_dcomplex
  end interface pblasfx_pher2k

  interface pblasfx_psymv
    module procedure pblasfx_psymv_real, pblasfx_psymv_dreal
  end interface pblasfx_psymv

  interface pblasfx_phemv
    module procedure pblasfx_phemv_complex, pblasfx_phemv_dcomplex
  end interface pblasfx_phemv

  interface pblasfx_psymm
    module procedure pblasfx_psymm_real, pblasfx_psymm_dreal
  end interface pblasfx_psymm

  interface pblasfx_phemm
    module procedure pblasfx_phemm_complex, pblasfx_phemm_dcomplex
  end interface pblasfx_phemm

  interface pblasfx_ptrmm
    module procedure pblasfx_ptrmm_real, pblasfx_ptrmm_dreal, &
        & pblasfx_ptrmm_complex, pblasfx_ptrmm_dcomplex
  end interface pblasfx_ptrmm

  interface pblasfx_pgemm
    module procedure pblasfx_pgemm_real, pblasfx_pgemm_dreal, &
        & pblasfx_pgemm_complex, pblasfx_pgemm_dcomplex
  end interface pblasfx_pgemm

  interface pblasfx_ptran
    module procedure pblasfx_ptran_real, pblasfx_ptran_dreal
  end interface pblasfx_ptran

  interface pblasfx_ptranu
    module procedure pblasfx_ptranu_complex, pblasfx_ptranu_dcomplex
  end interface pblasfx_ptranu

  interface pblasfx_ptranc
    module procedure pblasfx_ptranc_complex, pblasfx_ptranc_dcomplex
  end interface pblasfx_ptranc

contains


  subroutine pblasfx_psyr_real(xx, descx, aa, desca, uplo, nn, alpha, ix, jx, incx,&
      & ia, ja)
    real(sp), intent(in) :: xx(:,:)
    integer, intent(in) :: descx(DLEN_)
    real(sp), intent(inout) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    character, intent(in), optional :: uplo
    integer, intent(in), optional :: nn
    real(sp), intent(in), optional :: alpha
    integer, intent(in), optional :: ix, jx, incx, ia, ja

    real(sp) :: alpha0
    character :: uplo0
    integer :: nn0, ix0, jx0, incx0, ia0, ja0

  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(ix)) then
    ix0 = ix
  else
    ix0 = 1
  end if
  if (present(jx)) then
    jx0 = jx
  else
    jx0 = 1
  end if
  if (present(incx)) then
    incx0 = incx
  else
    incx0 = 1
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
    call psyr(uplo0, nn0, alpha0, xx, ix0, jx0, descx, incx0, aa, ia0, ja0, desca)

  end subroutine pblasfx_psyr_real


  subroutine pblasfx_psyr_dreal(xx, descx, aa, desca, uplo, nn, alpha, ix, jx, incx,&
      & ia, ja)
    real(dp), intent(in) :: xx(:,:)
    integer, intent(in) :: descx(DLEN_)
    real(dp), intent(inout) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    character, intent(in), optional :: uplo
    integer, intent(in), optional :: nn
    real(dp), intent(in), optional :: alpha
    integer, intent(in), optional :: ix, jx, incx, ia, ja

    real(dp) :: alpha0
    character :: uplo0
    integer :: nn0, ix0, jx0, incx0, ia0, ja0

  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(ix)) then
    ix0 = ix
  else
    ix0 = 1
  end if
  if (present(jx)) then
    jx0 = jx
  else
    jx0 = 1
  end if
  if (present(incx)) then
    incx0 = incx
  else
    incx0 = 1
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
    call psyr(uplo0, nn0, alpha0, xx, ix0, jx0, descx, incx0, aa, ia0, ja0, desca)

  end subroutine pblasfx_psyr_dreal


  subroutine pblasfx_pher_complex(xx, descx, aa, desca, uplo, nn, alpha, ix, jx, incx,&
      & ia, ja)
    complex(sp), intent(in) :: xx(:,:)
    integer, intent(in) :: descx(DLEN_)
    complex(sp), intent(inout) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    character, intent(in), optional :: uplo
    integer, intent(in), optional :: nn
    real(sp), intent(in), optional :: alpha
    integer, intent(in), optional :: ix, jx, incx, ia, ja

    real(sp) :: alpha0
    character :: uplo0
    integer :: nn0, ix0, jx0, incx0, ia0, ja0

  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(ix)) then
    ix0 = ix
  else
    ix0 = 1
  end if
  if (present(jx)) then
    jx0 = jx
  else
    jx0 = 1
  end if
  if (present(incx)) then
    incx0 = incx
  else
    incx0 = 1
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
    call pher(uplo0, nn0, alpha0, xx, ix0, jx0, descx, incx0, aa, ia0, ja0, desca)

  end subroutine pblasfx_pher_complex


  subroutine pblasfx_pher_dcomplex(xx, descx, aa, desca, uplo, nn, alpha, ix, jx, incx,&
      & ia, ja)
    complex(dp), intent(in) :: xx(:,:)
    integer, intent(in) :: descx(DLEN_)
    complex(dp), intent(inout) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    character, intent(in), optional :: uplo
    integer, intent(in), optional :: nn
    real(dp), intent(in), optional :: alpha
    integer, intent(in), optional :: ix, jx, incx, ia, ja

    real(dp) :: alpha0
    character :: uplo0
    integer :: nn0, ix0, jx0, incx0, ia0, ja0

  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(ix)) then
    ix0 = ix
  else
    ix0 = 1
  end if
  if (present(jx)) then
    jx0 = jx
  else
    jx0 = 1
  end if
  if (present(incx)) then
    incx0 = incx
  else
    incx0 = 1
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
    call pher(uplo0, nn0, alpha0, xx, ix0, jx0, descx, incx0, aa, ia0, ja0, desca)

  end subroutine pblasfx_pher_dcomplex



  !> Symmetric/Hermitian rank-k update.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param trans  "N" for normal, "T" for transposed aa (default: "N").
  !! \param alpha  Prefactor.
  subroutine pblasfx_psyrk_real(aa, desca, cc, descc, uplo, trans, alpha, beta,&
      & nn, kk, ia, ja, ic, jc)
    real(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: uplo, trans
    real(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, kk
    integer, intent(in), optional :: ia, ja, ic, jc

    real(sp) :: alpha0, beta0
    character :: uplo0, trans0
    integer :: nn0, kk0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=sp)
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(trans)) then
    trans0 = trans
  else
    trans0 = "N"
  end if
    if (trans0 == "N") then
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
    else
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(M_)
  end if
    end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call psyrk(uplo0, trans0, nn0, kk0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_psyrk_real


  !> Symmetric/Hermitian rank-k update.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param trans  "N" for normal, "T" for transposed aa (default: "N").
  !! \param alpha  Prefactor.
  subroutine pblasfx_psyrk_dreal(aa, desca, cc, descc, uplo, trans, alpha, beta,&
      & nn, kk, ia, ja, ic, jc)
    real(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: uplo, trans
    real(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, kk
    integer, intent(in), optional :: ia, ja, ic, jc

    real(dp) :: alpha0, beta0
    character :: uplo0, trans0
    integer :: nn0, kk0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=dp)
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(trans)) then
    trans0 = trans
  else
    trans0 = "N"
  end if
    if (trans0 == "N") then
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
    else
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(M_)
  end if
    end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call psyrk(uplo0, trans0, nn0, kk0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_psyrk_dreal


  !> Symmetric/Hermitian rank-k update.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param trans  "N" for normal, "T" for transposed aa (default: "N").
  !! \param alpha  Prefactor.
  subroutine pblasfx_pherk_complex(aa, desca, cc, descc, uplo, trans, alpha, beta,&
      & nn, kk, ia, ja, ic, jc)
    complex(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: uplo, trans
    real(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, kk
    integer, intent(in), optional :: ia, ja, ic, jc

    real(sp) :: alpha0, beta0
    character :: uplo0, trans0
    integer :: nn0, kk0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=sp)
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(trans)) then
    trans0 = trans
  else
    trans0 = "N"
  end if
    if (trans0 == "N") then
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
    else
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(M_)
  end if
    end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call pherk(uplo0, trans0, nn0, kk0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_pherk_complex


  !> Symmetric/Hermitian rank-k update.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param trans  "N" for normal, "T" for transposed aa (default: "N").
  !! \param alpha  Prefactor.
  subroutine pblasfx_pherk_dcomplex(aa, desca, cc, descc, uplo, trans, alpha, beta,&
      & nn, kk, ia, ja, ic, jc)
    complex(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: uplo, trans
    real(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, kk
    integer, intent(in), optional :: ia, ja, ic, jc

    real(dp) :: alpha0, beta0
    character :: uplo0, trans0
    integer :: nn0, kk0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=dp)
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(trans)) then
    trans0 = trans
  else
    trans0 = "N"
  end if
    if (trans0 == "N") then
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
    else
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(M_)
  end if
    end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call pherk(uplo0, trans0, nn0, kk0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_pherk_dcomplex



  !> Symmetric/Hermitian rank-2k update.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param bb  Matrix to update with.
  !! \param descb  Descriptor of bb.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param trans  "N" for normal, "T" for transposed aa (default: "N").
  !! \param alpha  Prefactor.
  subroutine pblasfx_psyr2k_real(aa, desca, bb, descb, cc, descc, uplo, trans, alpha, beta,&
      & nn, kk, ia, ja, ib, jb, ic, jc)
    real(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(sp), intent(in) :: bb(:,:)
    integer, intent(in) :: descb(DLEN_)
    real(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: uplo, trans
    real(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, kk
    integer, intent(in), optional :: ia, ja, ib, jb, ic, jc

    real(sp) :: alpha0, beta0
    character :: uplo0, trans0
    integer :: nn0, kk0, ia0, ja0, ib0, jb0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=sp)
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(trans)) then
    trans0 = trans
  else
    trans0 = "N"
  end if
    if (trans0 == "N") then
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
    else
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(M_)
  end if
    end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call psyr2k(uplo0, trans0, nn0, kk0, alpha0, aa, ia0, ja0, desca, bb, ib0, jb0, descb, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_psyr2k_real


  !> Symmetric/Hermitian rank-2k update.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param bb  Matrix to update with.
  !! \param descb  Descriptor of bb.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param trans  "N" for normal, "T" for transposed aa (default: "N").
  !! \param alpha  Prefactor.
  subroutine pblasfx_psyr2k_dreal(aa, desca, bb, descb, cc, descc, uplo, trans, alpha, beta,&
      & nn, kk, ia, ja, ib, jb, ic, jc)
    real(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(dp), intent(in) :: bb(:,:)
    integer, intent(in) :: descb(DLEN_)
    real(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: uplo, trans
    real(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, kk
    integer, intent(in), optional :: ia, ja, ib, jb, ic, jc

    real(dp) :: alpha0, beta0
    character :: uplo0, trans0
    integer :: nn0, kk0, ia0, ja0, ib0, jb0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=dp)
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(trans)) then
    trans0 = trans
  else
    trans0 = "N"
  end if
    if (trans0 == "N") then
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
    else
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(M_)
  end if
    end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call psyr2k(uplo0, trans0, nn0, kk0, alpha0, aa, ia0, ja0, desca, bb, ib0, jb0, descb, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_psyr2k_dreal


  !> Symmetric/Hermitian rank-2k update.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param bb  Matrix to update with.
  !! \param descb  Descriptor of bb.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param trans  "N" for normal, "T" for transposed aa (default: "N").
  !! \param alpha  Prefactor.
  subroutine pblasfx_pher2k_complex(aa, desca, bb, descb, cc, descc, uplo, trans, alpha, beta,&
      & nn, kk, ia, ja, ib, jb, ic, jc)
    complex(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(sp), intent(in) :: bb(:,:)
    integer, intent(in) :: descb(DLEN_)
    complex(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: uplo, trans
    real(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, kk
    integer, intent(in), optional :: ia, ja, ib, jb, ic, jc

    real(sp) :: alpha0, beta0
    character :: uplo0, trans0
    integer :: nn0, kk0, ia0, ja0, ib0, jb0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=sp)
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(trans)) then
    trans0 = trans
  else
    trans0 = "N"
  end if
    if (trans0 == "N") then
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
    else
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(M_)
  end if
    end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call pher2k(uplo0, trans0, nn0, kk0, alpha0, aa, ia0, ja0, desca, bb, ib0, jb0, descb, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_pher2k_complex


  !> Symmetric/Hermitian rank-2k update.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param bb  Matrix to update with.
  !! \param descb  Descriptor of bb.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param trans  "N" for normal, "T" for transposed aa (default: "N").
  !! \param alpha  Prefactor.
  subroutine pblasfx_pher2k_dcomplex(aa, desca, bb, descb, cc, descc, uplo, trans, alpha, beta,&
      & nn, kk, ia, ja, ib, jb, ic, jc)
    complex(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(dp), intent(in) :: bb(:,:)
    integer, intent(in) :: descb(DLEN_)
    complex(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: uplo, trans
    real(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, kk
    integer, intent(in), optional :: ia, ja, ib, jb, ic, jc

    real(dp) :: alpha0, beta0
    character :: uplo0, trans0
    integer :: nn0, kk0, ia0, ja0, ib0, jb0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=dp)
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(trans)) then
    trans0 = trans
  else
    trans0 = "N"
  end if
    if (trans0 == "N") then
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
    else
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(M_)
  end if
    end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call pher2k(uplo0, trans0, nn0, kk0, alpha0, aa, ia0, ja0, desca, bb, ib0, jb0, descb, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_pher2k_dcomplex



  subroutine pblasfx_psymv_real(aa, desca, xx, descx, yy, descy, uplo, alpha, beta, &
      & nn, ia, ja, ix, jx, incx, iy, jy, incy)
    real(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(sp), intent(in) :: xx(:,:)
    integer, intent(in) :: descx(DLEN_)
    real(sp), intent(inout) :: yy(:,:)
    integer, intent(in) :: descy(DLEN_)
    character, intent(in), optional :: uplo
    real(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, ia, ja, ix, jx, incx, iy, jy, incy

    real(sp) :: alpha0, beta0
    character :: uplo0
    integer :: nn0, ia0, ja0, ix0, jx0, incx0, iy0, jy0, incy0

  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=sp)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ix)) then
    ix0 = ix
  else
    ix0 = 1
  end if
  if (present(jx)) then
    jx0 = jx
  else
    jx0 = 1
  end if
  if (present(incx)) then
    incx0 = incx
  else
    incx0 = 1
  end if
  if (present(iy)) then
    iy0 = iy
  else
    iy0 = 1
  end if
  if (present(jy)) then
    jy0 = jy
  else
    jy0 = 1
  end if
  if (present(incy)) then
    incy0 = incy
  else
    incy0 = 1
  end if
    call psymv(uplo0, nn0, alpha0, aa, ia0, ja0, desca, xx, ix0, jx0, descx, &
        & incx0, beta0, yy, iy0, jy0, descy, incy0)

  end subroutine pblasfx_psymv_real


  subroutine pblasfx_psymv_dreal(aa, desca, xx, descx, yy, descy, uplo, alpha, beta, &
      & nn, ia, ja, ix, jx, incx, iy, jy, incy)
    real(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(dp), intent(in) :: xx(:,:)
    integer, intent(in) :: descx(DLEN_)
    real(dp), intent(inout) :: yy(:,:)
    integer, intent(in) :: descy(DLEN_)
    character, intent(in), optional :: uplo
    real(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, ia, ja, ix, jx, incx, iy, jy, incy

    real(dp) :: alpha0, beta0
    character :: uplo0
    integer :: nn0, ia0, ja0, ix0, jx0, incx0, iy0, jy0, incy0

  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=dp)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ix)) then
    ix0 = ix
  else
    ix0 = 1
  end if
  if (present(jx)) then
    jx0 = jx
  else
    jx0 = 1
  end if
  if (present(incx)) then
    incx0 = incx
  else
    incx0 = 1
  end if
  if (present(iy)) then
    iy0 = iy
  else
    iy0 = 1
  end if
  if (present(jy)) then
    jy0 = jy
  else
    jy0 = 1
  end if
  if (present(incy)) then
    incy0 = incy
  else
    incy0 = 1
  end if
    call psymv(uplo0, nn0, alpha0, aa, ia0, ja0, desca, xx, ix0, jx0, descx, &
        & incx0, beta0, yy, iy0, jy0, descy, incy0)

  end subroutine pblasfx_psymv_dreal


  subroutine pblasfx_phemv_complex(aa, desca, xx, descx, yy, descy, uplo, alpha, beta, &
      & nn, ia, ja, ix, jx, incx, iy, jy, incy)
    complex(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(sp), intent(in) :: xx(:,:)
    integer, intent(in) :: descx(DLEN_)
    complex(sp), intent(inout) :: yy(:,:)
    integer, intent(in) :: descy(DLEN_)
    character, intent(in), optional :: uplo
    complex(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, ia, ja, ix, jx, incx, iy, jy, incy

    complex(sp) :: alpha0, beta0
    character :: uplo0
    integer :: nn0, ia0, ja0, ix0, jx0, incx0, iy0, jy0, incy0

  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, kind=sp)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ix)) then
    ix0 = ix
  else
    ix0 = 1
  end if
  if (present(jx)) then
    jx0 = jx
  else
    jx0 = 1
  end if
  if (present(incx)) then
    incx0 = incx
  else
    incx0 = 1
  end if
  if (present(iy)) then
    iy0 = iy
  else
    iy0 = 1
  end if
  if (present(jy)) then
    jy0 = jy
  else
    jy0 = 1
  end if
  if (present(incy)) then
    incy0 = incy
  else
    incy0 = 1
  end if
    call phemv(uplo0, nn0, alpha0, aa, ia0, ja0, desca, xx, ix0, jx0, descx, &
        & incx0, beta0, yy, iy0, jy0, descy, incy0)

  end subroutine pblasfx_phemv_complex


  subroutine pblasfx_phemv_dcomplex(aa, desca, xx, descx, yy, descy, uplo, alpha, beta, &
      & nn, ia, ja, ix, jx, incx, iy, jy, incy)
    complex(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(dp), intent(in) :: xx(:,:)
    integer, intent(in) :: descx(DLEN_)
    complex(dp), intent(inout) :: yy(:,:)
    integer, intent(in) :: descy(DLEN_)
    character, intent(in), optional :: uplo
    complex(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: nn, ia, ja, ix, jx, incx, iy, jy, incy

    complex(dp) :: alpha0, beta0
    character :: uplo0
    integer :: nn0, ia0, ja0, ix0, jx0, incx0, iy0, jy0, incy0

  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, kind=dp)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(M_)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ix)) then
    ix0 = ix
  else
    ix0 = 1
  end if
  if (present(jx)) then
    jx0 = jx
  else
    jx0 = 1
  end if
  if (present(incx)) then
    incx0 = incx
  else
    incx0 = 1
  end if
  if (present(iy)) then
    iy0 = iy
  else
    iy0 = 1
  end if
  if (present(jy)) then
    jy0 = jy
  else
    jy0 = 1
  end if
  if (present(incy)) then
    incy0 = incy
  else
    incy0 = 1
  end if
    call phemv(uplo0, nn0, alpha0, aa, ia0, ja0, desca, xx, ix0, jx0, descx, &
        & incx0, beta0, yy, iy0, jy0, descy, incy0)

  end subroutine pblasfx_phemv_dcomplex



  !> Symmetric/Hermitian matrix with general matrix product
  !! \param aa  Symmetric/Hermitian matrix.
  !! \param desca  Descriptor of aa.
  !! \param bb  general matrix.
  !! \param descb  Descriptor of bb.
  !! \param cc  Matrix to store result
  !! \param descc  Descriptor of cc.
  !! \param side "L" for for left, "R" for right (default: "L"),
  !!        if "L" C := alpha * A * B + beta*C
  !!        if "R" C := alpha * B * A + beta*C
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_psymm_real(aa, desca, bb, descb, cc, descc, side, uplo, &
      & alpha, beta, mm, nn, ia, ja, ib, jb, ic, jc)
    real(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(sp), intent(in) :: bb(:,:)
    integer, intent(in) :: descb(DLEN_)
    real(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: side
    character, intent(in), optional :: uplo
    real(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn, ia, ja, ib, jb, ic, jc

    real(sp) :: alpha0, beta0
    character :: side0, uplo0
    integer :: mm0, nn0, ia0, ja0, ib0, jb0, ic0, jc0

  if (present(side)) then
    side0 = side
  else
    side0 = "L"
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=sp)
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call psymm(side0, uplo0, mm0, nn0, alpha0, aa, ia0, ja0, desca, &
        & bb, ib0, jb0, descb, beta0, cc, ic0, jc0, descc)
  end subroutine pblasfx_psymm_real


  !> Symmetric/Hermitian matrix with general matrix product
  !! \param aa  Symmetric/Hermitian matrix.
  !! \param desca  Descriptor of aa.
  !! \param bb  general matrix.
  !! \param descb  Descriptor of bb.
  !! \param cc  Matrix to store result
  !! \param descc  Descriptor of cc.
  !! \param side "L" for for left, "R" for right (default: "L"),
  !!        if "L" C := alpha * A * B + beta*C
  !!        if "R" C := alpha * B * A + beta*C
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_psymm_dreal(aa, desca, bb, descb, cc, descc, side, uplo, &
      & alpha, beta, mm, nn, ia, ja, ib, jb, ic, jc)
    real(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(dp), intent(in) :: bb(:,:)
    integer, intent(in) :: descb(DLEN_)
    real(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: side
    character, intent(in), optional :: uplo
    real(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn, ia, ja, ib, jb, ic, jc

    real(dp) :: alpha0, beta0
    character :: side0, uplo0
    integer :: mm0, nn0, ia0, ja0, ib0, jb0, ic0, jc0

  if (present(side)) then
    side0 = side
  else
    side0 = "L"
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=dp)
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call psymm(side0, uplo0, mm0, nn0, alpha0, aa, ia0, ja0, desca, &
        & bb, ib0, jb0, descb, beta0, cc, ic0, jc0, descc)
  end subroutine pblasfx_psymm_dreal


  !> Symmetric/Hermitian matrix with general matrix product
  !! \param aa  Symmetric/Hermitian matrix.
  !! \param desca  Descriptor of aa.
  !! \param bb  general matrix.
  !! \param descb  Descriptor of bb.
  !! \param cc  Matrix to store result
  !! \param descc  Descriptor of cc.
  !! \param side "L" for for left, "R" for right (default: "L"),
  !!        if "L" C := alpha * A * B + beta*C
  !!        if "R" C := alpha * B * A + beta*C
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_phemm_complex(aa, desca, bb, descb, cc, descc, side, uplo, &
      & alpha, beta, mm, nn, ia, ja, ib, jb, ic, jc)
    complex(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(sp), intent(in) :: bb(:,:)
    integer, intent(in) :: descb(DLEN_)
    complex(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: side
    character, intent(in), optional :: uplo
    complex(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn, ia, ja, ib, jb, ic, jc

    complex(sp) :: alpha0, beta0
    character :: side0, uplo0
    integer :: mm0, nn0, ia0, ja0, ib0, jb0, ic0, jc0

  if (present(side)) then
    side0 = side
  else
    side0 = "L"
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, kind=sp)
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call phemm(side0, uplo0, mm0, nn0, alpha0, aa, ia0, ja0, desca, &
        & bb, ib0, jb0, descb, beta0, cc, ic0, jc0, descc)
  end subroutine pblasfx_phemm_complex


  !> Symmetric/Hermitian matrix with general matrix product
  !! \param aa  Symmetric/Hermitian matrix.
  !! \param desca  Descriptor of aa.
  !! \param bb  general matrix.
  !! \param descb  Descriptor of bb.
  !! \param cc  Matrix to store result
  !! \param descc  Descriptor of cc.
  !! \param side "L" for for left, "R" for right (default: "L"),
  !!        if "L" C := alpha * A * B + beta*C
  !!        if "R" C := alpha * B * A + beta*C
  !! \param uplo "U" for for upper, "L" for lower triangle matrix (default: "L").
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_phemm_dcomplex(aa, desca, bb, descb, cc, descc, side, uplo, &
      & alpha, beta, mm, nn, ia, ja, ib, jb, ic, jc)
    complex(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(dp), intent(in) :: bb(:,:)
    integer, intent(in) :: descb(DLEN_)
    complex(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    character, intent(in), optional :: side
    character, intent(in), optional :: uplo
    complex(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn, ia, ja, ib, jb, ic, jc

    complex(dp) :: alpha0, beta0
    character :: side0, uplo0
    integer :: mm0, nn0, ia0, ja0, ib0, jb0, ic0, jc0

  if (present(side)) then
    side0 = side
  else
    side0 = "L"
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, kind=dp)
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call phemm(side0, uplo0, mm0, nn0, alpha0, aa, ia0, ja0, desca, &
        & bb, ib0, jb0, descb, beta0, cc, ic0, jc0, descc)
  end subroutine pblasfx_phemm_dcomplex



  !> Matrix matrix product: alpha * A * B + beta * C.
  !!
  !! \see PBLAS documentation (p?gemm routines)
  !!
  subroutine pblasfx_pgemm_real(aa, desca, bb, descb, cc, descc, alpha, beta, &
      & transa, transb, ia, ja, ib, jb, ic, jc, mm, nn, kk)

    !> Left operand matrix A.
    real(sp), intent(in) :: aa(:,:)

    !> Descriptor of A.
    integer, intent(in) :: desca(DLEN_)

    !> Right operand matrix B.
    real(sp), intent(in) :: bb(:,:)

    !> Descriptor of B.
    integer, intent(in) :: descb(DLEN_)

    !> Added matrix C.
    real(sp), intent(inout) :: cc(:,:)

    !> Descriptor of C.
    integer, intent(in) :: descc(DLEN_)

    !> Prefactor alpha (alpha * A * B). Default: 1.0
    real(sp), intent(in), optional :: alpha

    !> Prefactor beta (beta * C). Default: 0.0
    real(sp), intent(in), optional :: beta

    !> Whether A should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transa

    !> Whether B should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transb

    !> First row of submatrix of A. Default: 1
    integer, intent(in), optional :: ia

    !> First column of submatrix of A. Default: 1
    integer, intent(in), optional :: ja

    !> First row of submatrix of B. Default: 1
    integer, intent(in), optional :: ib

    !> First column of submatrix of B. Default: 1
    integer, intent(in), optional :: jb

    !> First row of submatrix of C. Default: 1
    integer, intent(in), optional :: ic

    !> First column of submatrix of C. Default: 1
    integer, intent(in), optional :: jc

    !> Number of rows in the submatrix of A and C. Default: desca(M_)
    integer, intent(in), optional :: mm

    !> Number of colums in the submatrix of B and C. Default: descb(N_)
    integer, intent(in), optional :: nn

    !> Number of columns/rows in the submatrix A, B. Default: desca(N_)
    integer, intent(in), optional :: kk

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    real(sp) :: alpha0, beta0
    integer :: ia0, ja0, ib0, jb0, ic0, jc0, mm0, nn0, kk0
    character :: transa0, transb0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=sp)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = descb(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
  if (present(transa)) then
    transa0 = transa
  else
    transa0 = "N"
  end if
  if (present(transb)) then
    transb0 = transb
  else
    transb0 = "N"
  end if

    call pgemm(transa0, transb0, mm0, nn0, kk0, alpha0, aa, ia0, ja0, desca, &
        & bb, ib0, jb0, descb, beta0, cc, ic0, jc0, descc)

  end subroutine pblasfx_pgemm_real


  !> Matrix matrix product: alpha * A * B + beta * C.
  !!
  !! \see PBLAS documentation (p?gemm routines)
  !!
  subroutine pblasfx_pgemm_dreal(aa, desca, bb, descb, cc, descc, alpha, beta, &
      & transa, transb, ia, ja, ib, jb, ic, jc, mm, nn, kk)

    !> Left operand matrix A.
    real(dp), intent(in) :: aa(:,:)

    !> Descriptor of A.
    integer, intent(in) :: desca(DLEN_)

    !> Right operand matrix B.
    real(dp), intent(in) :: bb(:,:)

    !> Descriptor of B.
    integer, intent(in) :: descb(DLEN_)

    !> Added matrix C.
    real(dp), intent(inout) :: cc(:,:)

    !> Descriptor of C.
    integer, intent(in) :: descc(DLEN_)

    !> Prefactor alpha (alpha * A * B). Default: 1.0
    real(dp), intent(in), optional :: alpha

    !> Prefactor beta (beta * C). Default: 0.0
    real(dp), intent(in), optional :: beta

    !> Whether A should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transa

    !> Whether B should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transb

    !> First row of submatrix of A. Default: 1
    integer, intent(in), optional :: ia

    !> First column of submatrix of A. Default: 1
    integer, intent(in), optional :: ja

    !> First row of submatrix of B. Default: 1
    integer, intent(in), optional :: ib

    !> First column of submatrix of B. Default: 1
    integer, intent(in), optional :: jb

    !> First row of submatrix of C. Default: 1
    integer, intent(in), optional :: ic

    !> First column of submatrix of C. Default: 1
    integer, intent(in), optional :: jc

    !> Number of rows in the submatrix of A and C. Default: desca(M_)
    integer, intent(in), optional :: mm

    !> Number of colums in the submatrix of B and C. Default: descb(N_)
    integer, intent(in), optional :: nn

    !> Number of columns/rows in the submatrix A, B. Default: desca(N_)
    integer, intent(in), optional :: kk

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    real(dp) :: alpha0, beta0
    integer :: ia0, ja0, ib0, jb0, ic0, jc0, mm0, nn0, kk0
    character :: transa0, transb0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=dp)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = descb(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
  if (present(transa)) then
    transa0 = transa
  else
    transa0 = "N"
  end if
  if (present(transb)) then
    transb0 = transb
  else
    transb0 = "N"
  end if

    call pgemm(transa0, transb0, mm0, nn0, kk0, alpha0, aa, ia0, ja0, desca, &
        & bb, ib0, jb0, descb, beta0, cc, ic0, jc0, descc)

  end subroutine pblasfx_pgemm_dreal


  !> Matrix matrix product: alpha * A * B + beta * C.
  !!
  !! \see PBLAS documentation (p?gemm routines)
  !!
  subroutine pblasfx_pgemm_complex(aa, desca, bb, descb, cc, descc, alpha, beta, &
      & transa, transb, ia, ja, ib, jb, ic, jc, mm, nn, kk)

    !> Left operand matrix A.
    complex(sp), intent(in) :: aa(:,:)

    !> Descriptor of A.
    integer, intent(in) :: desca(DLEN_)

    !> Right operand matrix B.
    complex(sp), intent(in) :: bb(:,:)

    !> Descriptor of B.
    integer, intent(in) :: descb(DLEN_)

    !> Added matrix C.
    complex(sp), intent(inout) :: cc(:,:)

    !> Descriptor of C.
    integer, intent(in) :: descc(DLEN_)

    !> Prefactor alpha (alpha * A * B). Default: 1.0
    complex(sp), intent(in), optional :: alpha

    !> Prefactor beta (beta * C). Default: 0.0
    complex(sp), intent(in), optional :: beta

    !> Whether A should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transa

    !> Whether B should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transb

    !> First row of submatrix of A. Default: 1
    integer, intent(in), optional :: ia

    !> First column of submatrix of A. Default: 1
    integer, intent(in), optional :: ja

    !> First row of submatrix of B. Default: 1
    integer, intent(in), optional :: ib

    !> First column of submatrix of B. Default: 1
    integer, intent(in), optional :: jb

    !> First row of submatrix of C. Default: 1
    integer, intent(in), optional :: ic

    !> First column of submatrix of C. Default: 1
    integer, intent(in), optional :: jc

    !> Number of rows in the submatrix of A and C. Default: desca(M_)
    integer, intent(in), optional :: mm

    !> Number of colums in the submatrix of B and C. Default: descb(N_)
    integer, intent(in), optional :: nn

    !> Number of columns/rows in the submatrix A, B. Default: desca(N_)
    integer, intent(in), optional :: kk

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    complex(sp) :: alpha0, beta0
    integer :: ia0, ja0, ib0, jb0, ic0, jc0, mm0, nn0, kk0
    character :: transa0, transb0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, kind=sp)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = descb(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
  if (present(transa)) then
    transa0 = transa
  else
    transa0 = "N"
  end if
  if (present(transb)) then
    transb0 = transb
  else
    transb0 = "N"
  end if

    call pgemm(transa0, transb0, mm0, nn0, kk0, alpha0, aa, ia0, ja0, desca, &
        & bb, ib0, jb0, descb, beta0, cc, ic0, jc0, descc)

  end subroutine pblasfx_pgemm_complex


  !> Matrix matrix product: alpha * A * B + beta * C.
  !!
  !! \see PBLAS documentation (p?gemm routines)
  !!
  subroutine pblasfx_pgemm_dcomplex(aa, desca, bb, descb, cc, descc, alpha, beta, &
      & transa, transb, ia, ja, ib, jb, ic, jc, mm, nn, kk)

    !> Left operand matrix A.
    complex(dp), intent(in) :: aa(:,:)

    !> Descriptor of A.
    integer, intent(in) :: desca(DLEN_)

    !> Right operand matrix B.
    complex(dp), intent(in) :: bb(:,:)

    !> Descriptor of B.
    integer, intent(in) :: descb(DLEN_)

    !> Added matrix C.
    complex(dp), intent(inout) :: cc(:,:)

    !> Descriptor of C.
    integer, intent(in) :: descc(DLEN_)

    !> Prefactor alpha (alpha * A * B). Default: 1.0
    complex(dp), intent(in), optional :: alpha

    !> Prefactor beta (beta * C). Default: 0.0
    complex(dp), intent(in), optional :: beta

    !> Whether A should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transa

    !> Whether B should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transb

    !> First row of submatrix of A. Default: 1
    integer, intent(in), optional :: ia

    !> First column of submatrix of A. Default: 1
    integer, intent(in), optional :: ja

    !> First row of submatrix of B. Default: 1
    integer, intent(in), optional :: ib

    !> First column of submatrix of B. Default: 1
    integer, intent(in), optional :: jb

    !> First row of submatrix of C. Default: 1
    integer, intent(in), optional :: ic

    !> First column of submatrix of C. Default: 1
    integer, intent(in), optional :: jc

    !> Number of rows in the submatrix of A and C. Default: desca(M_)
    integer, intent(in), optional :: mm

    !> Number of colums in the submatrix of B and C. Default: descb(N_)
    integer, intent(in), optional :: nn

    !> Number of columns/rows in the submatrix A, B. Default: desca(N_)
    integer, intent(in), optional :: kk

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    complex(dp) :: alpha0, beta0
    integer :: ia0, ja0, ib0, jb0, ic0, jc0, mm0, nn0, kk0
    character :: transa0, transb0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, kind=dp)
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = descb(N_)
  end if
  if (present(kk)) then
    kk0 = kk
  else
    kk0 = desca(N_)
  end if
  if (present(transa)) then
    transa0 = transa
  else
    transa0 = "N"
  end if
  if (present(transb)) then
    transb0 = transb
  else
    transb0 = "N"
  end if

    call pgemm(transa0, transb0, mm0, nn0, kk0, alpha0, aa, ia0, ja0, desca, &
        & bb, ib0, jb0, descb, beta0, cc, ic0, jc0, descc)

  end subroutine pblasfx_pgemm_dcomplex



  !> Computes matrix-matrix product with one triangle matrix
  !!
  !! \see PBLAS documentation (routines p?trmm)
  !!
  subroutine pblasfx_ptrmm_real(aa, desca, bb, descb, alpha, side, uplo, transa, &
      & diag, ia, ja, ib, jb, mm, nn)

    !> Unit or non-unit lower or upper triangular matrix A
    real(sp), intent(in) :: aa(:,:)

    !> Descriptor of A.
    integer, intent(in) :: desca(DLEN_)

    !> Second operand (general matrix) B on entry, result on exit.
    real(sp), intent(inout) :: bb(:,:)

    !> Descriptor of B.
    integer, intent(in) :: descb(DLEN_)

    !> Prefactor. Default: 1.0
    real(sp), intent(in), optional :: alpha

    !> From which side is B multiplied by A ("L"/"R"). Default: "L"
    character, intent(in), optional :: side

    !> Whether A is upper ("U") or lower("L") triangle. Default: "L".
    character, intent(in), optional :: uplo

    !> Whether A should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transa

    !> Whether A is unit triangular ("U") or not ("N"). Default: "N".
    character, intent(in), optional :: diag

    !> First row of matrix A to consider. Default: 1
    integer, intent(in), optional :: ia

    !> First column of matrix A to consider. Default: 1
    integer, intent(in), optional :: ja

    !> First row of matrix B to consider. Default: 1
    integer, intent(in), optional :: ib

    !> First column of matrix B to consider. Default: 1
    integer, intent(in), optional :: jb

    !> Number of rows for matrix A. Default: desca(M_)
    integer, intent(in), optional :: mm

    !> Number of columns for matrix A. Default: desca(N_)
    integer, intent(in), optional :: nn

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    integer :: ia0, ja0, ib0, jb0, mm0, nn0
    real(sp) :: alpha0
    character :: side0, uplo0, transa0, diag0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(side)) then
    side0 = side
  else
    side0 = "L"
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(transa)) then
    transa0 = transa
  else
    transa0 = "N"
  end if
  if (present(diag)) then
    diag0 = diag
  else
    diag0 = "N"
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

    call ptrmm(side0, uplo0, transa0, diag0, mm0, nn0, alpha0, aa, ia0, ja0, &
        & desca, bb, ib0, jb0, descb)

  end subroutine pblasfx_ptrmm_real


  !> Computes matrix-matrix product with one triangle matrix
  !!
  !! \see PBLAS documentation (routines p?trmm)
  !!
  subroutine pblasfx_ptrmm_dreal(aa, desca, bb, descb, alpha, side, uplo, transa, &
      & diag, ia, ja, ib, jb, mm, nn)

    !> Unit or non-unit lower or upper triangular matrix A
    real(dp), intent(in) :: aa(:,:)

    !> Descriptor of A.
    integer, intent(in) :: desca(DLEN_)

    !> Second operand (general matrix) B on entry, result on exit.
    real(dp), intent(inout) :: bb(:,:)

    !> Descriptor of B.
    integer, intent(in) :: descb(DLEN_)

    !> Prefactor. Default: 1.0
    real(dp), intent(in), optional :: alpha

    !> From which side is B multiplied by A ("L"/"R"). Default: "L"
    character, intent(in), optional :: side

    !> Whether A is upper ("U") or lower("L") triangle. Default: "L".
    character, intent(in), optional :: uplo

    !> Whether A should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transa

    !> Whether A is unit triangular ("U") or not ("N"). Default: "N".
    character, intent(in), optional :: diag

    !> First row of matrix A to consider. Default: 1
    integer, intent(in), optional :: ia

    !> First column of matrix A to consider. Default: 1
    integer, intent(in), optional :: ja

    !> First row of matrix B to consider. Default: 1
    integer, intent(in), optional :: ib

    !> First column of matrix B to consider. Default: 1
    integer, intent(in), optional :: jb

    !> Number of rows for matrix A. Default: desca(M_)
    integer, intent(in), optional :: mm

    !> Number of columns for matrix A. Default: desca(N_)
    integer, intent(in), optional :: nn

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    integer :: ia0, ja0, ib0, jb0, mm0, nn0
    real(dp) :: alpha0
    character :: side0, uplo0, transa0, diag0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(side)) then
    side0 = side
  else
    side0 = "L"
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(transa)) then
    transa0 = transa
  else
    transa0 = "N"
  end if
  if (present(diag)) then
    diag0 = diag
  else
    diag0 = "N"
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

    call ptrmm(side0, uplo0, transa0, diag0, mm0, nn0, alpha0, aa, ia0, ja0, &
        & desca, bb, ib0, jb0, descb)

  end subroutine pblasfx_ptrmm_dreal


  !> Computes matrix-matrix product with one triangle matrix
  !!
  !! \see PBLAS documentation (routines p?trmm)
  !!
  subroutine pblasfx_ptrmm_complex(aa, desca, bb, descb, alpha, side, uplo, transa, &
      & diag, ia, ja, ib, jb, mm, nn)

    !> Unit or non-unit lower or upper triangular matrix A
    complex(sp), intent(in) :: aa(:,:)

    !> Descriptor of A.
    integer, intent(in) :: desca(DLEN_)

    !> Second operand (general matrix) B on entry, result on exit.
    complex(sp), intent(inout) :: bb(:,:)

    !> Descriptor of B.
    integer, intent(in) :: descb(DLEN_)

    !> Prefactor. Default: 1.0
    complex(sp), intent(in), optional :: alpha

    !> From which side is B multiplied by A ("L"/"R"). Default: "L"
    character, intent(in), optional :: side

    !> Whether A is upper ("U") or lower("L") triangle. Default: "L".
    character, intent(in), optional :: uplo

    !> Whether A should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transa

    !> Whether A is unit triangular ("U") or not ("N"). Default: "N".
    character, intent(in), optional :: diag

    !> First row of matrix A to consider. Default: 1
    integer, intent(in), optional :: ia

    !> First column of matrix A to consider. Default: 1
    integer, intent(in), optional :: ja

    !> First row of matrix B to consider. Default: 1
    integer, intent(in), optional :: ib

    !> First column of matrix B to consider. Default: 1
    integer, intent(in), optional :: jb

    !> Number of rows for matrix A. Default: desca(M_)
    integer, intent(in), optional :: mm

    !> Number of columns for matrix A. Default: desca(N_)
    integer, intent(in), optional :: nn

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    integer :: ia0, ja0, ib0, jb0, mm0, nn0
    complex(sp) :: alpha0
    character :: side0, uplo0, transa0, diag0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, kind=sp)
  end if
  if (present(side)) then
    side0 = side
  else
    side0 = "L"
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(transa)) then
    transa0 = transa
  else
    transa0 = "N"
  end if
  if (present(diag)) then
    diag0 = diag
  else
    diag0 = "N"
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

    call ptrmm(side0, uplo0, transa0, diag0, mm0, nn0, alpha0, aa, ia0, ja0, &
        & desca, bb, ib0, jb0, descb)

  end subroutine pblasfx_ptrmm_complex


  !> Computes matrix-matrix product with one triangle matrix
  !!
  !! \see PBLAS documentation (routines p?trmm)
  !!
  subroutine pblasfx_ptrmm_dcomplex(aa, desca, bb, descb, alpha, side, uplo, transa, &
      & diag, ia, ja, ib, jb, mm, nn)

    !> Unit or non-unit lower or upper triangular matrix A
    complex(dp), intent(in) :: aa(:,:)

    !> Descriptor of A.
    integer, intent(in) :: desca(DLEN_)

    !> Second operand (general matrix) B on entry, result on exit.
    complex(dp), intent(inout) :: bb(:,:)

    !> Descriptor of B.
    integer, intent(in) :: descb(DLEN_)

    !> Prefactor. Default: 1.0
    complex(dp), intent(in), optional :: alpha

    !> From which side is B multiplied by A ("L"/"R"). Default: "L"
    character, intent(in), optional :: side

    !> Whether A is upper ("U") or lower("L") triangle. Default: "L".
    character, intent(in), optional :: uplo

    !> Whether A should be unchanged ("N"), transposed ("T") or transposed
    !! conjugated ("C"). Default: "N".
    character, intent(in), optional :: transa

    !> Whether A is unit triangular ("U") or not ("N"). Default: "N".
    character, intent(in), optional :: diag

    !> First row of matrix A to consider. Default: 1
    integer, intent(in), optional :: ia

    !> First column of matrix A to consider. Default: 1
    integer, intent(in), optional :: ja

    !> First row of matrix B to consider. Default: 1
    integer, intent(in), optional :: ib

    !> First column of matrix B to consider. Default: 1
    integer, intent(in), optional :: jb

    !> Number of rows for matrix A. Default: desca(M_)
    integer, intent(in), optional :: mm

    !> Number of columns for matrix A. Default: desca(N_)
    integer, intent(in), optional :: nn

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    integer :: ia0, ja0, ib0, jb0, mm0, nn0
    complex(dp) :: alpha0
    character :: side0, uplo0, transa0, diag0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, kind=dp)
  end if
  if (present(side)) then
    side0 = side
  else
    side0 = "L"
  end if
  if (present(uplo)) then
    uplo0 = uplo
  else
    uplo0 = "L"
  end if
  if (present(transa)) then
    transa0 = transa
  else
    transa0 = "N"
  end if
  if (present(diag)) then
    diag0 = diag
  else
    diag0 = "N"
  end if
  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ib)) then
    ib0 = ib
  else
    ib0 = 1
  end if
  if (present(jb)) then
    jb0 = jb
  else
    jb0 = 1
  end if
  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

    call ptrmm(side0, uplo0, transa0, diag0, mm0, nn0, alpha0, aa, ia0, ja0, &
        & desca, bb, ib0, jb0, descb)

  end subroutine pblasfx_ptrmm_dcomplex



  !> Real matrix transpose.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_ptran_real(aa, desca, cc, descc, alpha, beta,&
      & mm, nn, ia, ja, ic, jc)
    real(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    real(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn
    integer, intent(in), optional :: ia, ja, ic, jc

    real(sp) :: alpha0, beta0
    integer :: mm0, nn0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=sp)
  end if

  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call ptran(mm0, nn0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)
  end subroutine pblasfx_ptran_real


  !> Real matrix transpose.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_ptran_dreal(aa, desca, cc, descc, alpha, beta,&
      & mm, nn, ia, ja, ic, jc)
    real(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    real(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    real(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn
    integer, intent(in), optional :: ia, ja, ic, jc

    real(dp) :: alpha0, beta0
    integer :: mm0, nn0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = real(1, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = real(0, kind=dp)
  end if

  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call ptran(mm0, nn0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)
  end subroutine pblasfx_ptran_dreal


  !> Complex matrix transpose.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_ptranu_complex(aa, desca, cc, descc, alpha, beta,&
      & mm, nn, ia, ja, ic, jc)
    complex(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    complex(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn
    integer, intent(in), optional :: ia, ja, ic, jc

    complex(sp) :: alpha0, beta0
    integer :: mm0, nn0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, 0, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, 0, kind=sp)
  end if

  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call ptranu(mm0, nn0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_ptranu_complex


  !> Complex matrix transpose.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_ptranu_dcomplex(aa, desca, cc, descc, alpha, beta,&
      & mm, nn, ia, ja, ic, jc)
    complex(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    complex(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn
    integer, intent(in), optional :: ia, ja, ic, jc

    complex(dp) :: alpha0, beta0
    integer :: mm0, nn0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, 0, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, 0, kind=dp)
  end if

  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call ptranu(mm0, nn0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_ptranu_dcomplex


  !> Complex matrix hermitian transpose.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_ptranc_complex(aa, desca, cc, descc, alpha, beta,&
      & mm, nn, ia, ja, ic, jc)
    complex(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(sp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    complex(sp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn
    integer, intent(in), optional :: ia, ja, ic, jc

    complex(sp) :: alpha0, beta0
    integer :: mm0, nn0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, 0, kind=sp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, 0, kind=sp)
  end if

  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call ptranc(mm0, nn0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_ptranc_complex


  !> Complex matrix hermitian transpose.
  !! \param aa  Matrix to update with.
  !! \param desca  Descriptor of aa.
  !! \param cc  Matrix to be updated.
  !! \param desccc Descriptor of cc.
  !! \param alpha  Prefactor.
  !! \param beta  Prefactor.
  subroutine pblasfx_ptranc_dcomplex(aa, desca, cc, descc, alpha, beta,&
      & mm, nn, ia, ja, ic, jc)
    complex(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: desca(DLEN_)
    complex(dp), intent(inout) :: cc(:,:)
    integer, intent(in) :: descc(DLEN_)
    complex(dp), intent(in), optional :: alpha, beta
    integer, intent(in), optional :: mm, nn
    integer, intent(in), optional :: ia, ja, ic, jc

    complex(dp) :: alpha0, beta0
    integer :: mm0, nn0, ia0, ja0, ic0, jc0

  if (present(alpha)) then
    alpha0 = alpha
  else
    alpha0 = cmplx(1, 0, kind=dp)
  end if
  if (present(beta)) then
    beta0 = beta
  else
    beta0 = cmplx(0, 0, kind=dp)
  end if

  if (present(mm)) then
    mm0 = mm
  else
    mm0 = desca(M_)
  end if
  if (present(nn)) then
    nn0 = nn
  else
    nn0 = desca(N_)
  end if

  if (present(ia)) then
    ia0 = ia
  else
    ia0 = 1
  end if
  if (present(ja)) then
    ja0 = ja
  else
    ja0 = 1
  end if
  if (present(ic)) then
    ic0 = ic
  else
    ic0 = 1
  end if
  if (present(jc)) then
    jc0 = jc
  else
    jc0 = 1
  end if
    call ptranc(mm0, nn0, alpha0, aa, ia0, ja0, desca, beta0,&
        & cc, ic0, jc0, descc)

  end subroutine pblasfx_ptranc_dcomplex


end module pblasfx_module
