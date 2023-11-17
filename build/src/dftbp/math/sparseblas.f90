!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!



!> DFTB+ sparse structure operations for a few blas matrix operations
module dftbp_math_sparseblas
  use dftbp_common_accuracy, only : dp
  use dftbp_common_constants, only : imag, pi
  use dftbp_dftb_boundarycond, only : TBoundaryConditions
  use dftbp_type_commontypes, only : TOrbitals
  use dftbp_common_blacsenv, only : TBlacsEnv
  use dftbp_extlibs_scalapackfx, only : blacsfx_gemr2d
  use dftbp_type_densedescr, only : TDenseDescr
  implicit none
  private

  public :: symv, symm
  public :: redist_sqr2rows, redist_rows2sqr

  interface redist_sqr2rows
    module procedure sqr2rows_real
    module procedure sqr2rows_complex
  end interface redist_sqr2rows

  interface redist_rows2sqr
    module procedure rows2sqr_real
    module procedure rows2sqr_complex
  end interface redist_rows2sqr


  ! Rank 2 routines

  !> Routine for symmetric sparse matrix with dense vector multiply
  interface symv
    module procedure symv_r_gamma
    module procedure symv_bc_r_gamma
    module procedure symv_c_gamma
    module procedure symv_bc_c_gamma
    module procedure symv_kpt
    module procedure symv_bc_kpt
  end interface symv

  ! Rank 3 routines

  !> Routine for multiplication between a symmetric sparse matrix and a general
  !> dense matrix
  interface symm
    module procedure symm_r_gamma
    module procedure symm_bc_r_gamma
    module procedure symm_c_gamma
    module procedure symm_bc_c_gamma
    module procedure symm_kpt
    module procedure symm_bc_kpt
  end interface symm

contains


  !> Sparse Gamma point matrix with a real vector as a symv operation,
  !> y = alpha A x + beta * y
  subroutine symv_r_gamma(y, A, x, iNeighbour, nNeighbour, img2CentCell, iSparseStart,&
      & iAtomStart, orb, alpha, beta)

    !> Vector on return
    real(dp), intent(inout):: y(:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Vector on entry
    real(dp), intent(in) :: x(:)

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * x
    real(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming y
    real(dp), optional, intent(in) :: beta

    integer :: iOrig, ix, iy, jx, jy, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii
    real(dp) :: sqrTmp(orb%mOrb, orb%mOrb)
    real(dp) :: alphaTmp



    nAtom = size(iAtomStart)-1

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      y(:) = beta * y
    else
      y(:) = 0.0_dp
    end if

    !$OMP PARALLEL DO DEFAULT(SHARED) &
    !$OMP &PRIVATE(iAtom1,nOrb1,ix,jx,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iy,jy) &
    !$OMP & REDUCTION(+:y) SCHEDULE(RUNTIME)
    do iAtom1 = 1, nAtom
      ix = iAtomStart(iAtom1)
      jx = iAtomStart(iAtom1 + 1) -1
      nOrb1 = jx - ix + 1
      do iNeigh = 0, nNeighbour(iAtom1)
        iOrig = iSparseStart(iNeigh,iAtom1) + 1
        sqrTmp(:,:) = 0.0_dp
        iAtom2 = iNeighbour(iNeigh, iAtom1)
        iAtom2f = img2CentCell(iAtom2)
        iy = iAtomStart(iAtom2f)
        jy = iAtomStart(iAtom2f + 1) - 1
        nOrb2 = jy - iy + 1
        sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
        ! symmetrize on-diagonal blocks just in case
        if (iAtom1 == iAtom2f) then
          do ii = 1, nOrb1
            sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
          end do
        end if
        y(iy:jy) = y(iy:jy) + alphaTmp * matmul(sqrTmp(:nOrb2,:nOrb1),x(ix:jx))
        ! other triangle due to symmetry of matrix
        if (iAtom1 /= iAtom2f) then
          y(ix:jx) = y(ix:jx) + alphaTmp * matmul(transpose(sqrTmp(:nOrb2,:nOrb1)),x(iy:jy))
        end if
      end do
    end do
    !$OMP  END PARALLEL DO

  end subroutine symv_r_gamma


  !> Sparse Gamma point matrix with a real vector as a symv operation, using specified
  !> boundary conditions, y = alpha A x + beta * y
  subroutine symv_bc_r_gamma(y, A, x, bcs, iNeighbour, nNeighbour, img2CentCell,&
      & iSparseStart, iAtomStart, coords, species, orb, alpha, beta)

    !> Vector on return
    real(dp), intent(inout):: y(:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Vector on entry
    real(dp), intent(in) :: x(:)

    !> Boundary conditions on the system
    type(TBoundaryConditions), intent(in) :: bcs

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coords(:,:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * x
    real(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming y
    real(dp), optional, intent(in) :: beta

    integer :: iOrig, ix, iy, jx, jy, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii
    real(dp) :: sqrTmp(orb%mOrb, orb%mOrb)
    real(dp) :: alphaTmp



    nAtom = size(iAtomStart)-1

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      y(:) = beta * y
    else
      y(:) = 0.0_dp
    end if

    !$OMP PARALLEL DO DEFAULT(SHARED) &
    !$OMP &PRIVATE(iAtom1,nOrb1,ix,jx,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iy,jy) &
    !$OMP & REDUCTION(+:y) SCHEDULE(RUNTIME)
    do iAtom1 = 1, nAtom
      ix = iAtomStart(iAtom1)
      jx = iAtomStart(iAtom1 + 1) -1
      nOrb1 = jx - ix + 1
      do iNeigh = 0, nNeighbour(iAtom1)
        iOrig = iSparseStart(iNeigh,iAtom1) + 1
        sqrTmp(:,:) = 0.0_dp
        iAtom2 = iNeighbour(iNeigh, iAtom1)
        iAtom2f = img2CentCell(iAtom2)
        iy = iAtomStart(iAtom2f)
        jy = iAtomStart(iAtom2f + 1) - 1
        nOrb2 = jy - iy + 1
        sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
        call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
        ! symmetrize on-diagonal blocks just in case
        if (iAtom1 == iAtom2f) then
          do ii = 1, nOrb1
            sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
          end do
        end if
        y(iy:jy) = y(iy:jy) + alphaTmp * matmul(sqrTmp(:nOrb2,:nOrb1),x(ix:jx))
        ! other triangle due to symmetry of matrix
        if (iAtom1 /= iAtom2f) then
          y(ix:jx) = y(ix:jx) + alphaTmp * matmul(transpose(sqrTmp(:nOrb2,:nOrb1)),x(iy:jy))
        end if
      end do
    end do
    !$OMP  END PARALLEL DO

  end subroutine symv_bc_r_gamma


  !> Sparse Gamma point matrix with a complex vector as a symv operation,
  !> y = alpha A x + beta * y
  subroutine symv_c_gamma(y, A, x, iNeighbour, nNeighbour, img2CentCell, iSparseStart,&
      & iAtomStart, orb, alpha, beta)

    !> Vector on return
    complex(dp), intent(inout):: y(:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Vector on entry
    complex(dp), intent(in) :: x(:)

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * x
    complex(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming y
    complex(dp), optional, intent(in) :: beta

    integer :: iOrig, ix, iy, jx, jy, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii
    real(dp) :: sqrTmp(orb%mOrb, orb%mOrb)
    complex(dp) :: alphaTmp



    nAtom = size(iAtomStart)-1

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      y(:) = beta * y
    else
      y(:) = 0.0_dp
    end if

    !$OMP PARALLEL DO DEFAULT(SHARED) &
    !$OMP &PRIVATE(iAtom1,nOrb1,ix,jx,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iy,jy) &
    !$OMP & REDUCTION(+:y) SCHEDULE(RUNTIME)
    do iAtom1 = 1, nAtom
      ix = iAtomStart(iAtom1)
      jx = iAtomStart(iAtom1 + 1) -1
      nOrb1 = jx - ix + 1
      do iNeigh = 0, nNeighbour(iAtom1)
        iOrig = iSparseStart(iNeigh,iAtom1) + 1
        sqrTmp(:,:) = 0.0_dp
        iAtom2 = iNeighbour(iNeigh, iAtom1)
        iAtom2f = img2CentCell(iAtom2)
        iy = iAtomStart(iAtom2f)
        jy = iAtomStart(iAtom2f + 1) - 1
        nOrb2 = jy - iy + 1
        sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
        ! symmetrize on-diagonal blocks just in case
        if (iAtom1 == iAtom2f) then
          do ii = 1, nOrb1
            sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
          end do
        end if
        y(iy:jy) = y(iy:jy) + alphaTmp * matmul(sqrTmp(:nOrb2,:nOrb1),x(ix:jx))
        ! other triangle due to symmetry of matrix
        if (iAtom1 /= iAtom2f) then
          y(ix:jx) = y(ix:jx) + alphaTmp * matmul(transpose(sqrTmp(:nOrb2,:nOrb1)),x(iy:jy))
        end if
      end do
    end do
    !$OMP  END PARALLEL DO

  end subroutine symv_c_gamma


  !> Sparse Gamma point matrix with a complex vector as a symv operation, using specified
  !> boundary conditions, y = alpha A x + beta * y
  subroutine symv_bc_c_gamma(y, A, x, bcs, iNeighbour, nNeighbour, img2CentCell,&
      & iSparseStart, iAtomStart, coords, species, orb, alpha, beta)

    !> Vector on return
    complex(dp), intent(inout):: y(:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Vector on entry
    complex(dp), intent(in) :: x(:)

    !> Boundary conditions on the system
    type(TBoundaryConditions), intent(in) :: bcs

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coords(:,:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * x
    complex(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming y
    complex(dp), optional, intent(in) :: beta

    integer :: iOrig, ix, iy, jx, jy, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii
    real(dp) :: sqrTmp(orb%mOrb, orb%mOrb)
    complex(dp) :: alphaTmp



    nAtom = size(iAtomStart)-1

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      y(:) = beta * y
    else
      y(:) = 0.0_dp
    end if

    !$OMP PARALLEL DO DEFAULT(SHARED) &
    !$OMP &PRIVATE(iAtom1,nOrb1,ix,jx,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iy,jy) &
    !$OMP & REDUCTION(+:y) SCHEDULE(RUNTIME)
    do iAtom1 = 1, nAtom
      ix = iAtomStart(iAtom1)
      jx = iAtomStart(iAtom1 + 1) -1
      nOrb1 = jx - ix + 1
      do iNeigh = 0, nNeighbour(iAtom1)
        iOrig = iSparseStart(iNeigh,iAtom1) + 1
        sqrTmp(:,:) = 0.0_dp
        iAtom2 = iNeighbour(iNeigh, iAtom1)
        iAtom2f = img2CentCell(iAtom2)
        iy = iAtomStart(iAtom2f)
        jy = iAtomStart(iAtom2f + 1) - 1
        nOrb2 = jy - iy + 1
        sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
        call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
        ! symmetrize on-diagonal blocks just in case
        if (iAtom1 == iAtom2f) then
          do ii = 1, nOrb1
            sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
          end do
        end if
        y(iy:jy) = y(iy:jy) + alphaTmp * matmul(sqrTmp(:nOrb2,:nOrb1),x(ix:jx))
        ! other triangle due to symmetry of matrix
        if (iAtom1 /= iAtom2f) then
          y(ix:jx) = y(ix:jx) + alphaTmp * matmul(transpose(sqrTmp(:nOrb2,:nOrb1)),x(iy:jy))
        end if
      end do
    end do
    !$OMP  END PARALLEL DO

  end subroutine symv_bc_c_gamma



  !> Sparse matrix at specified k-point with a vector as a symv operation, y = alpha A x + beta * y
  subroutine symv_kpt(y, A, x, iNeighbour, nNeighbour, kPoint, iCellVec, cellVec, img2CentCell,&
      & iSparseStart, iAtomStart, orb, alpha, beta)

    !> Vector on return
    complex(dp), intent(inout):: y(:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Vector on entry
    complex(dp), intent(in) :: x(:)

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Relative coordinates of the k-point where the sparse matrix should be unfolded.
    real(dp), intent(in) :: kPoint(:)

    !> Index of the cell translation vector for each atom.
    integer, intent(in) :: iCellVec(:)

    !> Relative coordinates of the cell translation vectors.
    real(dp), intent(in) :: cellVec(:, :)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * x
    complex(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming y
    complex(dp), optional, intent(in) :: beta

    integer :: iOrig, ix, iy, jx, jy, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii, iVec
    complex(dp) :: sqrTmp(orb%mOrb, orb%mOrb), alphaTmp, phase
    real(dp) :: kPoint2p(3)



    nAtom = size(iAtomStart)-1

    kPoint2p(:) = 2.0_dp * pi * kPoint

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      y(:) = beta * y
    else
      y(:) = 0.0_dp
    end if

    !$OMP PARALLEL DO DEFAULT(SHARED) &
    !$OMP &PRIVATE(iAtom1,nOrb1,ix,jx,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iy,jy,iVec,phase)&
    !$OMP &REDUCTION(+:y) SCHEDULE(RUNTIME)
    do iAtom1 = 1, nAtom
      ix = iAtomStart(iAtom1)
      jx = iAtomStart(iAtom1 + 1) -1
      nOrb1 = jx - ix + 1
      do iNeigh = 0, nNeighbour(iAtom1)
        iOrig = iSparseStart(iNeigh,iAtom1) + 1
        sqrTmp(:,:) = 0.0_dp
        iAtom2 = iNeighbour(iNeigh, iAtom1)
        iAtom2f = img2CentCell(iAtom2)
        iy = iAtomStart(iAtom2f)
        jy = iAtomStart(iAtom2f + 1) - 1
        nOrb2 = jy - iy + 1
        iVec = iCellVec(iAtom2)
        phase = exp(imag * dot_product(kPoint2p, cellVec(:, iVec)))
        sqrTmp(:nOrb2,:nOrb1) = phase * reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
        ! hermitian symmetry for on-diagonal blocks, just in case
        if (iAtom1 == iAtom2f) then
          do ii = 1, nOrb1
            sqrTmp(ii,ii+1:nOrb2) = conjg(sqrTmp(ii+1:nOrb2,ii))
          end do
        end if
        y(iy:jy) = y(iy:jy) + alphaTmp * matmul(sqrTmp(:nOrb2,:nOrb1),x(ix:jx))
        ! other triangle due to symmetry of matrix
        if (iAtom1 /= iAtom2f) then
          y(ix:jx) = y(ix:jx) + alphaTmp*matmul(transpose(conjg(sqrTmp(:nOrb2,:nOrb1))),x(iy:jy))
        end if
      end do
    end do
    !$OMP  END PARALLEL DO

  end subroutine symv_kpt


  !> Sparse matrix at specified k-point with a vector as a symv operation, using specified boundary
  !> conditions, y = alpha A x + beta * y
  subroutine symv_bc_kpt(y, A, x, bcs, iNeighbour, nNeighbour, kPoint, iCellVec, cellVec,&
      & img2CentCell, iSparseStart, iAtomStart, coords, species, orb, alpha, beta)

    !> Vector on return
    complex(dp), intent(inout):: y(:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Vector on entry
    complex(dp), intent(in) :: x(:)

    !> Boundary conditions on the system
    type(TBoundaryConditions), intent(in) :: bcs

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Relative coordinates of the k-point where the sparse matrix should be unfolded.
    real(dp), intent(in) :: kPoint(:)

    !> Index of the cell translation vector for each atom.
    integer, intent(in) :: iCellVec(:)

    !> Relative coordinates of the cell translation vectors.
    real(dp), intent(in) :: cellVec(:, :)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coords(:,:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * x
    complex(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming y
    complex(dp), optional, intent(in) :: beta

    integer :: iOrig, ix, iy, jx, jy, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii, iVec
    complex(dp) :: sqrTmp(orb%mOrb, orb%mOrb), alphaTmp, phase
    real(dp) :: kPoint2p(2)



    nAtom = size(iAtomStart)-1

    kPoint2p(:) = 2.0_dp * pi * kPoint

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      y(:) = beta * y
    else
      y(:) = 0.0_dp
    end if

    !$OMP PARALLEL DO DEFAULT(SHARED) &
    !$OMP &PRIVATE(iAtom1,nOrb1,ix,jx,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iy,jy,iVec,phase)&
    !$OMP &REDUCTION(+:y) SCHEDULE(RUNTIME)
    do iAtom1 = 1, nAtom
      ix = iAtomStart(iAtom1)
      jx = iAtomStart(iAtom1 + 1) -1
      nOrb1 = jx - ix + 1
      do iNeigh = 0, nNeighbour(iAtom1)
        iOrig = iSparseStart(iNeigh,iAtom1) + 1
        sqrTmp(:,:) = 0.0_dp
        iAtom2 = iNeighbour(iNeigh, iAtom1)
        iAtom2f = img2CentCell(iAtom2)
        iy = iAtomStart(iAtom2f)
        jy = iAtomStart(iAtom2f + 1) - 1
        nOrb2 = jy - iy + 1
        iVec = iCellVec(iAtom2)
        phase = exp(imag * dot_product(kPoint2p, cellVec(:, iVec)))
        sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
        call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
        sqrTmp(:nOrb2,:nOrb1) = phase * sqrTmp
        ! hermitian symmetry for on-diagonal blocks, just in case
        if (iAtom1 == iAtom2f) then
          do ii = 1, nOrb1
            sqrTmp(ii,ii+1:nOrb2) = conjg(sqrTmp(ii+1:nOrb2,ii))
          end do
        end if
        y(iy:jy) = y(iy:jy) + alphaTmp * matmul(sqrTmp(:nOrb2,:nOrb1),x(ix:jx))
        ! other triangle due to symmetry of matrix
        if (iAtom1 /= iAtom2f) then
          y(ix:jx) = y(ix:jx) + alphaTmp*matmul(transpose(conjg(sqrTmp(:nOrb2,:nOrb1))),x(iy:jy))
        end if
      end do
    end do
    !$OMP  END PARALLEL DO

  end subroutine symv_bc_kpt



  !> Sparse Gamma point matrix with dense real matrix as a symm operation,
  !> symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and sparse and B is general.
  subroutine symm_r_gamma(C, side, A, B, iNeighbour, nNeighbour, img2CentCell,&
      & iSparseStart, iAtomStart, orb, alpha, beta)

    !> Dense matrix on return
    real(dp), intent(inout):: C(:,:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Side which is the sparse matrix. SIDE = 'L' or 'l' C := alpha*A*B + beta*C; SIDE = 'R' or 'r'
    !> C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> Dense matrix on entry
    real(dp), intent(in) :: B(:,:)

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * B
    real(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming C
    real(dp), optional, intent(in) :: beta

    integer :: iOrig, iB, iC, jB, jC, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii
    real(dp) :: sqrTmp(orb%mOrb, orb%mOrb)
    real(dp) :: alphaTmp




    nAtom = size(iAtomStart)-1

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      C(:,:) = beta * C
    else
      C(:,:) = 0.0_dp
    end if

    select case(side)

    case ("L", "l")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(iAtom1,nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC) &
      !$OMP & REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          ! symmetrize on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
            end do
          end if
          C(iC:jC,:) = C(iC:jC,:) + alphaTmp*matmul(sqrTmp(:nOrb2,:nOrb1),B(iB:jB,:))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(iB:jB, :) = C(iB:jB,:) + alphaTmp*matmul(transpose(sqrTmp(:nOrb2,:nOrb1)),B(iC:jC,:))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    case ("R", "r")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(iAtom1,nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC) &
      !$OMP & REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          ! symmetrize on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
            end do
          end if
          C(:,iC:jC) = C(:,iC:jC) + alphaTmp*matmul(B(:,iB:jB),transpose(sqrTmp(:nOrb2,:nOrb1)))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(:,iB:jB) = C(:,iB:jB) + alphaTmp*matmul(B(:,iC:jC),sqrTmp(:nOrb2,:nOrb1))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    end select

  end subroutine symm_r_gamma


  !> Sparse Gamma point matrix with dense real matrix as a symm operation, using specified
  !> boundary conditions. Symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and sparse
  !> and B is general.
  subroutine symm_bc_r_gamma(C, side, A, B, bcs, iNeighbour, nNeighbour, img2CentCell,&
      & iSparseStart, iAtomStart, coords, species, orb, alpha, beta)

    !> Dense matrix on return
    real(dp), intent(inout):: C(:,:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Side which is the sparse matrix. SIDE = 'L' or 'l' C := alpha*A*B + beta*C; SIDE = 'R' or 'r'
    !> C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> Dense matrix on entry
    real(dp), intent(in) :: B(:,:)

    !> Boundary conditions on the system
    type(TBoundaryConditions), intent(in) :: bcs

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coords(:,:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * B
    real(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming C
    real(dp), optional, intent(in) :: beta

    integer :: iOrig, iB, iC, jB, jC, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii
    real(dp) :: sqrTmp(orb%mOrb, orb%mOrb)
    real(dp) :: alphaTmp




    nAtom = size(iAtomStart)-1

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      C(:,:) = beta * C
    else
      C(:,:) = 0.0_dp
    end if

    select case(side)

    case ("L", "l")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(iAtom1,nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC) &
      !$OMP & REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
          ! symmetrize on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
            end do
          end if
          C(iC:jC,:) = C(iC:jC,:) + alphaTmp*matmul(sqrTmp(:nOrb2,:nOrb1),B(iB:jB,:))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(iB:jB, :) = C(iB:jB,:) + alphaTmp*matmul(transpose(sqrTmp(:nOrb2,:nOrb1)),B(iC:jC,:))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    case ("R", "r")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(iAtom1,nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC) &
      !$OMP & REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
          ! symmetrize on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
            end do
          end if
          C(:,iC:jC) = C(:,iC:jC) + alphaTmp*matmul(B(:,iB:jB),transpose(sqrTmp(:nOrb2,:nOrb1)))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(:,iB:jB) = C(:,iB:jB) + alphaTmp*matmul(B(:,iC:jC),sqrTmp(:nOrb2,:nOrb1))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    end select

  end subroutine symm_bc_r_gamma


  !> Sparse Gamma point matrix with dense complex matrix as a symm operation,
  !> symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and sparse and B is general.
  subroutine symm_c_gamma(C, side, A, B, iNeighbour, nNeighbour, img2CentCell,&
      & iSparseStart, iAtomStart, orb, alpha, beta)

    !> Dense matrix on return
    complex(dp), intent(inout):: C(:,:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Side which is the sparse matrix. SIDE = 'L' or 'l' C := alpha*A*B + beta*C; SIDE = 'R' or 'r'
    !> C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> Dense matrix on entry
    complex(dp), intent(in) :: B(:,:)

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * B
    complex(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming C
    complex(dp), optional, intent(in) :: beta

    integer :: iOrig, iB, iC, jB, jC, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii
    real(dp) :: sqrTmp(orb%mOrb, orb%mOrb)
    complex(dp) :: alphaTmp




    nAtom = size(iAtomStart)-1

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      C(:,:) = beta * C
    else
      C(:,:) = 0.0_dp
    end if

    select case(side)

    case ("L", "l")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(iAtom1,nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC) &
      !$OMP & REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          ! symmetrize on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
            end do
          end if
          C(iC:jC,:) = C(iC:jC,:) + alphaTmp*matmul(sqrTmp(:nOrb2,:nOrb1),B(iB:jB,:))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(iB:jB, :) = C(iB:jB,:) + alphaTmp*matmul(transpose(sqrTmp(:nOrb2,:nOrb1)),B(iC:jC,:))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    case ("R", "r")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(iAtom1,nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC) &
      !$OMP & REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          ! symmetrize on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
            end do
          end if
          C(:,iC:jC) = C(:,iC:jC) + alphaTmp*matmul(B(:,iB:jB),transpose(sqrTmp(:nOrb2,:nOrb1)))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(:,iB:jB) = C(:,iB:jB) + alphaTmp*matmul(B(:,iC:jC),sqrTmp(:nOrb2,:nOrb1))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    end select

  end subroutine symm_c_gamma


  !> Sparse Gamma point matrix with dense complex matrix as a symm operation, using specified
  !> boundary conditions. Symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and sparse
  !> and B is general.
  subroutine symm_bc_c_gamma(C, side, A, B, bcs, iNeighbour, nNeighbour, img2CentCell,&
      & iSparseStart, iAtomStart, coords, species, orb, alpha, beta)

    !> Dense matrix on return
    complex(dp), intent(inout):: C(:,:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Side which is the sparse matrix. SIDE = 'L' or 'l' C := alpha*A*B + beta*C; SIDE = 'R' or 'r'
    !> C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> Dense matrix on entry
    complex(dp), intent(in) :: B(:,:)

    !> Boundary conditions on the system
    type(TBoundaryConditions), intent(in) :: bcs

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coords(:,:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * B
    complex(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming C
    complex(dp), optional, intent(in) :: beta

    integer :: iOrig, iB, iC, jB, jC, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii
    real(dp) :: sqrTmp(orb%mOrb, orb%mOrb)
    complex(dp) :: alphaTmp




    nAtom = size(iAtomStart)-1

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      C(:,:) = beta * C
    else
      C(:,:) = 0.0_dp
    end if

    select case(side)

    case ("L", "l")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(iAtom1,nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC) &
      !$OMP & REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
          ! symmetrize on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
            end do
          end if
          C(iC:jC,:) = C(iC:jC,:) + alphaTmp*matmul(sqrTmp(:nOrb2,:nOrb1),B(iB:jB,:))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(iB:jB, :) = C(iB:jB,:) + alphaTmp*matmul(transpose(sqrTmp(:nOrb2,:nOrb1)),B(iC:jC,:))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    case ("R", "r")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(iAtom1,nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC) &
      !$OMP & REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
          ! symmetrize on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = sqrTmp(ii+1:nOrb2,ii)
            end do
          end if
          C(:,iC:jC) = C(:,iC:jC) + alphaTmp*matmul(B(:,iB:jB),transpose(sqrTmp(:nOrb2,:nOrb1)))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(:,iB:jB) = C(:,iB:jB) + alphaTmp*matmul(B(:,iC:jC),sqrTmp(:nOrb2,:nOrb1))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    end select

  end subroutine symm_bc_c_gamma



  !> Sparse matrix at specified k-point with dense complex matrix as a symm operation,
  !> symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and sparse and B is general.
  subroutine symm_kpt(C, side, A, B, iNeighbour, nNeighbour, kPoint, iCellVec, cellVec,&
      & img2CentCell, iSparseStart, iAtomStart, orb, alpha, beta)

    !> Dense matrix on return
    complex(dp), intent(inout):: C(:,:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Side which is the sparse matrix. SIDE = 'L' or 'l' C := alpha*A*B + beta*C; SIDE = 'R' or 'r'
    !> C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> Dense matrix on entry
    complex(dp), intent(in) :: B(:,:)

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Relative coordinates of the k-point where the sparse matrix should be unfolded.
    real(dp), intent(in) :: kPoint(:)

    !> Index of the cell translation vector for each atom.
    integer, intent(in) :: iCellVec(:)

    !> Relative coordinates of the cell translation vectors.
    real(dp), intent(in) :: cellVec(:, :)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * B
    complex(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming C
    complex(dp), optional, intent(in) :: beta

    integer :: iOrig, iB, iC, jB, jC, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii, iVec
    complex(dp) :: sqrTmp(orb%mOrb, orb%mOrb), alphaTmp, phase
    real(dp) :: kPoint2p(3)




    nAtom = size(iAtomStart)-1

    kPoint2p(:) = 2.0_dp * pi * kPoint

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      C(:,:) = beta * C
    else
      C(:,:) = 0.0_dp
    end if

    select case(side)

    case ("L", "l")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC,iVec,&
      !$OMP &phase) REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          iVec = iCellVec(iAtom2)
          phase = exp(imag * dot_product(kPoint2p, cellVec(:, iVec)))
          sqrTmp(:nOrb2,:nOrb1) = phase * reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          ! Hermitian symmetry on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = conjg(sqrTmp(ii+1:nOrb2,ii))
            end do
          end if
          C(iC:jC,:) = C(iC:jC,:) + alphaTmp*matmul(sqrTmp(:nOrb2,:nOrb1),B(iB:jB,:))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(iB:jB, :) = C(iB:jB,:)&
                & + alphaTmp * matmul(transpose(conjg(sqrTmp(:nOrb2,:nOrb1))),B(iC:jC,:))
          end if

        end do
      end do
      !$OMP END PARALLEL DO

    case ("R", "r")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC,iVec,&
      !$OMP &phase) REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          iVec = iCellVec(iAtom2)
          phase = exp(imag * dot_product(kPoint2p, cellVec(:, iVec)))
          sqrTmp(:nOrb2,:nOrb1) = phase * reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          ! Hermitian symmetry on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = conjg(sqrTmp(ii+1:nOrb2,ii))
            end do
          end if
          C(:,iC:jC) = C(:,iC:jC)&
              & + alphaTmp * matmul(B(:,iB:jB),transpose(conjg(sqrTmp(:nOrb2,:nOrb1))))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(:,iB:jB) = C(:,iB:jB) + alphaTmp*matmul(B(:,iC:jC),sqrTmp(:nOrb2,:nOrb1))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    end select

  end subroutine symm_kpt


  !> Sparse matrix at specified k-point with dense complex matrix as a symm operation, using
  !> specified boundary conditions. symmetric matrix on 'l'eft or 'r'ight , where A is symmetric and
  !> sparse and B is general.
  subroutine symm_bc_kpt(C, side, A, B, bcs, iNeighbour, nNeighbour, kPoint, iCellVec, cellVec,&
      & img2CentCell, iSparseStart, iAtomStart, coords, species, orb, alpha, beta)

    !> Dense matrix on return
    complex(dp), intent(inout):: C(:,:)

    !> Sparse matrix
    real(dp), intent(in) :: A(:)

    !> Side which is the sparse matrix. SIDE = 'L' or 'l' C := alpha*A*B + beta*C; SIDE = 'R' or 'r'
    !> C := alpha*B*A + beta*C
    character, intent(in) :: side

    !> Dense matrix on entry
    complex(dp), intent(in) :: B(:,:)

    !> Boundary conditions on the system
    type(TBoundaryConditions), intent(in) :: bcs

    !> Atom neighbour list
    integer, intent(in) :: iNeighbour(0:,:)

    !> Number of neighbours for atoms
    integer, intent(in) :: nNeighbour(:)

    !> Relative coordinates of the k-point where the sparse matrix should be unfolded.
    real(dp), intent(in) :: kPoint(:)

    !> Index of the cell translation vector for each atom.
    integer, intent(in) :: iCellVec(:)

    !> Relative coordinates of the cell translation vectors.
    real(dp), intent(in) :: cellVec(:, :)

    !> Image to central cell mapping
    integer, intent(in) :: img2CentCell(:)

    !> Sparse indexing
    integer, intent(in) :: iSparseStart(0:,:)

    !> Dense indexing
    integer, intent(in) :: iAtomStart(:)

    !> Coordinates of all atoms
    real(dp), intent(in) :: coords(:,:)

    !> Species of each atom
    integer, intent(in) :: species(:)

    !> data type for atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Scaling factor for A * B
    complex(dp), optional, intent(in) :: alpha

    !> Scaling factor for incoming C
    complex(dp), optional, intent(in) :: beta

    integer :: iOrig, iB, iC, jB, jC, iNeigh, nAtom, iAtom1, iAtom2, iAtom2f, nOrb1, nOrb2, ii, iVec
    complex(dp) :: sqrTmp(orb%mOrb, orb%mOrb), alphaTmp, phase
    real(dp) :: kPoint2p(2)




    nAtom = size(iAtomStart)-1

    kPoint2p(:) = 2.0_dp * pi * kPoint

    if (present(alpha)) then
      alphaTmp = alpha
    else
      alphaTmp = 1.0_dp
    end if

    if (present(beta)) then
      C(:,:) = beta * C
    else
      C(:,:) = 0.0_dp
    end if

    select case(side)

    case ("L", "l")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC,iVec,&
      !$OMP &phase) REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          iVec = iCellVec(iAtom2)
          phase = exp(imag * dot_product(kPoint2p, cellVec(:, iVec)))
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
          sqrTmp(:nOrb2,:nOrb1) = phase * sqrTmp
          ! Hermitian symmetry on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = conjg(sqrTmp(ii+1:nOrb2,ii))
            end do
          end if
          C(iC:jC,:) = C(iC:jC,:) + alphaTmp*matmul(sqrTmp(:nOrb2,:nOrb1),B(iB:jB,:))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(iB:jB, :) = C(iB:jB,:)&
                & + alphaTmp * matmul(transpose(conjg(sqrTmp(:nOrb2,:nOrb1))),B(iC:jC,:))
          end if

        end do
      end do
      !$OMP END PARALLEL DO

    case ("R", "r")

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP &PRIVATE(nOrb1,iB,jB,iNeigh,sqrTmp,iAtom2,iAtom2f,nOrb2,iOrig,ii,iC,jC,iVec,&
      !$OMP &phase) REDUCTION(+:C) SCHEDULE(RUNTIME)
      do iAtom1 = 1, nAtom
        iB = iAtomStart(iAtom1)
        jB = iAtomStart(iAtom1 + 1) - 1
        nOrb1 = jB - iB + 1
        do iNeigh = 0, nNeighbour(iAtom1)
          iOrig = iSparseStart(iNeigh,iAtom1) + 1
          sqrTmp(:,:) = 0.0_dp
          iAtom2 = iNeighbour(iNeigh, iAtom1)
          iAtom2f = img2CentCell(iAtom2)
          iC = iAtomStart(iAtom2f)
          jC = iAtomStart(iAtom2f + 1) - 1
          nOrb2 = jC - iC + 1
          iVec = iCellVec(iAtom2)
          phase = exp(imag * dot_product(kPoint2p, cellVec(:, iVec)))
          sqrTmp(:nOrb2,:nOrb1) = reshape(A(iOrig:iOrig+nOrb2*nOrb1-1), [nOrb2,nOrb1])
          sqrTmp(:nOrb2,:nOrb1) = phase * sqrTmp
          call bcs%foldInDiatomicBlock(sqrTmp, iAtom1, iAtom2, coords, species, img2centCell, orb)
          ! Hermitian symmetry on-diagonal blocks just in case
          if (iAtom1 == iAtom2f) then
            do ii = 1, nOrb1
              sqrTmp(ii,ii+1:nOrb2) = conjg(sqrTmp(ii+1:nOrb2,ii))
            end do
          end if
          C(:,iC:jC) = C(:,iC:jC)&
              & + alphaTmp * matmul(B(:,iB:jB),transpose(conjg(sqrTmp(:nOrb2,:nOrb1))))
          ! other triangle due to symmetry of matrix
          if (iAtom1 /= iAtom2f) then
            C(:,iB:jB) = C(:,iB:jB) + alphaTmp*matmul(B(:,iC:jC),sqrTmp(:nOrb2,:nOrb1))
          end if
        end do
      end do
      !$OMP END PARALLEL DO

    end select

  end subroutine symm_bc_kpt



  !> Re-distributes data for square matrices between BLACS block cyclic data and whole global rows
  !> on each processor
  subroutine sqr2rows_real(square, row, denseDesc, blacsEnv)

    !> Real matrix in block cyclic, last index over spin/kpts
    real(dp), allocatable, intent(in) :: square(:,:,:)

    !> Real matrix with individual rows on each processor, last index over spin/kpts
    real(dp), allocatable, intent(inout) :: row(:,:,:)

    !> Descriptors for dense matrices
    type(TDenseDescr), intent(in) :: denseDesc

    !> BLACS environment and information on grids
    type(TBlacsEnv), intent(in) :: blacsEnv

    integer :: iKS



    if (.not.allocated(square)) then
      return
    end if





    do iKS = 1, size(square, dim=3)
      call blacsfx_gemr2d(blacsEnv%nn, blacsEnv%nn, square(:,:,iKS), 1, 1,&
          & denseDesc%blacsOrbSqr, row(:,:,iKS), 1, 1, denseDesc%blacsColumnSqr,&
          & blacsEnv%orbitalGrid%ctxt)
    end do

  end subroutine sqr2rows_real


  !> Re-distributes data for square matrices between BLACS whole global rows on each processor and
  !> block cyclic data
  subroutine rows2sqr_real(row, square, denseDesc, blacsEnv)

    !> Real matrix with individual rows on each processor, last index over spin/kpts
    real(dp), allocatable, intent(in) :: row(:,:,:)

    !> Real matrix in block cyclic, last index over spin/kpts
    real(dp), allocatable, intent(inout) :: square(:,:,:)

    !> Descriptors for dense matrices
    type(TDenseDescr), intent(in) :: denseDesc

    !> BLACS environment and information on grids
    type(TBlacsEnv), intent(in) :: blacsEnv

    integer :: iKS



    if (.not.allocated(row)) then
      return
    end if





    do iKS = 1, size(square, dim=3)
      call blacsfx_gemr2d(blacsEnv%nn, blacsEnv%nn, row(:,:,iKS), 1, 1,&
          & denseDesc%blacsColumnSqr, square(:,:,iKS), 1, 1, denseDesc%blacsOrbSqr,&
          & blacsEnv%orbitalGrid%ctxt)
    end do

  end subroutine rows2sqr_real


  !> Re-distributes data for square matrices between BLACS block cyclic data and whole global rows
  !> on each processor
  subroutine sqr2rows_complex(square, row, denseDesc, blacsEnv)

    !> Real matrix in block cyclic, last index over spin/kpts
    complex(dp), allocatable, intent(in) :: square(:,:,:)

    !> Real matrix with individual rows on each processor, last index over spin/kpts
    complex(dp), allocatable, intent(inout) :: row(:,:,:)

    !> Descriptors for dense matrices
    type(TDenseDescr), intent(in) :: denseDesc

    !> BLACS environment and information on grids
    type(TBlacsEnv), intent(in) :: blacsEnv

    integer :: iKS



    if (.not.allocated(square)) then
      return
    end if





    do iKS = 1, size(square, dim=3)
      call blacsfx_gemr2d(blacsEnv%nn, blacsEnv%nn, square(:,:,iKS), 1, 1,&
          & denseDesc%blacsOrbSqr, row(:,:,iKS), 1, 1, denseDesc%blacsColumnSqr,&
          & blacsEnv%orbitalGrid%ctxt)
    end do

  end subroutine sqr2rows_complex


  !> Re-distributes data for square matrices between BLACS whole global rows on each processor and
  !> block cyclic data
  subroutine rows2sqr_complex(row, square, denseDesc, blacsEnv)

    !> Real matrix with individual rows on each processor, last index over spin/kpts
    complex(dp), allocatable, intent(in) :: row(:,:,:)

    !> Real matrix in block cyclic, last index over spin/kpts
    complex(dp), allocatable, intent(inout) :: square(:,:,:)

    !> Descriptors for dense matrices
    type(TDenseDescr), intent(in) :: denseDesc

    !> BLACS environment and information on grids
    type(TBlacsEnv), intent(in) :: blacsEnv

    integer :: iKS



    if (.not.allocated(row)) then
      return
    end if





    do iKS = 1, size(square, dim=3)
      call blacsfx_gemr2d(blacsEnv%nn, blacsEnv%nn, row(:,:,iKS), 1, 1,&
          & denseDesc%blacsColumnSqr, square(:,:,iKS), 1, 1, denseDesc%blacsOrbSqr,&
          & blacsEnv%orbitalGrid%ctxt)
    end do

  end subroutine rows2sqr_complex



end module dftbp_math_sparseblas
