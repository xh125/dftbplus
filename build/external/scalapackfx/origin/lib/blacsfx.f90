
!> Contains wrapper for the BLACS library
module blacsfx_module
  use scalapackfx_common_module
  use blacsgrid_module
  use blacs_module
  implicit none
  private

  ! Public names.
  public :: DLEN_, DT_, CTXT_, M_, N_, MB_, NB_, RSRC_, CSRC_, LLD_
  public :: blacsgrid
  public :: blacsfx_gebs, blacsfx_gebr
  public :: blacsfx_gesd, blacsfx_gerv
  public :: blacsfx_gsum
  public :: blacsfx_gemr2d
  public :: blacsfx_barrier
  public :: blacsfx_pinfo, blacsfx_pcoord, blacsfx_pnum, blacsfx_exit


  interface blacsfx_gebs
        module procedure blacsfx_gebs_i0
        module procedure blacsfx_gebs_s0
        module procedure blacsfx_gebs_d0
        module procedure blacsfx_gebs_c0
        module procedure blacsfx_gebs_z0
        module procedure blacsfx_gebs_i1
        module procedure blacsfx_gebs_s1
        module procedure blacsfx_gebs_d1
        module procedure blacsfx_gebs_c1
        module procedure blacsfx_gebs_z1
        module procedure blacsfx_gebs_i2
        module procedure blacsfx_gebs_s2
        module procedure blacsfx_gebs_d2
        module procedure blacsfx_gebs_c2
        module procedure blacsfx_gebs_z2
  end interface blacsfx_gebs

  interface blacsfx_gebr
        module procedure blacsfx_gebr_i0
        module procedure blacsfx_gebr_s0
        module procedure blacsfx_gebr_d0
        module procedure blacsfx_gebr_c0
        module procedure blacsfx_gebr_z0
        module procedure blacsfx_gebr_i1
        module procedure blacsfx_gebr_s1
        module procedure blacsfx_gebr_d1
        module procedure blacsfx_gebr_c1
        module procedure blacsfx_gebr_z1
        module procedure blacsfx_gebr_i2
        module procedure blacsfx_gebr_s2
        module procedure blacsfx_gebr_d2
        module procedure blacsfx_gebr_c2
        module procedure blacsfx_gebr_z2
  end interface blacsfx_gebr

  interface blacsfx_gesd
        module procedure blacsfx_gesd_i0
        module procedure blacsfx_gesd_s0
        module procedure blacsfx_gesd_d0
        module procedure blacsfx_gesd_c0
        module procedure blacsfx_gesd_z0
        module procedure blacsfx_gesd_i1
        module procedure blacsfx_gesd_s1
        module procedure blacsfx_gesd_d1
        module procedure blacsfx_gesd_c1
        module procedure blacsfx_gesd_z1
        module procedure blacsfx_gesd_i2
        module procedure blacsfx_gesd_s2
        module procedure blacsfx_gesd_d2
        module procedure blacsfx_gesd_c2
        module procedure blacsfx_gesd_z2
  end interface blacsfx_gesd

  interface blacsfx_gerv
        module procedure blacsfx_gerv_i0
        module procedure blacsfx_gerv_s0
        module procedure blacsfx_gerv_d0
        module procedure blacsfx_gerv_c0
        module procedure blacsfx_gerv_z0
        module procedure blacsfx_gerv_i1
        module procedure blacsfx_gerv_s1
        module procedure blacsfx_gerv_d1
        module procedure blacsfx_gerv_c1
        module procedure blacsfx_gerv_z1
        module procedure blacsfx_gerv_i2
        module procedure blacsfx_gerv_s2
        module procedure blacsfx_gerv_d2
        module procedure blacsfx_gerv_c2
        module procedure blacsfx_gerv_z2
  end interface blacsfx_gerv

  interface blacsfx_gsum
        module procedure blacsfx_gsum_i0
        module procedure blacsfx_gsum_s0
        module procedure blacsfx_gsum_d0
        module procedure blacsfx_gsum_c0
        module procedure blacsfx_gsum_z0
        module procedure blacsfx_gsum_i1
        module procedure blacsfx_gsum_s1
        module procedure blacsfx_gsum_d1
        module procedure blacsfx_gsum_c1
        module procedure blacsfx_gsum_z1
        module procedure blacsfx_gsum_i2
        module procedure blacsfx_gsum_s2
        module procedure blacsfx_gsum_d2
        module procedure blacsfx_gsum_c2
        module procedure blacsfx_gsum_z2
  end interface blacsfx_gsum

  interface blacsfx_gemr2d
        module procedure blacsfx_gemr2d_i
        module procedure blacsfx_gemr2d_s
        module procedure blacsfx_gemr2d_d
        module procedure blacsfx_gemr2d_c
        module procedure blacsfx_gemr2d_z
  end interface blacsfx_gemr2d

contains










































  !> Starts broadcast (integer, rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_i0(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(in), target :: aa
    character, intent(in), optional :: scope, top

    integer :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_i0


  !> Receives broadcast (integer, rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_i0(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(out), target :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    integer :: buffer(1,1)

    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gebr_i0


  !> Sends general rectangular matrix to destination process
  !! (integer, rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_i0(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in), target :: aa
    integer, intent(in) :: rdest, cdest

    integer :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_i0


  !> Receives general rectangular matrix from source process (integer, rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_i0(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(out),target :: aa
    integer, intent(in), optional :: rsrc, csrc

    integer :: buffer(1,1)

    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gerv_i0


  !> Performs element-wise summation(integer, rank 0).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Scalar to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_i0(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(inout) :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    integer :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)
    aa = buffer(1, 1)

  end subroutine blacsfx_gsum_i0


  !> Starts broadcast (real(sp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_s0(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(in), target :: aa
    character, intent(in), optional :: scope, top

    real(sp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_s0


  !> Receives broadcast (real(sp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_s0(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(out), target :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    real(sp) :: buffer(1,1)

    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gebr_s0


  !> Sends general rectangular matrix to destination process
  !! (real(sp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_s0(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    real(sp), intent(in), target :: aa
    integer, intent(in) :: rdest, cdest

    real(sp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_s0


  !> Receives general rectangular matrix from source process (real(sp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_s0(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    real(sp), intent(out),target :: aa
    integer, intent(in), optional :: rsrc, csrc

    real(sp) :: buffer(1,1)

    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gerv_s0


  !> Performs element-wise summation(real(sp), rank 0).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Scalar to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_s0(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(inout) :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    real(sp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)
    aa = buffer(1, 1)

  end subroutine blacsfx_gsum_s0


  !> Starts broadcast (real(dp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_d0(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(in), target :: aa
    character, intent(in), optional :: scope, top

    real(dp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_d0


  !> Receives broadcast (real(dp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_d0(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(out), target :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    real(dp) :: buffer(1,1)

    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gebr_d0


  !> Sends general rectangular matrix to destination process
  !! (real(dp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_d0(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    real(dp), intent(in), target :: aa
    integer, intent(in) :: rdest, cdest

    real(dp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_d0


  !> Receives general rectangular matrix from source process (real(dp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_d0(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    real(dp), intent(out),target :: aa
    integer, intent(in), optional :: rsrc, csrc

    real(dp) :: buffer(1,1)

    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gerv_d0


  !> Performs element-wise summation(real(dp), rank 0).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Scalar to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_d0(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(inout) :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    real(dp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)
    aa = buffer(1, 1)

  end subroutine blacsfx_gsum_d0


  !> Starts broadcast (complex(sp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_c0(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(in), target :: aa
    character, intent(in), optional :: scope, top

    complex(sp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_c0


  !> Receives broadcast (complex(sp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_c0(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(out), target :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    complex(sp) :: buffer(1,1)

    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gebr_c0


  !> Sends general rectangular matrix to destination process
  !! (complex(sp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_c0(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(in), target :: aa
    integer, intent(in) :: rdest, cdest

    complex(sp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_c0


  !> Receives general rectangular matrix from source process (complex(sp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_c0(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(out),target :: aa
    integer, intent(in), optional :: rsrc, csrc

    complex(sp) :: buffer(1,1)

    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gerv_c0


  !> Performs element-wise summation(complex(sp), rank 0).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Scalar to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_c0(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(inout) :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    complex(sp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)
    aa = buffer(1, 1)

  end subroutine blacsfx_gsum_c0


  !> Starts broadcast (complex(dp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_z0(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(in), target :: aa
    character, intent(in), optional :: scope, top

    complex(dp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_z0


  !> Receives broadcast (complex(dp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_z0(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(out), target :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    complex(dp) :: buffer(1,1)

    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gebr_z0


  !> Sends general rectangular matrix to destination process
  !! (complex(dp), rank 0).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_z0(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(in), target :: aa
    integer, intent(in) :: rdest, cdest

    complex(dp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_z0


  !> Receives general rectangular matrix from source process (complex(dp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_z0(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(out),target :: aa
    integer, intent(in), optional :: rsrc, csrc

    complex(dp) :: buffer(1,1)

    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)
    aa = buffer(1,1)

  end subroutine blacsfx_gerv_z0


  !> Performs element-wise summation(complex(dp), rank 0).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Scalar to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_z0(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(inout) :: aa
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    complex(dp) :: buffer(1,1)

    buffer(1,1) = aa
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)
    aa = buffer(1, 1)

  end subroutine blacsfx_gsum_z0

  !> Starts broadcast (integer, rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_i1(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(in), target :: aa(:)
    character, intent(in), optional :: scope, top

    integer, pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_i1


  !> Receives broadcast (integer, rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_i1(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(out), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    integer, pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)

  end subroutine blacsfx_gebr_i1


  !> Sends general rectangular matrix to destination process
  !! (integer, rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_i1(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in), target :: aa(:)
    integer, intent(in) :: rdest, cdest

    integer, pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_i1


  !> Receives general rectangular matrix from source process (integer, rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_i1(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(out),target :: aa(:)
    integer, intent(in), optional :: rsrc, csrc

    integer, pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)

  end subroutine blacsfx_gerv_i1


  !> Performs element-wise summation(i1, rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Vector to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_i1(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(inout), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    integer, pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)

  end subroutine blacsfx_gsum_i1

  !> Starts broadcast (real(sp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_s1(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(in), target :: aa(:)
    character, intent(in), optional :: scope, top

    real(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_s1


  !> Receives broadcast (real(sp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_s1(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(out), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    real(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)

  end subroutine blacsfx_gebr_s1


  !> Sends general rectangular matrix to destination process
  !! (real(sp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_s1(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    real(sp), intent(in), target :: aa(:)
    integer, intent(in) :: rdest, cdest

    real(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_s1


  !> Receives general rectangular matrix from source process (real(sp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_s1(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    real(sp), intent(out),target :: aa(:)
    integer, intent(in), optional :: rsrc, csrc

    real(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)

  end subroutine blacsfx_gerv_s1


  !> Performs element-wise summation(s1, rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Vector to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_s1(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(inout), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    real(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)

  end subroutine blacsfx_gsum_s1

  !> Starts broadcast (real(dp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_d1(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(in), target :: aa(:)
    character, intent(in), optional :: scope, top

    real(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_d1


  !> Receives broadcast (real(dp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_d1(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(out), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    real(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)

  end subroutine blacsfx_gebr_d1


  !> Sends general rectangular matrix to destination process
  !! (real(dp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_d1(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    real(dp), intent(in), target :: aa(:)
    integer, intent(in) :: rdest, cdest

    real(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_d1


  !> Receives general rectangular matrix from source process (real(dp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_d1(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    real(dp), intent(out),target :: aa(:)
    integer, intent(in), optional :: rsrc, csrc

    real(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)

  end subroutine blacsfx_gerv_d1


  !> Performs element-wise summation(d1, rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Vector to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_d1(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(inout), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    real(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)

  end subroutine blacsfx_gsum_d1

  !> Starts broadcast (complex(sp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_c1(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(in), target :: aa(:)
    character, intent(in), optional :: scope, top

    complex(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_c1


  !> Receives broadcast (complex(sp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_c1(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(out), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    complex(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)

  end subroutine blacsfx_gebr_c1


  !> Sends general rectangular matrix to destination process
  !! (complex(sp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_c1(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(in), target :: aa(:)
    integer, intent(in) :: rdest, cdest

    complex(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_c1


  !> Receives general rectangular matrix from source process (complex(sp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_c1(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(out),target :: aa(:)
    integer, intent(in), optional :: rsrc, csrc

    complex(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)

  end subroutine blacsfx_gerv_c1


  !> Performs element-wise summation(c1, rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Vector to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_c1(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(inout), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    complex(sp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)

  end subroutine blacsfx_gsum_c1

  !> Starts broadcast (complex(dp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).
  subroutine blacsfx_gebs_z1(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(in), target :: aa(:)
    character, intent(in), optional :: scope, top

    complex(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebs(mygrid, buffer, scope, top)

  end subroutine blacsfx_gebs_z1


  !> Receives broadcast (complex(dp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_z1(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(out), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    complex(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gebr(mygrid, buffer, scope, top, rsrc, csrc)

  end subroutine blacsfx_gebr_z1


  !> Sends general rectangular matrix to destination process
  !! (complex(dp), rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_z1(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(in), target :: aa(:)
    integer, intent(in) :: rdest, cdest

    complex(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gesd(mygrid, buffer, rdest, cdest)

  end subroutine blacsfx_gesd_z1


  !> Receives general rectangular matrix from source process (complex(dp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_z1(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(out),target :: aa(:)
    integer, intent(in), optional :: rsrc, csrc

    complex(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gerv(mygrid, buffer, rsrc, csrc)

  end subroutine blacsfx_gerv_z1


  !> Performs element-wise summation(z1, rank 1).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Vector to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_z1(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(inout), target :: aa(:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    complex(dp), pointer :: buffer(:,:)

    buffer(1:size(aa), 1:1) => aa(1:size(aa))
    call blacsfx_gsum(mygrid, buffer, scope, top, rdest, cdest)

  end subroutine blacsfx_gsum_z1

  !> Starts broadcast (integer, rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).


  subroutine blacsfx_gebs_i2(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: aa(:,:)
    character, intent(in), optional :: scope, top

    character :: scope0, top0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = 'A'
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if

    call gebs2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
        & aa, size(aa, dim=1))

  end subroutine blacsfx_gebs_i2


  !> Receives broadcast (integer, rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_i2(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(out) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    character :: scope0, top0
    integer :: rsrc0, csrc0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if

    call gebr2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gebr_i2


  !> Sends general rectangular matrix to destination process
  !! (integer, rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_i2(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: aa(:,:)
    integer, intent(in) :: rdest, cdest

    call gesd2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa,&
      & size(aa, dim=1), rdest, cdest)

  end subroutine blacsfx_gesd_i2


  !> Receives general rectangular matrix from source process (integer, rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_i2(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(out) :: aa(:,:)
    integer, intent(in), optional :: rsrc, csrc

    integer :: rsrc0, csrc0

  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if
    call gerv2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa, &
      & size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gerv_i2


  !> Performs element-wise summation(i2, rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_i2(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    integer, intent(inout) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    character :: scope0, top0
    integer :: rdest0, cdest0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rdest)) then
    rdest0 = rdest
  else
    rdest0 = mygrid%leadrow
  end if
  if (present(cdest)) then
    cdest0 = cdest
  else
    cdest0 = mygrid%leadcol
  end if
    call gsum2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rdest0, cdest0)

  end subroutine blacsfx_gsum_i2

  !> Starts broadcast (real(sp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).


  subroutine blacsfx_gebs_s2(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(in) :: aa(:,:)
    character, intent(in), optional :: scope, top

    character :: scope0, top0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = 'A'
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if

    call gebs2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
        & aa, size(aa, dim=1))

  end subroutine blacsfx_gebs_s2


  !> Receives broadcast (real(sp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_s2(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(out) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    character :: scope0, top0
    integer :: rsrc0, csrc0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if

    call gebr2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gebr_s2


  !> Sends general rectangular matrix to destination process
  !! (real(sp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_s2(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    real(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: rdest, cdest

    call gesd2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa,&
      & size(aa, dim=1), rdest, cdest)

  end subroutine blacsfx_gesd_s2


  !> Receives general rectangular matrix from source process (real(sp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_s2(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    real(sp), intent(out) :: aa(:,:)
    integer, intent(in), optional :: rsrc, csrc

    integer :: rsrc0, csrc0

  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if
    call gerv2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa, &
      & size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gerv_s2


  !> Performs element-wise summation(s2, rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_s2(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    real(sp), intent(inout) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    character :: scope0, top0
    integer :: rdest0, cdest0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rdest)) then
    rdest0 = rdest
  else
    rdest0 = mygrid%leadrow
  end if
  if (present(cdest)) then
    cdest0 = cdest
  else
    cdest0 = mygrid%leadcol
  end if
    call gsum2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rdest0, cdest0)

  end subroutine blacsfx_gsum_s2

  !> Starts broadcast (real(dp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).


  subroutine blacsfx_gebs_d2(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(in) :: aa(:,:)
    character, intent(in), optional :: scope, top

    character :: scope0, top0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = 'A'
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if

    call gebs2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
        & aa, size(aa, dim=1))

  end subroutine blacsfx_gebs_d2


  !> Receives broadcast (real(dp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_d2(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(out) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    character :: scope0, top0
    integer :: rsrc0, csrc0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if

    call gebr2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gebr_d2


  !> Sends general rectangular matrix to destination process
  !! (real(dp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_d2(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    real(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: rdest, cdest

    call gesd2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa,&
      & size(aa, dim=1), rdest, cdest)

  end subroutine blacsfx_gesd_d2


  !> Receives general rectangular matrix from source process (real(dp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_d2(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    real(dp), intent(out) :: aa(:,:)
    integer, intent(in), optional :: rsrc, csrc

    integer :: rsrc0, csrc0

  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if
    call gerv2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa, &
      & size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gerv_d2


  !> Performs element-wise summation(d2, rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_d2(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    real(dp), intent(inout) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    character :: scope0, top0
    integer :: rdest0, cdest0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rdest)) then
    rdest0 = rdest
  else
    rdest0 = mygrid%leadrow
  end if
  if (present(cdest)) then
    cdest0 = cdest
  else
    cdest0 = mygrid%leadcol
  end if
    call gsum2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rdest0, cdest0)

  end subroutine blacsfx_gsum_d2

  !> Starts broadcast (complex(sp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).


  subroutine blacsfx_gebs_c2(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(in) :: aa(:,:)
    character, intent(in), optional :: scope, top

    character :: scope0, top0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = 'A'
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if

    call gebs2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
        & aa, size(aa, dim=1))

  end subroutine blacsfx_gebs_c2


  !> Receives broadcast (complex(sp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_c2(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(out) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    character :: scope0, top0
    integer :: rsrc0, csrc0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if

    call gebr2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gebr_c2


  !> Sends general rectangular matrix to destination process
  !! (complex(sp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_c2(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(in) :: aa(:,:)
    integer, intent(in) :: rdest, cdest

    call gesd2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa,&
      & size(aa, dim=1), rdest, cdest)

  end subroutine blacsfx_gesd_c2


  !> Receives general rectangular matrix from source process (complex(sp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_c2(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(out) :: aa(:,:)
    integer, intent(in), optional :: rsrc, csrc

    integer :: rsrc0, csrc0

  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if
    call gerv2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa, &
      & size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gerv_c2


  !> Performs element-wise summation(c2, rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_c2(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    complex(sp), intent(inout) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    character :: scope0, top0
    integer :: rdest0, cdest0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rdest)) then
    rdest0 = rdest
  else
    rdest0 = mygrid%leadrow
  end if
  if (present(cdest)) then
    cdest0 = cdest
  else
    cdest0 = mygrid%leadcol
  end if
    call gsum2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rdest0, cdest0)

  end subroutine blacsfx_gsum_c2

  !> Starts broadcast (complex(dp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to broadcast.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default " ").
  !! \see BLACS documentation (routine ?gebs2d).


  subroutine blacsfx_gebs_z2(mygrid, aa, scope, top)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(in) :: aa(:,:)
    character, intent(in), optional :: scope, top

    character :: scope0, top0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = 'A'
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if

    call gebs2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
        & aa, size(aa, dim=1))

  end subroutine blacsfx_gebs_z2


  !> Receives broadcast (complex(dp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to receive.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rsrc  Row of the source (default: row of lead process).
  !! \param csrc  Column of the source (default: column of lead process).
  !! \see BLACS documentation (routine ?gebr2d).
  subroutine blacsfx_gebr_z2(mygrid, aa, scope, top, rsrc, csrc)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(out) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rsrc, csrc

    character :: scope0, top0
    integer :: rsrc0, csrc0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if

    call gebr2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gebr_z2


  !> Sends general rectangular matrix to destination process
  !! (complex(dp), rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Object to send.
  !! \param rdest  Row of the destination process.
  !! \param cdest  Column of the destination proces.
  !! \see BLACS documentation (routine ?gesd2d).
  subroutine blacsfx_gesd_z2(mygrid, aa, rdest, cdest)
    type(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(in) :: aa(:,:)
    integer, intent(in) :: rdest, cdest

    call gesd2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa,&
      & size(aa, dim=1), rdest, cdest)

  end subroutine blacsfx_gesd_z2


  !> Receives general rectangular matrix from source process (complex(dp), rank 2).
  !! \param mygrid  BLACS descriptor
  !! \param aa  Object to receive.
  !! \param rdest  Row of the destination process (default: lead row).
  !! \param cdest  Column of the destination proces (default: lead col).
  !! \see BLACS documentation (routine ?gerv2d).
  subroutine blacsfx_gerv_z2(mygrid, aa, rsrc, csrc)
    type(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(out) :: aa(:,:)
    integer, intent(in), optional :: rsrc, csrc

    integer :: rsrc0, csrc0

  if (present(rsrc)) then
    rsrc0 = rsrc
  else
    rsrc0 = mygrid%leadrow
  end if
  if (present(csrc)) then
    csrc0 = csrc
  else
    csrc0 = mygrid%leadcol
  end if
    call gerv2d(mygrid%ctxt, size(aa, dim=1), size(aa, dim=2), aa, &
      & size(aa, dim=1), rsrc0, csrc0)

  end subroutine blacsfx_gerv_z2


  !> Performs element-wise summation(z2, rank 2).
  !! \param mygrid  BLACS descriptor.
  !! \param aa  Matrix to sum up.
  !! \param scope  Scope of the broadcast (default: "A").
  !! \param top  Topology of the broadcast (default: " ").
  !! \param rdest  Row of the destination (default: row of lead process).
  !! \param rcol  Column of the destination (default: column of lead process).
  !! \see BLACS documentation (routine ?gsum2d).
  subroutine blacsfx_gsum_z2(mygrid, aa, scope, top, rdest, cdest)
    class(blacsgrid), intent(in) :: mygrid
    complex(dp), intent(inout) :: aa(:,:)
    character, intent(in), optional :: scope, top
    integer, intent(in), optional :: rdest, cdest

    character :: scope0, top0
    integer :: rdest0, cdest0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
  if (present(top)) then
    top0 = top
  else
    top0 = " "
  end if
  if (present(rdest)) then
    rdest0 = rdest
  else
    rdest0 = mygrid%leadrow
  end if
  if (present(cdest)) then
    cdest0 = cdest
  else
    cdest0 = mygrid%leadcol
  end if
    call gsum2d(mygrid%ctxt, scope0, top0, size(aa, dim=1), size(aa, dim=2),&
      & aa, size(aa, dim=1), rdest0, cdest0)

  end subroutine blacsfx_gsum_z2



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Matrix copy/redistribution
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  !> Copies/redistributes matrix (integer).
  !! \param mm number of rows of AA to copy.
  !! \param mm number of columns of AA to copy.
  !! \param aa distributed matrix AA from which to copy.
  !! \param ia first row of AA from which to copy.
  !! \param ja first column of AA from which to copy.
  !! \param descA BLACS descriptor for source matrix.
  !! \param bb distributed matrix BB into which data is copied.
  !! \param ib first row of BB at which to copy.
  !! \param jb first column of BB at which to copy.
  !! \param descB BLACS descriptor for destination matrix.
  !! \param ictxt Context for for union of all processes holding A or B
  !! \see BLACS documentation (routine p?gemr2d).
  subroutine blacsfx_gemr2d_i(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: descA(DLEN_)
    integer, intent(in) :: descB(DLEN_)
    integer, intent(in) :: aa(:,:)
    integer, intent(inout) :: bb(:,:)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb, ictxt

    ! AA and BB should be references to starting corner of matrices
    call gemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine blacsfx_gemr2d_i


  !> Copies/redistributes matrix (real(sp)).
  !! \param mm number of rows of AA to copy.
  !! \param mm number of columns of AA to copy.
  !! \param aa distributed matrix AA from which to copy.
  !! \param ia first row of AA from which to copy.
  !! \param ja first column of AA from which to copy.
  !! \param descA BLACS descriptor for source matrix.
  !! \param bb distributed matrix BB into which data is copied.
  !! \param ib first row of BB at which to copy.
  !! \param jb first column of BB at which to copy.
  !! \param descB BLACS descriptor for destination matrix.
  !! \param ictxt Context for for union of all processes holding A or B
  !! \see BLACS documentation (routine p?gemr2d).
  subroutine blacsfx_gemr2d_s(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: descA(DLEN_)
    integer, intent(in) :: descB(DLEN_)
    real(sp), intent(in) :: aa(:,:)
    real(sp), intent(inout) :: bb(:,:)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb, ictxt

    ! AA and BB should be references to starting corner of matrices
    call gemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine blacsfx_gemr2d_s


  !> Copies/redistributes matrix (real(dp)).
  !! \param mm number of rows of AA to copy.
  !! \param mm number of columns of AA to copy.
  !! \param aa distributed matrix AA from which to copy.
  !! \param ia first row of AA from which to copy.
  !! \param ja first column of AA from which to copy.
  !! \param descA BLACS descriptor for source matrix.
  !! \param bb distributed matrix BB into which data is copied.
  !! \param ib first row of BB at which to copy.
  !! \param jb first column of BB at which to copy.
  !! \param descB BLACS descriptor for destination matrix.
  !! \param ictxt Context for for union of all processes holding A or B
  !! \see BLACS documentation (routine p?gemr2d).
  subroutine blacsfx_gemr2d_d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: descA(DLEN_)
    integer, intent(in) :: descB(DLEN_)
    real(dp), intent(in) :: aa(:,:)
    real(dp), intent(inout) :: bb(:,:)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb, ictxt

    ! AA and BB should be references to starting corner of matrices
    call gemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine blacsfx_gemr2d_d


  !> Copies/redistributes matrix (complex(sp)).
  !! \param mm number of rows of AA to copy.
  !! \param mm number of columns of AA to copy.
  !! \param aa distributed matrix AA from which to copy.
  !! \param ia first row of AA from which to copy.
  !! \param ja first column of AA from which to copy.
  !! \param descA BLACS descriptor for source matrix.
  !! \param bb distributed matrix BB into which data is copied.
  !! \param ib first row of BB at which to copy.
  !! \param jb first column of BB at which to copy.
  !! \param descB BLACS descriptor for destination matrix.
  !! \param ictxt Context for for union of all processes holding A or B
  !! \see BLACS documentation (routine p?gemr2d).
  subroutine blacsfx_gemr2d_c(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: descA(DLEN_)
    integer, intent(in) :: descB(DLEN_)
    complex(sp), intent(in) :: aa(:,:)
    complex(sp), intent(inout) :: bb(:,:)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb, ictxt

    ! AA and BB should be references to starting corner of matrices
    call gemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine blacsfx_gemr2d_c


  !> Copies/redistributes matrix (complex(dp)).
  !! \param mm number of rows of AA to copy.
  !! \param mm number of columns of AA to copy.
  !! \param aa distributed matrix AA from which to copy.
  !! \param ia first row of AA from which to copy.
  !! \param ja first column of AA from which to copy.
  !! \param descA BLACS descriptor for source matrix.
  !! \param bb distributed matrix BB into which data is copied.
  !! \param ib first row of BB at which to copy.
  !! \param jb first column of BB at which to copy.
  !! \param descB BLACS descriptor for destination matrix.
  !! \param ictxt Context for for union of all processes holding A or B
  !! \see BLACS documentation (routine p?gemr2d).
  subroutine blacsfx_gemr2d_z(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)
    integer, intent(in) :: descA(DLEN_)
    integer, intent(in) :: descB(DLEN_)
    complex(dp), intent(in) :: aa(:,:)
    complex(dp), intent(inout) :: bb(:,:)
    integer, intent(in) :: mm, nn, ia, ja, ib, jb, ictxt

    ! AA and BB should be references to starting corner of matrices
    call gemr2d(mm, nn, aa, ia, ja, descA, bb, ib, jb, descB, ictxt)

  end subroutine blacsfx_gemr2d_z



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Barrier
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Holds up execution of all processes within given scope.
  !!
  !! \param self  BLACS group descriptor
  !! \param scope  Scope of the barrier (default: "A")
  !!
  subroutine blacsfx_barrier(mygrid, scope)
    type(blacsgrid), intent(in) :: mygrid
    character, intent(in), optional :: scope

    character :: scope0

  if (present(scope)) then
    scope0 = scope
  else
    scope0 = "A"
  end if
    call blacs_barrier(mygrid%ctxt, scope0)

  end subroutine blacsfx_barrier

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Grid information
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Delivers process information.
  !!
  !! \param iproc  Id of the process (0 <= iproc < nproc)
  !! \param nproc  Nr. of processes.
  !!
  subroutine blacsfx_pinfo(iproc, nproc)
    integer, intent(out) :: iproc, nproc

    call blacs_pinfo(iproc, nproc)

  end subroutine blacsfx_pinfo


  !> Delivers row and column of a given process in a grid.
  !!
  !! \param mygrid  BLACS grid.
  !! \param iproc  Process of which position should be determined.
  !! \param prow  Row of the process.
  !! \param pcol  Column of the process.
  !!
  subroutine blacsfx_pcoord(mygrid, iproc, prow, pcol)
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: iproc
    integer, intent(out) :: prow, pcol

    call blacs_pcoord(mygrid%ctxt, iproc, prow, pcol)

  end subroutine blacsfx_pcoord


  !> Delivers process number for a given process in the grid.
  !!
  !! \param mygrid BLACS grid.
  !! \param prow  Row of the process.
  !! \param pcol  Column of the process.
  !! \return  Process number (id) of the process with the given coordinates.
  !!
  function blacsfx_pnum(mygrid, prow, pcol) result(pnum)
    type(blacsgrid), intent(in) :: mygrid
    integer, intent(in) :: prow, pcol
    integer :: pnum

    pnum = blacs_pnum(mygrid%ctxt, prow, pcol)

  end function blacsfx_pnum

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Stop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Stops BLACS communication.
  !!
  !! \param keepmpi If set to yes, the MPI framework will kept alive after
  !!     BLACS is switched off (default: .false.)
  !!
  subroutine blacsfx_exit(keepmpi)
    logical, intent(in), optional :: keepmpi

    logical :: keepmpi0

  if (present(keepmpi)) then
    keepmpi0 = keepmpi
  else
    keepmpi0 = .false.
  end if
    if (keepmpi0) then
      call blacs_exit(1)
    else
      call blacs_exit(0)
    end if

  end subroutine blacsfx_exit



end module blacsfx_module
