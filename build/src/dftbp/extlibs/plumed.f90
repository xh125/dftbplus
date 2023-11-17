!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!




!> Exporting the functionality from the plumed library
module dftbp_extlibs_plumed
  use, intrinsic :: iso_c_binding, only : c_int, c_char, c_ptr, c_loc
  use dftbp_common_accuracy, only : dp
  use dftbp_io_message, only : error
  implicit none
  private


  public :: TPlumedCalc, TPlumedCalc_init, TPlumedCalc_final
  public :: withPlumed


  type, bind(c) :: c_plumed
    type(c_ptr) :: cptr
  end type c_plumed


  type :: TPlumedCalc
    private
    type(c_plumed) :: desc
  contains
    private
        procedure :: sendCmdValInt0
        generic, public :: sendCmdVal => sendCmdValInt0
        procedure :: sendCmdPtrInt0
        generic, public :: sendCmdPtr => sendCmdPtrInt0
        procedure :: sendCmdValInt1
        generic, public :: sendCmdVal => sendCmdValInt1
        procedure :: sendCmdPtrInt1
        generic, public :: sendCmdPtr => sendCmdPtrInt1
        procedure :: sendCmdValInt2
        generic, public :: sendCmdVal => sendCmdValInt2
        procedure :: sendCmdPtrInt2
        generic, public :: sendCmdPtr => sendCmdPtrInt2
        procedure :: sendCmdValReal0
        generic, public :: sendCmdVal => sendCmdValReal0
        procedure :: sendCmdPtrReal0
        generic, public :: sendCmdPtr => sendCmdPtrReal0
        procedure :: sendCmdValReal1
        generic, public :: sendCmdVal => sendCmdValReal1
        procedure :: sendCmdPtrReal1
        generic, public :: sendCmdPtr => sendCmdPtrReal1
        procedure :: sendCmdValReal2
        generic, public :: sendCmdVal => sendCmdValReal2
        procedure :: sendCmdPtrReal2
        generic, public :: sendCmdPtr => sendCmdPtrReal2
    procedure :: sendCmdValChar
    generic, public :: sendCmdVal => sendCmdValChar
  end type TPlumedCalc


  ! Explicit interfaces for the C-API provided by PLUMED
  interface

    function plumed_create() result(instance) bind(C, name='plumed_create')
      import :: c_plumed
      type(c_plumed) :: instance
    end function plumed_create

    subroutine plumed_cmd(instance, key, val) bind(C, name='plumed_cmd')
      import :: c_plumed, c_char, c_ptr
      type(c_plumed), value :: instance
      character(kind=c_char), intent(in) :: key(*)
      type(c_ptr), value :: val
    end subroutine plumed_cmd

    subroutine plumed_finalize(instance) bind(C, name='plumed_finalize')
      import :: c_plumed
      type(c_plumed), value :: instance
    end subroutine plumed_finalize

    function plumed_installed(instance) result(installed) bind(C, name='plumed_installed')
      import :: c_plumed, c_int
      type(c_plumed), value :: instance
      integer(c_int) :: installed
    end function plumed_installed

  end interface


  !> Whether package was build with PLUMED support
  logical, parameter :: withPlumed = .false.


contains

  !> Initializes plumed.
  !>
  subroutine TPlumedCalc_init(this)
    type(TPlumedCalc), intent(out) :: this

      call stubError("TPlumedCalc_init")

  end subroutine TPlumedCalc_init


  !> Destroys the PLUMED instance.
  subroutine TPlumedCalc_final(this)
    type(TPlumedCalc), intent(inout) :: this

      call stubError("TPlumedCalc_final")

  end subroutine TPlumedCalc_final



      !> Wrapper for passing a value to a PLUMED instance.
      !>
      !> NOTE: This wrapper should only be used to pass values to PLUMED which are
      !> are immediately COPIED in PLUMED before returning, as the argument may contain
      !> temporary expression
      !>
      subroutine sendCmdValInt0(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character).
        character(len=*, kind=c_char), intent(in) :: key

        !> Value to pass.
        integer, target, intent(in)  :: val

          call stubError("sendCmdValInt0")

      end subroutine sendCmdValInt0


      !> Wrapper for passing a value to a PLUMED instance.
      !>
      !> NOTE: This wrapper should only be used to pass values to PLUMED which are
      !> are immediately COPIED in PLUMED before returning, as the argument may contain
      !> temporary expression
      !>
      subroutine sendCmdValInt1(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character).
        character(len=*, kind=c_char), intent(in) :: key

        !> Value to pass.
        integer, target, intent(in) , contiguous :: val(:)

          call stubError("sendCmdValInt1")

      end subroutine sendCmdValInt1


      !> Wrapper for passing a value to a PLUMED instance.
      !>
      !> NOTE: This wrapper should only be used to pass values to PLUMED which are
      !> are immediately COPIED in PLUMED before returning, as the argument may contain
      !> temporary expression
      !>
      subroutine sendCmdValInt2(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character).
        character(len=*, kind=c_char), intent(in) :: key

        !> Value to pass.
        integer, target, intent(in) , contiguous :: val(:,:)

          call stubError("sendCmdValInt2")

      end subroutine sendCmdValInt2


      !> Wrapper for passing a value to a PLUMED instance.
      !>
      !> NOTE: This wrapper should only be used to pass values to PLUMED which are
      !> are immediately COPIED in PLUMED before returning, as the argument may contain
      !> temporary expression
      !>
      subroutine sendCmdValReal0(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character).
        character(len=*, kind=c_char), intent(in) :: key

        !> Value to pass.
        real(dp), target, intent(in)  :: val

          call stubError("sendCmdValReal0")

      end subroutine sendCmdValReal0


      !> Wrapper for passing a value to a PLUMED instance.
      !>
      !> NOTE: This wrapper should only be used to pass values to PLUMED which are
      !> are immediately COPIED in PLUMED before returning, as the argument may contain
      !> temporary expression
      !>
      subroutine sendCmdValReal1(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character).
        character(len=*, kind=c_char), intent(in) :: key

        !> Value to pass.
        real(dp), target, intent(in) , contiguous :: val(:)

          call stubError("sendCmdValReal1")

      end subroutine sendCmdValReal1


      !> Wrapper for passing a value to a PLUMED instance.
      !>
      !> NOTE: This wrapper should only be used to pass values to PLUMED which are
      !> are immediately COPIED in PLUMED before returning, as the argument may contain
      !> temporary expression
      !>
      subroutine sendCmdValReal2(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character).
        character(len=*, kind=c_char), intent(in) :: key

        !> Value to pass.
        real(dp), target, intent(in) , contiguous :: val(:,:)

          call stubError("sendCmdValReal2")

      end subroutine sendCmdValReal2



  !> Wrapper for passing a value to a PLUMED instance (character version).
  !>
  !> NOTE: This wrapper should only be used to pass values to PLUMED which are
  !> are immediately COPIED in PLUMED before returning, as the argument may contain
  !> temporary expression.
  !>
  subroutine sendCmdValChar(this, key, val)

    !> Instance
    class(TPlumedCalc), intent(inout) :: this

    !> Key (will be automatically extended with the necessary termination character)
    character(len=*, kind=c_char), intent(in) :: key

    !> Value to pass (will be automatically extended with the necessary termination character)
    character(len=*, kind=c_char), intent(in) :: val


      call stubError("sendCmdValChar")

  end subroutine sendCmdValChar



      !> Wrapper for passing a reference to a PLUMED instance.
      !>
      !> NOTE: This wrapper passes the address of the value object. Make sure, the object
      !> exists long enough, that PLUMED can access and eventually modify it when necessary.
      !>
      subroutine sendCmdPtrInt0(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character)
        character(len=*, kind=c_char), intent(in) :: key

        !> Object which should be passed as a reference.
        !> Contains workaround for bug in Intel 19 compiler (pointer => target)
        integer, target, intent(in)  :: val

          call stubError("sendCmdPtrInt0")

      end subroutine sendCmdPtrInt0


      !> Wrapper for passing a reference to a PLUMED instance.
      !>
      !> NOTE: This wrapper passes the address of the value object. Make sure, the object
      !> exists long enough, that PLUMED can access and eventually modify it when necessary.
      !>
      subroutine sendCmdPtrInt1(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character)
        character(len=*, kind=c_char), intent(in) :: key

        !> Object which should be passed as a reference.
        !> Contains workaround for bug in Intel 19 compiler (pointer => target)
        integer, target, intent(in) , contiguous :: val(:)

          call stubError("sendCmdPtrInt1")

      end subroutine sendCmdPtrInt1


      !> Wrapper for passing a reference to a PLUMED instance.
      !>
      !> NOTE: This wrapper passes the address of the value object. Make sure, the object
      !> exists long enough, that PLUMED can access and eventually modify it when necessary.
      !>
      subroutine sendCmdPtrInt2(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character)
        character(len=*, kind=c_char), intent(in) :: key

        !> Object which should be passed as a reference.
        !> Contains workaround for bug in Intel 19 compiler (pointer => target)
        integer, target, intent(in) , contiguous :: val(:,:)

          call stubError("sendCmdPtrInt2")

      end subroutine sendCmdPtrInt2


      !> Wrapper for passing a reference to a PLUMED instance.
      !>
      !> NOTE: This wrapper passes the address of the value object. Make sure, the object
      !> exists long enough, that PLUMED can access and eventually modify it when necessary.
      !>
      subroutine sendCmdPtrReal0(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character)
        character(len=*, kind=c_char), intent(in) :: key

        !> Object which should be passed as a reference.
        !> Contains workaround for bug in Intel 19 compiler (pointer => target)
        real(dp), target, intent(in)  :: val

          call stubError("sendCmdPtrReal0")

      end subroutine sendCmdPtrReal0


      !> Wrapper for passing a reference to a PLUMED instance.
      !>
      !> NOTE: This wrapper passes the address of the value object. Make sure, the object
      !> exists long enough, that PLUMED can access and eventually modify it when necessary.
      !>
      subroutine sendCmdPtrReal1(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character)
        character(len=*, kind=c_char), intent(in) :: key

        !> Object which should be passed as a reference.
        !> Contains workaround for bug in Intel 19 compiler (pointer => target)
        real(dp), target, intent(in) , contiguous :: val(:)

          call stubError("sendCmdPtrReal1")

      end subroutine sendCmdPtrReal1


      !> Wrapper for passing a reference to a PLUMED instance.
      !>
      !> NOTE: This wrapper passes the address of the value object. Make sure, the object
      !> exists long enough, that PLUMED can access and eventually modify it when necessary.
      !>
      subroutine sendCmdPtrReal2(this, key, val)

        !> Instance
        class(TPlumedCalc), intent(inout) :: this

        !> Key (will be automatically extended with the necessary termination character)
        character(len=*, kind=c_char), intent(in) :: key

        !> Object which should be passed as a reference.
        !> Contains workaround for bug in Intel 19 compiler (pointer => target)
        real(dp), target, intent(in) , contiguous :: val(:,:)

          call stubError("sendCmdPtrReal2")

      end subroutine sendCmdPtrReal2




    !> Raises an error signalizing the call of a stub-function.
    subroutine stubError(name)

      !> Name of the stub function which was called.
      character(*), intent(in) :: name

      call error("Internal error: Function '" // name // "' called but code was compiled without&
          & PLUMED support")

    end subroutine stubError



end module dftbp_extlibs_plumed
