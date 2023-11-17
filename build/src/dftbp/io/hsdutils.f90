!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Contains high level functions for converting the values in a XML/HSD DOM-tree to Fortran
!> intrinsic types.
!> Todo: Some more routines for complex numbers?
module dftbp_io_hsdutils
  use dftbp_common_accuracy, only : dp
  use dftbp_common_status, only : TStatus
  use dftbp_extlibs_xmlf90, only : fnode, fnodeList, getFirstChild, getParentNode, string,&
      & appendChild, xmlf_t, TEXT_NODE, textNodeName, ELEMENT_NODE, char, getLength,&
      & assignment(=),getNodeType, replaceChild, createTextNode, createElement, removeChild, trim,&
      & getAttribute, setAttribute, append_to_string, resize_string, len, xml_NewElement,&
      & xml_AddPCData, xml_EndElement, getItem1, prepend_to_string, getAttribute, getNodeName,&
      & getNodeValue, destroyNode, setAttribute, getAttribute, normalize
  use dftbp_io_charmanip, only : newline, whiteSpaces, space, tolower, unquote, complementaryScan
  use dftbp_io_hsdparser, only : attrEnd, attrFile, attrList, attrStart, attrModifier, attrName,&
      & getHSDPath, getNodeHSDName
  use dftbp_io_indexselection, only : getIndexSelection
  use dftbp_io_message, only : error, warning
  use dftbp_io_tokenreader, only : TOKEN_EOS, TOKEN_ERROR, LOGICAL_TRUE, LOGICAL_FALSE, TOKEN_OK,&
      & getNextToken
  use dftbp_io_xmlutils, only : getChildrenByName, getFirstChildByName
  use dftbp_type_linkedlist, only : len, TListString, TListReal, TListRealR1, TListComplex,&
      & TListComplexR1, TListInt, TlistIntR1, append, init, asArray, destruct
  implicit none

  private
  public :: checkError, detailedError, detailedWarning
  public :: getFirstTextChild, getChildValue, setChildValue
  public :: writeChildValue, getAsString
  public :: getSelectedAtomIndices, getSelectedIndices, appendPathAndLine
  public :: getChild, getChildren, setChild
  public :: attrProcessed


  !> Returns the value (the child) of a child node identified by its name.
  !>
  !> These routines investigate the provided node and look for a child with the supplied name. If
  !> this child found, its child (which should be a single text node or a usual node if the value
  !> argument is of type node) is returned as value converted to the appropriate type. If the child
  !> is not found, an error is raised, unless a default value was specified.In that case, a child is
  !> created with the provided name and is appended to the node. Furthermore a text node containing
  !> the string converted default value is appended to the child node. If default value is provided,
  !> it must be also indicated, if the created child is only allowed to have one further child or
  !> not. (This corresponds to an assignment with '=' in the HSD input.) If the child (identified by
  !> the provided name) is allowed to have a modifier, an argument for the modifier must be provided
  !> to contain the parsed value on return. If the argument for the modifier is missing, but a
  !> modifier is found, the program raises an error. The pointer to the found (or created) child can
  !> be queried through an appropriate argument. If the name of the child to look for is an empty
  !> string, the passed node itself is treated as if it would be the child, which had been found.
  interface getChildValue
    module procedure getChVal_logical
    module procedure getChVal_logicalR1
    module procedure getChVal_node
    module procedure getChVal_string
    module procedure getChVal_lString
    module procedure getChVal_lReal
    module procedure getChVal_lRealR1
    module procedure getChVal_lComplex
    module procedure getChVal_lComplexR1
    module procedure getChVal_lInt
    module procedure getChVal_lIntR1
    module procedure getChVal_real
    module procedure getChVal_realR1
    module procedure getChVal_realR2
    module procedure getChVal_complex
    module procedure getChVal_complexR1
    module procedure getChVal_int
    module procedure getChVal_intR1
    module procedure getChVal_intR2
    module procedure getChVal_lIntR1RealR1
    module procedure getChVal_lStringIntR1RealR1
  end interface getChildValue


  !> Sets the value (the child) of a child node identified by its name
  !>
  !> Those functions are the inverse of the getChildValue functions. They create a child with the
  !> provided name and append to that child a text node (or a normal node, if the provided value is
  !> of type node) containing the provided value. It must be indicated, if the created child is
  !> allowed to have only one single further child. If a child with the specified name already
  !> exists, the program raises an error, unless replacement flag is set on .true.. In that case,
  !> the existing child is replaced. If the name of the child is the empty string, the current
  !> node is treated as if it would be the child, which had been found.
  interface setChildValue
    module procedure setChVal_logical
    module procedure setChVal_logicalR1
    module procedure setChVal_node
    module procedure setChVal_char
    module procedure setChVal_charR1
    module procedure setChVal_real
    module procedure setChVal_realR1
    module procedure setChVal_realR2
    module procedure setChVal_complex
    module procedure setChVal_complexR1
    module procedure setChVal_int
    module procedure setChVal_intR1
    module procedure setChVal_intR2
    module procedure setChVal_intR2RealR2
    module procedure setChVal_charR1intR2RealR2
  end interface setChildValue


  !> Writes a child and its value to an xml-write stream
  interface writeChildValue
    module procedure writeChVal_logical
    module procedure writeChVal_logicalR1
    module procedure writeChVal_real
    module procedure writeChVal_realR1
    module procedure writeChVal_realR2
    module procedure writeChVal_complex
    module procedure writeChVal_complexR1
    module procedure writeChVal_int
    module procedure writeChVal_intR1
    module procedure writeChVal_intR2
    module procedure writeChVal_intR2RealR2
    module procedure writeChVal_charR1
    module procedure writeChVal_charR1IntR2RealR2
  end interface writeChildValue


  !> Returns a string representation of an object
  interface getAsString
    module procedure getAsString_logical
    module procedure getAsString_logicalR1
    module procedure getAsString_real
    module procedure getAsString_realR1
    module procedure getAsString_realR2
    module procedure getAsString_complex
    module procedure getAsString_complexR1
    module procedure getAsString_int
    module procedure getAsString_intR1
    module procedure getAsString_intR2
    module procedure getAsString_intR2RealR2
    module procedure getAsString_charR1
    module procedure getAsString_charR1IntR2RealR2
  end interface getAsString


  !> Error messages
  character(len=*), parameter :: MSG_MISSING_FIELD = "Missing child: "
  character(len=*), parameter :: MSG_EXISTING_CHILD = "Already existing child: "
  character(len=*), parameter :: MSG_NOMODIFIER = "Entity is not allowed to have a modifier"
  character(len=*), parameter :: MSG_MISSING_VALUES = "Not enough values provided."


  !> Length of a line (for wrapping long lines when writing values)
  integer, parameter :: lineLength = 80


  !> Maximal number of characters needed to represent an integer
  integer, parameter :: nCharInt = 50


  !> Maximal number of characters needed to represent a real number
  integer, parameter :: nCharReal = 50


  !> Maximal number of characters needed to represent a logical value
  integer, parameter :: nCharLogical = 4


  !> Attribute signals that a tag was processed
  character(len=*), parameter :: attrProcessed = "proc"


  !> Preallocateated size for temporary buffer strings
  integer, parameter :: preAllocSize = 1024

contains


  !> Returns the value (the child) of a child node as logical.
  subroutine getChVal_logical(node, name, variableValue, default, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    logical, intent(out) :: variableValue

    !> Default value for the child, if child is not found
    logical, intent(in), optional :: default

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    integer :: iStart, iErr
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      iStart = 1
      call getFirstTextChild(child2, text)
      call getNextToken(char(text), variableValue, iStart, iErr)
      call checkError(child2, iErr, "Invalid logical value")
      call checkNoData(child2, char(text), iStart)
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, variableValue, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_logical


  !> Returns the value (the child) of a child node as logical.
  subroutine getChVal_logicalR1(node, name, variableValue, default, nItem, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    logical, intent(out) :: variableValue(:)

    !> Default value for the child, if child is not found
    logical, intent(in), optional :: default(:)

    !> Nr. of read items. If this argument is not passed, and the nr. of read items is less than the
    !> size of the array, the subroutine raises an error.
    integer, intent(out), optional :: nItem

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    integer :: iStart, iErr, nReadItem
    type(fnode), pointer :: child2




    if (present(nItem)) then
      nItem = 0
    end if
    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      iStart = 1
      call getFirstTextChild(child2, text)
      call getNextToken(char(text), variableValue, iStart, iErr, nReadItem)
      call checkError(child2, iErr, "Invalid logical value")
      call checkNoData(child2, char(text), iStart)
      if (present(nItem)) then
        nItem = nReadItem
      elseif (nReadItem /= size(variableValue)) then
        call detailedError(node, MSG_MISSING_VALUES)
      end if
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(nItem)) then
        nItem = size(default)
      end if
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, variableValue, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    call setAttribute(child2, attrProcessed, "")
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_logicalR1


  !> Returns the value (the child) of a child node as string.
  subroutine getChVal_string(node, name, variableValue, default, modifier, child, multiple)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    type(string), intent(inout) :: variableValue

    !> Default value for the child, if child is not found
    character(len=*), intent(in), optional :: default

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    !> If true, string contains as many tokens as possible, not just one (with spaces between the
    !> tokens).
    logical, intent(in), optional :: multiple

    type(string) :: text, modif
    integer :: iStart, iErr
    type(fnode), pointer :: child2
    logical :: tMultiple



    if (present(multiple)) then
      tMultiple = multiple
    else
      tMultiple = .false.
    end if

    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      if (tMultiple) then
        variableValue = unquote(trim(adjustl(char(text))))
      else
        iStart = 1
        call getNextToken(char(text), variableValue, iStart, iErr)
        call checkError(child2, iErr, "Invalid string value")
        call checkNoData(child2, char(text), iStart)
      end if
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, default, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_string


  !> Returns the value (the child) of a child node as real.
  subroutine getChVal_real(node, name, variableValue, default, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    real(dp), intent(out) :: variableValue

    !> Default value for the child, if child is not found
    real(dp), intent(in), optional :: default

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    integer :: iStart, iErr
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      iStart = 1
      call getFirstTextChild(child2, text)
      call getNextToken(char(text), variableValue, iStart, iErr)
      call checkError(child2, iErr, "Invalid real value")
      call checkNoData(child2, char(text), iStart)
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, variableValue, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    call setAttribute(child2, attrProcessed, "")
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_real


  !> Returns the value (the child) of a child node as a rank one real array.
  subroutine getChVal_realR1(node, name, variableValue, default, nItem, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    real(dp), intent(out) :: variableValue(:)

    !> Default value for the child, if child is not found
    real(dp), intent(in), optional :: default(:)

    !> Nr. of read items. If this argument is not passed, and the nr. of read items is less than the
    !> size of the array, the subroutine raises an error.
    integer, intent(out), optional :: nItem

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    integer :: iStart, iErr, nReadItem
    type(fnode), pointer :: child2




    if (present(nItem)) then
      nItem = 0
    end if
    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      iStart = 1
      call getFirstTextChild(child2, text)
      call getNextToken(char(text), variableValue, iStart, iErr, nReadItem)
      call checkError(child2, iErr, "Invalid real value")
      call checkNoData(child2, char(text), iStart)
      if (present(nItem)) then
        nItem = nReadItem
      elseif (nReadItem /= size(variableValue)) then
        call detailedError(node, MSG_MISSING_VALUES)
      end if
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(nItem)) then
        nItem = size(default)
      end if
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, variableValue, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    call setAttribute(child2, attrProcessed, "")
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_realR1


  !> Returns the value (the child) of a child node as a rank two real array.
  !>
  !> This is just a wrapper around the rank one version, to make sure that two dimensional arrays
  !> are pretty printed. For higher ranked arrays the rank one version should be used with some
  !> reshaping after.
  subroutine getChVal_realR2(node, name, variableValue, default, nItem, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    real(dp), intent(out) :: variableValue(:,:)

    !> Default value for the child, if child is not found
    real(dp), intent(in), optional :: default(:,:)

    !> Nr. of read items. If this argument is not passed, and the nr. of read items is less than the
    !> size of the array, the subroutine raises an error.
    integer, intent(out), optional :: nItem

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    real(dp) :: buffer(size(variableValue))
    integer :: nReadItem
    type(string) :: modif
    type(fnode), pointer :: child2




    nReadItem = 0
    variableValue = 0.0_dp
    if (present(default)) then
      call getChildValue(node, name, buffer, reshape(default, shape(buffer)), &
          &nReadItem, modifier=modif, child=child2)
    else
      call getChildValue(node, name, buffer, nItem=nReadItem, modifier=modif, &
          &child=child2)
    end if
    if (present(nItem)) then
      nItem = nReadItem
    elseif (nReadItem /= size(variableValue)) then
      call detailedError(node, MSG_MISSING_VALUES)
    end if
    if (present(modifier)) then
      modifier = modif
    elseif (len(modif) > 0) then
      call detailedError(child2, MSG_NOMODIFIER)
    end if
    variableValue(:,:) = reshape(buffer, shape(variableValue))
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_realR2


  !> Returns the value (the child) of a child node as complex.
  subroutine getChVal_complex(node, name, variableValue, default, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    complex(dp), intent(out) :: variableValue

    !> Default value for the child, if child is not found
    complex(dp), intent(in), optional :: default

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    integer :: iStart, iErr
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      iStart = 1
      call getFirstTextChild(child2, text)
      call getNextToken(char(text), variableValue, iStart, iErr)
      call checkError(child2, iErr, "Invalid real value")
      call checkNoData(child2, char(text), iStart)
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, variableValue, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    call setAttribute(child2, attrProcessed, "")
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_complex


  !> Returns the value (the child) of a child node as a rank one complex array.
  subroutine getChVal_complexR1(node, name, variableValue, default, nItem, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    complex(dp), intent(out) :: variableValue(:)

    !> Default value for the child, if child is not found
    complex(dp), intent(in), optional :: default(:)

    !> Nr. of read items. If this argument is not passed, and the nr. of read items is less than the
    !> size of the array, the subroutine raises an error.
    integer, intent(out), optional :: nItem

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    integer :: iStart, iErr, nReadItem
    type(fnode), pointer :: child2




    if (present(nItem)) then
      nItem = 0
    end if
    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      iStart = 1
      call getFirstTextChild(child2, text)
      call getNextToken(char(text), variableValue, iStart, iErr, nReadItem)
      call checkError(child2, iErr, "Invalid complex value '" // trim(char(text)) // "'")
      call checkNoData(child2, char(text), iStart)
      if (present(nItem)) then
        nItem = nReadItem
      elseif (nReadItem /= size(variableValue)) then
        call detailedError(node, MSG_MISSING_VALUES)
      end if
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(nItem)) then
        nItem = size(default)
      end if
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, variableValue, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    call setAttribute(child2, attrProcessed, "")
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_complexR1


  !> Returns the value (the child) of a child node as integer.
  subroutine getChVal_int(node, name, variableValue, default, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    integer, intent(out) :: variableValue

    !> Default value for the child, if child is not found
    integer, intent(in), optional :: default

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    integer :: iStart, iErr
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      iStart = 1
      call getFirstTextChild(child2, text)
      call getNextToken(char(text), variableValue, iStart, iErr)
      call checkError(child2, iErr, "Invalid integer variableValue")
      call checkNoData(child2, char(text), iStart)
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, variableValue, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_int


  !> Returns the value (the child) of a child node as a rank one integer array.
  subroutine getChVal_intR1(node, name, variableValue, default, nItem, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    integer, intent(out) :: variableValue(:)

    !> Default value for the child, if child is not found
    integer, intent(in), optional :: default(:)

    !> Nr. of read items. If this argument is not passed, and the nr. of read items is less than the
    !> size of the array, the subroutine raises an error.
    integer, intent(out), optional :: nItem

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    integer :: iStart, iErr, nReadItem
    type(fnode), pointer :: child2




    if (present(nItem)) then
      nItem = 0
    end if
    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      iStart = 1
      call getFirstTextChild(child2, text)
      call getNextToken(char(text), variableValue, iStart, iErr, nReadItem)
      call checkError(child2, iErr, "Invalid integer value")
      call checkNoData(child2, char(text), iStart)
      if (present(nItem)) then
        nItem = nReadItem
      elseif (nReadItem /= size(variableValue)) then
        call detailedError(node, MSG_MISSING_VALUES)
      end if
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      variableValue = default
      if (present(nItem)) then
        nItem = size(default)
      end if
      if (present(modifier)) then
        modifier = ""
      end if
      call setChildValue(node, name, variableValue, .false., child=child2)
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_intR1


  !> Returns the value (the child) of a child node as a rank two integer array.
  !>
  !> This is just a wrapper around the rank one version, to make sure that two dimensional arrays
  !> are pretty printed. For higher ranked arrays the rank one version should be used with some
  !> reshaping after.
  subroutine getChVal_intR2(node, name, variableValue, default, nItem, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    integer, intent(out) :: variableValue(:,:)

    !> Default value for the child, if child is not found
    integer, intent(in), optional :: default(:,:)

    !> Nr. of read items. If this argument is not passed, and the nr. of read items is less than the
    !> size of the array, the subroutine raises an error.
    integer, intent(out), optional :: nItem

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    integer :: buffer(size(variableValue))
    integer :: nReadItem
    type(string) :: modif
    type(fnode), pointer :: child2




    nReadItem = 0
    if (present(default)) then
      call getChildValue(node, name, buffer, reshape(default, shape(buffer)), &
          &nReadItem, modif, child=child2)
    else
      call getChildValue(node, name, buffer, nItem=nReadItem, modifier=modif, &
          &child=child2)
    end if
    if (present(nItem)) then
      nItem = nReadItem
    elseif (nReadItem /= size(variableValue)) then
      call detailedError(node, MSG_MISSING_VALUES)
    end if
    if (present(modifier)) then
      modifier = modif
    elseif (len(modif) > 0) then
      call detailedError(child2, MSG_NOMODIFIER)
    end if
    variableValue(:,:) = reshape(buffer, shape(variableValue))
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_intR2


  !> Returns the value (the child) of a child node as a linked list of strings.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lString(node, name, variableValue, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    type(TListString), intent(inout) :: variableValue

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2


    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lString_h(char(text), variableValue, child2)
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lString


  !> Helper function for getChVal_lString to avoid string to character conversion in the do-loop.
  subroutine getChVal_lString_h(text, variableValue, node)

    !> Text to parse
    character(len=*), intent(in) :: text

    !> Contains the value of the parsed text
    type(TListString), intent(inout) :: variableValue

    !> node for error handling
    type(fnode), pointer :: node

    integer :: iStart, iErr
    type(string) :: token

    iStart = 1
    call getNextToken(text, token, iStart, iErr)
    do while (iErr == TOKEN_OK)
      call append(variableValue, trim(unquote(char(token))))
      call getNextToken(text, token, iStart, iErr)
    end do
    if (iErr == TOKEN_ERROR) then
      call detailedError(node, "Invalid string")
    end if

  end subroutine getChVal_lString_h


  !> Returns the value (the child) of a child node as a linked list of reals.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lReal(node, name, variableValue, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    type(TListReal), intent(inout) :: variableValue

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lReal_h(char(text), variableValue, child2)
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lReal


  !> Helper function for getChVal_lReal to avoid string to character conversion in the do-loop.
  subroutine getChVal_lReal_h(text, variableValue, node)

    !> text  Text to parse
    character(len=*), intent(in) :: text

    !> value Contains the value of the parsed text
    type(TListReal), intent(inout) :: variableValue
    type(fnode), pointer :: node

    integer :: iStart, iErr
    real(dp) :: buffer

    iStart = 1
    call getNextToken(text, buffer, iStart, iErr)
    do while (iErr == TOKEN_OK)
      call append(variableValue, buffer)
      call getNextToken(text, buffer, iStart, iErr)
    end do
    if (iErr == TOKEN_ERROR) then
      call detailedError(node, "Invalid real value")
    end if

  end subroutine getChVal_lReal_h


  !> Returns the value (the child) of a child node as a linked list of rank one real arrays.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lRealR1(node, name, dim, variableValue, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Dimension of the arrays
    integer, intent(in) :: dim

    !> Value on return
    type(TListRealR1), intent(inout) :: variableValue

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lRealR1_h(char(text), dim, variableValue, child2)
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lRealR1


  !> Helper function for getChVal_lReal to avoid string to character conversion in the do-loop.
  subroutine getChVal_lRealR1_h(text, dim, variableValue, node)

    !> Text to parse
    character(len=*), intent(in) :: text

    !> buffer sizing
    integer, intent(in) :: dim

    !> Contains the value of the parsed text
    type(TListRealR1), intent(inout) :: variableValue

    !> nodes for error handling
    type(fnode), pointer :: node

    integer :: iStart, iErr
    real(dp) :: buffer(dim)
    integer :: nItem

    iStart = 1
    call getNextToken(text, buffer, iStart, iErr, nItem)
    do while (iErr == TOKEN_OK)
      call append(variableValue, buffer)
      call getNextToken(text, buffer, iStart, iErr, nItem)
    end do
    if (iErr == TOKEN_ERROR) then
      call detailedError(node, "Invalid real value")
    elseif (iErr == TOKEN_EOS .and. nItem /= 0) then
      call detailedError(node, "Unexpected end of data")
    end if

  end subroutine getChVal_lRealR1_h


  !> Returns the value (the child) of a child node as a linked list of complex numbers.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lComplex(node, name, variableValue, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    type(TListComplex), intent(inout) :: variableValue

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lComplex_h(char(text), variableValue, child2)
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lComplex


  !> Helper function for getChVal_lReal to avoid string to character conversion in the do-loop.
  subroutine getChVal_lComplex_h(text, variableValue, node)

    !> text  Text to parse
    character(len=*), intent(in) :: text

    !> value Contains the value of the parsed text
    type(TListComplex), intent(inout) :: variableValue
    type(fnode), pointer :: node

    integer :: iStart, iErr
    complex(dp) :: buffer

    iStart = 1
    call getNextToken(text, buffer, iStart, iErr)
    do while (iErr == TOKEN_OK)
      call append(variableValue, buffer)
      call getNextToken(text, buffer, iStart, iErr)
    end do
    if (iErr == TOKEN_ERROR) then
      call detailedError(node, "Invalid complex value")
    end if

  end subroutine getChVal_lComplex_h


  !> Returns the value (the child) of a child node as a linked list of rank one real arrays.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lComplexR1(node, name, dim, variableValue, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Dimension of the arrays
    integer, intent(in) :: dim

    !> Value on return
    type(TListComplexR1), intent(inout) :: variableValue

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lComplexR1_h(char(text), dim, variableValue, child2)
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lComplexR1


  !> Helper function for getChVal_lReal to avoid string to character conversion in the do-loop.
  subroutine getChVal_lComplexR1_h(text, dim, variableValue, node)

    !> Text to parse
    character(len=*), intent(in) :: text

    !> buffer sizing
    integer, intent(in) :: dim

    !> Contains the value of the parsed text
    type(TListComplexR1), intent(inout) :: variableValue

    !> nodes for error handling
    type(fnode), pointer :: node

    integer :: iStart, iErr
    complex(dp) :: buffer(dim)
    integer :: nItem

    iStart = 1
    call getNextToken(text, buffer, iStart, iErr, nItem)
    do while (iErr == TOKEN_OK)
      call append(variableValue, buffer)
      call getNextToken(text, buffer, iStart, iErr, nItem)
    end do
    if (iErr == TOKEN_ERROR) then
      call detailedError(node, "Invalid real value")
    elseif (iErr == TOKEN_EOS .and. nItem /= 0) then
      call detailedError(node, "Unexpected end of data")
    end if

  end subroutine getChVal_lComplexR1_h


  !> Returns the value (the child) of a child node as linked list of integers.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lInt(node, name, variableValue, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    type(TListInt), intent(inout) :: variableValue

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lInt_h(char(text), variableValue, child2)
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lInt


  !> Helper function for getChVal_lReal to avoid string to character conversion in the do-loop.
  subroutine getChVal_lInt_h(text, variableValue, node)

    !> Text to parse
    character(len=*), intent(in) :: text

    !> Contains the value of the parsed text
    type(TListInt), intent(inout) :: variableValue

    !> node for error handling
    type(fnode), pointer :: node

    integer :: iStart, iErr
    integer :: buffer

    iStart = 1
    call getNextToken(text, buffer, iStart, iErr)
    do while (iErr == TOKEN_OK)
      call append(variableValue, buffer)
      call getNextToken(text, buffer, iStart, iErr)
    end do
    if (iErr == TOKEN_ERROR) then
      call detailedError(node, "Invalid real value")
    end if

  end subroutine getChVal_lInt_h


  !> Returns the value (the child) of a child node as linked list of rank one integer arrays.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lIntR1(node, name, dim, variableValue, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    integer, intent(in) :: dim

    !> Modifier of the child on return
    type(TListIntR1), intent(inout) :: variableValue

    !> Pointer to the child node (with the spec. name) on return
    type(string), intent(inout), optional :: modifier

    !> the child itself
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2



    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lIntR1_h(char(text), dim, variableValue, child2)
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lIntR1


  !> Helper function for getChVal_lReal to avoid string to character conversion in the do-loop.
  subroutine getChVal_lIntR1_h(text, dim, variableValue, node)

    !> Text to parse
    character(len=*), intent(in) :: text

    !> buffer sizing
    integer, intent(in) :: dim

    !> Contains the value of the parsed text
    type(TListIntR1), intent(inout) :: variableValue

    !> node for error handling
    type(fnode), pointer :: node

    integer :: iStart, iErr
    integer :: buffer(dim)
    integer :: nItem

    iStart = 1
    call getNextToken(text, buffer, iStart, iErr, nItem)
    do while (iErr == TOKEN_OK)
      call append(variableValue, buffer)
      call getNextToken(text, buffer, iStart, iErr, nItem)
    end do
    if (iErr == TOKEN_ERROR) then
      call detailedError(node, "Invalid real value")
    elseif (iErr == TOKEN_EOS .and. nItem /= 0) then
      call detailedError(node, "Unexpected end of data")
    end if

  end subroutine getChVal_lIntR1_h


  !> Returns the value (the child) of a child node as a linked list rank one integer and rank one
  !> real arrays.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lIntR1RealR1(node, name, dimInt, valueInt, dimReal, valueReal, modifier, &
      & child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Dimension of the integer arrays in the list
    integer, intent(in) :: dimInt

    !> List of integer arrays on return
    integer, intent(in) :: dimReal

    !> Dimension of the real arrays in the list
    type(TListIntR1), intent(inout) :: valueInt

    !> List of real array on return
    type(TListRealR1), intent(inout) :: valueReal

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2





    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lIntR1RealR1_h(char(text), dimInt, valueInt, &
          &dimReal, valueReal, child2)
      if (len(valueInt) /= len(valueReal)) then
        call detailedError(node, "Unexpected end of data")
      end if
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lIntR1RealR1


  !> Helper function for getChVal_lIntR1RealR1 to avoid string to char conversion in the do-loop.
  subroutine getChVal_lIntR1RealR1_h(text, dimInt, valueInt, dimReal, valueReal, node)

    !> Text to parse
    character(len=*), intent(in) :: text

    !> integer buffer dimensioning
    integer, intent(in) :: dimInt

    !> Contains the value of the integer in the parsed text
    type(TListIntR1), intent(inout) :: valueInt

    !> real buffer dimensioning
    integer, intent(in) :: dimReal

    !> Contains the value of the real in the parsed text
    type(TListRealR1), intent(inout) :: valueReal

    !> for error handling
    type(fnode), pointer :: node

    integer :: iStart, iErr
    real(dp) :: bufferReal(dimReal)
    integer :: bufferInt(dimInt)
    integer :: nItem

    iErr = TOKEN_OK
    iStart = 1
    do while (iErr == TOKEN_OK)
      call getNextToken(text, bufferInt, iStart, iErr, nItem)
      if (iErr == TOKEN_ERROR) then
        call detailedError(node, "Invalid integer")
      elseif (iErr == TOKEN_EOS .and. nItem /= 0) then
        call detailedError(node, "Unexpected end of data")
      end if
      if (iErr == TOKEN_OK) then
        call append(valueInt, bufferInt)
        call getNextToken(text, bufferReal, iStart, iErr, nItem)
        call checkError(node, iErr, "Invalid real")
        if (iErr == TOKEN_OK) then
          call append(valueReal, bufferReal)
        end if
      end if
    end do

  end subroutine getChVal_lIntR1RealR1_h


  !> Returns the value (the child) of a child node as a linked list of string, rank one integer and
  !> rank one real arrays.
  !>
  !> In order to prevent a double packaging (from array to linked list and then from linked list to
  !> array), the setting of defaults for list types is not allowed. The presence of the child must
  !> be explicitly queried in the caller routine and an eventual default setting must be set with
  !> an explicit setChildValue call.
  subroutine getChVal_lStringIntR1RealR1(node, name, valueStr, dimInt, valueInt, dimReal, &
      & valueReal, modifier, child)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> List of strings on return.
    type(TListString), intent(inout) :: valueStr

    !> Dimension of the integer arrays in the list
    integer, intent(in) :: dimInt

    !> List of integer arrays on return
    type(TListIntR1), intent(inout) :: valueInt

    !> Dimension of the real arrays in the list
    integer, intent(in) :: dimReal

    !> List of real array on return
    type(TListRealR1), intent(inout) :: valueReal

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child on return
    type(fnode), pointer, optional :: child

    type(string) :: text, modif
    type(fnode), pointer :: child2





    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      call getFirstTextChild(child2, text)
      call getChVal_lStringIntR1RealR1_h(char(text), valueStr, &
          &dimInt, valueInt, dimReal, valueReal, child2)
      if (len(valueStr) /= len(valueInt) &
          &.or. len(valueInt) /= len(valueReal)) then
        call detailedError(node, "Unexpected end of data")
      end if
      call setAttribute(child2, attrProcessed, "")
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_lStringIntR1RealR1


  !> Helper function for getChVal_lIntR1RealR1 to avoid string to char conversion in the do-loop.
  subroutine getChVal_lStringIntR1RealR1_h(text, valueStr, dimInt, valueInt, dimReal, valueReal, &
      & node)

    !> Text to parse
    character(len=*), intent(in) :: text

    !> Contains the string part of the parsed text
    type(TListString), intent(inout) :: valueStr

    !> integer buffer dimensioning
    integer, intent(in) :: dimInt

    !> Contains the integer part of the parsed text
    type(TListIntR1), intent(inout) :: valueInt

    !> integer buffer dimensioning
    integer, intent(in) :: dimReal

    !> Contains the real value part of the parsed text
    type(TListRealR1), intent(inout) :: valueReal

    !> for error handling
    type(fnode), pointer :: node

    integer :: iStart, iErr
    real(dp) :: bufferReal(dimReal)
    integer :: bufferInt(dimInt)
    integer :: nItem
    type(string) :: bufferStr

    iErr = TOKEN_OK
    iStart = 1
    do while (iErr == TOKEN_OK)
      call getNextToken(text, bufferStr, iStart, iErr)
      if (iErr == TOKEN_ERROR) then
        call detailedError(node, "Invalid string")
      elseif (iErr == TOKEN_EOS) then
        exit
      end if
      call append(valueStr, char(bufferStr))

      call getNextToken(text, bufferInt, iStart, iErr, nItem)
      call checkError(node, iErr, "Invalid integer")
      call append(valueInt, bufferInt)

      call getNextToken(text, bufferReal, iStart, iErr, nItem)
      call checkError(node, iErr, "Invalid real")
      call append(valueReal, bufferReal)
    end do

  end subroutine getChVal_lStringIntR1RealR1_h


  !> Returns the value (the child) of a child node as a node.
  !>
  !> Caveat: If allowEmptyValue is set to .true. and the child has no subnodes (empty value) then
  !> the returned value is an unassociated pointer
  subroutine getChVal_node(node, name, variableValue, default, modifier, child, list, &
      & allowEmptyValue, dummyValue)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value on return
    type(fnode), pointer :: variableValue

    !> Default value for the child, if child is not found. If the empty string is passed as default
    !> value, the child is created but no value is added to it. The returned value pointer will be

    !> unassociated. (allowEmptyValue must be explicitly set to .true.)
    character(len=*), intent(in), optional :: default

    !> Modifier of the child on return
    type(string), intent(inout), optional :: modifier

    !> Pointer to the child node (with the spec. name) on return
    type(fnode), pointer, optional :: child

    !> If the node created as default should be tagged as list.
    logical, intent(in), optional :: list

    !> If the child is allowed to have an empty value.
    logical, intent(in), optional :: allowEmptyValue

    !> If true, the value is not marked as processed.
    logical, intent(in), optional :: dummyValue

    type(string) :: modif
    type(fnode), pointer :: child2
    logical :: tList, tAllowEmptyVal, tDummyValue




    if (present(list)) then
      tList = list
    else
      tList = .false.
    end if
    if (present(allowEmptyValue)) then
      tAllowEmptyVal = allowEmptyValue
    else
      tAllowEmptyVal = .false.
    end if
    if (present(dummyValue)) then
      tDummyValue = dummyValue
    else
      tDummyValue = .false.
    end if

    child2 => getFirstChildByName(node, tolower(name))
    if (associated(child2)) then
      call getAttribute(child2, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child2, MSG_NOMODIFIER)
      end if
      variableValue => getFirstChild(child2)
      if ((.not. associated(variableValue)) .and. (.not. tAllowEmptyVal)) then
        call detailedError(child2, "Missing value")
      end if
      call setAttribute(child2, attrProcessed, "")
    elseif (present(default)) then
      if (present(modifier)) then
        modifier = ""
      end if
      if (len(default) > 0) then
        variableValue => createElement(tolower(default))
        call setChildValue(node, name, variableValue, .false., child=child2, list=tList)
        call setAttribute(variableValue, attrName, default)
      else
        nullify(variableValue)
        call setChild(node, name, child2, .false., list=tList)
      end if
    else
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if
    if (associated(variableValue) .and. .not. tDummyValue) then
      if (getNodeType(variableValue) == ELEMENT_NODE) then
        call setAttribute(variableValue, attrProcessed, "")
      end if
    end if
    if (present(child)) then
      child => child2
    end if

  end subroutine getChVal_node


  !> Converts a string containing atom indices, ranges and species names to a list of atom indices.
  subroutine getSelectedAtomIndices(node, selectionExpr, speciesNames, species, selectedIndices,&
        & selectionRange, indexRange)

    !> Top node for detailed errors.
    type(fnode), pointer, intent(in) :: node

    !> String to convert
    character(len=*), intent(in) :: selectionExpr

    !> Contains the valid species names.
    character(len=*), intent(in) :: speciesNames(:)

    !> Contains for every atom its species index
    integer, intent(in) :: species(:)

    !> Integer list of atom indices on return.
    integer, allocatable, intent(out) :: selectedIndices(:)

    !> The range of indices [from, to] available for selection. Default: [1, size(species)]
    integer, optional, intent(in) :: selectionRange(:)

    !> The range of indices [from, to] available in general. Must contain the range specified in
    !> selectionRange. Default: selectionRange.
    integer, optional, intent(in) :: indexRange(:)

    type(TStatus) :: errStatus
    logical, allocatable :: selected(:)
    integer :: selectionRange_(2)
    integer :: ii

    if (present(selectionRange)) then
      selectionRange_(:) = selectionRange
    else
      selectionRange_(:) = [1, size(species)]
    end if

    allocate(selected(selectionRange_(2) - selectionRange_(1) + 1))
    call getIndexSelection(selectionExpr, selectionRange_, selected, errStatus,&
        & indexRange=indexRange, speciesNames=speciesNames, species=species)
    if (errStatus%hasError()) then
      call detailedError(node, "Invalid atom selection expression '" // trim(selectionExpr) &
          & // "': " // errStatus%message)
    end if
    selectedIndices = pack([(ii, ii = selectionRange_(1), selectionRange_(2))], selected)
    if (size(selectedIndices) == 0) then
      call detailedWarning(node, "Atom index selection expression selected no atoms")
    end if

  end subroutine getSelectedAtomIndices


  !> Converts a string containing indices and ranges to a list of indices.
  subroutine getSelectedIndices(node, selectionExpr, selectionRange, selectedIndices, indexRange)

    !> Top node for detailed errors.
    type(fnode), pointer, intent(in) :: node

    !> String to convert
    character(len=*), intent(in) :: selectionExpr

    !> The range of indices [from, to] offered for selection.
    integer, intent(in) :: selectionRange(:)

    !> Integer list of atom indices on return.
    integer, allocatable, intent(out) :: selectedIndices(:)

    !> The range of indices [from, to] available in general. Must contain the range specified in
    !> selectionRange. Default: selectionRange.
    integer, optional, intent(in) :: indexRange(:)

    type(TStatus) :: errStatus
    logical, allocatable :: selected(:)
    integer :: ii

    allocate(selected(selectionRange(2) - selectionRange(1) + 1))
    call getIndexSelection(selectionExpr, selectionRange, selected, errStatus,&
        & indexRange=indexRange)
    if (errStatus%hasError()) then
      call detailedError(node, "Invalid atom selection expression '" // trim(selectionExpr) &
          & // "': " // errStatus%message)
    end if
    selectedIndices = pack([(ii, ii = selectionRange(1), selectionRange(2))], selected)
    if (size(selectedIndices) == 0) then
      call detailedWarning(node, "Atom index selection expression selected no atoms")
    end if

  end subroutine getSelectedIndices


  !> Returns a child node with a specified name
  subroutine getChild(node, name, child, requested, modifier)

    !> Node to investigate
    type(fnode), pointer :: node

    !> Name of the child node to look for
    character(len=*), intent(in) :: name

    !> Contains a pointer to the child on return
    type(fnode), pointer :: child

    !> If true and child not found, error is issued
    logical, intent(in), optional :: requested

    !> Contains modifier on exit.
    type(string), intent(inout), optional :: modifier

    logical :: tRequested
    type(string) :: modif



    if (present(requested)) then
      tRequested = requested
    else
      tRequested = .true.
    end if

    child => getFirstChildByName(node, tolower(name))
    if (associated(child)) then
      call getAttribute(child, attrModifier, modif)
      if (present(modifier)) then
        modifier = modif
      elseif (len(modif) > 0) then
        call detailedError(child, MSG_NOMODIFIER)
      end if
      call setAttribute(child, attrProcessed, "")
    elseif (tRequested) then
      call detailedError(node, MSG_MISSING_FIELD // name)
    end if

  end subroutine getChild


  !> Returns a list of children with the specified name.
  subroutine getChildren(node, name, children)

    !> Parent node to investigate
    type(fnode), pointer :: node

    !> Name of the children to look for
    character(len=*), intent(in) :: name

    !> List of the children.
    type(fnodeList), pointer :: children

    type(fnode), pointer :: child
    integer :: ii

    children => getChildrenByName(node, tolower(name))
    do ii = 1, getLength(children)
      call getItem1(children, ii, child)
      call setAttribute(child, attrProcessed, "")
    end do

  end subroutine getChildren


  !> Sets the value (child) of a child with given name.
  subroutine setChVal_logical(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    logical, intent(in) :: variableValue

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(string) :: strBuffer
    type(fnode), pointer :: child2
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if

    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .false., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_logical


  !> Sets the value (child) of a child with given name.
  subroutine setChVal_logicalR1(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    logical, intent(in) :: variableValue(:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(string) :: strBuffer
    type(fnode), pointer :: child2
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if

    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .false., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_logicalR1


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_logical(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    logical, intent(in) :: variableValue

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_logical


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_logicalR1(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    logical, intent(in) :: variableValue(:)

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_logicalR1


  !> Returns the text representation of the passed object
  subroutine getAsString_logical(variableValue, strBuffer)

    !> Value to represent
    logical, intent(in) :: variableValue

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    if (variableValue) then
      strBuffer = LOGICAL_TRUE
    else
      strBuffer = LOGICAL_FALSE
    end if

  end subroutine getAsString_logical


  !> Returns the text representation of the passed object
  subroutine getAsString_logicalR1(variableValue, strBuffer)

    !> Value to represent
    logical, intent(in) :: variableValue(:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=nCharLogical) :: buffer
    integer :: buffLen, len
    integer :: ii

    call resize_string(strBuffer, preAllocSize)
    len = 0
    do ii = 1, size(variableValue)
      if (variableValue(ii)) then
        write (buffer, *)LOGICAL_TRUE
      else
        write (buffer, *)LOGICAL_FALSE
      end if
      buffer = adjustl(buffer)
      buffLen = len_trim(buffer)
      len = len + buffLen
      if (len > lineLength) then
        call append_to_string(strBuffer, newline // trim(buffer))
        len = buffLen
      else
        call append_to_string(strBuffer, space // trim(buffer))
      end if
    end do

  end subroutine getAsString_logicalR1


  !> Sets the value (child) of a child with given name.
  !>
  !> Caveat: This subroutines assumes, that a real can be represented as text with less than
  !> nCharReal characters.
  subroutine setChVal_real(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    real(dp), intent(in) :: variableValue

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(string) :: strBuffer
    type(fnode), pointer :: child2
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if

    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .false., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_real


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_real(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    real(dp), intent(in) :: variableValue

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_real


  !> Returns the text representation of the passed object
  subroutine getAsString_real(variableValue, strBuffer)

    !> Value to represent
    real(dp), intent(in) :: variableValue

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=nCharReal) :: buffer

    write (buffer, *) variableValue
    strBuffer = trim(adjustl(buffer))

  end subroutine getAsString_real


  !> Sets the value (child) of a child with given name.
  !>
  !> Caveat: This subroutines assumes, that a real can be represented as text with less than
  !> nCharReal characters.
  subroutine setChVal_realR1(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    real(dp), intent(in) :: variableValue(:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(string) :: strBuffer
    type(fnode), pointer :: child2
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .true., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_realR1


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_realR1(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    real(dp), intent(in) :: variableValue(:)

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_realR1


  !> Returns the text representation of the passed object
  subroutine getAsString_realR1(variableValue, strBuffer)

    !> Value to represent
    real(dp), intent(in) :: variableValue(:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=nCharReal) :: buffer
    integer :: buffLen, len
    integer :: ii

    call resize_string(strBuffer, preAllocSize)
    len = 0
    do ii = 1, size(variableValue)
      write (buffer, *) variableValue(ii)
      buffer = adjustl(buffer)
      buffLen = len_trim(buffer)
      len = len + buffLen
      if (len > lineLength) then
        call append_to_string(strBuffer, newline // trim(buffer))
        len = buffLen
      else
        call append_to_string(strBuffer, space // trim(buffer))
      end if
    end do

  end subroutine getAsString_realR1


  !> Sets the value (child) of a child with given name.  The node to investigate
  !>
  !> This is just a wrapper around the rank one version, to make sure that two dimensional arrays
  !> are pretty printed. For higher ranked arrays the rank one version should be used with some
  !> reshaping before.
  !>
  !> This subroutines assumes, that a real can be represented as text with less than nCharReal
  !> characters.
  subroutine setChVal_realR2(node, name, variableValue, replace, child, modifier)

    !> node to process from
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    real(dp), intent(in) :: variableValue(:,:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), intent(in), optional :: modifier

    type(fnode), pointer :: child2
    type(string) :: strBuffer
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if

    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .true., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_realR2


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_realR2(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    real(dp), intent(in) :: variableValue(:,:)

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_realR2


  !> Returns the text representation of the passed object
  subroutine getAsString_realR2(variableValue, strBuffer)

    !> Value to represent
    real(dp), intent(in) :: variableValue(:,:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=nCharReal) :: buffer
    integer :: ii, jj

    call resize_string(strBuffer, preAllocSize)
    do ii = 1, size(variableValue, dim=2)
      do jj = 1, size(variableValue, dim=1)
        write (buffer, *) variableValue(jj, ii)
        buffer = adjustl(buffer)
        call append_to_string(strBuffer, space // trim(buffer))
      end do
      call append_to_string(strBuffer, newline)
    end do

  end subroutine getAsString_realR2


  !> Sets the value (child) of a child with given name.
  !>
  !> Caveat: This subroutines assumes, that a real can be represented as text with less than
  !> nCharReal characters.
  subroutine setChVal_complex(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    complex(dp), intent(in) :: variableValue

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(string) :: strBuffer
    type(fnode), pointer :: child2
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if

    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .false., tReplace, child2, variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_complex


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_complex(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    complex(dp), intent(in) :: variableValue

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_complex


  !> Returns the text representation of the passed object
  subroutine getAsString_complex(variableValue, strBuffer)

    !> Value to represent
    complex(dp), intent(in) :: variableValue

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(nCharReal) :: buffer1, buffer2
    character :: sep

    write(buffer1, *) variableValue%re
    write(buffer2, *) abs(variableValue%im)
    if (variableValue%im < 0.0_dp) then
      sep = "-"
    else
      sep = "+"
    end if
    strBuffer = trim(adjustl(buffer1)) // sep // trim(adjustl(buffer2)) // "i"

  end subroutine getAsString_complex


  !> Sets the value (child) of a child with given name.
  !>
  !> Caveat: This subroutines assumes, that a real can be represented as text with less than
  !> nCharReal characters.
  subroutine setChVal_complexR1(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    complex(dp), intent(in) :: variableValue(:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(string) :: strBuffer
    type(fnode), pointer :: child2
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .true., tReplace, child2, variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_complexR1


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_complexR1(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    complex(dp), intent(in) :: variableValue(:)

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_complexR1


  !> Returns the text representation of the passed object
  subroutine getAsString_complexR1(variableValue, strBuffer)

    !> Value to represent
    complex(dp), intent(in) :: variableValue(:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    type(string) :: buffer
    integer :: buffLen, totalLen
    integer :: ii

    call resize_string(strBuffer, preAllocSize)
    totalLen = 0
    do ii = 1, size(variableValue)
      call getAsString(variableValue(ii), buffer)
      buffLen = len(buffer)
      totalLen = totalLen + buffLen
      if (totalLen > lineLength) then
        call append_to_string(strBuffer, newline // char(buffer))
        totalLen = buffLen
      else
        call append_to_string(strBuffer, space // char(buffer))
        totalLen = totalLen + 1
      end if
    end do

  end subroutine getAsString_complexR1


  !> Sets the value (child) of a child with given name.
  !>
  !> Caveat: This subroutines assumes, that an integer can be represented as text with less than
  !> nCharInt characters.
  subroutine setChVal_int(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    integer, intent(in) :: variableValue

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(fnode), pointer :: child2
    type(string) :: strBuffer
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .false., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_int


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_int(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    integer, intent(in) :: variableValue

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_int


  !> Returns the text representation of the passed object
  subroutine getAsString_int(variableValue, strBuffer)

    !> Value to represent
    integer, intent(in) :: variableValue

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=nCharInt) :: buffer

    write (buffer, *) variableValue
    strBuffer = trim(adjustl(buffer))

  end subroutine getAsString_int


  !> Sets the value (child) of a child with given name.
  !>
  !> Caveat: This subroutines assumes, that an integer can be represented as text with less than
  !> nCharInt characters.
  subroutine setChVal_intR1(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    integer, intent(in) :: variableValue(:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Optional modifier for the child
    type(fnode), pointer, optional :: child

    character(len=*), optional, intent(in) :: modifier

    type(fnode), pointer :: child2
    type(string) :: strBuffer
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .true., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_intR1


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_intR1(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    integer, intent(in) :: variableValue(:)

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_intR1


  !> Returns the text representation of the passed object
  subroutine getAsString_intR1(variableValue, strBuffer)

    !> Value to represent
    integer, intent(in) :: variableValue(:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=nCharInt) :: buffer
    integer :: buffLen, len
    integer :: ii

    call resize_string(strBuffer, preAllocSize)
    len = 0
    do ii = 1, size(variableValue)
      write (buffer, *) variableValue(ii)
      buffer = adjustl(buffer)
      buffLen = len_trim(buffer)
      len = len + buffLen
      if (len > lineLength) then
        call append_to_string(strBuffer, newline // trim(buffer))
        len = buffLen
      else
        call append_to_string(strBuffer, space // trim(buffer))
      end if
    end do

  end subroutine getAsString_intR1


  !> Sets the value (child) of a child with given name.
  !>
  !> This is just a wrapper around the rank one version, to make sure that two dimensional arrays
  !> are pretty printed. For higher ranked arrays the rank one version should be used with some
  !> reshaping beforehand.
  !>
  !> Caveat: This subroutines assumes, that an integer can be represented as text with less than
  !> nCharInt characters.
  subroutine setChVal_intR2(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    integer, intent(in) :: variableValue(:,:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(fnode), pointer :: child2
    type(string) :: strBuffer
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .true., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_intR2


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_intR2(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    integer, intent(in) :: variableValue(:,:)

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_intR2


  !> Returns the text representation of the passed object
  subroutine getAsString_intR2(variableValue, strBuffer)

    !> Value to represent
    integer, intent(in) :: variableValue(:,:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=nCharInt) :: buffer
    integer :: ii, jj

    call resize_string(strBuffer, preAllocSize)
    do ii = 1, size(variableValue, dim=2)
      do jj = 1, size(variableValue, dim=1)
        write (buffer, *) variableValue(jj, ii)
        buffer = adjustl(buffer)
        call append_to_string(strBuffer, space // trim(buffer))
      end do
      call append_to_string(strBuffer, newline)
    end do

  end subroutine getAsString_intR2


  !> Sets the value (child) of a child with given name.
  subroutine setChVal_char(node, name, variableValue, replace, child, omitQuotes, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    character(len=*), intent(in) :: variableValue

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> If quotes around the string should be omitted
    logical, intent(in), optional :: omitQuotes

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(fnode), pointer :: child2
    logical :: tReplace, tQuotes

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    if (present(omitQuotes)) then
      tQuotes = .not. omitQuotes
    else
      tQuotes = .true.
    end if
    if (tQuotes) then
      call createChild_local(node, name, .false., tReplace, child2, &
          &variableValue='"'//variableValue//'"')
    else
      call createChild_local(node, name, .false., tReplace, child2, variableValue=variableValue)
    end if

    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_char


  !> Sets the value (child) of a child with given name.
  subroutine setChVal_charR1(node, name, variableValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    character(len=*), intent(in) :: variableValue(:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(string) :: strBuffer
    type(fnode), pointer :: child2
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    call getAsString(variableValue, strBuffer)
    call createChild_local(node, name, .true., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_charR1


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_charR1(xf, name, variableValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Value of the node
    character(len=*), intent(in) :: variableValue(:)

    type(string) :: strBuffer

    call getAsString(variableValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_charR1


  !> Returns the text representation of the passed object
  subroutine getAsString_charR1(variableValue, strBuffer)

    !> Value to represent
    character(len=*), intent(in) :: variableValue(:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    integer :: buffLen, len
    integer :: ii

    call resize_string(strBuffer, preAllocSize)
    len = 0
    do ii = 1, size(variableValue)
      buffLen = len_trim(variableValue(ii))
      len = len + buffLen
      if (len > lineLength) then
        call append_to_string(strBuffer, newline // '"'//trim(variableValue(ii))//'"')
        len = buffLen
      else
        call append_to_string(strBuffer, space // '"'//trim(variableValue(ii))//'"')
      end if
    end do

  end subroutine getAsString_charR1


  !> Sets the value (child) of a child with given name.
  subroutine setChVal_intR2RealR2(node, name, intValue, realValue, replace, child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value for the integers
    integer, intent(in) :: intValue(:,:)

    !> Value for the reals
    real(dp), intent(in) :: realValue(:,:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(fnode), pointer :: child2
    type(string) :: strBuffer
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    call getAsString(intValue, realValue, strBuffer)
    call createChild_local(node, name, .true., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_intR2RealR2


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_intR2RealR2(xf, name, intValue, realValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> Integer value of the node
    integer, intent(in) :: intValue(:,:)

    !> real values of the node
    real(dp), intent(in) :: realValue(:,:)

    type(string) :: strBuffer

    call getAsString(intValue, realValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_intR2RealR2


  !> Returns the text representation of the passed object
  subroutine getAsString_intR2RealR2(intValue, realValue, strBuffer)

    !> integer value in node
    integer, intent(in) :: intValue(:,:)

    !> real value in node
    real(dp), intent(in) :: realValue(:,:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=100) :: buffer
    integer :: nRow, nCol1, nCol2
    integer :: ii, jj

    nRow = size(intValue, dim=2)


    nCol1 = size(intValue, dim=1)
    nCol2 = size(realValue, dim=1)
    call resize_string(strBuffer, preAllocSize)
    do ii = 1, nRow
      do jj = 1, nCol1
        write (buffer, *) intValue(jj, ii)
        buffer = adjustl(buffer)
        call append_to_string(strBuffer, space // trim(buffer))
      end do
      do jj = 1, nCol2
        write (buffer, *) realValue(jj, ii)
        buffer = adjustl(buffer)
        call append_to_string(strBuffer, space // trim(buffer))
      end do
      call append_to_string(strBuffer, newline)
    end do

  end subroutine getAsString_intR2RealR2


  !> Sets the value (child) of a child with given name.
  subroutine setChVal_charR1IntR2RealR2(node, name, charValue, intValue, realValue, replace, &
      & child, modifier)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value for the characters
    character(len=*), intent(in) :: charValue(:)

    !> Value for the integers
    integer, intent(in) :: intValue(:,:)

    !> Value for the reals
    real(dp), intent(in) :: realValue(:,:)

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    type(fnode), pointer :: child2
    type(string) :: strBuffer
    logical :: tReplace

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    call getAsString(charValue, intValue, realValue, strBuffer)
    call createChild_local(node, name, .true., tReplace, child2, &
        &variableValue=char(strBuffer))
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_charR1IntR2RealR2


  !> Writes the text representation of a node and its value to an xmlwriter.
  subroutine writeChVal_charR1IntR2RealR2(xf, name, charValue, intValue, realValue)

    !> Xmlwriter stream
    type(xmlf_t), intent(inout) :: xf

    !> Name of the node
    character(len=*), intent(in) :: name

    !> character part of node
    character(len=*), intent(in) :: charValue(:)

    !> integer part of node
    integer, intent(in) :: intValue(:,:)

    !> real value part of node
    real(dp), intent(in) :: realValue(:,:)

    type(string) :: strBuffer

    call getAsString(charValue, intValue, realValue, strBuffer)
    call writeChild_local(xf, name, char(strBuffer))

  end subroutine writeChVal_charR1IntR2RealR2


  !> Returns the text representation of the passed object
  subroutine getAsString_charR1IntR2RealR2(charValue, intValue, realValue, strBuffer)

    !> character part of node
    character(len=*), intent(in) :: charValue(:)

    !> integer part of node
    integer, intent(in) :: intValue(:,:)

    !> real value part of node
    real(dp), intent(in) :: realValue(:,:)

    !> Text representation on exit
    type(string), intent(inout) :: strBuffer

    character(len=100) :: buffer
    integer :: nRow, nCol1, nCol2
    integer :: ii, jj

    nRow = size(charValue)



    nCol1 = size(intValue, dim=1)
    nCol2 = size(realValue, dim=1)
    call resize_string(strBuffer, preAllocSize)
    do ii = 1, nRow
      call append_to_string(strBuffer, charValue(ii))
      do jj = 1, nCol1
        write (buffer, *) intValue(jj, ii)
        buffer = adjustl(buffer)
        call append_to_string(strBuffer, space // trim(buffer))
      end do
      do jj = 1, nCol2
        write (buffer, *) realValue(jj, ii)
        buffer = adjustl(buffer)
        call append_to_string(strBuffer, space // trim(buffer))
      end do
      call append_to_string(strBuffer, newline)
    end do

  end subroutine getAsString_charR1IntR2RealR2


  !> Sets the value (child) of a child with given name.
  subroutine setChVal_node(node, name, variableValue, replace, child, modifier, list)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to look for
    character(len=*), intent(in) :: name

    !> Value to set
    type(fnode), pointer :: variableValue

    !> Replace if child with same name already exists
    logical, intent(in), optional :: replace

    !> Pointer to the child node (with the provided name)
    type(fnode), pointer, optional :: child

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    !> If created child should be marked as a list.
    logical, optional, intent(in) :: list

    type(fnode), pointer :: child2, dummy
    logical :: tReplace, tList

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    if (present(list)) then
      tList = list
    else
      tList = .false.
    end if
    call createChild_local(node, name, tList, tReplace, child2)
    if (associated(variableValue)) then
      dummy => appendChild(child2, variableValue)
    end if
    if (present(child)) then
      child => child2
    end if
    if (present(modifier)) then
      call setAttribute(child2, attrModifier, modifier)
    end if

  end subroutine setChVal_node


  !> Workhorse for the setChildValue routines
  !>
  !> If an empty string is provided as child name, no child is created, and the current node is
  !> replace instead. The pointer "node" becomes associated with the new node, since the old
  !> instance will be destroyed.
  subroutine createChild_local(node, name, list, replace, child, variableValue)

    !> The node to investigate
    type(fnode), pointer :: node

    !> Name of the child to create
    character(len=*), intent(in) :: name

    !> True, if child should be signed as a list
    logical, intent(in) :: list

    !> Replace if child with same name already exists
    logical, intent(in) :: replace

    !> Pointer to the created child on return
    type(fnode), pointer :: child

    !> Value to set (if empty, no child is appended to the created child)
    character(len=*), intent(in), optional :: variableValue

    type(fnode), pointer :: parent, oldChild, child2, text, dummy
    character(len=len(name)) :: loName
    type(string) :: newName, parentname

    if (replace) then
      if (len(name) == 0) then
        call getNodeHSDName(node, newName)
        parent => getParentNode(node)
        oldChild => node
        child2 => createElement(tolower(char(newName)))
        node => child2
      else
        newName = name
        parent => node
        loName = tolower(name)
        oldChild => getFirstChildByName(node, loName)
        child2 => createElement(loName)
      end if
    else
      newName = name
      parent => node
      oldChild => null()
      child2 => createElement(tolower(name))
    end if

    ! If parent is a text mode, no subnodes should be allowed.
    dummy => getFirstChild(parent)
    if (associated(dummy)) then
      call getNodeName(dummy, parentname)
      if (char(parentname) == textNodeName) then
        call detailedError(node, "Node contains superfluous free text: '"&
            & // trim(dummy%nodeValue) // "'")
      end if
    end if

    if (associated(oldChild)) then
      dummy => replaceChild(parent, child2, oldChild)
      call destroyNode(oldChild)
    else
      dummy => appendChild(parent, child2)
    end if

    if (len(newName) > 0) then
      call setAttribute(child2, attrName, char(newName))
    end if
    if (list) then
      call setAttribute(child2, attrList, "")
    end if

    child => child2
    call setAttribute(child, attrProcessed, "")
    if (present(variableValue)) then
      text => createTextNode(variableValue)
      dummy => appendChild(child, text)
    end if

  end subroutine createChild_local


  !> new child in the xml
  subroutine writeChild_local(xf, name, variableValue)

    !> xmlWriter stream
    type(xmlf_t), intent(inout) :: xf

    !> node name
    character(len=*), intent(in) :: name

    !> stored variable string
    character(len=*), intent(in) :: variableValue

    call xml_NewElement(xf, name)
    call xml_AddPCData(xf, variableValue)
    call xml_EndElement(xf, name)

  end subroutine writeChild_local


  !> Creates a child with the given name
  subroutine setChild(node, name, child, replace, list, modifier)

    !> Node to append the child to
    type(fnode), pointer :: node

    !> Name of the child node to append
    character(len=*), intent(in) :: name

    !> Contains the pointer to the added child node on return
    type(fnode), pointer :: child

    !> If an already existing child with the same name should be replaced
    logical, intent(in), optional :: replace

    !> If child should be signed as a list tag
    logical, intent(in), optional :: list

    !> Optional modifier for the child
    character(len=*), optional, intent(in) :: modifier

    logical :: tReplace, tList
    type(fnode), pointer :: dummy

    if (present(replace)) then
      tReplace = replace
    else
      tReplace = .false.
    end if
    if (present(list)) then
      tList = list
    else
      tList = .false.
    end if

    child => getFirstChildByName(node, tolower(name))
    if (associated(child)) then
      if (tReplace) then
        dummy => removeChild(node, child)
        call destroyNode(child)
      else
        call detailedError(node, MSG_EXISTING_CHILD // name)
      end if
    end if
    child => createElement(tolower(name))
    dummy => appendChild(node, child)
    call setAttribute(child, attrName, name)
    call setAttribute(child, attrProcessed, "")
    if (tList) then
      call setAttribute(child, attrList, "")
    end if
    if (present(modifier)) then
      call setAttribute(child, attrModifier, modifier)
    end if

  end subroutine setChild


  !> Returns the content of the first TEXT_NODE child of a given node or empty string, if such a
  !> node does not exist.
  !>
  !> Note: the document tree is normalized, every node has only one TEXT_NODE child.
  subroutine getFirstTextChild(node, str)

    !> The node to investigate.
    type(fnode), pointer :: node

    !> String representation of the TEXT_NODE.
    type(string), intent(out) :: str

    type(fnode), pointer :: child

    child => getFirstChild(node)
    if (.not. associated(child)) then
      str = ""
    elseif (getNodeType(child) /= TEXT_NODE) then
      call detailedError(child, "Invalid node type.")
    else
      call getNodeValue(child, str)
    end if

  end subroutine getFirstTextChild


  !> Checks if error flag signals an error. If yes, raises error.
  subroutine checkError(node, iErr, msg)

    !> Node which the error flag was set for
    type(fnode), pointer :: node

    !> Content of the error flag.
    integer, intent(in) :: iErr

    !> Message to print, if error occurred
    character(len=*), intent(in) :: msg

    if (iErr == TOKEN_ERROR) then
      call detailedError(node, msg)
    elseif (iErr == TOKEN_EOS) then
      call detailedError(node, "Unexpected end of data")
    end if

  end subroutine checkError


  !> Issues an error, if the string from a given position contains non-whitespace characters.
  subroutine checkNoData(node, str, start)

    !> Node which is being processed (for error message)
    type(fnode), pointer :: node

    !> String content of the child.
    character(len=*), intent(in) :: str

    !> Starting position, after which the string should not contain any whitespace characters.
    integer, intent(in) :: start

    if (complementaryScan(str(start:), whiteSpaces) > 0) then
      call detailedError(node, "Superfluous data found.")
    end if

  end subroutine checkNoData


  !> Prints detailed error, including line number and path
  subroutine detailedError(node, msg)

    !> Node where the error occurred.
    type(fnode), pointer :: node

    !> Message to print
    character(len=*), intent(in) :: msg

    type(string) :: str

    str = trim(msg)
    call appendPathAndLine(node, str)
    call error(char(str) // newline)

  end subroutine detailedError


  !> Prints detailed warning, including line number and path
  subroutine detailedWarning(node, msg)

    !> Node where the error occurred.
    type(fnode), pointer :: node

    !> Message to print
    character(len=*), intent(in) :: msg

    type(string) :: str

    str = msg
    call appendPathAndLine(node, str)
    call warning(char(str) // newline)

  end subroutine detailedWarning


  !> Appends path and line information to a string.
  subroutine appendPathAndLine(node, str)

    !> Node, for which path and line should be added
    type(fnode), pointer :: node

    !> String prepending the path and line information
    type(string), intent(inout) :: str

    type(string) :: str2, str3

    call append_to_string(str, newline // "Path: ")
    call getHSDPath(node, str2, excludeRoot=.true.)
    call append_to_string(str, str2)
    call getAttribute(node, attrStart, str2)
    call getAttribute(node, attrEnd, str3)
    if (len(str2) /= 0) then
      call append_to_string(str, newline // "Line: ")
      call append_to_string(str, str2)
      if (len(str3) /= 0) then
        call append_to_string(str, "-")
        call append_to_string(str, str3)
      end if
    end if
    call getAttribute(node, attrFile, str2)
    if (len(str2) /= 0) then
      call append_to_string(str, " (File: ")
      call append_to_string(str, str2)
      call append_to_string(str, ")")
    end if

  end subroutine appendPathAndLine

end module dftbp_io_hsdutils