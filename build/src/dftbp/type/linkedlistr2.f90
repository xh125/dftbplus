!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Linked list for real arrays
module dftbp_type_linkedlistr2
  use dftbp_common_accuracy, only : dp
  implicit none

  private


  public :: TListRealR2
  public :: init, destruct
  public :: append, len, find, hasElement, isUnishaped
  public :: get, set, asArray
  public :: elemShape, intoArray


  type :: TNode
    real(dp), dimension(:,:), allocatable :: data
    type(TNode), pointer :: pNext
  end type TNode


  type :: TListRealR2
    private
    integer :: length
    integer :: elemShape(2)
    logical :: tUnishaped
    type(TNode), pointer :: pFirst
    type(TNode), pointer :: pLast
    integer :: iCache
    type(TNode), pointer :: pCache
    logical :: tInitialized = .false.
  end type TListRealR2


  interface init
    module procedure TListRealR2_init
  end interface init

  interface destruct
    module procedure TListRealR2_destruct
  end interface destruct

  interface append
    module procedure TListRealR2_append
  end interface append

  interface len
    module procedure TListRealR2_len
  end interface len

  interface find
    module procedure TListRealR2_find
  end interface find

  interface hasElement
    module procedure TListRealR2_hasElement
  end interface hasElement

  interface isUnishaped
    module procedure TListRealR2_isUnishaped
  end interface isUnishaped

  interface get
    module procedure TListRealR2_get
  end interface get

  interface set
    module procedure TListRealR2_set
  end interface set

  interface asArray
    module procedure TListRealR2_asArray
  end interface asArray

  interface intoArray
    module procedure TListRealR2_intoArray
  end interface intoArray

  interface elemShape
    module procedure TListRealR2_elemShape
  end interface elemShape



contains

  !!* Initializes the list.
  !!* @param list The list to initialize.
  subroutine TListRealR2_init(list)
    type(TListRealR2), intent(inout) :: list



    list%length = 0
    list%elemShape(:) = 0
    list%tUnishaped = .true.
    nullify(list%pFirst)
    nullify(list%pLast)
    list%iCache = 0
    nullify(list%pCache)
    list%tInitialized = .true.

  end subroutine TListRealR2_init


  !!* Destructs the list.
  !!* @param list The list to destruct.
  subroutine TListRealR2_destruct(list)
    type(TListRealR2), intent(inout) :: list

    type(TNode), pointer :: pCur, pNext

    if (.not. list%tInitialized) then
      return
    end if

    pCur => list%pFirst
    do while(associated(pCur))
      pNext => pCur%pNext
      deallocate(pCur)
      pCur => pNext
    end do
    list%tInitialized = .false.

  end subroutine TListRealR2_destruct


  !!* Appends an element to the list.
  !!* @param list  The list to extend.
  !!* @param data The data to add.
  subroutine TListRealR2_append(list, data)
    type(TListRealR2), intent(inout) :: list
    real(dp), dimension(:,:), intent(in) :: data



    !! List contains already elements -> append to the end otherwise as first
    if (associated(list%pLast)) then
      allocate(list%pLast%pNext)
      list%pLast => list%pLast%pNext
      if (list%tUnishaped .and. (.not. all(shape(data) == list%elemShape))) then
        list%tUnishaped = .false.
      end if
    else
      allocate(list%pFirst)
      list%pLast => list%pFirst
      list%elemShape(:) = shape(data)
    end if
    list%length = list%length + 1

    !! initialize node
    nullify(list%pLast%pNext)
    ! WORKAROUND: GFortran 7.1 crashes on automatic allocation
    !list%pLast%data = data
    ! WORKAROUND: GFortran 5.3 can not cope with source allocation
    !allocate(list%pLast%data, source=data)
    allocate(list%pLast%data(size(data, dim=1),size(data, dim=2)))
    list%pLast%data(:,:) = data

  end subroutine TListRealR2_append


  !!* Returns the length(nr. of elements) of the list
  !!* @param list The list to get the length of.
  !!* @return     Nr. of elements in the list.
  integer function TListRealR2_len(list) result(len)
    type(TListRealR2), intent(in) :: list

    len = list%length
  end function TListRealR2_len


  !!* Returns the index of an element in the list.
  !!* @param list  The list object.
  !!* @param data The data to look for.
  !!* @return      Index of the element or zero if not found
  function TListRealR2_find(list, data) result(ind)
    integer :: ind
    type(TListRealR2), intent(inout) :: list
    real(dp), dimension(:,:), intent(in) :: data

    type(TNode), pointer :: pCur
    integer :: ii



    pCur => list%pFirst
    ii = 1
    do while(associated(pCur))
      if (all(shape(pCur%data) == shape(data))) then
        if (all(pCur%data == data)) then
          exit
        end if
      end if
      pCur => pCur%pNext
      ii = ii + 1
    end do

    if (associated(pCur)) then
      ind = ii
      list%iCache = ii
      list%pCache => pCur
    else
      ind = 0
    endif

  end function TListRealR2_find


  !!* Check if given element is in the list
  !!* @param list   The list object
  !!* @param data  Element to look for
  !!* @return       True if element had been found, false otherwise
  logical function TListRealR2_hasElement(list, data) result(hasElement)
    type(TListRealR2), intent(inout) :: list
    real(dp), dimension(:,:), intent(in) :: data



    if (find(list, data) == 0) then
      hasElement = .false.
    else
      hasElement = .true.
    end if

  end function TListRealR2_hasElement


  !!* Fills a variable with the speciefied element of the list
  !!* @param list  The list object.
  !!* @param data The variable to put the element in.
  !!* @param index Index of the element (0 < index < length of the list)
  subroutine TListRealR2_get(list, data, index)
    type(TListRealR2), intent(inout) :: list
    real(dp), dimension(:,:), allocatable, intent(out) :: data
    integer, intent(in) :: index

    type(TNode), pointer :: pCur




    pCur => getNode(list, index)

    data = pCur%data

  end subroutine TListRealR2_get


  !!* Fills a speciefied element of the list with a variable
  !!* @param list  The list object.
  !!* @param data The variable to put the element in.
  !!* @param index Index of the element (0 < index < length of the list)
  subroutine TListRealR2_set(list, data, index)
    type(TListRealR2), intent(inout) :: list
    real(dp), dimension(:,:), intent(in) :: data
    integer, intent(in) :: index

    type(TNode), pointer :: pCur




    pCur => getNode(list, index)

    pCur%data = data

  end subroutine TListRealR2_set


  !!* Checks if list contains members with equal shaped
  !!* @param list The list object.
  !!* @return     True, if elements have equals shaped, False otherwise.
  logical function TListRealR2_isUnishaped(list) result(isUnishaped)
    type(TListRealR2), intent(in) :: list

    isUnishaped = list%tUnishaped
  end function TListRealR2_isUnishaped


  !!* Returns the list as an array of elements.
  !!* @param list The list to get the elements from.
  !!* @param val  Array which will be filled with the elements of the list.
  !!* @note
  !!*   The passed array has to have the rank of the list elements + 1.
  !!*   According to Fortran traditions, the last index of the array addresses
  !!*   the list elements, the indexes before address the elements inside
  !!*   the list elements.
  !!* @note Only unishaped lists can be converted to array!
  !!* @assert Array has the shape(:, :, :, ..., :, <length of the list>)
  !!*         and the dimensions before the last one are compatible with the
  !!*         shape of the elements in the list.
  subroutine TListRealR2_asArray(list, val)
    type(TListRealR2), intent(in) :: list
    real(dp), dimension(:,:,:), intent(out) :: val

    type(TNode), pointer :: pCur
    integer :: lenVal
    integer :: ii

    lenVal = size(val, dim=size(shape(val)))





    pCur => list%pFirst
    ii = 1
    do while(associated(pCur))
      val(:,:, ii) = pCur%data
      ii = ii + 1
      pCur => pCur%pNext
    end do
    if (ii <= lenVal) then
      val(:,:, ii:lenVal) = 0.0_dp
    end if

  end subroutine TListRealR2_asArray



  !!* Get the shape of the elements in the list
  !!* @param list     The list object
  !!* @param valshape contains the shape at return
  subroutine TListRealR2_elemShape(list, valshape, index)
    type(TListRealR2), intent(inout) :: list
    integer, intent(out) :: valshape(:)
    integer, intent(in) :: index

    type(TNode), pointer :: pCur





    if (list%tUnishaped) then
      valshape(:) = list%elemShape
    else
      pCur => getNode(list, index)
      valshape(:) = shape(pCur%data)
    end if

  end subroutine TListRealR2_elemShape




  !!* Fills a variable with the speciefied element of the list
  !!* @param list  The list object.
  !!* @param data The variable to put the element in.
  !!* @param nElem number of elements returned in data
  !!* @param index Index of the element (0 < index < length of the list)
  subroutine TListRealR2_intoArray(list, data, nElem, index)
    type(TListRealR2), intent(inout) :: list
    real(dp), dimension(:,:), intent(inout) :: data
    integer, intent(out) :: nElem
    integer, intent(in) :: index

    type(TNode), pointer :: pCur




    pCur => getNode(list, index)
    nElem = size(pCur%data, dim=2)

    data(:, 1:nElem) = pCur%data
  end subroutine TListRealR2_intoArray





  !!* Returns a pointer to a node with a given index
  !!* @param list  The list object.
  !!* @param pNode Pointer to set to the wanted node.
  !!* @param index Index of the wanted node.
  function getNode(list, index)
    type(TNode), pointer :: getNode
    type(TListRealR2), intent(inout) :: list
    integer, intent(in) :: index

    integer :: ii, iStart



    if (list%iCache == index) then
      getNode => list%pCache
      return
    end if

    if (list%iCache > 0 .and. list%iCache < index) then
      iStart = list%iCache
      getNode => list%pCache
    else
      iStart = 1
      getNode => list%pFirst
    end if

    do ii = iStart + 1, index
      getNode => getNode%pNext
    end do
    list%pCache => getNode
    list%iCache = index

   end function getNode


end module dftbp_type_linkedlistr2
