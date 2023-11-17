!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!


!> Implements interface for the repulsive (force-field like) potential
module dftbp_dftb_repulsive_repulsivelist
  use dftbp_dftb_repulsive_repulsive, only : TRepulsive
  implicit none


real, parameter :: TRepulsiveList_scalingFactor = 1.5

integer, parameter :: TRepulsiveList_minSlots = 4


!> Container for an allocatable item
type :: TRepulsiveListItem
  private
  class(TRepulsive), allocatable :: item
end type TRepulsiveListItem


!> Defines a list of allocatables of a specific type.
!>
!> The list stores allocatable items. All items transfered to the list via the push() method
!> and not removed via the pop() method will be deallocated if the list goes out of scope.
!>
type :: TRepulsiveList
  private
  ! Needs to be a pointer (instead of allocatable) to ensure, that we can safely point to
  ! the individal items in the view() call.
  type(TRepulsiveListItem), pointer :: items(:) => null()
  integer :: nItem = 0
  integer :: minSlots = TRepulsiveList_minSlots
  real :: scalingFactor = TRepulsiveList_scalingFactor
contains
  procedure :: push => TRepulsiveList_push
  procedure :: pop => TRepulsiveList_pop
  procedure :: view => TRepulsiveList_view
  procedure :: size => TRepulsiveList_size
  final :: TRepulsiveList_final
end type TRepulsiveList


contains


!> Initializes the list with explicit prealloc size.
!>
!> The initialization is optional and only needed, if you wish to override the preallocated
!> list size.
!>
subroutine TRepulsiveList_init(this, minSlots, scalingFactor)

  !> Instance
  type(TRepulsiveList), intent(out) :: this

  !> Minimal number of empty slots to create at startup or when resizing the internal storage
  !> (default TRepulsiveList_minElements).
  integer, optional, intent(in) :: minSlots

  !> Growth scaling factor to use when enlarging or shrinking the list
  real, optional, intent(in) :: scalingFactor

  if (present(minSlots)) then

    this%minSlots = minSlots
  end if
  if (present(scalingFactor)) then

    this%scalingFactor = scalingFactor
  end if
  allocate(this%items(this%minSlots))

end subroutine TRepulsiveList_init


!> Pushes an item to the list.
!>
!> The list takes ownership of the allocatable object.
!>
subroutine TRepulsiveList_push(this, item, pos)

  !> Instance.
  class(TRepulsiveList), intent(inout) :: this

  !> Item to push, unallocated on exit.
  class(TRepulsive), allocatable, intent(inout) :: item

  !> The item will be pushed at the specified position. Possible values: from 1 to size() + 1,
  !> or from -size() to 0. The original element at this position and all following ones are
  !> shifted by one position. Default: size() + 1, the pushed element will be the last one.
  integer, optional, intent(in) :: pos

  type(TRepulsiveListItem), pointer :: buffer(:)
  integer :: pos_, ii

  if (.not. associated(this%items)) call TRepulsiveList_init(this)
  if (present(pos)) then

    pos_ = pos
  else
    pos_ = this%nItem + 1
  end if

  if (this%nItem == size(this%items)) then
    allocate(buffer(max(nint(real(this%nItem) * this%scalingFactor), this%nItem + this%minSlots)))
    do ii = 1, this%nItem
      call move_alloc(this%items(ii)%item, buffer(ii)%item)
    end do
    deallocate(this%items)
    this%items => buffer
  end if

  if (pos_ == 0) then
    pos_ = this%nItem + 1
  else if (pos_ < 0) then
    pos_ = this%nItem + 1 + pos_
  end if
  do ii = this%nItem, pos_, -1
    call move_alloc(this%items(ii)%item, this%items(ii + 1)%item)
  end do
  call move_alloc(item, this%items(pos_)%item)
  this%nItem = this%nItem + 1

end subroutine TRepulsiveList_push


!> Pops an element from the list.
!>
!> The list releases the ownership of the item.
!>
subroutine TRepulsiveList_pop(this, item, pos)

  !> Instance.
  class(TRepulsiveList), intent(inout) :: this

  !> Item which was popped from the list.
  class(TRepulsive), allocatable, intent(out) :: item

  !> The item will be poped from the specified position. Possible values: from 1 to size(),
  !> or from -size() to -1. The original elements following this position will be shifted backwards
  !> by one position. Default: size(this), the last element is popped from the list.
  integer, optional, intent(in) :: pos

  integer :: newSize, pos_, ii
  type(TRepulsiveListItem), pointer :: buffer(:)


  if (present(pos)) then

    pos_ = pos
  else
    pos_ = this%nItem
  end if
  if (pos_ < 0) pos_ = this%nItem + 1 + pos_

  call move_alloc(this%items(pos_)%item, item)
  do ii = pos_, this%nItem - 1
    call move_alloc(this%items(ii + 1)%item, this%items(ii)%item)
  end do
  this%nItem = this%nItem - 1

  newSize = nint(size(this%items) / this%scalingFactor)
  if (newSize > this%nItem + this%minSlots) then
    allocate(buffer(newSize))
    do ii = 1, this%nItem
      call move_alloc(this%items(ii)%item, buffer(ii)%item)
    end do
    deallocate(this%items)
    this%items => buffer
  end if

end subroutine TRepulsiveList_pop


!> Gives a view to a given item in the list.
!>
!> NOTE: The list keeps the ownership of the object.
!>
!> This routine may only be called, if at least one element had already been pushed to the list.
!>
subroutine TRepulsiveList_view(this, pos, item)

  !> Instance
  class(TRepulsiveList), intent(in) :: this

  !> The view is returned for the specified position. Possible values: from 1 to size(),
  !> or from -size() to -1.
  integer, intent(in) :: pos

  !> Pointer to the item at the given position. Do not deallocate it!
  class(TRepulsive), pointer, intent(out) :: item

  integer :: pos_



  if (pos < 0) then
    pos_ = this%nItem + 1 + pos
  else
    pos_ = pos
  end if
  item => this%items(pos_)%item

end subroutine TRepulsiveList_view


!> Returns the size of the list
function TRepulsiveList_size(this) result(nItem)

  !> Instance
  class(TRepulsiveList), intent(in) :: this

  !> Nr. of elements in the list.
  integer :: nItem

  nItem = this%nItem

end function TRepulsiveList_size


!> Finalizer
subroutine TRepulsiveList_final(this)

  type(TRepulsiveList), intent(inout) :: this

  if(associated(this%items)) deallocate(this%items)

end subroutine TRepulsiveList_final


end module dftbp_dftb_repulsive_repulsivelist
