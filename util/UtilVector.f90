!> ============================================================================================================
!> @file UtilVector.f90
!> Author: Mao ZiAn
!> Updated: 2025-09-08
!!
!> Module providing a dynamic vector (resizable array) data structure for Real(8) values.
!! This module implements a dynamic array that can grow and shrink automatically as elements
!! are added or removed, providing efficient memory management.
!!
!! Type:
!! - Vector: A dynamic array that automatically manages its size and capacity.
!!
!! Public Methods:
!! - InitVector(init_capacity): Initialize vector with given initial capacity.
!! - Push(value): Add an element to the end of the vector.
!! - Pop(value): Remove and return the last element from the vector.
!! - Get(index, value): Get the value at the specified index (1-based).
!! - Set(index, value): Set the value at the specified index (1-based).
!! - GetSize(vector_size): Get the current number of elements in the vector.
!! - GetCapacity(vector_capacity): Get the current capacity of the vector.
!! - IsEmpty(empty_status): Check if the vector is empty.
!! - Clear(): Remove all elements from the vector but keep allocated memory.
!! - DestroyVector(): Deallocate all memory and reset the vector.
!!
!! Private Methods:
!! - GrowVector(): Automatically increase the capacity when needed.
!! - ShrinkVector(): Automatically decrease the capacity when possible.
!!
!! Usage Example:
!!   Type(Vector) :: my_vector
!!   Real(8) :: value
!!   Integer(4) :: size
!!   
!!   ! Initialize
!!   call my_vector%InitVector(4)
!!   
!!   ! Add elements
!!   call my_vector%Push(1.0d0)
!!   call my_vector%Push(2.0d0)
!!   
!!   ! Access elements
!!   call my_vector%Get(1, value)  ! value = 1.0
!!   call my_vector%Set(2, 5.0d0)  ! change second element to 5.0
!!   
!!   ! Check size
!!   call my_vector%GetSize(size)
!!   
!!   ! Remove elements
!!   call my_vector%Pop(value)     ! removes and returns last element
!!   
!!   ! Cleanup
!!   call my_vector%DestroyVector()
!!
!! Features:
!! - Automatic memory management with grow/shrink capabilities
!! - Bounds checking for safe array access
!! - Efficient memory reallocation strategy (doubling/halving)
!! - Memory leak prevention with proper deallocation
!! ============================================================================================================

Module ModuleVector
    Implicit none

    ! Vector parameters
    Integer(4), Parameter :: INITIAL_CAPACITY = 4
    Integer(4), Parameter :: MIN_CAPACITY = 1

    Type :: Vector
        Real(8), Allocatable            :: data(:)
        
        Integer(4)                      :: size = 0
        Integer(4)                      :: capacity = 0
    contains
        Procedure                       :: InitVector
        Procedure                       :: Push
        Procedure                       :: Pop
        Procedure                       :: Get
        Procedure                       :: Set
        Procedure                       :: GetSize
        Procedure                       :: GetCapacity
        Procedure                       :: IsEmpty
        Procedure                       :: Clear
        Procedure                       :: DestroyVector
        Procedure, private              :: GrowVector
        Procedure, private              :: ShrinkVector
    End Type Vector
contains

    Subroutine InitVector(this, init_capacity)
        Class(Vector), Intent(inout)     :: this
        Integer(4), Intent(in)           :: init_capacity

        this%capacity = init_capacity
        Allocate(this%data(this%capacity))
        this%size = 0
    End Subroutine InitVector

    Subroutine Push(this, value)
        Class(Vector), Intent(inout)     :: this
        Real(8), Intent(in)              :: value

        if (this%size >= this%capacity) then
            call this%GrowVector()
        end if

        this%size = this%size + 1
        this%data(this%size) = value
    End Subroutine Push

    Subroutine Pop(this, value)
        Class(Vector), Intent(inout)     :: this
        Real(8), Intent(out)             :: value

        if (this%size <= 0) then
            print *, "Error: Attempt to pop from an empty vector."
            stop
        end if

        value = this%data(this%size)
        this%size = this%size - 1
        call this%ShrinkVector()
    End Subroutine Pop

    Subroutine GrowVector(this)
        Class(Vector), Intent(inout)     :: this
        Integer(4)                       :: new_capacity
        Real(8), Allocatable             :: new_data(:)

        new_capacity = max(this%capacity * 2, INITIAL_CAPACITY)
        if (new_capacity <= this%capacity) then
            return
        end if

        allocate(new_data(new_capacity))
        new_data = 0.0d0
        if (this%size > 0) then
            new_data(1:this%size) = this%data(1:this%size)
        end if

        if (allocated(this%data)) deallocate(this%data)
        this%data = new_data
        this%capacity = new_capacity
    End Subroutine GrowVector

    Subroutine ShrinkVector(this)
        Class(Vector), Intent(inout)     :: this
        Integer(4)                       :: new_capacity
        Real(8), Allocatable             :: new_data(:)

        if (this%size < this%capacity / 3 .and. this%capacity > 1) then
            new_capacity = max(this%capacity / 2, 1)
            allocate(new_data(new_capacity))
            new_data = 0.0d0
            if (this%size > 0) then
                new_data(1:this%size) = this%data(1:this%size)
            end if

            deallocate(this%data)
            this%data = new_data
            this%capacity = new_capacity
        end if
    End Subroutine ShrinkVector

    Subroutine Get(this, index, value)
        Class(Vector), Intent(in)        :: this
        Integer(4), Intent(in)           :: index
        Real(8), Intent(out)             :: value

        if (index < 1 .or. index > this%size) then
            print *, "Error: Index out of bounds in Vector%Get."
            stop
        end if

        value = this%data(index)
    End Subroutine Get

    Subroutine Set(this, index, value)
        Class(Vector), Intent(inout)     :: this
        Integer(4), Intent(in)           :: index
        Real(8), Intent(in)              :: value

        if (index < 1 .or. index > this%size) then
            print *, "Error: Index out of bounds in Vector%Set."
            stop
        end if

        this%data(index) = value
    End Subroutine Set

    Subroutine GetSize(this, vector_size)
        Class(Vector), Intent(in)        :: this
        Integer(4), Intent(out)          :: vector_size

        vector_size = this%size
    End Subroutine GetSize

    Subroutine GetCapacity(this, vector_capacity)
        Class(Vector), Intent(in)        :: this
        Integer(4), Intent(out)          :: vector_capacity

        vector_capacity = this%capacity
    End Subroutine GetCapacity

    Subroutine IsEmpty(this, empty_status)
        Class(Vector), Intent(in)        :: this
        Logical, Intent(out)             :: empty_status

        empty_status = (this%size == 0)
    End Subroutine IsEmpty

    Subroutine Clear(this)
        Class(Vector), Intent(inout)     :: this

        this%size = 0
        call this%ShrinkVector()
    End Subroutine Clear

    Subroutine DestroyVector(this)
        Class(Vector), Intent(inout)     :: this

        if (allocated(this%data)) then
            deallocate(this%data)
        end if
        this%size = 0
        this%capacity = 0
    End Subroutine DestroyVector

End Module ModuleVector