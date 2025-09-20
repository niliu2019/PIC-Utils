!-------------------------------------------------------------------------------
!  File:    UtilTimeQueue.f90
!  Author:  Mao Zian
!  Brief:   This module implements a time queue data structure for maintaining
!           chronological order of data. When new data is enqueued and the queue
!           is full, the oldest data is removed to make space for the new entry.
!           This ensures the queue always contains the most recent data up to
!           its capacity, making it suitable for sliding window operations on
!           time-series data.
!-------------------------------------------------------------------------------
Module ModuleTimeQueue
    Implicit None

    Type TimeQueue
        Real(8), Allocatable            :: data(:)
        Integer(4)                      :: capacity = 0      ! Maximum size of the queue (fixed capacity)
        Integer(4)                      :: currentSize = 0  ! Current number of elements in the queue
        
    contains
        Procedure :: Init
        Procedure :: Enqueue            ! Add a new data point to the queue
        Procedure :: Destroy
        Procedure :: Resize
        Procedure :: GetAllData         ! Get all data in chronological order (oldest to newest)
        Procedure :: GetAt              ! Get data at a specific position (1 = oldest, size = newest)
    End Type TimeQueue

    contains

        Subroutine Init(this, capacity)
            Class(TimeQueue), Intent(inout)    :: this
            Integer(4), Intent(in)            :: capacity

            this%capacity = capacity
            Allocate(this%data(this%capacity))
            this%data = 0.0d0
            this%currentSize = 0
        End Subroutine Init

        Subroutine Enqueue(this, value)
            Class(TimeQueue), Intent(inout)     :: this
            Real(8), Intent(in)                :: value

            if (this%currentSize == this%capacity) then
                ! Queue is full, use vectorized operation to shift all elements at once
                ! This is much faster for large arrays than element-by-element movement
                ! This implementation only works in Intel API or GNU Fortran
                this%data(1:this%capacity-1) = this%data(2:this%capacity)
                
                ! Add new value at the end
                this%data(this%capacity) = value
            else
                ! Queue is not full, add new element and increase size
                this%currentSize = this%currentSize + 1
                this%data(this%currentSize) = value
            end if
        End Subroutine Enqueue

        Subroutine Resize(this, new_capacity)
            Class(TimeQueue), intent(inout)    :: this
            Integer(4), intent(in)            :: new_capacity
            Real(8), Allocatable              :: new_data(:)
            Integer :: i, elements_to_copy

            if (new_capacity <= 0) then
                print *, "Error: New size must be greater than 0."
                return
            end if

            if (new_capacity == this%capacity) then
                return
            end if

            Allocate(new_data(new_capacity))
            
            if (new_capacity < this%capacity) then
                print *, "Warning: New size is smaller than current size. Data may be lost. Keeping the most recent data."

                ! Determine how many elements to copy (the most recent ones)
                elements_to_copy = min(new_capacity, this%currentSize)
                
                ! Copy the most recent elements
                if (elements_to_copy > 0) then
                    ! If current queue has more elements than new size, start from newer ones
                    if (this%currentSize > new_capacity) then
                        ! Copy newest elements only
                        do i = 1, elements_to_copy
                            new_data(i) = this%data(this%currentSize - elements_to_copy + i)
                        end do
                    else
                        ! Copy all existing elements
                        do i = 1, elements_to_copy
                            new_data(i) = this%data(i)
                        end do
                    end if
                end if
                
                ! Update size
                this%currentSize = elements_to_copy
            else
                ! New capacity is larger, copy all existing data
                do i = 1, this%currentSize
                    new_data(i) = this%data(i)
                end do
            end if
            
            ! Update data and size
            if (allocated(this%data)) Deallocate(this%data)
            this%data = new_data
            this%capacity = new_capacity
        End Subroutine Resize

        Subroutine Destroy(this)
            Class(TimeQueue), Intent(inout)     :: this
            if (Allocated(this%data)) then
                Deallocate(this%data)
            end if
            this%currentSize = 0
            this%capacity = 0
        End Subroutine Destroy
        
        ! Get all data in chronological order (oldest to newest)
        Function GetAllData(this) result(ordered_data)
            Class(TimeQueue), Intent(in)     :: this
            Real(8), Allocatable            :: ordered_data(:)
            
            Allocate(ordered_data(this%capacity))
            ordered_data = 0.0d0
            if (this%currentSize > 0) then
                ordered_data(1:this%currentSize) = this%data(1:this%currentSize)
            end if
        End Function GetAllData

        ! Get single element by chronological index (1 = oldest, size = newest)
        Function GetAt(this, index) result(value)
            Class(TimeQueue), Intent(in)     :: this
            Integer, Intent(in)             :: index
            Real(8)                        :: value
            
            ! Check index range
            if (index < 1 .or. index > this%currentSize) then
                print *, "Error: Index out of bounds in GetAt"
                value = 0.0d0  ! Return default value
                return
            end if
            
            ! Direct array access (index already in correct order)
            value = this%data(index)
        End Function GetAt
End Module ModuleTimeQueue
