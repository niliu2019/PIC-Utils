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

    ! TimeQueue: A simple queue data structure that maintains data in chronological order
    ! When the queue is full, adding new data will remove the oldest data (First-In-First-Out)
    ! This creates a sliding window effect for time-series data
    Type TimeQueue
        Real(8), Allocatable            :: data(:)
        Integer(4)                      :: capacity = 0      ! Maximum size of the queue (fixed capacity)
        Integer(4)                      :: currentSize = 0   ! Current number of elements in the queue
        Integer(4)                      :: head = 1          ! Points to the oldest data (queue front)
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
            this%head = 1
        End Subroutine Init

        Subroutine Enqueue(this, value)
            Class(TimeQueue), Intent(inout)     :: this
            Real(8), Intent(in)                :: value
            Integer :: tail
            if (this%currentSize < this%capacity) then
                tail = mod(this%head + this%currentSize - 2 + this%capacity*10, this%capacity) + 1
                this%data(tail) = value
                this%currentSize = this%currentSize + 1
            else
                tail = this%head
                this%data(tail) = value
                this%head = mod(this%head, this%capacity) + 1
            end if
        End Subroutine Enqueue

        Subroutine Resize(this, new_capacity)
            Class(TimeQueue), intent(inout)    :: this
            Integer(4), intent(in)            :: new_capacity
            Real(8), Allocatable              :: new_data(:)
            Integer :: i, elements_to_copy, idx, old_head

            if (new_capacity <= 0) then
                print *, "Error: New size must be greater than 0."
                return
            end if

            if (new_capacity == this%capacity) then
                return
            end if

            Allocate(new_data(new_capacity))
            new_data = 0.0d0
            elements_to_copy = min(new_capacity, this%currentSize)
            old_head = this%head
            ! Copy the most recent elements in correct order
            do i = 1, elements_to_copy
                idx = mod(old_head - elements_to_copy - 1 + i, this%capacity) + 1
                new_data(i) = this%data(idx)
            end do
            this%currentSize = elements_to_copy
            this%head = mod(elements_to_copy, new_capacity) + 1
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
            this%head = 1
        End Subroutine Destroy
        
        ! Get all data in chronological order (oldest to newest)
        Function GetAllData(this) result(ordered_data)
            Class(TimeQueue), Intent(in)     :: this
            Real(8), Allocatable            :: ordered_data(:)
            Integer :: i, idx
            Allocate(ordered_data(this%currentSize))
            do i = 1, this%currentSize
                idx = mod(this%head + i - 2 + this%capacity*10, this%capacity) + 1
                ordered_data(i) = this%data(idx)
            end do
        End Function GetAllData

        ! Get single element by chronological index (1 = oldest, size = newest)
        Function GetAt(this, index) result(value)
            Class(TimeQueue), Intent(in)     :: this
            Integer, Intent(in)             :: index
            Real(8)                        :: value
            Integer :: idx
            ! Check index range
            if (index < 1 .or. index > this%currentSize) then
                print *, "Error: Index out of bounds in GetAt"
                value = 0.0d0  ! Return default value
                return
            end if
            idx = mod(this%head + index - 2 + this%capacity*10, this%capacity) + 1
            value = this%data(idx)
        End Function GetAt
End Module ModuleTimeQueue

