!> ============================================================================================================
!> @file vector_test.f90
!> 
!> Test program for ModuleVector functionality.
!> This program tests basic vector operations including initialization, push, pop, get, set, and cleanup.
!> ============================================================================================================

Program VectorTest
    Use ModuleVector
    Implicit none

    Type(Vector) :: my_vector
    Real(8) :: value
    Integer(4) :: size, capacity
    Logical :: empty

    print *, "=== Vector Test Started ==="

    ! Test 1: Initialize vector
    print *, "Test 1: Initializing vector with capacity 2"
    call my_vector%InitVector(2)
    call my_vector%GetSize(size)
    call my_vector%GetCapacity(capacity)
    call my_vector%IsEmpty(empty)
    print *, "Size:", size, "Capacity:", capacity, "Empty:", empty

    ! Test 2: Push elements
    print *, "Test 2: Pushing elements 1.0, 2.0, 3.0, 4.0, 5.0"
    call my_vector%Push(1.0d0)
    call my_vector%Push(2.0d0)
    call my_vector%Push(3.0d0)
    call my_vector%Push(4.0d0)
    call my_vector%Push(5.0d0)
    
    call my_vector%GetSize(size)
    call my_vector%GetCapacity(capacity)
    print *, "After pushing: Size:", size, "Capacity:", capacity

    ! Test 3: Get elements
    print *, "Test 3: Getting elements"
    call my_vector%Get(1, value)
    print *, "Element 1:", value
    call my_vector%Get(3, value)
    print *, "Element 3:", value
    call my_vector%Get(5, value)
    print *, "Element 5:", value

    ! Test 4: Set elements
    print *, "Test 4: Setting element 3 to 99.0"
    call my_vector%Set(3, 99.0d0)
    call my_vector%Get(3, value)
    print *, "Element 3 after set:", value

    ! Test 5: Pop elements
    print *, "Test 5: Popping elements"
    call my_vector%Pop(value)
    print *, "Popped value:", value
    call my_vector%Pop(value)
    print *, "Popped value:", value
    call my_vector%Pop(value)
    print *, "Popped value:", value
    call my_vector%Pop(value)
    print *, "Popped value:", value
    
    call my_vector%GetSize(size)
    call my_vector%GetCapacity(capacity)
    print *, "After popping: Size:", size, "Capacity:", capacity

    ! Test 6: Clear vector
    print *, "Test 6: Clearing vector"
    call my_vector%Clear()
    call my_vector%GetSize(size)
    call my_vector%GetCapacity(capacity)
    call my_vector%IsEmpty(empty)
    print *, "After clear: Size:", size, "Capacity:", capacity, "Empty:", empty

    ! Test 7: Cleanup
    print *, "Test 7: Destroying vector"
    call my_vector%DestroyVector()

    print *, "=== Vector Test Completed ==="

End Program VectorTest
