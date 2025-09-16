!> =========================================================
!> Module: ModuleRegisterDump
!> Author: Mao ZiAn
!> Created Time: 2025-09-16
!> =========================================================
!> 
!> @brief Efficient variable registration and data output system for simulation diagnostics
!>
!> This module provides a system for registering and tracking simulation variables,
!> and outputting them to formatted data files. It features optimized variable lookup
!> using hash tables, automatic file management, and formatted data output.
!>
!> The RegisterDump type supports:
!> - Fast variable registration with hash-based lookup
!> - Efficient memory management with dynamic resizing
!> - Automatic header generation and formatted output
!> - Pointer-based variable tracking for zero-copy updates
!> - Support for various file opening modes and output formats
!> - Support register variables in Loop without performance penalty !!
!>
!> Usage Example:
!>   Type(RegisterDump) :: diag
!>   Real(8) :: var1 = 1.0, var2 = 2.0, time = 0.0
!>
!>   ! Initialize and open file
!>   Call diag%Init("output.dat")
!>   Call diag%Open()
!>   
!>   ! Register variables
!>   Call diag%Register("variable1", var1)
!>   Call diag%Register("variable2", var2)
!>   
!>   ! Dump values at different time steps
!>   Do while (time < 10.0)
!>     var1 = var1 * 1.01
!>     var2 = var2 + 0.5
!>     time = time + 0.1
!>     Call diag%Register("variable1", var1)  ! Registered variable will skip so register in loop safely
!>     Call diag%Register("variable2", var2)
!>     Call diag%Dump(time)
!>   End Do
!>   
!>   ! Cleanup
!>   Call diag%Close()
!>   Call diag%Destroy()
!>

Module ModuleRegisterDump
Implicit None

    ! Initial size for arrays
    Integer, Parameter :: INITIAL_SIZE = 4  
    Integer, Parameter :: FMT_DEFAULT = 1   ! Default format for output

Type :: VarPointer
    Real(8), Pointer :: ptr => null()  ! Pointer to track variable values
End Type VarPointer

Type RegisterDump
    Character(len=32), Allocatable      :: id(:)             ! Variable identifiers
    Character(len=256)                  :: filename          ! Output file name
    Integer(4)                          :: fmt               ! Format option
    Integer(4)                          :: var_num = 0       ! Number of registered variables
    Integer(4)                          :: file_unit = -1    ! File unit number for output
    Logical, Allocatable                :: register_list(:)  ! Track registered status
    Type(VarPointer), Allocatable       :: var_ptrs(:)       ! Pointers to variables
    Logical                             :: header_written = .false. ! Track if header has been written
    Integer(4), Allocatable             :: var_hash_table(:) ! Hash table for quick variable lookup
    Character(len=32)                   :: last_registered   ! Last registered variable name for quick check
    
    contains
        procedure :: Init        ! Initialize dump structure (without opening file)
        procedure :: Open        ! Open the file for writing/reading
        procedure :: Register    ! Register a variable
        procedure :: Dump        ! Dump all registered variables
        procedure :: Close       ! Close the file only
        procedure :: Destroy     ! Clean up and deallocate all memory
        procedure, private :: ResizeArrays
        procedure, private :: WriteHeader
        procedure, private :: RebuildHashTable
        procedure, private :: ComputeHash
End Type RegisterDump

contains
    ! Initialize the register dump with settings (but don't open file)
    Subroutine Init(this, filename, fmt)
        Class(RegisterDump), Intent(inout)      :: this
        Character(len=*), Intent(in)            :: filename
        Integer(4), Intent(in), Optional        :: fmt

        ! First clean up any existing allocations and close files
        Call this%Destroy()

        ! Set basic properties
        this%filename = filename
        If (Present(fmt)) Then
            this%fmt = fmt
        Else
            this%fmt = FMT_DEFAULT  ! Assuming FMT_DEFAULT is defined somewhere
        End If
        this%var_num = 0
        this%header_written = .false.
        this%last_registered = ""

        ! Allocate arrays with initial size
        Allocate(this%id(INITIAL_SIZE))
        Allocate(this%register_list(INITIAL_SIZE))
        Allocate(this%var_ptrs(INITIAL_SIZE))
        Allocate(this%var_hash_table(INITIAL_SIZE))

        ! Initialize arrays
        this%id = ""
        this%register_list = .false.
        this%var_hash_table = 0
    End Subroutine Init
    
    ! Open the dump file with specified parameters
    Subroutine Open(this, status, position, action)
        Class(RegisterDump), Intent(inout)      :: this
        Character(len=*), Intent(in), Optional  :: status   ! 'replace', 'new', 'old', 'scratch', 'unknown'
        Character(len=*), Intent(in), Optional  :: position ! 'append', 'rewind', 'asis'
        Character(len=*), Intent(in), Optional  :: action   ! 'read', 'write', 'readwrite'
        
        Character(len=10) :: local_status, local_position, local_action
        
        ! Set default values if parameters not provided
        If (Present(status)) Then
            local_status = status
        Else
            local_status = 'unknown'  ! Default to replace (create new file)
        End If
        
        If (Present(position)) Then
            local_position = position
        Else
            local_position = 'asis'   ! Default position
        End If
        
        If (Present(action)) Then
            local_action = action
        Else
            local_action = 'write'    ! Default to write mode
        End If
        
        ! Close file if it's already open
        If (this%file_unit > 0) Then
            Close(this%file_unit)
        End If
        
        ! Get an available file unit
        this%file_unit = GetFreeUnit()
        
        ! Reset header written flag when opening a new file
        ! this%header_written = .false.
        
        ! Open the file with direct Fortran parameters
        If (local_position == 'append') Then
            Open(unit=this%file_unit, file=trim(this%filename), status=local_status, &
                 action=local_action, position='append')
        Else
            Open(unit=this%file_unit, file=trim(this%filename), status=local_status, &
                 action=local_action)
        End If
    End Subroutine Open

    ! Register a variable with a specific ID
    Subroutine Register(this, var_id, var)
        Class(RegisterDump), Intent(inout)      :: this
        Character(len=*), Intent(in)            :: var_id
        Real(8), Target, Intent(in)             :: var

        Integer :: i, hash_val, idx
        Logical :: found
        Character(len=32) :: var_id_local

        ! Make a local copy of var_id with fixed length
        var_id_local = var_id

        ! OPTIMIZATION 1: Quick check if this variable is exactly the same as the last registered
        ! This is a common case when the same code registers variables repeatedly
        If (TRIM(this%last_registered) == TRIM(var_id_local)) Then
            ! This is the same variable as the last time - nothing to do
            Return
        End If

        ! OPTIMIZATION 2: Check if any variables registered yet - quick path for first variable
        If (this%var_num == 0) Then
            ! This is the first variable, register it immediately
            this%var_num = 1
            this%id(1) = var_id_local
            this%register_list(1) = .true.
            this%var_ptrs(1)%ptr => var
            
            ! Update last registered
            this%last_registered = var_id_local
            
            ! Update hash table
            If (ALLOCATED(this%var_hash_table) .and. SIZE(this%var_hash_table) > 0) Then
                hash_val = this%ComputeHash(TRIM(var_id_local))
                this%var_hash_table(hash_val) = 1
            End If
            
            Return
        End If

        ! OPTIMIZATION 3: Use hash table for quick lookup
        hash_val = this%ComputeHash(TRIM(var_id_local))
        idx = 0
        
        If (ALLOCATED(this%var_hash_table) .and. SIZE(this%var_hash_table) >= hash_val) Then
            idx = this%var_hash_table(hash_val)
            
            ! Check if hash points to a valid entry and matches our variable
            If (idx > 0 .and. idx <= this%var_num) Then
                If (this%register_list(idx) .and. TRIM(this%id(idx)) == TRIM(var_id_local)) Then
                    ! Found using hash - nothing to do
                    this%last_registered = var_id_local
                    Return
                End If
            End If
        End If
        
        ! If hash lookup failed, do a standard search
        found = .false.
        Do i = 1, this%var_num
            If (this%register_list(i) .and. TRIM(this%id(i)) == TRIM(var_id_local)) Then
                ! Variable already registered - nothing to do
                found = .true.
                this%last_registered = var_id_local
                
                ! Update hash table for future lookups
                If (ALLOCATED(this%var_hash_table) .and. SIZE(this%var_hash_table) >= hash_val) Then
                    this%var_hash_table(hash_val) = i
                End If
                
                Exit
            End If
        End Do

        ! If not found, register the variable
        If (.not. found) Then
            ! Check if we need to resize arrays
            If (this%var_num >= SIZE(this%id)) Then
                Call ResizeArrays(this)
                ! Note: ResizeArrays rebuilds the hash table
            End If

            this%var_num = this%var_num + 1
            this%id(this%var_num) = var_id_local
            this%register_list(this%var_num) = .true.
            this%var_ptrs(this%var_num)%ptr => var
            
            ! Update last registered
            this%last_registered = var_id_local
            
            ! Update hash table
            If (ALLOCATED(this%var_hash_table)) Then
                hash_val = this%ComputeHash(TRIM(var_id_local))
                If (hash_val <= SIZE(this%var_hash_table)) Then
                    this%var_hash_table(hash_val) = this%var_num
                End If
            End If
        End If
    End Subroutine Register

    ! Write header with variable names
    Subroutine WriteHeader(this)
        Class(RegisterDump), Intent(inout)      :: this
        Integer :: i
        
        ! Check if file is open
        If (this%file_unit <= 0) Return
        
        ! Write "Time" as the first column header if used in the dump
        Write(this%file_unit, '(A15)', advance='no') "Time"
        
        ! Write all variable names
        Do i = 1, this%var_num
            If (this%register_list(i)) Then
                ! Use A format with width matching the data output format
                Write(this%file_unit, '(A15)', advance='no') trim(adjustl(this%id(i)))
            End If
        End Do
        
        Write(this%file_unit, *)
        
        ! Mark that header has been written
        this%header_written = .true.
    End Subroutine WriteHeader
    
    ! Dump all registered variables to the output file
    Subroutine Dump(this, time)
        Class(RegisterDump), Intent(inout)      :: this
        Real(8), Intent(in), Optional           :: time
        
        Integer :: i

        ! If file still not open after attempted open, throw error
        If (this%file_unit <= 0) Then
            Write(*,*) "ERROR: Failed to open file for dumping: ", TRIM(this%filename)
            Error Stop "File open failed in RegisterDump::Dump"
        End If
        
        ! Write header first if this is the first dump call
        If (.not. this%header_written) Then
            Call this%WriteHeader()
        End If

        ! Write time value if provided
        If (Present(time)) Then
            Write(this%file_unit, '(E15.7)', advance='no') time
        End If

        ! Write all registered variables
        Do i = 1, this%var_num
            If (this%register_list(i) .and. Associated(this%var_ptrs(i)%ptr)) Then
                Write(this%file_unit, '(ES15.7)', advance='no') this%var_ptrs(i)%ptr
            End If
        End Do
        
        ! End the line
        Write(this%file_unit, *)
        
        ! Flush to ensure data is written
        Flush(this%file_unit)
    End Subroutine Dump

    ! Close the register dump file (without deallocating memory)
    Subroutine Close(this)
        Class(RegisterDump), Intent(inout)      :: this

        ! Close file if it's open
        If (this%file_unit > 0) Then
            Close(this%file_unit)
            this%file_unit = -1
        End If
    End Subroutine Close
    
    ! Destroy the register dump and clean up all memory
    Subroutine Destroy(this)
        Class(RegisterDump), Intent(inout)      :: this
        
        ! First close the file if it's open
        If (this%file_unit > 0) Then
            Close(this%file_unit)
            this%file_unit = -1
        End If

        If (Allocated(this%id)) Deallocate(this%id)
        If (Allocated(this%register_list)) Deallocate(this%register_list)
        If (Allocated(this%var_ptrs)) Deallocate(this%var_ptrs)
        If (Allocated(this%var_hash_table)) Deallocate(this%var_hash_table)
        
        this%var_num = 0
        this%header_written = .false.
        this%last_registered = ""
        this%filename = ""
    End Subroutine Destroy

    ! Resize arrays when needed
    Subroutine ResizeArrays(this)
        Class(RegisterDump), Intent(inout)      :: this
        
        Character(len=32), Allocatable          :: temp_id(:)
        Logical, Allocatable                    :: temp_register_list(:)
        Type(VarPointer), Allocatable           :: temp_var_ptrs(:)
        Integer(4), Allocatable                 :: temp_hash_table(:)
        Integer                                 :: new_size, i

        new_size = 2 * size(this%id)
        
        Allocate(temp_id(new_size))
        Allocate(temp_register_list(new_size))
        Allocate(temp_var_ptrs(new_size))
        Allocate(temp_hash_table(new_size))
        
        temp_id = ""
        temp_register_list = .false.
        temp_hash_table = 0
        
        ! Copy existing data
        Do i = 1, this%var_num
            temp_id(i) = this%id(i)
            temp_register_list(i) = this%register_list(i)
            temp_var_ptrs(i)%ptr => this%var_ptrs(i)%ptr
            
            ! We don't copy hash table values as they'll be regenerated
        End Do
        
        Deallocate(this%id)
        Deallocate(this%register_list)
        Deallocate(this%var_ptrs)
        If (Allocated(this%var_hash_table)) Deallocate(this%var_hash_table)
        
        Call Move_Alloc(temp_id, this%id)
        Call Move_Alloc(temp_register_list, this%register_list)
        Call Move_Alloc(temp_var_ptrs, this%var_ptrs)
        Call Move_Alloc(temp_hash_table, this%var_hash_table)
        
        ! Rebuild hash table after resize
        Call RebuildHashTable(this)
    End Subroutine ResizeArrays

    ! Function to get an available file unit
    Function GetFreeUnit() Result(unit_number)
        Integer :: unit_number
        Logical :: is_open
        
        ! Start with unit 10 (avoiding lower numbers often used by system)
        unit_number = 10
        
        ! Find first available unit
        is_open = .true.
        Do While (is_open .and. unit_number < 1000)
            Inquire(unit=unit_number, opened=is_open)
            If (is_open) unit_number = unit_number + 1
        End Do
    End Function GetFreeUnit
    
    ! Compute a hash value for a variable name
    Function ComputeHash(this, var_name) Result(hash_val)
        Class(RegisterDump), Intent(in) :: this
        Character(len=*), Intent(in)    :: var_name
        Integer                         :: hash_val, len_var
        
        ! Simple hash function based on character values
        hash_val = 0
        len_var = LEN_TRIM(var_name)
        
        ! Use first, last, and middle character plus length for better distribution
        If (len_var > 0) Then
            hash_val = ICHAR(var_name(1:1))
            If (len_var > 1) Then
                hash_val = hash_val + ICHAR(var_name(len_var:len_var))
            End If
            If (len_var > 2) Then
                hash_val = hash_val + ICHAR(var_name(len_var/2:len_var/2))
            End If
            hash_val = hash_val + len_var
        End If
        
        ! Make sure hash is in range for our table
        ! Using size-1 and adding 1 after mod to avoid hash=0
        If (ALLOCATED(this%var_hash_table) .and. SIZE(this%var_hash_table) > 0) Then
            hash_val = MOD(hash_val, SIZE(this%var_hash_table)) + 1
        Else
            hash_val = 1
        End If
    End Function ComputeHash
    
    ! Rebuild the hash table
    Subroutine RebuildHashTable(this)
        Class(RegisterDump), Intent(inout) :: this
        Integer :: i, hash_val
        
        ! Initialize hash table
        If (ALLOCATED(this%var_hash_table)) Then
            this%var_hash_table = 0
        Else
            Return
        End If
        
        ! Build hash table entries
        Do i = 1, this%var_num
            If (this%register_list(i)) Then
                hash_val = this%ComputeHash(TRIM(this%id(i)))
                
                ! Simple collision handling - just store the most recent one
                ! More sophisticated handling could be added if needed
                this%var_hash_table(hash_val) = i
            End If
        End Do
    End Subroutine RebuildHashTable
    
End Module ModuleRegisterDump