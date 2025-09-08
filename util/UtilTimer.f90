!> @brief Timer module providing time management and value transition capabilities
!> 
!> @author Mao Zian
!> @date 2025-09-08
!> This module implements a timer system that can track simulation time and cycles,
!> and provides automatic value transitions over specified cycle ranges with callback support.

Module ModuleTimer
    Implicit none

    ! Interface for callback functions
    Abstract Interface
        Subroutine TransitionCallback(new_value)
            Real(8), Intent(in) :: new_value
        End Subroutine TransitionCallback
    End Interface

    ! Transition type to hold transition details
    Type :: Transition
        Real(8)     :: start_value
        Real(8)     :: end_value
        Integer(4)  :: start_cycle
        Integer(4)  :: end_cycle
        Logical     :: active = .false.
        Procedure(TransitionCallback), Pointer, Nopass :: callback => null()
    End Type Transition

    Type :: Timer
        Integer(4)  :: end_cycle                    = 0
        Integer(4)  :: cycles_counter               = 0
        Integer(4)  :: internal_cycle_steps         = 0
        Integer(4)  :: internal_time_counter        = 0
        Real(8)     :: internal_time_step           = 0.0d0
        Real(8)     :: start_time                   = 0.0d0
        Real(8)     :: cur_time                     = 0.0d0

        Type(Transition), Allocatable :: transitions(:)
        Integer(4)  :: num_transitions              = 0

        logical     :: has_active_transitions       = .true.    ! Use to track if there are any active transitions
    contains
        Procedure               :: InitTimer
        Procedure               :: UpdateTimer
        Procedure               :: GetCurrentTime
        Procedure               :: GetCurrentCycle
        Procedure               :: SetTimeInterval
        Procedure               :: AddTransition
        Procedure, private      :: UpdateTransitions
    End Type Timer

    Contains

    Subroutine InitTimer(this, end_cycle, cycle_step, time_step, start_time)
        Class(Timer), Intent(inout)                 :: this
        Integer(4), Intent(in)                      :: cycle_step
        Integer(4), Intent(in)                      :: end_cycle
        Real(8), Intent(in)                         :: time_step
        Real(8), Intent(in), Optional               :: start_time

        this%end_cycle              = end_cycle
        this%internal_cycle_steps   = cycle_step
        this%internal_time_step     = time_step
        if (present(start_time)) then
            this%start_time = start_time
        else
            this%start_time = 0.0d0
        end if
        this%cur_time               = this%start_time
        this%cycles_counter         = 0
        this%internal_time_counter  = 0
        this%has_active_transitions = .true.
        this%num_transitions        = 0
    End Subroutine InitTimer

    Subroutine UpdateTimer(this)
        Class(Timer), Intent(inout)                 :: this

        this%cur_time                   = this%cur_time + this%internal_time_step
        this%internal_time_counter      = this%internal_time_counter + 1

        if (this%end_cycle > 0 .and. this%cycles_counter >= this%end_cycle) then
            return
        end if

        if (this%internal_time_counter == INT(this%internal_cycle_steps)) then
            this%cycles_counter         = this%cycles_counter + 1
            this%internal_time_counter  = 0
            
            ! 更新所有活动的transitions
            if (this%has_active_transitions) then
                call this%UpdateTransitions()
            end if
        end if
    End Subroutine UpdateTimer

    Subroutine GetCurrentTime(this, current_time)
        Class(Timer), Intent(in)                    :: this
        Real(8), Intent(out)                        :: current_time

        current_time = this%cur_time
    End Subroutine GetCurrentTime

    Subroutine GetCurrentCycle(this, current_cycle)
        Class(Timer), Intent(in)                    :: this
        Integer(4), Intent(out)                     :: current_cycle

        current_cycle = this%cycles_counter
    End Subroutine GetCurrentCycle

    Subroutine SetTimeInterval(this, value_a, value_b, start_cycle, end_cycle)
        Class(Timer), Intent(inout)              :: this
        Real(8), Intent(in)                      :: value_a
        Real(8), Intent(in)                      :: value_b
        Integer(4), Intent(in)                   :: start_cycle
        Integer(4), Intent(in)                   :: end_cycle

        ! 此子程序预留，可用于设置特定的时间间隔
        ! 目前保持现有time_step不变
        ! 可以在此添加特定逻辑
        continue
    End Subroutine SetTimeInterval

    Subroutine AddTransition(this, start_val, end_val, start_cycle, end_cycle, callback_proc)
        Class(Timer), Intent(inout)     :: this
        Real(8), Intent(in)             :: start_val, end_val
        Integer(4), Intent(in)          :: start_cycle, end_cycle
        Procedure(TransitionCallback)   :: callback_proc
        
        Type(Transition), Allocatable   :: temp_transitions(:)
        Integer(4)                      :: i

        ! 扩展transitions数组
        if (.not. allocated(this%transitions)) then
            allocate(this%transitions(1))
            this%num_transitions = 1
        else
            allocate(temp_transitions(this%num_transitions + 1))
            do i = 1, this%num_transitions
                temp_transitions(i) = this%transitions(i)
            end do
            deallocate(this%transitions)
            this%num_transitions = this%num_transitions + 1
            call move_alloc(temp_transitions, this%transitions)
        end if

        ! 设置新的transition
        this%transitions(this%num_transitions)%start_value = start_val
        this%transitions(this%num_transitions)%end_value = end_val
        this%transitions(this%num_transitions)%start_cycle = start_cycle
        this%transitions(this%num_transitions)%end_cycle = end_cycle
        this%transitions(this%num_transitions)%active = .true.
        this%transitions(this%num_transitions)%callback => callback_proc

        this%has_active_transitions = .true.
    End Subroutine AddTransition

    Subroutine UpdateTransitions(this)
        Class(Timer), Intent(inout) :: this
        Integer(4) :: i
        Real(8) :: progress, new_value
        Logical :: any_active = .false.

        if (.not. allocated(this%transitions)) return

        do i = 1, this%num_transitions
            if (this%transitions(i)%active) then
                any_active = .true.

                ! Check if current cycle is within the transition range
                if (this%cycles_counter >= this%transitions(i)%start_cycle .and. &
                    this%cycles_counter <= this%transitions(i)%end_cycle) then
                    
                    ! From 0.0 to 1.0
                    if (this%transitions(i)%end_cycle > this%transitions(i)%start_cycle) then
                        progress = real(this%cycles_counter - this%transitions(i)%start_cycle) / &
                                  real(this%transitions(i)%end_cycle - this%transitions(i)%start_cycle)
                    else
                        progress = 1.0d0  ! Avoid division by zero
                    end if
                    
                    ! Linear interpolation
                    new_value = this%transitions(i)%start_value + progress * &
                               (this%transitions(i)%end_value - this%transitions(i)%start_value)
                    
                    ! Call the callback to update the variable
                    if (associated(this%transitions(i)%callback)) then
                        call this%transitions(i)%callback(new_value)
                    end if
                    
                    ! Check if transition is complete
                    if (this%cycles_counter >= this%transitions(i)%end_cycle) then
                        this%transitions(i)%active = .false.
                    end if
                end if
            end if
            this%has_active_transitions = any_active
        end do
    End Subroutine UpdateTransitions

End Module ModuleTimer