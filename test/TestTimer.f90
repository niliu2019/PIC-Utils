Program TimerExample
    Use ModuleTimer
    Implicit none
    
    Type(Timer) :: my_timer
    Real(8) :: my_variable = 10.0d0
    Integer(4) :: i
    
    ! 初始化计时器：每10个时间步为一个周期，时间步长0.1，起始时间0.0
    call my_timer%InitTimer(10, 0.1d0, 0.0d0)
    
    ! 添加transition：将变量从10.0在周期5-15内线性变化到20.0
    call my_timer%AddTransition(10.0d0, 20.0d0, 5, 15, update_my_variable)
    
    ! 模拟主循环
    do i = 1, 200
        call my_timer%UpdateTimer()
        
        ! 每个周期输出一次
        if (mod(i, 10) == 0) then
            write(*,'(A,I3,A,F8.3,A,F8.3)') 'Cycle:', my_timer%cycles_counter, &
                ', Time:', my_timer%cur_time, ', Variable:', my_variable
        end if
        
        if (my_timer%cycles_counter >= 20) exit
    end do
    
    Contains
    
    ! 回调函数：更新my_variable的值
    Subroutine update_my_variable(new_value)
        Real(8), Intent(in) :: new_value
        my_variable = new_value
    End Subroutine update_my_variable
    
End Program TimerExample
