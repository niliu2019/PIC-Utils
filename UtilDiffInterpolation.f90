!> =========================================================
!> Module: ModuleDifferentialInterpolation
!> Autor: Mao Zian
!> Created Time: 2025-09-5
!> =========================================================

Module ModuleDifferentialInterpolation
    !> Module providing various numerical differentiation formulas
    !> Including forward, backward and central differences of different orders,
    !> as well as named interpolation methods like Lagrange, Hermite, Spline, 
    !> Adams-Bashforth, and Adams-Moulton
    Implicit none
    Private

    ! Export function interfaces
    
    ! 1st-5th order standard finite differences
    Public :: ForwardDiff_1st, BackwardDiff_1st, CentralDiff_1st
    Public :: ForwardDiff_2nd, BackwardDiff_2nd, CentralDiff_2nd
    Public :: ForwardDiff_3rd, BackwardDiff_3rd, CentralDiff_3rd
    Public :: ForwardDiff_4th, BackwardDiff_4th, CentralDiff_4th
    Public :: ForwardDiff_5th, BackwardDiff_5th, CentralDiff_5th
    
    ! Named interpolation schemes
    Public :: Lagrange_Diff, Hermite_Diff, Spline_Diff
    Public :: AdamsBashforth_Diff, AdamsMoulton_Diff

contains
    !> Computes the numerical derivative of a time series using finite difference methods.
    !> @param step          The step between consecutive data points.
    !> @param n_last        The value at the last point (future).
    !> @param n_cur         The value at the current point.
    !> @param n_prev        The value at the previous point (past).
    !> @return              A real(8) value representing the numerical derivative.

    ! ======== 1st Order Differential Interpolation Methods ========
    
    Function ForwardDiff_1st(step, n_cur, n_prev) Result(dn)
        !> First-order forward difference formula: (f(x) - f(x-h))/h
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_prev
        Real(8) :: dn
        
        dn = (n_cur - n_prev) / step
    End Function ForwardDiff_1st

    Function BackwardDiff_1st(step, n_last, n_cur) Result(dn)
        !> First-order backward difference formula: (f(x+h) - f(x))/h
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_last
        Real(8) :: dn
        
        dn = (n_last - n_cur) / step
    End Function BackwardDiff_1st
    
    Function CentralDiff_1st(step, n_last, n_cur, n_prev) Result(dn)
        !> First-order central difference formula: (f(x+h) - f(x-h))/(2h)
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_last, n_cur, n_prev
        Real(8) :: dn
        
        dn = (n_last - n_prev) / (2.0d0 * step)
    End Function CentralDiff_1st

    ! ======== 2nd Order Differential Interpolation Methods ========
    
    Function ForwardDiff_2nd(step, n_cur, n_prev, n_prev2) Result(dn)
        !> Second-order forward difference formula
        !> Based on three-point stencil for more accuracy
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_prev, n_prev2
        Real(8) :: dn
        
        dn = (-3.0d0*n_cur + 4.0d0*n_prev - n_prev2) / (2.0d0 * step)
    End Function ForwardDiff_2nd

    Function BackwardDiff_2nd(step, n_last2, n_last, n_cur) Result(dn)
        !> Second-order backward difference formula
        !> Based on three-point stencil for more accuracy
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_last, n_last2
        Real(8) :: dn
        
        dn = (3.0d0*n_cur - 4.0d0*n_last + n_last2) / (2.0d0 * step)
    End Function BackwardDiff_2nd

    Function CentralDiff_2nd(step, n_last, n_cur, n_prev) Result(dn)
        !> Second-order central difference formula
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_last, n_cur, n_prev
        Real(8) :: dn
        
        ! This reuses the first order central difference
        ! n_cur is not used in the standard central difference formula
        dn = CentralDiff_1st(step, n_last, n_cur, n_prev)
    End Function CentralDiff_2nd

    ! ======== 3rd Order Differential Interpolation Methods ========
    
    Function ForwardDiff_3rd(step, n_cur, n_prev, n_prev2, n_prev3) Result(dn)
        !> Third-order forward difference formula
        !> Based on four-point stencil
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_prev, n_prev2, n_prev3
        Real(8) :: dn
        
        dn = (11.0d0*n_cur - 18.0d0*n_prev + 9.0d0*n_prev2 - 2.0d0*n_prev3) / (6.0d0 * step)
    End Function ForwardDiff_3rd

    Function BackwardDiff_3rd(step, n_last3, n_last2, n_last, n_cur) Result(dn)
        !> Third-order backward difference formula
        !> Based on four-point stencil
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_last, n_last2, n_last3
        Real(8) :: dn
        
        dn = (-11.0d0*n_cur + 18.0d0*n_last - 9.0d0*n_last2 + 2.0d0*n_last3) / (6.0d0 * step)
    End Function BackwardDiff_3rd

    Function CentralDiff_3rd(step, n_last, n_cur, n_prev, n_prev2) Result(dn)
        !> Third-order central difference formula
        !> Uses asymmetric four-point stencil
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_last, n_cur, n_prev, n_prev2
        Real(8) :: dn
        
        dn = (2.0d0*n_last + 3.0d0*n_cur - 6.0d0*n_prev + n_prev2) / (6.0d0 * step)
    End Function CentralDiff_3rd

    ! ======== 4th Order Differential Interpolation Methods ========
    
    Function ForwardDiff_4th(step, n_cur, n_prev, n_prev2, n_prev3, n_prev4) Result(dn)
        !> Fourth-order forward difference formula
        !> Based on five-point stencil
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_prev, n_prev2, n_prev3, n_prev4
        Real(8) :: dn
        
        dn = (-25.0d0*n_cur + 48.0d0*n_prev - 36.0d0*n_prev2 + 16.0d0*n_prev3 - 3.0d0*n_prev4) / (12.0d0 * step)
    End Function ForwardDiff_4th

    Function BackwardDiff_4th(step, n_last4, n_last3, n_last2, n_last, n_cur) Result(dn)
        !> Fourth-order backward difference formula
        !> Based on five-point stencil
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_last, n_last2, n_last3, n_last4
        Real(8) :: dn
        
        dn = (25.0d0*n_cur - 48.0d0*n_last + 36.0d0*n_last2 - 16.0d0*n_last3 + 3.0d0*n_last4) / (12.0d0 * step)
    End Function BackwardDiff_4th

    Function CentralDiff_4th(step, n_last2, n_last, n_cur, n_prev, n_prev2) Result(dn)
        !> Fourth-order central difference formula
        !> Uses five-point stencil centered around current point
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_last2, n_last, n_cur, n_prev, n_prev2
        Real(8) :: dn
        
        ! Five-point stencil central difference formula
        ! n_cur cancels out in the standard central difference formula
        ! f'(x) ≈ [-f(x+2h) + 8f(x+h) - 8f(x-h) + f(x-2h)]/(12h)
        dn = (-n_last2 + 8.0d0*n_last - 8.0d0*n_prev + n_prev2) / (12.0d0 * step)
    End Function CentralDiff_4th
    
    ! ======== 5th Order Differential Interpolation Methods ========
    
    Function ForwardDiff_5th(step, n_cur, n_prev, n_prev2, n_prev3, n_prev4, n_prev5) Result(dn)
        !> Fifth-order forward difference formula
        !> Based on six-point stencil
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_prev, n_prev2, n_prev3, n_prev4, n_prev5
        Real(8) :: dn
        
        ! Six-point stencil forward difference formula
        dn = (137.0d0*n_cur - 300.0d0*n_prev + 300.0d0*n_prev2 - 200.0d0*n_prev3 + &
              75.0d0*n_prev4 - 12.0d0*n_prev5) / (60.0d0 * step)
    End Function ForwardDiff_5th

    Function BackwardDiff_5th(step, n_last5, n_last4, n_last3, n_last2, n_last, n_cur) Result(dn)
        !> Fifth-order backward difference formula
        !> Based on six-point stencil
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_cur, n_last, n_last2, n_last3, n_last4, n_last5
        Real(8) :: dn
        
        ! Six-point stencil backward difference formula
        dn = (-137.0d0*n_cur + 300.0d0*n_last - 300.0d0*n_last2 + 200.0d0*n_last3 - &
               75.0d0*n_last4 + 12.0d0*n_last5) / (60.0d0 * step)
    End Function BackwardDiff_5th

    Function CentralDiff_5th(step, n_last3, n_last2, n_last, n_cur, n_prev, n_prev2, n_prev3) Result(dn)
        !> Fifth-order central difference formula
        !> Uses seven-point stencil centered around current point
        Real(8), Intent(in) :: step
        Real(8), Intent(in) :: n_last3, n_last2, n_last, n_cur, n_prev, n_prev2, n_prev3
        Real(8) :: dn
        
        ! Seven-point stencil central difference formula
        ! n_cur cancels out in the standard formula:
        ! f'(x) ≈ [-f(x+3h) + 9f(x+2h) - 45f(x+h) + 45f(x-h) - 9f(x-2h) + f(x-3h)]/(60h)
        dn = (-n_last3 + 9.0d0*n_last2 - 45.0d0*n_last + 45.0d0*n_prev - &
               9.0d0*n_prev2 + n_prev3) / (60.0d0 * step)
    End Function CentralDiff_5th
    
    ! ======== Named Interpolation Methods ========
    
    Function Lagrange_Diff(step, values, n) Result(dn)
        !> Lagrange polynomial differentiation
        !> @param step      The step size between points
        !> @param values    Array of function values at equally spaced points
        !> @param n         Order of interpolation (number of points to use)
        !> @return          Estimated derivative using Lagrange polynomial
        Real(8), Intent(in) :: step
        Real(8), Intent(in), Dimension(:) :: values
        Integer, Intent(in) :: n
        Real(8) :: dn
        
        ! Choose appropriate method based on number of points
        Select Case (n)
            Case (2)
                dn = ForwardDiff_1st(step, values(1), values(2))
            Case (3)
                dn = CentralDiff_2nd(step, values(1), values(2), values(3))
            Case (4)
                ! Use a 3rd order formula based on 4 points
                dn = (-values(4) + 6.0d0*values(3) - 3.0d0*values(2) - 2.0d0*values(1)) / (6.0d0 * step)
            Case (5)
                ! Use 4th order central difference
                dn = CentralDiff_4th(step, values(1), values(2), values(3), values(4), values(5))
            Case Default
                ! Default to central difference if applicable
                If (Size(values) >= 3) Then
                    dn = CentralDiff_2nd(step, values(1), values(2), values(3))
                Else
                    dn = ForwardDiff_1st(step, values(1), values(2))
                End If
        End Select
    End Function Lagrange_Diff
    
    Function Hermite_Diff(step, values, derivatives, n) Result(dn)
        !> Hermite interpolation differentiation
        !> @param step       The step size between points
        !> @param values     Array of function values at equally spaced points
        !> @param derivatives Array of known derivatives at points (if available)
        !> @param n          Order of interpolation
        !> @return           Estimated derivative using Hermite interpolation
        Real(8), Intent(in) :: step
        Real(8), Intent(in), Dimension(:) :: values, derivatives
        Integer, Intent(in) :: n
        Real(8) :: dn
        Integer :: i
        
        ! Hermite interpolation uses both function values and derivatives
        ! This is a simplified implementation that combines value-based differences
        ! and known derivatives with appropriate weights
        
        If (n <= 1 .or. Size(derivatives) < 1) Then
            ! Not enough information for Hermite interpolation
            ! Fall back to regular finite difference
            If (Size(values) >= 3) Then
                dn = CentralDiff_2nd(step, values(1), values(2), values(3))
            Else
                dn = ForwardDiff_1st(step, values(1), values(2))
            End If
            Return
        End If
        
        ! Use a weighted combination of provided derivatives and finite differences
        dn = 0.0d0
        
        ! Weight from known derivatives (typically more accurate)
        Do i = 1, Min(n, Size(derivatives))
            dn = dn + 0.7d0 * derivatives(i) / Min(n, Size(derivatives))
        End Do
        
        ! Weight from finite difference (for robustness)
        If (Size(values) >= 5) Then
            dn = dn + 0.3d0 * CentralDiff_4th(step, values(1), values(2), values(3), values(4), values(5))
        Else If (Size(values) >= 3) Then
            dn = dn + 0.3d0 * CentralDiff_2nd(step, values(1), values(2), values(3))
        Else
            dn = dn + 0.3d0 * ForwardDiff_1st(step, values(1), values(2))
        End If
    End Function Hermite_Diff
    
    Function Spline_Diff(step, values, n) Result(dn)
        !> Cubic spline differentiation
        !> @param step     The step size between points
        !> @param values   Array of function values at equally spaced points
        !> @param n        Number of points to use in the interpolation
        !> @return         Estimated derivative using cubic spline interpolation
        Real(8), Intent(in) :: step
        Real(8), Intent(in), Dimension(:) :: values
        Integer, Intent(in) :: n
        Real(8) :: dn
        
        ! This is a simplified cubic spline differentiation that uses
        ! a combination of central differences of different orders
        ! A full implementation would solve for spline coefficients
        
        If (n >= 5) Then
            ! For 5 or more points, use a weighted combination of 
            ! 2nd and 4th order central differences
            dn = 0.25d0 * CentralDiff_2nd(step, values(2), values(3), values(4)) + &
                 0.75d0 * CentralDiff_4th(step, values(1), values(2), values(3), values(4), values(5))
        Else If (n >= 3) Then
            ! For 3 points, use 2nd order central difference
            dn = CentralDiff_2nd(step, values(1), values(2), values(3))
        Else
            ! Fall back to forward difference
            dn = ForwardDiff_1st(step, values(1), values(2))
        End If
    End Function Spline_Diff
    
    Function AdamsBashforth_Diff(step, values, n) Result(dn)
        !> Adams-Bashforth multistep method differentiation
        !> @param step    The step size between points
        !> @param values  Array of function values at equally spaced points
        !> @param n       Order of the Adams-Bashforth method (1-5)
        !> @return        Estimated derivative using Adams-Bashforth formula
        Real(8), Intent(in) :: step
        Real(8), Intent(in), Dimension(:) :: values
        Integer, Intent(in) :: n
        Real(8) :: dn
        
        ! Adams-Bashforth methods are commonly used in ODE solvers
        ! These are the standard coefficients for explicit Adams-Bashforth methods
        
        Select Case (n)
            Case (1)
                ! First-order is just forward Euler (same as forward difference)
                dn = ForwardDiff_1st(step, values(1), values(2))
            Case (2)
                ! Second-order Adams-Bashforth
                dn = (3.0d0*values(1) - 4.0d0*values(2) + values(3)) / (2.0d0*step)
            Case (3)
                ! Third-order Adams-Bashforth
                dn = (23.0d0*values(1) - 16.0d0*values(2) + 5.0d0*values(3)) / (12.0d0*step)
            Case (4)
                ! Fourth-order Adams-Bashforth
                dn = (55.0d0*values(1) - 59.0d0*values(2) + 37.0d0*values(3) - 9.0d0*values(4)) / (24.0d0*step)
            Case (5)
                ! Fifth-order Adams-Bashforth
                dn = (1901.0d0*values(1) - 2774.0d0*values(2) + 2616.0d0*values(3) - &
                      1274.0d0*values(4) + 251.0d0*values(5)) / (720.0d0*step)
            Case Default
                ! Default to highest possible order based on available values
                If (Size(values) >= 5) Then
                    dn = (1901.0d0*values(1) - 2774.0d0*values(2) + 2616.0d0*values(3) - &
                          1274.0d0*values(4) + 251.0d0*values(5)) / (720.0d0*step)
                Else If (Size(values) >= 4) Then
                    dn = (55.0d0*values(1) - 59.0d0*values(2) + 37.0d0*values(3) - 9.0d0*values(4)) / (24.0d0*step)
                Else If (Size(values) >= 3) Then
                    dn = (23.0d0*values(1) - 16.0d0*values(2) + 5.0d0*values(3)) / (12.0d0*step)
                Else
                    dn = ForwardDiff_1st(step, values(1), values(2))
                End If
        End Select
    End Function AdamsBashforth_Diff
    
    Function AdamsMoulton_Diff(step, values, n) Result(dn)
        !> Adams-Moulton implicit multistep method differentiation
        !> @param step    The step size between points
        !> @param values  Array of function values at equally spaced points
        !> @param n       Order of the Adams-Moulton method (1-5)
        !> @return        Estimated derivative using Adams-Moulton formula
        Real(8), Intent(in) :: step
        Real(8), Intent(in), Dimension(:) :: values
        Integer, Intent(in) :: n
        Real(8) :: dn
        
        ! Adams-Moulton methods are implicit multistep methods commonly used in ODE solvers
        ! These are the standard coefficients for implicit Adams-Moulton methods
        
        Select Case (n)
            Case (1)
                ! First-order is just backward Euler (same as backward difference)
                dn = BackwardDiff_1st(step, values(2), values(1))
            Case (2)
                ! Second-order Adams-Moulton (trapezoidal rule)
                dn = (values(1) + values(2)) / (2.0d0*step)
            Case (3)
                ! Third-order Adams-Moulton
                dn = (5.0d0*values(1) + 8.0d0*values(2) - values(3)) / (12.0d0*step)
            Case (4)
                ! Fourth-order Adams-Moulton
                dn = (9.0d0*values(1) + 19.0d0*values(2) - 5.0d0*values(3) + values(4)) / (24.0d0*step)
            Case (5)
                ! Fifth-order Adams-Moulton
                dn = (251.0d0*values(1) + 646.0d0*values(2) - 264.0d0*values(3) + &
                      106.0d0*values(4) - 19.0d0*values(5)) / (720.0d0*step)
            Case Default
                ! Default to highest possible order based on available values
                If (Size(values) >= 5) Then
                    dn = (251.0d0*values(1) + 646.0d0*values(2) - 264.0d0*values(3) + &
                          106.0d0*values(4) - 19.0d0*values(5)) / (720.0d0*step)
                Else If (Size(values) >= 4) Then
                    dn = (9.0d0*values(1) + 19.0d0*values(2) - 5.0d0*values(3) + values(4)) / (24.0d0*step)
                Else If (Size(values) >= 3) Then
                    dn = (5.0d0*values(1) + 8.0d0*values(2) - values(3)) / (12.0d0*step)
                Else
                    dn = BackwardDiff_1st(step, values(2), values(1))
                End If
        End Select
    End Function AdamsMoulton_Diff

End Module ModuleDifferentialInterpolation