! Heat Equation Simulation
! Using https://en.wikipedia.org/wiki/Finite_difference_method#Example:_The_heat_equation
! Finite Difference Model (explicit) on
! U(x: real, t: real) -> real where x on [0, 1] and t on [0, T]
! dU/t    = dU/x^2
! constraints
! U(0, t) = (U1, t) = 0
! U(x, 0) = U0(x)
program heat
    use iso_fortran_env, only: &
        stdin => input_unit
    use common
    implicit none
    ! grid size for Space and Time
    integer, parameter :: &
        XMAX    = 30, &
        TMAX    = 3,  &
        SECONDS = 20
    ! step size for time and shape
    real, parameter ::  &
        dx = 1 / real(XMAX), &
        dt = 1 / real(TMAX), & 
        r  = dx / (dt ** 2)    ! paramter used for calculations below
    ! loop variables                   
    integer :: x, t ! loop counter
    ! array on Space X Time
    real, dimension(XMAX,TMAX * SECONDS) :: array

    print *, r

    ! 0 out array for easy printing
    do x = 1, XMAX
        do t = 1, TMAX * SECONDS
            array(x, t) = 0
        end do
    end do
    
    ! initialize first array
    do x = 1, XMAX
        array(x, 1) = 10 * U(real(x) / XMAX)
    end do
    ! ensure boundary conditions for initial values
    array(1,    1) = 0
    array(XMAX, 1) = 0

    ! perform FDM
    do t = 2, TMAX * SECONDS
        ! ensure boundary conditions
        array(1, t) = 0
        array(XMAX, t) = 0
        
        do x = 2, XMAX-1
            array(x, t) = &
                (1- 2*r) * array(x, t-1) &
                 + r * array(x - 1,t - 1) &
                 + r * array(x + 1, t - 1)
        end do 
    end do

    ! call print_array(arr=array, timestep=dt)
    call write_array(array, "explicit.dat")

    contains

end program heat