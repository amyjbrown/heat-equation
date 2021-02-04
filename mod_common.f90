!! module common
!! Contains pretty printing and saving formats
!! Initializtion functoin
module common
    implicit none
    
contains
    pure function U(x) result(retval)
    !! Exponential function representing an exponential bump
    !! on the range x: [0,1], t:[0,1]
    !! Used for intiializing arrays
    implicit none
        real, intent(in) :: x
        real :: retval
        
        retval = exp(-((x - 0.5)) **2 )
    end function U

    subroutine print_array(arr, timestep)
        !! Pretty prints result to stdout
        real, dimension(:, :), intent(in) :: arr
        real, intent(in) :: timestep
        integer x, t

        do t = 1, ubound(arr, dim=2)
            write(*, fmt="(F5.1, 's:')", advance="no") timestep * t
            
            do x = 1, ubound(arr, dim=1)
                write(*, fmt="(F5.2)", advance='no') arr(x, t)
            
            enddo
            write (*, *)
        
        enddo
    end subroutine print_array

    subroutine write_array(arr, filename)
        !! Write `array` to `filename` in TSV format
        real, dimension(:, :), intent(in) :: arr
        character (*), intent(in) :: filename
        integer unit, x, t

        open(newunit=unit, file=trim(filename))
        do t = 1, ubound(arr, dim=2)
            do x = 1, ubound(arr, dim=1)
                write(unit, "(F12.5, 2X)", advance = "no") arr(t,x)
            end do
                write(unit, "(/)", advance="no") ! append newline
        end do

        flush(unit)
        close(unit)

    end subroutine write_array

end module common