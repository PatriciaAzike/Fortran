program conditional
    implicit none

    logical :: tof ! true or false
    real :: x,y

    ! tof = .true.

    ! write(*,*) tof
    x = 7.009!5.0
    y = x!6.0

    ! when writing nested loops, you can name your loops
    mainCond:if (y > x) then
        write(*,*) "y is greater than x"
    else if(y < x) then
        write(*,*) "x is greater than y"
    else
        write(*,*) "x and y are the same"
    end if mainCond

    ! Fortran isn't case sensitive
    ! write(*,*) x == y ! .eq. in older fortran
    ! ! we can also write tof = x==y. This changes the original value of tof to the current state of the comparison
    ! write(*,*) x /= y ! use /= for not equal to or .ne.
    ! write(*,*) x < y ! .lt.
    ! write(*,*) x > y ! .gt.
    ! write(*,*) x <= y ! .le.
    ! write(*,*) X >= y ! .ge.


    ! write(*,*) (x < y) .or. (x == y) ! this is same thing as checking x <= y T or F = T
    ! write(*,*) (x < y) .and. (x == y) ! T and F = F
    ! write(*,*) (x < y) .XOR. (x == y)

    ! x = 1.0
    ! write(*,*) x
    ! x = x/3.0
    ! write(*,*) x
    ! x = x * 3.0
    ! write(*,*) x
    ! ! Fortran returned the original answer that we would get if we solved the problem by hand. Machine precision!
    

    ! y = 1.0
    ! write(*,*) y, epsilon(y), y + y*epsilon(y)/2




end program conditional