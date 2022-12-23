program subroutine
    implicit none

    ! Declare variables
    integer:: n
    real:: prod

    print*, "Enter the integer, n: "
    read*, n

    call fact(n,prod)

    print*, "The factorial of ", n, "is ", prod

end program subroutine


subroutine fact(n1,prod1)
    integer:: n1,i
    real:: prod1

    prod1 = 1

    do i=1,n1
        prod1 = prod1 * i
    end do


end subroutine
