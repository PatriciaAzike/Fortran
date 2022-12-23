program factorial
    implicit none

    ! Declare variable 
    integer:: x,i,fact
    real:: n, pow

    print*, "Enter x and n: "
    read(*,*) x,n

    i = 1
    fact = 1
    do while (i<=x) ! you can use a do loop here and there will be no need to increment i
        fact = fact*i
        i = i+1
        
    end do
    print*, x, "! = ", fact

    pow  = x**n
    print*, "x^n = ", pow
    
end program factorial