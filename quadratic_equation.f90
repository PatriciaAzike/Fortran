program quadratic
    implicit none
    
    ! Allow the user to enter the values of a, b, c
    real :: a,b,c, dis ! Discriminant
    complex :: z1,z2
    

    write(*,*) "Enter the coefficients a, b, and c: "
    read(*,*) a,b,c
    

    dis = b**2 - 4.0*a*c
    write(*,*) "Discriminant = ", dis


    mainCond:if (dis == 0) then ! abs(dis) < abs(b)*epsilon(b)
        write(*,*) "x = ", -b/(2.0*a) ! repeated roots
    else if(dis < 0) then ! you have to transform the discriminant to a complex number in order to get the roots
        z1 = (cmplx(-b) + sqrt(cmplx(dis)))/(2.0*a)
        z2 = (cmplx(-b) - sqrt(cmplx(dis)))/(2.0*a)
        write(*,*) "x = ", z1,z2 ! complex roots
    else
        write(*,*) "x = ", (-b + sqrt(dis))/(2.0*a), (-b - sqrt(dis))/(2.0*a)  ! real roots
    end if mainCond



end program quadratic