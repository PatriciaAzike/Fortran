module factrial
    implicit none

    ! declare global parameter
    ! real, parameter:: i = 1
    ! real, parameter:: fact = 1

contains

    ! Define factorial function
    real function factorial(x)
        implicit none

        ! Declare variable 
        real, intent(in):: x
        integer:: F,j
        
        F = 1
        j = 1

        do while(j<=x)
            F = F*j
            j = j + 1
        end do
        factorial = F

    end function factorial

end module factrial

module trigfunction
    implicit none

    ! Declare constants to be used in the module
    real, parameter:: Pi = 3.14159265359
    real, parameter:: degree_180 = 180.0
    real, parameter:: rad_to_deg = degree_180 / Pi
    real, parameter:: deg_to_rad = Pi / degree_180

contains ! seperates the global data from module procedures

    ! Define function that converts radians to degrees
    real function RadiantoDegree(radian)
        implicit none

        real, intent(in):: radian

        RadiantoDegree = radian * rad_to_deg

    end function RadiantoDegree

    ! Define function that converts degrees to radians
    real function DegreetoRadian(degree)
        implicit none

        real, intent(in):: degree

        DegreetoRadian = degree * deg_to_rad

    end function DegreetoRadian

end module trigfunction



program trig_func
    ! import the modules
    use factrial 
    use trigfunction
    implicit none

    ! Declare variable
    real:: x,y
    real:: n
    double precision:: sum, exact
    double precision, parameter:: tol = 1.0d-6

    print*, "Enter x: "
    read(*,*) x
    
    y = DegreetoRadian(x)

    exact = sin(y)

    sum = 0
    n = 0


    do
        sum = sum + (((-1)**n /(factorial(2*n + 1))) * y**((2*n) + 1))
        if (abs(exact - sum) <= tol) exit
        n = n+1
    end do

    print*, "Sin(x) = ", sum


end program trig_func
