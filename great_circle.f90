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



program great_circle
    use trigfunction ! imports module trigfunction
    implicit none

    ! Declare variables to be entered by the user
    real:: x1,y1,x2,y2
    real:: d ! great distance

    write(*,*) "Enter x1, y1, x2, y2: "
    read(*,*) x1,y1,x2,y2

    d = 60 * RadiantoDegree(acos(sin(DegreetoRadian(x1)) * sin(DegreetoRadian(x2)) + cos(DegreetoRadian(x1)) &
                    * cos(DegreetoRadian(x2))* cos(DegreetoRadian(y1 - y2))))

    write(*,*) "Great distance between ", x1, y1, "and", x2, y2, "is: ", d

end program great_circle