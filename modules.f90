module trigfunction
    implicit none

    ! Declare constants to be used in the module
    real, parameter:: Pi = 3.1415926
    real, parameter:: degree_180 = 180.0
    real, parameter:: rad_to_deg = degree_180 / Pi

contains ! seperates the global data from module procedures
    real function RadiantoDegree(radian)
        implicit none

        real, intent(in):: radian

        RadiantoDegree = radian * rad_to_deg

    end function RadiantoDegree
end module trigfunction