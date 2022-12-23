program ordercheck
    implicit none

    ! Declaration of variables
    !integer, parameter:: dp = selected_real_kind(15) : double precision
    real:: x,y,z
    write(*,*) "Enter x, y, and z: "
    read(*,*) x,y,z

    mainCond:if ((x<y) .and. (y<z) .or. (x>y) .and. (y>z)) then
        write(*,*) "True"
    else
        write(*,*) "False"
    end if mainCond



end program ordercheck
