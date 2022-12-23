program matrix
    implicit none

    ! Declare variables
    real:: a(2,2) ! 2 rows and 2 columns
    integer i,j

    read*, ((a(i,j), i=1,2), j=1,2)
   ! print*, ((a(i,j), i=1,2), j=1,2)
    ! To print in matrix form, use a do loop for j
    do j=1,2
        print*, (a(i,j), i=1,2)
    end do



end program matrix
