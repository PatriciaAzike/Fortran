program loops
    implicit none

    ! declare variable
    integer :: i, n

    ! print*, "Enter the last value: "
    ! read(*,*) n

    ! do loop
    ! do i=1,n,2 ! if the stepsize isn't specified, then fortran asumes the step is 1
    !     print*, i !write(*,*) i 
    ! end do

    ! do while loop
    ! i = 1
    ! do while(i<=10)
    !     print*, i
    !     i = i + 2 ! heere, you can specify the increment
    ! end do

    ! infinite do loop
    i = 1
    do
        print*, i
        if (i==30) exit ! this prevents it from running infinitely
        i = i+1 ! increment
    end do



end program loops