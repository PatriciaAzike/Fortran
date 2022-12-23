program arrays
    implicit none

    ! Declare variables
    ! real :: a(-2:4)
    ! real, dimension(3) :: a ! here, you can just have the variable after specify-
    !                         ! ing the dimension    
    ! integer:: i,j

    ! a(1) = 2.5
    ! a(2) = 5.3
    ! a(3) = 6.3

    ! You can allow the user to enter the objects using two methods
    ! 1. do loop
    ! do i=1,3
    !     read*, a(i)
    ! end do

    ! 2. using only the read function
    ! read*, (a(i), i=1,3)

    !3. dynamic allocation of dimension
    real, allocatable, dimension(:) :: a
    integer:: i,n ! n is the dimension to be allocated

    read*, n
    allocate(a(n))
    read*, (a(i), i=1,3)

    print*, "a = ", a

    deallocate(a)
    !print*, a(-1)





end program arrays