program variables
    implicit none ! without this, you don't have to declare the variables.
    ! If you don't declare implicit none, Fortran tries to figure out what your variable type is. 
    ! In the event that you do declare, you have the upper hand as Fortran reads it just the way you have declared .

    ! Variable declaration
    real :: x,y
    integer :: i,j
    complex :: z,w
    logical :: tof
    !character :: myname ! this will be a single character. to have more than one character,
    character(len=*), parameter :: myname = "Tricia" ! this allocates space for the length of character specified. One can also use a number to specify
    ! note that the line above will ensure that no other name can be eneter. Hence myname has to be defined as a parameter
    x = 3.0
    y = 10.0

    ! write(*,*) is what reads the instruction in the code
    ! write(*,*) "x + y: ", x + y
    ! write(*,*) "x - y: ", x - y
    ! write(*,*) "x * y: ", x * y
    ! write(*,*) "x / y: ", x / y
    ! write(*,*) "x**y: ", x**y

    ! Handling complex operations
    z = cmplx(1.0,1.0)
    w = cmplx(3.0)

    write(*,*) "w**z: ", w**z
    write(*,*) "real part of z: ", real(z)
    write(*,*) "imaginary part of z: ", imag(z) 
    write(*,*) "square root of z: ", sqrt(z)
    write(*,*) "complex conjugate of z: ", conjg(z)
    write(*,*) "exp(z): ", exp(z) 


end program variables