module global
    implicit none
    save ! allows every section of the program to see the module
    integer, parameter :: x_end = 1000, t_end = 1000
    real*8, dimension(0:t_end,0:x_end) :: u = 0 ! u matrix with t rows, and x columns
    real*8, dimension(0:x_end) :: x
    !real*, dimension(0:t_end) :: t


end module global

program linear_advection
    use global
    implicit none

    real*8, parameter :: pi = 4.0*atan(1.0)
    real*8 :: bx = 1000.0, c = 2.0, bt = 1000.0, t1
    real*8 :: CFL, dt = 1.0, dx = 4.0
    integer :: Nt, Nx, i, j
    
    Nx = int(bx/dx)
    Nt = int(bt/dt)

    CFL = c*dt/dx

10 format(f30.10, 5x, f30.10)  

    x(0:Nx) = [(i*dx, i=0, Nx)]
    u(0,0:Nx) = [(sin(i*2*pi*dx/bx), i=0,Nx)]

    do j = 1, Nt
        t1 =  u(j-1, Nx) ! setting the value of the previous time step to t1
        do i = 1,Nx
            u(j, i) = (1 - cfl)*u(j-1, i) + cfl*u(j-1, i-1)
        end do
        u(j, 0) = t1 ! setting the starting point of the current time step
    end do

    do j = 0, Nt
        open(unit = 1, file = "data.txt")
        do i = 0, Nx
            write(unit=1,fmt=10) x(i), u(j,i)
        end do
        close(unit=1)
        call system("gnuplot plot_lae.gnu") ! calls bash command
    end do


end program linear_advection