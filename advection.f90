 module global_var
    implicit none
    save
    integer,parameter :: t1=0, t2=1, x1=0 , x2 = 1
    real, parameter :: dt = 0.01, dx = 0.1
    integer, parameter :: Nx=int((x2 - x1)/dx) , Nt=int((t2-t1)/dt)

    
end module global_var

program advection
    use global_var
    implicit none

    real :: x(Nx+1), t(Nt+1), mu(Nx+1),a(Nx+1), initial(Nx+1), bc(Nt+1)
    !real, parameter:: u(Nt+1, Nx+1) = 0
    real :: u(Nt+1, Nx+1), exact(Nt+1, Nx+1), error(Nt+1, Nx+1)
    integer :: i,j


    do i = 1,Nx+1
        x(i) = (i-1)*dx
        
    end do

    do j = 1,Nt+1
        t(j) = (j-1)*dt
        bc = exp(-t)
    end do

    initial = x + 1
    a = x + 1
    mu = a*dt/dx

10 format(f30.10, 5x, f30.10)

    ! Exact solution
    do j = 1,Nt+1
        do i = 1,Nx+1
            exact(j, i) = (x(i)+1) * exp(-t(j))
        end do
    end do


    do j = 1,Nt+1
        do i = 1,Nx+1
            u(j, i) = 0
        end do
    end do

    u(1,:) = initial
    u(:,1) = bc

    do j = 1,Nt ! that is len(t) - 1 = (Nt+1) -1
        do i = 2, Nx+1 ! that is len(x)
            u(j+1, i) = (1 - mu(i))*u(j,i) + mu(i)*u(j, i-1)
        end do
    end do

    open(UNIT=12, FILE="aoutput.txt", ACTION="write", STATUS="replace")
    do i=1,Nt+1
        write(12,*) (u(i,j), j=1,Nx+1)
    end do

    ! do j = 1, Nt+1
    !     open(unit = 1, file = "advection.txt")
    !     do i = 1, Nx+1
    !         write(unit=1,fmt=10) u(j+1,i)
    !     end do
    !     close(unit=1)
    !     !call system("gnuplot plot_lae.gnu") ! calls bash command
    ! end do

    
   


    
    
end program advection


