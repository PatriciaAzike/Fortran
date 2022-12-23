program helloworld
    implicit none

    character(len=20) :: myname  ! with the len of character, you are only allowed to enter a string with that length or less
    ! in order to fix this, include "trim" in front of the variable being entered

!    write(*,*) "Hello world!"  ! prints hello world to the screen
    write(*,*) "What is your name?"
    read(*,*) myname
    write(*,*) "Hi ", trim(myname), ", it's nice to meet you"



end program helloworld