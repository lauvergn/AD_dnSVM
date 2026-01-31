#include "defs.h"

module MOD_B_NAME 
    implicit none

    contains
    subroutine hello_world
        print *, "hello from ", MOD_B_NAME
    end subroutine
end module
