#include "defs.h"

module MOD_A_NAME 
    implicit none

    contains
    subroutine hello_world
        print *, "hello from ",  MOD_A_NAME
    end subroutine
end module
