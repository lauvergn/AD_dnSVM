submodule (test) test_sub
    implicit none
    contains

    module subroutine print_n()
        print *, n
    end subroutine
end submodule
