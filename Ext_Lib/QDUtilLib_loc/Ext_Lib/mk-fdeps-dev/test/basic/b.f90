Module b
    Use a
    USE a, only: n
    ! use c
    
    contains

    subroutine work_at_b()
        ! use d
        print *, "hello world", n
    end subroutine
end module
