module &
        c
    interface work
        module procedure work_at_c
    end interface

    contains

    subroutine work_at_c
        use b, only: work_at_b
        call work_at_b()
    end subroutine
end module c

program main_c
    use c
    use a

    call work_at_c
    print *, n
end program
