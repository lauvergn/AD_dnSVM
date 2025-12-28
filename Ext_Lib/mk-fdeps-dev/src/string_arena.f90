module string_arena_mod
    use, intrinsic :: iso_fortran_env, only: int32, int8

    private
    public :: string_arena_t, int32_to_iarr, iarr_to_int32
    
    integer, parameter :: init_size = 100 * 1024
    integer, parameter :: growth_factor = 2

    type :: string_arena_t
        integer(int8), allocatable :: buffer(:)
        integer(int32) :: cap, head, size

        contains

        procedure :: initialize, grow
        procedure :: insert, get, find
    end type

    contains

    subroutine initialize(self)
        class(string_arena_t) :: self
        
        allocate(self%buffer(init_size))
        self%cap = init_size
        self%head = 1
        self%size = 0
    end subroutine

    subroutine grow(self, target_cap)
        class(string_arena_t) :: self
        integer, intent(in) :: target_cap
        
        integer :: new_cap
        integer(int8), allocatable :: tmp(:)

        new_cap = max(self%cap * growth_factor, target_cap)
        allocate(tmp(new_cap))
        tmp(:self%cap) = self%buffer
        call move_alloc(from=tmp, to=self%buffer)

        self%cap = new_cap
    end subroutine

    subroutine insert(self, string)
        class(string_arena_t) :: self
        character(*), intent(in) :: string

        integer(int32) :: string_size, required_cap
        integer(int8) :: size_data(4), data(len(string))

        string_size = int(len(string), int32)

        required_cap = self%head + string_size + 4_int32
        if (required_cap > self%cap) then
            call self%grow(required_cap)
        end if
        
        ! Insert size data
        call int32_to_iarr(string_size, size_data)
        self%buffer(self%head : self%head + 3) = size_data
        self%head = self%head + 4

        ! Insert string
        data = transfer(string, data)
        self%buffer(self%head : self%head + string_size - 1) = data
        self%head = self%head + string_size

        self%size = self%size + 1
    end subroutine

    function get(self, idx) result(string)
        class(string_arena_t) :: self
        integer, intent(in) :: idx
        character(len=:), allocatable :: string

        integer(int8) :: size_data(4)
        integer(int32) :: i, pos, size

        pos = 1
        i = 1
        size = 0

        do while(i /= idx .and. pos <= self%head)
            size_data = self%buffer(pos : pos + 3)
            call iarr_to_int32(size_data, size)

            pos = pos + size + 4
            i = i + 1
        end do

        if (i == idx) then
            size_data = self%buffer(pos : pos + 3)
            call iarr_to_int32(size_data, size)
            pos = pos + 4

            allocate(character(len=size) :: string)
            string = transfer(self%buffer(pos: pos + size), string)
        end if
    end function 

    integer(int32) function find(self, string) result(i)
        class(string_arena_t) :: self
        character(*), intent(in) :: string

        integer(int8) :: size_data(4), data(len(string))
        integer(int32) ::  pos, size

        pos = 1
        size = 0
        i = 1

        data = transfer(string, data)

        do while(pos < self%head)
            size_data = self%buffer(pos : pos + 3)
            call iarr_to_int32(size_data, size)

            pos = pos + 4

            if (all(self%buffer(pos : pos + size - 1) == data)) then
                return
            end if

            pos = pos + size
            i = i + 1
        end do
        
        i = -1
    end function 

    subroutine int32_to_iarr(input, output)
        integer(int32), intent(in) :: input
        integer(int8), intent(out) :: output(:)
        output = transfer(input, output)
    end subroutine

    subroutine iarr_to_int32(input, output)
        integer(int8), intent(in) :: input(:)
        integer(int32), intent(out) :: output 
        output = transfer(input, output)
    end subroutine
end module
