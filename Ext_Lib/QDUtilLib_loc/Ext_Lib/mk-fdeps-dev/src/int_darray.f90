module int_darray_mod
    use, intrinsic :: iso_fortran_env, only: int32, int8

    private
    public :: int_darray_t, int_arena_t
    
    integer, parameter :: init_size = 1024
    integer, parameter :: growth_factor = 2

    type :: int_darray_t
        integer(int32), allocatable :: buffer(:)
        integer(int32) :: cap, size

        contains

        procedure :: initialize, grow
        procedure :: insert, reset
    end type

    type, extends(int_darray_t) :: int_arena_t
        integer(int32) :: count

        contains

        procedure :: insert => insert_array
        procedure :: get
    end type

    contains

    subroutine initialize(self)
        class(int_darray_t) :: self
        
        allocate(self%buffer(init_size))
        self%cap = init_size
        self%size = 0
    end subroutine

    subroutine grow(self, target_cap)
        class(int_darray_t) :: self
        integer, intent(in) :: target_cap
        
        integer :: new_cap
        integer(int32), allocatable :: tmp(:)

        new_cap = max(self%cap * growth_factor, target_cap)
        allocate(tmp(new_cap))
        tmp(:self%cap) = self%buffer
        call move_alloc(from=tmp, to=self%buffer)

        self%cap = new_cap
    end subroutine

    subroutine insert(self, vals)
        class(int_darray_t) :: self
        integer(int32) :: vals(:)

        integer(int32) :: required_cap

        required_cap = self%size + size(vals)
        if (required_cap > self%cap) then
            call self%grow(required_cap)
        end if
        
        ! Insert 
        self%buffer(self%size + 1 : self%size + size(vals)) = vals
        self%size = self%size + size(vals)
    end subroutine

    subroutine insert_array(self, vals)
        class(int_arena_t) :: self
        integer(int32) :: vals(:)

        if (size(vals) > 1) then
            call insert(self, [size(vals)])
            call insert(self, vals)
            self%count = self%count + 1
        end if
    end subroutine

    logical function get(self, idx, istart, iend)
        class(int_arena_t) :: self
        integer, intent(in) :: idx
        integer, intent(out) :: istart, iend

        integer :: i

        i = 0
        istart = 0 ; iend = 0
        do while(i < idx)
            istart = 1 + iend
            iend   = 1 + iend + self%buffer(istart - 1) 
            i = i + 1 

            if (istart >= self%size) then
                get = .false.
                return
            end if
        end do

        get = .true.
    end function 

    subroutine reset(self)
        class(int_darray_t) :: self
        self%size = 0
    end subroutine
end module
