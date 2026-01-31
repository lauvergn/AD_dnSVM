module string_builder_mod
    use, intrinsic :: iso_fortran_env, only: int32, int8

    private
    public :: string_builder_t
    
    integer, parameter :: init_size = 1024
    integer, parameter :: growth_factor = 2

    type :: string_builder_t
        character(len=:), allocatable :: buffer
        integer(int32) :: cap, size

        contains

        procedure :: initialize, grow
        procedure :: append, reset
    end type

    contains

    subroutine initialize(self)
        class(string_builder_t) :: self
        
        allocate(character(len=init_size) :: self%buffer)
        self%cap = init_size
        self%size = 0
    end subroutine

    subroutine grow(self, target_cap)
        class(string_builder_t) :: self
        integer, intent(in) :: target_cap
        
        integer :: new_cap
        character(len=:), allocatable :: tmp

        new_cap = max(self%cap * growth_factor, target_cap)
        allocate(character(len = new_cap) :: tmp)
        tmp(:self%cap) = self%buffer
        call move_alloc(from=tmp, to=self%buffer)

        self%cap = new_cap
    end subroutine

    subroutine append(self, str)
        class(string_builder_t) :: self
        character(*) :: str

        integer(int32) :: required_cap

        required_cap = self%size + len(str)
        if (required_cap > self%cap) then
            call self%grow(required_cap)
        end if
        
        ! Insert 
        self%buffer(self%size + 1 : self%size + len(str)) = str
        self%size = self%size + len(str)
    end subroutine

    subroutine reset(self)
        class(string_builder_t) :: self
        self%size = 0
    end subroutine
end module
