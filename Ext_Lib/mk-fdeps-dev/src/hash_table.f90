module hash_table_mod
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64
    implicit none

    private
    public :: hash_table_t, hash

    interface hash
        module procedure hash_str
        module procedure hash_iarr
    end interface

    integer(int64), parameter :: fnv_offset = int(z'CBF29CE484222325', int64)
    integer(int64), parameter :: fnv_prime = int(z'100000001B3', int64)

    type entry_t
        logical :: occupied = .false.
        integer(int32) :: id = -1, data
        integer(int8), allocatable :: key(:)

        contains

        procedure :: has_key_str, has_key_iarr
        generic :: has_key => has_key_str, has_key_iarr
    end type

    integer, parameter :: init_size = 4096
    integer, parameter :: growth_factor = 2

    type hash_table_t
        type(entry_t), allocatable :: entries(:)
        integer :: cap, count

        contains

        procedure :: initialize, grow
        procedure :: get_slot, next

        procedure :: get_data_iarr, get_data_str, get_data_idx
        generic :: get_data => get_data_iarr, get_data_str, get_data_idx

        procedure :: insert_iarr, insert_str
        generic :: insert => insert_iarr, insert_str

        procedure :: get_id_iarr, get_id_str, get_id_idx
        generic :: get_id => get_id_iarr, get_id_str, get_id_idx
    end type

    contains

    logical function has_key_str(self, key) result(has_key)
        class(entry_t) :: self
        character(*) :: key
        integer(int8) :: bytes(len(key))

        bytes = transfer(key, bytes)

        has_key = .false.

        if (self%occupied .and. allocated(self%key)) then
            has_key = all(self%key == bytes)
        end if
    end function

    logical function has_key_iarr(self, key) result(has_key)
        class(entry_t) :: self
        integer(int8) :: key(:)

        has_key = .false.

        if (self%occupied .and. allocated(self%key)) then
            has_key = all(self%key == key)
        end if
    end function

    subroutine initialize(self)
        class(hash_table_t) :: self
        
        allocate(self%entries(init_size))
        self%cap = init_size
        self%count = 0
    end subroutine

    subroutine grow(self)
        class(hash_table_t) :: self

        type(entry_t), allocatable :: tmp(:)
        integer :: new_cap, old_cap, i, dummy
        logical :: existed

        old_cap = size(self%entries)
        new_cap = old_cap * growth_factor
        self%cap = new_cap
        
        ! Rebuild table
        call move_alloc(from=self%entries, to=tmp)
        allocate(self%entries(new_cap))

        do i = 1, old_cap
            if (tmp(i)%occupied) then
                dummy = self%insert_iarr(tmp(i)%key, tmp(i)%data)
            end if
        end do
    end subroutine

    function get_slot(self, key, hashed) result(slot)
        class(hash_table_t) :: self
        integer(int8) :: key(:)
        integer(int64) :: hashed
        integer :: slot, i, init_i

        slot = 0
        i = int(iand(hashed, int(self%cap - 1, int64))) + 1

        init_i = i
        do while( self%entries(i)%occupied .and. (.not. self%entries(i)%has_key(key)) )
            i = next_mod(i, self%cap)
            if (i == init_i) return
        end do

        slot = i
    end function

    logical function get_data_iarr(self, key, data) result(found)
        class(hash_table_t) :: self
        integer(int8) :: key(:)
        integer(int32), intent(out) :: data

        integer :: id
        logical :: found_internal

        integer(int64) :: hashed
        hashed = hash_iarr(key)
        found = get_data(self, key, hashed, data)
    end function

    logical function get_data_str(self, key, data) result(found)
        class(hash_table_t) :: self
        character(*), intent(in) :: key
        integer(int32), intent(out) :: data

        integer(int64) :: hashed
        integer(int8), allocatable :: key_iarr(:)

        logical :: found_internal

        allocate(key_iarr(len(key)) )
        key_iarr = transfer(key, key_iarr)

        hashed = hash_str(key)
        found = get_data(self, key_iarr, hashed, data)
    end function

    logical function get_data(self, key, hashed, data) result(found)
        class(hash_table_t) :: self
        integer(int8), intent(in) :: key(:)
        integer(int64), intent(in) :: hashed
        integer(int32), intent(out) :: data

        integer :: i

        i = self%get_slot(key, hashed)
        if (.not. self%entries(i)%occupied) then
            found = .true.
            data = self%entries(i)%data
        end if
    end function

    logical function get_data_idx(self, i, data) result(found)
        class(hash_table_t) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(out) :: data

        found = .false.
        if (i < 0 .or. i > self%cap) return

        found = self%entries(i)%occupied
        data = self%entries(i)%data
    end function

    integer function insert_iarr(self, key, data, existed) result(id)
        class(hash_table_t) :: self
        integer(int8), intent(in) :: key(:)
        integer, optional, intent(in) :: data
        logical, optional, intent(out) :: existed

        integer(int64) :: hashed
        integer :: internal_data
        logical :: internal_existed

        internal_data = 0
        if (present(data)) internal_data = data

        hashed = hash_iarr(key)
        id = insert(self, key, hashed, internal_data, internal_existed)
        if (present(existed)) existed = internal_existed
    end function

    integer function insert_str(self, key, data, existed) result(id)
        class(hash_table_t) :: self
        character(*) :: key
        integer, optional, intent(in) :: data
        logical, optional, intent(out) :: existed

        integer(int8), allocatable :: key_iarr(:)
        integer(int64) :: hashed
        integer :: internal_data
        logical :: internal_existed

        hashed = hash_str(key)

        allocate(key_iarr(len(key)))
        key_iarr = transfer(key, key_iarr)

        internal_data = 0
        if (present(data)) internal_data = data

        hashed = hash_str(key)
        id = insert(self, key_iarr, hashed, internal_data, internal_existed)
        if (present(existed)) existed = internal_existed
    end function

    integer function insert(self, key, hashed, data, existed) result(id)
        class(hash_table_t) :: self
        integer(int8), intent(in) :: key(:)
        integer(int64), intent(in) :: hashed
        integer, intent(in) :: data
        logical, intent(out) :: existed

        integer :: i

        i = self%get_slot(key, hashed)

        if (i == 0) then
            call self%grow()
            i = self%get_slot(key, hashed)
        end if        

        if (.not. self%entries(i)%occupied) then
            self%count = self%count + 1

            self%entries(i)%occupied = .true.
            self%entries(i)%key = key
            self%entries(i)%id = self%count

            self%entries(i)%data = data

            existed = .false.
        else
            existed = .true.
        end if

        id = self%entries(i)%id
    end function

    integer function get_id_iarr(self, key) result(id)
        class(hash_table_t) :: self
        integer(int8) :: key(:)
        integer(int64) :: hashed

        hashed = hash_iarr(key)

        id = get_id(self, key, hashed)
    end function

    integer function get_id_str(self, key) result(id)
        class(hash_table_t) :: self
        character(*), intent(in) :: key

        integer(int8), allocatable :: key_iarr(:)
        integer(int64) :: hashed

        hashed = hash_str(key)

        allocate(key_iarr(len(key)))
        key_iarr = transfer(key, key_iarr)

        id = get_id(self, key_iarr, hashed)
    end function

    integer function get_id(self, key, hashed) result(id)
        class(hash_table_t) :: self
        integer(int8), intent(in) :: key(:)
        integer(int64), intent(in) :: hashed

        integer :: i

        id = -1
        i = self%get_slot(key, hashed)
        if (.not. self%entries(i)%occupied) then
             id = self%entries(i)%id
        end if
    end function

    integer function get_id_idx(self, i) result(id)
        class(hash_table_t) :: self
        integer(int32), intent(in) :: i

        id = -1
        if (i < 0 .or. i > self%cap) return

        if (.not. self%entries(i)%occupied) then
             id = self%entries(i)%id
        end if
    end function

    logical function next(self, i)
        class(hash_table_t) :: self
        integer, intent(inout) :: i
        
        next = .false.
        i = i + 1
        do while (i <= self%cap)
            if (self%entries(i)%occupied) then
                next = .true.
                return
            end if
            i = i + 1
        end do
    end function

    integer(int64) function hash_str(input)
        character(*) :: input
        integer(int8) :: bytes(len(input))
        integer :: i

        bytes = transfer(input, bytes)
        hash_str = hash_iarr(bytes)
    end function 

    integer(int64) function hash_iarr(bytes) result(hash)
        integer(int8) :: bytes(:)
        integer :: i

        hash = fnv_offset
        do i = 1, size(bytes)
            hash = ieor(hash, int(bytes(i), int64))
            hash = hash * fnv_prime
        end do
    end function 

    integer function next_mod(i, n)
        integer, intent(in) :: i, n
        next_mod = modulo(i, n) + 1
    end function
end module

