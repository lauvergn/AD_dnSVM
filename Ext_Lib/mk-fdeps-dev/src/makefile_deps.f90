module makefile_deps_mod
    use, intrinsic :: iso_fortran_env, only: int32, int8
    use minimal_parser_mod
    use hash_table_mod
    use string_arena_mod
    use int_darray_mod
    use graph_mod
    implicit none

    private
    public :: makefile_deps_t, clean_path, join_path

    type, extends(minimal_parser_t) :: makefile_deps_t
        type(string_arena_t) :: file_names, punit_names
        type(hash_table_t) :: files, puntis, dependencies, providers, targets, submodules
        integer(int32) :: current_file, current_module

        character(len=:), allocatable :: with_parent, with_ext
        character(len=:), allocatable :: with_suffix, with_prefix
        integer :: strip_parents = 1
        logical :: include_targets = .false., replace_ext = .false.

        contains

        procedure :: initialize 
        procedure :: on_file, on_file_end, on_module, on_module_end, on_use, on_program, on_submodule
        procedure :: add_file, add_punit 
        procedure :: export, format_path
        procedure, private :: expand_submodule_dependencies, write_file
    end type

    contains

    subroutine initialize(self)
        class(makefile_deps_t) :: self

        call self%files%initialize()
        call self%puntis%initialize()
        call self%dependencies%initialize()
        call self%submodules%initialize()
        call self%providers%initialize()
        call self%targets%initialize()

        call self%file_names%initialize()
        call self%punit_names%initialize()

        self%with_ext = ".o"
        self%replace_ext = .true.
        self%with_parent = "build/"

        self%lexer%preprocessor_cmd = "gfortran -cpp -E"
    end subroutine

    function format_path(self, path) result(output_path)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: path
        character(len=:), allocatable :: output_path

        integer :: i, parent_start, parent_end, stem_end
        
        output_path = ""
        if (len_trim(self%with_parent) > 0) then
            output_path = output_path // self%with_parent
        end if

        i = 0 
        parent_start = 1
        do while(i < self%strip_parents)
            parent_start = parent_start + index(path(parent_start:), "/")
            i = i + 1
        end do
        
        parent_end = index(path, "/", back=.true.)
        parent_end = max(parent_end, 1)

        stem_end = parent_end + index(path(parent_end:), '.') - 1
        if (stem_end == parent_end) stem_end = len(path)

        output_path = output_path &
                        // path(parent_start:parent_end) &
                        // self%with_prefix &
                        // path(parent_end + 1:stem_end - 1) &
                        // self%with_suffix

        if (self%replace_ext) then
            output_path = output_path // self%with_ext
        else 
            output_path = output_path // path(stem_end + 1:)
        end if
    end function

    function clean_path(path) result(output_path)
        character(*), intent(in) :: path
        character(len=:), allocatable :: output_path

        integer :: istart, iend, pos, slice_len
        
        allocate(character(len = len(path)) :: output_path)

        istart = 1
        pos = 1

        do while (istart < len(path))
            do while (path(istart + 1:istart + 1) == '/' .and. istart + 1 < len(path))
                istart = istart + 1
            end do

            iend = istart + index(path(istart + 1:), "/")
            if (iend <= istart) iend = len(path)

            slice_len = iend - istart
            output_path(pos:pos + slice_len) = path(istart:iend)
            pos = pos + slice_len

            istart = iend
        end do

        output_path = adjustl(trim(output_path))

        if (output_path(pos:pos) == '/') then
            output_path = output_path(:pos - 1)
        end if
    end function

    function join_path(a, b) result(output_path)
        character(*), intent(in) :: a, b
        character(len=:), allocatable :: output_path

        integer :: istart, iend

        iend = len(a)
        do while(iend > 1 .and. a(iend:iend) == '/')
            iend = iend - 1
        end do

        istart = len(b)
        do while(istart + 1 < len(b) .and. b(istart:istart) == '/')
            istart = istart + 1
        end do

        output_path = a(:iend) // "/" // b(istart:)
    end function

    subroutine export(self, output)
        use iso_fortran_env, only: output_unit, error_unit
        class(makefile_deps_t) :: self
        character(*), intent(in) :: output
        
        integer(int32), allocatable :: provider(:), dependencies(:, :)
        integer, allocatable :: csr_dep_offset(:), csr_dep(:)
        logical, allocatable :: has_target(:)
        character(len=:), allocatable :: file, ext
        character(len=:), allocatable :: module, other_file

        type(bfs_iterator_t) :: bfs_iterator
        integer :: istart, iend

        integer :: i, j, mod_id, submod_id, file_id, n_deps, n_sub_deps
        logical :: first
        integer :: unit

        call self%expand_submodule_dependencies()

        allocate(dependencies(2, self%dependencies%count))
        dependencies = 0    

        ! Find edges
        j = 1
        i = 0
        do while(self%dependencies%next(i))
            associate (p_entry => self%dependencies%entries(i)) 
                call decode_dependency_key(p_entry%key, file_id, mod_id)
            end associate
            dependencies(1, j) = file_id
            dependencies(2, j) = mod_id
            j = j + 1
        end do

        ! Find providers
        allocate(provider(self%puntis%count))
        provider = 0

        i = 0
        do while(self%providers%next(i))
            associate (p_entry => self%providers%entries(i)) 
                call decode_dependency_key(p_entry%key, file_id, mod_id)
            end associate
            
            if (provider(mod_id) == 0) then
                provider(mod_id) = file_id
            else
                module = self%punit_names%get(mod_id)
                file = self%file_names%get(provider(mod_id))
                other_file = self%file_names%get(provider(file_id))

                101 FORMAT('Module ', A, ' is provided by: ', A, ' but is also declared in: ', A)
                write(error_unit, 101) module, file, other_file
            end if
        end do

        ! Resolve dependencies to files
        dependencies(2, :) = provider(dependencies(2, :))
        dependencies(2, :) = merge(0, dependencies(2, :), dependencies(1, :) == dependencies(2, :))

        call to_csr_format(dependencies, self%files%count, csr_dep_offset, csr_dep)

        ! Export object rules
        unit = output_unit
        if (len_trim(output) > 0) then
            open(newunit = unit, file = output, status="replace")
        end if

        do i = 1, self%files%count
            istart = csr_dep_offset(i)
            if (i < self%files%count) then
                iend = csr_dep_offset(i + 1) - 1
            else
                iend = size(csr_dep)
            end if

            if (all(csr_dep(istart:iend) == 0)) cycle

            call self%write_file(unit, i)
            write(unit, "(A)", advance="no") ":"

            do j = istart, iend
                if (csr_dep(j) == 0) cycle
                write(unit, "(A)", advance="no") " "
                call self%write_file(unit, csr_dep(j))
            end do

            write(unit, *)
        end do

        if (self%include_targets) then
            call bfs_iterator%initialize(self%files%count)
            write(unit, *)

            i = 0
            do while(self%targets%next(i))
                associate (p_entry => self%targets%entries(i)) 
                    call decode_dependency_key(p_entry%key, file_id, mod_id)
                end associate

                ! Write main dependency
                call self%write_file(unit, file_id, "")
                write(unit, "(A)", advance="no") ": "

                call bfs_iterator%start(file_id)

                do while(bfs_iterator%next(file_id, csr_dep_offset, csr_dep))
                    if (file_id == 0) cycle
                    
                    call self%write_file(unit, file_id)
                    write(unit, "(A)", advance="no") " "
                end do

                write(unit, *)
            end do
        end if

        if (unit /= output_unit) then
            close(unit)
        end if
    end subroutine

    subroutine write_file(self, unit, file_id, custom_ext)
        class(makefile_deps_t) :: self
        integer, intent(in) :: unit, file_id
        character(*), optional, intent(in) :: custom_ext

        character(len=:), allocatable :: filepath, ext

        filepath = self%file_names%get(file_id)
        if (.not. present(custom_ext)) then
            write(unit, "(A)", advance = 'no') self%format_path(filepath)
        else 
            ext = self%with_ext
            self%with_ext = custom_ext
            write(unit, "(A)", advance = 'no') self%format_path(filepath)
            self%with_ext = ext
        end if
        
    end subroutine

    subroutine expand_submodule_dependencies(self)
        class(makefile_deps_t) :: self

        type(int_darray_t), allocatable :: module_sub_deps(:), sub_deps(:)
        integer(int8) :: key(8)
        integer :: file_id, mod_id, submod_id, dep_id, i, j

        ! Save submodule dependencies
        allocate(module_sub_deps(self%punit_names%size))
        do i = 1, size(module_sub_deps)
            call module_sub_deps(i)%initialize()
        end do

        i = 0
        do while(self%submodules%next(i))
            associate (e => self%submodules%entries(i)) 
                call decode_dependency_key(e%key, submod_id, mod_id)
            end associate
            call module_sub_deps(mod_id)%insert([submod_id])
        end do

        ! Add extra dependencies for submodules
        allocate(sub_deps(self%file_names%size)) 
        do i = 1, size(sub_deps)
            call sub_deps(i)%initialize()
        end do

        i = 0
        do while(self%dependencies%next(i))
            associate (e => self%dependencies%entries(i)) 
                call decode_dependency_key(e%key, file_id, mod_id)
            end associate

            associate (deps => module_sub_deps(mod_id))
                call sub_deps(file_id)%insert(deps%buffer(:deps%size))
            end associate
        end do

        ! Edit the dependency graph
        do file_id = 1, self%file_names%size
            do j = 1, sub_deps(file_id)%size
                call encode_dependency_key(file_id, sub_deps(file_id)%buffer(j), key)
                dep_id = self%dependencies%insert(key)
            end do
        end do
    end subroutine

    logical function on_file(self, filepath) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.

        self%current_file = self%add_file(clean_path(filepath), existed = abort)
    end function

    logical function on_file_end(self, filepath) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.

        self%current_file = 0
    end function

    logical function on_module(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name

        integer(int32) :: dep_id
        integer(int8) :: key(8)
        abort = .false.

        self%current_module = self%add_punit(name)
        call encode_dependency_key(self%current_file, self%current_module, key)
        dep_id = self%providers%insert(key)
    end function

    logical function on_module_end(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name
        integer :: other_file_id
        character(len=:), allocatable :: other_file
        logical :: existed
        abort = .false.

        self%current_module = self%add_punit(name)
    end function

    logical function on_program(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name

        integer(int32) :: dep_id
        integer(int8) :: key(8)
        abort = .false.

        call encode_dependency_key(self%current_file, 0, key)
        dep_id = self%targets%insert(key)
    end function

    logical function on_submodule(self, filepath, ancestor, parent, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, ancestor, parent, name
        
        integer :: punit_id, submod_id, dep_id
        integer(int8) :: key(8)
        abort = .false.

        submod_id = self%add_punit(name)
        punit_id  = self%add_punit(ancestor)

        call encode_dependency_key(self%current_file, submod_id, key)
        dep_id = self%providers%insert(key)

        ! Add submodule host dependency
        call encode_dependency_key(submod_id, punit_id, key)
        dep_id = self%submodules%insert(key)

        call encode_dependency_key(self%current_file, punit_id, key)
        dep_id = self%dependencies%insert(key)

        ! Add submodule ancestor dependency
        if (len_trim(parent) > 0) then
            punit_id = self%add_punit(parent)
            call encode_dependency_key(self%current_file, punit_id, key)
            dep_id = self%dependencies%insert(key)
        end if
         
    end function

    logical function on_use(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name
        
        integer :: punit_id, dep_id
        integer(int8) :: key(8)
        abort = .false.
        
        punit_id = self%add_punit(name)
        call encode_dependency_key(self%current_file, punit_id, key)
        dep_id = self%dependencies%insert(key)
    end function

    integer function add_file(self, name, existed) result(id)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: name
        logical, intent(out) :: existed

        id = self%files%insert(name, existed = existed)
        if (.not. existed) call self%file_names%insert(name)
    end function

    integer function add_punit(self, name) result(id)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: name

        logical :: existed

        id = self%puntis%insert(name, existed = existed)
        if (.not. existed) call self%punit_names%insert(name)
    end function

    subroutine encode_dependency_key(from, to, key)
        integer(int32), intent(in) :: from, to     
        integer(int8), intent(out) :: key(8)
        
        call int32_to_iarr(from, key(1:4))
        call int32_to_iarr(to, key(5:8))
    end subroutine

    subroutine decode_dependency_key(key, from, to)
        integer(int8), intent(in) :: key(8)
        integer(int32), intent(out) :: from, to     
        
        call iarr_to_int32(key(1:4), from)
        call iarr_to_int32(key(5:8), to)
    end subroutine
end module
