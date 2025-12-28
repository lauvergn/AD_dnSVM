module minimal_parser_mod 
    use lexer_mod    
    implicit none

    private
    public :: minimal_parser_t

    type :: minimal_parser_t
        type(lexer_t) :: lexer

        contains 

        procedure :: process_file
        procedure :: on_file => empty_on_file
        procedure :: on_module => empty_on_module
        procedure :: on_submodule => empty_on_submodule
        procedure :: on_program => empty_on_program
        procedure :: on_use => empty_on_use
    end type

    contains

    subroutine process_file(self, filepath)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath 

        character(len = :), allocatable :: name, ancestor, parent
        logical :: first, ok

        associate (lexer => self%lexer)
        
        call lexer%load_file(filepath)
        if (self%on_file(filepath)) return
        first = .true.

        do while(.not. lexer%is_eof())
            if (first) then
                first = .false.
                call lexer%skip_extra()
            else
                call lexer%skip_line()
            end if

            ! Parse module declaration
            if (lexer%accept("module")) then
                if (.not. lexer%accept_name(name)) cycle

                if (.not. is_eol(lexer%current())) cycle

                ! Dispatch event
                if (self%on_module(filepath, name)) exit
                cycle
            end if

            ! Parse program declaration
            if (lexer%accept("program")) then
                if (lexer%accept_name(name)) then
                    if (self%on_program(filepath, name)) exit
                else if (is_eol(lexer%current())) then
                    name = "MAIN" // "$" //filepath
                    if (self%on_program(filepath, name)) exit
                end if
                cycle
            end if

            ! Parse use statement
            if (lexer%accept("use")) then
                if (lexer%accept(",")) then
                    ok = lexer%accept_name(name)
                end if

                ok = lexer%accept("::")

                if (lexer%accept_name(name)) then
                    ! Dispatch event
                    if (self%on_use(filepath, name)) exit
                end if

                cycle
            end if

            ! Parse submodule
            if (lexer%accept("submodule")) then
                if (.not. lexer%accept("(")) cycle

                if (.not. lexer%accept_name(ancestor)) cycle

                parent = ""
                if (lexer%accept(":")) then
                    if (.not. lexer%accept_name(parent)) cycle
                end if

                if (.not. lexer%accept(")")) cycle

                if (.not. lexer%accept_name(name)) cycle

                if (self%on_submodule(filepath, ancestor, parent, name)) exit

                cycle
            end if
        end do

        end associate
    end subroutine

    logical function empty_on_file(self, filepath) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.
    end function

    logical function empty_on_file_end(self, filepath) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.
    end function

    logical function empty_on_module(self, filepath, name) result(abort) 
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, name
        abort = .false.
    end function

    logical function empty_on_module_end(self, filepath) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.
    end function

    logical function empty_on_submodule(self, filepath, ancestor, parent, name) result(abort) 
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, ancestor, parent, name
        abort = .false.
    end function

    logical function empty_on_program(self, filepath, name) result(abort) 
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, name
        abort = .false.
    end function

    logical function empty_on_use(self, filepath, name) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, name
        abort = .false.
    end function
end module
