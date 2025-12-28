program mk_fdeps
    use makefile_deps_mod 
    use string_arena_mod 

    type(makefile_deps_t) :: makefile_deps 
    type(string_arena_t) :: sources 

    character(len=:), allocatable :: source, output
    integer :: n_args, i
    logical :: ok

    call sources%initialize()
    call makefile_deps%initialize()

    call parse_args()
    if (sources%size == 0) then
        call print_help()
        call exit(1)
    end if

    do i = 1, sources%size 
        source = sources%get(i)
        call makefile_deps%process_file(source)
    end do

    call makefile_deps%export(output)

    contains

    subroutine get_arg(i, arg)
        integer, intent(in) :: i
        character(len=:), allocatable, intent(out) :: arg

        integer :: len
        call get_command_argument(i, length = len)
        allocate(character(len=len) :: arg)
        call get_command_argument(i, arg)
    end subroutine

    subroutine parse_args()
        integer :: n_args, i
        logical :: ok
        character(len=:), allocatable :: arg

        n_args = command_argument_count()
        i = 1

        do while(i <= n_args)
            call get_arg(i, arg); i = i + 1

            if (index(arg, "--") /= 1) then
                call sources%insert(arg)
            else 
                if (arg == "--with-prefix") then
                    if (i > n_args) call fail("Missing argument of --with-prefix")
                    
                    call get_arg(i, arg); i = i + 1
                    makefile_deps%with_prefix = arg

                else if (arg == "--with-suffix") then
                    if (i > n_args) call fail("Missing argument of --with-suffix")
                    
                    call get_arg(i, arg); i = i + 1
                    makefile_deps%with_suffix = arg

                else if (arg == "--with-parent") then
                    if (i > n_args) call fail("Missing argument of --with-parent")
                    
                    call get_arg(i, arg); i = i + 1
                    makefile_deps%with_parent = arg

                else if (arg == "--with-ext") then
                    if (i > n_args) call fail("Missing argument of --with-ext")
                    
                    call get_arg(i, arg); i = i + 1
                    makefile_deps%with_ext = arg
                    makefile_deps%replace_ext = .true.

                else if (arg == "--strip-parents") then
                    if (i > n_args) call fail("Missing argument of --strip-parents")
                    
                    call get_arg(i, arg); i = i + 1
                    makefile_deps%strip_parents = parse_int(arg)

                    if (makefile_deps%strip_parents < 0) call fail("Invalid value for --strip-parents: " // arg)
                else if (arg == "--include-targets") then

                    makefile_deps%include_targets = .true.

                else if (arg == "--output") then
                    if (i > n_args) call fail("Missing argument of --output")
                    
                    call get_arg(i, output); i = i + 1

                else if (arg == "--preprocess") then
                    makefile_deps%lexer%preprocess = .true.

                else if (arg == "--preprocessor-cmd") then
                    if (i > n_args) call fail("Missing argument of --preprocessor-cmd")

                    call get_arg(i, arg); i = i + 1
                    makefile_deps%lexer%preprocessor_cmd = arg

                else if (arg == "--help") then
                    call print_help()
                    call exit(0)

                else 
                    call fail("Unrecognized option: " // arg)
                end if
            end if
        end do
    end subroutine

    subroutine print_help()
        print *, "usage: mk-fdeps [source / option] ..."
        print *, "Options:"

        101 FORMAT(A20, 2x, A)
        print 101, "--with-prefix", "Add prefix to the file stem: parent/[prefix]stem.ext"
        print 101, "", "type: string"

        print 101, "--with-suffix", "Add suffix to the file stem: parent/stem[suffix].ext"
        print 101, "", "type: string"

        print 101, "--with-parent", "Add parent to the whole path: [new-parent]parent/stem.ext"
        print 101, "", "type: string, default: build/"

        print 101, "--with-ext", "Replace the file extension: parent/stem.[ext]"
        print 101, "", "type: string, default: .o"

        print 101, "--strip-parents", "Remove the first 'n' parents of the original path"
        print 101, "", "type: integer, default: 1"

        print 101, "--include-targets", "Generate rules for files with programs in it"
        print 101, "", "type: bool, default: absent"

        print 101, "--output", "Output location"
        print 101, "", "type: string"

        print 101, "--preprocess", "Run preprocessing beforehand"
        print 101, "", "type: bool, default: absent"

        print 101, "--preprocessor-cmd", "Preprocessor command to use"
        print 101, "", "type: string, default: 'gfortran -cpp -E'"
    end subroutine

    integer function parse_int(str) 
        use lexer_mod, only: is_digit
        character(*), intent(in) :: str
        integer :: i

        parse_int = -1
        do i = 1, len(str)
            if (.not. is_digit(str(i:i))) return
        end do
        read(str, *) parse_int
    end function

    subroutine fail(msg)
        use iso_fortran_env, only: output_unit, error_unit
        character(*) :: msg
        write(error_unit, "(A)") msg
        call exit(1)
    end subroutine
end program

