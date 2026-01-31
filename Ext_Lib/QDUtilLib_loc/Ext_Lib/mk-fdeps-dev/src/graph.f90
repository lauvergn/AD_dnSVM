module graph_mod 
    implicit none
    private
    public :: bfs_iterator_t, to_csr_format

    type :: bfs_iterator_t
        integer :: qstart, qend
        integer, allocatable :: queue(:)
        logical, allocatable :: visited(:)

        contains

        procedure :: initialize => bfs_initialize
        procedure :: next => bfs_next
        procedure :: start => bfs_start
    end type

    contains

    subroutine bfs_initialize(this, num_nodes)
        class(bfs_iterator_t), intent(inout) :: this
        integer, intent(in) :: num_nodes

        allocate(this%queue(num_nodes))
        allocate(this%visited(num_nodes))
        this%visited = .false.
    end subroutine

   logical function bfs_next(this, node, csr_dep_offset, csr_dep)
        class(bfs_iterator_t), intent(inout) :: this
        integer, intent(out) :: node
        integer, intent(in) :: csr_dep_offset(:)
        integer, intent(in) :: csr_dep(:)
        integer :: j, neib

        if (this%qstart > this%qend) then
            bfs_next = .false.
            return
        end if

        node = this%queue(this%qstart)
        this%qstart = this%qstart + 1

        ! iterate neibs
        do j = csr_dep_offset(node), csr_dep_offset(node+1)-1
            neib = csr_dep(j)
            if (neib == 0) cycle

            if (.not. this%visited(neib)) then
                this%qend = this%qend + 1
                this%queue(this%qend) = neib
                this%visited(neib) = .true.
            end if
        end do

        bfs_next = .true.
    end function

    subroutine bfs_start(this, start_node)
        class(bfs_iterator_t), intent(inout) :: this
        integer, intent(in) :: start_node

        this%visited = .false.
        this%qstart = 1
        this%qend = 1
        this%queue(this%qend) = start_node
        this%visited(start_node) = .true.
    end subroutine

    subroutine to_csr_format(edges, n_nodes, neib_start, neib)
        integer, intent(in) :: edges(:, :), n_nodes
        integer, allocatable, intent(out) :: neib_start(:), neib(:)

        integer :: edge, i, j, cnt(n_nodes)

        cnt = 0
        do edge = 1, size(edges, 2)
            cnt(edges(1, edge)) = cnt(edges(1, edge)) + 1
        end do

        allocate(neib_start(n_nodes + 1))
        neib_start(1) = 1
        do i = 1, n_nodes
            neib_start(i + 1) = neib_start(i) + cnt(i)
        end do

        allocate(neib(size(edges, 2)))
        cnt = 0
        do edge = 1, size(edges, 2)
            i = edges(1, edge); j = edges(2, edge)
            neib(neib_start(i) + cnt(i)) = j
            cnt(i) = cnt(i) + 1
        end do
    end subroutine

end module
