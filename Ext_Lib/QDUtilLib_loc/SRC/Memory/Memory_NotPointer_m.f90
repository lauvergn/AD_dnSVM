!===============================================================================
!===============================================================================
!This file is part of QDUtil.
!
!===============================================================================
! MIT License
!
! Copyright (c) 2022 David Lauvergnat
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!===============================================================================
!===============================================================================
MODULE QDUtil_Memory_NotPointer_m
      USE QDUtil_NumParameters_m, only : Rkind, Ikind, ILkind, ZERO, CZERO
      USE QDUtil_Memory_base_m,   only : write_error_not_null, sub_test_tab_ub, sub_test_tab_lb, &
                                   error_memo_allo, write_error_null
      IMPLICIT NONE

      PRIVATE
      PUBLIC :: alloc_NParray,dealloc_NParray
      PUBLIC :: alloc_array,dealloc_array

      INTERFACE alloc_NParray
        MODULE PROCEDURE QDUtil_alloc_array_OF_Ldim1,QDUtil_alloc_array_OF_Ldim2

        MODULE PROCEDURE QDUtil_alloc_array_OF_I4dim1,QDUtil_alloc_array_OF_I8dim1
        MODULE PROCEDURE QDUtil_alloc_array_OF_Idim2
        MODULE PROCEDURE QDUtil_alloc_array_OF_Idim3,QDUtil_alloc_array_OF_Idim4
        MODULE PROCEDURE QDUtil_alloc_array_OF_Idim5

        MODULE PROCEDURE QDUtil_alloc_array_OF_Rdim1,QDUtil_alloc_array_OF_Rdim2
        MODULE PROCEDURE QDUtil_alloc_array_OF_Rdim3,QDUtil_alloc_array_OF_Rdim4
        MODULE PROCEDURE QDUtil_alloc_array_OF_Rdim5

        MODULE PROCEDURE QDUtil_alloc_array_OF_Cdim1,QDUtil_alloc_array_OF_Cdim2
        MODULE PROCEDURE QDUtil_alloc_array_OF_Cdim3,QDUtil_alloc_array_OF_Cdim4
        MODULE PROCEDURE QDUtil_alloc_array_OF_Cdim5

      END INTERFACE
      INTERFACE dealloc_NParray
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Ldim1,QDUtil_dealloc_array_OF_Ldim2

        MODULE PROCEDURE QDUtil_dealloc_array_OF_I4dim1,QDUtil_dealloc_array_OF_I8dim1
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Idim2
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Idim3,QDUtil_dealloc_array_OF_Idim4
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Idim5

        MODULE PROCEDURE QDUtil_dealloc_array_OF_Rdim1,QDUtil_dealloc_array_OF_Rdim2
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Rdim3,QDUtil_dealloc_array_OF_Rdim4
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Rdim5

        MODULE PROCEDURE QDUtil_dealloc_array_OF_Cdim1,QDUtil_dealloc_array_OF_Cdim2
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Cdim3,QDUtil_dealloc_array_OF_Cdim4
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Cdim5

      END INTERFACE

      INTERFACE alloc_array
        MODULE PROCEDURE QDUtil_alloc_array_OF_Ldim1,QDUtil_alloc_array_OF_Ldim2

        MODULE PROCEDURE QDUtil_alloc_array_OF_I4dim1,QDUtil_alloc_array_OF_I8dim1
        MODULE PROCEDURE QDUtil_alloc_array_OF_Idim2
        MODULE PROCEDURE QDUtil_alloc_array_OF_Idim3,QDUtil_alloc_array_OF_Idim4
        MODULE PROCEDURE QDUtil_alloc_array_OF_Idim5

        MODULE PROCEDURE QDUtil_alloc_array_OF_Rdim1,QDUtil_alloc_array_OF_Rdim2
        MODULE PROCEDURE QDUtil_alloc_array_OF_Rdim3,QDUtil_alloc_array_OF_Rdim4
        MODULE PROCEDURE QDUtil_alloc_array_OF_Rdim5

        MODULE PROCEDURE QDUtil_alloc_array_OF_Cdim1,QDUtil_alloc_array_OF_Cdim2
        MODULE PROCEDURE QDUtil_alloc_array_OF_Cdim3,QDUtil_alloc_array_OF_Cdim4
        MODULE PROCEDURE QDUtil_alloc_array_OF_Cdim5

      END INTERFACE
      INTERFACE dealloc_array
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Ldim1,QDUtil_dealloc_array_OF_Ldim2

        MODULE PROCEDURE QDUtil_dealloc_array_OF_I4dim1,QDUtil_dealloc_array_OF_I8dim1
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Idim2
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Idim3,QDUtil_dealloc_array_OF_Idim4
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Idim5

        MODULE PROCEDURE QDUtil_dealloc_array_OF_Rdim1,QDUtil_dealloc_array_OF_Rdim2
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Rdim3,QDUtil_dealloc_array_OF_Rdim4
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Rdim5

        MODULE PROCEDURE QDUtil_dealloc_array_OF_Cdim1,QDUtil_dealloc_array_OF_Cdim2
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Cdim3,QDUtil_dealloc_array_OF_Cdim4
        MODULE PROCEDURE QDUtil_dealloc_array_OF_Cdim5

      END INTERFACE
      CONTAINS

      !=================================================================
      ! Logical
      !=================================================================
      SUBROUTINE QDUtil_alloc_array_OF_Ldim1(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      logical, allocatable, intent(inout) :: tab(:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub

      integer, parameter :: ndim=1

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Ldim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'logical')

      END SUBROUTINE QDUtil_alloc_array_OF_Ldim1

      SUBROUTINE QDUtil_dealloc_array_OF_Ldim1(tab,name_var,name_sub)
      IMPLICIT NONE

      logical, allocatable, intent(inout) :: tab(:)
      character (len=*), intent(in) :: name_var,name_sub

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Ldim1'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN
       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)

       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'logical')

      END SUBROUTINE QDUtil_dealloc_array_OF_Ldim1

      SUBROUTINE QDUtil_alloc_array_OF_Ldim2(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      logical, allocatable, intent(inout)        :: tab(:,:)
      integer,              intent(in)           :: tab_ub(:)
      integer,              intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=2

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Ldim2'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'logical')

      END SUBROUTINE QDUtil_alloc_array_OF_Ldim2
      SUBROUTINE QDUtil_dealloc_array_OF_Ldim2(tab,name_var,name_sub)
      IMPLICIT NONE

      logical, allocatable, intent(inout) :: tab(:,:)
      character (len=*), intent(in) :: name_var,name_sub

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Ldim2'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
                 CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)

       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'logical')


      END SUBROUTINE QDUtil_dealloc_array_OF_Ldim2

      !=================================================================
      ! integer
      !=================================================================

      SUBROUTINE QDUtil_alloc_array_OF_I8dim1(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      integer (kind=ILkind), allocatable, intent(inout) :: tab(:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=1


!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_I8dim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'integer')
       tab = 0

      END SUBROUTINE QDUtil_alloc_array_OF_I8dim1
      SUBROUTINE QDUtil_dealloc_array_OF_I8dim1(tab,name_var,name_sub)
      IMPLICIT NONE

      integer (kind=ILkind), allocatable, intent(inout) :: tab(:)
      character (len=*), intent(in) :: name_var,name_sub


!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_I8dim1'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)

       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'integer')

      END SUBROUTINE QDUtil_dealloc_array_OF_I8dim1
      SUBROUTINE QDUtil_alloc_array_OF_I4dim1(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      integer (kind=Ikind), allocatable, intent(inout) :: tab(:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=1


!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_I4dim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'integer')
       tab = 0

      END SUBROUTINE QDUtil_alloc_array_OF_I4dim1

      SUBROUTINE QDUtil_dealloc_array_OF_I4dim1(tab,name_var,name_sub)
      IMPLICIT NONE

      integer (kind=Ikind), allocatable, intent(inout) :: tab(:)
      character (len=*), intent(in) :: name_var,name_sub

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_I4dim1'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

      IF (.NOT. allocated(tab))                                       &
          CALL Write_error_null(name_sub_alloc,name_var,name_sub)

      memory = size(tab,kind=ILkind)
      deallocate(tab,stat=err_mem)
      CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'integer')

      END SUBROUTINE QDUtil_dealloc_array_OF_I4dim1

      SUBROUTINE QDUtil_alloc_array_OF_Idim2(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      integer, allocatable, intent(inout) :: tab(:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=2


!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Idim2'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'integer')
       tab = 0

      END SUBROUTINE QDUtil_alloc_array_OF_Idim2

!=======================================================================================
      SUBROUTINE QDUtil_dealloc_array_OF_Idim2(tab,name_var,name_sub)
      IMPLICIT NONE

      integer, allocatable, intent(inout) :: tab(:,:)
      character (len=*), intent(in) :: name_var,name_sub

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Idim2'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'integer')


      END SUBROUTINE QDUtil_dealloc_array_OF_Idim2
!=======================================================================================

      SUBROUTINE QDUtil_alloc_array_OF_Idim3(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      integer, allocatable, intent(inout) :: tab(:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub

      integer, parameter :: ndim=3

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Idim3'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'integer')
       tab = 0

      END SUBROUTINE QDUtil_alloc_array_OF_Idim3
      SUBROUTINE QDUtil_dealloc_array_OF_Idim3(tab,name_var,name_sub)
      IMPLICIT NONE

      integer, allocatable, intent(inout) :: tab(:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Idim3'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'integer')


      END SUBROUTINE QDUtil_dealloc_array_OF_Idim3

      SUBROUTINE QDUtil_alloc_array_OF_Idim4(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      integer, allocatable, intent(inout) :: tab(:,:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=4

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Idim4'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3),                              &
                      tab_lb(4):tab_ub(4)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3),tab_ub(4)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'integer')
       tab = 0

      END SUBROUTINE QDUtil_alloc_array_OF_Idim4
      SUBROUTINE QDUtil_dealloc_array_OF_Idim4(tab,name_var,name_sub)
      IMPLICIT NONE

      integer, allocatable, intent(inout) :: tab(:,:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Idim4'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'integer')


      END SUBROUTINE QDUtil_dealloc_array_OF_Idim4

      SUBROUTINE QDUtil_alloc_array_OF_Idim5(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      integer, allocatable, intent(inout) :: tab(:,:,:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=5

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Idim5'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3),                              &
                      tab_lb(4):tab_ub(4),                              &
                      tab_lb(5):tab_ub(5)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3),tab_ub(4),tab_ub(5)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'integer')
       tab = 0

      END SUBROUTINE QDUtil_alloc_array_OF_Idim5
      SUBROUTINE QDUtil_dealloc_array_OF_Idim5(tab,name_var,name_sub)
      IMPLICIT NONE

      integer, allocatable, intent(inout) :: tab(:,:,:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Idim5'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'integer')


      END SUBROUTINE QDUtil_dealloc_array_OF_Idim5

      !=================================================================
      ! real (kind=Rkind)
      !=================================================================

      SUBROUTINE QDUtil_alloc_array_OF_Rdim1(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=1

      !----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Rdim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
      !----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         !write(out_unit,*) 'tab_lb',tab_lb
         !write(out_unit,*) 'tab_ub',tab_ub

         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'real8')
       tab = ZERO

       !write(out_unit,*) 'lbound',lbound(tab)
       !write(out_unit,*) 'ubound',ubound(tab)

      END SUBROUTINE QDUtil_alloc_array_OF_Rdim1

      SUBROUTINE QDUtil_alloc_BigArray_OF_Rdim1(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:)
      integer (kind=ILkind), intent(in) :: tab_ub(:)
      integer(kind=ILkind), intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=1

      !----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_BigArray_OF_Rdim1'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
      !----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         !write(out_unit,*) 'tab_lb',tab_lb
         !write(out_unit,*) 'tab_ub',tab_ub

         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'real8')
       tab = ZERO

       !write(out_unit,*) 'lbound',lbound(tab)
       !write(out_unit,*) 'ubound',ubound(tab)

      END SUBROUTINE QDUtil_alloc_BigArray_OF_Rdim1
      SUBROUTINE QDUtil_dealloc_array_OF_Rdim1(tab,name_var,name_sub)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:)
      character (len=*), intent(in) :: name_var,name_sub


!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Rdim1'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'real8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Rdim1
      SUBROUTINE QDUtil_alloc_array_OF_Rdim2(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=2

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Rdim2'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'real8')
       tab = ZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Rdim2
      SUBROUTINE QDUtil_dealloc_array_OF_Rdim2(tab,name_var,name_sub)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Rdim2'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'real8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Rdim2

      SUBROUTINE QDUtil_alloc_array_OF_Rdim3(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=3

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Rdim3'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'real8')
       tab = ZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Rdim3
      SUBROUTINE QDUtil_dealloc_array_OF_Rdim3(tab,name_var,name_sub)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Rdim3'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'real8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Rdim3

      SUBROUTINE QDUtil_alloc_array_OF_Rdim4(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=4

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Rdim4'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3),                              &
                      tab_lb(4):tab_ub(4)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3),tab_ub(4)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'real8')
       tab = ZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Rdim4
      SUBROUTINE QDUtil_dealloc_array_OF_Rdim4(tab,name_var,name_sub)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Rdim4'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'real8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Rdim4

      SUBROUTINE QDUtil_alloc_array_OF_Rdim5(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=5

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Rdim5'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3),                              &
                      tab_lb(4):tab_ub(4),                              &
                      tab_lb(5):tab_ub(5)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3),tab_ub(4),tab_ub(5)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'real8')
       tab = ZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Rdim5
      SUBROUTINE QDUtil_dealloc_array_OF_Rdim5(tab,name_var,name_sub)
      IMPLICIT NONE

      real (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Rdim5'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'real8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Rdim5

      !=================================================================
      ! complex (kind=Rkind)
      !=================================================================

      SUBROUTINE QDUtil_alloc_array_OF_Cdim1(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=1

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Cdim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'complex8')
       tab = CZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Cdim1
      SUBROUTINE QDUtil_dealloc_array_OF_Cdim1(tab,name_var,name_sub)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:)
      character (len=*), intent(in) :: name_var,name_sub


!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Cdim1'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'complex8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Cdim1
      SUBROUTINE QDUtil_alloc_array_OF_Cdim2(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=2

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Cdim2'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'complex8')
       tab = CZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Cdim2
      SUBROUTINE QDUtil_dealloc_array_OF_Cdim2(tab,name_var,name_sub)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Cdim2'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'complex8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Cdim2

      SUBROUTINE QDUtil_alloc_array_OF_Cdim3(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=3

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Cdim3'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'complex8')
       tab = CZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Cdim3
      SUBROUTINE QDUtil_dealloc_array_OF_Cdim3(tab,name_var,name_sub)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Cdim3'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'complex8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Cdim3

      SUBROUTINE QDUtil_alloc_array_OF_Cdim4(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=4

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Cdim4'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3),                              &
                      tab_lb(4):tab_ub(4)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3),tab_ub(4)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'complex8')
       tab = CZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Cdim4
      SUBROUTINE QDUtil_dealloc_array_OF_Cdim4(tab,name_var,name_sub)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Cdim4'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'complex8')

      END SUBROUTINE QDUtil_dealloc_array_OF_Cdim4

      SUBROUTINE QDUtil_alloc_array_OF_Cdim5(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:,:,:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub


      integer, parameter :: ndim=5

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_Cdim5'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)


         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1),                              &
                      tab_lb(2):tab_ub(2),                              &
                      tab_lb(3):tab_ub(3),                              &
                      tab_lb(4):tab_ub(4),                              &
                      tab_lb(5):tab_ub(5)),stat=err_mem)

       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1),tab_ub(2),tab_ub(3),tab_ub(4),tab_ub(5)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'complex8')
       tab = CZERO

      END SUBROUTINE QDUtil_alloc_array_OF_Cdim5
      SUBROUTINE QDUtil_dealloc_array_OF_Cdim5(tab,name_var,name_sub)
      IMPLICIT NONE

      complex (kind=Rkind), allocatable, intent(inout) :: tab(:,:,:,:,:)
      character (len=*), intent(in) :: name_var,name_sub



!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_Cdim5'
      integer :: err_mem
      integer (kind=ILkind) :: memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN

       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       memory = size(tab,kind=ILkind)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'complex8')


      END SUBROUTINE QDUtil_dealloc_array_OF_Cdim5

END MODULE QDUtil_Memory_NotPointer_m
