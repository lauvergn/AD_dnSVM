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
MODULE QDUtil_RealVec_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE

  TYPE RealVec_t
    real(kind=Rkind), allocatable  :: vec(:)
  END TYPE RealVec_t

  PUBLIC :: RealVec_t,get_size
  PUBLIC :: alloc_RealVec,dealloc_RealVec,check_alloc_RealVec,alloc_array,dealloc_array
  PUBLIC :: Write_RealVec,Set_ZERO_TO_RealVec

  INTERFACE get_size
    MODULE PROCEDURE QDUtil_get_Size_FROM_RealVec
  END INTERFACE

  INTERFACE alloc_RealVec
    MODULE PROCEDURE QDUtil_alloc_RealVec
  END INTERFACE
  INTERFACE dealloc_RealVec
    MODULE PROCEDURE QDUtil_dealloc_RealVec
  END INTERFACE
  INTERFACE alloc_array
    MODULE PROCEDURE QDUtil_alloc_array_OF_RealVecdim1, &
                     QDUtil_alloc_NParray_OF_RealVecdim1
  END INTERFACE
  INTERFACE dealloc_array
    MODULE PROCEDURE QDUtil_dealloc_array_OF_RealVecdim1, &
                     QDUtil_dealloc_NParray_OF_RealVecdim1
  END INTERFACE
  INTERFACE check_alloc_RealVec
    MODULE PROCEDURE QDUtil_check_alloc_RealVec
  END INTERFACE
  INTERFACE Write_RealVec
    MODULE PROCEDURE QDUtil_Write_RealVec
  END INTERFACE
  INTERFACE Set_ZERO_TO_RealVec
    MODULE PROCEDURE QDUtil_ZERO_TO_RealVec
  END INTERFACE

CONTAINS
  FUNCTION QDUtil_get_Size_FROM_RealVec(RealVec) RESULT(SizeVec)
    integer                      :: SizeVec
    TYPE (RealVec_t), intent(in) :: RealVec

    IF (.NOT. allocated(RealVec%vec)) THEN
      SizeVec = 0
    ELSE
      SizeVec = size(RealVec%vec)
    END IF

  END FUNCTION QDUtil_get_Size_FROM_RealVec
  SUBROUTINE QDUtil_alloc_RealVec(RealVec,SizeVec)
    TYPE (RealVec_t), intent(inout) :: RealVec
    integer,         intent(in)    :: SizeVec

    IF (allocated(RealVec%vec)) RETURN

    allocate(RealVec%vec(SizeVec))

  END SUBROUTINE QDUtil_alloc_RealVec
  SUBROUTINE QDUtil_dealloc_RealVec(RealVec)
    TYPE (RealVec_t), intent(inout) :: RealVec

    IF (allocated(RealVec%vec)) deallocate(RealVec%vec)

  END SUBROUTINE QDUtil_dealloc_RealVec
 

  SUBROUTINE QDUtil_alloc_array_OF_RealVecdim1(tab,tab_ub,name_var,name_sub,tab_lb)
    USE QDUtil_Memory_m
    IMPLICIT NONE

    TYPE (RealVec_t), pointer, intent(inout)        :: tab(:)
    integer,                  intent(in)           :: tab_ub(:)
    integer,                  intent(in), optional :: tab_lb(:)

    character (len=*),        intent(in)           :: name_var,name_sub

    integer, parameter :: ndim=1
    logical :: memory_test

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_alloc_array_OF_RealVecdim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------

    IF (associated(tab))                                             &
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
    CALL error_memo_allo(err_mem,memory,name_var,name_sub,'RealVec_t')

  END SUBROUTINE QDUtil_alloc_array_OF_RealVecdim1
  SUBROUTINE QDUtil_dealloc_array_OF_RealVecdim1(tab,name_var,name_sub)
    USE QDUtil_Memory_m
    IMPLICIT NONE

    TYPE (RealVec_t), pointer, intent(inout) :: tab(:)
    character (len=*),           intent(in)    :: name_var,name_sub

    integer :: i
    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_dealloc_array_OF_RealVecdim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------

    IF (.NOT. associated(tab))                                       &
    CALL Write_error_null(name_sub_alloc,name_var,name_sub)

    DO i=lbound(tab,dim=1),ubound(tab,dim=1)
      CALL dealloc_RealVec(tab(i))
    END DO
 
    memory = size(tab)
    deallocate(tab,stat=err_mem)
    CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'Type_RealVec')
    nullify(tab)

  END SUBROUTINE QDUtil_dealloc_array_OF_RealVecdim1



  SUBROUTINE QDUtil_alloc_NParray_OF_RealVecdim1(tab,tab_ub,name_var,name_sub,tab_lb)
    USE QDUtil_Memory_m
    IMPLICIT NONE

    TYPE (RealVec_t), allocatable, intent(inout)        :: tab(:)
    integer,                       intent(in)           :: tab_ub(:)
    integer,                       intent(in), optional :: tab_lb(:)
    character (len=*),             intent(in)           :: name_var,name_sub

    integer, parameter :: ndim=1
    logical :: memory_test

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_alloc_NParray_OF_RealVecdim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
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
    CALL error_memo_allo(err_mem,memory,name_var,name_sub,'RealVec_t')

  END SUBROUTINE QDUtil_alloc_NParray_OF_RealVecdim1
  SUBROUTINE QDUtil_dealloc_NParray_OF_RealVecdim1(tab,name_var,name_sub)
    USE QDUtil_Memory_m
    IMPLICIT NONE

    TYPE (RealVec_t), allocatable, intent(inout) :: tab(:)
    character (len=*),             intent(in)    :: name_var,name_sub

    integer :: i
    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_dealloc_NParray_OF_RealVecdim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------

    IF (.NOT. allocated(tab))                                       &
    CALL Write_error_null(name_sub_alloc,name_var,name_sub)

    DO i=lbound(tab,dim=1),ubound(tab,dim=1)
      CALL dealloc_RealVec(tab(i))
    END DO
 
    memory = size(tab)
    deallocate(tab,stat=err_mem)
    CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'Type_RealVec')

  END SUBROUTINE QDUtil_dealloc_NParray_OF_RealVecdim1

  SUBROUTINE QDUtil_check_alloc_RealVec(A,name_A,name_sub)
    USE QDUtil_NumParameters_m

    TYPE (RealVec_t),   intent(in) :: A
    character (len=*), intent(in) :: name_A
    character (len=*), intent(in) :: name_sub

    IF ( .NOT. allocated(A%vec)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) name_A,' has NOT been initiated with "QDUtil_alloc_RealVec"'
      write(out_unit,*) ' CHECK the source!!!!!'
      STOP 'STOP in QDUtil_check_alloc_RealVec: A is not allocated'
    END IF
  END SUBROUTINE QDUtil_check_alloc_RealVec
  SUBROUTINE QDUtil_Write_RealVec(RealVec)
    USE QDUtil_NumParameters_m

    TYPE (RealVec_t), intent(in) :: RealVec

    CALL check_alloc_RealVec(RealVec,'RealVec','QDUtil_write_RealVec')

    write(out_unit,*) 'BEGINNING QDUtil_Write_RealVec'
    write(out_unit,*) 'SizeVec',size(RealVec%vec)
    write(out_unit,*) RealVec%vec
    write(out_unit,*) 'END QDUtil_Write_RealVec'


  END SUBROUTINE QDUtil_Write_RealVec

  SUBROUTINE QDUtil_ZERO_TO_RealVec(RealVec)
    TYPE (RealVec_t), intent(inout) :: RealVec

    CALL check_alloc_RealVec(RealVec,'RealVec','QDUtil__ZERO_TO_RealVec')

    RealVec%vec(:) = 0

  END SUBROUTINE QDUtil_ZERO_TO_RealVec
END MODULE QDUtil_RealVec_m
SUBROUTINE Test_QDUtil_RealVec()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_RW_MatVec_m
    USE QDUtil_RealVec_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    integer                          :: io,ioerr
    TYPE(RealVec_t) :: V1,V2

    
    STOP 'NOT Yet'
    !CALL Initialize_Test(test_var,test_name='RealVec')
    !CALL Logical_Test(test_var,test1=res_test,info='descending sort')
    !CALL Finalize_Test(test_var)
END SUBROUTINE Test_QDUtil_RealVec
