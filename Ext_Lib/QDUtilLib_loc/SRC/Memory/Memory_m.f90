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
MODULE QDUtil_Memory_m

  USE QDUtil_Memory_base_m
  USE QDUtil_Memory_Pointer_m
  USE QDUtil_Memory_NotPointer_m
  IMPLICIT NONE

  INTERFACE Mem_TO_String
    MODULE PROCEDURE QDUtil_Mem_TO_String
  END INTERFACE
  PRIVATE QDUtil_Mem_TO_String

  CONTAINS
  FUNCTION QDUtil_Mem_TO_String(mem) RESULT(string)
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
    IMPLICIT NONE

    character (len=:), allocatable    :: string
    real(kind=Rkind),   intent(in)    :: mem

    character (len=2) :: MemUnit
    real(kind=Rkind)  :: mem_loc

    mem_loc = mem
    CALL convertMem(mem_loc,MemUnit)

    string = TO_string(mem_loc,Rformat='f0.0') // ' ' // trim(MemUnit)

  END FUNCTION QDUtil_Mem_TO_String
END MODULE QDUtil_Memory_m

SUBROUTINE Test_QDUtil_Memory()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_Memory_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    character (len=:), allocatable :: string1,string2
    character (len=2) :: mult(5) = ['O ','kO','MO','GO','GO']

    real (kind=Rkind) :: mem
    integer :: i


    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_Memory'
    !logical, parameter :: debug = .FALSE.
    logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='Memory')


    mem = 123._Rkind/1024_Rkind
    DO i=1,size(mult)-1
      mem = mem * 1024_Rkind
      string1 = Mem_TO_String(mem)
      string2 = '123. ' // trim(mult(i))
      write(test_var%test_log_file_unit,*) 'mem:  ',mem
      write(test_var%test_log_file_unit,*) 'mem1: ',string1
      write(test_var%test_log_file_unit,*) 'mem2: ',string2

      res_test = (string1 == string2)
      CALL Logical_Test(test_var,test1=res_test,info='memory in ' // trim(mult(i)))
      CALL Flush_Test(test_var)
    END DO

    mem = mem * 1024_Rkind
    string1 = Mem_TO_String(mem)
    string2 = '125952. ' // trim(mult(i))
    write(test_var%test_log_file_unit,*) 'mem:  ',mem
    write(test_var%test_log_file_unit,*) 'mem1: ',string1
    write(test_var%test_log_file_unit,*) 'mem2: ',string2

    res_test = (string1 == string2)
    CALL Logical_Test(test_var,test1=res_test,info='memory in ' // trim(mult(i)))
    CALL Flush_Test(test_var)

    ! finalize the tests
    CALL Finalize_Test(test_var)

END SUBROUTINE Test_QDUtil_Memory
