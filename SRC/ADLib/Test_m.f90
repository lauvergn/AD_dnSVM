!===============================================================================
!===============================================================================
!This file is part of AD_dnSVM.
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
MODULE ADLib_Test_m
USE ADLib_NumParameters_m
!$ USE omp_lib
IMPLICIT NONE

  PRIVATE

  TYPE, PUBLIC :: test_t
    PRIVATE
    integer                        :: nb_Test  = 0
    integer                        :: nb_OK    = 0
    integer                        :: nb_Err   = 0

    logical, public                :: PrintFlag    = .FALSE.

    real (kind=Rkind)              :: ZeroTresh    = ONETENTH**10

    character (len=:), allocatable :: test_name
    character (len=:), allocatable :: test_log_file_name
    integer, public                :: test_log_file_unit = -1

    character (len=:), allocatable, public :: test_log
    character (len=:), allocatable, public :: test_res


  END TYPE test_t

  PUBLIC :: Logical_Test,Finalize_Test,Initialize_Test,Flush_Test,Append_Test

  INTERFACE Logical_Test
    MODULE PROCEDURE AD_Logical_Test
  END INTERFACE
  INTERFACE Finalize_Test
    MODULE PROCEDURE AD_Finalize_Test
  END INTERFACE
  INTERFACE Initialize_Test
    MODULE PROCEDURE AD_Initialize_Test
  END INTERFACE

  INTERFACE Append_Test
    MODULE PROCEDURE AD_Append_Test_reslog
  END INTERFACE
  INTERFACE Flush_Test
    MODULE PROCEDURE AD_Flush_Test_reslog
  END INTERFACE

CONTAINS

  SUBROUTINE AD_Logical_Test(test_var,test1,test2,info)
  USE ADLib_Util_m
  IMPLICIT NONE

    TYPE (test_t),      intent(inout)         :: test_var
    logical,            intent(in)            :: test1
    logical,            intent(in),  optional :: test2

    character (len=*),  intent(in)            :: info

    logical :: test2_loc


    IF (present(test2)) THEN
      test2_loc = test2
    ELSE
      test2_loc = .TRUE.
    END IF


    test_var%nb_Test = test_var%nb_Test + 1

    CALL Append_Test(test_var,'-------------------------------------------------------')
    CALL Append_Test(test_var,'------------------ test #' // int_TO_char(test_var%nb_Test))

    IF (test1 .eqv. test2_loc) THEN
      CALL Append_Test(test_var,info // ': OK')
      test_var%nb_OK = test_var%nb_OK + 1
    ELSE
      CALL Append_Test(test_var,info // ': Err')
      test_var%nb_Err = test_var%nb_Err + 1
    END IF

  END SUBROUTINE AD_Logical_Test
  SUBROUTINE AD_Finalize_Test(test_var)
  USE ADLib_Util_m
  IMPLICIT NONE

    TYPE (test_t),      intent(inout)    :: test_var

    CALL Append_Test(test_var,'-------------------------------------------------------')
    CALL Append_Test(test_var,'')

    IF (test_var%nb_Test /= test_var%nb_OK + test_var%nb_Err) THEN
      CALL Append_Test(test_var,'ERROR while testing ' //                       &
                     test_var%test_name // ' module: nb_Test /= nb_OK + nb_Err')
      CALL Append_Test(test_var,'nb_Test' // int_TO_char(test_var%nb_Test))
      CALL Append_Test(test_var,'nb_OK  ' // int_TO_char(test_var%nb_OK))
      CALL Append_Test(test_var,'nb_Err ' // int_TO_char(test_var%nb_Err))

    END IF

    CALL Append_Test(test_var,'TESTING ' // test_var%test_name //               &
                ' module. Number of tests   :' // int_TO_char(test_var%nb_Test))
    CALL Append_Test(test_var,'TESTING ' // test_var%test_name //               &
                ' module. Number of error(s):' // int_TO_char(test_var%nb_Err))
    CALL Append_Test(test_var,'== END TESTING ' // test_var%test_name // ' module ====')


    CALL Flush_Test(test_var)

    close(unit=test_var%test_log_file_unit)


 END SUBROUTINE AD_Finalize_Test

 SUBROUTINE AD_Initialize_Test(test_var,test_name,log_file_name,PrintFlag,ZeroTresh)
 USE ADLib_Util_m
 IMPLICIT NONE

  TYPE (test_t),      intent(inout)          :: test_var
  character (len=*),  intent(in),  optional  :: test_name
  character (len=*),  intent(in),  optional  :: log_file_name
  logical,            intent(in),  optional  :: PrintFlag
  real (kind=Rkind),  intent(in),  optional  :: ZeroTresh

  test_var%nb_Test = 0
  test_var%nb_OK   = 0
  test_var%nb_Err  = 0

  IF (present(PrintFlag)) test_var%PrintFlag = PrintFlag
  IF (present(ZeroTresh)) test_var%ZeroTresh = ZeroTresh

  IF (present(test_name)) THEN
    test_var%test_name = test_name
  ELSE
    test_var%test_name = 'XXX'
  END IF

  IF (present(log_file_name)) THEN
    test_var%test_log_file_name = log_file_name
  ELSE
    test_var%test_log_file_name = test_var%test_name // '.log'
  END IF

  open(newunit=test_var%test_log_file_unit,file=test_var%test_log_file_name)

  CALL Append_Test(test_var,'== TESTING ' // test_var%test_name // ' module ====')


END SUBROUTINE AD_Initialize_Test

SUBROUTINE AD_Append_Test_reslog(test_var,info,Print_res)
IMPLICIT NONE

  TYPE (test_t),      intent(inout)         :: test_var
  character (len=*),  intent(in)            :: info
  logical,            intent(in), optional  :: Print_res

  logical :: Print_res_loc

  IF (present(Print_res)) THEN
    Print_res_loc = Print_res
  ELSE
    Print_res_loc = .TRUE.
  END IF

  IF (allocated(test_var%test_log)) THEN
    test_var%test_log = test_var%test_log // info // new_line('a')
  ELSE
    test_var%test_log = info // new_line('a')
  END IF

  IF (Print_res_loc) THEN
    IF (allocated(test_var%test_res)) THEN
      test_var%test_res = test_var%test_res // info // new_line('a')
    ELSE
      test_var%test_res = info // new_line('a')
    END IF
  END IF

END SUBROUTINE AD_Append_Test_reslog
SUBROUTINE AD_Flush_Test_reslog(test_var)
IMPLICIT NONE

  TYPE (test_t),      intent(inout)         :: test_var

  IF (allocated(test_var%test_log)) THEN
    write(test_var%test_log_file_unit,*) test_var%test_log
    deallocate(test_var%test_log)
  END IF

  IF (allocated(test_var%test_res)) THEN
    write(out_unitp,*) test_var%test_res
    deallocate(test_var%test_res)
  END IF

END SUBROUTINE AD_Flush_Test_reslog
END MODULE ADLib_Test_m
