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

    logical, public                :: prlev    = .FALSE.

    character (len=:), allocatable :: test_name
    character (len=:), allocatable :: test_log_file_name
    integer, public                :: test_log_file_unit = -1

  END TYPE test_t

  PUBLIC :: test_logical,test_finalize,test_initialyze

  INTERFACE test_logical
    MODULE PROCEDURE AD_test_logical
  END INTERFACE
  INTERFACE test_finalize
    MODULE PROCEDURE AD_test_finalize
  END INTERFACE
  INTERFACE test_initialyze
    MODULE PROCEDURE AD_test_initialyze
  END INTERFACE

CONTAINS

  SUBROUTINE AD_test_logical(test_var,test1,test2,info)
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
    write(test_var%test_log_file_unit,*) '-------------------------------------------------------'
    write(test_var%test_log_file_unit,*) '------------------ test #',test_var%nb_Test

    IF (test1 .eqv. test2_loc) THEN
      write(out_unitp,*) info,': OK'
      write(test_var%test_log_file_unit,*) info,': OK'
      test_var%nb_OK = test_var%nb_OK + 1
    ELSE
      write(out_unitp,*) info,': Err'
      write(test_var%test_log_file_unit,*) info,': Err'
      test_var%nb_Err = test_var%nb_Err + 1
    END IF

  END SUBROUTINE AD_test_logical
  SUBROUTINE AD_test_finalize(test_var)
  IMPLICIT NONE

    TYPE (test_t),      intent(in)    :: test_var

    write(test_var%test_log_file_unit,*) '-------------------------------------------------------'
    write(test_var%test_log_file_unit,*)
    write(out_unitp,*)

    IF (test_var%nb_Test /= test_var%nb_OK + test_var%nb_Err) THEN
      write(out_unitp,*) 'ERROR while testing ',test_var%test_name,' module: nb_Test /= nb_OK + nb_Err'
      write(out_unitp,*) 'nb_Test',test_var%nb_Test
      write(out_unitp,*) 'nb_OK  ',test_var%nb_OK
      write(out_unitp,*) 'nb_Err ',test_var%nb_Err

      write(test_var%test_log_file_unit,*) 'ERROR while testing ',test_var%test_name,' module: nb_Test /= nb_OK + nb_Err'
      write(test_var%test_log_file_unit,*) 'nb_Test',test_var%nb_Test
      write(test_var%test_log_file_unit,*) 'nb_OK  ',test_var%nb_OK
      write(test_var%test_log_file_unit,*) 'nb_Err ',test_var%nb_Err

    END IF
    write(out_unitp,*) 'TESTING ',test_var%test_name,' module. Number of tests   :',test_var%nb_Test
    write(out_unitp,*) 'TESTING ',test_var%test_name,' module. Number of error(s):',test_var%nb_Err

    write(test_var%test_log_file_unit,*) 'TESTING ',test_var%test_name,' module. Number of tests   :',test_var%nb_Test
    write(test_var%test_log_file_unit,*) 'TESTING ',test_var%test_name,' module. Number of error(s):',test_var%nb_Err


    write(out_unitp,*) '== END TESTING ',test_var%test_name,' module ===='
    write(test_var%test_log_file_unit,*) '== END TESTING ',test_var%test_name,' module ===='

    close(unit=test_var%test_log_file_unit)


 END SUBROUTINE AD_test_finalize

 SUBROUTINE AD_test_initialyze(test_var,test_name,log_file_name)
 IMPLICIT NONE

  TYPE (test_t),      intent(inout)          :: test_var
  character (len=*),  intent(in),  optional  :: test_name
  character (len=*),  intent(in),  optional  :: log_file_name

  test_var%nb_Test = 0
  test_var%nb_OK   = 0
  test_var%nb_Err  = 0

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

  write(out_unitp,*) "== TESTING ",test_var%test_name," module ===="
  write(test_var%test_log_file_unit,*) "== TESTING ",test_var%test_name," module ===="

END SUBROUTINE AD_test_initialyze

END MODULE ADLib_Test_m
