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
MODULE QDUtil_Test_m
  IMPLICIT NONE

  PRIVATE

  TYPE, PUBLIC :: test_t
    PRIVATE
    integer                        :: nb_AllTest  = 0
    integer                        :: nb_AllOK    = 0
    integer                        :: nb_AllErr   = 0

    integer                        :: nb_Test  = 0
    integer                        :: nb_OK    = 0
    integer                        :: nb_Err   = 0

    logical, public                :: PrintFlag    = .FALSE.

    character (len=:), allocatable :: test_name
    character (len=:), allocatable :: test_log_file_name
    integer, public                :: test_log_file_unit = -1

    character (len=:), allocatable, public :: test_log
    character (len=:), allocatable, public :: test_res

  END TYPE test_t

  PUBLIC :: Logical_Test,Flush_Test,Append_Test
  PUBLIC :: Initialize_Test,Finalize_Test,Finalize_Intermediate_Test

  INTERFACE Logical_Test
    MODULE PROCEDURE QD_Logical_Test
  END INTERFACE
  INTERFACE Finalize_Test
    MODULE PROCEDURE QD_Finalize_Test
  END INTERFACE
  INTERFACE Finalize_Intermediate_Test
    MODULE PROCEDURE QD_Finalize_Intermediate_Test
  END INTERFACE
  INTERFACE Initialize_Test
    MODULE PROCEDURE QD_Initialize_Test
  END INTERFACE

  INTERFACE Append_Test
    MODULE PROCEDURE QD_Append_Test_reslog
  END INTERFACE
  INTERFACE Flush_Test
    MODULE PROCEDURE QD_Flush_Test_reslog
  END INTERFACE

  INTERFACE TO_String ! private function only for the Test module.
    MODULE PROCEDURE QDTest_int_TO_char
  END INTERFACE
  
CONTAINS

  SUBROUTINE QD_Logical_Test(test_var,test1,test2,info)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : OUTPUT_UNIT
    IMPLICIT NONE

    TYPE (test_t),      intent(inout)         :: test_var
    logical,            intent(in)            :: test1
    logical,            intent(in),  optional :: test2

    character (len=*),  intent(in)            :: info

    logical :: test2_loc

    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.


    IF (present(test2)) THEN
      test2_loc = test2
    ELSE
      test2_loc = .TRUE.
    END IF
    test_var%nb_Test    = test_var%nb_Test    + 1
    test_var%nb_AllTest = test_var%nb_AllTest + 1

    IF (debug) THEN
      write(OUTPUT_UNIT,*)
      write(OUTPUT_UNIT,*) 'BEGINNING in QD_Logical_Test'
      write(OUTPUT_UNIT,*) 'test1,test2_loc',test1,test2_loc
      write(OUTPUT_UNIT,*) 'test_var%nb_Test',test_var%nb_Test
    END IF


    CALL Append_Test(test_var,'-------------------------------------------------------')
    CALL Append_Test(test_var,'------------------ test #' // TO_String(test_var%nb_Test))

    IF (test1 .eqv. test2_loc) THEN
      CALL Append_Test(test_var,info // ': OK')
      test_var%nb_OK    = test_var%nb_OK    + 1
      test_var%nb_AllOK = test_var%nb_AllOK + 1
    ELSE
      CALL Append_Test(test_var,info // ': Err')
      test_var%nb_Err    = test_var%nb_Err    + 1
      test_var%nb_AllErr = test_var%nb_AllErr + 1
    END IF

    CALL Flush_Test(test_var)

    IF (debug) THEN
      write(OUTPUT_UNIT,*) 'test OK',(test1 .eqv. test2_loc)
      write(OUTPUT_UNIT,*) 'END in QD_Logical_Test'
      flush(OUTPUT_UNIT)
    END IF
  END SUBROUTINE QD_Logical_Test
  SUBROUTINE QD_Finalize_Intermediate_Test(test_var,info)
    IMPLICIT NONE
  
      TYPE (test_t),      intent(inout)          :: test_var
      character (len=*),  intent(in),  optional  :: info

      character (len=:),  allocatable  :: info_loc

      IF (present(info)) THEN
        info_loc = info
      ELSE
        info_loc = test_var%test_name
      END IF

      CALL Append_Test(test_var,'-------------------------------------------------------')
      IF (present(info)) THEN
        CALL Append_Test(test_var,'   Intermediate Test: ' // info)
      ELSE
        CALL Append_Test(test_var,'   Intermediate Test')
      END IF
      CALL Append_Test(test_var,'')
  
      IF (test_var%nb_Test /= test_var%nb_OK + test_var%nb_Err) THEN
        CALL Append_Test(test_var,'ERROR while testing ' //                       &
                       test_var%test_name // ' module: nb_Test /= nb_OK + nb_Err')
        CALL Append_Test(test_var,'nb_Test' // TO_String(test_var%nb_Test))
        CALL Append_Test(test_var,'nb_OK  ' // TO_String(test_var%nb_OK))
        CALL Append_Test(test_var,'nb_Err ' // TO_String(test_var%nb_Err))
      END IF
  
      CALL Append_Test(test_var,'TESTING ' // info_loc //               &
                  ' module. Number of tests   :' // TO_String(test_var%nb_Test))
      CALL Append_Test(test_var,'TESTING ' // info_loc //               &
                  ' module. Number of error(s):' // TO_String(test_var%nb_Err))
      CALL Append_Test(test_var,'== END TESTING ' // info_loc // ' module ====')
  
      test_var%nb_Test = 0
      test_var%nb_OK   = 0
      test_var%nb_Err  = 0

      CALL Append_Test(test_var,'-------------------------------------------------------')
      CALL Flush_Test(test_var)
    
    END SUBROUTINE QD_Finalize_Intermediate_Test
  SUBROUTINE QD_Finalize_Test(test_var)
  IMPLICIT NONE

    TYPE (test_t),      intent(inout)    :: test_var

    CALL Append_Test(test_var,'-------------------------------------------------------')
    CALL Append_Test(test_var,'')

    IF (test_var%nb_AllTest /= test_var%nb_AllOK + test_var%nb_AllErr) THEN
      CALL Append_Test(test_var,'ERROR while testing ' //                       &
                       test_var%test_name // ' module: nb_Test /= nb_OK + nb_Err')
      CALL Append_Test(test_var,'nb_Test' // TO_String(test_var%nb_AllTest))
      CALL Append_Test(test_var,'nb_OK  ' // TO_String(test_var%nb_AllOK))
      CALL Append_Test(test_var,'nb_Err ' // TO_String(test_var%nb_AllErr))

    END IF

    CALL Append_Test(test_var,'TESTING ' // test_var%test_name //               &
                ' module. Number of tests   :' // TO_String(test_var%nb_AllTest))
    CALL Append_Test(test_var,'TESTING ' // test_var%test_name //               &
                ' module. Number of error(s):' // TO_String(test_var%nb_AllErr))
    CALL Append_Test(test_var,'== END TESTING ' // test_var%test_name // ' module ====')


    CALL Flush_Test(test_var)

    close(unit=test_var%test_log_file_unit)

  END SUBROUTINE QD_Finalize_Test

  SUBROUTINE QD_Initialize_Test(test_var,test_name,log_file_name,PrintFlag)
  IMPLICIT NONE

    TYPE (test_t),      intent(inout)          :: test_var
    character (len=*),  intent(in),  optional  :: test_name
    character (len=*),  intent(in),  optional  :: log_file_name
    logical,            intent(in),  optional  :: PrintFlag

    test_var%nb_AllTest = 0
    test_var%nb_AllOK   = 0
    test_var%nb_AllErr  = 0

    test_var%nb_Test = 0
    test_var%nb_OK   = 0
    test_var%nb_Err  = 0

    IF (present(PrintFlag)) test_var%PrintFlag = PrintFlag
  
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

    CALL Flush_Test(test_var)

  END SUBROUTINE QD_Initialize_Test

  SUBROUTINE QD_Append_Test_reslog(test_var,info,Print_res)
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

  END SUBROUTINE QD_Append_Test_reslog
  SUBROUTINE QD_Flush_Test_reslog(test_var)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : OUTPUT_UNIT
    IMPLICIT NONE

    TYPE (test_t),      intent(inout)         :: test_var

    IF (allocated(test_var%test_log)) THEN
      write(test_var%test_log_file_unit,'(a)') test_var%test_log
      deallocate(test_var%test_log)
    END IF
  
    IF (allocated(test_var%test_res)) THEN
      write(OUTPUT_UNIT,'(a)') test_var%test_res
      deallocate(test_var%test_res)
    END IF

  END SUBROUTINE QD_Flush_Test_reslog

  PURE FUNCTION QDTest_int_TO_char(i) RESULT(string)
  USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
  IMPLICIT NONE

    character (len=:),  allocatable             :: string
    integer,                        intent(in)  :: i


    character (len=:), allocatable  :: name_int
    integer :: clen

    ! first approximated size of name_int
    IF (i == 0) THEN
      clen = 1
    ELSE IF (i < 0) THEN
      clen = int(log10(abs(real(i,kind=real64))))+2
    ELSE
      clen = int(log10(real(i,kind=real64)))+1
    END IF

    ! allocate name_int
    allocate(character(len=clen) :: name_int)

    ! write i in name_int
    write(name_int,'(i0)') i

    ! transfert name_int in QD_int_TO_char
    string = trim(adjustl(name_int))

    ! deallocate name_int
    deallocate(name_int)

  END FUNCTION QDTest_int_TO_char
END MODULE QDUtil_Test_m
