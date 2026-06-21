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
! The above copyright notice and this permis sion notice shall be included in all
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
MODULE QDUtil_String_only_m
#ifndef __WITHRK16
#define __WITHRK16 1
#endif

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: string_uppercase_TO_lowercase,string_lowercase_TO_uppercase,TO_uppercase,TO_lowercase

  PUBLIC :: string_IS_empty

  PUBLIC :: Read_line,Read_name_advNo

  PUBLIC :: ADD_TO_string,SET_string
 
  PUBLIC :: alloc_array,dealloc_array
  PUBLIC :: alloc_NParray,dealloc_NParray


  INTERFACE string_uppercase_TO_lowercase
    MODULE PROCEDURE QDUtil_string_uppercase_TO_lowercase
  END INTERFACE
  INTERFACE string_lowercase_TO_uppercase
    MODULE PROCEDURE QDUtil_string_lowercase_TO_uppercase
  END INTERFACE
  INTERFACE TO_lowercase
    MODULE PROCEDURE QDUtil_string_TO_lowercase
  END INTERFACE
  INTERFACE TO_uppercase
    MODULE PROCEDURE QDUtil_string_TO_uppercase
  END INTERFACE

  INTERFACE strdup
    MODULE PROCEDURE QDUtil_strdup
  END INTERFACE
  INTERFACE string_IS_empty
    MODULE PROCEDURE QDUtil_string_IS_empty
  END INTERFACE

  INTERFACE ADD_TO_string
    MODULE PROCEDURE QDUtil_ADD_TO_Astring
  END INTERFACE

  INTERFACE SET_string
    MODULE PROCEDURE   QDUtil_SET_Astring
  END INTERFACE

  INTERFACE Read_line
    MODULE PROCEDURE QDUtil_Read_line
  END INTERFACE
  INTERFACE read_name_advNo
    MODULE PROCEDURE QDUtil_read_name_advNo
  END INTERFACE
  
  INTERFACE alloc_array
    MODULE PROCEDURE QDUtil_alloc_array_OF_Stringdim1 ! ,QDUtil_alloc_array_OF_ChLendim1
  END INTERFACE
  INTERFACE dealloc_array
    MODULE PROCEDURE QDUtil_dealloc_array_OF_Stringdim1
  END INTERFACE
  INTERFACE alloc_NParray
    MODULE PROCEDURE QDUtil_alloc_NParray_OF_Stringdim1 ! ,QDUtil_alloc_NParray_OF_ChLendim1
  END INTERFACE
  INTERFACE dealloc_NParray
    MODULE PROCEDURE QDUtil_dealloc_NParray_OF_Stringdim1
  END INTERFACE

CONTAINS
  PURE FUNCTION QDUtil_string_TO_lowercase(string) RESULT (lstring)
    IMPLICIT NONE

    character (len=*),         intent(in)  :: string
    character (len=len(string))            :: lstring

    integer  :: i,ascii_char

    lstring = string

    DO i=1,len_trim(lstring)
      ascii_char = iachar(lstring(i:i))
      IF (ascii_char >= 65 .AND. ascii_char <= 90) lstring(i:i) = achar(ascii_char+32)
    END DO

  END FUNCTION QDUtil_string_TO_lowercase
  PURE FUNCTION QDUtil_string_TO_uppercase(string) RESULT (ustring)
    IMPLICIT NONE
  
    character (len=*),         intent(in)  :: string
    character (len=len(string))            :: ustring

    integer  :: i,ascii_char
  
    ustring = string

    DO i=1,len_trim(ustring)
      ascii_char = iachar(ustring(i:i))
      IF (ascii_char >= 97 .AND. ascii_char <= 122) ustring(i:i) = achar(ascii_char-32)
    END DO

  END FUNCTION QDUtil_string_TO_uppercase
  !!@description: Change the case of a string (default lowercase)
  !!@param: string: character (len=*)
  !!@param: lower If the variable is present and its value is F,
  !!              the string will be converted into a uppercase string, otherwise,
  !!              it will be convert into a lowercase string.
  SUBROUTINE QDUtil_string_uppercase_TO_lowercase(string,lower)
  IMPLICIT NONE

    character (len=*), intent(inout)  :: string
    logical, optional  :: lower

    IF (present(lower)) THEN
      IF (lower) THEN
        string = QDUtil_string_TO_lowercase(string)
      ELSE
        string = QDUtil_string_TO_uppercase(string)
      END IF
    ELSE
      string = QDUtil_string_TO_lowercase(string)
    END IF

  END SUBROUTINE QDUtil_string_uppercase_TO_lowercase
  !!@description: Change the case of a string to upercase
  !!@param: string: character (len=*)
  SUBROUTINE QDUtil_string_lowercase_TO_uppercase(string)

    character (len=*), intent(inout)  :: string
  
    string = QDUtil_string_TO_uppercase(string)
 
  END SUBROUTINE QDUtil_string_lowercase_TO_uppercase
  PURE FUNCTION QDUtil_strdup(string)
  IMPLICIT NONE

   character (len=*), intent(in)   :: string
   character (len=:), allocatable  :: QDUtil_strdup

   allocate(character(len=len_trim(string)) :: QDUtil_strdup)
   QDUtil_strdup = trim(string)

  END FUNCTION QDUtil_strdup
  SUBROUTINE QDUtil_SET_Astring(string,string1,string2,string3,string4,string5, &
                              string6,string7,string8,string9,string10)
  IMPLICIT NONE

    character (len=:),  allocatable, intent(inout) :: string
    character (len=*),               intent(in)    :: string1
    character (len=*),  optional,    intent(in)    :: string2,string3,string4,string5
    character (len=*),  optional,    intent(in)    :: string6,string7,string8,string9,string10

    !$OMP  CRITICAL (QDUtil_SET_Astring_CRIT)

    string = string1
    IF (present(string2 )) string = string // string2
    IF (present(string3 )) string = string // string3
    IF (present(string4 )) string = string // string4
    IF (present(string5 )) string = string // string5
    IF (present(string6 )) string = string // string6
    IF (present(string7 )) string = string // string7
    IF (present(string8 )) string = string // string8
    IF (present(string9 )) string = string // string9
    IF (present(string10)) string = string // string10

    !$OMP  END CRITICAL (QDUtil_SET_Astring_CRIT)

  END SUBROUTINE QDUtil_SET_Astring
  SUBROUTINE QDUtil_ADD_TO_Astring(string,string1,string2,string3,string4,string5, &
                                          string6,string7,string8,string9,string10)
    IMPLICIT NONE
  
      character (len=:),  allocatable, intent(inout) :: string
      character (len=*),               intent(in)    :: string1
      character (len=*),  optional,    intent(in)    :: string2,string3,string4,string5
      character (len=*),  optional,    intent(in)    :: string6,string7,string8,string9,string10

      !$OMP  CRITICAL (QDUtil_ADD_TO_Astring_CRIT)

      IF (.NOT. allocated(string)) THEN
        string = string1
      ELSE
        string = string // string1
      END IF
      IF (present(string2 )) string = string // string2
      IF (present(string3 )) string = string // string3
      IF (present(string4 )) string = string // string4
      IF (present(string5 )) string = string // string5

      IF (present(string6 )) string = string // string6
      IF (present(string7 )) string = string // string7
      IF (present(string8 )) string = string // string8
      IF (present(string9 )) string = string // string9
      IF (present(string10)) string = string // string10

      !$OMP  END CRITICAL (QDUtil_ADD_TO_Astring_CRIT)
  
  END SUBROUTINE QDUtil_ADD_TO_Astring

  FUNCTION QDUtil_string_IS_empty(String)
    IMPLICIT NONE

    logical                          :: QDUtil_string_IS_empty
    character(len=*), intent(in)     :: String

    QDUtil_string_IS_empty = (len_trim(String) == 0)

  END FUNCTION QDUtil_string_IS_empty

  FUNCTION QDUtil_Read_line(nio,ioerr)  RESULT(string)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : IOSTAT_END,IOSTAT_EOR
    IMPLICIT NONE
    
    character(len=:), allocatable                 :: string
    integer,                      intent(in)      :: nio
    integer,                      intent(inout)   :: ioerr
  
  
    character(len=:), allocatable    :: line
    character(len=1)                 :: ch
  
  
    line = ""
    DO
      read(nio,'(a1)',IOSTAT=ioerr,advance='no') ch
      IF (ioerr /= 0) EXIT
      !write(6,*) 'ch: ',ch ; flush(6)
      line = line // ch
    END DO
    IF (ioerr == IOSTAT_EOR) ioerr = 0 ! end of record: the full line is read.
  
    string = line
  
    deallocate(line)
  
  END FUNCTION QDUtil_Read_line


  SUBROUTINE QDUtil_read_name_advNo(nio,Read_name,err_io)
    character(len=*), intent(inout) :: Read_name
    integer,          intent(inout) :: err_io
    integer,          intent(in)    :: nio

    character(len=1) :: chara
    logical          :: first
    integer :: ic

    Read_name = ''
    first     = .TRUE.
    ic        = 0
    DO
      err_io    = 0
      read(nio,'(a1)',IOSTAT=err_io,advance='no') chara

      IF (err_io /= 0)   EXIT
      !write(out_unitp,*) 'ic,chara',ic,'"',chara,'"'
      IF (chara == ' ' .AND. .NOT. first) EXIT

      IF (chara == ' ' .AND. first) CYCLE

      ic = ic + 1
      Read_name(ic:ic) = chara
      first = .FALSE.

    END DO
    !write(out_unitp,*) 'Read_name: ',trim(Read_name)

  END SUBROUTINE QDUtil_read_name_advNo


  SUBROUTINE QDUtil_alloc_array_OF_Stringdim1(tab,tab_ub,name_var,name_sub,tab_lb)
    USE QDUtil_Memory_base_m
    IMPLICIT NONE
  
    character (len=*), pointer,     intent(inout) :: tab(:)
    integer,                        intent(in)    :: tab_ub(:)
    integer, optional,              intent(in)    :: tab_lb(:)
    character (len=*),              intent(in)    :: name_var,name_sub
  
    integer, parameter :: ndim=1
  
    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_alloc_array_OF_Stringdim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------
  
  
    IF (associated(tab)) CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

    CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

    IF (present(tab_lb)) THEN
      CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

      memory = product(tab_ub(:)-tab_lb(:)+1)
      allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
    ELSE
      memory = product(tab_ub(:))
      allocate(tab(tab_ub(1)),stat=err_mem)
    END IF
    memory = len(tab(tab_ub(1))) * size(tab)
    CALL error_memo_allo(err_mem,memory,name_var,name_sub,'character')
  
  END SUBROUTINE QDUtil_alloc_array_OF_Stringdim1
  SUBROUTINE QDUtil_alloc_array_OF_ChLendim1(tab,tab_ub,ChLen,name_var,name_sub,tab_lb)
    USE QDUtil_Memory_base_m
    IMPLICIT NONE
  
    integer,                        intent(in)    :: ChLen
    character (len=*), pointer,     intent(inout) :: tab(:)
    integer,                        intent(in)    :: tab_ub(:)
    integer, optional,              intent(in)    :: tab_lb(:)
    character (len=*),              intent(in)    :: name_var,name_sub
  
    integer, parameter :: ndim=1
  
    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_alloc_array_OF_ChLendim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------
  
  
    IF (associated(tab)) CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)
  
    CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)
  
    IF (present(tab_lb)) THEN
      CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)
  
      memory = ChLen * product(tab_ub(:)-tab_lb(:)+1)
      allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
    ELSE
      memory = ChLen * product(tab_ub(:))
      allocate(tab(tab_ub(1)),stat=err_mem)
    END IF
    CALL error_memo_allo(err_mem,memory,name_var,name_sub,'character')
  
  END SUBROUTINE QDUtil_alloc_array_OF_ChLendim1
  SUBROUTINE QDUtil_dealloc_array_OF_Stringdim1(tab,name_var,name_sub)
    USE QDUtil_Memory_base_m
    IMPLICIT NONE
  
    character (len=*), pointer, intent(inout) :: tab(:)
    character (len=*),          intent(in)    :: name_var,name_sub
  
    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_dealloc_array_OF_Stringdim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------
  
    !IF (.NOT. associated(tab)) RETURN
    IF (.NOT. associated(tab)) CALL Write_error_null(name_sub_alloc,name_var,name_sub)
  
    memory = size(tab) * len(tab(lbound(tab,dim=1)))
    deallocate(tab,stat=err_mem)
    CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'character')
    nullify(tab)
  
  END SUBROUTINE QDUtil_dealloc_array_OF_Stringdim1


  SUBROUTINE QDUtil_alloc_NParray_OF_Stringdim1(tab,tab_ub,name_var,name_sub,tab_lb)
    USE QDUtil_Memory_base_m
    IMPLICIT NONE
  
    character (len=*), allocatable, intent(inout) :: tab(:)
    integer,                        intent(in)    :: tab_ub(:)
    integer, optional,              intent(in)    :: tab_lb(:)
    character (len=*),              intent(in)    :: name_var,name_sub
  
    integer, parameter :: ndim=1
  
    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_alloc_NParray_OF_Stringdim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------
  
  
    IF (allocated(tab)) CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

    CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

    IF (present(tab_lb)) THEN
      CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

      memory = product(tab_ub(:)-tab_lb(:)+1)
      allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
    ELSE
      memory = product(tab_ub(:))
      allocate(tab(tab_ub(1)),stat=err_mem)
    END IF
    memory = len(tab(tab_ub(1))) * size(tab)
    CALL error_memo_allo(err_mem,memory,name_var,name_sub,'character')
  
  END SUBROUTINE QDUtil_alloc_NParray_OF_Stringdim1
  SUBROUTINE QDUtil_alloc_NParray_OF_ChLendim1(tab,tab_ub,ChLen,name_var,name_sub,tab_lb)
    USE QDUtil_Memory_base_m
    IMPLICIT NONE
  
    integer,                        intent(in)    :: ChLen
    character (len=*), allocatable, intent(inout) :: tab(:)
    integer,                        intent(in)    :: tab_ub(:)
    integer, optional,              intent(in)    :: tab_lb(:)
    character (len=*),              intent(in)    :: name_var,name_sub
  
    integer, parameter :: ndim=1
  
    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_alloc_NParray_OF_ChLendim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------
  
    IF (allocated(tab)) CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)
  
    CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)
  
    IF (present(tab_lb)) THEN
      CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)
  
      memory = ChLen * product(tab_ub(:)-tab_lb(:)+1)
      allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
    ELSE
      memory = ChLen * product(tab_ub(:))
      allocate(tab(tab_ub(1)),stat=err_mem)
    END IF
    CALL error_memo_allo(err_mem,memory,name_var,name_sub,'character')
  
  END SUBROUTINE QDUtil_alloc_NParray_OF_ChLendim1
  SUBROUTINE QDUtil_dealloc_NParray_OF_Stringdim1(tab,name_var,name_sub)
    USE QDUtil_Memory_base_m
    IMPLICIT NONE
  
    character (len=*), allocatable, intent(inout) :: tab(:)
    character (len=*),              intent(in)    :: name_var,name_sub
  
    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub_alloc = 'QDUtil_dealloc_NParray_OF_Stringdim1'
    integer :: err_mem,memory
    logical,parameter :: debug=.FALSE.
    !logical,parameter :: debug=.TRUE.
    !----- for debuging --------------------------------------------------
  
    !IF (.NOT. allocated(tab)) RETURN
    IF (.NOT. allocated(tab)) CALL Write_error_null(name_sub_alloc,name_var,name_sub)
  
    memory = size(tab) * len(tab(lbound(tab,dim=1)))
    deallocate(tab,stat=err_mem)
    CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'character')
  
  END SUBROUTINE QDUtil_dealloc_NParray_OF_Stringdim1

END MODULE QDUtil_String_only_m

SUBROUTINE Test_QDUtil_String_only()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_only_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    character (len=:), allocatable :: string,lstring,ustring


    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_String_only'
    !logical, parameter :: debug = .FALSE.
    logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='String_only')
  
  
    lstring = "abcdefghijklmnopqrstuvwxyz0123456789"
    ustring = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    string  = lstring
  
    write(out_unit,*) 'string:           ',string
    write(out_unit,*) 'string lower:     ',TO_lowercase(string)
  
    write(out_unit,*) 'string upper:     ',TO_uppercase(string)
    CALL string_uppercase_TO_lowercase(string)
    write(out_unit,*) 'string lower sub: ',string
    CALL string_lowercase_TO_uppercase(string)
    write(out_unit,*) 'string upper sub: ',string
    write(out_unit,*) 'string empty:             ',string_IS_empty(string)
    string = ''
    write(out_unit,*) 'string empty:             ',string_IS_empty(string)
    string = '  '
    write(out_unit,*) 'string with blanks empty: ',string_IS_empty(string)
  
    string   = lstring
    !#1
    res_test = (ustring == TO_lowercase(string))
    CALL Logical_Test(test_var,test1=res_test,info='TO_lowercase',test2=.FALSE.)

    !#2
    res_test = (lstring == TO_lowercase(string))
    CALL Logical_Test(test_var,test1=res_test,info='TO_lowercase',test2=.TRUE.)

    !#3
    res_test = (ustring == TO_uppercase(string))
    CALL Logical_Test(test_var,test1=res_test,info='TO_uppercase',test2=.TRUE.)

    !#4
    res_test = (lstring == TO_uppercase(string))
    CALL Logical_Test(test_var,test1=res_test,info='TO_uppercase',test2=.FALSE.)

    CALL Flush_Test(test_var)
  
    !#17-18
    res_test = string_IS_empty(string)
    CALL Logical_Test(test_var,test1=res_test,info='string_IS_empty (F)',test2=.FALSE.)
    string = ''
    res_test = string_IS_empty(string)
    CALL Logical_Test(test_var,test1=res_test,info='string_IS_empty (T)')
    CALL Flush_Test(test_var)

    ! finalize the tests
    CALL Finalize_Test(test_var)

END SUBROUTINE Test_QDUtil_String_only
