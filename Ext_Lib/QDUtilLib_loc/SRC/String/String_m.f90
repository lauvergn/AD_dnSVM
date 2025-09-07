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
MODULE QDUtil_String_m
IMPLICIT NONE

  PRIVATE

  PUBLIC :: string_uppercase_TO_lowercase,string_lowercase_TO_uppercase,TO_uppercase,TO_lowercase

  PUBLIC :: string_IS_empty

  PUBLIC :: Read_line,Read_name_advNo

  PUBLIC :: ADD_TO_string,SET_string
  PUBLIC :: int_TO_char,logical_TO_char,real_TO_char
  PUBLIC :: TO_string

  PUBLIC :: alloc_array,dealloc_array

  PUBLIC :: Test_QDUtil_String

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

  INTERFACE TO_string
    MODULE PROCEDURE QDUtil_int32_TO_string,QDUtil_int64_TO_string
    MODULE PROCEDURE QDUtil_logical_TO_string
    MODULE PROCEDURE QDUtil_Rk4_TO_string,QDUtil_Rk8_TO_string,QDUtil_Rk16_TO_string
    MODULE PROCEDURE QDUtil_Ck4_TO_string,QDUtil_Ck8_TO_string,QDUtil_Ck16_TO_string

    MODULE PROCEDURE QDUtil_Dim1int32_TO_string,QDUtil_Dim1int64_TO_string
    MODULE PROCEDURE QDUtil_Dim1logical_TO_string
    MODULE PROCEDURE QDUtil_Dim1Rk4_TO_string,QDUtil_Dim1Rk8_TO_string,QDUtil_Dim1Rk16_TO_string
    MODULE PROCEDURE QDUtil_Dim1Ck4_TO_string,QDUtil_Dim1Ck8_TO_string,QDUtil_Dim1Ck16_TO_string
  END INTERFACE

  INTERFACE int_TO_char
    MODULE PROCEDURE QDUtil_int32_TO_string,QDUtil_int64_TO_string
  END INTERFACE
  INTERFACE logical_TO_char
    MODULE PROCEDURE QDUtil_logical_TO_string
  END INTERFACE
  INTERFACE real_TO_char
    MODULE PROCEDURE QDUtil_Rk4_TO_string,QDUtil_Rk8_TO_string,QDUtil_Rk16_TO_string
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
  FUNCTION QDUtil_logical_TO_string(l)  RESULT(string)

    character (len=:), allocatable  :: string
    logical, intent(in)             :: l

    !$OMP  CRITICAL (QDUtil_logical_TO_string_CRIT)
    IF (l) THEN
      string = 'T'
    ELSE
      string = 'F'
    END IF
    !$OMP  END CRITICAL (QDUtil_logical_TO_string_CRIT)

  END FUNCTION QDUtil_logical_TO_string
  FUNCTION QDUtil_int32_TO_string(i) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Rk8
    IMPLICIT NONE

    character (len=:),  allocatable             :: string
    integer (kind=Ik4),             intent(in)  :: i


    character (len=:), allocatable  :: name_int
    integer :: clen

    !$OMP  CRITICAL (QDUtil_int32_TO_string_CRIT)

    ! first approximated size of name_int
    IF (i == 0) THEN
      clen = 1
    ELSE IF (i < 0) THEN
      clen = int(log10(abs(real(i,kind=Rk8))))+2
    ELSE
      clen = int(log10(real(i,kind=Rk8)))+1
    END IF

    ! allocate name_int
    allocate(character(len=clen) :: name_int)

    ! write i in name_int
    write(name_int,'(i0)') i

    ! transfert name_int in QDUtil_int_TO_char
    string = trim(adjustl(name_int))

    ! deallocate name_int
    deallocate(name_int)
    !$OMP  END CRITICAL (QDUtil_int32_TO_string_CRIT)

  END FUNCTION QDUtil_int32_TO_string
  FUNCTION QDUtil_int64_TO_string(i) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik8,Rk8
    IMPLICIT NONE

    character (len=:),  allocatable             :: string
    integer (kind=Ik8),             intent(in)  :: i


    character (len=:), allocatable  :: name_int
    integer :: clen

    !$OMP  CRITICAL (QDUtil_int64_TO_string_CRIT)

    ! first approximated size of name_int
    IF (i == 0) THEN
      clen = 1
    ELSE IF (i < 0) THEN
      clen = int(log10(abs(real(i,kind=Rk8))))+2
    ELSE
      clen = int(log10(real(i,kind=Rk8)))+1
    END IF

    ! allocate name_int
    allocate(character(len=clen) :: name_int)

    ! write i in name_int
    write(name_int,'(i0)') i

    ! transfert name_int in QDUtil_int_TO_char
    string = trim(adjustl(name_int))

    ! deallocate name_int
    deallocate(name_int)

    !$OMP  END CRITICAL (QDUtil_int64_TO_string_CRIT)

  END FUNCTION QDUtil_int64_TO_string
  FUNCTION QDUtil_Rk16_TO_string(r,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk16
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    real (kind=Rk16), intent(in)              :: r
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_Rk16_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      write(name_real,'(' // Rformat // ')') r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

    ELSE
      write(name_real,*) r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))

      !this is add for ifort because ZERO is written as 0.000..0E+000
      i = len(string)
      IF (string(i:i) == '+' .OR. string(i:i) == '-') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'E' .OR. string(i:i) == 'e') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'D' .OR. string(i:i) == 'd') string = string(1:i-1) ! just in case we have 0.000...D+000

      !then, the 0 at the end are removed
      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))
    END IF

    !$OMP  END CRITICAL (QDUtil_Rk16_TO_string_CRIT)

  END FUNCTION QDUtil_Rk16_TO_string
  FUNCTION QDUtil_Rk8_TO_string(r,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk8
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    real (kind=Rk8),   intent(in)            :: r
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_Rk8_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      write(name_real,'(' // Rformat // ')') r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

    ELSE
      write(name_real,*) r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))

      !this is add for ifort because ZERO is written as 0.000..0E+000
      i = len(string)
      IF (string(i:i) == '+' .OR. string(i:i) == '-') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'E' .OR. string(i:i) == 'e') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'D' .OR. string(i:i) == 'd') string = string(1:i-1) ! just in case we have 0.000...D+000

      !then, the 0 at the end are removed
      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))

    END IF

    !$OMP  END CRITICAL (QDUtil_Rk8_TO_string_CRIT)

  END FUNCTION QDUtil_Rk8_TO_string
  FUNCTION QDUtil_Rk4_TO_string(r,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk4
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    real (kind=Rk4), intent(in)              :: r
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_Rk4_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      write(name_real,'(' // Rformat // ')') r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

    ELSE
      write(name_real,*) r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))

      !this is add for ifort because ZERO is written as 0.000..0E+000
      i = len(string)
      IF (string(i:i) == '+' .OR. string(i:i) == '-') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'E' .OR. string(i:i) == 'e') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'D' .OR. string(i:i) == 'd') string = string(1:i-1) ! just in case we have 0.000...D+000

      !then, the 0 at the end are removed
      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))
    END IF

    !$OMP  END CRITICAL (QDUtil_Rk4_TO_string_CRIT)

  END FUNCTION QDUtil_Rk4_TO_string

  FUNCTION QDUtil_Ck4_TO_string(c,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk4
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    complex (kind=Rk4), intent(in)           :: c
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_Ck4_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      string = '(' // TO_string(c%re,Rformat) // ',' // TO_string(c%im,Rformat) // ')'
    ELSE
      string = '(' // TO_string(c%re) // ',' // TO_string(c%im) // ')'
    END IF

    !$OMP  END CRITICAL (QDUtil_Ck4_TO_string_CRIT)

  END FUNCTION QDUtil_Ck4_TO_string
  FUNCTION QDUtil_Ck8_TO_string(c,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk8
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    complex (kind=Rk8), intent(in)           :: c
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_Ck8_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      string = '(' // TO_string(c%re,Rformat) // ',' // TO_string(c%im,Rformat) // ')'
    ELSE
      string = '(' // TO_string(c%re) // ',' // TO_string(c%im) // ')'
    END IF

    !$OMP  END CRITICAL (QDUtil_Ck8_TO_string_CRIT)

  END FUNCTION QDUtil_Ck8_TO_string
  FUNCTION QDUtil_Ck16_TO_string(c,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk16
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    complex (kind=Rk16), intent(in)           :: c
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_Ck16_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      string = '(' // TO_string(c%re,Rformat) // ',' // TO_string(c%im,Rformat) // ')'
    ELSE
      string = '(' // TO_string(c%re) // ',' // TO_string(c%im) // ')'
    END IF

    !$OMP  END CRITICAL (QDUtil_Ck16_TO_string_CRIT)

  END FUNCTION QDUtil_Ck16_TO_string

  FUNCTION QDUtil_Dim1logical_TO_string(tab,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable            :: string
    logical,           intent(in)             :: tab(:)
    integer,           intent(in), optional   :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1logical_TO_string_CRIT)
    string = ''
    icol   = 0
    DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
      icol = icol + 1
      string = string // TO_string(tab(i)) // ' '
      IF (present(max_col)) THEN
        IF (mod(icol,max_col) == 0) string = string // new_line('nl')
      END IF
    END DO
    string = string // TO_string((tab(ubound(tab,dim=1))))
    !$OMP  END CRITICAL (QDUtil_Dim1logical_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1logical_TO_string
  FUNCTION QDUtil_Dim1int32_TO_string(tab,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable  :: string
    integer (kind=Ik4), intent(in)             :: tab(:)
    integer,            intent(in), optional   :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1int32_TO_string_CRIT)
    string = ''
    icol   = 0
    DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
      icol = icol + 1
      string = string // TO_string(tab(i)) // ' '
      IF (present(max_col)) THEN
        IF (mod(icol,max_col) == 0) string = string // new_line('nl')
      END IF
    END DO
    string = string // TO_string((tab(ubound(tab,dim=1))))
    !$OMP  END CRITICAL (QDUtil_Dim1int32_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1int32_TO_string
  FUNCTION QDUtil_Dim1int64_TO_string(tab,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable  :: string
    integer (kind=Ik8), intent(in)             :: tab(:)
    integer,            intent(in), optional   :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1int64_TO_string_CRIT)
    string = ''
    icol   = 0
    DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
      icol = icol + 1
      string = string // TO_string(tab(i)) // ' '
      IF (present(max_col)) THEN
        IF (mod(icol,max_col) == 0) string = string // new_line('nl')
      END IF
    END DO
    string = string // TO_string((tab(ubound(tab,dim=1))))
    !$OMP  END CRITICAL (QDUtil_Dim1int64_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1int64_TO_string
  FUNCTION QDUtil_Dim1Rk4_TO_string(tab,Rformat,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable  :: string
    real (kind=Rk4),   intent(in)            :: tab(:)
    character (len=*), intent(in), optional  :: Rformat
    integer,           intent(in), optional   :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1Rk4_TO_string_CRIT)
    string = ''
    IF (present(Rformat)) THEN
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1
        string = string // TO_string(tab(i),Rformat) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))),Rformat)
    ELSE
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1
        string = string // TO_string(tab(i)) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))))
    END IF
    !$OMP  END CRITICAL (QDUtil_Dim1Rk4_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1Rk4_TO_string
  FUNCTION QDUtil_Dim1Rk8_TO_string(tab,Rformat,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable  :: string
    real (kind=Rk8),   intent(in)            :: tab(:)
    character (len=*), intent(in), optional  :: Rformat
    integer,           intent(in), optional   :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1Rk8_TO_string_CRIT)
    string = ''
    IF (present(Rformat)) THEN
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1
        string = string // TO_string(tab(i),Rformat) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))),Rformat)
    ELSE
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1
        string = string // TO_string(tab(i)) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))))
    END IF
    !$OMP  END CRITICAL (QDUtil_Dim1Rk8_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1Rk8_TO_string
  FUNCTION QDUtil_Dim1Rk16_TO_string(tab,Rformat,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable  :: string
    real (kind=Rk16),  intent(in)            :: tab(:)
    character (len=*), intent(in), optional  :: Rformat
    integer,           intent(in), optional   :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1Rk16_TO_string_CRIT)
    string = ''
    IF (present(Rformat)) THEN
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1
        string = string // TO_string(tab(i),Rformat) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))),Rformat)
    ELSE
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1
        string = string // TO_string(tab(i)) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))))
    END IF
    !$OMP  END CRITICAL (QDUtil_Dim1Rk16_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1Rk16_TO_string
  FUNCTION QDUtil_Dim1Ck4_TO_string(tab,Rformat,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable  :: string
    complex (kind=Rk4), intent(in)            :: tab(:)
    character (len=*),  intent(in), optional  :: Rformat
    integer,            intent(in), optional   :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1Ck4_TO_string_CRIT)
    string = ''
    IF (present(Rformat)) THEN
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1
        string = string // TO_string(tab(i),Rformat) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))),Rformat)
    ELSE
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1        
        string = string // TO_string(tab(i)) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))))
    END IF
    !$OMP  END CRITICAL (QDUtil_Dim1Ck4_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1Ck4_TO_string
  FUNCTION QDUtil_Dim1Ck8_TO_string(tab,Rformat,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable  :: string
    complex (kind=Rk8), intent(in)            :: tab(:)
    character (len=*),  intent(in), optional  :: Rformat
    integer,            intent(in), optional   :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1Ck8_TO_string_CRIT)
    string = ''
    IF (present(Rformat)) THEN
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1        
        string = string // TO_string(tab(i),Rformat) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))),Rformat)
    ELSE
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1        
        string = string // TO_string(tab(i)) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))))
    END IF
    !$OMP  END CRITICAL (QDUtil_Dim1Ck8_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1Ck8_TO_string
  FUNCTION QDUtil_Dim1Ck16_TO_string(tab,Rformat,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4,Ik8,Rk4,Rk8,Rk16

    character (len=:), allocatable  :: string
    complex (kind=Rk16), intent(in)            :: tab(:)
    character (len=*),   intent(in), optional  :: Rformat
    integer,             intent(in), optional  :: max_col

    integer :: i,icol,max_col_loc

    !$OMP CRITICAL (QDUtil_Dim1Ck16_TO_string_CRIT)
    string = ''
    IF (present(Rformat)) THEN
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1        
        string = string // TO_string(tab(i),Rformat) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))),Rformat)
    ELSE
      icol   = 0
      DO i=lbound(tab,dim=1),ubound(tab,dim=1)-1
        icol = icol + 1        
        string = string // TO_string(tab(i)) // ' '
        IF (present(max_col)) THEN
          IF (mod(icol,max_col) == 0) string = string // new_line('nl')
        END IF
      END DO
      string = string // TO_string((tab(ubound(tab,dim=1))))
    END IF
    !$OMP  END CRITICAL (QDUtil_Dim1Ck16_TO_string_CRIT)

  END FUNCTION QDUtil_Dim1Ck16_TO_string


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

  SUBROUTINE Test_QDUtil_String()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    character (len=:), allocatable :: string,lstring,ustring


    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_String'
    !logical, parameter :: debug = .FALSE.
    logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='String')
  
  
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
  
    !#5, 6, 7
    res_test = ('T' == TO_string(.TRUE.))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (T)')
    res_test = ('F' == TO_string(.FALSE.))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (F)')
    res_test = ('F T '// new_line('nl') //'F' == TO_string([.FALSE.,.TRUE.,.FALSE.],max_col=2))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (F T F)')
    IF (.NOT. res_test) THEN
      write(out_unit,*) 'Logicals:',[.FALSE.,.TRUE.,.FALSE.],' string (max_col=2): '
      write(out_unit,'(a)') TO_string([.FALSE.,.TRUE.,.FALSE.],max_col=2)
    END IF
    CALL Flush_Test(test_var)

    !#8-10
    res_test = ('0' == TO_string(0))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (0)')
    res_test = ('10' == TO_string(10_Ik4))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (10_Ik4)')
    res_test = ('-1099' == TO_string(-1099_Ik8))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-1099_Ik8)')
    res_test = ('-1 0 13' == TO_string([-1,0,13]))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-1 0 13)')
    CALL Flush_Test(test_var)

    !#11-13
    res_test = ('0.' == TO_string(0._Rkind))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (0.)')
    res_test = ('1.' == TO_string(1._Rk4))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (1._Rk4)')
    res_test = ('-10.' == TO_string(-10._Rk8))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-10._Rk8)')
    res_test = ('-999.5' == TO_string(-999.5_Rk16))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-999.5_Rk16)')
    res_test = ('-1. 0. 13.' == TO_string([-1._Rk4,0._Rk4,13._Rk4]))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-1. 0. 13.)')
    res_test = ('-1.000 0.000 13.000' == TO_string([-1._Rk4,0._Rk4,13._Rk4],Rformat='f6.3'))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-1.000 0.000 13.000)')
    IF (.NOT. res_test) write(out_unit,*) 'Reals:',[-1._Rk4,0._Rk4,13._Rk4], &
             ' string: ',TO_string([-1._Rk4,0._Rk4,13._Rk4],Rformat='f6.3')
    CALL Flush_Test(test_var)

    !#14-15
    res_test = ('(0.,0.)' == TO_string(cmplx(0._Rk4,0._Rk4,kind=Rk4)))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (0.,0.)')
    res_test = ('(0.,1.)' == TO_string(cmplx(0._Rk8,1._Rk8,kind=Rk8)))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (0.,1.)')
    res_test = ('(-1.,1.)' == TO_string(cmplx(-1._Rk8,1._Rk8,kind=Rk8)))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-1.,1.)')
    res_test = ('(-1.,1.) (0.,1.)' == TO_string([cmplx(-1._Rk8,1._Rk8,kind=Rk8),cmplx(0._Rk8,1._Rk8,kind=Rk8)]))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string(-1.,1.) (0.,1.)')

    !#17-18
    res_test = string_IS_empty(string)
    CALL Logical_Test(test_var,test1=res_test,info='string_IS_empty (F)',test2=.FALSE.)
    string = ''
    res_test = string_IS_empty(string)
    CALL Logical_Test(test_var,test1=res_test,info='string_IS_empty (T)')
    CALL Flush_Test(test_var)

    !#19
    deallocate(string)
    CALL ADD_TO_string(string,'1')
    res_test = (string == '1')
    CALL Logical_Test(test_var,test1=res_test,info='ADD_TO_string (1)')
    deallocate(string)
    CALL ADD_TO_string(string,'1','2')
    res_test = (string == '12')
    CALL Logical_Test(test_var,test1=res_test,info='ADD_TO_string (12)')
    deallocate(string)
    CALL ADD_TO_string(string,'1','2','3')
    res_test = (string == '123')
    CALL Logical_Test(test_var,test1=res_test,info='ADD_TO_string (123)')
    deallocate(string)
    CALL ADD_TO_string(string,'1','2','3','4')
    res_test = (string == '1234')
    CALL Logical_Test(test_var,test1=res_test,info='ADD_TO_string (1234)')
    deallocate(string)
    CALL ADD_TO_string(string,'1','2','3','4','5')
    res_test = (string == '12345')
    CALL Logical_Test(test_var,test1=res_test,info='ADD_TO_string (12345)')

    CALL SET_string(string,'1','2','3','4','5','6','7','8','9','10')
    res_test = (string == '12345678910')
    CALL Logical_Test(test_var,test1=res_test,info='SET_string (12345678910)')
    CALL Flush_Test(test_var)

    ! finalize the tests
    CALL Finalize_Test(test_var)

  END SUBROUTINE Test_QDUtil_String
END MODULE QDUtil_String_m
