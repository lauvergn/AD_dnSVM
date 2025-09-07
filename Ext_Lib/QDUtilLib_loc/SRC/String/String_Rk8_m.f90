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
MODULE QDUtil_String_Rk8_m
IMPLICIT NONE

  PUBLIC :: real_TO_char
  PUBLIC :: TO_string


  INTERFACE operator(//)
    MODULE PROCEDURE QDUtil_string_concat_Rk8
    MODULE PROCEDURE QDUtil_Rk8_concat_string

    MODULE PROCEDURE QDUtil_string_concat_Ck8
    MODULE PROCEDURE QDUtil_Ck8_concat_string
  END INTERFACE

  INTERFACE TO_string
    MODULE PROCEDURE QDUtil_Rk8_TO_string
    MODULE PROCEDURE QDUtil_Ck8_TO_string

    MODULE PROCEDURE QDUtil_Dim1Rk8_TO_string
    MODULE PROCEDURE QDUtil_Dim1Ck8_TO_string
  END INTERFACE

  INTERFACE real_TO_char
    MODULE PROCEDURE QDUtil_Rk8_TO_string
  END INTERFACE

  PRIVATE :: QDUtil_string_concat_Rk8,QDUtil_Rk8_concat_string
  PRIVATE :: QDUtil_string_concat_Ck8,QDUtil_Ck8_concat_string

  PRIVATE :: QDUtil_Rk8_TO_string,QDUtil_Ck8_TO_string
  PRIVATE :: QDUtil_Dim1Rk8_TO_string,QDUtil_Dim1Ck8_TO_string


CONTAINS

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

  FUNCTION QDUtil_Dim1Rk8_TO_string(tab,Rformat,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk8

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
 
  FUNCTION QDUtil_Dim1Ck8_TO_string(tab,Rformat,max_col)  RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk8

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

  FUNCTION QDUtil_string_concat_Rk8(str1,s2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk8
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str1
    real (kind=Rk8),                intent(in)  :: s2
  
    string = str1 // TO_string(s2)

  END FUNCTION QDUtil_string_concat_Rk8
  FUNCTION QDUtil_Rk8_concat_string(s1,str2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk8
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str2
    real (kind=Rk8),                intent(in)  :: s1
  
    string = TO_string(s1) // str2

  END FUNCTION QDUtil_Rk8_concat_string

  FUNCTION QDUtil_string_concat_Ck8(str1,s2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk8
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str1
    complex (kind=Rk8),             intent(in)  :: s2
  
    string = str1 // TO_string(s2)

  END FUNCTION QDUtil_string_concat_Ck8
  FUNCTION QDUtil_Ck8_concat_string(s1,str2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Rk8
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str2
    complex (kind=Rk8),             intent(in)  :: s1
  
    string = TO_string(s1) // str2

  END FUNCTION QDUtil_Ck8_concat_string

END MODULE QDUtil_String_Rk8_m
