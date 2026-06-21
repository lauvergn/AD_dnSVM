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
MODULE QDUtil_String_m
#ifndef __WITHRK16
#define __WITHRK16 1
#endif
  USE QDUtil_String_only_m
  USE QDUtil_String_TO_data_m
  USE QDUtil_String_Rk4_m
  USE QDUtil_String_Rk8_m
  USE QDUtil_String_Rk16_m

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: string_uppercase_TO_lowercase,string_lowercase_TO_uppercase,TO_uppercase,TO_lowercase
  PUBLIC :: string_IS_empty
  PUBLIC :: Read_line,Read_name_advNo
  PUBLIC :: ADD_TO_string,SET_string
  PUBLIC :: alloc_array,dealloc_array
  PUBLIC :: alloc_NParray,dealloc_NParray

  PUBLIC :: int_TO_char,logical_TO_char,real_TO_char
  PUBLIC :: TO_string

  PUBLIC :: TO_data

  PUBLIC :: Operator (//)


  INTERFACE operator(//)
    MODULE PROCEDURE QDUtil_string_concat_logical
    MODULE PROCEDURE QDUtil_logical_concat_string

    MODULE PROCEDURE QDUtil_string_concat_Ik4,QDUtil_string_concat_Ik8
    MODULE PROCEDURE QDUtil_Ik4_concat_string,QDUtil_Ik8_concat_string
  END INTERFACE

  INTERFACE TO_string
    MODULE PROCEDURE QDUtil_int32_TO_string,QDUtil_int64_TO_string
    MODULE PROCEDURE QDUtil_logical_TO_string

    MODULE PROCEDURE QDUtil_Dim1int32_TO_string,QDUtil_Dim1int64_TO_string
    MODULE PROCEDURE QDUtil_Dim1logical_TO_string
  END INTERFACE

  INTERFACE int_TO_char
    MODULE PROCEDURE QDUtil_int32_TO_string,QDUtil_int64_TO_string
  END INTERFACE
  INTERFACE logical_TO_char
    MODULE PROCEDURE QDUtil_logical_TO_string
  END INTERFACE

CONTAINS
  PURE FUNCTION QDUtil_logical_TO_string(l)  RESULT(string)

    character (len=:), allocatable  :: string
    logical, intent(in)             :: l

    IF (l) THEN
      string = 'T'
    ELSE
      string = 'F'
    END IF

  END FUNCTION QDUtil_logical_TO_string
  PURE FUNCTION QDUtil_int32_TO_string(i) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4
    IMPLICIT NONE

    character (len=:),  allocatable             :: string
    integer (kind=Ik4),             intent(in)  :: i


    character (len=:), allocatable  :: name_int
    character (len=1), allocatable  :: ch
    integer (kind=Ik4) :: i0,iloc

    name_int = ''
    iloc = abs(i)
    DO
      i0 = mod(iloc,10)
      iloc = iloc/10_Ik4
      SELECT CASE(i0)
      CASE(0_Ik4)
        ch = '0'
      CASE(1_Ik4)
        ch = '1'
      CASE(2_Ik4)
        ch = '2'
      CASE(3_Ik4)
        ch = '3'
      CASE(4_Ik4)
        ch = '4'
      CASE(5_Ik4)
        ch = '5'
      CASE(6_Ik4)
        ch = '6'
      CASE(7_Ik4)
        ch = '7'
      CASE(8_Ik4)
        ch = '8'
      CASE(9_Ik4)
        ch = '9'
      END SELECT
      name_int = ch // name_int
      IF (iloc == 0_Ik4) EXIT
    END DO

    IF (i < 0_Ik4) THEN
      string = '-' // trim(adjustl(name_int))
    ELSE
      string = trim(adjustl(name_int))
    END IF

    ! deallocate name_int
    deallocate(name_int)

  END FUNCTION QDUtil_int32_TO_string
  PURE FUNCTION QDUtil_int64_TO_string(i) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik8
    IMPLICIT NONE

    character (len=:),  allocatable             :: string
    integer (kind=Ik8),             intent(in)  :: i


    character (len=:), allocatable  :: name_int
    character (len=1), allocatable  :: ch
    integer (kind=Ik8) :: i0,iloc

    name_int = ''
    iloc = abs(i)
    DO
      i0 = mod(iloc,10)
      iloc = iloc/10_ik8
      SELECT CASE(i0)
      CASE(0_ik8)
        ch = '0'
      CASE(1_ik8)
        ch = '1'
      CASE(2_ik8)
        ch = '2'
      CASE(3_ik8)
        ch = '3'
      CASE(4_ik8)
        ch = '4'
      CASE(5_ik8)
        ch = '5'
      CASE(6_ik8)
        ch = '6'
      CASE(7_ik8)
        ch = '7'
      CASE(8_Ik8)
        ch = '8'
      CASE(9_Ik8)
        ch = '9'
      END SELECT
      name_int = ch // name_int
      IF (iloc == 0_Ik8) EXIT
    END DO

    IF (i < 0_Ik8) THEN
      string = '-' // trim(adjustl(name_int))
    ELSE
      string = trim(adjustl(name_int))
    END IF

    deallocate(name_int)

  END FUNCTION QDUtil_int64_TO_string
  FUNCTION QDUtil_int32_TO_string_old(i) RESULT(string)
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

  END FUNCTION QDUtil_int32_TO_string_old
  FUNCTION QDUtil_int64_TO_string_old(i) RESULT(string)
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

  END FUNCTION QDUtil_int64_TO_string_old

  FUNCTION QDUtil_Dim1logical_TO_string(tab,max_col)  RESULT(string)

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
    USE QDUtil_NumParameters_m, ONLY : Ik4

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
    USE QDUtil_NumParameters_m, ONLY : Ik8

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

  FUNCTION QDUtil_string_concat_logical(str1,s2) RESULT(string)
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str1
    logical,                        intent(in)  :: s2
  
    string = str1 // TO_string(s2)

  END FUNCTION QDUtil_string_concat_logical
  FUNCTION QDUtil_logical_concat_string(s1,str2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str2
    logical,                        intent(in)  :: s1
  
    string = TO_string(s1) // str2

  END FUNCTION QDUtil_logical_concat_string

  FUNCTION QDUtil_string_concat_Ik4(str1,i2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str1
    integer (kind=Ik4),             intent(in)  :: i2
  
    string = str1 // TO_string(i2)

  END FUNCTION QDUtil_string_concat_Ik4
  FUNCTION QDUtil_Ik4_concat_string(i1,str2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik4
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str2
    integer (kind=Ik4),             intent(in)  :: i1
  
    string = TO_string(i1) // str2

  END FUNCTION QDUtil_Ik4_concat_string
  FUNCTION QDUtil_string_concat_Ik8(str1,i2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik8
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str1
    integer (kind=Ik8),             intent(in)  :: i2
  
    string = str1 // TO_string(i2)

  END FUNCTION QDUtil_string_concat_Ik8
  FUNCTION QDUtil_Ik8_concat_string(i1,str2) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : Ik8
    IMPLICIT NONE
  
    character (len=:),  allocatable             :: string
    character (len=*),              intent(in)  :: str2
    integer (kind=Ik8),             intent(in)  :: i1
  
    string = TO_string(i1) // str2

  END FUNCTION QDUtil_Ik8_concat_string
END MODULE QDUtil_String_m

SUBROUTINE Test_QDUtil_String()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
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
#if __WITHRK16 == 1
    res_test = ('-999.5' == TO_string(-999.5_Rk16))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-999.5_Rk16)')
#endif
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

    ! test concatenation
    string = 'coucou' // 1
    res_test = (string == 'coucou1')
    CALL Logical_Test(test_var,test1=res_test,info="'coucou' // 1")
    string = 'coucou' // -1_Ik4
    res_test = (string == 'coucou-1')
    CALL Logical_Test(test_var,test1=res_test,info="'coucou' // -1 (Ik4)")
    string = 'coucou' // 99999999999_Ik8
    res_test = (string == 'coucou99999999999')
    CALL Logical_Test(test_var,test1=res_test,info="'coucou' // 99999999999 (Ik8)")

    string = 1 // 'coucou' // 1
    res_test = (string == '1coucou1')
    CALL Logical_Test(test_var,test1=res_test,info="1 // 'coucou' // 1")
    string = +1_Ik8 // 'coucou' // -1_Ik4
    res_test = (string == '1coucou-1')
    CALL Logical_Test(test_var,test1=res_test,info="1 (Ik8) // 'coucou' // -1 (Ik4)")

    string = CONE // 'coucou' // ONE
    res_test = (string == '(1.,0.)coucou1.')
    CALL Logical_Test(test_var,test1=res_test,info="cplx // 'coucou' // real")
    string = TWO // 'coucou' // EYE
    res_test = (string == '2.coucou(0.,1.)')
    CALL Logical_Test(test_var,test1=res_test,info="real // 'coucou' // cplx")

    string = .TRUE. // 'coucou' // .FALSE.
    res_test = (string == 'TcoucouF')
    CALL Logical_Test(test_var,test1=res_test,info="logical // 'coucou' // logical")

    CALL Flush_Test(test_var)

    ! finalize the tests
    CALL Finalize_Test(test_var)

END SUBROUTINE Test_QDUtil_String
