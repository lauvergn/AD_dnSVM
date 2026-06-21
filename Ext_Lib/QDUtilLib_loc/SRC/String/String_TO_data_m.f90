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
MODULE QDUtil_String_TO_data_m
#ifndef __WITHRK16
#define __WITHRK16 1
#endif

  USE QDUtil_String_only_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: TO_data

  INTERFACE TO_data
    MODULE PROCEDURE QDUtil_string_TO_logical
    MODULE PROCEDURE QDUtil_string_TO_Ik4,QDUtil_string_TO_Ik8
    MODULE PROCEDURE QDUtil_string_TO_Rk4,QDUtil_string_TO_Rk8
    MODULE PROCEDURE QDUtil_string_TO_Ck4,QDUtil_string_TO_Ck8
#if __WITHRK16 == 1
    MODULE PROCEDURE QDUtil_string_TO_Rk16,QDUtil_string_TO_Ck16
#endif
  END INTERFACE

CONTAINS

  ELEMENTAL FUNCTION QDUtil_string_TO_logical(string,data_type)  RESULT(l)

    logical                                   :: l
    character (len=*), intent(in)             :: string
    logical,           intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    SELECT CASE (TO_Lowercase(string))
    CASE ('t','.true.')
      l = .TRUE.
    CASE ('f','.false.')
      l = .FALSE.
    CASE default
      ERROR STOP 'ERROR in QDUtil_string_TO_logical: the "string" value does not contain .TRUE. (t) or .FALSE. (f).'
    END SELECT

  END FUNCTION QDUtil_string_TO_logical
  ELEMENTAL FUNCTION QDUtil_string_TO_Ik4(string,data_type)  RESULT(I)
    USE QDUtil_NumParameters_m

    integer(kind=Ik4)                         :: I
    character (len=*), intent(in)             :: string
    integer(kind=Ik4), intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    integer :: ierr

    read(string,*,IOSTAT=ierr) I

    IF (ierr /= 0) &
      ERROR STOP 'ERROR in QDUtil_string_TO_Ik4: the "string" value does not contain an integer'

  END FUNCTION QDUtil_string_TO_Ik4
  ELEMENTAL FUNCTION QDUtil_string_TO_Ik8(string,data_type)  RESULT(I)
    USE QDUtil_NumParameters_m

    integer(kind=Ik8)                         :: I
    character (len=*), intent(in)             :: string
    integer(kind=Ik8), intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    integer :: ierr

    read(string,*,IOSTAT=ierr) I

    IF (ierr /= 0) &
      ERROR STOP 'ERROR in QDUtil_string_TO_Ik8: the "string" value does not contain an integer'

  END FUNCTION QDUtil_string_TO_Ik8
  ELEMENTAL FUNCTION QDUtil_string_TO_Rk4(string,data_type)  RESULT(R)
    USE QDUtil_NumParameters_m

    real(kind=Rk4)                            :: R
    character (len=*), intent(in)             :: string
    real(kind=Rk4),    intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    integer :: ierr

    read(string,*,IOSTAT=ierr) R

    IF (ierr /= 0) &
      ERROR STOP 'ERROR in QDUtil_string_TO_Rk4: the "string" value does not contain a real'

  END FUNCTION QDUtil_string_TO_Rk4
  ELEMENTAL FUNCTION QDUtil_string_TO_Rk8(string,data_type)  RESULT(R)
    USE QDUtil_NumParameters_m

    real(kind=Rk8)                            :: R
    character (len=*), intent(in)             :: string
    real(kind=Rk8),    intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    integer :: ierr

    read(string,*,IOSTAT=ierr) R

    IF (ierr /= 0) &
      ERROR STOP 'ERROR in QDUtil_string_TO_Rk8: the "string" value does not contain a real'

  END FUNCTION QDUtil_string_TO_Rk8
  ELEMENTAL FUNCTION QDUtil_string_TO_Ck4(string,data_type)  RESULT(C)
    USE QDUtil_NumParameters_m

    complex(kind=Rk4)                         :: C
    character (len=*), intent(in)             :: string
    complex(kind=Rk4), intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    integer :: ierr

    read(string,*,IOSTAT=ierr) C

    IF (ierr /= 0) &
      ERROR STOP 'ERROR in QDUtil_string_TO_Ck4: the "string" value does not contain a complex'

  END FUNCTION QDUtil_string_TO_Ck4
  ELEMENTAL FUNCTION QDUtil_string_TO_Ck8(string,data_type)  RESULT(C)
    USE QDUtil_NumParameters_m

    complex(kind=Rk8)                         :: C
    character (len=*), intent(in)             :: string
    complex(kind=Rk8), intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    integer :: ierr

    read(string,*,IOSTAT=ierr) C

    IF (ierr /= 0) &
      ERROR STOP 'ERROR in QDUtil_string_TO_Ck8: the "string" value does not contain a complex'

  END FUNCTION QDUtil_string_TO_Ck8
#if __WITHRK16 == 1
  ELEMENTAL FUNCTION QDUtil_string_TO_Rk16(string,data_type)  RESULT(R)
    USE QDUtil_NumParameters_m

    real(kind=Rk16)                           :: R
    character (len=*), intent(in)             :: string
    real(kind=Rk16),   intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    integer :: ierr

    read(string,*,IOSTAT=ierr) R

    IF (ierr /= 0) &
      ERROR STOP 'ERROR in QDUtil_string_TO_Rk16: the "string" value does not contain a real'

  END FUNCTION QDUtil_string_TO_Rk16
  ELEMENTAL FUNCTION QDUtil_string_TO_Ck16(string,data_type)  RESULT(C)
    USE QDUtil_NumParameters_m

    complex(kind=Rk16)                         :: C
    character (len=*),  intent(in)             :: string
    complex(kind=Rk16), intent(in)             :: data_type ! for the data type
    !logical,           intent(out), optional  :: err


    integer :: ierr

    read(string,*,IOSTAT=ierr) C

    IF (ierr /= 0) &
      ERROR STOP 'ERROR in QDUtil_string_TO_Ck16: the "string" value does not contain a complex'

  END FUNCTION QDUtil_string_TO_Ck16
#endif
END MODULE QDUtil_String_TO_data_m

SUBROUTINE Test_QDUtil_String_TO_data()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_TO_data_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    integer, parameter :: nepsi = -10 ! to set up epsilon (10.**nepsi)

    integer            :: j
    integer (kind=Ik4) :: j4
    integer (kind=Ik8) :: j8

    real               :: R
    real(kind=Rk4)     :: R4
    real(kind=Rk8)     :: R8
#if __WITHRK16 == 1
    real(kind=Rk16)    :: R16
#endif

    complex            :: C
    complex(kind=Rk4)  :: C4
    complex(kind=Rk8)  :: C8
#if __WITHRK16 == 1
    complex(kind=Rk16) :: C16
#endif

    logical            :: lo


    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_String_TO_data'
    !logical, parameter :: debug = .FALSE.
    logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='String_TO_data')


    !#1-8
    res_test = (TO_data('t',lo))
    CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=t')
    res_test = (TO_data('T',lo))
    CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=T')
    res_test = (TO_data('.true.',lo))
    CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=.true.')
    res_test = (TO_data('.TRUE.',lo))
    CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=.TRUE.')

    res_test = (.NOT. TO_data('f',lo))
    CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=f')
    res_test = (.NOT. TO_data('F',lo))
    CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=F')
    res_test = (.NOT. TO_data('.false.',lo))
    CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=.false.')
    res_test = (.NOT. TO_data('.FALSE.',lo))
    CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=.FALSE.')

    !res_test = (TO_data('a',lo))
    !CALL Logical_Test(test_var,test1=res_test,info='TO_logical: l=.FALSE.')

    !#9-12
    res_test = (12 == TO_data('12',j))
    CALL Logical_Test(test_var,test1=res_test,info='TO_int: Int=12')
    res_test = (-1 == TO_data('-1',j4))
    CALL Logical_Test(test_var,test1=res_test,info='TO_Ik4: Int=-1')
    res_test = (5000000000_Ik8 == TO_data('5000000000',j8))
    CALL Logical_Test(test_var,test1=res_test,info='TO_Ik4: Int=5*10^9')

    res_test = (all([0,1,-11] == TO_data([character(len=3)::'0','1','-11'],j)))
    CALL Logical_Test(test_var,test1=res_test,info='TO_int: Int=[0,1,-11]')


    !#13-18
    res_test = (abs(1. - TO_data('1.0',R)) < TEN**nepsi)
    CALL Logical_Test(test_var,test1=res_test,info='TO_real: R=1.')
    res_test = (abs(-2. - TO_data('-2.0',R4)) < TEN**nepsi)
    CALL Logical_Test(test_var,test1=res_test,info='TO_Rk4: R=-2.')
    res_test = (abs(TO_data('0.',R8)) < TEN**nepsi)
    CALL Logical_Test(test_var,test1=res_test,info='TO_Rk8: R=0.0')
    res_test = (abs(1.d-8 - TO_data('1.d-8',R8)) < TEN**nepsi)
    CALL Logical_Test(test_var,test1=res_test,info='TO_Rk8: R=1.d-8')
#if __WITHRK16 == 1
    res_test = (abs(1.e13_Rk16 - TO_data('1.e13',R16)) < TEN**(2*nepsi))
    CALL Logical_Test(test_var,test1=res_test,info='TO_Rk16: R=1.e13')
#endif
    res_test = (all([1._Rk8,2000._Rk8,-552345._Rk8] - TO_data([character(len=8)::'1.','2.e3','-552345.'],R8)<TEN**nepsi))
    CALL Logical_Test(test_var,test1=res_test,info='TO_Rk8: R=[1.,2000.,-552345.]')

    !#19
    res_test = (abs(EYE - TO_data('(0.,1.)',C)) < TEN**nepsi)
    CALL Logical_Test(test_var,test1=res_test,info='TO_cplx: C=(0.,1.)')
    res_test = (abs((-1._Rk4,1._Rk4) - TO_data('(-1.,1.)',C4)) < TEN**nepsi)
    CALL Logical_Test(test_var,test1=res_test,info='TO_Ck4: C=(-1.,1.)')
    res_test = (abs((-1._Rk8,1._Rk8) - TO_data('(-1.,1.)',C8)) < TEN**nepsi)
    CALL Logical_Test(test_var,test1=res_test,info='TO_Ck8: C=(-1.,1.)')
#if __WITHRK16 == 1
    res_test = (abs((-1._Rk16,1._Rk16) - TO_data('(-1.,1.)',C16)) < TEN**nepsi)
    CALL Logical_Test(test_var,test1=res_test,info='TO_Ck16: C=(-1.,1.)')
#endif

! finalize the tests
    CALL Finalize_Test(test_var)
  END SUBROUTINE Test_QDUtil_String_TO_data
