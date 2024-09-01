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
MODULE QDUtil_MathUtil_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC gamma_perso
  INTERFACE gamma_perso
    MODULE PROCEDURE QDUtil_gamma_perso
  END INTERFACE

  PUBLIC factorial
  INTERFACE factorial
    MODULE PROCEDURE QDUtil_factorial
  END INTERFACE

  PUBLIC binomial
  INTERFACE binomial
    MODULE PROCEDURE QDUtil_Rbinomial
  END INTERFACE
  PUBLIC combi
  INTERFACE combi
    MODULE PROCEDURE QDUtil_Rbinomial
  END INTERFACE

  PUBLIC ibinomial
  INTERFACE ibinomial
    MODULE PROCEDURE QDUtil_Ibinomial
  END INTERFACE
  PUBLIC icombi
  INTERFACE icombi
    MODULE PROCEDURE QDUtil_Ibinomial
  END INTERFACE

  PUBLIC :: Test_QDUtil_MathUtil

  CONTAINS
  ELEMENTAL FUNCTION QDUtil_gamma_perso(n) RESULT(gamma_perso)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rkind)           :: gamma_perso
    integer,        intent(in) :: n

    integer          :: i

    gamma_perso = gamma(real(n,kind=Rkind))

    ! IF (n <= 0) THEN
      ! write(out_unit,*) 'ERROR in QDUtil_gamma_perso: n <= 0',n
      ! STOP 'ERROR in QDUtil_gamma_perso: n <= 0'
    ! END IF

    ! gamma_perso = ONE
    ! DO i=1,n-1
      ! gamma_perso = gamma_perso * i
    ! END DO

  END FUNCTION QDUtil_gamma_perso 
  FUNCTION QDUtil_factorial(n) RESULT(factorial)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rkind)           :: factorial
    integer,        intent(in) :: n

    IF (n < 0) THEN
      write(out_unit,*) 'ERROR in QDUtil_factorial: n < 0',n
      STOP 'ERROR in QDUtil_factorial: n < 0'
    END IF

    factorial = gamma_perso(n+1)

  END FUNCTION QDUtil_factorial
  FUNCTION QDUtil_Rbinomial(n,i) RESULT(binomial)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rkind)           :: binomial
    integer,        intent(in) :: n,i

    real(kind=Rkind) :: a
    integer          :: k

    IF (n < 0 .OR. i > n .OR. i < 0) THEN
      write(out_unit,*) 'ERROR in QDUtil_Rbinomial: n<0  or i<0 or i>n',n,i
      STOP 'ERROR in QDUtil_Rbinomial: n<0  or i<0 or i>n'
    END IF

    a = ONE
    DO k=1,n
      a = a * real(k,kind=Rkind)
    END DO
    DO k=1,n-i
      a = a / real(k,kind=Rkind)
    END DO
    DO k=1,i
      a = a / real(k,kind=Rkind)
    END DO
    binomial = a

    ! write(out_unit,*) 'binomial',n,i,a
  END FUNCTION QDUtil_Rbinomial
  FUNCTION QDUtil_Ibinomial(n,i) RESULT(icombi)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    integer               :: icombi
    integer,   intent(in) :: n,i

    integer :: a
    integer :: f1,f2,k

    IF (n <= 0 .OR. i > n .OR. i < 0) THEN
      write(out_unit,*) 'ERROR in QDUtil_Ibinomial: n<=0 or i<0 or i>n',n,i
      STOP 'ERROR in QDUtil_Ibinomial: n<=0 or i<0 or i>n'
    END IF

    IF (i > n-i) THEN
      f1 = i
      f2 = n-i
    ELSE
      f2 = i
      f1 = n-i
    END IF
    a = 1
    DO k=f1+1,n
      a = a * k
    END DO
    DO k=1,f2
      a = a / k
    END DO
    icombi = a

    !write(out_unit,*) 'combi',n,i,a
    !STOP
  END FUNCTION QDUtil_Ibinomial
  SUBROUTINE Test_QDUtil_MathUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind), parameter     :: ZeroTresh    = 10._Rkind**(-10)
    integer                          :: iVal,iExaVal
    real (kind=Rkind)                :: Val,ExaVal


    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_MathUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------
    !Val = gamma_perso(0)
    !write(out_unit,*) 'gamma_perso(0)',Val
    !Val = factorial(-1)
    !write(out_unit,*) 'factorial(-1)',Val

    CALL Initialize_Test(test_var,test_name='MathUtil')

    Val = gamma_perso(5)
    ExaVal = real(1*2*3*4,kind=Rkind)
    res_test = (abs(Val-ExaVal) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='gamma_perso(5)')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'Val,ExaVal,Diff',Val,ExaVal,Val-ExaVal
    END IF
    CALL Flush_Test(test_var)

    Val = factorial(5)
    ExaVal = real(1*2*3*4*5,kind=Rkind)
    res_test = (abs(Val-ExaVal) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='factorial(5)')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'Val,ExaVal,Diff',Val,ExaVal,Val-ExaVal
    END IF
    CALL Flush_Test(test_var)

    Val = binomial(5,2)
    ExaVal = real(1*2*3*4*5,kind=Rkind)/real(1*2,kind=Rkind)/real(1*2*3,kind=Rkind)
    res_test = (abs(Val-ExaVal) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='binomial(5,2)')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'Val,ExaVal,Diff',Val,ExaVal,Val-ExaVal
    END IF

    Val = combi(5,2)
    ExaVal = real(1*2*3*4*5,kind=Rkind)/real(1*2,kind=Rkind)/real(1*2*3,kind=Rkind)
    res_test = (abs(Val-ExaVal) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='combi(5,2)')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'Val,ExaVal,Diff',Val,ExaVal,Val-ExaVal
    END IF
    CALL Flush_Test(test_var)


    iVal = ibinomial(5,2)
    iExaVal = (1*2*3*4*5)/(1*2)/(1*2*3)
    res_test = (abs(iVal-iExaVal) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='ibinomial(5,2)')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'iVal,iExaVal,iDiff',iVal,iExaVal,iVal-iExaVal
    END IF
    iVal = icombi(5,2)
    iExaVal = (1*2*3*4*5)/(1*2)/(1*2*3)
    res_test = (abs(iVal-iExaVal) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='icombi(5,2)')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'iVal,iExaVal,iDiff',iVal,iExaVal,iVal-iExaVal
    END IF
    CALL Flush_Test(test_var)


    ! finalize the tests
    CALL Finalize_Test(test_var)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_QDUtil_MathUtil
END MODULE QDUtil_MathUtil_m