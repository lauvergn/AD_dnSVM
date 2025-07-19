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
MODULE QDUtil_HermiteP_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE


  PUBLIC :: Test_HermiteP_QDUtil

CONTAINS
  ! Pn+1 = (an.x+bn).Pn - cn.Pn-1
  FUNCTION HermiteP0n_QDUtil(x,n,ReNorm) RESULT(P0n)
    IMPLICIT NONE

    integer,             intent(in)           :: n

    real (kind=Rkind),   allocatable          :: P0n(:)

    real (kind=Rkind),   intent(in)           :: x
    logical,             intent(in), optional :: ReNorm

    integer           :: i
    real (kind=Rkind) :: R
    real (kind=Rkind),   allocatable          :: Rnorm(:)

    IF ( n<0 ) RETURN


    IF (n == 0) THEN
      P0n   = [ONE]
      Rnorm = [sqrt(pi)]
    ELSE IF (n == 1) THEN
      P0n   = [ONE,TWO*x]
      Rnorm = sqrt(pi)*[ONE,TWO]
    ELSE
      allocate(P0n(0:n))
      allocate(Rnorm(0:n))

      P0n(0:1)   = [ONE,TWO*x]
      Rnorm(0:1) = sqrt(pi)*[ONE,TWO]

      DO i=2,n
        Rnorm(i) = Rnorm(i-1)*TWO*i
        P0n(i)   = TWO * ( x * P0n(i-1) -i * P0n(i-2) )
      END DO
    END IF

    IF (present(ReNorm)) THEN
      IF (ReNorm) THEN
        P0n = P0n /  sqrt(Rnorm)
      END IF
    ELSE
      P0n = P0n /  sqrt(Rnorm)
    END IF

    
  END FUNCTION HermiteP0n_QDUtil

  FUNCTION X_HermiteF_QDUtil(nq) RESULT(X)
    USE QDUtil_String_m
    IMPLICIT NONE
    real (kind=Rkind), allocatable :: X(:,:)

    integer,             intent(in)           :: nq


    integer :: i,d


    IF (nq < 1) THEN
      STOP 'ERROR in X_Hermite_QDUtil: nq < 0'
    END IF

    allocate(X(nq,nq))
    X = ZERO

    d = 0
    X(2,1) = sqrt(HALF*(d+1))
    DO i=2,nq-1
      d = i-1
      X(i-1,i) = sqrt(HALF*d)
      X(i+1,i) = sqrt(HALF*(d+1))
    END DO
    d = nq-1
    X(nq-1,nq) = sqrt(HALF*d)

  END FUNCTION X_HermiteF_QDUtil

  SUBROUTINE Test_HermiteP_QDUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_Quadrature_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind), parameter     :: ZeroTresh    = 10._Rkind**(-10)
    integer                          :: iVal,iExaVal
    real (kind=Rkind)                :: Val,ExaVal

    TYPE (Quadrature_t) :: xw_sine,xw_HO
    real (kind=Rkind), allocatable   :: bi(:)

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_Quadrature_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------
    CALL Initialize_Test(test_var,test_name='Quadrature')

    xw_sine = Init_Quadrature_QDUtil(nq=10,type_name='sine')
    CALL Write_Quadrature_QDUtil(xw_sine,nio=test_var%test_log_file_unit)


    xw_HO = Init_Quadrature_QDUtil(nq=11,type_name='HO')
    CALL Write_Quadrature_QDUtil(xw_HO,nio=test_var%test_log_file_unit)

    bi = exp(-HALF*xw_HO%x(1,:)**2)/sqrt(sqrt(pi))
    write(*,*) 'norm HO(1)',sum(bi**2*xw_HO%w)
    ! finalize the tests
    CALL Finalize_Test(test_var)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_HermiteP_QDUtil
END MODULE QDUtil_HermiteP_m
