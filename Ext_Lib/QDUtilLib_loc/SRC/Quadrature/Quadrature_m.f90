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
MODULE QDUtil_Quadrature_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE

  TYPE Quadrature_t
    real (kind=Rkind), allocatable :: x(:,:) ! x(ndim,nq): position
    real (kind=Rkind), allocatable :: w(:)   ! w(ndim,nq): weight
    character (len=:), allocatable :: name
  END TYPE Quadrature_t


  PUBLIC :: Test_Quadrature_QDUtil,Quadrature_t,Init_Quadrature_QDUtil,Write_Quadrature_QDUtil

CONTAINS
  FUNCTION Init_Quadrature_QDUtil(nq,type_name,A,B) RESULT(Quadrature)
    USE QDUtil_String_m
    USE QDUtil_diago_m
    USE QDUtil_RW_MatVec_m
    IMPLICIT NONE
    TYPE (Quadrature_t) :: Quadrature

    integer,             intent(in)           :: nq
    character (len=*),   intent(in)           :: type_name
    real (kind=Rkind),   intent(in), optional :: A,B


    real (kind=Rkind) :: dx
    integer           :: i
    real (kind=Rkind), allocatable :: Xmat(:,:),DVR(:,:),x(:)

    SELECT CASE(TO_lowercase(type_name))
    CASE ('sine')
      Quadrature%name = 'sine'
      dx  = PI/nq
      Quadrature%x = reshape([(dx*(-HALF+i),i=1,nq)],shape=[1,nq])
      Quadrature%w = [(dx,i=1,nq)]
    CASE ('boxab')
      IF (.NOT. present(A) .AND. .NOT. present(B)) THEN
        STOP 'ERROR in Init_Quadrature_QDUtil: A and B must be present for BoxAB quadrature'
      END IF
      Quadrature%name = 'BoxAB'
      dx  = (B-A)/nq
      Quadrature%x = reshape([(A+dx*(-HALF+i),i=1,nq)],shape=[1,nq])
      Quadrature%w = [(dx,i=1,nq)]
    CASE ('ho','hermite')
      allocate(x(nq))
      allocate(DVR(nq,nq))

      Xmat = X_Hermite_QDUtil(nq)
      CALL Write_Mat(Xmat,nio=out_unit,nbcol=5,info='Xmat')
      CALL diagonalization(Xmat,x,DVR)

      !DO i=1,nq
      !  write(66,*) x(i),DVR(:,i)
      !END DO

      Quadrature%x = reshape(x,shape=[1,nq])

      Quadrature%w = [(ONE/DVR(i,i)**2,i=1,nq)]

    CASE default
      STOP 'ERROR in Init_Quadrature_QDUtil: no default quadrature'
    END SELECT
    
  END FUNCTION Init_Quadrature_QDUtil
  SUBROUTINE Write_Quadrature_QDUtil(Quadrature,nio,info)
    USE QDUtil_String_m
    IMPLICIT NONE
    TYPE (Quadrature_t), intent(in)           :: Quadrature
    integer,             intent(in), optional :: nio
    character (len=*),   intent(in), optional :: info

    integer :: i,nio_loc

    nio_loc = out_unit
    IF (present(nio)) nio_loc = nio

    IF (present(info)) write(nio_loc,'(a)') info
    IF (allocated(Quadrature%x)) THEN
      DO i=1,size(Quadrature%x,dim=1)
        write(nio_loc,*) 'x(' // TO_string(i) // ',:): ',(Quadrature%x(i,:))
      END DO
    ELSE
      write(nio_loc,'(a)') 'Quadrature%x is not allocated'
    END IF
    IF (allocated(Quadrature%x)) THEN
        write(nio_loc,*) 'w(:):   ',Quadrature%w
    ELSE
      write(nio_loc,'(a)') 'Quadrature%w is not allocated'
    END IF
  END SUBROUTINE Write_Quadrature_QDUtil

  FUNCTION X_Hermite_QDUtil(nq) RESULT(X)
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

  END FUNCTION X_Hermite_QDUtil

  SUBROUTINE Test_Quadrature_QDUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
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

  END SUBROUTINE Test_Quadrature_QDUtil
END MODULE QDUtil_Quadrature_m