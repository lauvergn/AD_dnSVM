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
MODULE QDUtil_BoxAB_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE


  TYPE BoxAB_t
    real(kind=Rkind) :: A         = ZERO
    real(kind=Rkind) :: B         = PI
    integer          :: isym_grid = 0 ! Possible values: -1, 0, +1, the grids starts in A, A+dx/2, A+dx
    logical          :: ReNorm    = .TRUE. ! renormalization os the basis functions
  END TYPE BoxAB_t
  PUBLIC :: BoxAB_t,BoxAB_Quadrature,TabGB_BoxAB

  INTERFACE BoxAB_Quadrature
    MODULE PROCEDURE BoxAB_Quadrature_QDutil
  END INTERFACE
  INTERFACE TabGB_BoxAB
    MODULE PROCEDURE TabGB_BoxAB_QDUtil
  END INTERFACE
  INTERFACE TabB_BoxAB
    MODULE PROCEDURE TabB_BoxAB_QDUtil
  END INTERFACE
  INTERFACE BoxAB_func
    MODULE PROCEDURE BoxAB_func_QDUtil
  END INTERFACE

CONTAINS
  SUBROUTINE TabGB_BoxAB_QDUtil(d0GB,x,BoxAB)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: d0GB(:,:)
    real (kind=Rkind),   intent(in)           :: x(:)
    TYPE(BoxAB_t),       intent(in)           :: BoxAB

    integer           :: nb,nq
    integer           :: iq
    real (kind=Rkind),   allocatable          :: Rnorm(:)

    nq = size(d0GB,dim=1)
    nb = size(d0GB,dim=2)

    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in TabGB_BoxAB_QDUtil:'
      write(out_unit,*) 'nb < 1',nb
      STOP 'ERROR in TabGB_BoxAB_QDUtil: nb<1'
    END IF
    IF (nq /= size(x)) THEN
      write(out_unit,*) 'ERROR in TabGB_BoxAB_QDUtil:'
      write(out_unit,*) 'size(x) and nq differ',size(x),nq
      STOP 'ERROR in TabGB_BoxAB_QDUtil: size(x) and nq differ'
    END IF

    DO iq=1,nq
      CALL TabB_BoxAB(d0gb(iq,:),x(iq),BoxAB)
    END DO
    IF (nb == nq .AND. BoxAB%ReNorm)  d0gb(:,nq) = d0gb(:,nq) / sqrt(TWO)

  END SUBROUTINE TabGB_BoxAB_QDUtil
  SUBROUTINE TabB_BoxAB_QDUtil(d0b,x,BoxAB)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: d0b(:)
    real (kind=Rkind),   intent(in)           :: x
    TYPE(BoxAB_t),       intent(in)           :: BoxAB

    integer           :: ib,nb

    nb = size(d0b)
    IF ( nb < 1 ) RETURN

    DO ib=1,nb
      d0b(ib) = BoxAB_func(x,ib,BoxAB)
    END DO

  END SUBROUTINE TabB_BoxAB_QDUtil
  FUNCTION BoxAB_func_QDUtil(x,ib,BoxAB) RESULT(f)
    IMPLICIT NONE

    real(kind=Rkind)    :: f

    real (kind=Rkind),   intent(in)   :: x
    integer,             intent(in)   :: ib
    TYPE(BoxAB_t),       intent(in)   :: BoxAB

    !---------------------------------------------------------------------
    real(kind=Rkind) :: xx
    real(kind=Rkind), parameter :: Rnorm = ONE/sqrt(pi*HALF)
    !---------------------------------------------------------------------

    xx = mod((x-BoxAB%A)/(BoxAB%B-BoxAB%A)*pi*ib,pi+pi)
    f = sin(xx)
    IF (BoxAB%ReNorm) THEN
      f = f * Rnorm*sqrt(pi/(BoxAB%B-BoxAB%A))
    END IF

  END function BoxAB_func_QDUtil


  SUBROUTINE BoxAB_Quadrature_QDutil(x,w,nq,BoxAB,err)
    IMPLICIT NONE

    real (kind=Rkind), allocatable,  intent(inout) :: x(:),w(:)
    integer,                         intent(in)    :: nq
    TYPE(BoxAB_t),                   intent(in)    :: BoxAB
    integer,                         intent(inout) :: err

    integer           :: i
    real (kind=Rkind) :: dx

    err = 0
    dx  = (BoxAB%B-BoxAB%A)/nq
    x   = [(BoxAB%A+dx*(i-HALF),i=1,nq)]
    w   = [(dx,i=1,nq)]

  END SUBROUTINE BoxAB_Quadrature_QDutil
END MODULE QDUtil_BoxAB_m
