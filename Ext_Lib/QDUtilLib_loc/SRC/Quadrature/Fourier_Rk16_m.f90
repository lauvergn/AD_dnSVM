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
MODULE QDUtil_Fourier_Rk16_m
#ifndef __WITHRK16
#define __WITHRK16 1
#endif
#if __WITHRK16 == 1

  USE QDUtil_NumParameters_m, ONLY : out_unit, Rkind => Rk16, pi => pi_Rk16
  IMPLICIT NONE

  PRIVATE

  real(kind=Rkind), parameter :: ZERO      = 0._Rkind
  real(kind=Rkind), parameter :: ONE       = 1._Rkind
  real(kind=Rkind), parameter :: TWO       = 2._Rkind
  real(kind=Rkind), parameter :: TEN       = 10._Rkind
  real(kind=Rkind), parameter :: HUNDRED   = 100._Rkind

  real(kind=Rkind), parameter :: HALF      = 0.5_Rkind
  real(kind=Rkind), parameter :: ONETENTH  = 0.1_Rkind
  real(kind=Rkind), parameter :: TWOTENTHS = TWO/TEN

  TYPE FourierAB_Rk16_t
    real(kind=Rkind) :: A         = -PI
    real(kind=Rkind) :: B         = +PI
    integer          :: isym_grid = 0 ! Possible values: -1, 0, +1, the grids starts in A, A+dx/2, A+dx
    logical          :: ReNorm    = .TRUE. ! renormalization os the basis functions
  END TYPE FourierAB_Rk16_t
  PUBLIC :: FourierAB_Rk16_t,Fourier_Quadrature,TabGB_Fourier

  INTERFACE Fourier_Quadrature
    MODULE PROCEDURE Fourier_Quadrature_Rk16_QDutil
  END INTERFACE
  INTERFACE TabGB_Fourier
    MODULE PROCEDURE TabGB_Fourier_Rk16_QDUtil
  END INTERFACE
  INTERFACE TabB_Fourier
    MODULE PROCEDURE TabB_Fourier_Rk16_QDUtil
  END INTERFACE
  INTERFACE Fourier_func
    MODULE PROCEDURE Fourier_func_Rk16_QDUtil
  END INTERFACE

CONTAINS
  SUBROUTINE TabGB_Fourier_Rk16_QDUtil(d0GB,x,FourierAB)
    IMPLICIT NONE

    real (kind=Rkind),        intent(inout)        :: d0GB(:,:)
    real (kind=Rkind),        intent(in)           :: x(:)
    TYPE(FourierAB_Rk16_t),   intent(in)           :: FourierAB

    integer           :: nb,nq
    integer           :: iq
    real (kind=Rkind),   allocatable          :: Rnorm(:)

    nq = size(d0GB,dim=1)
    nb = size(d0GB,dim=2)

    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in TabGB_Fourier_Rk16_QDUtil:'
      write(out_unit,*) 'nb < 1',nb
      STOP 'ERROR in TabGB_Fourier_Rk16_QDUtil: nb<1'
    END IF
    IF (nq /= size(x)) THEN
      write(out_unit,*) 'ERROR in TabGB_Fourier_Rk16_QDUtil:'
      write(out_unit,*) 'size(x) and nq differ',size(x),nq
      STOP 'ERROR in TabGB_Fourier_Rk16_QDUtil: size(x) and nq differ'
    END IF

    DO iq=1,nq
      CALL TabB_Fourier(d0gb(iq,:),x(iq),FourierAB)
    END DO
    IF (nb == nq .AND. mod(nb,2) == 0 .AND. FourierAB%ReNorm)  d0gb(:,nq) = d0gb(:,nq) / sqrt(TWO)

  END SUBROUTINE TabGB_Fourier_Rk16_QDUtil
  SUBROUTINE TabB_Fourier_Rk16_QDUtil(Fourier,x,FourierAB)
    IMPLICIT NONE

    real (kind=Rkind),        intent(inout)        :: Fourier(:)
    real (kind=Rkind),        intent(in)           :: x
    TYPE(FourierAB_Rk16_t),   intent(in)           :: FourierAB

    integer           :: ib,nb

    nb = size(Fourier)
    IF ( nb < 1 ) RETURN

    DO ib=1,nb
      Fourier(ib) = Fourier_Func(x,ib,FourierAB)
    END DO

  END SUBROUTINE TabB_Fourier_Rk16_QDUtil
  FUNCTION Fourier_Func_Rk16_QDutil(x,ib,FourierAB) RESULT(f)
    IMPLICIT NONE

    real(kind=Rkind)    :: f

    real (kind=Rkind),      intent(in)   :: x
    integer,                intent(in)   :: ib
    TYPE(FourierAB_Rk16_t), intent(in)   :: FourierAB

    !---------------------------------------------------------------------
    real(kind=Rkind) :: xx
    integer          :: ii
    real(kind=Rkind), parameter :: twopi = pi+pi
    real(kind=Rkind), parameter :: sqpi = ONE/sqrt(pi)
    real(kind=Rkind), parameter :: sq2pi = ONE/sqrt(pi+pi)
    !---------------------------------------------------------------------

    ii = ib/2
    xx = mod((x/(FourierAB%B-FourierAB%A)*twopi)*ii,twopi)

    IF (ii == 0) THEN
      IF (FourierAB%ReNorm) THEN
        f = ONE/sqrt(FourierAB%B-FourierAB%A)
      ELSE
        f = ONE
      END IF
    ELSE
      IF (mod(ib,2) == 0) THEN
        f = sin(xx)
      ELSE
        f = cos(xx)
      END IF
      IF (FourierAB%ReNorm) f = f /sqrt((FourierAB%B-FourierAB%A)/TWO)
    END IF
  END function Fourier_Func_Rk16_QDutil

  SUBROUTINE Fourier_Quadrature_Rk16_QDutil(x,w,nq,FourierAB,err)
    IMPLICIT NONE

    real (kind=Rkind), allocatable,  intent(inout) :: x(:),w(:)
    integer,                         intent(in)    :: nq
    TYPE(FourierAB_Rk16_t),          intent(in)    :: FourierAB
    integer,                         intent(inout) :: err

    integer           :: i
    real (kind=Rkind) :: dx

    err = 0
    IF (mod(nq,2) == 0 .AND. FourierAB%isym_grid /= 0) err = -1

    IF (err == 0) THEN
      dx  = (FourierAB%B-FourierAB%A)/nq
      SELECT CASE (FourierAB%isym_grid)
      CASE (-1)
        x   = [(FourierAB%A+dx*(i-ONE),i=1,nq)]
      CASE (0)
        x   = [(FourierAB%A+dx*(i-HALF),i=1,nq)]
      CASE (1)
        x   = [(FourierAB%A+dx*(i-ZERO),i=1,nq)]
      CASE default
        err = -2
        write(out_unit,*) 'ERROR in Fourier_Quadrature_Rk16_QDutil: Wrong isym_grid value'
        write(out_unit,*) 'isym_grid: ',FourierAB%isym_grid
        write(out_unit,*) 'Possible value: -1, 0, 1'
      END SELECT
      w = [(dx,i=1,nq)]
    END IF

  END SUBROUTINE Fourier_Quadrature_Rk16_QDutil
#endif
END MODULE QDUtil_Fourier_Rk16_m
