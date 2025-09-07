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
MODULE QDUtil_LegendreP_Rk16_m
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
  real(kind=Rkind), parameter :: FOUR      = 4._Rkind
  real(kind=Rkind), parameter :: TEN       = 10._Rkind
  real(kind=Rkind), parameter :: HUNDRED   = 100._Rkind

  real(kind=Rkind), parameter :: HALF      = 0.5_Rkind
  real(kind=Rkind), parameter :: ONETENTH  = 0.1_Rkind
  real(kind=Rkind), parameter :: TWOTENTHS = TWO/TEN

  TYPE LegendreP_Rk16_t
    logical          :: ReNorm    = .TRUE. ! renormalization of the basis functions
  END TYPE LegendreP_Rk16_t
  PUBLIC :: LegendreP_Rk16_t,TabGB_LegendreP, X_LegendreP

  INTERFACE TabGB_LegendreP
    MODULE PROCEDURE TabGB_LegendreP_Rk16_QDUtil
  END INTERFACE
  INTERFACE TabB_LegendreP
    MODULE PROCEDURE TabB_LegendreP_Rk16_QDUtil
  END INTERFACE
  INTERFACE X_LegendreP
    MODULE PROCEDURE X_LegendreP_Rk16_QDUtil
  END INTERFACE

CONTAINS

  SUBROUTINE TabGB_LegendreP_Rk16_QDUtil(d0GB,x,ReNorm)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: d0GB(:,:)
    real (kind=Rkind),   intent(in)           :: x(:)
    logical,             intent(in)           :: ReNorm

    integer           :: nb,nq
    integer           :: iq
    real (kind=Rkind),   allocatable          :: Rnorm(:)

    nq = size(d0GB,dim=1)
    nb = size(d0GB,dim=2)

    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in TabGB_LegendreP_Rk16_QDUtil:'
      write(out_unit,*) 'nb < 1',nb
      STOP 'ERROR in TabGB_LegendreP_Rk16_QDUtil: nb<1'
    END IF
    IF (nq /= size(x)) THEN
      write(out_unit,*) 'ERROR in TabGB_LegendreP_Rk16_QDUtil:'
      write(out_unit,*) 'size(x) and nq differ',size(x),nq
      STOP 'ERROR in TabGB_LegendreP_Rk16_QDUtil: size(x) and nq differ'
    END IF

    DO iq=1,nq
      CALL TabB_LegendreP(d0gb(iq,:),x(iq),ReNorm)
    END DO
    
  END SUBROUTINE TabGB_LegendreP_Rk16_QDUtil

  SUBROUTINE TabB_LegendreP_Rk16_QDUtil(P0n,x,ReNorm)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: P0n(:)
    real (kind=Rkind),   intent(in)           :: x
    logical,             intent(in)           :: ReNorm

    integer           :: degree
    integer           :: ib,id,nb

    nb = size(P0n)
    degree = nb-1
    IF ( degree < 0 ) RETURN

    IF (degree == 0) THEN
      P0n(1)   = ONE
    ELSE
      P0n(1:2)   = [ONE,x]

      DO id=2,degree
        ib = id + 1
        P0n(ib)   = (x*(TWO*id-ONE)*P0n(ib-1) - (id-ONE)*P0n(ib-2) ) / real(id,kind=Rkind)
      END DO
    END IF

    IF (ReNorm) THEN
      DO ib=1,nb
        P0n(ib)   = P0n(ib) * sqrt(ib-HALF)
      END DO
    END IF
    
  END SUBROUTINE TabB_LegendreP_Rk16_QDUtil

  SUBROUTINE X_LegendreP_Rk16_QDUtil(X)
    IMPLICIT NONE
    real (kind=Rkind), intent(inout) :: X(:,:)


    integer :: ib,nb

    nb = size(X,dim=1)
    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in X_LegendreP_Rk16_QDUtil:'
      write(out_unit,*) 'nb < 0',nb
      STOP 'ERROR in X_LegendreP_Rk16_QDUtil: nb < 0'
    END IF

    X = ZERO
    DO ib = 1, nb - 1
      X(ib,ib+1) = real(ib,kind=Rkind)/sqrt(FOUR*ib**2-ONE)
      X(ib+1,ib) = real(ib,kind=Rkind)/sqrt(FOUR*ib**2-ONE)
    END DO

  END SUBROUTINE X_LegendreP_Rk16_QDUtil
#endif

END MODULE QDUtil_LegendreP_Rk16_m
