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
MODULE QDUtil_HermiteH_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE

  TYPE HO_t
    real(kind=Rkind) :: xc        = ZERO
    real(kind=Rkind) :: Scale     = ONE
    logical          :: ReNorm    = .TRUE. ! renormalization of the basis functions
  END TYPE HO_t
  PUBLIC :: HO_t,TabGB_HO,TabGB_HermiteH, X_HermiteH

  INTERFACE TabGB_HO
    MODULE PROCEDURE TabGB_HO_QDUtil
  END INTERFACE
  INTERFACE TabB_HO
    MODULE PROCEDURE TabB_HO_QDUtil
  END INTERFACE
  INTERFACE TabGB_HermiteH
    MODULE PROCEDURE TabGB_HermiteH_QDUtil
  END INTERFACE
  INTERFACE TabB_HermiteH
    MODULE PROCEDURE TabB_HermiteH_QDUtil
  END INTERFACE
  INTERFACE X_HermiteH
    MODULE PROCEDURE X_HermiteH_QDUtil
  END INTERFACE  

CONTAINS

  SUBROUTINE TabGB_HO_QDUtil(d0GB,xHO,HO)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: d0GB(:,:)
    real (kind=Rkind),   intent(in)           :: xHO(:)
    TYPE(HO_t),          intent(in)           :: HO

    integer           :: iq,nb,nq

    nq = size(d0GB,dim=1)
    nb = size(d0GB,dim=2)

    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in TabGB_HO_QDUtil:'
      write(out_unit,*) 'nb < 1',nb
      STOP 'ERROR in TabGB_HO_QDUtil: nb<1'
    END IF
    IF (nq /= size(xHO)) THEN
      write(out_unit,*) 'ERROR in TabGB_HO_QDUtil:'
      write(out_unit,*) 'size(x) and nq differ',size(xHO),nq
      STOP 'ERROR in TabGB_HO_QDUtil: size(x) and nq differ'
    END IF

    DO iq=1,nq
      CALL TabB_HO(d0gb(iq,:),xHO(iq),HO)
    END DO
    
  END SUBROUTINE TabGB_HO_QDUtil
  SUBROUTINE TabB_HO_QDUtil(d0b,xHO,HO)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: d0b(:)
    real (kind=Rkind),   intent(in)           :: xHO
    TYPE(HO_t),          intent(in)           :: HO

    real (kind=Rkind) :: x

    x = HO%Scale * (xHO - HO%xc)
    CALL TabB_HermiteH(d0b,x,HO%ReNorm)

    IF (HO%ReNorm) THEN
      d0b = d0b * exp(-HALF*x**2) /  sqrt(HO%Scale)
    ELSE
      d0b = d0b * exp(-HALF*x**2)
    END IF
    
  END SUBROUTINE TabB_HO_QDUtil

  SUBROUTINE TabGB_HermiteH_QDUtil(d0GB,x,gauss,ReNorm)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: d0GB(:,:)
    real (kind=Rkind),   intent(in)           :: x(:)
    logical,             intent(in)           :: gauss
    logical,             intent(in)           :: ReNorm

    integer           :: nb,nq
    integer           :: iq
    real (kind=Rkind),   allocatable          :: Rnorm(:)

    nq = size(d0GB,dim=1)
    nb = size(d0GB,dim=2)

    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in TabGB_HermiteH_QDUtil:'
      write(out_unit,*) 'nb < 1',nb
      STOP 'ERROR in TabGB_HermiteH_QDUtil: nb<1'
    END IF
    IF (nq /= size(x)) THEN
      write(out_unit,*) 'ERROR in TabGB_HermiteH_QDUtil:'
      write(out_unit,*) 'size(x) and nq differ',size(x),nq
      STOP 'ERROR in TabGB_HermiteH_QDUtil: size(x) and nq differ'
    END IF

    DO iq=1,nq
      CALL TabB_HermiteH(d0gb(iq,:),x(iq),ReNorm)
      IF (gauss) d0gb(iq,:) = d0gb(iq,:) * exp(-HALF*x(iq)**2)
    END DO
    
  END SUBROUTINE TabGB_HermiteH_QDUtil

  SUBROUTINE TabB_HermiteH_QDUtil(P0n,x,ReNorm)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: P0n(:)
    real (kind=Rkind),   intent(in)           :: x
    logical,             intent(in)           :: ReNorm

    integer           :: degree
    integer           :: ib,id,nb
    real (kind=Rkind),   allocatable          :: Rnorm(:)

    nb = size(P0n)
    degree = nb-1
    IF ( degree < 0 ) RETURN

    allocate(Rnorm(nb))

    IF (degree == 0) THEN
      P0n(1)   = ONE
      Rnorm(1) = sqrt(pi)
    ELSE
      P0n(1:2)   = [ONE,TWO*x]
      Rnorm(1:2) = sqrt(pi)*[ONE,TWO]

      DO id=2,degree
        ib = id + 1
        Rnorm(ib) = Rnorm(ib-1)*TWO*id
        P0n(ib)   = TWO * ( x * P0n(ib-1) -(id-1) * P0n(ib-2) )
      END DO
    END IF

    IF (ReNorm) THEN
      P0n = P0n /  sqrt(Rnorm)
    END IF
    
  END SUBROUTINE TabB_HermiteH_QDUtil

  SUBROUTINE X_HermiteH_QDUtil(X)
    IMPLICIT NONE
    real (kind=Rkind), intent(inout) :: X(:,:)


    integer :: ib,nb

    nb = size(X,dim=1)
    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in X_HermiteH_QDUtil:'
      write(out_unit,*) 'nb < 0',nb
      STOP 'ERROR in X_HermiteH_QDUtil: nb < 0'
    END IF

    X = ZERO
    DO ib = 1, nb - 1
      X(ib,ib+1) = sqrt(real(ib,kind=Rkind)*HALF)
      X(ib+1,ib) = sqrt(real(ib,kind=Rkind)*HALF)
    END DO

  END SUBROUTINE X_HermiteH_QDUtil

END MODULE QDUtil_HermiteH_m
