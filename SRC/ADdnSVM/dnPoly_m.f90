!===============================================================================
!===============================================================================
!This file is part of AD_dnSVM.
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
!> @brief Module which deals with derivatives of a scalar functions.
!!
!! This module deals with polynomila of dnS_t
!!
!! @author David Lauvergnat
!! @date 26/04/2020
!!
MODULE ADdnSVM_dnPoly_m
  USE QDUtil_m
  USE ADdnSVM_dnS_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: dnMonomial,dnBox,dnJacobi,dnHermite,dnExpHermite
  PUBLIC :: dnLegendre0,dnLegendre
  PUBLIC :: dnFourier,dnFourier2

  INTERFACE dnMonomial
     MODULE PROCEDURE AD_dnMonomial
  END INTERFACE

  INTERFACE dnBox
     MODULE PROCEDURE AD_dnBox
  END INTERFACE

  INTERFACE dnFourier
     MODULE PROCEDURE AD_dnFourier
  END INTERFACE
  INTERFACE dnFourier2
     MODULE PROCEDURE AD_dnFourier2
  END INTERFACE

  INTERFACE dnLegendre0
     MODULE PROCEDURE AD_dnLegendre0
  END INTERFACE
  INTERFACE dnLegendre
     MODULE PROCEDURE AD_dnLegendre
  END INTERFACE

  INTERFACE dnJacobi
     MODULE PROCEDURE AD_dnJacobi
  END INTERFACE

  INTERFACE dnHermite
     MODULE PROCEDURE AD_dnHermite
  END INTERFACE

  INTERFACE dnExpHermite
     MODULE PROCEDURE AD_dnExpHermite
  END INTERFACE

CONTAINS
  ELEMENTAL FUNCTION AD_dnMonomial(x,i) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: i


    character (len=*), parameter :: name_sub='AD_dnMonomial'

    Sres = x**i

  END FUNCTION AD_dnMonomial
  ELEMENTAL FUNCTION AD_dnBox(x,i,ReNorm) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: i
    logical,   optional, intent(in)    :: ReNorm

    real(kind=Rkind), parameter :: sqpih  = ONE/sqrt(pi*HALF)


    character (len=*), parameter :: name_sub='AD_dnBox'


    IF (present(ReNorm)) THEN
      IF (ReNorm) THEN
        Sres = sin(x*real(i,kind=Rkind)) * sqpih
      ELSE
        Sres = sin(x*real(i,kind=Rkind))
      END IF
    ELSE
      Sres = sin(x*real(i,kind=Rkind)) * sqpih
    END IF

  END FUNCTION AD_dnBox
  ELEMENTAL FUNCTION AD_dnFourier(x,i,ReNorm) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: i
    logical,   optional, intent(in)    :: ReNorm

    real(kind=Rkind), parameter :: sqpi  = ONE/sqrt(pi)
    real(kind=Rkind), parameter :: sq2pi = ONE/sqrt(pi+pi)

    integer           :: ii
    TYPE (dnS_t)      :: xx
    real (kind=Rkind) :: Rnorm

    character (len=*), parameter :: name_sub='AD_dnFourier'


    ii = int(i/2)
    xx = x*real(ii,kind=Rkind)

    IF (ii == 0) THEN
      Sres = x ! initialisation
      Sres = ONE
      Rnorm = sq2pi
    ELSE IF (mod(i,2) == 0) THEN
      Sres = sin(xx)
      Rnorm = sqpi
    ELSE
      Sres = cos(xx)
      Rnorm = sqpi
    END IF


    IF (present(ReNorm)) THEN
      IF (ReNorm) THEN
        Sres = Sres * Rnorm
      END IF
    ELSE
      Sres = Sres * Rnorm
    END IF

  END FUNCTION AD_dnFourier
  ELEMENTAL FUNCTION AD_dnFourier2(x,i,ReNorm) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: i
    logical,   optional, intent(in)    :: ReNorm

    real(kind=Rkind), parameter :: sqpi  = ONE/sqrt(pi)
    real(kind=Rkind), parameter :: sq2pi = ONE/sqrt(pi+pi)

    integer           :: ii
    TYPE (dnS_t)      :: xx
    real (kind=Rkind) :: Rnorm

    character (len=*), parameter :: name_sub='AD_dnFourier2'


    ii = abs(i)
    xx = x*real(ii,kind=Rkind)

    IF (ii == 0) THEN
      Sres = x ! initialisation
      Sres = ONE
      Rnorm = sq2pi
    ELSE IF (i < 0) THEN
      Sres = sin(xx)
      Rnorm = sqpi
    ELSE ! i > 0
      Sres = cos(xx)
      Rnorm = sqpi
    END IF


    IF (present(ReNorm)) THEN
      IF (ReNorm) THEN
        Sres = Sres * Rnorm
      END IF
    ELSE
      Sres = Sres * Rnorm
    END IF

  END FUNCTION AD_dnFourier2
  ELEMENTAL FUNCTION AD_dnLegendre0(x,i,ReNorm) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: i
    logical,   optional, intent(in)    :: ReNorm

    TYPE (dnS_t)     :: P2,P1,P0
    integer          :: j
    real(kind=Rkind) :: Pnorm2

    character (len=*), parameter :: name_sub='AD_dnLegendre0'

    IF ( i<= 0) THEN
      Sres = x ! initialisation
      Sres = ONE
    ELSE IF ( i== 1) THEN
      Sres = x
    ELSE
      P0 = ONE
      P1 = x
      DO j=2,i
        P2 = (real(2*j-1,kind=Rkind)*x*P1 -real(j-1,kind=Rkind)*P0 )/real(j,kind=Rkind)
        P0 = P1
        P1 = P2
      END DO
      Sres = P2
      CALL dealloc_dnS(P0)
      CALL dealloc_dnS(P1)
      CALL dealloc_dnS(P2)
    END IF

    Pnorm2 = ONE/sqrt(TWO/(2*i+1))

    IF (present(ReNorm)) THEN
      IF (ReNorm) THEN
        Sres = Sres * Pnorm2
      END IF
    ELSE
      Sres = Sres * Pnorm2
    END IF

  END FUNCTION AD_dnLegendre0
  ELEMENTAL FUNCTION AD_dnLegendre(x,l,m,ReNorm) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: l,m
    logical,   optional, intent(in)    :: ReNorm

    TYPE (dnS_t)     :: pmm,pll,pmmp1,somx2,poly
    integer          :: i,ll
    real(kind=Rkind) :: fact,Pnorm2
    logical          :: ReNorm_loc

    character (len=*), parameter :: name_sub='AD_dnLegendre'

    ! IF (m < 0 .OR. l < 0 .OR. abs(x) > ONE) THEN
    !   write(out_unit,*) 'mauvais arguments dans poly_legendre :'
    !   write(out_unit,*) ' m l : ',m,l,' et x = ',x
    !   STOP
    ! END IF

    IF (present(ReNorm)) THEN
      ReNorm_loc = ReNorm
    ELSE
      ReNorm_loc = .TRUE.
    END IF

    Sres = x ! initialisation

    IF (m > l .OR. l < 0 .OR. m < 0) THEN
      Sres = ZERO
      RETURN
    END IF

    pmm = ONE

    IF (m > 0) THEN
      somx2 = sqrt(ONE - x*x)
      fact = ONE
      DO i=1,m
        pmm = -pmm*fact*somx2
        fact = fact+TWO
      END DO
    END IF


    IF (m == l) THEN
      Sres = pmm
    ELSE
      pmmp1 = x*(2*m+1)*pmm
      IF (l == m+1) THEN
        Sres = pmmp1
      ELSE
        DO ll=m+2,l
          pll   = ( x*(2*ll-1)*pmmp1 - (ll+m-1)*pmm ) / (ll-m)
          pmm   = pmmp1
          pmmp1 = pll
        END DO
        Sres = pll
      END IF
    END IF

    IF (ReNorm_loc) THEN
      Pnorm2 = TWO/(2*l+1)
      DO i=l-m+1,l+m
       Pnorm2 = Pnorm2 * i
      END DO
      Sres = Sres/sqrt(Pnorm2)
    END IF

  END FUNCTION AD_dnLegendre

  ELEMENTAL FUNCTION AD_dnJacobi(x,n,alpha,beta,ReNorm) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: n,alpha,beta
    logical,   optional, intent(in)    :: ReNorm

    TYPE (dnS_t)      :: P2,P1,P0
    integer           :: j,c
    real(kind=Rkind)  :: Pnorm2
    logical           :: ReNorm_loc

    character (len=*), parameter :: name_sub='AD_dnJacobi'

    IF (present(ReNorm)) THEN
      IF (ReNorm) THEN
        ReNorm_loc = .TRUE.
      ELSE
        ReNorm_loc = .FALSE.
      END IF
    ELSE
      ReNorm_loc = .TRUE.
    END IF


    IF ( n <= 0) THEN
      Sres = x ! initialisation
      Sres = ONE
    ELSE IF ( n == 1) THEN
      Sres = (alpha+1)+(alpha+beta+2)*(x-ONE)*HALF
    ELSE
      P0 = ONE
      P1 = (alpha+1)+(alpha+beta+2)*(x-ONE)*HALF
      DO j=2,n
        c = 2*j+alpha+beta
        P2 = ( (c-1)*(c*(c-2)*x + alpha**2-beta**2)*P1 - &
              2*(j+alpha-1)*(j+beta-1)*c*P0 ) / (2*j*(c-j)*(c-2))
        P0 = P1
        P1 = P2
      END DO
      Sres = P2
    END IF

    IF (ReNorm_loc) THEN
      Pnorm2 = TWO**(alpha+beta+1)/(2*n+alpha+beta+1) *                           &
                        gamma_perso(n+alpha+1)*gamma_perso(n+beta+1) /            &
                        ( gamma_perso(n+alpha+beta+1)*gamma_perso(n+1) )
      Sres = Sres / sqrt(Pnorm2)
    END IF

  END FUNCTION AD_dnJacobi

!===================================================
!
!   Normalized Hermite polynomial Hm(x,l)
!   with x in ( -inf =< x =< inf )
!
!===================================================
  ELEMENTAL FUNCTION AD_dnHermite(x,l,ReNorm)  RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: l
    logical,   optional, intent(in)    :: ReNorm


    ! Polynomial for     l,  l-1,l-2
    TYPE (dnS_t)      :: pl0,pl1,pl2
    real (kind=Rkind) :: Rnorm
    integer           :: i


    IF (l == 0) THEN
      Sres = x ! initialisation
      Sres = ONE
      Rnorm = ONE/sqrt(sqrt(pi))
    ELSE IF (l == 1) THEN
      Sres = TWO*x
      Rnorm = ONE/sqrt(TWO*sqrt(pi))
    ELSE

      pl2   = ONE
      pl1   = TWO*x
      Rnorm = sqrt(pi)*TWO

      DO i=2,l
        Rnorm = Rnorm*TWO*i
        pl0  = TWO*( x*pl1 - real(i-1,kind=Rkind)*pl2 )
        pl2   = pl1
        pl1   = pl0
      END DO
      Sres  = pl0
      Rnorm = ONE/sqrt(Rnorm)
    END IF

    IF (present(ReNorm)) THEN
      IF (ReNorm) THEN
        Sres = Sres * Rnorm
      END IF
    ELSE
      Sres = Sres * Rnorm
    END IF

  END FUNCTION AD_dnHermite
!===================================================
!
!   Normalized Hermite polynomial with gaussian part:Hm(x,l).exp(-x**2/2)
!   with x in ( -inf =< x =< inf )
!
!===================================================
  ELEMENTAL FUNCTION AD_dnExpHermite(x,l,ReNorm)  RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: x
    integer,             intent(in)    :: l
    logical,   optional, intent(in)    :: ReNorm

    IF (present(ReNorm)) THEN
      IF (ReNorm) THEN
        Sres = AD_dnHermite(x,l,ReNorm=.TRUE.) * exp(-x*x*HALF)
      ELSE
        Sres = AD_dnHermite(x,l,ReNorm=.FALSE.) * exp(-x*x*HALF)
      END IF
    ELSE
        Sres = AD_dnHermite(x,l,ReNorm=.TRUE.) * exp(-x*x*HALF)
    END IF

 END FUNCTION AD_dnExpHermite
END MODULE ADdnSVM_dnPoly_m
