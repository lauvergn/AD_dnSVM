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
!! This module deals with operations on vectors or matrices of dnS_t
!!
!! @author David Lauvergnat
!! @date 26/04/2020
!!
MODULE ADdnSVM_dnFunc_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: RSphericalHarmonics,RSphericalHarmonics2

  INTERFACE RSphericalHarmonics
     MODULE PROCEDURE AD_RSphericalHarmonics
  END INTERFACE
  INTERFACE RSphericalHarmonics2
     MODULE PROCEDURE AD_RSphericalHarmonics2
  END INTERFACE

CONTAINS

  ! real spherical harmonics with the following convention:
  !  l is as usual (l>0)
  !  lm range is [0,2l], with
  !       lm=0  => cte
  !       lm=1  => sin(phi)
  !       lm=2  => cos(phi)
  !       lm=3  => sin(2phi)
  !       lm=4  => cos(2phi)
  !       .....
  ELEMENTAL FUNCTION AD_RSphericalHarmonics(th,phi,l,lm,ReNorm) RESULT(Sres)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    USE ADdnSVM_dnPoly_m
    IMPLICIT NONE

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: th,phi
    integer,             intent(in)    :: l,lm
    logical,   optional, intent(in)    :: ReNorm

    TYPE (dnS_t)      :: x
    integer           :: m

    x = cos(th)
    m = (lm+1)/2
    IF (present(ReNorm)) THEN
      Sres = dnLegendre(x,l,abs(m),ReNorm) * dnFourier(phi,lm,ReNorm)
    ELSE
      Sres = dnLegendre(x,l,abs(m)) * dnFourier(phi,lm)
    END IF

  END FUNCTION AD_RSphericalHarmonics

  ! real spherical harmonics with the following convention:
  !  l is as usual (l>0)
  !  m range is [-l,l], with
  !       m=0   => cte
  !       m=-1  => sin(phi)
  !       m=+1  => cos(phi)
  !       m=-2  => sin(2phi)
  !       m=+2  => cos(2phi)
  !       .....
  ELEMENTAL FUNCTION AD_RSphericalHarmonics2(th,phi,l,m,ReNorm) RESULT(Sres)
  USE QDUtil_m
  USE ADdnSVM_dnS_m
  USE ADdnSVM_dnPoly_m
  IMPLICIT NONE

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: th,phi
    integer,             intent(in)    :: l,m
    logical,   optional, intent(in)    :: ReNorm

    TYPE (dnS_t)      :: x

    x = cos(th)

    IF (present(ReNorm)) THEN
      Sres = dnLegendre(x,l,abs(m),ReNorm) * dnFourier2(phi,m,ReNorm)
    ELSE
      Sres = dnLegendre(x,l,abs(m)) * dnFourier2(phi,m)
    END IF

  END FUNCTION AD_RSphericalHarmonics2

END MODULE ADdnSVM_dnFunc_m
