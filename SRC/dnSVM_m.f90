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
!> @brief Module which deals with derivatives of a scalar, vector (not yet) and matrices.
!!
!!
!! @author David Lauvergnat
!! @date 06/02/2022
!!
MODULE ADdnSVM_m
  USE ADdnSVM_dnS_m
  USE ADdnSVM_dnPoly_m
  USE ADdnSVM_dnFunc_m
  USE ADdnSVM_dnS_Op_m
  USE ADdnSVM_dnMat_m
  USE ADdnSVM_dnVec_m
  IMPLICIT NONE

  PRIVATE ADdnSVM_dnS_TO_TaylorDeltaQ, ADdnSVM_dnS_TO_TaylordnDeltaQ
  INTERFACE TO_Taylor
     MODULE PROCEDURE ADdnSVM_dnS_TO_TaylorDeltaQ, ADdnSVM_dnS_TO_TaylordnDeltaQ
  END INTERFACE
CONTAINS
  FUNCTION ADdnSVM_dnS_TO_TaylorDeltaQ(dnS,DeltaQ,nderiv) RESULT(Val)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real (kind=Rkind)                       :: Val
    TYPE (dnS_t),      intent(in)           :: dnS
    real (kind=Rkind), intent(in)           :: DeltaQ(:)
    integer,           intent(in), optional :: nderiv


    integer :: nderiv_loc,nVar_loc,i,j
    real (kind=Rkind), allocatable           :: Q1(:),M2(:,:),d3(:,:,:)


    nderiv_loc = get_nderiv(dnS)
    IF (present(nderiv)) nderiv_loc = min(nderiv,nderiv_loc)
    nVar_loc = get_nVar(dnS)
    IF (size(DeltaQ) /= nVar_loc .OR. nderiv_loc < 1) nderiv_loc = 0 ! just the zero order

    SELECT CASE (nderiv_loc)
    CASE(0)
      Val = get_d0(dnS)
    CASE(1)
      Val = get_d0(dnS) + dot_product(DeltaQ, get_d1(dnS))
    CASE(2)
      Q1  = get_d1(dnS) + HALF * matmul(get_d2(dnS),DeltaQ)
      Val = get_d0(dnS) + dot_product(DeltaQ,Q1)
    CASE(3)
      M2 = get_d2(dnS)
      d3 = get_d3(dnS)
      DO i=1,nVar_loc
      DO j=1,nVar_loc
        M2(j,i) = M2(j,i)/TWO + dot_product(d3(:,j,i), DeltaQ)/SIX
      END DO
      END DO
      Q1  = get_d1(dnS) + matmul(M2,DeltaQ)
      Val = get_d0(dnS) + dot_product(DeltaQ,Q1)
    END SELECT


  END FUNCTION ADdnSVM_dnS_TO_TaylorDeltaQ

  FUNCTION ADdnSVM_dnS_TO_TaylordnDeltaQ(dnS,DeltaQ,nderiv) RESULT(Val)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    TYPE (dnS_t)                            :: Val
    TYPE (dnS_t),      intent(in)           :: dnS
    TYPE (dnS_t),      intent(in)           :: DeltaQ(:)
    integer,           intent(in), optional :: nderiv


    integer :: nderiv_loc,nVar_loc,i,j
    TYPE (dnS_t),      allocatable           :: Q1(:),M2(:,:)
    real (kind=Rkind), allocatable           :: d3(:,:,:)


    nderiv_loc = get_nderiv(dnS)
    IF (present(nderiv)) nderiv_loc = min(nderiv,nderiv_loc)
    nVar_loc = get_nVar(dnS)
    IF (size(DeltaQ) /= nVar_loc .OR. nderiv_loc < 1) nderiv_loc = 0 ! just the zero order

    SELECT CASE (nderiv_loc)
    CASE(0)
      Val = get_d0(dnS)
    CASE(1)
      Val = get_d0(dnS) + dot_product(DeltaQ, get_d1(dnS))
    CASE(2)
      Q1  = get_d1(dnS) + HALF * matmul(get_d2(dnS),DeltaQ)
      Val = get_d0(dnS) + dot_product(DeltaQ,Q1)
    CASE(3)
      M2 = get_d2(dnS)
      d3 = get_d3(dnS)
      DO i=1,nVar_loc
      DO j=1,nVar_loc
        M2(j,i) = M2(j,i)/TWO + dot_product(d3(:,j,i), DeltaQ)/SIX
      END DO
      END DO
      Q1  = get_d1(dnS) + matmul(M2,DeltaQ)
      Val = get_d0(dnS) + dot_product(DeltaQ,Q1)
    END SELECT


  END FUNCTION ADdnSVM_dnS_TO_TaylordnDeltaQ

END MODULE ADdnSVM_m
