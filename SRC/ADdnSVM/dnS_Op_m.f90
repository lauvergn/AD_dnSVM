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
MODULE ADdnSVM_dnS_Op_m
  USE ADLib_NumParameters_m
  USE ADdnSVM_dnS_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: dot_product,product,sum,matmul


    INTERFACE dot_product
       MODULE PROCEDURE AD_dot_product_VecOFdnS,AD_dot_product_Vec_VecOFdnS,AD_dot_product_VecOFdnS_Vec
    END INTERFACE
    INTERFACE product
       MODULE PROCEDURE AD_product_VecOFdnS
    END INTERFACE
    INTERFACE sum
       MODULE PROCEDURE AD_sum_VecOFdnS
    END INTERFACE

    INTERFACE matmul
       !MODULE PROCEDURE AD_matmul_MatOFdnS_VecOFdnS,AD_matmul_MatOFdnS_Vec,AD_matmul_Mat_VecOFdnS
       MODULE PROCEDURE AD_matmul_MatOFdnS_VecOFdnS
    END INTERFACE


CONTAINS

    FUNCTION AD_dot_product_VecOFdnS(VecA,VecB) RESULT(Sres)
      USE ADLib_NumParameters_m

      TYPE (dnS_t)                       :: Sres
      TYPE (dnS_t),        intent(in)    :: VecA(:),VecB(:)

      integer :: i
      integer :: err_dnS_loc
      real(kind=Rkind) :: d0f,d1f,d2f,d3f
      character (len=*), parameter :: name_sub='AD_dot_product_VecOFdnS'


      IF (size(VecA) /= size(VecB)) THEN
         write(out_unitp,*) ' ERROR in ',name_sub
         write(out_unitp,*) '  size of both vectors are different'
         write(out_unitp,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'Problem in dot_product_VecOFdnS'
      END IF

      IF (size(VecA) < 1) THEN
         write(out_unitp,*) ' ERROR in ',name_sub
         write(out_unitp,*) '  size of both vectors are < 1'
         write(out_unitp,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'Problem in dot_product_VecOFdnS'
      END IF

      !Sres = VecA(lbound(VecA,dim=1)) ! for the initialization
      Sres = ZERO
      DO i=lbound(VecA,dim=1),ubound(VecA,dim=1)
        Sres = Sres + VecA(i) * VecB(lbound(VecB,dim=1)+i-1)
      END DO

    END FUNCTION AD_dot_product_VecOFdnS
    FUNCTION AD_dot_product_VecOFdnS_Vec(VecA,VecB) RESULT(Sres)
      USE ADLib_NumParameters_m

      TYPE (dnS_t)                       :: Sres
      TYPE (dnS_t),        intent(in)    :: VecA(:)
      real(kind=Rkind),    intent(in)    :: VecB(:)

      integer :: i
      integer :: err_dnS_loc
      real(kind=Rkind) :: d0f,d1f,d2f,d3f
      character (len=*), parameter :: name_sub='AD_dot_product_VecOFdnS_Vec'


      IF (size(VecA) /= size(VecB)) THEN
         write(out_unitp,*) ' ERROR in ',name_sub
         write(out_unitp,*) '  size of both vectors are different'
         write(out_unitp,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'Problem in AD_dot_product_VecOFdnS_Vec'
      END IF

      IF (size(VecA) < 1) THEN
         write(out_unitp,*) ' ERROR in ',name_sub
         write(out_unitp,*) '  size of both vectors are < 1'
         write(out_unitp,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'Problem in AD_dot_product_VecOFdnS_Vec'
      END IF


      !Sres = VecA(lbound(VecA,dim=1)) ! for the initialization
      Sres = ZERO
      DO i=lbound(VecA,dim=1),ubound(VecA,dim=1)
        Sres = Sres + VecA(i) * VecB(lbound(VecB,dim=1)+i-1)
      END DO

    END FUNCTION AD_dot_product_VecOFdnS_Vec
    FUNCTION AD_dot_product_Vec_VecOFdnS(VecA,VecB) RESULT(Sres)
      USE ADLib_NumParameters_m

      TYPE (dnS_t)                       :: Sres
      TYPE (dnS_t),        intent(in)    :: VecB(:)
      real(kind=Rkind),    intent(in)    :: VecA(:)

      integer :: i
      integer :: err_dnS_loc
      real(kind=Rkind) :: d0f,d1f,d2f,d3f
      character (len=*), parameter :: name_sub='AD_dot_product_Vec_VecOFdnS'


      IF (size(VecA) /= size(VecB)) THEN
         write(out_unitp,*) ' ERROR in ',name_sub
         write(out_unitp,*) '  size of both vectors are different'
         write(out_unitp,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'Problem in AD_dot_product_Vec_VecOFdnS'
      END IF

      IF (size(VecA) < 1) THEN
         write(out_unitp,*) ' ERROR in ',name_sub
         write(out_unitp,*) '  size of both vectors are < 1'
         write(out_unitp,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'Problem in AD_dot_product_Vec_VecOFdnS'
      END IF

      !Sres = VecB(lbound(VecB,dim=1)) ! for the initialization
      Sres = ZERO
      DO i=lbound(VecA,dim=1),ubound(VecA,dim=1)
        Sres = Sres + VecA(i) * VecB(lbound(VecB,dim=1)+i-1)
      END DO

    END FUNCTION AD_dot_product_Vec_VecOFdnS
    FUNCTION AD_product_VecOFdnS(Vec) RESULT(Sres)
      USE ADLib_NumParameters_m

      TYPE (dnS_t)                              :: Sres
      TYPE (dnS_t),        intent(in)           :: Vec(:)

      integer :: i
      character (len=*), parameter :: name_sub='AD_product_VecOFdnS'

      !Sres = Vec(lbound(Vec,dim=1)) ! for the initialization
      Sres = ONE
      DO i=lbound(Vec,dim=1),ubound(Vec,dim=1)
        Sres = Sres * Vec(i)
      END DO

    END FUNCTION AD_product_VecOFdnS
    FUNCTION AD_SUM_VecOFdnS(Vec) RESULT(Sres)
      USE ADLib_NumParameters_m

      TYPE (dnS_t)                              :: Sres
      TYPE (dnS_t),        intent(in)           :: Vec(:)

      integer :: i
      character (len=*), parameter :: name_sub='AD_SUM_VecOFdnS'

      !Sres = Vec(lbound(Vec,dim=1)) ! for the initialization
      Sres = ZERO
      DO i=lbound(Vec,dim=1),ubound(Vec,dim=1)
        Sres = Sres + Vec(i)
      END DO

    END FUNCTION AD_SUM_VecOFdnS

    FUNCTION AD_matmul_MatOFdnS_VecOFdnS(Mat,Vec) RESULT(Vres)
      USE ADLib_NumParameters_m

      TYPE (dnS_t),        intent(in)           :: Vec(:)
      TYPE (dnS_t),        intent(in)           :: Mat(:,:)

      TYPE (dnS_t)                              :: Vres(size(Mat,dim=1))

      integer :: i,im
      character (len=*), parameter :: name_sub='AD_matmul_MatOFdnS_VecOFdnS'


      IF (size(Vec) /= size(Mat,dim=2)) THEN
         write(out_unitp,*) ' ERROR in ',name_sub
         write(out_unitp,*) '  size of Vec(:) and Mat(.,:) are different'
         write(out_unitp,*) '  size(Vec),size(Mat,dim=2)',size(Vec),size(Mat,dim=2)
         STOP 'Problem in AD_matmul_MatOFdnS_VecOFdnS'
      END IF

      DO i=lbound(Vres,dim=1),ubound(Vres,dim=1)
        im = i - lbound(Vres,dim=1) + lbound(Mat,dim=1)
        Vres(i) = dot_product(Mat(im,:),Vec)
      END DO

    END FUNCTION AD_matmul_MatOFdnS_VecOFdnS

END MODULE ADdnSVM_dnS_Op_m
