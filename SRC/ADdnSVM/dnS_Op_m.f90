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
  USE QDUtil_m
  USE ADdnSVM_dnS_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: dot_product,cross_product,product,sum,matmul,transpose


  INTERFACE dot_product
    MODULE PROCEDURE AD_dot_product_VecOFdnS,AD_dot_product_Vec_VecOFdnS,AD_dot_product_VecOFdnS_Vec
  END INTERFACE
  INTERFACE cross_product
    MODULE PROCEDURE AD_cross_product_VecOFdnS
  END INTERFACE
  INTERFACE product
    MODULE PROCEDURE AD_product_VecOFdnS
  END INTERFACE
  INTERFACE sum
    MODULE PROCEDURE AD_sum_VecOFdnS,AD_SUM_MatOFdnS
  END INTERFACE
  INTERFACE transpose
    MODULE PROCEDURE AD_transpose_MatOFdnS
  END INTERFACE
  INTERFACE matmul
    MODULE PROCEDURE AD_matmul_MatOFdnS_VecOFdnS,AD_matmul_MatOFdnS_Vec,AD_matmul_Mat_VecOFdnS
    MODULE PROCEDURE AD_matmul_MatOFdnS_MatOFdnS
  END INTERFACE


CONTAINS

    FUNCTION AD_dot_product_VecOFdnS(VecA,VecB) RESULT(Sres)
      USE QDUtil_m

      TYPE (dnS_t)                       :: Sres
      TYPE (dnS_t),        intent(in)    :: VecA(:),VecB(:)

      integer :: i,iv
      integer :: err_dnS_loc
      real(kind=Rkind) :: d0f,d1f,d2f,d3f
      character (len=*), parameter :: name_sub='AD_dot_product_VecOFdnS'


      IF (size(VecA) /= size(VecB)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of both vectors are incompatible'
         write(out_unit,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'ERROR in AD_dot_product_VecOFdnS: sizes are incomptible'
      END IF

      IF (size(VecA) < 1) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of both vectors are < 1'
         write(out_unit,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'ERROR in AD_dot_product_VecOFdnS: sizes are < 1'
      END IF

      Sres = ZERO
      DO i=lbound(VecA,dim=1),ubound(VecA,dim=1)
        iv   = i - lbound(VecA,dim=1) + lbound(VecB,dim=1)
        Sres = Sres + VecA(i) * VecB(iv)
      END DO

    END FUNCTION AD_dot_product_VecOFdnS
    FUNCTION AD_dot_product_VecOFdnS_Vec(VecA,VecB) RESULT(Sres)
      USE QDUtil_m

      TYPE (dnS_t)                       :: Sres
      TYPE (dnS_t),        intent(in)    :: VecA(:)
      real(kind=Rkind),    intent(in)    :: VecB(:)

      integer :: i,iv
      integer :: err_dnS_loc
      real(kind=Rkind) :: d0f,d1f,d2f,d3f
      character (len=*), parameter :: name_sub='AD_dot_product_VecOFdnS_Vec'


      IF (size(VecA) /= size(VecB)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of both vectors are incompatible'
         write(out_unit,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'ERROR in AD_dot_product_VecOFdnS_Vec: sizes are incomptible'
      END IF

      IF (size(VecA) < 1) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of both vectors are < 1'
         write(out_unit,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'ERROR in AD_dot_product_VecOFdnS_Vec: sizes are < 1'
      END IF


      Sres = ZERO
      DO i=lbound(VecA,dim=1),ubound(VecA,dim=1)
        iv   = i - lbound(VecA,dim=1) + lbound(VecB,dim=1)
        Sres = Sres + VecA(i) * VecB(iv)
      END DO

    END FUNCTION AD_dot_product_VecOFdnS_Vec
    FUNCTION AD_dot_product_Vec_VecOFdnS(VecA,VecB) RESULT(Sres)
      USE QDUtil_m

      TYPE (dnS_t)                       :: Sres
      TYPE (dnS_t),        intent(in)    :: VecB(:)
      real(kind=Rkind),    intent(in)    :: VecA(:)

      integer :: i,iv
      integer :: err_dnS_loc
      real(kind=Rkind) :: d0f,d1f,d2f,d3f
      character (len=*), parameter :: name_sub='AD_dot_product_Vec_VecOFdnS'


      IF (size(VecA) /= size(VecB)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of both vectors are incompatible'
         write(out_unit,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'ERROR in AD_dot_product_Vec_VecOFdnS: sizes are incomptible'
      END IF

      IF (size(VecA) < 1) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of both vectors are < 1'
         write(out_unit,*) '  size(VecA),size(VecB)',size(VecA),size(VecB)
         STOP 'ERROR in AD_dot_product_Vec_VecOFdnS: sizes are < 1'
      END IF

      Sres = ZERO
      DO i=lbound(VecA,dim=1),ubound(VecA,dim=1)
        iv   = i - lbound(VecA,dim=1) + lbound(VecB,dim=1)
        Sres = Sres + VecA(i) * VecB(iv)
      END DO

    END FUNCTION AD_dot_product_Vec_VecOFdnS


    FUNCTION AD_cross_product_VecOFdnS(V1,V2) RESULT(V3)
      IMPLICIT NONE
  
      TYPE (dnS_t),        intent(in)    :: V1(:),V2(:)
      TYPE (dnS_t), allocatable          :: V3(:)
  
      allocate(V3(3))
  
      v3(1) = v1(2)*v2(3) - v1(3)*v2(2)
      v3(2) =-v1(1)*v2(3) + v1(3)*v2(1)
      v3(3) = v1(1)*v2(2) - v1(2)*v2(1)
  
    END FUNCTION AD_cross_product_VecOFdnS

    FUNCTION AD_product_VecOFdnS(Vec) RESULT(Sres)
      USE QDUtil_m

      TYPE (dnS_t)                              :: Sres
      TYPE (dnS_t),        intent(in)           :: Vec(:)

      integer :: i
      character (len=*), parameter :: name_sub='AD_product_VecOFdnS'

      Sres = ONE
      DO i=lbound(Vec,dim=1),ubound(Vec,dim=1)
        Sres = Sres * Vec(i)
      END DO

    END FUNCTION AD_product_VecOFdnS
    FUNCTION AD_SUM_VecOFdnS(Vec) RESULT(Sres)
      USE QDUtil_m

      TYPE (dnS_t)                              :: Sres
      TYPE (dnS_t),        intent(in)           :: Vec(:)

      integer :: i
      character (len=*), parameter :: name_sub='AD_SUM_VecOFdnS'

      Sres = ZERO
      DO i=lbound(Vec,dim=1),ubound(Vec,dim=1)
        Sres = Sres + Vec(i)
      END DO

    END FUNCTION AD_SUM_VecOFdnS

    FUNCTION AD_SUM_MatOFdnS(Mat) RESULT(Sres)
      USE QDUtil_m

      TYPE (dnS_t)                              :: Sres
      TYPE (dnS_t),        intent(in)           :: Mat(:,:)

      integer :: i,j
      character (len=*), parameter :: name_sub='AD_SUM_MatOFdnS'

      Sres = ZERO
      DO i=lbound(Mat,dim=2),ubound(Mat,dim=2)
      DO j=lbound(Mat,dim=1),ubound(Mat,dim=1)
        Sres = Sres + Mat(j,i)
      END DO
      END DO


    END FUNCTION AD_SUM_MatOFdnS

    FUNCTION AD_transpose_MatOFdnS(Mat) RESULT(Mres)
      USE QDUtil_m

      TYPE (dnS_t),        intent(in)           :: Mat(:,:)

      TYPE (dnS_t)                              :: Mres(size(Mat,dim=2),size(Mat,dim=1))

      integer :: i,j,im,jm
      character (len=*), parameter :: name_sub='AD_transpose_MatOFdnS'


      DO i=lbound(Mres,dim=2),ubound(Mres,dim=2)
      DO j=lbound(Mres,dim=1),ubound(Mres,dim=1)
        jm = j - lbound(Mres,dim=1) + lbound(Mat,dim=1)
        im = i - lbound(Mres,dim=2) + lbound(Mat,dim=2)
        Mres(j,i) = Mat(im,jm)
      END DO
      END DO

    END FUNCTION AD_transpose_MatOFdnS

    FUNCTION AD_matmul_MatOFdnS_VecOFdnS(Mat,Vec) RESULT(Vres)
      USE QDUtil_m

      TYPE (dnS_t),        intent(in)           :: Vec(:)
      TYPE (dnS_t),        intent(in)           :: Mat(:,:)

      TYPE (dnS_t)                              :: Vres(size(Mat,dim=1))

      integer :: i,im
      character (len=*), parameter :: name_sub='AD_matmul_MatOFdnS_VecOFdnS'


      IF (size(Vec) /= size(Mat,dim=2)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of Vec(:) and Mat(.,:) are incomptible'
         write(out_unit,*) '  size(Vec),size(Mat,dim=2)',size(Vec),size(Mat,dim=2)
         STOP 'ERROR in AD_matmul_MatOFdnS_VecOFdnS: sizes are incomptible'
      END IF

      DO i=lbound(Vres,dim=1),ubound(Vres,dim=1)
        im = i - lbound(Vres,dim=1) + lbound(Mat,dim=1)
        Vres(i) = dot_product(Mat(im,:),Vec)
      END DO

    END FUNCTION AD_matmul_MatOFdnS_VecOFdnS

    FUNCTION AD_matmul_MatOFdnS_Vec(Mat,Vec) RESULT(Vres)
      USE QDUtil_m

      real(kind=Rkind),    intent(in)           :: Vec(:)
      TYPE (dnS_t),        intent(in)           :: Mat(:,:)

      TYPE (dnS_t)                              :: Vres(size(Mat,dim=1))

      integer :: i,im
      character (len=*), parameter :: name_sub='AD_matmul_MatOFdnS_Vec'


      IF (size(Vec) /= size(Mat,dim=2)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of Vec(:) and Mat(.,:) are incomptible'
         write(out_unit,*) '  size(Vec),size(Mat,dim=2)',size(Vec),size(Mat,dim=2)
         STOP 'ERROR in AD_matmul_MatOFdnS_Vec: sizes are incomptible'
      END IF

      DO i=lbound(Vres,dim=1),ubound(Vres,dim=1)
        im = i - lbound(Vres,dim=1) + lbound(Mat,dim=1)
        Vres(i) = dot_product(Mat(im,:),Vec)
      END DO

    END FUNCTION AD_matmul_MatOFdnS_Vec

    FUNCTION AD_matmul_Mat_VecOFdnS(Mat,Vec) RESULT(Vres)
      USE QDUtil_m

      TYPE (dnS_t),        intent(in)           :: Vec(:)
      real(kind=Rkind),    intent(in)           :: Mat(:,:)

      TYPE (dnS_t)                              :: Vres(size(Mat,dim=1))

      integer :: i,im
      character (len=*), parameter :: name_sub='AD_matmul_Mat_VecOFdnS'


      IF (size(Vec) /= size(Mat,dim=2)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of Vec(:) and Mat(.,:) are incomptible'
         write(out_unit,*) '  size(Vec),size(Mat,dim=2)',size(Vec),size(Mat,dim=2)
         STOP 'ERROR in AD_matmul_Mat_VecOFdnS: sizes are incomptible'

      END IF

      DO i=lbound(Vres,dim=1),ubound(Vres,dim=1)
        im = i - lbound(Vres,dim=1) + lbound(Mat,dim=1)
        Vres(i) = dot_product(Mat(im,:),Vec)
      END DO

    END FUNCTION AD_matmul_Mat_VecOFdnS


    FUNCTION AD_matmul_MatOFdnS_MatOFdnS(MatA,MatB) RESULT(Mres)
      USE QDUtil_m

      TYPE (dnS_t),        intent(in)           :: MatA(:,:)
      TYPE (dnS_t),        intent(in)           :: MatB(:,:)

      TYPE (dnS_t)                              :: Mres(size(MatA,dim=1),size(MatB,dim=2))

      integer :: i,j,im,jm
      character (len=*), parameter :: name_sub='AD_matmul_MatOFdnS_MatOFdnS'


      IF (size(MatB,dim=1) /= size(MatA,dim=2)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of MatA(.,:) and MatB(:,.) are incomptible'
         write(out_unit,*) '  size(MatA(.,:)),size(MatB(:,.))',size(MatA,dim=2),size(MatB,dim=1)
         STOP 'ERROR in AD_matmul_MatOFdnS_MatOFdnS: sizes are incomptible'

      END IF

      DO i=lbound(Mres,dim=2),ubound(Mres,dim=2)
      DO j=lbound(Mres,dim=1),ubound(Mres,dim=1)
        jm = j - lbound(Mres,dim=1) + lbound(MatA,dim=1)
        im = i - lbound(Mres,dim=2) + lbound(MatB,dim=2)
        Mres(j,i) = dot_product(MatA(jm,:),MatB(:,im))
      END DO
      END DO

    END FUNCTION AD_matmul_MatOFdnS_MatOFdnS

    FUNCTION AD_matmul_Mat_MatOFdnS(MatA,MatB) RESULT(Mres)
      USE QDUtil_m

      real(kind=Rkind),    intent(in)           :: MatA(:,:)
      TYPE (dnS_t),        intent(in)           :: MatB(:,:)

      TYPE (dnS_t)                              :: Mres(size(MatA,dim=1),size(MatB,dim=2))

      integer :: i,j,im,jm
      character (len=*), parameter :: name_sub='AD_matmul_Mat_MatOFdnS'


      IF (size(MatB,dim=1) /= size(MatA,dim=2)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of MatA(.,:) and MatB(:,.) are incomptible'
         write(out_unit,*) '  size(MatA(.,:)),size(MatB(:,.))',size(MatA,dim=2),size(MatB,dim=1)
         STOP 'ERROR in AD_matmul_Mat_MatOFdnS: sizes are incomptible'

      END IF

      DO i=lbound(Mres,dim=2),ubound(Mres,dim=2)
      DO j=lbound(Mres,dim=1),ubound(Mres,dim=1)
        jm = j - lbound(Mres,dim=1) + lbound(MatA,dim=1)
        im = i - lbound(Mres,dim=2) + lbound(MatB,dim=2)
        Mres(j,i) = dot_product(MatA(jm,:),MatB(:,im))
      END DO
      END DO

    END FUNCTION AD_matmul_Mat_MatOFdnS

    FUNCTION AD_matmul_MatOFdnS_Mat(MatA,MatB) RESULT(Mres)
      USE QDUtil_m

      TYPE (dnS_t),        intent(in)           :: MatA(:,:)
      real(kind=Rkind),    intent(in)           :: MatB(:,:)

      TYPE (dnS_t)                              :: Mres(size(MatA,dim=1),size(MatB,dim=2))

      integer :: i,j,im,jm
      character (len=*), parameter :: name_sub='AD_matmul_MatOFdnS_Mat'


      IF (size(MatB,dim=1) /= size(MatA,dim=2)) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) '  sizes of MatA(.,:) and MatB(:,.) are incomptible'
         write(out_unit,*) '  size(MatA(.,:)),size(MatB(:,.))',size(MatA,dim=2),size(MatB,dim=1)
         STOP 'ERROR in AD_matmul_MatOFdnS_Mat: sizes are incomptible'

      END IF

      DO i=lbound(Mres,dim=2),ubound(Mres,dim=2)
      DO j=lbound(Mres,dim=1),ubound(Mres,dim=1)
        jm = j - lbound(Mres,dim=1) + lbound(MatA,dim=1)
        im = i - lbound(Mres,dim=2) + lbound(MatB,dim=2)
        Mres(j,i) = dot_product(MatA(jm,:),MatB(:,im))
      END DO
      END DO

    END FUNCTION AD_matmul_MatOFdnS_Mat

END MODULE ADdnSVM_dnS_Op_m
