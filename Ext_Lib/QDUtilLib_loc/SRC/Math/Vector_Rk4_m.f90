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
MODULE QDUtil_Vector_Rk4_m
  IMPLICIT NONE

  PUBLIC

  INTERFACE Sort_Vec
    MODULE PROCEDURE QDUtil_Sort_Rk4Vec
  END INTERFACE

  INTERFACE inferior_tab
    MODULE PROCEDURE QDUtil_inferior_tab_Rk4
  END INTERFACE

  INTERFACE compare_tab
    MODULE PROCEDURE QDUtil_compare_Rk4
  END INTERFACE

  PRIVATE :: QDUtil_Sort_Rk4Vec,QDUtil_inferior_tab_Rk4,QDUtil_compare_Rk4

CONTAINS
  !================================================================
  !   Sort a real vector Rvec (in the same vector)
  !   subroutine
  !================================================================
  SUBROUTINE QDUtil_Sort_Rk4Vec(RVec,sort_type)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rk4), intent(inout)              :: RVec(:)
    integer,          intent(in),    optional  :: sort_type

    integer            :: sort_type_loc
    integer, parameter :: sort_type_default = 1 ! ascending sort

    real(kind=Rk4)   :: a
    integer          :: i,j

    IF (present(sort_type)) THEN
      sort_type_loc = sort_type
    ELSE
      sort_type_loc = sort_type_default
    END IF

    SELECT CASE (sort_type_loc)
    CASE (1) ! ascending
      DO i=lbound(RVec,dim=1),ubound(RVec,dim=1)
      DO j=i+1,ubound(RVec,dim=1)
       IF (RVec(i) > RVec(j)) THEN
          ! permutation
          a=RVec(i)
          RVec(i)=RVec(j)
          RVec(j)=a
        END IF
      END DO
      END DO
    CASE (-1) ! descending
      DO i=lbound(RVec,dim=1),ubound(RVec,dim=1)
      DO j=i+1,ubound(RVec,dim=1)
       IF (RVec(i) < RVec(j)) THEN
          ! permutation
          a=RVec(i)
          RVec(i)=RVec(j)
          RVec(j)=a
        END IF
      END DO
      END DO
    END SELECT

  END SUBROUTINE QDUtil_Sort_Rk4Vec

  FUNCTION QDUtil_inferior_tab_Rk4(x1,x2) RESULT (inferior)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real (kind=Rk4), intent(in) :: x1(:),x2(:)

    logical :: inferior
    integer       :: i


    IF (size(x1) /= size(x2)) then
      write(out_unit,*) 'the size of the tab are different !!'
      write(out_unit,*) 'x1(:)',x1(:)
      write(out_unit,*) 'x2(:)',x2(:)
      write(out_unit,*) 'Check the fortran'
      STOP
    END IF
 
    inferior = .FALSE.
 
    DO i=1,size(x1)
      inferior = (x1(i) < x2(i))
      IF (x1(i) == x2(i)) CYCLE
      EXIT
    END DO
  
  END FUNCTION QDUtil_inferior_tab_Rk4

 !! @description: Compare two real (kind=Rk4) arrays L1 and L2 of equal size
 !! @param: L1 First logical array
 !! @param: L2 Second logical array
 FUNCTION QDUtil_compare_Rk4(L1, L2) RESULT (compare)
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  logical :: compare
  real(kind=Rk4), intent(in) :: L1(:), L2(:)

  integer :: i

  if (size(L1) /= size(L2)) then
    compare = .false.
    return
  end if

  compare = .true.
  do i=1, size(L1)
    compare = (abs(L1(i)-L2(i)) <= ONETENTH**13)
    IF (.NOT. compare) RETURN
  end do

  END FUNCTION QDUtil_compare_Rk4

END MODULE QDUtil_Vector_Rk4_m
