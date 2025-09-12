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
MODULE QDUtil_Vector_m
  USE QDUtil_Vector_Rk4_m
  USE QDUtil_Vector_Rk8_m
  USE QDUtil_Vector_Rk16_m
  IMPLICIT NONE

  PRIVATE

  INTERFACE compare_tab
  END INTERFACE

  INTERFACE inferior_tab
    MODULE PROCEDURE QDUtil_inferior_tab_Ik4, QDUtil_inferior_tab_Ik8
  END INTERFACE

  INTERFACE compare_tab
    MODULE PROCEDURE QDUtil_compare_Ik4, QDUtil_compare_Ik8
    MODULE PROCEDURE QDUtil_compare_L
  END INTERFACE

  !PUBLIC :: Test_QDUtil_Vector
  PUBLIC :: Sort_Vec,compare_tab,compare_tab

  CONTAINS

  FUNCTION QDUtil_inferior_tab_Ik4(x1,x2) RESULT (inferior)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE
    logical :: inferior
    integer       :: i
    integer (kind=Ik4), intent(in) :: x1(:),x2(:)
  
  
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
  
  END FUNCTION QDUtil_inferior_tab_Ik4
  FUNCTION QDUtil_inferior_tab_Ik8(x1,x2) RESULT (inferior)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE
    logical :: inferior
    integer       :: i
    integer (kind=Ik8), intent(in) :: x1(:),x2(:)
  
  
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
  
  END FUNCTION QDUtil_inferior_tab_Ik8

  !! @description: Compare two integer (kind=Ik4) arrays L1 and L2 of equal size
  !! @param: L1 First logical array
  !! @param: L2 Second logical array
  FUNCTION QDUtil_compare_Ik4(L1, L2) RESULT (compare)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE
  
    logical :: compare
    integer(kind=Ik4), intent(in) :: L1(:), L2(:)
  
    integer :: i
  
    if (size(L1) /= size(L2)) then
      compare = .false.
      return
    end if
  
    compare = .true.
    do i=1, size(L1)
      compare = (L1(i) == L2(i))
      IF (.NOT. compare) RETURN
    end do

  END FUNCTION QDUtil_compare_Ik4
  !! @description: Compare two integer (kind=Ik8) arrays L1 and L2 of equal size
  !! @param: L1 First logical array
  !! @param: L2 Second logical array
  FUNCTION QDUtil_compare_Ik8(L1, L2) RESULT (compare)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE
  
    logical :: compare
    integer(kind=Ik8), intent(in) :: L1(:), L2(:)
  
    integer :: i
  
    if (size(L1) /= size(L2)) then
      compare = .false.
      return
    end if
  
    compare = .true.
    do i=1, size(L1)
      compare = (L1(i) == L2(i))
      IF (.NOT. compare) RETURN
    end do

  END FUNCTION QDUtil_compare_Ik8
  !! @description: Compare two logical arrays L1 and L2 of equal size
  !! @param: L1 First logical array
  !! @param: L2 Second logical array
  FUNCTION QDUtil_compare_L(L1, L2) RESULT (compare)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    logical :: compare
    logical, intent(in) :: L1(:), L2(:)

    integer :: i
  
    if (size(L1) /= size(L2)) then
      compare = .false.
      return
    end if
  
    compare = .true.
    do i=1, size(L1)
      if (L1(i) .neqv. L2(i)) then
        compare = .false.
        return
      end if
    end do
  
  END FUNCTION QDUtil_compare_L
END MODULE QDUtil_Vector_m

SUBROUTINE Test_QDUtil_Vector()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_RW_MatVec_m
    USE QDUtil_Vector_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    integer                          :: io,ioerr
    real(kind=Rkind),    allocatable :: R1Vec(:),R2Vec(:)
    real (kind=Rkind),   parameter   :: ZeroTresh    = TEN**2*epsilon(ONE)

    !====================================================================
    ! Tests the sorting
    !
    ! define the matrices
    R1Vec = [ZERO,ZERO,ONE,TWO,THREE,FIVE,TEN] ! sorted vector
    R2Vec = [TWO,ZERO,FIVE,ZERO,TEN,ONE,THREE] ! unsorted vector

    ! tests
    CALL Initialize_Test(test_var,test_name='Vector')

    CALL Sort_Vec(R2Vec)
    res_test = all(abs(R1Vec-R2Vec) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='ascending sort')
    IF (.NOT. res_test) THEN
      CALL Write_Vec(R1Vec,out_unit, nbcol=7, info='R1Vec')
      CALL Write_Vec(R2Vec,out_unit, nbcol=7, info='R2Vec')
    END IF

    R1Vec = [TEN,FIVE,THREE,TWO,ONE,ZERO,ZERO] ! sorted vector
    R2Vec = [TWO,ZERO,FIVE,ZERO,TEN,ONE,THREE] ! unsorted vector

    CALL Sort_Vec(R2Vec,sort_type=-1)

    res_test = all(abs(R1Vec-R2Vec) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='descending sort')
    IF (.NOT. res_test) THEN
      CALL Write_Vec(R1Vec,out_unit, nbcol=7, info='R1Vec')
      CALL Write_Vec(R2Vec,out_unit, nbcol=7, info='R2Vec')
    END IF

    R1Vec = [TEN,FIVE,THREE,TWO,ONE,ZERO,ZERO] ! sorted vector
    R2Vec = [TWO,ZERO,FIVE,ZERO,TEN,ONE,THREE] ! unsorted vector

    res_test = compare_tab(R1Vec,R1Vec)
    CALL Logical_Test(test_var,test1=res_test,info='compare_tab real(Rkind):T')

    res_test = compare_tab(R1Vec,R2Vec)
    CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,info='compare_tab real(Rkind):F')
    CALL Flush_Test(test_var)
    !====================================================================

    ! finalize the tests
    CALL Finalize_Test(test_var)
END SUBROUTINE Test_QDUtil_Vector
