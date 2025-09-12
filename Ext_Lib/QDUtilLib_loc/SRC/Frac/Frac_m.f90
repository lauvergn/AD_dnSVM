!===============================================================================
!===============================================================================
!This file is part of QDUtil.
!
!===============================================================================
! MIT License
!
! Copyright (c) 2023 David Lauvergnat
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
 MODULE QDUtil_Frac_m
!! This module contains functions and subroutines to manipulate fractions.
!! All functions are **elemental** (except **TO_string(frac)**). Therefore, one can use those functions with table of fractions.
!!
!!- A type: **Frac_t**, which contains the numerator and denominator
!!  When the fraction is negative, the numerator has the minus sign.
!!  When the denominator is **0**, the fraction is simplified as 1/0.
!!- The operators (+, -, *, /, **, ==, >, <, <=, >=, /=) and the assignment (=) are orverloaded.
!!- When Frac_t is used for initialization and after numerical operations, the fraction is always reduced  (with the Greatest common divisor).
!!- One can test if a fraction is an integer with: frac_IS_integer(Frac)
!!- A fraction can be exported to:
!!  - to string: **TO_string(frac)**
!!  - to real (with the default kind=Rkind): **TO_real(frac)**
!!- The assignment can be done as follows:
!!  - Frac2 = Frac_t(3,-6) ! stored as -1/2
!!  - Frac2 = Frac1
!!  - Frac2 = Int1
!!  - Frac2 = '1/4' ! using a string of characters
!!  - where Frac1, Frac2 are fraction, Int1 is an integer
!!- Examples:
!!```Fortran
!!  TYPE(Frac_t) :: Frac1, Frac2
!!  TYPE(Frac_t), allocatable :: tab_Frac(:)
!!
!!  Frac1 = '1/-2' ! use the conversion from string to Frac_t
!!  write(*,*) 'Frac1: ',TO_String(Frac1) ! it give "Frac1: -1/2"
!!  Frac2 = -2*Frac1 ! here the result is one and it is simplified
!!  write(*,*) 'Frac2: ',TO_String(Frac2) ! it give "Frac2: 1"
!!  Frac2 = Frac1**3
!!  write(*,*) 'Frac2: ',TO_String(Frac2) ! it give "Frac2: -1/8"
!!  tab_Frac = Frac_t(1,[2,3,4])
!!  write(*,*) 'tab_Frac: ',(TO_String(tab_Frac(i)) // ' ',i=1,size(tab_Frac)) ! it give "tab_Frac: 1/2 1/3 1/4 "
!!```
  IMPLICIT NONE

  ! fraction num/den
  TYPE Frac_t
    integer  :: num      =  0
    integer  :: den      =  1
  CONTAINS
    PROCEDURE, PRIVATE, PASS(frac)   :: QDUtil_PLUS_frac
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_PLUS_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_PLUS_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_PLUS_frac2
    GENERIC,   PUBLIC  :: operator(+) => QDUtil_PLUS_frac,QDUtil_frac1_PLUS_frac2,     &
                                         QDUtil_frac1_PLUS_Int2,QDUtil_Int1_PLUS_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), computes \(g_1+g_2\). 
    !! The operator (+) is overloaded.
    !! 
    !! @note
    !!    \(g_1\) and \(g_2\) are both integers. The operation is done through and integer operation.
    !!    Then, the result can be assigned to a fraction through the integer assignment.
    !!    Given \(f_1\) (a fraction), \(+f_1\) is just an assignment.           
    !! @endnote                                      

    PROCEDURE, PRIVATE, PASS(frac)   :: QDUtil_MINUS_frac
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_MINUS_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_MINUS_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_MINUS_frac2
    GENERIC,   PUBLIC  :: operator(-) => QDUtil_MINUS_frac,QDUtil_frac1_MINUS_frac2,   &
                                        QDUtil_frac1_MINUS_Int2,QDUtil_Int1_MINUS_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), computes \(g_1-g_2\). 
    !!The operator (-) is overloaded.
    !!                                    
    !! @note
    !!    \(g_1\) and \(g_2\) are both integers. The operation is done through and integer operation.
    !!    Then, the result can be assigned to a fraction through the integer assignment.
    !!    Given \(f_1\) (a fraction), \(-f_1\) is assigned to a fraction.             
    !! @endnote

    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_TIME_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_TIME_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_TIME_frac2
    GENERIC,   PUBLIC  :: operator(*) => QDUtil_frac1_TIME_frac2,QDUtil_frac1_TIME_Int2, &
                                         QDUtil_Int1_TIME_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), computes \(g_1*g_2\).
    !!The operator (*) is overloaded.
    !!                                     
    !! @note
    !!    \(g_1\) and \(g_2\) are both integers. The multiplication is done through and integer multiplication.
    !!    Then, the result can be assigned to a fraction through the integer assignment.                          
    !! @endnote

    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_DIVIDEBY_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_DIVIDEBY_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_DIVIDEBY_frac2
    GENERIC,   PUBLIC  :: operator(/) => QDUtil_frac1_DIVIDEBY_frac2,QDUtil_frac1_DIVIDEBY_Int2, &
                                         QDUtil_Int1_DIVIDEBY_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), computes \(\frac{g_1}{g_2}\). 
    !! The operator (/) is overloaded.
    !! @note
    !!    \(g_1\) and \(g_2\) cannot be both integers (integer division).
    !!    Instead, the function (constructor) *Fract_t* shall be used.                          
    !! @endnote                   

    PROCEDURE, PRIVATE :: QDUtil_frac1_EXP_Int2
    GENERIC,   PUBLIC  :: operator (**) => QDUtil_frac1_EXP_Int2
    !! Given a fraction \(f\) and an integer \(i\), computes \(f^i\). The operator (**) is overloaded.

    PROCEDURE, PRIVATE, PASS(frac)   :: QDUtil_Int_TO_frac
    PROCEDURE, PRIVATE, PASS(frac)   :: QDUtil_string_TO_frac
    GENERIC,   PUBLIC  :: assignment(=) => QDUtil_Int_TO_frac,QDUtil_string_TO_frac
    !! These subroutines initialize a fraction from an integer or a string. The assignement (=) is overloaded.

    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_EQ_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_EQ_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_EQ_frac2
    GENERIC,   PUBLIC  :: operator(==) => QDUtil_frac1_EQ_frac2,QDUtil_frac1_EQ_Int2,QDUtil_Int1_EQ_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), checks: \(g_1 == g_2\). 
    !! The result (return value) is a logical.
    !! The operator (==) is overloaded.
    !!
    !! @note
    !!    If \(g_1\) and \(g_2\) are both integers, the test is performed on the integers.                          
    !! @endnote  

    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_NEQ_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_NEQ_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_NEQ_frac2
    GENERIC,   PUBLIC  :: operator(/=) => QDUtil_frac1_NEQ_frac2,QDUtil_frac1_NEQ_Int2,QDUtil_Int1_NEQ_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), checks: \(g_1 /= g_2\). 
    !! The result (return value) is a logical.
    !! The operator (/=) is overloaded.
    !!
    !! @note
    !!    If \(g_1\) and \(g_2\)are both integers, the test is performed on the integers.                          
    !! @endnote
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_GT_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_GT_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_GT_frac2
    GENERIC,   PUBLIC  :: operator(>) => QDUtil_frac1_GT_frac2,QDUtil_frac1_GT_Int2,QDUtil_Int1_GT_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), checks: \(g_1 > g_2\). 
    !! The result (return value) is a logical.
    !! The operator (>) is overloaded.
    !!
    !! @note
    !!    If \(g_1\) and \(g_2\)are both integers, the test is performed on the integers.                          
    !! @endnote
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_LT_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_LT_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_LT_frac2
    GENERIC,   PUBLIC  :: operator(<) => QDUtil_frac1_LT_frac2,QDUtil_frac1_LT_Int2,QDUtil_Int1_LT_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), checks: \(g_1 < g_2\). 
    !! The result (return value) is a logical.
    !! The operator (<) is overloaded.
    !!
    !! @note
    !!    If \(g_1\) and \(g_2\)are both integers, the test is performed on the integers.                          
    !! @endnote
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_GE_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_GE_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_GE_frac2
    GENERIC,   PUBLIC  :: operator(>=) => QDUtil_frac1_GE_frac2,QDUtil_frac1_GE_Int2,QDUtil_Int1_GE_frac2
    !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), checks: \(g_1 >= g_2\). 
    !! The result (return value) is a logical.
    !! The operator (>=) is overloaded.
    !!
    !! @note
    !!    If \(g_1\) and \(g_2\)are both integers, the test is performed on the integers.                          
    !! @endnote
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_LE_frac2
    PROCEDURE, PRIVATE, PASS(frac1)  :: QDUtil_frac1_LE_Int2
    PROCEDURE, PRIVATE, PASS(frac2)  :: QDUtil_Int1_LE_frac2
    GENERIC,   PUBLIC  :: operator(<=) => QDUtil_frac1_LE_frac2,QDUtil_frac1_LE_Int2,QDUtil_Int1_LE_frac2
     !! Given \(g_1\) (a fraction or an integer) and \(g_2\) (a fraction or an integer), checks: \(g_1 <= g_2\). 
    !! The result (return value) is a logical.
    !! The operator (<=) is overloaded.
    !!
    !! @note
    !!    If \(g_1\) and \(g_2\)are both integers, the test is performed on the integers.                          
    !! @endnote
  END TYPE Frac_t

  INTERFACE Frac_t
    MODULE PROCEDURE QDUtil_construct_Frac
  END INTERFACE Frac_t
  INTERFACE TO_string
    MODULE PROCEDURE QDUtil_TO_string
  END INTERFACE TO_string
  INTERFACE TO_real
    MODULE PROCEDURE QDUtil_TO_real
  END INTERFACE TO_real
  INTERFACE IS_integer
    MODULE PROCEDURE QDUtil_frac_IS_integer
  END INTERFACE IS_integer
  INTERFACE simplification
    MODULE PROCEDURE QDUtil_frac_simplification
  END INTERFACE simplification

  PRIVATE
  PUBLIC :: Frac_t,IS_integer
  PUBLIC :: TO_string,TO_real
  PUBLIC :: simplification

CONTAINS

  !> This function tests if a fraction is an integer.
  !> Given a fraction ([frac]), return true if it is an integer othervise false
  ELEMENTAL FUNCTION QDUtil_frac_IS_integer(frac) RESULT(test)
   TYPE(Frac_t), intent(in) :: frac 
   logical                  :: test 

   test = (frac%den == 1) .OR. (frac%num == 0)

  END FUNCTION QDUtil_frac_IS_integer

  !> This subroutine initializes a fraction from an integer.
  !> Given an integer ([i]), returns a fraction [frac]
  !>
  !> @note
  !>   This subroutine overloads the assignement (=)
  !> @endnote
  ELEMENTAL SUBROUTINE QDUtil_Int_TO_frac(frac,i)
   CLASS(Frac_t), intent(inout) :: frac
   integer,       intent(in)    :: i

   frac%num = i
   frac%den = 1

  END SUBROUTINE QDUtil_Int_TO_frac

  !> This function is the constructor of the derive type *Frac_t*.
  !> Given two integers, [num] and [den], returns a fraction [frac] (result).
  !>
  !> @note
  !>   The usual constructor is not used, so that a simplication can be performed.
  !> @endnote
  ELEMENTAL FUNCTION QDUtil_construct_Frac(num,den) RESULT(frac)
    TYPE(Frac_t)                     :: frac
    integer,           intent(in)    :: num,den

    frac%num = num
    frac%den = den

    CALL QDUtil_frac_simplification(frac%num,frac%den)

  END FUNCTION QDUtil_construct_Frac

  !> This subroutine convert a string of characters into a fraction.
  !> Given a string [String], constructs a fraction [frac].
  SUBROUTINE QDUtil_string_TO_frac(frac,String)
   USE QDUtil_NumParameters_m, ONLY : out_unit
    character (len=*),  intent(in)    :: String
    CLASS(Frac_t),      intent(inout) :: frac
 
    integer :: islash
 
    IF (len_trim(String) == 0) THEN
      frac = 0
    ELSE
      islash = index(String,'/')
      IF (islash == 0 .OR. islash == 1 .OR. islash == len(String)) THEN
        write(out_unit,*) ' ERROR in QDUtil_string_TO_frac'
        write(out_unit,*) ' The string does not contain a "/" or its postion is wrong'
        write(out_unit,*) ' String: ',String
        STOP ' ERROR in QDUtil_string_TO_frac'
      END IF
      read(String(1:islash-1),*) frac%num
      read(String(islash+1:len(String)),*) frac%den
    END IF
 
    CALL QDUtil_frac_simplification(frac%num,frac%den)
 
  END SUBROUTINE QDUtil_string_TO_frac

  !> This function convert a fraction into a string of characters.
  !> Given a fraction [frac], returns a string (result).
  FUNCTION QDUtil_TO_string(frac) RESULT(String)
    USE QDUtil_String_m, only : TO_String
    character (len=:), allocatable  :: String
    TYPE(Frac_t), intent(in)   :: frac
 
    IF (frac%den == 1) THEN
      String = TO_String(frac%num)
    ELSE
      String = ( TO_String(frac%num) // '/' // TO_String(frac%den) )
    END IF
 
  END FUNCTION QDUtil_TO_string

  !> This function convert a fraction into a real.
  !> Given a fraction [frac], returns a real (result).
  ELEMENTAL FUNCTION QDUtil_TO_real(frac) RESULT(R)
  USE QDUtil_NumParameters_m, only : Rkind
    real(kind=Rkind)              :: R
    TYPE(Frac_t), intent(in) :: frac
 
    R = real(frac%num,kind=Rkind) / real(frac%den,kind=Rkind)
 
  END FUNCTION QDUtil_TO_real
  
  ! No documentation for the following functions/subroutines (except the test one)
  ELEMENTAL FUNCTION QDUtil_frac1_EQ_frac2(frac1,frac2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-frac2
    leq  = (frac%num == 0)
 
  END FUNCTION QDUtil_frac1_EQ_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_EQ_Int2(frac1,Int2) RESULT(leq)
    logical                   :: leq
    CLASS(Frac_t), intent(in) :: frac1
    Integer,       intent(in) :: Int2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-Frac_t(Int2,1)
    leq  = (frac%num == 0)
 
  END FUNCTION QDUtil_frac1_EQ_Int2
  ELEMENTAL FUNCTION QDUtil_Int1_EQ_frac2(Int1,frac2) RESULT(leq)
    logical                        :: leq
    Integer,            intent(in) :: Int1
    CLASS(Frac_t), intent(in) :: frac2
 
    TYPE(Frac_t) :: frac
 
    frac = Frac_t(Int1,1) - frac2
    leq  = (frac%num == 0)
 
  END FUNCTION QDUtil_Int1_EQ_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_NEQ_frac2(frac1,frac2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-frac2
    leq  = (frac%num /= 0)
 
  END FUNCTION QDUtil_frac1_NEQ_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_NEQ_Int2(frac1,Int2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1
    Integer,           intent(in) :: Int2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-Frac_t(Int2,1)
    leq  = (frac%num /= 0)
 
  END FUNCTION QDUtil_frac1_NEQ_Int2
  ELEMENTAL FUNCTION QDUtil_Int1_NEQ_frac2(Int1,frac2) RESULT(leq)
    logical                        :: leq
    Integer,            intent(in) :: Int1
    CLASS(Frac_t), intent(in) :: frac2
 
    TYPE(Frac_t) :: frac
 
    frac = Frac_t(Int1,1) - frac2
    leq  = (frac%num /= 0)
 
  END FUNCTION QDUtil_Int1_NEQ_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_GT_frac2(frac1,frac2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-frac2
    leq  = (frac%num > 0)
 
  END FUNCTION QDUtil_frac1_GT_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_GT_Int2(frac1,Int2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1
    Integer,           intent(in) :: Int2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-Frac_t(Int2,1)
    leq  = (frac%num > 0)
 
  END FUNCTION QDUtil_frac1_GT_Int2
  ELEMENTAL FUNCTION QDUtil_Int1_GT_frac2(Int1,frac2) RESULT(leq)
    logical                        :: leq
    Integer,            intent(in) :: Int1
    CLASS(Frac_t), intent(in) :: frac2
 
    TYPE(Frac_t) :: frac
 
    frac = Frac_t(Int1,1) - frac2
    leq  = (frac%num > 0)
 
  END FUNCTION QDUtil_Int1_GT_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_LT_frac2(frac1,frac2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-frac2
    leq  = (frac%num < 0)
 
  END FUNCTION QDUtil_frac1_LT_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_LT_Int2(frac1,Int2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1
    Integer,           intent(in) :: Int2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-Frac_t(Int2,1)
    leq  = (frac%num < 0)
 
  END FUNCTION QDUtil_frac1_LT_Int2
  ELEMENTAL FUNCTION QDUtil_Int1_LT_frac2(Int1,frac2) RESULT(leq)
    logical                        :: leq
    Integer,            intent(in) :: Int1
    CLASS(Frac_t), intent(in) :: frac2
 
    TYPE(Frac_t) :: frac
 
    frac = Frac_t(Int1,1) - frac2
    leq  = (frac%num < 0)
 
  END FUNCTION QDUtil_Int1_LT_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_GE_frac2(frac1,frac2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-frac2
    leq  = (frac%num >= 0)
 
  END FUNCTION QDUtil_frac1_GE_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_GE_Int2(frac1,Int2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1
    Integer,           intent(in) :: Int2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-Frac_t(Int2,1)
    leq  = (frac%num >= 0)
 
  END FUNCTION QDUtil_frac1_GE_Int2
  ELEMENTAL FUNCTION QDUtil_Int1_GE_frac2(Int1,frac2) RESULT(leq)
    logical                        :: leq
    Integer,            intent(in) :: Int1
    CLASS(Frac_t), intent(in) :: frac2
 
    TYPE(Frac_t) :: frac
 
    frac = Frac_t(Int1,1) - frac2
    leq  = (frac%num >= 0)
 
  END FUNCTION QDUtil_Int1_GE_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_LE_frac2(frac1,frac2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-frac2
    leq  = (frac%num <= 0)
 
  END FUNCTION QDUtil_frac1_LE_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_LE_Int2(frac1,Int2) RESULT(leq)
    logical                       :: leq
    CLASS(Frac_t), intent(in) :: frac1
    Integer,           intent(in) :: Int2
 
    TYPE(Frac_t) :: frac
 
    frac = frac1-Frac_t(Int2,1)
    leq  = (frac%num <= 0)
 
  END FUNCTION QDUtil_frac1_LE_Int2
  ELEMENTAL FUNCTION QDUtil_Int1_LE_frac2(Int1,frac2) RESULT(leq)
    logical                        :: leq
    Integer,            intent(in) :: Int1
    CLASS(Frac_t), intent(in) :: frac2
 
    TYPE(Frac_t) :: frac
 
    frac = Frac_t(Int1,1) - frac2
    leq  = (frac%num <= 0)
 
  END FUNCTION QDUtil_Int1_LE_frac2
  ELEMENTAL FUNCTION QDUtil_PLUS_frac(frac) RESULT(Resfrac)
    TYPE(Frac_t)              :: Resfrac
    CLASS(Frac_t), intent(in) :: frac
 
    ResFrac = frac
 
  END FUNCTION QDUtil_PLUS_frac
  ELEMENTAL FUNCTION QDUtil_frac1_PLUS_frac2(frac1,frac2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    Frac%num = frac1%num * frac2%den + frac1%den * frac2%num
    Frac%den = frac1%den * frac2%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_PLUS_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_PLUS_Int2(frac1,Int2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac1
    Integer,           intent(in) :: Int2
 
    Frac%num = frac1%num + frac1%den * Int2
    Frac%den = frac1%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_PLUS_Int2
  ELEMENTAL FUNCTION QDUtil_Int1_PLUS_frac2(Int1,frac2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac2
    Integer,           intent(in) :: Int1
 
    Frac%num = frac2%num + frac2%den * Int1
    Frac%den = frac2%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_Int1_PLUS_frac2
  ELEMENTAL FUNCTION QDUtil_MINUS_frac(frac) RESULT(Resfrac)
    TYPE(Frac_t)              :: Resfrac
    CLASS(Frac_t), intent(in) :: frac
 
    ResFrac%num = -frac%num
    ResFrac%den = frac%den
 
  END FUNCTION QDUtil_MINUS_frac
  ELEMENTAL FUNCTION QDUtil_frac1_MINUS_frac2(frac1,frac2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    Frac%num = frac1%num * frac2%den - frac1%den * frac2%num
    Frac%den = frac1%den * frac2%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_MINUS_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_MINUS_Int2(frac1,Int2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac1
    Integer,           intent(in) :: Int2
 
    Frac%num = frac1%num - frac1%den * Int2
    Frac%den = frac1%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_MINUS_Int2
  ELEMENTAL FUNCTION QDUtil_Int1_MINUS_frac2(Int1,frac2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac2
    Integer,           intent(in) :: Int1
 
    Frac%num = frac2%den * Int1 - frac2%num
    Frac%den = frac2%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_Int1_MINUS_frac2
 
  ELEMENTAL FUNCTION QDUtil_frac1_TIME_frac2(frac1,frac2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    Frac%num = frac1%num * frac2%num
    Frac%den = frac1%den * frac2%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_TIME_frac2
  ELEMENTAL FUNCTION QDUtil_Int1_TIME_frac2(Int1,frac2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac2
    Integer,           intent(in) :: Int1
 
    Frac%num = Int1 * frac2%num
    Frac%den = frac2%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_Int1_TIME_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_TIME_Int2(frac1,Int2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac1
    Integer,           intent(in) :: Int2
 
    Frac%num = frac1%num * Int2
    Frac%den = frac1%den
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_TIME_Int2
  ELEMENTAL FUNCTION QDUtil_frac1_DIVIDEBY_frac2(frac1,frac2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac1,frac2
 
    Frac%num = frac1%num * frac2%den
    Frac%den = frac1%den * frac2%num
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_DIVIDEBY_frac2
  ELEMENTAL FUNCTION QDUtil_Int1_DIVIDEBY_frac2(Int1,frac2) RESULT(frac)
    TYPE(Frac_t)             :: frac
    CLASS(Frac_t), intent(in) :: frac2
    Integer,           intent(in) :: Int1
 
    Frac%num = Int1 * frac2%den
    Frac%den = frac2%num
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_Int1_DIVIDEBY_frac2
  ELEMENTAL FUNCTION QDUtil_frac1_DIVIDEBY_Int2(frac1,Int2) RESULT(frac)
    TYPE(Frac_t)              :: frac
    CLASS(Frac_t), intent(in) :: frac1
    Integer,       intent(in) :: Int2
 
    Frac%num = frac1%num
    Frac%den = frac1%den * Int2
 
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_DIVIDEBY_Int2
  ELEMENTAL FUNCTION QDUtil_frac1_EXP_Int2(frac1,Int2) RESULT(frac)
    TYPE(Frac_t)              :: frac
    CLASS(Frac_t), intent(in) :: frac1
    Integer,       intent(in) :: Int2
  
    IF (Int2 == 0) THEN
      frac = 1
    ELSE IF (Int2 > 0) THEN
      frac = Frac_t(frac1%num**Int2,frac1%den**Int2)
    ELSE ! < 0
      frac = Frac_t(frac1%den**(-Int2),frac1%num**(-Int2))
    END IF
  
    CALL QDUtil_frac_simplification(Frac%num, Frac%den)
 
  END FUNCTION QDUtil_frac1_EXP_Int2

  !>  This function computes the greatest common divisor.
  !> Given to integers [a] and [b], returns the greatest common divisor [gcd] (integer)
  ELEMENTAL FUNCTION QDUtil_gcd(a, b) RESULT(gcd)
    integer              :: gcd
    integer, intent (in) :: a,b

    integer :: aa,bb,t

    aa = a
    bb = b
    DO
      IF (bb == 0) EXIT
      t = bb
      bb = mod(aa,bb)
      aa = t
    END DO
    gcd = aa

  END FUNCTION QDUtil_gcd

  !> This subroutine preforms a simplication on two integer using the greatest common divisor.
  !> Given two integers [a] and [b], divides them by the the greatest common divisor.
  !> Changes the sign of [a] and [b] if [b] is negative.
  ELEMENTAL SUBROUTINE QDUtil_frac_simplification(a, b)
    integer, intent (inout) :: a,b

    integer :: aa,bb,t

    IF (b == 1 .OR. b == 0) RETURN
    aa = a
    bb = b
    t = QDUtil_gcd(aa,bb)

    a = aa/t
    b = bb/t

    IF (b < 0) THEN
      b = -b
      a = -a
    END IF

  END SUBROUTINE QDUtil_frac_simplification
END MODULE QDUtil_Frac_m

  !> This subroutine performs tests of the module.
SUBROUTINE Test_QDUtil_Frac
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m, only : Rkind, TEN, ONE, out_unit
    USE QDUtil_Frac_m
    IMPLICIT NONE

    TYPE(Frac_t)              :: frac1,frac2,frac3
    TYPE(Frac_t), allocatable :: tab_frac1(:)
    TYPE(Frac_t), allocatable :: tab_frac2(:)
    integer                   :: i
    character(len=:), allocatable :: tab_string(:)

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind),   parameter   :: ZeroTresh    = TEN**2*epsilon(ONE)

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_Frac'
    !logical, parameter :: debug = .FALSE.
    logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='Frac')
    CALL Flush_Test(test_var)

    frac3 = ' 1 / 3 '
    res_test = ('1/3' == TO_string(frac3))
    CALL Logical_Test(test_var,test1=res_test,info='frac3="1/3" 1/3')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac3="1/3"   1/3 ?: ',TO_string(frac3)
    END IF
    frac3 = '-1/3'
    res_test = ('-1/3' == TO_string(frac3))
    CALL Logical_Test(test_var,test1=res_test,info='frac3="-1/3" -1/3')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac3="-1/3" -1/3 ?: ',TO_string(frac3)
    END IF
    frac3 = '1/-3'
    res_test = ('-1/3' == TO_string(frac3))
    CALL Logical_Test(test_var,test1=res_test,info='frac3="1/-3" -1/3')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac3="1/-3" -1/3 ?: ',TO_string(frac3)
    END IF
    frac3 = '-1/-3'
    res_test = ('1/3' == TO_string(frac3))
    CALL Logical_Test(test_var,test1=res_test,info='frac3="-1/-3" 1/3')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac3="-1/-3" 1/3 ?: ',TO_string(frac3)
    END IF
    CALL Flush_Test(test_var)

    write(out_unit,*)

    frac1 = Frac_t(1,2)
    frac2 = 2
    res_test = ('1/2' == TO_string(frac1))
    CALL Logical_Test(test_var,test1=res_test,info='frac1=        1/2 ?')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1=        1/2 ?: ',TO_string(frac1)
    END IF
    res_test = ('2' == TO_string(frac2))
    CALL Logical_Test(test_var,test1=res_test,info='frac2=        2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac2=        2   ?: ',TO_string(frac2)
    END IF
    res_test = abs(0.5_Rkind - TO_real(frac1)) < ZeroTresh
    CALL Logical_Test(test_var,test1=res_test,info='frac1=        0.5')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,'(1x,a,f4.1)') 'frac1=        0.5 ?: ',TO_real(frac1)
    END IF
    CALL Flush_Test(test_var)

    write(out_unit,*)

    res_test = ('1/2' == TO_string(+frac1))
    CALL Logical_Test(test_var,test1=res_test,info='+frac1=       1/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '+frac1=       1/2 ?: ',TO_string(+frac1)
    END IF
    res_test = ('5/2' == TO_string(frac1+frac2))
    CALL Logical_Test(test_var,test1=res_test,info='frac1+frac2=  5/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1+frac2=  5/2 ?: ',TO_string(frac1+frac2)
    END IF
    res_test = ('5/2' == TO_string(frac1+2))
    CALL Logical_Test(test_var,test1=res_test,info='frac1+2=      5/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1+2=      5/2 ?: ',TO_string(frac1+2)
    END IF
    write(out_unit,*)
    res_test = ('5/2' == TO_string(2+frac1))
    CALL Logical_Test(test_var,test1=res_test,info='2+frac1=      5/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '2+frac1=      5/2 ?: ',TO_string(2+frac1)
    END IF
    CALL Flush_Test(test_var)

    write(out_unit,*)

    res_test = ('-1/2' == TO_string(-frac1))
    CALL Logical_Test(test_var,test1=res_test,info='-frac1=      -1/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '-frac1=      -1/2 ?: ',TO_string(-frac1)
    END IF
    res_test = ('-3/2' == TO_string(frac1-frac2))
    CALL Logical_Test(test_var,test1=res_test,info='frac1-frac2= -3/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1-frac2= -3/2 ?: ',TO_string(frac1-frac2)
    END IF
    CALL Flush_Test(test_var)
    res_test = ('-3/2' == TO_string(frac1-2))
    CALL Logical_Test(test_var,test1=res_test,info='frac1-2=     -3/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1-2=     -3/2 ?: ',TO_string(frac1-2)
    END IF
    res_test = ('3/2' == TO_string(2-frac1))
    CALL Logical_Test(test_var,test1=res_test,info='2-frac1=      3/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '2-frac1=      3/2 ?: ',TO_string(2-frac1)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    res_test = ('1' == TO_string(frac1*frac2))
    CALL Logical_Test(test_var,test1=res_test,info='frac1*frac2=  1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1*frac2=  1   ?: ',TO_string(frac1*frac2)
    END IF
    res_test = ('1' == TO_string(frac1*2))
    CALL Logical_Test(test_var,test1=res_test,info='frac1*2=  1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1*2=      1   ?: ',TO_string(frac1*2)
    END IF
    res_test = ('1' == TO_string(2*frac1))
    CALL Logical_Test(test_var,test1=res_test,info='2*frac1=      1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '2*frac1=      1   ?: ',TO_string(2*frac1)
    END IF
    CALL Flush_Test(test_var)

    write(out_unit,*)

    res_test = ('1/4' == TO_string(frac1/frac2))
    CALL Logical_Test(test_var,test1=res_test,info='frac1/frac2=  1/4')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1/frac2=  1/4 ?: ',TO_string(frac1/frac2)
    END IF
    res_test = ('1/4' == TO_string(frac1/2))
    CALL Logical_Test(test_var,test1=res_test,info='2/frac1=      4 ')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1/2=      1/4 ?: ',TO_string(frac1/2)
    END IF
    res_test = ('4' == TO_string(2/frac1))
    CALL Logical_Test(test_var,test1=res_test,info='2/frac1=      4')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '2/frac1=      4   ?: ',TO_string(2/frac1)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    ! tests on **
    res_test = (frac1**0 == 1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1**0 == 1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1**0 == 1   ?: ',TO_string(frac1**0)
    END IF
    res_test = (frac1**1 == frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1**1 == frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1**1 == frac1   ?: ',TO_string(frac1**1),TO_string(frac1)
    END IF
    res_test = (frac1**(-1) == 1/frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1**(-1) == 1/frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1**(-1) == 1/frac1   ?: ',TO_string(frac1**(-1)),TO_string(1/frac1)
    END IF

    res_test = (frac1**2 == frac1*frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1**2 == frac1*frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1**2 == frac1*frac1   ?: ',TO_string(frac1**2),TO_string(frac1*frac1)
    END IF
    res_test = (frac1**(-2) == 1/frac1**2)
    CALL Logical_Test(test_var,test1=res_test,info='frac1**(-2) == 1/frac1**2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1**(-2) == 1/frac1**2   ?: ',TO_string(frac1**(-2)),TO_string(1/frac1**2)
    END IF
    res_test = (frac1**5 == (frac1**2)**2 * frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1**5 == (frac1**2)**2 * frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1**5 == (frac1**2)**2 * frac1   ?: ',TO_string(frac1**5),TO_string((frac1**2)**2 * frac1)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    ! tests on ==
    res_test = (frac1 == frac2)
    CALL Logical_Test(test_var,test1=res_test,info='frac1==frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1==frac2    F ?: ',(frac1 == frac2)
    END IF
    res_test = (1 == frac2)
    CALL Logical_Test(test_var,test1=res_test,info='1==frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    1==frac2    F ?: ',(1 == frac2)
    END IF
    res_test = (frac1 == 1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1==1',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1==1        F ?: ',(frac1 == 1)
    END IF
    res_test = (frac1 == frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1==frac1',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1==frac1    T ?: ',(frac1 == frac1)
    END IF
    res_test = (frac2 == 2)
    CALL Logical_Test(test_var,test1=res_test,info='frac2==2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac2==2        T ?: ',(frac2 == 2)
    END IF
    res_test = (2 == frac2)
    CALL Logical_Test(test_var,test1=res_test,info='    2==frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    2==frac2    T ?: ',(2 == frac2)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    ! tests on /=
    res_test = (frac1 /= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='frac1/=frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1==frac2    F ?: ',(frac1 /= frac2)
    END IF
    res_test = (1 /= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='1/=frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    1/=frac2    F ?: ',(1 /= frac2)
    END IF
    res_test = (frac1 /= 1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1/=1',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1/=1        F ?: ',(frac1 /= 1)
    END IF
    res_test = (frac1 /= frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1/=frac1',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1/=frac1    T ?: ',(frac1 /= frac1)
    END IF
    res_test = (frac2 /= 2)
    CALL Logical_Test(test_var,test1=res_test,info='frac2/=2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac2/=2        T ?: ',(frac2 /= 2)
    END IF
    res_test = (2 /= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='    2/=frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    2/=frac2    T ?: ',(2 /= frac2)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    ! tests on <
    res_test = (frac1 < frac2)
    CALL Logical_Test(test_var,test1=res_test,info='frac1<frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1<frac2    F ?: ',(frac1 < frac2)
    END IF
    res_test = (1 < frac2)
    CALL Logical_Test(test_var,test1=res_test,info='1<frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    1<frac2    F ?: ',(1 < frac2)
    END IF
    res_test = (frac1 < 1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1<1',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1<1        F ?: ',(frac1 < 1)
    END IF
    res_test = (frac1 < frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1<frac1',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1<frac1    T ?: ',(frac1 < frac1)
    END IF
    res_test = (frac2 < 2)
    CALL Logical_Test(test_var,test1=res_test,info='frac2<2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac2<2        T ?: ',(frac2 < 2)
    END IF
    res_test = (2 < frac2)
    CALL Logical_Test(test_var,test1=res_test,info='    2<frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    2<frac2    T ?: ',(2 < frac2)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    ! tests on >
    res_test = (frac1 > frac2)
    CALL Logical_Test(test_var,test1=res_test,info='frac1>frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1>frac2    F ?: ',(frac1 > frac2)
    END IF
    res_test = (1 > frac2)
    CALL Logical_Test(test_var,test1=res_test,info='1>frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    1>frac2    F ?: ',(1 > frac2)
    END IF
    res_test = (frac1 > 1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1>1',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1>1        F ?: ',(frac1 > 1)
    END IF
    res_test = (frac1 > frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1>frac1',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1>frac1    T ?: ',(frac1 > frac1)
    END IF
    res_test = (frac2 > 2)
    CALL Logical_Test(test_var,test1=res_test,info='frac2>2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac2>2        T ?: ',(frac2 > 2)
    END IF
    res_test = (2 > frac2)
    CALL Logical_Test(test_var,test1=res_test,info='    2>frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    2>frac2    T ?: ',(2 > frac2)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    ! tests on >=
    res_test = (frac1 >= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='frac1>=frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1>=frac2    F ?: ',(frac1 >= frac2)
    END IF
    res_test = (1 >= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='1>=frac2',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    1>=frac2    F ?: ',(1 >= frac2)
    END IF
    res_test = (frac1 >= 1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1>=1',test2=.FALSE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1>=1        F ?: ',(frac1 >= 1)
    END IF
    res_test = (frac1 >= frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1>=frac1',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1>=frac1    T ?: ',(frac1 >= frac1)
    END IF
    res_test = (frac2 >= 2)
    CALL Logical_Test(test_var,test1=res_test,info='frac2>=2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac2>=2        T ?: ',(frac2 >= 2)
    END IF
    res_test = (2 >= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='    2>=frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    2>=frac2    T ?: ',(2 >= frac2)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    ! tests on <=
    res_test = (frac1 <= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='frac1<=frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1<=frac2    F ?: ',(frac1 <= frac2)
    END IF
    res_test = (1 <= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='1<=frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    1<=frac2    F ?: ',(1 <= frac2)
    END IF
    res_test = (frac1 <= 1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1<=1',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1<=1        F ?: ',(frac1 <= 1)
    END IF
    res_test = (frac1 <= frac1)
    CALL Logical_Test(test_var,test1=res_test,info='frac1<=frac1',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1<=frac1    T ?: ',(frac1 <= frac1)
    END IF
    res_test = (frac2 <= 2)
    CALL Logical_Test(test_var,test1=res_test,info='frac2<=2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac2<=2        T ?: ',(frac2 <= 2)
    END IF
    res_test = (2 <= frac2)
    CALL Logical_Test(test_var,test1=res_test,info='    2<=frac2',test2=.TRUE.)
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '    2<=frac2    T ?: ',(2 <= frac2)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)

    ! simplification 
    frac1 = Frac_t(1,6)
    frac2 = Frac_t(18,12)

    res_test = ('1/6' == TO_string(frac1))
    CALL Logical_Test(test_var,test1=res_test,info='frac1         1/6')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1         1/6 ?: ',TO_string(frac1)
    END IF
    res_test = ('3/2' == TO_string(frac2))
    CALL Logical_Test(test_var,test1=res_test,info='frac2=18/12:  3/2')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac2=18/12:  3/2 ?: ',TO_string(frac2)
    END IF
    res_test = ('5/3' == TO_string(frac1+frac2))
    CALL Logical_Test(test_var,test1=res_test,info='frac1+frac2:  5/3')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1+frac2:  5/3 ?: ',TO_string(frac1+frac2)
    END IF
    CALL Flush_Test(test_var)
    write(out_unit,*)
    
    write(out_unit,*) 'table of Frac:'
    tab_frac1 = Frac_t([1,1,1],[2,3,4])
    !write(out_unit,*) 'lbound,ubound tab_frac1(:)',lbound(tab_frac1),ubound(tab_frac1)
    allocate(character(len=3) :: tab_string(size(tab_frac1)))
    !write(out_unit,*) 'lbound,ubound tab_string(:)',lbound(tab_string),ubound(tab_string)

    !tab_string(:) = [(TO_string(tab_frac1(i)),i=1,size(tab_string))] ! this has a problem with gfortran
    DO i=1,size(tab_string)
      tab_string(i) = TO_string(tab_frac1(i))
    END DO
    write(out_unit,*) 'tab_string            ',(tab_string(i) // ' ',i=1,size(tab_string))

    res_test = all(['1/2','1/3','1/4'] == tab_string)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac1:  1/2 1/3 1/4')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_string (tab_frac1)',(tab_string(i) // ' ',i=1,size(tab_string))
      write(out_unit,*) 'tab_string            ','1/2 1/3 1/4'
    END IF
    tab_frac2 = tab_frac1
    res_test = all(tab_frac2 == tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2=tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac2=tab_frac1  ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = 5*tab_frac1
    res_test = all(tab_frac2 == 5*tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = 5*tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '5*tab_frac1          ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = tab_frac1*5
    res_test = all(tab_frac2 == tab_frac1*5)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1*5')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1*5          ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = frac1*tab_frac1
    res_test = all(tab_frac2 == frac1*tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = frac1*tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'frac1*tab_frac1      ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = tab_frac1*frac1
    res_test = all(tab_frac2 == tab_frac1*frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1*frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1*frac1      ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = tab_frac1*tab_frac1
    res_test = all(tab_frac2 == tab_frac1*tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1*tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1*tab_frac1  ',tab_frac2
    END IF
    write(out_unit,*)
    
    tab_frac2 = 5/tab_frac1
    res_test = all(tab_frac2 == 5/tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = 5/tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '5/tab_frac1          ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = tab_frac1/5
    res_test = all(tab_frac2 == tab_frac1/5)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1/5')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1/5          ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = tab_frac1/tab_frac1
    res_test = all(tab_frac2 == tab_frac1/tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1/tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1/tab_frac1  ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = tab_frac1/0
    res_test = all(tab_frac2 == tab_frac1/0)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1/0')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1/0          ',tab_frac2
    END IF
    write(out_unit,*)
    
    tab_frac2 = tab_frac1 + tab_frac1
    res_test = all(tab_frac2 == tab_frac1 + tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1 + tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1+tab_frac1  ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = tab_frac1 + 5
    res_test = all(tab_frac2 == tab_frac1 + 5)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1 + 5')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1+5          ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = 5+tab_frac1
    res_test = all(tab_frac2 == 5+tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = 5+tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '5+tab_frac1          ',tab_frac2
    END IF
    write(out_unit,*)
    
    tab_frac2 = tab_frac1 - tab_frac1
    res_test = all(tab_frac2 == tab_frac1 - tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1 - tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1-tab_frac1  ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = tab_frac1 - 5
    res_test = all(tab_frac2 == tab_frac1 - 5)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = tab_frac1 - 5')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) 'tab_frac1-5          ',tab_frac2
    END IF
    write(out_unit,*)
    tab_frac2 = 5-tab_frac1
    res_test = all(tab_frac2 == 5-tab_frac1)
    CALL Logical_Test(test_var,test1=res_test,info='tab_frac2 = 5-tab_frac1')
    IF (.NOT. res_test .OR. debug) THEN
      write(out_unit,*) '5-tab_frac1          ',tab_frac2
    END IF
    write(out_unit,*)

    ! finalize the tests
    CALL Finalize_Test(test_var)
END SUBROUTINE Test_QDUtil_Frac
