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
!! This module deals with operations or functions of a scalar function and its derivatives, dnS.
!!
!! There is a mapping between the saclar function S, its derivatives and the dnS derived type components:
!!
!! @li S                 => S%d0
!! @li dS/dQ_i           => S%d1(i)
!! @li d^2S/dQ_idQ_j     => S%d2(i,j)
!! @li d^3S/dQ_idQ_jdQ_k => S%d3(i,j,k)
!!
!! with S defined as:
!!  TYPE (dnS_t) :: S
!!
!!
!! All standard fortran operators (= + - * / **) are overloaded:
!!
!! For instance the sum (+) of two dnS variables, S1 and S2 corresponds to:
!! @li (S1+S2)                 => S1%d0    + S2%d0
!! @li d(S1+S2)/dQ_i           => S1%d1(i) + S2%d1(i)
!! @li ....
!!
!! The product (*) of two dnS variables, S1 and S2 correspond to:
!! @li (S1*S2)                 => S1%d0 * S1%d0
!! @li d(S1*S2)/dQ_i           => S1%d0 * S2%d1(i) + S1%d1(i) * S2%d0    (derivative of a product)
!! @li ....
!!
!! All standard fortran functions (exp, sqrt, log ... sin ... sinh) are overloaded
!!
!! For instance the function, f, of dnS variables, S, corresponds to:
!! @li f(S)                    =>           f(S%d0)
!! @li d(f(S))/dQ_i            => S%d1(i) * f'(S%d0)
!! @li ....
!!
!! All fortran comparison operators (== /= > >= < <=) and (.EQ. .LT. ....) are overloaded, as well.
!! The comparison are done on the zero-order component:
!! S1 == S2                   =>   S1%d0 == S2%d0
!! S1 > S2                    =>   S1%d0  > S2%d0
!!
!!
!! @author David Lauvergnat
!! @date 09/08/2017
!!
MODULE ADdnSVM_dnS_m
  USE QDUtil_m, ONLY : Rkind, out_unit
  IMPLICIT NONE
  PRIVATE

!> @brief Derived type which deals with the derivatives of a scalar functions. It is a semi-private type.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param nderiv                  integer: it enables to chose the derivative order (from 0 to 3)
!! @param d0                      real:    0 order derivative (no derivative)
!! @param d1                      real:    1st order derivative (gradient: table of nVar derivatives)
!! @param d2                      real:    2d  order derivative (hessian: matrix of nVar*nVar derivatives)
!! @param d3                      real:    3d  order derivative (nVar*nVar*nVar derivatives)

  integer:: AD_dnS_test = 0


  TYPE, PUBLIC :: dnS_t
     PRIVATE
     integer                        :: nderiv = -1

     real (kind=Rkind)              :: d0
     real (kind=Rkind), allocatable :: d1(:)
     real (kind=Rkind), allocatable :: d2(:,:)
     real (kind=Rkind), allocatable :: d3(:,:,:)
  CONTAINS
    PROCEDURE, PRIVATE :: AD_set_dnS_TO_I
    PROCEDURE, PRIVATE :: AD_set_dnS_TO_R
    GENERIC,   PUBLIC  :: assignment(=) => AD_set_dnS_TO_R,AD_set_dnS_TO_I

    PROCEDURE, PRIVATE :: AD_dnS_EXP_R
    PROCEDURE, PRIVATE :: AD_dnS_EXP_I
    GENERIC,   PUBLIC  :: operator (**) => AD_dnS_EXP_R,AD_dnS_EXP_I

    PROCEDURE, PRIVATE :: AD_dnS2_PLUS_dnS1
    PROCEDURE, PRIVATE :: AD_PLUS_dnS
    PROCEDURE, PRIVATE :: AD_dnS_PLUS_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_PLUS_dnS
    PROCEDURE, PRIVATE :: AD_dnS_PLUS_I
    PROCEDURE, PRIVATE, PASS(S) :: AD_I_PLUS_dnS
    GENERIC,   PUBLIC  :: operator (+) => AD_dnS2_PLUS_dnS1,AD_PLUS_dnS,        &
                        AD_dnS_PLUS_R,AD_R_PLUS_dnS,AD_dnS_PLUS_I,AD_I_PLUS_dnS

    PROCEDURE, PRIVATE :: AD_dnS2_MINUS_dnS1
    PROCEDURE, PRIVATE :: AD_MINUS_dnS
    PROCEDURE, PRIVATE :: AD_dnS_MINUS_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_MINUS_dnS
    PROCEDURE, PRIVATE :: AD_dnS_MINUS_I
    PROCEDURE, PRIVATE, PASS(S) :: AD_I_MINUS_dnS
    GENERIC,   PUBLIC  :: operator (-) => AD_dnS2_MINUS_dnS1,AD_MINUS_dnS,      &
                     AD_dnS_MINUS_R,AD_R_MINUS_dnS,AD_dnS_MINUS_I,AD_I_MINUS_dnS

    PROCEDURE, PRIVATE :: AD_dnS2_TIME_dnS1
    PROCEDURE, PRIVATE :: AD_dnS_TIME_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_TIME_dnS
    PROCEDURE, PRIVATE :: AD_dnS_TIME_I
    PROCEDURE, PRIVATE, PASS(S) :: AD_I_TIME_dnS
    GENERIC,   PUBLIC  :: operator (*) => AD_dnS2_TIME_dnS1,AD_dnS_TIME_R,      &
                                       AD_R_TIME_dnS,AD_dnS_TIME_I,AD_I_TIME_dnS

    PROCEDURE, PRIVATE :: AD_dnS2_OVER_dnS1
    PROCEDURE, PRIVATE :: AD_dnS_OVER_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_OVER_dnS
    PROCEDURE, PRIVATE :: AD_dnS_OVER_I
    PROCEDURE, PRIVATE, PASS(S) :: AD_I_OVER_dnS
    GENERIC,   PUBLIC  :: operator (/) => AD_dnS2_OVER_dnS1,AD_dnS_OVER_R,      &
                                       AD_R_OVER_dnS,AD_dnS_OVER_I,AD_I_OVER_dnS

    PROCEDURE, PRIVATE :: AD_dnS_EQ_dnS
    PROCEDURE, PRIVATE :: AD_dnS_EQ_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_EQ_dnS
    GENERIC,   PUBLIC  :: operator (==) => AD_dnS_EQ_dnS,AD_dnS_EQ_R,AD_R_EQ_dnS

    PROCEDURE, PRIVATE :: AD_dnS_NEQ_dnS
    PROCEDURE, PRIVATE :: AD_dnS_NEQ_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_NEQ_dnS
    GENERIC,   PUBLIC  :: operator (/=) => AD_dnS_NEQ_dnS,AD_dnS_NEQ_R,AD_R_NEQ_dnS

    PROCEDURE, PRIVATE :: AD_dnS_LE_dnS
    PROCEDURE, PRIVATE :: AD_dnS_LE_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_LE_dnS
    GENERIC,   PUBLIC  :: operator (<=) => AD_dnS_LE_dnS,AD_dnS_LE_R,AD_R_LE_dnS

    PROCEDURE, PRIVATE :: AD_dnS_LT_dnS
    PROCEDURE, PRIVATE :: AD_dnS_LT_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_LT_dnS
    GENERIC,   PUBLIC  :: operator (<) => AD_dnS_LT_dnS,AD_dnS_LT_R,AD_R_LT_dnS

    PROCEDURE, PRIVATE :: AD_dnS_GE_dnS
    PROCEDURE, PRIVATE :: AD_dnS_GE_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_GE_dnS
    GENERIC,   PUBLIC  :: operator (>=) => AD_dnS_GE_dnS,AD_dnS_GE_R,AD_R_GE_dnS

    PROCEDURE, PRIVATE :: AD_dnS_GT_dnS
    PROCEDURE, PRIVATE :: AD_dnS_GT_R
    PROCEDURE, PRIVATE, PASS(S) :: AD_R_GT_dnS
    GENERIC,   PUBLIC  :: operator (>) => AD_dnS_GT_dnS,AD_dnS_GT_R,AD_R_GT_dnS

  END TYPE dnS_t

  ! overloded operators, functions
  PUBLIC :: sqrt
  PUBLIC :: exp,abs,log,log10
  PUBLIC :: cos,sin,tan,acos,asin,atan,cosh,sinh,tanh,acosh,asinh,atanh,atan2
  PUBLIC :: mod,modulo

  PUBLIC :: Variable,alloc_dnS,dealloc_dnS,set_dnS,set_d0S,Write_dnS
  PUBLIC :: deriv,grad

  PUBLIC :: get_nderiv,get_nVar
  PUBLIC :: sub_get_dn,get_d0,get_d1,get_d2,get_d3,get_Jacobian
  PUBLIC :: get_Flatten
  PUBLIC :: ReduceDerivatives_dnS2_TO_dnS1
  PUBLIC :: dnf_OF_dnS

  PUBLIC :: AD_get_Num_dnS_FROM_f_x,AD_Check_dnS_IS_ZERO,AD_d0S_TIME_R

  INTERFACE Variable_dnS
    MODULE PROCEDURE AD_init_dnS, AD_init_Tab_OF_dnS
  END INTERFACE
  INTERFACE Variable
     MODULE PROCEDURE AD_init_dnS, AD_init_Tab_OF_dnS
  END INTERFACE
  INTERFACE alloc_dnS
     MODULE PROCEDURE AD_alloc_dnS
  END INTERFACE
  INTERFACE dealloc_dnS
     MODULE PROCEDURE AD_dealloc_dnS
  END INTERFACE
  INTERFACE set_dnS
     MODULE PROCEDURE AD_set_dnS,AD_set_dnS_with_ider
  END INTERFACE
  INTERFACE set_d0S
    MODULE PROCEDURE AD_set_d0S
  END INTERFACE
  INTERFACE deriv
    MODULE PROCEDURE AD_Deriv_OF_dnS
  END INTERFACE
  INTERFACE grad
    MODULE PROCEDURE AD_Grad_OF_dnS
  END INTERFACE
  INTERFACE Write_dnS
     MODULE PROCEDURE AD_Write_dnS_file,AD_Write_dnS_string
  END INTERFACE
  INTERFACE get_nderiv
     MODULE PROCEDURE AD_get_nderiv_FROM_dnS
  END INTERFACE
  INTERFACE get_nVar
     MODULE PROCEDURE AD_get_nVar_FROM_dnS
  END INTERFACE
  INTERFACE sub_get_dn
     MODULE PROCEDURE AD_get_dn_FROM_dnS
  END INTERFACE
  INTERFACE get_d0
     MODULE PROCEDURE AD_get_d0_FROM_dnS
  END INTERFACE
  INTERFACE get_d1
     MODULE PROCEDURE AD_get_d1_FROM_dnS
  END INTERFACE

  INTERFACE get_Jacobian
     MODULE PROCEDURE AD_get_Jacobian_FROM_Vec_OF_dnS
  END INTERFACE

  INTERFACE get_d2
     MODULE PROCEDURE AD_get_d2_FROM_dnS
  END INTERFACE
  INTERFACE get_d3
     MODULE PROCEDURE AD_get_d3_FROM_dnS
  END INTERFACE
  INTERFACE   get_Flatten
     MODULE PROCEDURE   AD_get_Flatten_dnS
  END INTERFACE
  INTERFACE ReduceDerivatives_dnS2_TO_dnS1
     MODULE PROCEDURE AD_ReduceDerivatives_dnS2_TO_dnS1
  END INTERFACE

  INTERFACE dnf_OF_dnS
    MODULE PROCEDURE AD_dnF_OF_dnS
  END INTERFACE

  INTERFACE sqrt
     MODULE PROCEDURE AD_get_SQRT_dnS
  END INTERFACE
  INTERFACE abs
     MODULE PROCEDURE AD_get_ABS_dnS
  END INTERFACE
  INTERFACE exp
     MODULE PROCEDURE AD_get_EXP_dnS
  END INTERFACE
  INTERFACE log
     MODULE PROCEDURE AD_get_LOG_dnS
  END INTERFACE
  INTERFACE log10
     MODULE PROCEDURE AD_get_LOG10_dnS
  END INTERFACE
  INTERFACE cos
     MODULE PROCEDURE AD_get_COS_dnS
  END INTERFACE
  INTERFACE acos
     MODULE PROCEDURE AD_get_ACOS_dnS
  END INTERFACE
  INTERFACE sin
     MODULE PROCEDURE AD_get_SIN_dnS
  END INTERFACE
  INTERFACE asin
     MODULE PROCEDURE AD_get_ASIN_dnS
  END INTERFACE
  INTERFACE tan
     MODULE PROCEDURE AD_get_TAN_dnS
  END INTERFACE
  INTERFACE atan
     MODULE PROCEDURE AD_get_ATAN_dnS
  END INTERFACE
  INTERFACE atan2
     MODULE PROCEDURE AD_get_ATAN2_dnS
  END INTERFACE
  INTERFACE cosh
     MODULE PROCEDURE AD_get_COSH_dnS
  END INTERFACE
  INTERFACE acosh
     MODULE PROCEDURE AD_get_ACOSH_dnS
  END INTERFACE
  INTERFACE sinh
     MODULE PROCEDURE AD_get_SINH_dnS
  END INTERFACE
  INTERFACE asinh
     MODULE PROCEDURE AD_get_ASINH_dnS
  END INTERFACE
  INTERFACE tanh
     MODULE PROCEDURE AD_get_TANH_dnS
  END INTERFACE
  INTERFACE atanh
     MODULE PROCEDURE AD_get_ATANH_dnS
  END INTERFACE

  INTERFACE mod
    MODULE PROCEDURE AD_get_MOD_dnS
  END INTERFACE
  INTERFACE modulo
    MODULE PROCEDURE AD_get_MODULO_dnS
  END INTERFACE
CONTAINS
!> @brief Public subroutine which allocates a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                  TYPE (dnS_t):           derived type which deals with the derivatives of a scalar functions.
!! @param nVar               integer (optional):    number of variables (coordiantes) for the derivatives.
!! @param nderiv             integer (optional):    it enables to chose the derivative order (from 0 to 3).
!! @param err_dnS            integer (optional):  to handle the errors errors (0: no error).
  SUBROUTINE AD_alloc_dnS(S,nVar,nderiv,err_dnS)
    USE QDUtil_m, ONLY : out_unit
    IMPLICIT NONE

    TYPE (dnS_t), intent(inout)          :: S         !< derived type, which contains, matrix potential, its derivatives
    integer,      intent(in),  optional  :: nVar      !< number of variables (coordiantes)
    integer,      intent(in),  optional  :: nderiv    !< order of the derivatives [0,1,3]
    integer,      intent(out), optional  :: err_dnS !< to handle the errors

    ! local variables
    integer :: nVar_loc,err_dnS_loc,nderiv_loc
    character (len=*), parameter :: name_sub='AD_alloc_dnS'

    err_dnS_loc = 0 ! no error
    IF (present(err_dnS)) err_dnS = 0

    CALL AD_dealloc_dnS(S)

    ! test nVar
    IF (present(nVar)) THEN
      nVar_loc = nVar
    ELSE
      nVar_loc = 1
    END IF

    ! test nderiv
    IF (present(nderiv)) THEN
      nderiv_loc = max(0,nderiv)
      nderiv_loc = min(3,nderiv_loc)
    ELSE
      nderiv_loc = 0
    END IF
    S%nderiv = nderiv_loc

    !write(out_unit,*) 'S%nderiv in alloc_dnS',S%nderiv


    IF (nderiv_loc >= 1) THEN
      allocate(S%d1(nVar_loc),stat=err_dnS_loc)
      IF (err_dnS_loc /= 0 .OR. nVar_loc < 1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) '  Problem with allocate of S%d1'
        write(out_unit,*) '  nVar > 0?',nVar_loc
        IF (present(err_dnS)) THEN
          err_dnS = err_dnS_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnS'
        END IF
      END IF
    END IF

    IF (nderiv_loc >= 2) THEN
      allocate(S%d2(nVar_loc,nVar_loc),stat=err_dnS_loc)
      IF (err_dnS_loc /= 0 .OR. nVar_loc < 1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) '  Problem with allocate of S%d2'
        write(out_unit,*) '  nVar > 0',nVar_loc
        IF (present(err_dnS)) THEN
          err_dnS = err_dnS_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnS'
        END IF
      END IF
    END IF

    IF (nderiv_loc >= 3) THEN
      allocate(S%d3(nVar_loc,nVar_loc,nVar_loc),stat=err_dnS_loc)
      IF (err_dnS_loc /= 0 .OR. nVar_loc < 1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) '  Problem with allocate of S%d3'
        write(out_unit,*) '  nVar > 0',nVar_loc
        IF (present(err_dnS)) THEN
          err_dnS = err_dnS_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnS'
        END IF
      END IF
    END IF

    !write(out_unit,*) 'err_dnS_loc',err_dnS_loc
    !IF (present(err_dnS)) write(out_unit,*) 'err_dnS',err_dnS

  END SUBROUTINE AD_alloc_dnS

!> @brief Public subroutine which deallocates a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                  TYPE (dnS_t):             derived type which deals with the derivatives of a scalar functions.
  ELEMENTAL SUBROUTINE AD_dealloc_dnS(S)
    IMPLICIT NONE

    TYPE (dnS_t), intent(inout)       :: S !< derived type, which contains, matrix potential, its derivatives

    IF (allocated(S%d1)) deallocate(S%d1)
    IF (allocated(S%d2)) deallocate(S%d2)
    IF (allocated(S%d3)) deallocate(S%d3)

    S%nderiv = -1

  END SUBROUTINE AD_dealloc_dnS
!> @brief Public subroutine which checks if the derived type dnS is (correctly) allocated.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                  TYPE (dnS_t):  derived type which deals with the derivatives of a scalar functions.
!! @param nderiv             integer:    the derivative order.
  ELEMENTAL FUNCTION AD_Check_NotAlloc_dnS(S,nderiv) RESULT (NotAlloc)
    IMPLICIT NONE

    logical                     :: NotAlloc
    TYPE (dnS_t), intent(in)    :: S
    integer,      intent(in)    :: nderiv

    NotAlloc = (nderiv < 0)
    NotAlloc = NotAlloc .OR. (nderiv >= 1 .AND. .NOT. allocated(S%d1))
    NotAlloc = NotAlloc .OR. (nderiv >= 2 .AND. .NOT. allocated(S%d2))
    NotAlloc = NotAlloc .OR. (nderiv >= 3 .AND. .NOT. allocated(S%d3))

  END FUNCTION AD_Check_NotAlloc_dnS
!> @brief Public function which initializes a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Sres               TYPE (dnS_t) (result):    derived type which deals with the derivatives of a scalar functions.
!! @param Val                 real:                  Value of Sres%d0.
!! @param nVar               integer (optional):    number of variables (coordiantes) for the derivatives.
!! @param nderiv             integer (optional):    it enables to chose the derivative order (from 0 to 3).
!! @param iVar                 integer (optional):    when nVar > 1, dSres/dQ_iVar = Sres%d1(iVar)= 1, the other derivatives are zero
  FUNCTION AD_init_dnS(Val,nVar,nderiv,iVar) RESULT(Sres)
    USE QDUtil_m, ONLY : Rkind, ZERO, ONE, out_unit
    IMPLICIT NONE

    TYPE (dnS_t)                     :: Sres
    real (kind=Rkind), intent(in)    :: Val
    integer, optional, intent(in)    :: nderiv,nVar,iVar

    integer :: err_dnS_loc,nderiv_loc,nVar_loc,iVar_loc
    character (len=*), parameter :: name_sub='AD_init_dnS'

    ! test nVar
    IF (present(nVar)) THEN
      nVar_loc = nVar
    ELSE
      nVar_loc = 1
    END IF

    ! test nderiv
    IF (present(nderiv)) THEN
      nderiv_loc = max(0,nderiv)
      nderiv_loc = min(3,nderiv_loc)
    ELSE
      nderiv_loc = 0
    END IF

    ! test iVar
    IF (present(iVar)) THEN
      iVar_loc = iVar
    ELSE
      iVar_loc = 1
    END IF

    IF (iVar_loc < 0 .OR. iVar_loc > nVar_loc) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' the iVar value (',iVar_loc,') is out of range: [1:',nVar_loc,']'
      STOP 'Problem with the iVar value in init_dnS'
    END IF

    !write(out_unit,*) 'iVar_loc',iVar_loc ; flush(out_unit)
    !write(out_unit,*) 'nVar_loc,nderiv_loc',nVar_loc,nderiv_loc ; flush(out_unit)

    CALL AD_alloc_dnS(Sres,nVar_loc,nderiv_loc,err_dnS_loc)
    IF (err_dnS_loc /= 0) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' Problem in alloc_dnS CALL'
      STOP 'Problem Problem in AD_alloc_dnS CALL in AD_init_dnS'
    END IF

    Sres = ZERO

    Sres%d0 = Val
    IF (nderiv_loc > 0) Sres%d1(iVar_loc) = ONE

  END FUNCTION AD_init_dnS
  !> @brief Public function which initializes a derived type dnS.
  !!
  !> @author David Lauvergnat
  !! @date 03/08/2017
  !!
  !! @param Sres               TYPE (dnS_t) (result):    derived type which deals with the derivatives of a scalar functions.
  !! @param Val                 real:                  Value of Sres%d0.
  !! @param nVar               integer (optional):    number of variables (coordiantes) for the derivatives.
  !! @param nderiv             integer (optional):    it enables to chose the derivative order (from 0 to 3).
  !! @param iVar                 integer (optional):    when nVar > 1, dSres/dQ_iVar = Sres%d1(iVar)= 1, the other derivatives are zero
  FUNCTION AD_init_Tab_OF_dnS_old(TabVal,nderiv) RESULT(TabdnS)
    USE QDUtil_m, ONLY : Rkind, ZERO, ONE, out_unit
    IMPLICIT NONE

    TYPE (dnS_t), ALLOCATABLE        :: TabdnS(:)
    real (kind=Rkind), intent(in)    :: TabVal(:)
    integer, optional, intent(in)    :: nderiv

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_init_Tab_OF_dnS_old'

    integer :: nVar,iVar,nderiv_loc,i


    nVar = size(TabVal)
    IF (nVar < 1) RETURN

    ! test nderiv
    IF (present(nderiv)) THEN
      nderiv_loc = max(0,nderiv)
    ELSE
      nderiv_loc = 0
    END IF

    allocate(TabdnS(nVar))

    DO iVar=1,nVar
      CALL AD_alloc_dnS(TabdnS(iVar),nVar,nderiv_loc,err_dnS_loc)
      IF (err_dnS_loc /= 0) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' Problem in alloc_dnS CALL'
        STOP 'Problem Problem in AD_alloc_dnS CALL in AD_init_Tab_OF_dnS'
      END IF

      TabdnS(iVar) = ZERO

      TabdnS(iVar)%d0 = TabVal(iVar)
      IF (nderiv_loc > 0) TabdnS(iVar)%d1(iVar) = ONE
    END DO

  END FUNCTION AD_init_Tab_OF_dnS_old
  FUNCTION AD_init_Tab_OF_dnS(TabVal,nVar,nderiv,iVar) RESULT(TabdnS)
      USE QDUtil_m, ONLY : Rkind, ZERO, ONE, out_unit
      IMPLICIT NONE

      TYPE (dnS_t), ALLOCATABLE        :: TabdnS(:)
      real (kind=Rkind), intent(in)    :: TabVal(:)
      integer, optional, intent(in)    :: nderiv,nVar
      integer, optional, intent(in)    :: iVar(:)

      integer :: err_dnS_loc
      character (len=*), parameter :: name_sub='AD_init_Tab_OF_dnS'

    integer :: nVar_loc,nderiv_loc,iV,nVec

    nVec = size(TabVal)

    IF (present(nVar)) THEN
      IF (nVar < nVec) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' nVar is present and < size(TabVal)'
        write(out_unit,*) ' nVar        ',nVar
        write(out_unit,*) ' size(TabVal)',nVec
        write(out_unit,*) ' Check your Fortran or your data'
        STOP 'ERROR in AD_init_Tab_OF_dnS: nVar is present and < size(TabVal) '
      END IF
      nVar_loc = nVar
    ELSE
      nVar_loc = nVec
    END IF
    IF (nVar_loc < 1) RETURN

    ! test nderiv
    IF (present(nderiv)) THEN
      nderiv_loc = max(0,nderiv)
    ELSE
      nderiv_loc = 0
    END IF

    IF (present(iVar)) THEN
      IF (size(iVar) /= nVec) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' iVar(:) is present and sizes of TabVal and ivar are different'
        write(out_unit,*) ' size(iVar)  ',size(iVar)
        write(out_unit,*) ' size(TabVal)',nVec
        write(out_unit,*) ' Check your Fortran or your data'
        STOP 'ERROR in AD_init_Tab_OF_dnS: size(iVar) /= size(TabVal) '
      END IF
    END IF

    allocate(TabdnS(nVec))

    IF (present(iVar)) THEN
      DO iV=1,nVec
        TabdnS(iV) = Variable(TabVal(iV),nVar=nVar_loc,nderiv=nderiv_loc,iVar=iVar(iV))
      END DO
    ELSE
      DO iV=1,nVec
        TabdnS(iV) = Variable(TabVal(iV),nVar=nVar_loc,nderiv=nderiv_loc,iVar=iV)
      END DO
    END IF

  END FUNCTION AD_init_Tab_OF_dnS
!> @brief Public subroutine which initializes a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                  TYPE (dnS_t):        derived type which deals with the derivatives of a scalar functions.
!! @param d0,d1,d2,d3        real  (optional):    real value (d0) or table to initialize S.
  SUBROUTINE AD_set_dnS(S,d0,d1,d2,d3)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    real (kind=Rkind), optional,   intent(in)     :: d0
    real (kind=Rkind), optional,   intent(in)     :: d1(:)
    real (kind=Rkind), optional,   intent(in)     :: d2(:,:)
    real (kind=Rkind), optional,   intent(in)     :: d3(:,:,:)

    TYPE (dnS_t),                  intent(inout)  :: S


    character (len=*), parameter :: name_sub='AD_set_dnS'

    IF (present(d0)) THEN
      S%d0 = d0
      S%nderiv = 0
    END IF

    IF (present(d1)) THEN
      S%d1     = d1
      S%nderiv = 1
      IF (.NOT. present(d0)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d1 is present but not d0'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnS'
      END IF
    END IF

    IF (present(d2)) THEN
      S%d2     = d2
      S%nderiv = 2
      IF (.NOT. present(d1)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d2 is present but not d1'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnS'
      END IF
    END IF

    IF (present(d3)) THEN
      S%d3     = d3
      S%nderiv = 3
      IF (.NOT. present(d2)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d3 is present but not d2'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnS'
      END IF
    END IF

  END SUBROUTINE AD_set_dnS
  SUBROUTINE AD_set_d0S(S,d0)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    real (kind=Rkind),   intent(in)     :: d0
    TYPE (dnS_t),        intent(inout)  :: S


    character (len=*), parameter :: name_sub='AD_set_d0S'

    S%d0 = d0

  END SUBROUTINE AD_set_d0S
  SUBROUTINE AD_set_dnS_with_ider(S,val,ider)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    real (kind=Rkind),   intent(in)     :: val
    integer,             intent(in)     :: ider(:)

    TYPE (dnS_t),        intent(inout)  :: S

    integer :: nderiv,i1,i2,i3

    character (len=*), parameter :: name_sub='AD_set_dnS_with_ider'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.

    IF (debug) THEN
      write(out_unit,*) ' BEGINNNING ',name_sub
      write(out_unit,*) ' val ',val
      write(out_unit,*) ' ider ',ider
      flush(out_unit)
   END IF

    nderiv = count(ider > 0)
    IF (debug) write(out_unit,*) ' nderiv ',nderiv

    IF (size(ider) /= nderiv .AND. nderiv > 0) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' incompatible ider(:) values. ider(:)',ider
      write(out_unit,*) ' no zero or negative values are possible'
      write(out_unit,*) ' Except ider = [0] for nderiv=0'
      write(out_unit,*) ' CHECK the fortran!!'
      STOP 'ERROR in AD_set_dnS_with_ider: incompatible ider(:) values'
    END IF

    SELECT CASE (nderiv)
    CASE (0)
      S%d0                          = val
    CASE (1)
      S%d1(ider(1))                 = val
    CASE (2)
      S%d2(ider(1),ider(2))         = val
    CASE (3)
      S%d3(ider(1),ider(2),ider(3)) = val
    CASE Default
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' incompatible ider(:) values. ider(:)',ider
      write(out_unit,*) ' more than 3 values (nderiv >3)'
      write(out_unit,*) ' CHECK the fortran!!'
      STOP 'ERROR in AD_set_dnS_with_ider: incompatible ider(:) values'
    END SELECT

    IF (debug) write(out_unit,*) ' END ',name_sub 

  END SUBROUTINE AD_set_dnS_with_ider
  SUBROUTINE AD_ReduceDerivatives_dnS2_TO_dnS1(S1,S2,list_act)
    USE QDUtil_m, ONLY : out_unit
    IMPLICIT NONE

    CLASS (dnS_t), intent(inout) :: S1
    CLASS (dnS_t), intent(in)    :: S2
    integer,       intent(in)    :: list_act(:)

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_ReduceDerivatives_dnS2_TO_dnS1'

    CALL AD_dealloc_dnS(S1)

    IF (allocated(S2%d1)) THEN
    IF (size(list_act) > size(S2%d1) ) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' size(list_act) > size(S2%d1)'
      write(out_unit,*) ' size(list_act) ',size(list_act)
      write(out_unit,*) ' size(S2%d1)    ',size(S2%d1)
      write(out_unit,*) ' CHECK the fortran!!'
      STOP 'ERROR in AD_ReduceDerivatives_dnS2_TO_dnS1'
    END IF
    END IF

    S1%nderiv = S2%nderiv

    S1%d0 = S2%d0
    IF (allocated(S2%d1)) S1%d1 = S2%d1(list_act)
    IF (allocated(S2%d2)) S1%d2 = S2%d2(list_act,list_act)
    IF (allocated(S2%d3)) S1%d3 = S2%d3(list_act,list_act,list_act)

  END SUBROUTINE AD_ReduceDerivatives_dnS2_TO_dnS1
  FUNCTION AD_Deriv_OF_dnS(S,ider) RESULT(dS)
    USE QDUtil_m, ONLY : Rkind, out_unit, ZERO, TO_string
    IMPLICIT NONE

    TYPE (dnS_t)                               :: dS
    TYPE (dnS_t),                  intent(in)  :: S
    integer,                       intent(in)  :: ider

    integer :: nderiv,nVar
    real (kind=Rkind), allocatable :: d1(:),d2(:,:),d3(:,:,:)
    character (len=*), parameter :: name_sub='AD_Deriv_OF_dnS'

    nderiv = get_nderiv(S)
    nVar   = get_nVar(S)

    IF (ider > nVar .OR. ider < 1) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' ider is not in the [1:',TO_string(nVar),'] range.'
      write(out_unit,*) ' ider',ider
      STOP 'ERROR in AD_Deriv_OF_dnS: wrong ider'
    END IF

    SELECT CASE (nderiv)
    CASE (1)
      d1 = get_d1(S)
      CALL set_dnS(dS,d0=d1(ider))
    CASE (2)
      d1 = get_d1(S)
      d2 = get_d2(S)
      CALL set_dnS(dS,d0=d1(ider),d1=d2(:,ider))
    CASE (3)
      d1 = get_d1(S)
      d2 = get_d2(S)
      d3 = get_d3(S)
      CALL set_dnS(dS,d0=d1(ider),d1=d2(:,ider),d2=d3(:,:,ider))
    CASE default ! nderiv = 0 or <0
      CALL set_dnS(dS,d0=ZERO)
    END SELECT

  END FUNCTION AD_Deriv_OF_dnS
  FUNCTION AD_Grad_OF_dnS(S) RESULT(TabdS)
    USE QDUtil_m, ONLY : Rkind, ZERO, ONE, out_unit
    IMPLICIT NONE

    TYPE (dnS_t), ALLOCATABLE   :: TabdS(:)
    TYPE (dnS_t), intent(in)    :: S

    character (len=*), parameter :: name_sub='AD_Grad_OF_dnS'

  integer :: iV,nVar

  nVar = get_nVar(S)
  allocate(TabdS(nVar))
  DO iV=1,nVar
    TabdS(iV) = Deriv(S,iV)
  END DO

END FUNCTION AD_Grad_OF_dnS
  !> @brief Public function to get d0 from a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                  TYPE (dnS_t):           derived type which deals with the derivatives of a scalar functions.
!! @param d0                 real  (result):      d0=S%d0
  ELEMENTAL FUNCTION AD_get_d0_FROM_dnS(S) RESULT(d0)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    real (kind=Rkind)                :: d0
    TYPE (dnS_t),        intent(in)  :: S

    character (len=*), parameter :: name_sub='AD_get_d0_FROM_dnS'

    d0 = S%d0

  END FUNCTION AD_get_d0_FROM_dnS
!> @brief Public function to get d1 from a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                  TYPE (dnS_t):           derived type which deals with the derivatives of a scalar functions.
!! @param d1                 real  (result):        d1=S%d1, d1 is allocatable
  FUNCTION AD_get_d1_FROM_dnS(S) RESULT(d1)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    real (kind=Rkind), allocatable     :: d1(:)
    TYPE (dnS_t),        intent(in)    :: S

    character (len=*), parameter :: name_sub='AD_get_d1_FROM_dnS'

    IF (allocated(S%d1)) d1 = S%d1

  END FUNCTION AD_get_d1_FROM_dnS
  FUNCTION AD_get_Jacobian_FROM_Vec_OF_dnS(Vec) RESULT(Jac)
    USE QDUtil_m, ONLY : Rkind, ZERO, out_unit
    IMPLICIT NONE

    real (kind=Rkind), allocatable     :: Jac(:,:)
    TYPE (dnS_t),        intent(in)    :: Vec(:)

    integer :: iVar,nVarNew,NvarOld
    character (len=*), parameter :: name_sub='AD_get_Jacobian_FROM_Vec_OF_dnS'

    nVarNew = size(Vec)
    IF (nVarNew < 1 .OR. get_nderiv(Vec(1)) < 1) RETURN

    nVarOld = 0

    DO iVar=1,nVarNew
      IF (get_nderiv(Vec(iVar)) < 1) CYCLE
      IF (nVarOld == 0) nVarOld = get_nVar(Vec(iVar))

      IF (nVarOld /= get_nVar(Vec(iVar))) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) '  The nVarOld from Vec(:) are different'
        write(out_unit,*) '  iVar',iVar
        write(out_unit,*) '  nVar from Vec_OF_dnS(iVar)',get_nVar(Vec(iVar))
        write(out_unit,*) '  nVarOld',nVarOld
        STOP 'ERROR in AD_get_Jacobian_FROM_Vec_OF_dnS: incomptiple nVar from Vec(:)'
      END IF

    END DO
    IF (nVarOld > 0) THEN
      allocate(Jac(nVarNew,nVarOld))
      Jac(:,:) = ZERO
      DO iVar=1,nVarNew

        IF (get_nderiv(Vec(iVar)) < 1) CYCLE

        Jac(iVar,:) = get_d1(Vec(iVar))
      END DO
    END IF

  END FUNCTION AD_get_Jacobian_FROM_Vec_OF_dnS
  !> @brief Public function to get d2 from a derived type dnS.
  !!
  !> @author David Lauvergnat
  !! @date 03/08/2017
  !!
  !! @param S                  TYPE (dnS_t):           derived type which deals with the derivatives of a scalar functions.
  !! @param d2                 real  (result):        d2=S%d2, d2 is allocatable
  FUNCTION AD_get_d2_FROM_dnS(S) RESULT(d2)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

      real (kind=Rkind),   allocatable   :: d2(:,:)
      TYPE (dnS_t),        intent(in)    :: S

      character (len=*), parameter :: name_sub='AD_get_d2_FROM_dnS'

      IF (allocated(S%d2)) d2 = S%d2

    END FUNCTION AD_get_d2_FROM_dnS
  !> @brief Public function to get d3 from a derived type dnS.
  !!
  !> @author David Lauvergnat
  !! @date 03/08/2017
  !!
  !! @param S                  TYPE (dnS_t):           derived type which deals with the derivatives of a scalar functions.
  !! @param d3                 real  (result):        d3=S%d3, d3 is allocatable
  FUNCTION AD_get_d3_FROM_dnS(S) RESULT(d3)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

      real (kind=Rkind), allocatable   :: d3(:,:,:)
      TYPE (dnS_t),        intent(in)    :: S

      character (len=*), parameter :: name_sub='AD_get_d3_FROM_dnS'

      IF (allocated(S%d3)) d3 = S%d3

  END FUNCTION AD_get_d3_FROM_dnS
!> @brief Public function to get FlatdnS(:) from a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 09/01/2023
!!
!! @param S            TYPE (dnS_t):      derived type which deals with the derivatives of a scalar functions.
!! @param FlatdnS      real  (result):
  FUNCTION AD_get_Flatten_dnS(S,all_der,i_der) RESULT(FlatdnS)
      USE QDUtil_m, ONLY : Rkind, out_unit
      IMPLICIT NONE

      real (kind=Rkind),    allocatable          :: FlatdnS(:)

      TYPE (dnS_t),         intent(in)           :: S
      logical,              intent(in), optional :: all_der
      integer,              intent(in), optional :: i_der

      logical :: tab_der(0:3)
      integer :: nderiv,FlatdnS_size,i,f

      nderiv = get_nderiv(S)
      IF (nderiv < 0) THEN
        allocate(FlatdnS(0))
        RETURN ! S%d... are not allocated
      END IF

      tab_der(:) = .FALSE.

      IF (present(all_der)) THEN
        IF (all_der) THEN
          tab_der(0:nderiv) = .TRUE.
          IF (present(i_der)) THEN
             write(out_unit,*) ' ERROR in AD_get_Flatten_dnS'
             write(out_unit,*) ' all_der and i_der are present and incompatible'
             write(out_unit,*) '   all_der,i_der',all_der,i_der
             STOP 'ERROR in AD_get_Flatten_dnS: all_der and i_der are present and incompatible'
          END IF
        END IF
      END IF

      IF (present(i_der)) THEN
        IF (i_der < 0 .OR. i_der > nderiv) THEN
          write(out_unit,*) ' ERROR in AD_get_Flatten_dnS'
          write(out_unit,*) ' i_der MUST be >= 0 and <= nderiv'
          write(out_unit,*) '   i_der,nderiv',i_der,nderiv
          STOP 'ERROR in AD_get_Flatten_dnS: i_der MUST be >= 0 and <= nderiv'
        END IF
        tab_der(0:nderiv) = .FALSE.
        tab_der(i_der)    = .TRUE.
      ELSE
        tab_der(0:nderiv) = .TRUE.
      END IF
      ! size of FlatdnS
      FlatdnS_size = 0
      IF (tab_der(0)) FlatdnS_size = FlatdnS_size + 1
      IF (tab_der(1)) FlatdnS_size = FlatdnS_size + size(S%d1)
      IF (tab_der(2)) FlatdnS_size = FlatdnS_size + size(S%d2)
      IF (tab_der(3)) FlatdnS_size = FlatdnS_size + size(S%d3)
      !write(out_unit,*) 'tab_der,FlatdnS_size',tab_der,FlatdnS_size

      allocate(FlatdnS(FlatdnS_size))

      i = 0
      IF (tab_der(0)) THEN
        f = i + 1
        FlatdnS(i+1:f) = S%d0
        i = f
      END IF
      IF (tab_der(1)) THEN
        f = i + size(S%d1)
        FlatdnS(i+1:f) = S%d1
        i = f
      END IF
      IF (tab_der(2)) THEN
        f = i + size(S%d2)
        FlatdnS(i+1:f) = reshape(S%d2,shape=[size(S%d2)])
        i = f
      END IF
      IF (tab_der(3)) THEN
        f = i + size(S%d3)
        FlatdnS(i+1:f) = reshape(S%d3,shape=[size(S%d3)])
        i = f
      END IF

  END FUNCTION AD_get_Flatten_dnS
  !> @brief Public subroutine to get d0,d1,d2,d3 from a derived type dnS.
  !!
  !> @author David Lauvergnat
  !! @date 25/05/2018
  !!
  !! @param S                  TYPE (dnS_t):          derived type which deals with the derivatives of a scalar functions.
  !! @param d0,d1,d2,d3                 real:         NOT allocatable
  SUBROUTINE AD_get_dn_FROM_dnS(S,d0,d1,d2,d3)
    USE QDUtil_m, ONLY : Rkind, ZERO, out_unit
    IMPLICIT NONE

      TYPE (dnS_t),        intent(in)              :: S
      real (kind=Rkind),   intent(inout), optional :: d0,d1(:),d2(:,:),d3(:,:,:)

      character (len=*), parameter :: name_sub='AD_get_dn_FROM_dnS'

      IF (present(d0)) THEN
        d0 = S%d0
      END IF
      IF (present(d1)) THEN
        IF (allocated(S%d1)) THEN
           d1(:) = S%d1
        ELSE
           d1(:) = ZERO
        END IF
      END IF
      IF (present(d2)) THEN
        IF (allocated(S%d2)) THEN
           d2(:,:) = S%d2
        ELSE
           d2(:,:) = ZERO
        END IF
      END IF
      IF (present(d3)) THEN
        IF (allocated(S%d3)) THEN
           d3(:,:,:) = S%d3
        ELSE
           d3(:,:,:) = ZERO
        END IF
      END IF

  END SUBROUTINE AD_get_dn_FROM_dnS
!> @brief Public subroutine which prints a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                  TYPE (dnS_t):         derived type which deals with the derivatives of a scalar functions.
!! @param nio                integer (optional):   when present unit to print S, otherwise it is the default unit:out_unit
!! @param info               character (optional): when present, write info
!! @param all_type           character (optional): when present and true, write all the type variable (old WriteAll_dnS)
!! @param FOR_test           character (optional): when present and true, write for the test (old Write_dnS_FOR_test)
  SUBROUTINE AD_Write_dnS_file(S,nio,info,all_type,FOR_test,Rfmt,nderiv)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnS_t),     intent(in)           :: S
    integer,          intent(in), optional :: nio
    character(len=*), intent(in), optional :: info
    logical,          intent(in), optional :: all_type,FOR_test
    character(len=*), intent(in), optional :: Rfmt
    integer,          intent(in), optional :: nderiv


    integer :: i,j,k,nio_loc,nderiv_loc,nVar
    logical :: all_type_loc,FOR_test_loc
    character (len=:), allocatable :: fformat,Rfmt_loc

    IF (present(nio)) THEN
      nio_loc = nio
    ELSE
      nio_loc = out_unit
    END IF

    all_type_loc = .FALSE.
    IF (present(all_type)) all_type_loc = all_type
    FOR_test_loc = .FALSE.
    IF (present(FOR_test)) FOR_test_loc = FOR_test

    nderiv_loc = S%nderiv
    IF (present(nderiv)) nderiv_loc = min(nderiv_loc,nderiv)

    IF (all_type_loc) THEN ! write all variables
      IF (present(info)) write(nio_loc,*) info
      write(nio_loc,*) '-------------------------------------------'
      write(nio_loc,*) 'Write_dnS (all)'
      write(nio_loc,*) 'nderiv',S%nderiv
      write(nio_loc,*) 'S%d0',S%d0
      IF (allocated(S%d1) .AND. nderiv_loc > 0) THEN
        write(nio_loc,*) 'S%d1:',S%d1
      ELSE
        write(nio_loc,*) 'S%d1: not allocated or not printed'
      END IF
      IF (allocated(S%d2) .AND. nderiv_loc > 1) THEN
        write(nio_loc,*) 'S%d2:',S%d2
      ELSE
        write(nio_loc,*) 'S%d2: not allocated or not printed'
      END IF
      IF (allocated(S%d3) .AND. nderiv_loc > 2) THEN
        write(nio_loc,*) 'S%d3:',S%d3
      ELSE
        write(nio_loc,*) 'S%d3: not allocated or not printed'
      END IF
      write(nio_loc,*) 'END Write_dnS (all)'
      write(nio_loc,*) '-------------------------------------------'
    ELSE IF (FOR_test_loc) THEN ! for Test
      AD_dnS_test = AD_dnS_test + 1
      IF (present(info)) THEN
        write(nio_loc,'(a,i0,1x,i0,a,a)') 'TEST #',AD_dnS_test,S%nderiv,' dnSca: ',trim(adjustl(info))
      ELSE
        write(nio_loc,'(a,i0,1x,i0,a)') 'TEST #',AD_dnS_test,S%nderiv,' dnSca'
      END IF
      write(nio_loc,*) 'S%d0'
      write(nio_loc,*) 1
      write(nio_loc,*) S%d0
      IF (allocated(S%d1) .AND. nderiv_loc > 0) THEN
        write(nio_loc,*) 'S%d1'
        write(nio_loc,*) size(S%d1)
        write(nio_loc,*) S%d1
      END IF
      IF (allocated(S%d2) .AND. nderiv_loc > 1) THEN
        write(nio_loc,*) 'S%d2'
        write(nio_loc,*) size(S%d2)
        write(nio_loc,*) S%d2
      END IF
      IF (allocated(S%d3) .AND. nderiv_loc > 2) THEN
        write(nio_loc,*) 'S%d3'
        write(nio_loc,*) size(S%d3)
        write(nio_loc,*) S%d3
      END IF

      write(nio_loc,'(a,i0,a)') 'END_TEST #',AD_dnS_test,' dnSca'
    ELSE ! normal writing
      IF (present(Rfmt)) THEN
        Rfmt_loc = Rfmt
      ELSE
        Rfmt_loc = 'e12.3'
      END IF
      IF (present(info)) write(nio_loc,*) info

      nVar = 0
      IF (allocated(S%d1)) nVar = size(S%d1)

      fformat = '(a,3(3x),1x,sp,' // Rfmt_loc // ')'
      write(nio_loc,fformat) ' 0   derivative',S%d0

      IF (allocated(S%d1) .AND. nderiv_loc > 0) THEN
        IF (nVar > 99) THEN
          fformat = '(a,(1x,i0),1x,sp,' // Rfmt_loc // ')'
        ELSE
          fformat = '(a,(1x,i2),2(3x),1x,sp,' // Rfmt_loc // ')'
        END IF

        DO i=1,ubound(S%d1,dim=1)
          write(nio_loc,fformat) ' 1st derivative',i,S%d1(i)
        END DO
      END IF
      IF (allocated(S%d2) .AND. nderiv_loc > 1) THEN
        IF (nVar > 99) THEN
          fformat = '(a,2(1x,i0),1x,sp,' // Rfmt_loc // ')'
        ELSE
          fformat = '(a,2(1x,i2),1(3x),1x,sp,' // Rfmt_loc // ')'
        END IF
        DO i=1,ubound(S%d2,dim=2)
        DO j=1,ubound(S%d2,dim=1)
          write(nio_loc,fformat) ' 2d  derivative',j,i,S%d2(j,i)
        END DO
        END DO
      END IF
      IF (allocated(S%d3) .AND. nderiv_loc > 2) THEN
        IF (nVar > 99) THEN
          fformat = '(a,3(1x,i0),1x,sp,' // Rfmt_loc // ')'
        ELSE
          fformat = '(a,3(1x,i2),1x,sp,' // Rfmt_loc // ')'
        END IF
        DO i=1,ubound(S%d3,dim=3)
        DO j=1,ubound(S%d3,dim=2)
        DO k=1,ubound(S%d3,dim=1)
          write(nio_loc,fformat) ' 3d  derivative',k,j,i,S%d3(k,j,i)
        END DO
        END DO
        END DO
      END IF
    END IF

  END SUBROUTINE AD_Write_dnS_file
  SUBROUTINE AD_Write_dnS_string(S,string,info,all_type,FOR_test,Rfmt,nderiv)
    USE QDUtil_m, ONLY : Rkind, TO_string, out_unit
    IMPLICIT NONE

    TYPE (dnS_t),     intent(in)           :: S
    character (len=:), allocatable         :: string
    character(len=*), intent(in), optional :: info
    logical,          intent(in), optional :: all_type,FOR_test
    character(len=*), intent(in), optional :: Rfmt
    integer,          intent(in), optional :: nderiv

    integer :: i,j,k,nderiv_loc
    logical :: all_type_loc,FOR_test_loc
    character (len=:), allocatable :: fformat,Rfmt_loc

    IF (.NOT. allocated(string)) string = ''

    all_type_loc = .FALSE.
    IF (present(all_type)) all_type_loc = all_type
    FOR_test_loc = .FALSE.
    IF (present(FOR_test)) FOR_test_loc = FOR_test

    nderiv_loc = S%nderiv
    IF (present(nderiv)) nderiv_loc = min(nderiv_loc,nderiv)

    IF (all_type_loc) THEN ! write all variables
      IF (present(info)) string = string // info // new_line('a')
      string = string // '-------------------------------------------' // new_line('a') // &
         'Write_dnS (all)' // new_line('a') //                                  &
         'nderiv' // TO_string(S%nderiv) // new_line('a') //                  &
         'S%d0' // TO_string(S%d0) // new_line('a')
      IF (allocated(S%d1) .AND. nderiv_loc > 0 ) THEN
        string = string // 'S%d1:'
        DO i=lbound(S%d1,dim=1),ubound(S%d1,dim=1)
          string = string // ' ' // TO_string(S%d1(i))
        END DO
        string = string // new_line('a')
      ELSE
        string = string // 'S%d1: not allocated or not printed' // new_line('a')
      END IF
      IF (allocated(S%d2) .AND. nderiv_loc > 1) THEN
        string = string // 'S%d2:'
        DO j=lbound(S%d2,dim=2),ubound(S%d2,dim=2)
        DO i=lbound(S%d2,dim=1),ubound(S%d2,dim=1)
          string = string // ' ' // TO_string(S%d2(i,j))
        END DO
        END DO
        string = string // new_line('a')
      ELSE
        string = string // 'S%d2: not allocated or not printed' // new_line('a')
      END IF
      IF (allocated(S%d3) .AND. nderiv_loc > 2) THEN
        string = string // 'S%d3:'
        DO k=lbound(S%d3,dim=3),ubound(S%d3,dim=3)
        DO j=lbound(S%d3,dim=2),ubound(S%d3,dim=2)
        DO i=lbound(S%d3,dim=1),ubound(S%d3,dim=1)
          string = string // ' ' // TO_string(S%d3(i,j,k))
        END DO
        END DO
        END DO
        string = string // new_line('a')
      ELSE
        string = string // 'S%d3: not allocated or not printed' // new_line('a')
      END IF
      string = string // 'END Write_dnS (all)' // new_line('a') //              &
            '-------------------------------------------' // new_line('a')
    ELSE ! normal writing
      IF (present(Rfmt)) THEN
        Rfmt_loc = Rfmt
      ELSE
        Rfmt_loc = 'e12.3'
      END IF
      IF (present(info)) string = string // info // new_line('a')


      fformat = '(a,3(3x),1x,sp,' // Rfmt_loc // ')'

      string = string // ' 0   derivative       ' //                            &
          TO_string(S%d0,Rformat=Rfmt_loc) // new_line('a')


      IF (allocated(S%d1)) THEN
        DO i=lbound(S%d1,dim=1),ubound(S%d1,dim=1)
          string = string // ' 1st derivative     ' // TO_string(i) // ' ' // &
                        TO_string(S%d1(i),Rformat=Rfmt_loc)  // new_line('a')
        END DO
      END IF
      IF (allocated(S%d2)) THEN
        DO i=lbound(S%d2,dim=2),ubound(S%d2,dim=2)
        DO j=lbound(S%d2,dim=1),ubound(S%d2,dim=1)
          string = string // ' 2d  derivative   ' // TO_string(j) // ' ' //   &
                        TO_string(i) // ' ' //                                &
                        TO_string(S%d2(j,i),Rformat=Rfmt_loc)  // new_line('a')
        END DO
        END DO
      END IF
      IF (allocated(S%d3)) THEN
        DO i=lbound(S%d3,dim=3),ubound(S%d3,dim=3)
        DO j=lbound(S%d3,dim=2),ubound(S%d3,dim=2)
        DO k=lbound(S%d3,dim=1),ubound(S%d3,dim=1)
          string = string // ' 3d  derivative ' // TO_string(k) // ' ' //     &
                         TO_string(j) // ' ' // TO_string(i) // ' ' //      &
                        TO_string(S%d3(k,j,i),Rformat=Rfmt_loc)  // new_line('a')
        END DO
        END DO
        END DO
      END IF
    END IF

  END SUBROUTINE AD_Write_dnS_string
!> @brief Public function to get nderiv from a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                     TYPE (dnS_t):        derived type which deals with the derivatives of a scalar functions.
!! @param get_nderiv_FROM_dnS   integer  (result):   nderiv value, check against S%nederiv and the allocated d1,d2 or d3
  ELEMENTAL FUNCTION AD_get_nderiv_FROM_dnS(S) RESULT(nderiv)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    integer                     :: nderiv
    TYPE (dnS_t), intent(in)    :: S

    nderiv = S%nderiv
    IF (S%nderiv == -1) THEN
      nderiv = -1
    ELSE IF (.NOT. allocated(S%d1)) THEN
      nderiv = 0
    ELSE IF (.NOT. allocated(S%d2)) THEN
      nderiv = 1
    ELSE IF (.NOT. allocated(S%d3)) THEN
      nderiv = 2
    ELSE
      nderiv = 3
    END IF

    END FUNCTION AD_get_nderiv_FROM_dnS
!> @brief Public function to get nVar from a derived type dnS.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param S                     TYPE (dnS_t):        derived type which deals with the derivatives of a scalar functions.
!! @param nVar                  integer  (result):   nVar value from the size of S%d1.
  ELEMENTAL FUNCTION AD_get_nVar_FROM_dnS(S) RESULT(nVar)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    integer                     :: nVar
    TYPE (dnS_t), intent(in)    :: S

    IF (.NOT. allocated(S%d1)) THEN
      nVar = 0
    ELSE
      nVar = size(S%d1(:))
    END IF

  END FUNCTION AD_get_nVar_FROM_dnS
!> @brief Public function which ckecks a derived type dnS is zero (all components).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Check_dnS_IS_ZERO   logical  (result):   result of the comparison
!! @param S                     TYPE (dnS_t):           derived type which deals with the derivatives of a scalar functions.
!! @param epsi                  real (optional):     when present zero limit, otherwise 10^-10
  ELEMENTAL FUNCTION AD_Check_dnS_IS_ZERO(S,epsi) RESULT(Check_dnS_IS_ZERO)
    USE QDUtil_m, ONLY : Rkind, ONETENTH, out_unit
    IMPLICIT NONE

    logical                                  :: Check_dnS_IS_ZERO
    TYPE (dnS_t),       intent(in)           :: S
    real(kind=Rkind),   intent(in), optional :: epsi

    real(kind=Rkind) :: epsi_loc


    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF


    Check_dnS_IS_ZERO = (AD_get_maxval_OF_dnS(S) <= epsi_loc)

  END FUNCTION AD_Check_dnS_IS_ZERO
!> @brief Public function which gets the largest value of a derived type dnS (all components).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param get_maxval_OF_dnS     real  (result):      largest value
!! @param S                     TYPE (dnS_t):           derived type which deals with the derivatives of a scalar functions.
  ELEMENTAL FUNCTION AD_get_maxval_OF_dnS(S) RESULT(maxval_OF_dnS)
    USE QDUtil_m, ONLY : Rkind, ZERO, out_unit
    IMPLICIT NONE

    real(kind=Rkind)            :: maxval_OF_dnS
    TYPE (dnS_t), intent(in)    :: S

    real(kind=Rkind) :: e0,e1,e2,e3

    e1 = ZERO
    e2 = ZERO
    e3 = ZERO
    e0 = abs(S%d0)
    IF (allocated(S%d1)) e1 = maxval(abs(S%d1))
    IF (allocated(S%d2)) e2 = maxval(abs(S%d2))
    IF (allocated(S%d3)) e3 = maxval(abs(S%d3))

    maxval_OF_dnS = max(e0,e1,e2,e3)

  END FUNCTION AD_get_maxval_OF_dnS
!> @brief Public function which calculates numerical derivative of a function
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!

!! @param Snum                     TYPE (dnS_t):    function value, f(x), and derivatives f' f" f'".
!! @param f                        real:            function (intrinsic or external)
!! @param x                        real:            abciss
!! @param nderiv                   integer:         order of the derivative
  FUNCTION AD_get_Num_dnS_FROM_f_x(x,f,nderiv) RESULT(Snum)
    USE QDUtil_m, ONLY : Rkind, ZERO,ONE,TWO,THREE,FOUR,EIGHT,TWELVE, ONETENTH, out_unit
    IMPLICIT NONE

    TYPE (dnS_t)                                 :: Snum
    real (kind=Rkind), external                  :: f ! an intrinsic function: sin exp ....
    real (kind=Rkind),           intent(in)      :: x
    integer,           optional, intent(in)      :: nderiv

    !local variables:
    TYPE (dnS_t)          :: Sloc
    real (kind=Rkind)     :: xloc,step=ONETENTH**4
    integer               :: nderiv_loc,i,j,k
    real (kind=Rkind)     :: f0,fp,fm,fpp,fmm,fppp,fmmm


    character (len=*), parameter :: name_sub='AD_get_Num_dnS_FROM_f_x'

    ! test nderiv
    IF (present(nderiv)) THEN
      nderiv_loc = max(0,nderiv)
      nderiv_loc = min(3,nderiv_loc)
    ELSE
      nderiv_loc = 0
    END IF

    CALL AD_alloc_dnS(Snum,nVar=1,nderiv=nderiv_loc)
    Snum = ZERO

    SELECT CASE (nderiv_loc)
    CASE(0)
      Snum%d0 = f(x)
    CASE(1)
      f0  = f(x)
      fp  = f(x+step)
      fm  = f(x-step)

      Snum%d0 = f0
      Snum%d1 = (fp-fm)/(step+step)
    CASE(2)
      f0  = f(x)
      fp  = f(x+step)
      fm  = f(x-step)
      fpp = f(x+step+step)
      fmm = f(x-step-step)

      Snum%d0 = f0
      Snum%d1 = (fp-fm)/(step+step)
      Snum%d2 = (fp+fm-TWO*f0)/step**2
    CASE(3)
      f0   = f(x)
      fp   = f(x+step)
      fm   = f(x-step)
      fpp  = f(x+step+step)
      fmm  = f(x-step-step)
      fppp = f(x+step+step+step)
      fmmm = f(x-step-step-step)

      Snum%d0 = f0
      Snum%d1 = ( THREE/FOUR*(fp-fm) - &
                 THREE/20._Rkind*(fpp-fmm) + &
                 ONE/60._Rkind*(fppp-fmmm) )/step

      Snum%d2 = (-30._Rkind*f0+16._Rkind*(fp+fm)-(fpp+fmm))/(TWELVE*step**2)

      Snum%d3 = (-13._Rkind*(fp-fm)+EIGHT*(fpp-fmm)-(fppp-fmmm))/(EIGHT*step**3)

    END SELECT

  END FUNCTION AD_get_Num_dnS_FROM_f_x

!=========================================================
! operators ==,/=,>=,>,<=,<
!=========================================================
  ELEMENTAL FUNCTION AD_dnS_EQ_dnS(S1,S2) RESULT(lres)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    logical                     :: lres
    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_dnS_EQ_dnS'

    lres = (S1%d0 == S2%d0)

    IF (allocated(S1%d1) .AND. allocated(S2%d1)) lres = lres .AND. all(S1%d1 == S2%d1)
    IF (allocated(S1%d2) .AND. allocated(S2%d2)) lres = lres .AND. all(S1%d2 == S2%d2)
    IF (allocated(S1%d3) .AND. allocated(S2%d3)) lres = lres .AND. all(S1%d3 == S2%d3)

  END FUNCTION AD_dnS_EQ_dnS
  ELEMENTAL FUNCTION AD_dnS_EQ_R(S1,R) RESULT(lres)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    logical                            :: lres
    CLASS (dnS_t),       intent(in)    :: S1
    real (kind=Rkind),   intent(in)    :: R

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_dnS_EQ_R'

    lres = (S1%d0 == R)

  END FUNCTION AD_dnS_EQ_R
  ELEMENTAL FUNCTION AD_R_EQ_dnS(R,S) RESULT(lres)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    logical                            :: lres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_R_EQ_dnS'

    lres = (R == S%d0)

  END FUNCTION AD_R_EQ_dnS

  ELEMENTAL FUNCTION AD_dnS_NEQ_dnS(S1,S2) RESULT(lres)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    logical                     :: lres
    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_dnS_NEQ_dnS'

    lres = (S1%d0 /= S2%d0)

    IF (allocated(S1%d1) .AND. allocated(S2%d1)) lres = lres .OR. all(S1%d1 /= S2%d1)
    IF (allocated(S1%d2) .AND. allocated(S2%d2)) lres = lres .OR. all(S1%d2 /= S2%d2)
    IF (allocated(S1%d3) .AND. allocated(S2%d3)) lres = lres .OR. all(S1%d3 /= S2%d3)

  END FUNCTION AD_dnS_NEQ_dnS
  ELEMENTAL FUNCTION AD_dnS_NEQ_R(S1,R) RESULT(lres)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    CLASS (dnS_t),     intent(in)    :: S1
    real (kind=Rkind), intent(in)    :: R

    logical                   :: lres
    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_dnS_NEQ_R'

    lres = (S1%d0 /= R)

  END FUNCTION AD_dnS_NEQ_R
  ELEMENTAL FUNCTION AD_R_NEQ_dnS(R,S) RESULT(lres)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    CLASS (dnS_t),     intent(in)    :: S
    real (kind=Rkind), intent(in)    :: R
    logical                   :: lres
    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_R_NEQ_dnS'

    lres = (R /= S%d0)

  END FUNCTION AD_R_NEQ_dnS

  ELEMENTAL FUNCTION AD_dnS_LE_dnS(S1,S2) RESULT(lres)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2
    logical                     :: lres

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_dnS_LE_dnS'

    lres = (S1%d0 <= S2%d0)

  END FUNCTION AD_dnS_LE_dnS
  ELEMENTAL FUNCTION AD_dnS_LE_R(S1,R) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t),     intent(in)    :: S1
    real (kind=Rkind), intent(in)    :: R
    logical                          :: lres

    character (len=*), parameter :: name_sub='AD_dnS_LE_R'

    lres = (S1%d0 <= R)

  END FUNCTION AD_dnS_LE_R
  ELEMENTAL FUNCTION AD_R_LE_dnS(R,S) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t),     intent(in)    :: S
    real (kind=Rkind), intent(in)    :: R
    logical                   :: lres

    character (len=*), parameter :: name_sub='AD_R_LE_dnS'

    lres = (R <= S%d0)

  END FUNCTION AD_R_LE_dnS

  ELEMENTAL FUNCTION AD_dnS_LT_dnS(S1,S2) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2
    logical                     :: lres

    character (len=*), parameter :: name_sub='AD_dnS_LT_dnS'

    lres = (S1%d0 < S2%d0)

  END FUNCTION AD_dnS_LT_dnS
  ELEMENTAL FUNCTION AD_dnS_LT_R(S1,R) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t),     intent(in)    :: S1
    real (kind=Rkind), intent(in)    :: R
    logical                   :: lres

    character (len=*), parameter :: name_sub='AD_dnS_LT_R'

    lres = (S1%d0 < R)

  END FUNCTION AD_dnS_LT_R
  ELEMENTAL FUNCTION AD_R_LT_dnS(R,S) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t),     intent(in)    :: S
    real (kind=Rkind), intent(in)    :: R
    logical                   :: lres
    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_R_LT_dnS'

    lres = (R < S%d0)

  END FUNCTION AD_R_LT_dnS

  ELEMENTAL FUNCTION AD_dnS_GE_dnS(S1,S2) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2
    logical                   :: lres

    character (len=*), parameter :: name_sub='AD_dnS_GE_dnS'

    lres = (S1%d0 >= S2%d0)

  END FUNCTION AD_dnS_GE_dnS
  ELEMENTAL FUNCTION AD_dnS_GE_R(S1,R) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t),     intent(in)    :: S1
    real (kind=Rkind), intent(in)    :: R
    logical                   :: lres

    character (len=*), parameter :: name_sub='AD_dnS_GE_R'

    lres = (S1%d0 >= R)

  END FUNCTION AD_dnS_GE_R
  ELEMENTAL FUNCTION AD_R_GE_dnS(R,S) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t),     intent(in)    :: S
    real (kind=Rkind), intent(in)    :: R
    logical                          :: lres

    character (len=*), parameter :: name_sub='AD_R_GE_dnS'

    lres = (R >= S%d0)

  END FUNCTION AD_R_GE_dnS

  ELEMENTAL FUNCTION AD_dnS_GT_dnS(S1,S2) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2
    logical                   :: lres
    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_dnS_GT_dnS'

    lres = (S1%d0 > S2%d0)

  END FUNCTION AD_dnS_GT_dnS
  ELEMENTAL FUNCTION AD_dnS_GT_R(S1,R) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t),     intent(in)    :: S1
    real (kind=Rkind), intent(in)    :: R
    logical                   :: lres

    character (len=*), parameter :: name_sub='AD_dnS_GT_R'

    lres = (S1%d0 > R)

  END FUNCTION AD_dnS_GT_R
  ELEMENTAL FUNCTION AD_R_GT_dnS(R,S) RESULT(lres)
    USE QDUtil_m

    CLASS (dnS_t),        intent(in)   :: S
    real (kind=Rkind),   intent(in)    :: R
    logical                            :: lres

    character (len=*), parameter :: name_sub='AD_R_GT_dnS'

    lres = (R > S%d0)

  END FUNCTION AD_R_GT_dnS
!=========================================================
! operators =,+,-,*,/,**
!=========================================================
  ELEMENTAL SUBROUTINE AD_sub_dnS2_TO_dnS1(S1,S2) ! not used anymore
    USE QDUtil_m
    CLASS (dnS_t), intent(inout) :: S1
    TYPE (dnS_t),  intent(in)    :: S2

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_sub_dnS2_TO_dnS1'

    CALL AD_dealloc_dnS(S1)

    S1%nderiv = S2%nderiv

    S1%d0 = S2%d0
    IF (allocated(S2%d1)) S1%d1 = S2%d1
    IF (allocated(S2%d2)) S1%d2 = S2%d2
    IF (allocated(S2%d3)) S1%d3 = S2%d3

  END SUBROUTINE AD_sub_dnS2_TO_dnS1

  ELEMENTAL SUBROUTINE AD_set_dnS_TO_R(S,R)
    USE QDUtil_m

    CLASS (dnS_t),     intent(inout) :: S
    real (kind=Rkind), intent(in)    :: R

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_set_dnS_TO_R'


    !if nderiv of S is > -1, nderiv must be unchanged
    !if nderiv = -1, nderiv must be undefined (-1). Therefore,nderiv must be unchanged

    S%d0 = R

    IF (allocated(S%d1)) S%d1 = ZERO
    IF (allocated(S%d2)) S%d2 = ZERO
    IF (allocated(S%d3)) S%d3 = ZERO

  END SUBROUTINE AD_set_dnS_TO_R
  ELEMENTAL SUBROUTINE AD_set_dnS_TO_I(S,I)
    USE QDUtil_m

    CLASS (dnS_t),     intent(inout) :: S
    integer,           intent(in)    :: I

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_set_dnS_TO_I'


    !if nderiv of S is > -1, nderiv must be unchanged
    !if nderiv = -1, nderiv must be undefined (-1). Therefore,nderiv must be unchanged

    S%d0 = I

    IF (allocated(S%d1)) S%d1 = ZERO
    IF (allocated(S%d2)) S%d2 = ZERO
    IF (allocated(S%d3)) S%d3 = ZERO

  END SUBROUTINE AD_set_dnS_TO_I
!=========================================================


  ELEMENTAL FUNCTION AD_dnS2_PLUS_dnS1(S1,S2) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                :: Sres
    CLASS (dnS_t), intent(in)   :: S1,S2

    character (len=*), parameter :: name_sub='AD_dnS2_PLUS_dnS1'


    !several cases:
    !Rq: R1=S1%d0       R2=S2%d0
    !   S1%nderiv       S2%nderiv   =>      Sres%nderiv
    !       -1              -1                  -1          =>     R1+R2
    !       -1              >= 0    =>      S2%nderiv       => eq: R1+S2
    !      >= 0             -1      =>      S1%nderiv       => eq: S1+R2
    !      >= 0             >= 0    =>    min(S1%nderiv,S2%nderiv) => all

    !if nderiv of S is > -1, nderiv must be unchanged
    !if nderiv = -1, nderiv must be undefined (-1). Therefore,nderiv must be unchanged

    IF (S1%nderiv == -1 .AND. S2%nderiv == -1) THEN
       CALL AD_dealloc_dnS(Sres)
       Sres%d0 = S1%d0 + S2%d0
       !Sres%nderiv = -1 (done in AD_dealloc_dnS)
    ELSE IF (S1%nderiv == -1 .AND. S2%nderiv > -1) THEN
       Sres = S1%d0 + S2 ! R+dnS
       !Sres%nderiv = S2%nderiv (done in the equality)
    ELSE IF (S1%nderiv > -1 .AND. S2%nderiv == -1) THEN
       Sres = S1 + S2%d0 ! dnS+R
       !Sres%nderiv = S1%nderiv
    ELSE
       CALL AD_dealloc_dnS(Sres)
       Sres%nderiv = min(S1%nderiv,S2%nderiv)
                                                    Sres%d0 = S1%d0 + S2%d0
       IF (allocated(S1%d1) .AND. allocated(S2%d1)) Sres%d1 = S1%d1 + S2%d1
       IF (allocated(S1%d2) .AND. allocated(S2%d2)) Sres%d2 = S1%d2 + S2%d2
       IF (allocated(S1%d3) .AND. allocated(S2%d3)) Sres%d3 = S1%d3 + S2%d3
    END IF

  END FUNCTION AD_dnS2_PLUS_dnS1
  ELEMENTAL FUNCTION AD_dnS_PLUS_R(S,R) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R


    character (len=*), parameter :: name_sub='AD_dnS_PLUS_R'

    Sres    = S
    Sres%d0 = Sres%d0 + R

  END FUNCTION AD_dnS_PLUS_R
  ELEMENTAL FUNCTION AD_R_PLUS_dnS(R,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R


    character (len=*), parameter :: name_sub='AD_R_PLUS_dnS'

    Sres    = S
    Sres%d0 = Sres%d0 + R

  END FUNCTION AD_R_PLUS_dnS
  ELEMENTAL FUNCTION AD_dnS_PLUS_I(S,I) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I


    character (len=*), parameter :: name_sub='AD_dnS_PLUS_I'

    Sres    = S
    Sres%d0 = Sres%d0 + I

  END FUNCTION AD_dnS_PLUS_I
  ELEMENTAL FUNCTION AD_I_PLUS_dnS(I,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I


    character (len=*), parameter :: name_sub='AD_I_PLUS_dnS'

    Sres    = S
    Sres%d0 = Sres%d0 + I

  END FUNCTION AD_I_PLUS_dnS
  ELEMENTAL FUNCTION AD_PLUS_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S


    character (len=*), parameter :: name_sub='AD_PLUS_dnS'

    Sres = S

  END FUNCTION AD_PLUS_dnS
  ELEMENTAL FUNCTION AD_dnS2_MINUS_dnS1(S1,S2) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                :: Sres
    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2

    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_dnS2_MINUS_dnS1'

    IF (S1%nderiv == -1 .AND. S2%nderiv == -1) THEN
       CALL AD_dealloc_dnS(Sres)
       Sres%d0 = S1%d0 - S2%d0
       !Sres%nderiv = -1 (done in AD_dealloc_dnS)
    ELSE IF (S1%nderiv == -1 .AND. S2%nderiv > -1) THEN
       Sres = S1%d0 - S2
       !Sres%nderiv = S2%nderiv (done in the equality)
    ELSE IF (S1%nderiv > -1 .AND. S2%nderiv == -1) THEN
       Sres = S1 - S2%d0
       !Sres%nderiv = S1%nderiv
    ELSE
       CALL AD_dealloc_dnS(Sres)
       Sres%nderiv = min(S1%nderiv,S2%nderiv)
                                                    Sres%d0 = S1%d0 - S2%d0
       IF (allocated(S1%d1) .AND. allocated(S2%d1)) Sres%d1 = S1%d1 - S2%d1
       IF (allocated(S1%d2) .AND. allocated(S2%d2)) Sres%d2 = S1%d2 - S2%d2
       IF (allocated(S1%d3) .AND. allocated(S2%d3)) Sres%d3 = S1%d3 - S2%d3
    END IF

  END FUNCTION AD_dnS2_MINUS_dnS1
  ELEMENTAL FUNCTION AD_dnS_MINUS_R(S,R) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R


    character (len=*), parameter :: name_sub='AD_dnS_mINUS_R'

    Sres    = S
    Sres%d0 = Sres%d0 - R

  END FUNCTION AD_dnS_MINUS_R
  ELEMENTAL FUNCTION AD_R_MINUS_dnS(R,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R


    character (len=*), parameter :: name_sub='AD_R_MINUS_dnS'

    !CALL AD_dealloc_dnS(Sres) (done in Sres = -S)

    Sres = -S

    Sres%d0 = R -S%d0

  END FUNCTION AD_R_MINUS_dnS
  ELEMENTAL FUNCTION AD_dnS_MINUS_I(S,I) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I


    character (len=*), parameter :: name_sub='AD_dnS_mINUS_I'

    Sres    = S
    Sres%d0 = Sres%d0 - I

  END FUNCTION AD_dnS_MINUS_I
  ELEMENTAL FUNCTION AD_I_MINUS_dnS(I,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I


    character (len=*), parameter :: name_sub='AD_I_MINUS_dnS'

    !CALL AD_dealloc_dnS(Sres) (done in Sres = -S)

    Sres = -S

    Sres%d0 = I -S%d0

  END FUNCTION AD_I_MINUS_dnS
  ELEMENTAL FUNCTION AD_MINUS_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S


    character (len=*), parameter :: name_sub='AD_MINUS_dnS'


    CALL AD_dealloc_dnS(Sres)

    Sres%nderiv = S%nderiv

                         Sres%d0 = -S%d0
    IF (allocated(S%d1)) Sres%d1 = -S%d1
    IF (allocated(S%d2)) Sres%d2 = -S%d2
    IF (allocated(S%d3)) Sres%d3 = -S%d3

  END FUNCTION AD_MINUS_dnS

  ELEMENTAL FUNCTION AD_dnS2_TIME_dnS1(S1,S2) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                :: Sres
    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2

    integer :: id,jd,kd
    character (len=*), parameter :: name_sub='AD_dnS2_TIME_dnS1'

    IF (S1%nderiv == -1 .AND. S2%nderiv == -1) THEN
       CALL AD_dealloc_dnS(Sres)
       Sres%d0 = S1%d0 * S2%d0
       !Sres%nderiv = -1 (done in AD_dealloc_dnS)
    ELSE IF (S1%nderiv == -1 .AND. S2%nderiv > -1) THEN
       Sres = S1%d0 * S2 ! R*dnS
       !Sres%nderiv = S2%nderiv (done in the equality)
    ELSE IF (S1%nderiv > -1 .AND. S2%nderiv == -1) THEN
       Sres = S1 * S2%d0 ! dnS*R
       !Sres%nderiv = S1%nderiv
    ELSE
       CALL AD_dealloc_dnS(Sres)
       Sres%nderiv = min(S1%nderiv,S2%nderiv)

       Sres%d0 = S1%d0 * S2%d0

       IF (allocated(S1%d1) .AND. allocated(S2%d1)) THEN
         Sres%d1 = S1%d1 * S2%d0 + S1%d0 * S2%d1

         IF (allocated(S1%d2) .AND. allocated(S2%d2)) THEN
           Sres%d2 = S1%d2 * S2%d0 + S1%d0 * S2%d2
           DO id=1,size(S1%d1)
           DO jd=1,size(S1%d1)
             Sres%d2(jd,id) = Sres%d2(jd,id) + S1%d1(id) * S2%d1(jd) + S1%d1(jd) * S2%d1(id)
           END DO
           END DO

           IF (allocated(S1%d3) .AND. allocated(S2%d3)) THEN
             Sres%d3 = S1%d3 * S2%d0 + S1%d0 * S2%d3
             DO id=1,size(S1%d1)
             DO jd=1,size(S1%d1)
             DO kd=1,size(S1%d1)
               Sres%d3(kd,jd,id) = Sres%d3(kd,jd,id) + S1%d1(id) * S2%d2(kd,jd) + &
                                                       S1%d1(jd) * S2%d2(kd,id) + &
                                                       S1%d1(kd) * S2%d2(jd,id) + &
                                                       S1%d2(kd,jd) * S2%d1(id) + &
                                                       S1%d2(kd,id) * S2%d1(jd) + &
                                                       S1%d2(jd,id) * S2%d1(kd)

             END DO
             END DO
             END DO
           END IF
         END IF
       END IF
    END IF

  END FUNCTION AD_dnS2_TIME_dnS1
  ELEMENTAL FUNCTION AD_d0S_TIME_R(S,R) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                         :: Sres
    TYPE (dnS_t),          intent(in)    :: S
    real (kind=Rkind),     intent(in)    :: R


    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_d0S_TIME_R'


    Sres = S

    Sres%d0 = R * S%d0

  END FUNCTION AD_d0S_TIME_R
  ELEMENTAL FUNCTION AD_dnS_TIME_R(S,R) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R


    character (len=*), parameter :: name_sub='AD_dnS_TIME_R'

    CALL AD_dealloc_dnS(Sres)

    Sres%nderiv = S%nderiv

                         Sres%d0 = R*S%d0
    IF (allocated(S%d1)) Sres%d1 = R*S%d1
    IF (allocated(S%d2)) Sres%d2 = R*S%d2
    IF (allocated(S%d3)) Sres%d3 = R*S%d3

  END FUNCTION AD_dnS_TIME_R

   ELEMENTAL FUNCTION AD_R_TIME_dnS(R,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R


    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_R_TIME_dnS'

    CALL AD_dealloc_dnS(Sres)

    Sres%nderiv = S%nderiv

                         Sres%d0 = R*S%d0
    IF (allocated(S%d1)) Sres%d1 = R*S%d1
    IF (allocated(S%d2)) Sres%d2 = R*S%d2
    IF (allocated(S%d3)) Sres%d3 = R*S%d3

  END FUNCTION AD_R_TIME_dnS

  ELEMENTAL FUNCTION AD_dnS_TIME_I(S,I) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I


    character (len=*), parameter :: name_sub='AD_dnS_TIME_I'

    Sres = S * real(I,kind=Rkind)

  END FUNCTION AD_dnS_TIME_I

   ELEMENTAL FUNCTION AD_I_TIME_dnS(I,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I


    integer :: err_dnS_loc
    character (len=*), parameter :: name_sub='AD_I_TIME_dnS'

    Sres = S * real(I,kind=Rkind)

  END FUNCTION AD_I_TIME_dnS

  ELEMENTAL FUNCTION AD_dnS2_OVER_dnS1(S1,S2) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                :: Sres
    CLASS (dnS_t), intent(in)   :: S1
    TYPE (dnS_t),  intent(in)   :: S2

    character (len=*), parameter :: name_sub='AD_dnS2_OVER_dnS1'


    Sres = S1 * S2**(-1) ! idiot !!!

  END FUNCTION AD_dnS2_OVER_dnS1
  ELEMENTAL FUNCTION AD_dnS_OVER_R(S,R) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R


    character (len=*), parameter :: name_sub='AD_dnS_OVER_R'


    Sres = S * (ONE/R)

  END FUNCTION AD_dnS_OVER_R

  ELEMENTAL FUNCTION AD_R_OVER_dnS(R,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R


    character (len=*), parameter :: name_sub='AD_R_OVER_dnS'


    Sres = R * S**(-1)


  END FUNCTION AD_R_OVER_dnS

  ELEMENTAL FUNCTION AD_dnS_OVER_I(S,I) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I


    character (len=*), parameter :: name_sub='AD_dnS_OVER_I'


    Sres = S * (ONE/real(I,kind=Rkind))

  END FUNCTION AD_dnS_OVER_I

  ELEMENTAL FUNCTION AD_I_OVER_dnS(I,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I


    character (len=*), parameter :: name_sub='AD_I_OVER_dnS'


    Sres = real(I,kind=Rkind) * S**(-1)


  END FUNCTION AD_I_OVER_dnS


!=========================================================
! mathematical intrinsic functions: cos, sin exp, log, cosh ....
! All functions in the fortran norm except atan2 because it has two arguments
!=========================================================

  ELEMENTAL FUNCTION AD_get_F_dnS(S,d0f,d1f,d2f,d3f) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: d0f,d1f,d2f,d3f

    integer :: id,jd,kd
    character (len=*), parameter :: name_sub='AD_get_F_dnS'

    CALL AD_dealloc_dnS(Sres)

    Sres%nderiv = S%nderiv

    Sres%d0 = d0f
    IF (allocated(S%d1)) THEN
      Sres%d1 =  d1f * S%d1

      IF (allocated(S%d2)) THEN
        Sres%d2 = d1f * S%d2
        DO id=1,size(S%d1)
        DO jd=1,size(S%d1)
          Sres%d2(jd,id) = Sres%d2(jd,id) + d2f * S%d1(id)*S%d1(jd)
        END DO
        END DO

        IF (allocated(S%d3)) THEN
          Sres%d3 = d1f * S%d3
          DO id=1,size(S%d1)
          DO jd=1,size(S%d1)
          DO kd=1,size(S%d1)
            Sres%d3(kd,jd,id) = Sres%d3(kd,jd,id) + &
                                d2f * (S%d1(id)*S%d2(kd,jd) + S%d1(jd)*S%d2(kd,id) + S%d1(kd)*S%d2(jd,id)) + &
                                d3f * S%d1(id)*S%d1(jd)*S%d1(kd)
          END DO
          END DO
          END DO
        END IF
      END IF
    END IF


  END FUNCTION AD_get_F_dnS

  FUNCTION AD_dnF_OF_dnS(f,S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S,f

    integer :: nderiv,id,jd,kd
    character (len=*), parameter :: name_sub='AD_dnF_OF_dnS'

    CALL AD_dealloc_dnS(Sres)

    IF (get_nVar(f) /= 1) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nVar of f is not 1'
      write(out_unit,*) '  nVar',get_nVar(f)
      STOP 'ERROR in AD_dnF_OF_dnS: nVar of f is not 1'
    END IF

    nderiv = min(get_nderiv(f),get_nderiv(S))

    CALL alloc_dnS(Sres, nVar=get_nVar(S), nderiv=nderiv)

    !Sres%nderiv = S%nderiv

    Sres%d0 = f%d0 ! f%d0 = func(s%d0)

    IF (allocated(Sres%d1)) THEN
      Sres%d1 =  f%d1(1) * S%d1
    END IF

    IF (allocated(Sres%d2)) THEN
      Sres%d2 = f%d1(1) * S%d2
      DO id=1,size(S%d1)
      DO jd=1,size(S%d1)
        Sres%d2(jd,id) = Sres%d2(jd,id) + f%d2(1,1) * S%d1(id)*S%d1(jd)
      END DO
      END DO
    END IF

    IF (allocated(Sres%d3)) THEN
      Sres%d3 = f%d1(1) * S%d3
      DO id=1,size(S%d1)
      DO jd=1,size(S%d1)
      DO kd=1,size(S%d1)
        Sres%d3(kd,jd,id) = Sres%d3(kd,jd,id) + &
                            f%d2(1,1) * (S%d1(id)*S%d2(kd,jd) + S%d1(jd)*S%d2(kd,id) + S%d1(kd)*S%d2(jd,id)) + &
                            f%d3(1,1,1) * S%d1(id)*S%d1(jd)*S%d1(kd)
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_dnF_OF_dnS

  ELEMENTAL FUNCTION AD_dnS_EXP_R(S,R) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    real (kind=Rkind),   intent(in)    :: R

    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_dnS_EXP_R'

    IF (R == ZERO) THEN
      Sres = S ! to have the right initialization. it cannot deallocate and set to the constant R
      Sres = ONE
    ELSE IF (R == ONE) THEN
      Sres = S
    ELSE IF (R == TWO) THEN
      Sres = S*S
    ELSE
      d0f = S%d0**R
      IF (S%nderiv >= 1) d1f = S%d0**(R-ONE)   * R
      IF (S%nderiv >= 2) d2f = S%d0**(R-TWO)   * R*(R-ONE)
      IF (S%nderiv >= 3) d3f = S%d0**(R-THREE) * R*(R-ONE)*(R-TWO)
      Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)
    END IF


  END FUNCTION AD_dnS_EXP_R
  ELEMENTAL FUNCTION AD_dnS_EXP_I(S,I) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S
    integer,             intent(in)    :: I

    real (kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_dnS_EXP_I'

    IF (I == 0) THEN
      Sres = S ! to have the right initialization. it cannot deallocate and set to the constant R
      Sres = ONE
    ELSE IF (I == -1) THEN
      d0f = ONE/S%d0
      IF (S%nderiv >= 1) d1f = -d0f*d0f
      IF (S%nderiv >= 2) d2f = -TWO*d0f*d1f
      IF (S%nderiv >= 3) d3f =  -THREE*d0f*d2f
      Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)
    ELSE IF (I == 1) THEN
      Sres = S
    ELSE IF (I == 2) THEN
      Sres = S*S
    ELSE
      Sres = S**real(I,kind=Rkind)
    END IF

  END FUNCTION AD_dnS_EXP_I
  ELEMENTAL FUNCTION AD_get_SQRT_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    CLASS (dnS_t),       intent(in)    :: S


    character (len=*), parameter :: name_sub='AD_get_SQRT_dnS'

    Sres = S**HALF

  END FUNCTION AD_get_SQRT_dnS

  ELEMENTAL FUNCTION AD_get_ABS_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S


    character (len=*), parameter :: name_sub='AD_get_ABS_dnS'

    Sres = (S*S)**HALF

  END FUNCTION AD_get_ABS_dnS

  ELEMENTAL FUNCTION AD_get_EXP_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f

    character (len=*), parameter :: name_sub='AD_get_EXP_dnS'

    d0f =  exp(S%d0)
    IF (S%nderiv >= 1) d1f =  exp(S%d0)
    IF (S%nderiv >= 2) d2f =  exp(S%d0)
    IF (S%nderiv >= 3) d3f =  exp(S%d0)

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_EXP_dnS
  ELEMENTAL FUNCTION AD_get_LOG_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f

    character (len=*), parameter :: name_sub='AD_get_LOG_dnS'


    d0f =  log(S%d0)
    IF (S%nderiv >= 1) d1f =  ONE/S%d0
    IF (S%nderiv >= 2) d2f = -ONE/S%d0**2
    IF (S%nderiv >= 3) d3f =  TWO/S%d0**3

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_LOG_dnS
  ELEMENTAL FUNCTION AD_get_LOG10_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f

    character (len=*), parameter :: name_sub='AD_get_LOG10_dnS'

    Sres = log(S)/log(TEN)

  END FUNCTION AD_get_LOG10_dnS
  ELEMENTAL FUNCTION AD_get_COS_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S


    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_COS_dnS'



    d0f =  cos(S%d0)
    IF (S%nderiv >= 1) d1f = -sin(S%d0)
    IF (S%nderiv >= 2) d2f = -cos(S%d0)
    IF (S%nderiv >= 3) d3f =  sin(S%d0)

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_COS_dnS
  ELEMENTAL FUNCTION AD_get_ACOS_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S


    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_ACOS_dnS'



    d0f =  acos(S%d0)
    IF (S%nderiv >= 1) d1f = -ONE/sqrt(ONE-S%d0**2)
    IF (S%nderiv >= 2) d2f = S%d0*d1f**3
    IF (S%nderiv >= 3) d3f = (ONE+TWO*S%d0**2)*d1f**5

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_ACOS_dnS
  ELEMENTAL FUNCTION AD_get_SIN_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_SIN_dnS'



     d0f =  sin(S%d0)
    IF (S%nderiv >= 1) d1f =  cos(S%d0)
    IF (S%nderiv >= 2) d2f = -sin(S%d0)
    IF (S%nderiv >= 3) d3f = -cos(S%d0)

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_SIN_dnS
  ELEMENTAL FUNCTION AD_get_ASIN_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_ASIN_dnS'



    d0f =  asin(S%d0)
    IF (S%nderiv >= 1) d1f = ONE/sqrt(ONE-S%d0**2)
    IF (S%nderiv >= 2) d2f = S%d0*d1f**3
    IF (S%nderiv >= 3) d3f = (ONE+TWO*S%d0**2)*d1f**5

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_ASIN_dnS
  ELEMENTAL FUNCTION AD_get_TAN_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_TAN_dnS'

    Sres = Sin(S)/cos(S)

  END FUNCTION AD_get_TAN_dnS

  ELEMENTAL FUNCTION AD_get_ATAN_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_ATAN_dnS'


    d0f =  atan(S%d0)
    IF (S%nderiv >= 1) d1f = ONE/(ONE+S%d0**2)
    IF (S%nderiv >= 2) d2f = -TWO*S%d0 * d1f**2
    IF (S%nderiv >= 3) d3f = (-TWO+SIX*S%d0**2) * d1f**3

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_ATAN_dnS

  ELEMENTAL FUNCTION AD_get_ATAN2_dnS(Sy,Sx) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: Sy,Sx

    character (len=*), parameter :: name_sub='AD_get_ATAN2_dnS'

    ! the derivatives of atan2(y,x) are identical to the atan(y/x) ones
    Sres = atan(Sy/Sx)

    Sres%d0 =  atan2(Sy%d0,Sx%d0)

  END FUNCTION AD_get_ATAN2_dnS

  ELEMENTAL FUNCTION AD_get_COSH_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S


    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_COSH_dnS'



    d0f =  cosh(S%d0)
    IF (S%nderiv >= 1) d1f =  sinh(S%d0)
    IF (S%nderiv >= 2) d2f =  cosh(S%d0)
    IF (S%nderiv >= 3) d3f =  sinh(S%d0)

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_COSH_dnS
  ELEMENTAL FUNCTION AD_get_ACOSH_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S


    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_ACOSH_dnS'


    d0f = acosh(S%d0)
    IF (S%nderiv >= 1) d1f = ONE/sqrt(-ONE+S%d0**2)
    IF (S%nderiv >= 2) d2f = -S%d0*d1f**3
    IF (S%nderiv >= 3) d3f = (ONE+TWO*S%d0**2)*d1f**5

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_ACOSH_dnS
  ELEMENTAL FUNCTION AD_get_SINH_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_SINH_dnS'

    d0f =  sinh(S%d0)
    IF (S%nderiv >= 1) d1f =  cosh(S%d0)
    IF (S%nderiv >= 2) d2f =  sinh(S%d0)
    IF (S%nderiv >= 3) d3f =  cosh(S%d0)

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_SINH_dnS
  ELEMENTAL FUNCTION AD_get_ASINH_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_ASINH_dnS'


    d0f =  asinh(S%d0)
    IF (S%nderiv >= 1) d1f = ONE/sqrt(ONE+S%d0**2)
    IF (S%nderiv >= 2) d2f = -S%d0*d1f**3
    IF (S%nderiv >= 3) d3f = (-ONE+TWO*S%d0**2)*d1f**5

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_ASINH_dnS

  ELEMENTAL FUNCTION AD_get_TANH_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_TANH_dnS'


    d0f =  tanh(S%d0)
    IF (S%nderiv >= 1) d1f =  ONE/cosh(S%d0)**2
    IF (S%nderiv >= 2) d2f = -TWO*tanh(S%d0) * d1f
    IF (S%nderiv >= 3) d3f = (-FOUR+TWO*cosh(TWO*S%d0)) * d1f**2

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_TANH_dnS

  ELEMENTAL FUNCTION AD_get_ATANH_dnS(S) RESULT(Sres)
    USE QDUtil_m

    TYPE (dnS_t)                       :: Sres
    TYPE (dnS_t),        intent(in)    :: S

    integer :: err_dnS_loc
    real(kind=Rkind) :: d0f,d1f,d2f,d3f
    character (len=*), parameter :: name_sub='AD_get_ATANH_dnS'


    d0f =  atanh(S%d0)
    IF (S%nderiv >= 1) d1f =  ONE/(ONE-S%d0**2)
    IF (S%nderiv >= 2) d2f =  TWO*S%d0 * d1f**2
    IF (S%nderiv >= 3) d3f =  (TWO+SIX*S%d0**2) * d1f**3

    Sres = AD_get_F_dnS(S,d0f,d1f,d2f,d3f)

  END FUNCTION AD_get_ATANH_dnS
  ELEMENTAL FUNCTION AD_get_MOD_dnS(S,R) RESULT(Sres)
  USE QDUtil_m

  TYPE (dnS_t)                       :: Sres
  TYPE (dnS_t),        intent(in)    :: S
  real (kind=Rkind),   intent(in)    :: R

  integer :: err_dnS_loc
  real(kind=Rkind) :: d0f,d1f,d2f,d3f
  character (len=*), parameter :: name_sub='AD_get_MOD_dnS'

  Sres    = S
  Sres%d0 = mod(Sres%d0,R)

END FUNCTION AD_get_MOD_dnS
ELEMENTAL FUNCTION AD_get_MODULO_dnS(S,R) RESULT(Sres)
USE QDUtil_m

TYPE (dnS_t)                       :: Sres
TYPE (dnS_t),        intent(in)    :: S
real (kind=Rkind),   intent(in)    :: R

integer :: err_dnS_loc
real(kind=Rkind) :: d0f,d1f,d2f,d3f
character (len=*), parameter :: name_sub='AD_get_MODULO_dnS'

Sres    = S
Sres%d0 = modulo(Sres%d0,R)

END FUNCTION AD_get_MODULO_dnS
END MODULE ADdnSVM_dnS_m