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
!> @brief Module which deals with derivatives of a vector.
!!
!! This module deals with operations or functions of a vector function and its derivatives, dnVec.
!!
!! There is a mapping between the vector function V, its derivatives and the dnV derived type components:
!!
!! @li V(:,:)                   => V%d0(:,:)
!! @li dV(:,:)/dQ_i             => V%d1(:,:,i)
!! @li d^2V(:,:)/dQ_idQ_j       => V%d2(:,:,i,j)
!! @li d^3V(:,:)/dQ_idQ_jdQ_k   => V%d3(:,:,i,j,k)
!!
!! with M defined as:
!!  TYPE (dnVec_t) :: V
!!
!!
!! Some standard fortran operators (= + - * / **) are overloaded:
!!
!! For instance the sum (+) of two dnVec variables, V1 and V2 correspond to:
!! @li (V1+V2)                 => V1%d0    + V2%d0
!! @li d(V1+V2)/dQ_i           => V1%d1(:,:,i) + V2%d1(:,:,i)
!! @li ....
!!
!!
!! @author David Lauvergnat
!! @date 23/04/2023
!!
MODULE ADdnSVM_dnVec_m
  USE QDUtil_m, ONLY : Rkind
  IMPLICIT NONE
  PRIVATE

  TYPE dnVec_t
     integer                        :: nderiv = -1

     real (kind=Rkind), allocatable :: d0(:)
     real (kind=Rkind), allocatable :: d1(:,:)
     real (kind=Rkind), allocatable :: d2(:,:,:)
     real (kind=Rkind), allocatable :: d3(:,:,:,:)

  CONTAINS
    PROCEDURE, PRIVATE :: AD_set_dnVec_TO_R
    PROCEDURE, PRIVATE :: AD_VecOFdnS_TO_dnVec
    PROCEDURE, PRIVATE, PASS(vec) :: AD_dnVec_TO_VecOFdnS
    PROCEDURE, PRIVATE :: AD_set_dnVec_TO_VecOfR
    GENERIC,   PUBLIC  :: assignment(=) => AD_set_dnVec_TO_R,                  &
              AD_dnVec_TO_VecOFdnS, AD_VecOFdnS_TO_dnVec, AD_set_dnVec_TO_VecOfR
  END TYPE dnVec_t

  PUBLIC :: Variable_dnVec,dnVec_t,alloc_dnVec,dealloc_dnVec,Write_dnVec
  PUBLIC :: Check_NotAlloc_dnVec,Check_Alloc_dnVec,CheckInit

  PUBLIC :: operator (*),operator (/),operator (**),operator (+),operator (-)
  PUBLIC :: subvector_dnVec2_TO_dnVec1,dnVec2_TO_subvector_dnVec1,dnS_TO_dnVec,dnVec_TO_dnS
  PUBLIC :: Vec_wADDTO_dnVec2_ider
  PUBLIC :: Check_dnVec_IS_ZERO,get_maxval_OF_dnVec
  PUBLIC :: get_nderiv,get_nVar,get_size
  PUBLIC :: get_d0,get_d1,get_d2,get_d3,get_Flatten
  PUBLIC :: set_dnVec
  PUBLIC :: dot_product,cross_product,matmul

  INTERFACE operator (*)
    MODULE PROCEDURE AD_dnVec_TIME_R,AD_R_TIME_dnVec
    MODULE PROCEDURE AD_dnVec_TIME_dnS,AD_dnS_TIME_dnVec
  END INTERFACE
  INTERFACE operator (/)
    MODULE PROCEDURE AD_dnVec_OVER_R
    MODULE PROCEDURE AD_dnVec_OVER_dnS
  END INTERFACE
  INTERFACE operator (**)
    MODULE PROCEDURE AD_dnVec_EXP_R
  END INTERFACE
  INTERFACE operator (+)
    MODULE PROCEDURE AD_dnVec2_PLUS_dnVec1, AD_dnVec_PLUS_R, AD_R_PLUS_dnVec, &
                     AD_PLUS_dnVec, AD_dnVec_PLUS_Vec, AD_Vec_PLUS_dnVec,     &
                     AD_dnVec_PLUS_VecOFdnS, AD_VecOFdnS_PLUS_dnVec
  END INTERFACE
  INTERFACE operator (-)
    MODULE PROCEDURE AD_dnVec2_MINUS_dnVec1, AD_dnVec_MINUS_R, AD_R_MINUS_dnVec, &
                     AD_MINUS_dnVec, AD_dnVec_MINUS_Vec, AD_Vec_MINUS_dnVec,     &
                     AD_dnVec_MINUS_VecOFdnS, AD_VecOFdnS_MINUS_dnVec
  END INTERFACE

  INTERFACE Variable_dnVec
    MODULE PROCEDURE AD_init_dnVec
  END INTERFACE

  INTERFACE alloc_dnVec
    MODULE PROCEDURE AD_alloc_dnVec
  END INTERFACE

  INTERFACE dealloc_dnVec
    MODULE PROCEDURE AD_dealloc_dnVec
  END INTERFACE
  INTERFACE set_dnVec
    MODULE PROCEDURE AD_set_dnVec
  END INTERFACE
  INTERFACE Write_dnVec
    MODULE PROCEDURE AD_Write_dnVec
  END INTERFACE


  INTERFACE subvector_dnVec2_TO_dnVec1
    MODULE PROCEDURE AD_Reduced_dnVec2_TO_dnVec1
  END INTERFACE
  INTERFACE dnVec2_TO_subvector_dnVec1
    MODULE PROCEDURE AD_dnVec2_TO_Reduced_dnVec1
  END INTERFACE
  INTERFACE dnS_TO_dnVec
    MODULE PROCEDURE AD_dnS_TO_dnVec
  END INTERFACE
  INTERFACE dnVec_TO_dnS
    MODULE PROCEDURE AD_dnVec_TO_dnS
  END INTERFACE
  INTERFACE Vec_wADDTO_dnVec2_ider
    MODULE PROCEDURE AD_Vec_wADDTO_dnVec2_ider
  END INTERFACE



  INTERFACE Check_dnVec_IS_ZERO
    MODULE PROCEDURE AD_Check_dnVec_IS_ZERO
  END INTERFACE
  INTERFACE get_maxval_OF_dnVec
    MODULE PROCEDURE AD_get_maxval_OF_dnVec
  END INTERFACE
  INTERFACE Check_NotAlloc_dnVec
    MODULE PROCEDURE AD_Check_NotAlloc_dnVec
  END INTERFACE
  INTERFACE Check_Alloc_dnVec
    MODULE PROCEDURE AD_Check_Alloc_dnVec
  END INTERFACE
  INTERFACE CheckInit
    MODULE PROCEDURE AD_CheckInit_dnVec1_dnVec2
  END INTERFACE
  
  INTERFACE get_nderiv
    MODULE PROCEDURE AD_get_nderiv_FROM_dnVec
  END INTERFACE
  INTERFACE get_nVar
    MODULE PROCEDURE AD_get_nVar_FROM_dnVec
  END INTERFACE
  INTERFACE get_size
    MODULE PROCEDURE AD_get_Size_FROM_dnVec
  END INTERFACE
  
  INTERFACE get_d0
     MODULE PROCEDURE AD_get_d0_FROM_dnVec
  END INTERFACE
  INTERFACE get_d1
     MODULE PROCEDURE AD_get_d1_FROM_dnVec
  END INTERFACE
  INTERFACE get_d2
     MODULE PROCEDURE AD_get_d2_FROM_dnVec
  END INTERFACE
  INTERFACE get_d3
     MODULE PROCEDURE AD_get_d3_FROM_dnVec
  END INTERFACE

  INTERFACE get_Flatten
     MODULE PROCEDURE AD_get_Flatten_dnVec
  END INTERFACE
  INTERFACE dot_product
     MODULE PROCEDURE AD_dot_product_dnVec,AD_dot_product_dnVec_Vec,AD_dot_product_Vec_dnVec
  END INTERFACE
  INTERFACE matmul
    MODULE PROCEDURE AD_matmul_Mat_dnVec,AD_matmul_dnVec_Mat
  END INTERFACE
  INTERFACE cross_product
    MODULE PROCEDURE AD_cross_product_dnVec,AD_cross_product_Vec_dnVec,AD_cross_product_dnVec_vec
  END INTERFACE
  CONTAINS
!> @brief Public subroutine which allocates a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 21/06/2018
!!
!! @param Vec                TYPE (dnVec_t):        derived type which deals with the derivatives of a vector.
!! @param SizeVec            integer (optional):    size of the vector.
!! @param nVar               integer (optional):    number of coordinates (for the derivatives).
!! @param nderiv             integer (optional):    it enables to chose the derivative order (from 0 to 2).
!! @param err_dnVec          integer (optional):    to handle the errors errors (0: no error).
!! @param name_var           character (optional):  Name of the variable from the calling subroutine (debuging purpose).
!! @param name_sub           character (optional):  Name of the calling subroutine (debuging purpose).
  SUBROUTINE AD_alloc_dnVec(vec,SizeVec,nVar,nderiv,name_var,name_sub,err_dnVec)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnVec_t),    intent(inout)         :: vec   !< derived type, which contains, vector potential, its derivatives
    integer,           intent(in),  optional :: SizeVec !< size of the vector
    integer,           intent(in),  optional :: nVar  !< number of coordinates (for the derivatives)
    integer,           intent(in),  optional :: nderiv  !< order of the derivatives [0,1,2]
    character (len=*), intent(in),  optional :: name_var,name_sub
    integer,           intent(out), optional :: err_dnVec  !< to handle the errors

    ! local variables
    integer :: SizeVec_loc,nVar_loc,err_dnVec_loc,nderiv_loc



    err_dnVec_loc = 0 ! no error
    IF (present(err_dnVec)) err_dnVec = 0

    CALL AD_dealloc_dnVec(vec,err_dnVec_loc)
    IF (err_dnVec_loc /= 0) THEN
      write(out_unit,*) ' ERROR in AD_alloc_dnVec'
      write(out_unit,*) ' Problem in AD_dealloc_dnVec CALL in AD_alloc_dnVec'
      IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
      IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
      IF (present(err_dnVec)) THEN
        err_dnVec = err_dnVec_loc
        RETURN
      ELSE
        STOP 'Problem in AD_dealloc_dnVec CALL in AD_alloc_dnVec'
      END IF
    END IF
    !write(out_unit,*) 'err_dnVec_loc (dealloc)',err_dnVec_loc

    ! test SizeVec
    IF (present(SizeVec)) THEN
      SizeVec_loc = SizeVec
    ELSE
      SizeVec_loc = 1
    END IF

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
    vec%nderiv = nderiv_loc

    !write(out_unit,*) 'vec%nderiv in alloc_dnVec',vec%nderiv

    allocate(vec%d0(SizeVec_loc),stat=err_dnVec_loc)
    IF (err_dnVec_loc /= 0 .OR. SizeVec_loc < 1) THEN
      write(out_unit,*) ' ERROR in AD_alloc_dnVec'
      write(out_unit,*) '  Problem with allocate of vec%d0'
      write(out_unit,*) '  SizeVec > 0?',SizeVec_loc
      IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
      IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
      IF (present(err_dnVec)) THEN
        err_dnVec = err_dnVec_loc
        RETURN
      ELSE
        STOP 'Problem with allocate in AD_alloc_dnVec'
      END IF
    END IF
    !write(out_unit,*) 'err_dnVec_loc (d0)',err_dnVec_loc

    IF (nderiv_loc >= 1) THEN
      allocate(vec%d1(SizeVec_loc,nVar_loc),stat=err_dnVec_loc)
      IF (err_dnVec_loc /= 0 .OR. SizeVec_loc < 1 .OR. nVar_loc < 1) THEN
        write(out_unit,*) ' ERROR in AD_alloc_dnVec'
        write(out_unit,*) '  Problem with allocate of vec%d1'
        write(out_unit,*) '  SizeVec > 0?',SizeVec_loc
        write(out_unit,*) '  nVar > 0?',nVar_loc
        IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
        IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
        IF (present(err_dnVec)) THEN
          err_dnVec = err_dnVec_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnVec'
        END IF
      END IF
    END IF
    !write(out_unit,*) 'err_dnVec_loc (d1)',err_dnVec_loc

    IF (nderiv_loc >= 2) THEN
      allocate(vec%d2(SizeVec_loc,nVar_loc,nVar_loc),stat=err_dnVec_loc)
      IF (err_dnVec_loc /= 0 .OR. SizeVec_loc < 1 .OR. nVar_loc < 1) THEN
        write(out_unit,*) ' ERROR in AD_alloc_dnVec'
        write(out_unit,*) '  Problem with allocate of vec%d2'
        write(out_unit,*) '  SizeVec > 0',SizeVec_loc
        write(out_unit,*) '  nVar > 0',nVar_loc
        IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
        IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
        IF (present(err_dnVec)) THEN
          err_dnVec = err_dnVec_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnVec'
        END IF
      END IF
    END IF
    !write(out_unit,*) 'err_dnVec_loc (d2)',err_dnVec_loc

    IF (nderiv_loc >= 3) THEN
      allocate(vec%d3(SizeVec_loc,nVar_loc,nVar_loc,nVar_loc),stat=err_dnVec_loc)
      IF (err_dnVec_loc /= 0 .OR. SizeVec_loc < 1 .OR. nVar_loc < 1) THEN
        write(out_unit,*) ' ERROR in AD_alloc_dnVec'
        write(out_unit,*) '  Problem with allocate of vec%d2'
        write(out_unit,*) '  SizeVec > 0',SizeVec_loc
        write(out_unit,*) '  nVar > 0',nVar_loc
        IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
        IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
        IF (present(err_dnVec)) THEN
          err_dnVec = err_dnVec_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnVec'
        END IF
      END IF
    END IF
    !write(out_unit,*) 'err_dnVec_loc (d3)',err_dnVec_loc
    !IF (present(err_dnVec)) write(out_unit,*) 'err_dnVec',err_dnVec

  END SUBROUTINE AD_alloc_dnVec
!> @brief Public subroutine which deallocates a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 21/06/2018
!!
!! @param vec                TYPE (dnVec_t):     derived type which deals with the derivatives of a vector.
!! @param err_dnVec       integer (optional):    to handle the errors errors (0: no error).
  SUBROUTINE AD_dealloc_dnVec(vec,err_dnVec)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnVec_t), intent(inout)         :: vec        !< derived type, which contains, vector potential, its derivatives
    integer,        intent(out), optional :: err_dnVec  !< to handle the errors

    ! local variables
    integer :: err_dnVec_loc

    err_dnVec_loc = 0
    IF (present(err_dnVec)) err_dnVec = 0

    IF (allocated(vec%d0)) THEN
      deallocate(vec%d0,stat=err_dnVec_loc)
      IF (err_dnVec_loc /= 0) THEN
        write(out_unit,*) ' ERROR in dealloc_dnVec'
        write(out_unit,*) '  Problem with deallocate of vec%d0'
        IF (present(err_dnVec)) THEN
          err_dnVec = err_dnVec_loc
          RETURN
        ELSE
          STOP 'Problem with deallocate in dealloc_dnVec'
        END IF
      END IF
    END IF

    IF (allocated(vec%d1)) THEN
      deallocate(vec%d1,stat=err_dnVec_loc)
      IF (err_dnVec_loc /= 0) THEN
        write(out_unit,*) ' ERROR in dealloc_dnVec'
        write(out_unit,*) '  Problem with deallocate of vec%d1'
        IF (present(err_dnVec)) THEN
          err_dnVec = err_dnVec_loc
          RETURN
        ELSE
          STOP 'Problem with deallocate in dealloc_dnVec'
        END IF
      END IF
    END IF

    IF (allocated(vec%d2)) THEN
      deallocate(vec%d2,stat=err_dnVec_loc)
      IF (err_dnVec_loc /= 0) THEN
        write(out_unit,*) ' ERROR in dealloc_dnVec'
        write(out_unit,*) '  Problem with deallocate of vec%d2'
        IF (present(err_dnVec)) THEN
          err_dnVec = err_dnVec_loc
          RETURN
        ELSE
          STOP 'Problem with deallocate in dealloc_dnVec'
        END IF
      END IF
    END IF

    IF (allocated(vec%d3)) THEN
      deallocate(vec%d3,stat=err_dnVec_loc)
      IF (err_dnVec_loc /= 0) THEN
        write(out_unit,*) ' ERROR in dealloc_dnVec'
        write(out_unit,*) '  Problem with deallocate of vec%d3'
        IF (present(err_dnVec)) THEN
          err_dnVec = err_dnVec_loc
          RETURN
        ELSE
          STOP 'Problem with deallocate in dealloc_dnVec'
        END IF
      END IF
    END IF

    vec%nderiv = -1

  END SUBROUTINE AD_dealloc_dnVec

  FUNCTION AD_init_dnVec(TabVal,nVar,nderiv,iVar) RESULT(Vec)
    USE QDUtil_m, ONLY : Rkind, ZERO, ONE, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                   :: Vec

    real (kind=Rkind), intent(in)    :: TabVal(:)
    integer, optional, intent(in)    :: nderiv,nVar
    integer, optional, intent(in)    :: iVar(:)

    integer :: err_dnVec
    character (len=*), parameter :: name_sub='AD_init_dnVec'

    integer :: nVar_loc,nderiv_loc,iV,nVec

    nVec = size(TabVal)
  
    IF (present(nVar)) THEN
      IF (nVar < nVec) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' nVar is present and < size(TabVal)'
        write(out_unit,*) ' nVar        ',nVar
        write(out_unit,*) ' size(TabVal)',nVec
        write(out_unit,*) ' Check your Fortran or your data'
        STOP 'ERROR in AD_init_dnVec: nVar is present and < size(TabVal) '
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
        STOP 'ERROR in AD_init_dnVec: size(iVar) /= size(TabVal) '
      END IF
    END IF

    CALL alloc_dnVec(Vec,nVec,nVar_loc,nderiv_loc,'vec',name_sub,err_dnVec)
    IF (err_dnVec /= 0) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' Problem with the allocation in "alloc_dnVec"'
      write(out_unit,*) ' nVec,nVar_loc,nderiv_loc',nVec,nVar_loc,nderiv_loc
      STOP 'ERROR in AD_init_dnVec: Problem with the allocation in "alloc_dnVec"'
    END IF

    CALL AD_set_dnVec_TO_R(Vec,ZERO)
    Vec%d0(:) = TabVal
    IF (nderiv_loc > 0) THEN
      IF (present(iVar)) THEN
        DO iV=1,nVec
          Vec%d1(iV,iVar(iV)) = ONE
        END DO
      ELSE
        DO iV=1,nVec
          Vec%d1(iV,iV) = ONE
        END DO
      END IF
    END IF

  END FUNCTION 
  SUBROUTINE AD_set_dnVec(Vec,d0,d1,d2,d3)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    real (kind=Rkind), optional,   intent(in)     :: d0(:)
    real (kind=Rkind), optional,   intent(in)     :: d1(:,:)
    real (kind=Rkind), optional,   intent(in)     :: d2(:,:,:)
    real (kind=Rkind), optional,   intent(in)     :: d3(:,:,:,:)

    TYPE (dnVec_t),                intent(inout)  :: Vec


    character (len=*), parameter :: name_sub='AD_set_dnS'

    IF (present(d0)) THEN
      Vec%d0 = d0
      Vec%nderiv = 0
    END IF

    IF (present(d1)) THEN
      Vec%d1     = d1
      Vec%nderiv = 1
      IF (.NOT. present(d0)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d1 is present but not d0'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnS'
      END IF
    END IF

    IF (present(d2)) THEN
      Vec%d2     = d2
      Vec%nderiv = 2
      IF (.NOT. present(d1)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d2 is present but not d1'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnS'
      END IF
    END IF

    IF (present(d3)) THEN
      Vec%d3     = d3
      Vec%nderiv = 3
      IF (.NOT. present(d2)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d3 is present but not d2'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnS'
      END IF
    END IF

  END SUBROUTINE AD_set_dnVec

  !> @brief Public subroutine which copies two "dnVec" derived types.
!!
!> @author David Lauvergnat
!! @date 21/06/2018
!!
!! @param dnVec1                TYPE (dnVec_t):     derived type which deals with the derivatives of a vector.
!! @param dnVec2                TYPE (dnVec_t):     derived type which deals with the derivatives of a vector.
  SUBROUTINE AD_dnVec2_TO_dnVec1(dnVec1,dnVec2)
    USE QDUtil_m
    IMPLICIT NONE

    CLASS (dnVec_t), intent(inout) :: dnVec1
    CLASS (dnVec_t), intent(in)    :: dnVec2

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec2_TO_dnVec1'

    nderiv_loc   = AD_get_nderiv_FROM_dnVec(dnVec2)
    SizeVec_loc  = AD_get_Size_FROM_dnVec(dnVec2)
    nVar_loc     = AD_get_nVar_FROM_dnVec(dnVec2)

    !write(out_unit,*) 'in ',name_sub,' nVar,SizeVec,nderiv',nVar_loc,SizeVec_loc,nderiv_loc


    IF (nderiv_loc < 0 .OR. SizeVec_loc < 1 .OR. (nderiv_loc > 0  .AND. nVar_loc < 1)) RETURN


    CALL AD_alloc_dnVec(dnVec1,SizeVec_loc,nVar_loc,nderiv_loc,name_var='dnVec1',name_sub=name_sub)


    IF (nderiv_loc == 0) THEN
       dnVec1%d0 = dnVec2%d0
    ELSE IF (nderiv_loc == 1) THEN
       dnVec1%d0 = dnVec2%d0
       dnVec1%d1 = dnVec2%d1
    ELSE IF (nderiv_loc == 2) THEN
       dnVec1%d0 = dnVec2%d0
       dnVec1%d1 = dnVec2%d1
       dnVec1%d2 = dnVec2%d2
    ELSE IF (nderiv_loc == 3) THEN
       dnVec1%d0 = dnVec2%d0
       dnVec1%d1 = dnVec2%d1
       dnVec1%d2 = dnVec2%d2
       dnVec1%d3 = dnVec2%d3
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END SUBROUTINE AD_dnVec2_TO_dnVec1
  SUBROUTINE AD_Reduced_dnVec2_TO_dnVec1(dnVec1,dnVec2,lb,ub)
    USE QDUtil_m
    IMPLICIT NONE

    CLASS (dnVec_t),  intent(inout) :: dnVec1
    CLASS (dnVec_t),  intent(in)    :: dnVec2
    integer,          intent(in)    :: lb,ub

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_Reduced_dnVec2_TO_dnVec1'

    nderiv_loc  = AD_get_nderiv_FROM_dnVec(dnVec2)
    SizeVec_loc = AD_get_Size_FROM_dnVec(dnVec2)
    nVar_loc    = AD_get_nVar_FROM_dnVec(dnVec2)

    !write(out_unit,*) 'in ',name_sub,' nVar,SizeVec,nderiv',nVar_loc,SizeVec_loc,nderiv_loc


    IF (nderiv_loc < 0 .OR. SizeVec_loc < 1 .OR. (nderiv_loc > 0  .AND. nVar_loc < 1)) RETURN

    IF (lb < 1 .OR. ub > SizeVec_loc .OR. lb > ub) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The indexes lb and ub are wrong.'
      write(out_unit,*) 'lb,ub',lb,ub
      write(out_unit,*) 'The range is [1...',SizeVec_loc,']'
      write(out_unit,*) 'It should never append! Check the source'
      STOP 'ERROR in AD_Reduced_dnVec2_TO_dnVec1: lb or ub are wrong'
    END IF

    CALL AD_alloc_dnVec(dnVec1,ub-lb+1,nVar_loc,nderiv_loc,name_var='dnVec1',name_sub=name_sub)


    IF (nderiv_loc == 0) THEN
       dnVec1%d0 = dnVec2%d0(lb:ub)
    ELSE IF (nderiv_loc == 1) THEN
       dnVec1%d0 = dnVec2%d0(lb:ub)
       dnVec1%d1 = dnVec2%d1(lb:ub,:)
    ELSE IF (nderiv_loc == 2) THEN
       dnVec1%d0 = dnVec2%d0(lb:ub)
       dnVec1%d1 = dnVec2%d1(lb:ub,:)
       dnVec1%d2 = dnVec2%d2(lb:ub,:,:)
    ELSE IF (nderiv_loc == 3) THEN
       dnVec1%d0 = dnVec2%d0(lb:ub)
       dnVec1%d1 = dnVec2%d1(lb:ub,:)
       dnVec1%d2 = dnVec2%d2(lb:ub,:,:)
       dnVec1%d3 = dnVec2%d3(lb:ub,:,:,:)
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END SUBROUTINE AD_Reduced_dnVec2_TO_dnVec1
  SUBROUTINE AD_dnVec2_TO_Reduced_dnVec1(dnVec1,dnVec2,lb,ub)
    USE QDUtil_m
    IMPLICIT NONE

    CLASS (dnVec_t),  intent(inout) :: dnVec1
    CLASS (dnVec_t),  intent(in)    :: dnVec2
    integer,          intent(in)    :: lb,ub

    integer :: nderiv,SizeVec1,SizeVec2,nVar
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec2_TO_Reduced_dnVec1'

    !write(out_unit,*) 'in ',name_sub,' nVar,SizeVec,nderiv',nVar_loc,SizeVec_loc,nderiv_loc

    IF (.NOT. Check_Alloc_dnVec(dnVec1)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' dnVec1 MUST be allocated'
      write(out_unit,*) 'It should never append! Check the source'
      STOP 'ERROR in AD_dnVec2_TO_Reduced_dnVec1: dnVec1 MUST be allocated'
    END IF
    IF (.NOT. Check_Alloc_dnVec(dnVec2)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' dnVec2 MUST be allocated'
      write(out_unit,*) 'It should never append! Check the source'
      STOP 'ERROR in AD_dnVec2_TO_Reduced_dnVec1: dnVec2 MUST be allocated'
    END IF

    SizeVec2 = get_Size(dnVec2)
    nVar     = get_nVar(dnVec2)

    SizeVec1 = get_Size(dnVec1)


    IF (lb < 1 .OR. ub > SizeVec1 .OR. lb > ub) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The indexes lb and ub are wrong.'
      write(out_unit,*) 'lb,ub',lb,ub
      write(out_unit,*) 'The range of dnVec1 is [1...',to_string(SizeVec1),']'
      write(out_unit,*) 'It should never append! Check the source'
      STOP 'ERROR in AD_dnVec2_TO_Reduced_dnVec1: lb or ub are wrong'
    END IF
    IF (ub-lb+1 /= SizeVec2) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The indexes lb and ub are wrong.'
      write(out_unit,*) 'lb,ub',lb,ub
      write(out_unit,*) 'The size of dnVec2 MUST be equal to the size of [',to_string(lb),':',to_string(ub),']'
      write(out_unit,*) 'It should never append! Check the source'
      STOP 'ERROR in AD_dnVec2_TO_Reduced_dnVec1: lb or ub are wrong'
    END IF

    IF (allocated(dnVec2%d0) .AND. allocated(dnVec2%d0) )   dnVec1%d0(lb:ub)       = dnVec2%d0
    IF (allocated(dnVec2%d1) .AND. allocated(dnVec2%d1) )   dnVec1%d1(lb:ub,:)     = dnVec2%d1
    IF (allocated(dnVec2%d2) .AND. allocated(dnVec2%d2) )   dnVec1%d2(lb:ub,:,:)   = dnVec2%d2
    IF (allocated(dnVec2%d3) .AND. allocated(dnVec2%d3) )   dnVec1%d3(lb:ub,:,:,:) = dnVec2%d3

  END SUBROUTINE AD_dnVec2_TO_Reduced_dnVec1
  !> @brief Public subroutine which copies a dnS derived type to one element of dnVec derived type.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param vec                   TYPE (dnVec_t):    derived type which deals with the derivatives of a vector.
!! @param S                     TYPE(dnS):         derived type which deals with the derivatives of a scalar.
!! @param i                     integer (optional) index of the vector element. If not present i=1
  SUBROUTINE AD_dnS_TO_dnVec(S,vec,i)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t),     intent(inout) :: vec
    TYPE (dnS_t),       intent(in)    :: S
    integer, optional,  intent(in)    :: i

    integer :: nderiv_dnVec,SizeVec_dnVec,nVar_dnVec,nderiv_dnS,nVar_dnS
    integer :: i_loc

    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnS_TO_dnVec'

    !write(out_unit,*) 'BEGINNING ',name_sub
    !CALL write_dnS(S,info='S')

    nderiv_dnS = get_nderiv(S)
    nVar_dnS   = get_nVar(S)

    nderiv_dnVec = AD_get_nderiv_FROM_dnVec(vec)
    SizeVec_dnVec  = AD_get_Size_FROM_dnVec(vec)
    nVar_dnVec   = AD_get_nVar_FROM_dnVec(vec)

    i_loc = 1
    IF (present(i)) i_loc = i
    !write(out_unit,*) 'i ',i_loc


    IF (i_loc < 1 .OR. i_loc > SizeVec_dnVec) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The vector index, (',i_loc,') is out of range [1...',SizeVec_dnVec,']'
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF

    IF (nderiv_dnS == -1) THEN
      IF (nderiv_dnVec == -1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' dnVec is not allocated.'
        write(out_unit,*) 'It should never append! Check the source.'
        STOP 'dnVec is not allocated.'
      END IF
      ! S (dnS) is a constant
      ! value
      vec%d0(i_loc) = get_d0(S)

      ! 1st order derivatives
      IF (nderiv_dnVec >= 1) vec%d1(i_loc,:) = ZERO

      ! 2d order derivatives
      IF (nderiv_dnVec >= 2) vec%d2(i_loc,:,:) = ZERO
    ELSE

      IF ( AD_check_notalloc_dnVec(vec,nderiv_dnS) .OR.                  &
           nderiv_dnS /= nderiv_dnVec  .OR.  nVar_dnS /= nVar_dnVec .OR.  &
           SizeVec_dnVec < 1 ) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' dnVec is not allocated or ...'
        write(out_unit,*) '  ... nderiv from dnVec or dnS are different or ...'
        write(out_unit,*) '  ... nVar from dnVec or dnS are different or ...'
        write(out_unit,*) '  ... SizeVec from dnVec is < 1'

        write(out_unit,*) 'nderiv from dnVec and dnS:',nderiv_dnVec,nderiv_dnS
        write(out_unit,*) 'nVar   from dnVec and dnS:',nVar_dnVec,nVar_dnS
        write(out_unit,*) 'SizeVec  from dnVec        :',SizeVec_dnVec

        write(out_unit,*) 'It should never append! Check the source'
        STOP 'dnVec is not allocated or inconsistent nVar,nderiv parameters.'
      END IF

      ! value
      vec%d0(i_loc) = get_d0(S)

      ! 1st order derivatives
      IF (nderiv_dnS >= 1) THEN
        CALL sub_get_dn(S,d1=vec%d1(i_loc,:))
        !write(out_unit,*) 'd1(:)',vec%d1(i_loc,:)
      END IF

      ! 2d order derivatives
      IF (nderiv_dnS >= 2) then
        CALL sub_get_dn(S,d2=vec%d2(i_loc,:,:))
        !write(out_unit,*) 'd2(:,:)',vec%d2(i_loc,:,:)
      END IF

      ! 3d order derivatives
      IF (nderiv_dnS >= 3) then
        CALL sub_get_dn(S,d3=vec%d3(i_loc,:,:,:))
        !write(out_unit,*) 'd3(:,:,:)',vec%d3(i_loc,:,:,:)
      END IF
      !write(out_unit,*) 'END ',name_sub

    END IF

  END SUBROUTINE AD_dnS_TO_dnVec
!> @brief Public subroutine which copies a dnS derived type to one element of dnVec derived type.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param vec                   TYPE (dnVec_t):    derived type which deals with the derivatives of a vector.
!! @param S                     TYPE(dnS):       derived type which deals with the derivatives of a scalar.
!! @param i                     integer (optional) index of the vector element. If not present i=1
  SUBROUTINE AD_dnVec_TO_dnS(vec,S,i)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t),     intent(in)    :: vec
    TYPE (dnS_t),       intent(inout) :: S
    integer, optional,  intent(in)    :: i

    integer :: nderiv_dnVec,SizeVec_dnVec,nVar_dnVec,nderiv_dnS,nVar_dnS
    integer :: i_loc,j_loc

    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec_TO_dnS'


    nderiv_dnS = get_nderiv(S)
    nVar_dnS   = get_nVar(S)

    nderiv_dnVec  = AD_get_nderiv_FROM_dnVec(vec)
    SizeVec_dnVec = AD_get_Size_FROM_dnVec(vec)
    nVar_dnVec    = AD_get_nVar_FROM_dnVec(vec)

    i_loc = 1
    IF (present(i)) i_loc = i


    IF (i_loc < 1 .OR. i_loc > SizeVec_dnVec) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The vector indexes, (',i_loc,') is out of range [1...',SizeVec_dnVec,']'
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF

    IF (nderiv_dnVec == -1) THEN
      IF (nderiv_dnS == -1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' dnS is not allocated.'
        write(out_unit,*) 'It should never append! Check the source.'
        STOP 'dnS is not allocated.'
      END IF
      ! dnVec is a constant
      S = vec%d0(i_loc)
    ELSE

      IF ( AD_check_notalloc_dnVec(vec,nderiv_dnS) .OR. SizeVec_dnVec < 1 ) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' dnVec is not allocated or ...'
        write(out_unit,*) '  ... SizeVec from dnVec is < 1'
        write(out_unit,*) 'SizeVec  from dnVec        :',SizeVec_dnVec

        write(out_unit,*) 'It should never append! Check the source'
        STOP 'dnVec is not allocated or inconsistent SizeVec parameter.'
      END IF

      SELECT CASE (nderiv_dnVec)
      CASE (1)
        CALL set_dnS(S, d0=vec%d0(i_loc),       &
                        d1=vec%d1(i_loc,:))
      CASE (2)
        CALL set_dnS(S, d0=vec%d0(i_loc),       &
                        d1=vec%d1(i_loc,:),     &
                        d2=vec%d2(i_loc,:,:))
      CASE (3)
        CALL set_dnS(S, d0=vec%d0(i_loc),       &
                        d1=vec%d1(i_loc,:),     &
                        d2=vec%d2(i_loc,:,:),   &
                        d3=vec%d3(i_loc,:,:,:))
      CASE Default ! nderiv_dnS = -1, 0
        CALL set_dnS(S, d0=vec%d0(i_loc))
      END SELECT

    END IF

  END SUBROUTINE AD_dnVec_TO_dnS
!> @brief Public subroutine which copies a dnS derived type to one element of dnVec derived type.
!!
!> @author David Lauvergnat
!! @date 30/07/2019
!!
!! @param vec                   TYPE (dnVec_t):  derived type which deals with the derivatives of a vector.
!! @param VecOFS                TYPE(dnS):       vector of derived type which deals with the derivatives of a scalar.
  SUBROUTINE AD_VecOFdnS_TO_dnVec(vec,VecOFS)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    CLASS (dnVec_t),   intent(inout) :: vec
    TYPE (dnS_t),      intent(in)    :: VecOFS(:)

    integer :: SizeVec,nVar,nderiv
    integer :: i,iv

    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_VecOFdnS_TO_dnVec'

    i = lbound(VecOFS,dim=1)
    SizeVec = size(VecOFS)
    nVar    = get_nVar(VecOFS(i))
    nderiv  = get_nderiv(VecOFS(i))

    CALL alloc_dnVec(vec,SizeVec,nVar,nderiv)

    DO i=lbound(VecOFS,dim=1),ubound(VecOFS,dim=1)
      iv = i-lbound(VecOFS,dim=1)+1
      CALL AD_dnS_TO_dnVec(VecOFS(i),vec,iv)
    END DO

  END SUBROUTINE AD_VecOFdnS_TO_dnVec
  SUBROUTINE AD_dnVec_TO_VecOFdnS(VecOFS,vec)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnS_t), allocatable,   intent(inout)  :: VecOFS(:)
    CLASS (dnVec_t),             intent(in)     :: vec

    integer :: SizeVec,nVar,nderiv
    integer :: i

    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec_TO_VecOFdnS'

    SizeVec = get_size(vec)
    nVar    = get_nVar(vec)
    nderiv  = get_nderiv(vec)

    IF (allocated(VecOFS)) THEN
      CALL dealloc_dnS(VecOFS)
      deallocate(VecOFS)
    END IF

    allocate(VecOFS(SizeVec))

    DO i=1,SizeVec
      CALL AD_dnVec_TO_dnS(Vec,VecOFS(i),i)
    END DO

  END SUBROUTINE AD_dnVec_TO_VecOFdnS
  SUBROUTINE AD_vec_wADDTO_dnVec2_ider(vec1,w1,dnVec2,ider)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    real (kind=Rkind),  intent(in)            :: vec1(:)
    TYPE (dnVec_t),     intent(inout)         :: dnVec2
    integer,            intent(in),  optional :: ider(:)
    real (kind=Rkind),  intent(in)            :: w1

    integer :: nderiv,SizeVec,nVar
    integer :: i1,i1i,i1f
    integer :: i2,i2i,i2f
    integer :: i3,i3i,i3f

    character (len=*), parameter :: name_sub='AD_vec_wADDTO_dnVec2_ider'

    nderiv  = AD_get_nderiv_FROM_dnVec(dnVec2)
    SizeVec = AD_get_Size_FROM_dnVec(dnVec2)
    nVar    = AD_get_nVar_FROM_dnVec(dnVec2)

    IF (.NOT. allocated(dnVec2%d0)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) '  dnVec2%d0 is not allocated.'
      write(out_unit,*) ' CHECK the fortran source!!'
      STOP
    END IF

    IF (.NOT. all(shape(vec1) == shape(dnVec2%d0))) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) '  The shape of vec1 dnVec2%d0 must be equal.'
      write(out_unit,*) '  shape(vec1):      ',shape(vec1)
      write(out_unit,*) '  shape(dnVec2%d0): ',shape(dnVec2%d0)
      write(out_unit,*) ' CHECK the fortran source!!'
      STOP
    END IF
    IF (present(ider)) THEN
      IF (size(ider) > nderiv) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' size(ider) cannot be > and nderiv.'
        write(out_unit,*) ' size(ider)',size(ider)
        write(out_unit,*) ' nderiv    ',nderiv
        write(out_unit,*) ' CHECK the fortran source!!'
        STOP
      END IF
      IF (any(ider < 0) .OR. any(ider > nVar)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' Some ider(:) values are out-of-range.'
        write(out_unit,*) ' ider(:)',ider
        write(out_unit,'(a,i0,a)') ' derivative range [0:',nVar,']'
        write(out_unit,*) ' CHECK the fortran source!!'
        STOP
      END IF
    END IF

    IF (present(ider)) THEN

      IF (size(ider) > 0) THEN
        IF (ider(1) == 0) THEN
          i1i = 1
          i1f = nVar
        ELSE
          i1i = ider(1)
          i1f = ider(1)
        END IF
      END IF
      IF (size(ider) > 1) THEN
        IF (ider(2) == 0) THEN
          i2i = 1
          i2f = nVar
        ELSE
          i2i = ider(2)
          i2f = ider(2)
        END IF
      END IF
      IF (size(ider) > 2) THEN
        IF (ider(3) == 0) THEN
          i3i = 1
          i3f = nVar
        ELSE
          i3i = ider(3)
          i3f = ider(3)
        END IF
      END IF


      SELECT CASE (size(ider))
      CASE (0)
        dnVec2%d0(:) = w1*vec1 + dnVec2%d0
      CASE (1)
        DO i1=i1i,i1f
          dnVec2%d1(:,i1) = w1*vec1 + dnVec2%d1(:,i1)
        END DO

      CASE (2)
        DO i2=i2i,i2f
        DO i1=i1i,i1f
          dnVec2%d2(:,i1,i2) = w1*vec1 + dnVec2%d2(:,i1,i2)
        END DO
        END DO

      CASE (3)

        !IF (present(ider)) write(6,*) 'ider',ider

        DO i3=i3i,i3f
        DO i2=i2i,i2f
        DO i1=i1i,i1f
          dnVec2%d3(:,i1,i2,i3) = w1*vec1 + dnVec2%d3(:,i1,i2,i3)
        END DO
        END DO
        END DO

      CASE Default
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' size(ider) > 3 is NOT possible.'
        write(out_unit,*) '   ider',ider
        write(out_unit,*) 'It should never append! Check the source'
        STOP
      END SELECT
    ELSE
      dnVec2%d0(:) = w1*vec1 + dnVec2%d0
    END IF

  END SUBROUTINE AD_vec_wADDTO_dnVec2_ider

!> @brief Public function which calculate set dnVec to zero (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                   TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param set_dnVec_TO_zero  TYPE (dnVec_t) (result):  dnVec derived type
  SUBROUTINE AD_set_dnVec_TO_zero(dnVec)
    USE QDUtil_m, ONLY : ZERO, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t), intent(inout) :: dnVec

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_set_dnVec_TO_zero'


    CALL AD_set_dnVec_TO_R(dnVec,ZERO)

  END SUBROUTINE AD_set_dnVec_TO_zero
!> @brief Public function which calculate set dnVec to R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param R                  real:                     some real number
!! @param set_dnVec_TO_R  TYPE (dnVec_t) (result):  dnVec derived type
  SUBROUTINE AD_set_dnVec_TO_R(dnVec,R)
    USE QDUtil_m, ONLY : Rkind, ZERO, out_unit
    IMPLICIT NONE

    CLASS (dnVec_t), intent(inout) :: dnVec
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_set_dnVec_TO_R'

    nderiv_loc = AD_get_nderiv_FROM_dnVec(dnVec)

    !write(out_unit,*) 'nderiv',nderiv_loc


    IF (nderiv_loc == 0) THEN
       dnVec%d0 = R
    ELSE IF (nderiv_loc == 1) THEN
       dnVec%d0 = R
       dnVec%d1 = ZERO
    ELSE IF (nderiv_loc == 2) THEN
       dnVec%d0 = R
       dnVec%d1 = ZERO
       dnVec%d2 = ZERO
    ELSE IF (nderiv_loc == 3) THEN
       dnVec%d0 = R
       dnVec%d1 = ZERO
       dnVec%d2 = ZERO
       dnVec%d3 = ZERO
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 or nderiv < 0 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END SUBROUTINE AD_set_dnVec_TO_R
  SUBROUTINE AD_set_dnVec_TO_VecOfR(dnVec,Vec)
    USE QDUtil_m, ONLY : Rkind, ZERO, out_unit
    IMPLICIT NONE

    CLASS (dnVec_t),   intent(inout) :: dnVec
    real (kind=Rkind), intent(in)    :: Vec(:)

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_set_dnVec_TO_VecOfR'

    IF (.NOT. Check_Alloc_dnVec(dnVec)) THEN
      dnVec%d0     = Vec
      dnVec%nderiv = 0
      RETURN
    END IF

    nderiv_loc = AD_get_nderiv_FROM_dnVec(dnVec)
    !write(out_unit,*) 'nderiv',nderiv_loc

    IF (size(dnVec%d0) /= size(Vec)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' Sizes of dnVec and Vec differ'
      write(out_unit,*) ' size(dnVec%d0), size(Vec)',size(dnVec%d0),size(Vec)
      write(out_unit,*) '  Check the source'
      STOP
    END IF

    IF (nderiv_loc == 0) THEN
       dnVec%d0 = Vec
    ELSE IF (nderiv_loc == 1) THEN
       dnVec%d0 = Vec
       dnVec%d1 = ZERO
    ELSE IF (nderiv_loc == 2) THEN
       dnVec%d0 = Vec
       dnVec%d1 = ZERO
       dnVec%d2 = ZERO
    ELSE IF (nderiv_loc == 3) THEN
       dnVec%d0 = Vec
       dnVec%d1 = ZERO
       dnVec%d2 = ZERO
       dnVec%d3 = ZERO
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 or nderiv < 0 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END SUBROUTINE AD_set_dnVec_TO_VecOfR
  !> @brief Public function which calculate dnVec*R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param R                  real:                     some real number
!! @param sub_dnVec_TIME_R TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_dnVec_TIME_R(dnVec,R) RESULT (sub_dnVec_TIME_R)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: sub_dnVec_TIME_R
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec_TIME_R'

    nderiv_loc  = AD_get_nderiv_FROM_dnVec(dnVec)
    SizeVec_loc = AD_get_Size_FROM_dnVec(dnVec)
    nVar_loc    = AD_get_nVar_FROM_dnVec(dnVec)

    !write(out_unit,*) 'nVar,SizeVec,nderiv',nVar_loc,SizeVec_loc,nderiv_loc

    CALL AD_alloc_dnVec(sub_dnVec_TIME_R,SizeVec_loc,nVar_loc,nderiv_loc,&
                         name_var='sub_dnVec_TIME_R',name_sub=name_sub)

    !write(out_unit,*) 'nderiv',nderiv_loc


    IF (nderiv_loc == 0) THEN
       sub_dnVec_TIME_R%d0 = dnVec%d0 * R

    ELSE IF (nderiv_loc == 1) THEN
       sub_dnVec_TIME_R%d0 = dnVec%d0 * R
       sub_dnVec_TIME_R%d1 = dnVec%d1 * R

    ELSE IF (nderiv_loc == 2) THEN
       sub_dnVec_TIME_R%d0 = dnVec%d0 * R
       sub_dnVec_TIME_R%d1 = dnVec%d1 * R
       sub_dnVec_TIME_R%d2 = dnVec%d2 * R
    ELSE IF (nderiv_loc == 3) THEN
       sub_dnVec_TIME_R%d0 = dnVec%d0 * R
       sub_dnVec_TIME_R%d1 = dnVec%d1 * R
       sub_dnVec_TIME_R%d2 = dnVec%d2 * R
       sub_dnVec_TIME_R%d3 = dnVec%d3 * R
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_dnVec_TIME_R
!> @brief Public function which calculate R*dnVec (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param R                  real:                     some real number
!! @param sub_R_TIME_dnVec TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_R_TIME_dnVec(R,dnVec)  RESULT(sub_R_TIME_dnVec)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                :: sub_R_TIME_dnVec
    TYPE (dnVec_t),   intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_R_TIME_dnVec'

    nderiv_loc  = AD_get_nderiv_FROM_dnVec(dnVec)
    SizeVec_loc = AD_get_Size_FROM_dnVec(dnVec)
    nVar_loc    = AD_get_nVar_FROM_dnVec(dnVec)

    !write(out_unit,*) 'nVar,SizeVec,nderiv',nVar_loc,SizeVec_loc,nderiv_loc

    CALL AD_alloc_dnVec(sub_R_TIME_dnVec,SizeVec_loc,nVar_loc,nderiv_loc,&
                         name_var='sub_R_TIME_dnVec',name_sub=name_sub)

    !write(out_unit,*) 'nderiv',nderiv_loc


    IF (nderiv_loc == 0) THEN
       sub_R_TIME_dnVec%d0 = dnVec%d0 * R

    ELSE IF (nderiv_loc == 1) THEN
       sub_R_TIME_dnVec%d0 = dnVec%d0 * R
       sub_R_TIME_dnVec%d1 = dnVec%d1 * R

    ELSE IF (nderiv_loc == 2) THEN
       sub_R_TIME_dnVec%d0 = dnVec%d0 * R
       sub_R_TIME_dnVec%d1 = dnVec%d1 * R
       sub_R_TIME_dnVec%d2 = dnVec%d2 * R
    ELSE IF (nderiv_loc == 3) THEN
       sub_R_TIME_dnVec%d0 = dnVec%d0 * R
       sub_R_TIME_dnVec%d1 = dnVec%d1 * R
       sub_R_TIME_dnVec%d2 = dnVec%d2 * R
       sub_R_TIME_dnVec%d3 = dnVec%d3 * R
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_R_TIME_dnVec
  FUNCTION AD_dnVec_TIME_dnS(dnVec,dnS) RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    TYPE (dnS_t),      intent(in)  :: dnS

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    integer :: i,j,k
    real (kind=Rkind)              :: d0
    real (kind=Rkind), allocatable :: d1(:)
    real (kind=Rkind), allocatable :: d2(:,:)
    real (kind=Rkind), allocatable :: d3(:,:,:)
    character (len=*), parameter :: name_sub='AD_dnVec_TIME_dnS'

    nderiv_loc  = min(get_nderiv(dnVec),get_nderiv(dnS))
    SizeVec_loc = get_Size(dnVec)
    nVar_loc    = get_nVar(dnVec)
    IF (nVar_loc /= get_nVar(dnS)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nVar of dnS and dnVec are different'
      write(out_unit,*) ' nVar of dnS and dnVec: ',get_nVar(dnS),get_nVar(dnVec)
      STOP 'ERROR in AD_dnVec_TIME_dnS: nVar of dnS and dnVec are different'
    END IF

    !write(out_unit,*) 'nVar,SizeVec,nderiv',nVar_loc,SizeVec_loc,nderiv_loc

    CALL AD_alloc_dnVec(dnVecRes,SizeVec_loc,nVar_loc,nderiv_loc,&
                        name_var='dnVecRes',name_sub=name_sub)

    !write(out_unit,*) 'nderiv',nderiv_loc


    IF (nderiv_loc == 0) THEN
      dnVecRes%d0(:) = dnVec%d0(:) * get_d0(dnS)

    ELSE IF (nderiv_loc == 1) THEN
      d0 = get_d0(dnS)
      d1 = get_d1(dnS)

      dnVecRes%d0(:) = dnVec%d0(:) * d0
      DO i=1,nVar_loc
        dnVecRes%d1(:,i) = dnVec%d1(:,i)*d0 + dnVec%d0(:) * d1(i)
      END DO
    ELSE IF (nderiv_loc == 2) THEN
      d0 = get_d0(dnS)
      d1 = get_d1(dnS)
      d2 = get_d2(dnS)

      dnVecRes%d0(:) = dnVec%d0(:) * d0
      DO i=1,nVar_loc
        dnVecRes%d1(:,i) = dnVec%d1(:,i)*d0 + dnVec%d0(:) * d1(i)
      END DO
      DO i=1,nVar_loc
      DO j=1,nVar_loc
        dnVecRes%d2(:,j,i) = dnVec%d2(:,j,i)*d0 + dnVec%d1(:,i)*d1(j) + dnVec%d1(:,j)*d1(i) + dnVec%d0(:) * d2(j,i)
      END DO
      END DO

    ELSE IF (nderiv_loc == 3) THEN
      d0 = get_d0(dnS)
      d1 = get_d1(dnS)
      d2 = get_d2(dnS)
      d3 = get_d3(dnS)

      dnVecRes%d0(:) = dnVec%d0(:) * d0
      DO i=1,nVar_loc
        dnVecRes%d1(:,i) = dnVec%d1(:,i)*d0 + dnVec%d0(:) * d1(i)
      END DO
      DO i=1,nVar_loc
      DO j=1,nVar_loc
        dnVecRes%d2(:,j,i) = dnVec%d2(:,j,i)*d0 + dnVec%d1(:,i)*d1(j) + dnVec%d1(:,j)*d1(i) + dnVec%d0(:) * d2(j,i)
      END DO
      END DO
      DO i=1,nVar_loc
      DO j=1,nVar_loc
      DO k=1,nVar_loc
            dnVecRes%d3(:,k,j,i) = dnVec%d3(:,k,j,i)*d0  + &
                                   dnVec%d2(:,i,k)*d1(j) + dnVec%d2(:,k,j)*d1(i) + dnVec%d2(:,j,i)*d1(k) + &
                                   dnVec%d1(:,j)*d2(i,k) + dnVec%d1(:,i)*d2(k,j) + dnVec%d1(:,k)*d2(j,i) + &
                                   dnVec%d0(:)*d3(k,j,i)
      END DO
      END DO
      END DO

    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_dnVec_TIME_dnS
  FUNCTION AD_dnS_TIME_dnVec(dnS,dnVec) RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    TYPE (dnS_t),      intent(in)  :: dnS

    dnVecRes = dnVec*dnS

  END FUNCTION AD_dnS_TIME_dnVec

  FUNCTION AD_dnVec_OVER_R(dnVec,R) RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, ONE, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: R

    dnVecRes = dnVec * (ONE/R)

  END FUNCTION AD_dnVec_OVER_R
  FUNCTION AD_dnVec_OVER_dnS(dnVec,dnS) RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, ONE, out_unit
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    TYPE (dnS_t),      intent(in)  :: dnS

    dnVecRes = dnVec * (ONE/dnS)

  END FUNCTION AD_dnVec_OVER_dnS
!> @brief Public function which calculate dnVec1+dnVec2 (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param dnVec1                    TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param dnVec2                    TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param dnVec2_PLUS_dnVec1 TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_dnVec2_PLUS_dnVec1(dnVec1,dnVec2)  RESULT(dnVec2_PLUS_dnVec1)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                :: dnVec2_PLUS_dnVec1
    TYPE (dnVec_t), intent(in)    :: dnVec1,dnVec2

    integer :: nderiv,SizeVec,nVar
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec2_PLUS_dnVec1'

    nderiv  = min(AD_get_nderiv_FROM_dnVec(dnVec1),AD_get_nderiv_FROM_dnVec(dnVec2))
    SizeVec = min(AD_get_Size_FROM_dnVec(dnVec1), AD_get_Size_FROM_dnVec(dnVec2))
    nVar    = min(AD_get_nVar_FROM_dnVec(dnVec1),  AD_get_nVar_FROM_dnVec(dnVec2))

    !write(out_unit,*) 'in ',name_sub,' SizeVec,nVar,nderiv',SizeVec,nVar,nderiv

    CALL AD_dealloc_dnVec(dnVec2_PLUS_dnVec1)

    IF (nderiv < 0 .OR. SizeVec < 1 .OR. (nderiv > 0  .AND. nVar < 1)) RETURN

    CALL AD_alloc_dnVec(dnVec2_PLUS_dnVec1,SizeVec,nVar,nderiv,          &
                         name_var='dnVec2_PLUS_dnVec1',name_sub=name_sub)

    IF (nderiv == 0) THEN
       dnVec2_PLUS_dnVec1%d0 = dnVec1%d0 + dnVec2%d0
    ELSE IF (nderiv == 1) THEN
       dnVec2_PLUS_dnVec1%d0 = dnVec1%d0 + dnVec2%d0
       dnVec2_PLUS_dnVec1%d1 = dnVec1%d1 + dnVec2%d1
    ELSE IF (nderiv == 2) THEN
       dnVec2_PLUS_dnVec1%d0 = dnVec1%d0 + dnVec2%d0
       dnVec2_PLUS_dnVec1%d1 = dnVec1%d1 + dnVec2%d1
       dnVec2_PLUS_dnVec1%d2 = dnVec1%d2 + dnVec2%d2
    ELSE IF (nderiv == 3) THEN
       dnVec2_PLUS_dnVec1%d0 = dnVec1%d0 + dnVec2%d0
       dnVec2_PLUS_dnVec1%d1 = dnVec1%d1 + dnVec2%d1
       dnVec2_PLUS_dnVec1%d2 = dnVec1%d2 + dnVec2%d2
       dnVec2_PLUS_dnVec1%d3 = dnVec1%d3 + dnVec2%d3
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_dnVec2_PLUS_dnVec1
!> @brief Public function which calculate dnVec+R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param R                  real:                     some real number
!! @param sub_dnVec_EXP_R TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_dnVec_PLUS_R(dnVec,R)  RESULT (sub_dnVec_PLUS_R)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: sub_dnVec_PLUS_R
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec_PLUS_R'


    sub_dnVec_PLUS_R    = dnVec

    sub_dnVec_PLUS_R%d0 = sub_dnVec_PLUS_R%d0 + R

    ! the derivatives of R are zero => nothing to be add to %d1 and %d2

  END FUNCTION AD_dnVec_PLUS_R
!> @brief Public function which calculate R+dnVec (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param R                  real:                     some real number
!! @param sub_R_PLUS_dnVec TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_R_PLUS_dnVec(R,dnVec) RESULT (sub_R_PLUS_dnVec)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: sub_R_PLUS_dnVec
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_R_PLUS_dnVec'


    sub_R_PLUS_dnVec    = dnVec

    sub_R_PLUS_dnVec%d0 = sub_R_PLUS_dnVec%d0 + R

    ! the derivatives of R are zero

  END FUNCTION AD_R_PLUS_dnVec
  FUNCTION AD_dnVec_PLUS_Vec(dnVec,Vec)  RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: Vec(:)

    character (len=*), parameter :: name_sub='AD_dnVec_PLUS_Vec'


    dnVecRes    = dnVec

    dnVecRes%d0 = dnVecRes%d0 + Vec

  END FUNCTION AD_dnVec_PLUS_Vec
  FUNCTION AD_Vec_PLUS_dnVec(Vec,dnVec)  RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: Vec(:)

    character (len=*), parameter :: name_sub='AD_Vec_PLUS_dnVec'


    dnVecRes    = dnVec

    dnVecRes%d0 = dnVecRes%d0 + Vec

  END FUNCTION AD_Vec_PLUS_dnVec
  FUNCTION AD_dnVec_PLUS_VecOFdnS(dnVec,VecOFdnS)  RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    TYPE (dnS_t),      intent(in)  :: VecOFdnS(:)

    character (len=*), parameter :: name_sub='AD_dnVec_PLUS_VecOFdnS'

    dnVecRes = VecOFdnS
    dnVecRes = dnVecRes + dnVec

  END FUNCTION AD_dnVec_PLUS_VecOFdnS
  FUNCTION AD_VecOFdnS_PLUS_dnVec(VecOFdnS,dnVec)  RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    TYPE (dnS_t),      intent(in)  :: VecOFdnS(:)

    character (len=*), parameter :: name_sub='AD_VecOFdnS_PLUS_dnVec'

    dnVecRes = VecOFdnS
    dnVecRes = dnVecRes + dnVec

  END FUNCTION AD_VecOFdnS_PLUS_dnVec
  FUNCTION AD_PLUS_dnVec(dnVec) RESULT (Vres)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: Vres
    TYPE (dnVec_t),    intent(in)  :: dnVec

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_R_PLUS_dnVec'


    Vres    = dnVec

  END FUNCTION AD_PLUS_dnVec
  !> @brief Public function which calculate dnVec1-dnVec2 (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param dnVec1                    TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param dnVec2                    TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param dnVec2_MINUS_dnVec1 TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_dnVec2_MINUS_dnVec1(dnVec1,dnVec2) RESULT (dnVec2_MINUS_dnVec1)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                :: dnVec2_MINUS_dnVec1
    TYPE (dnVec_t), intent(in)    :: dnVec1,dnVec2

    integer :: nderiv,SizeVec,nVar
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec2_MINUS_dnVec1'

    nderiv  = min(AD_get_nderiv_FROM_dnVec(dnVec1),AD_get_nderiv_FROM_dnVec(dnVec2))
    SizeVec = min(AD_get_Size_FROM_dnVec(dnVec1), AD_get_Size_FROM_dnVec(dnVec2))
    nVar    = min(AD_get_nVar_FROM_dnVec(dnVec1),  AD_get_nVar_FROM_dnVec(dnVec2))


    CALL AD_dealloc_dnVec(dnVec2_MINUS_dnVec1)

    IF (nderiv < 0 .OR. SizeVec < 1 .OR. (nderiv > 0  .AND. nVar < 1)) RETURN

    CALL AD_alloc_dnVec(dnVec2_MINUS_dnVec1,SizeVec,nVar,nderiv,         &
                         name_var='dnVec2_MINUS_dnVec1',name_sub=name_sub)

    IF (nderiv == 0) THEN
       dnVec2_MINUS_dnVec1%d0 = dnVec1%d0 - dnVec2%d0
    ELSE IF (nderiv == 1) THEN
       dnVec2_MINUS_dnVec1%d0 = dnVec1%d0 - dnVec2%d0
       dnVec2_MINUS_dnVec1%d1 = dnVec1%d1 - dnVec2%d1
    ELSE IF (nderiv == 2) THEN
       dnVec2_MINUS_dnVec1%d0 = dnVec1%d0 - dnVec2%d0
       dnVec2_MINUS_dnVec1%d1 = dnVec1%d1 - dnVec2%d1
       dnVec2_MINUS_dnVec1%d2 = dnVec1%d2 - dnVec2%d2
    ELSE IF (nderiv == 3) THEN
       dnVec2_MINUS_dnVec1%d0 = dnVec1%d0 - dnVec2%d0
       dnVec2_MINUS_dnVec1%d1 = dnVec1%d1 - dnVec2%d1
       dnVec2_MINUS_dnVec1%d2 = dnVec1%d2 - dnVec2%d2
       dnVec2_MINUS_dnVec1%d3 = dnVec1%d3 - dnVec2%d3
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_dnVec2_MINUS_dnVec1
!> @brief Public function which calculate dnVec-R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                  TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param R                    real:                     some real number
!! @param sub_dnVec_MINUS_R TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_dnVec_MINUS_R(dnVec,R) RESULT (sub_dnVec_MINUS_R)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE


    TYPE (dnVec_t)                 :: sub_dnVec_MINUS_R
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec_MINUS_R'


    sub_dnVec_MINUS_R = dnVec

    sub_dnVec_MINUS_R%d0 = dnVec%d0 - R

    ! the derivatives of R are zero

  END FUNCTION AD_dnVec_MINUS_R
!> @brief Public function which calculate R-dnVec (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                  TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param R                    real:                     some real number
!! @param sub_R_MINUS_dnVec TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_R_MINUS_dnVec(R,dnVec) RESULT (sub_R_MINUS_dnVec)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE


    TYPE (dnVec_t)                 :: sub_R_MINUS_dnVec
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,SizeVec_loc,nVar_loc
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_R_MINUS_dnVec'

    nderiv_loc  = AD_get_nderiv_FROM_dnVec(dnVec)
    SizeVec_loc = AD_get_Size_FROM_dnVec(dnVec)
    nVar_loc    = AD_get_nVar_FROM_dnVec(dnVec)

    !write(out_unit,*) 'nVar,SizeVec,nderiv',nVar_loc,SizeVec_loc,nderiv_loc

    CALL AD_alloc_dnVec(sub_R_MINUS_dnVec,SizeVec_loc,nVar_loc,nderiv_loc,&
                         name_var='sub_R_MINUS_dnVec',name_sub=name_sub)

    !write(out_unit,*) 'nderiv',nderiv_loc
    IF (nderiv_loc == 0) THEN
       sub_R_MINUS_dnVec%d0 = R - dnVec%d0

    ELSE IF (nderiv_loc == 1) THEN
       sub_R_MINUS_dnVec%d0 = R - dnVec%d0
       sub_R_MINUS_dnVec%d1 =   - dnVec%d1


    ELSE IF (nderiv_loc == 2) THEN
       sub_R_MINUS_dnVec%d0 = R - dnVec%d0
       sub_R_MINUS_dnVec%d1 =   - dnVec%d1
       sub_R_MINUS_dnVec%d2 =   - dnVec%d2

    ELSE IF (nderiv_loc == 3) THEN
       sub_R_MINUS_dnVec%d0 = R - dnVec%d0
       sub_R_MINUS_dnVec%d1 =   - dnVec%d1
       sub_R_MINUS_dnVec%d2 =   - dnVec%d2
       sub_R_MINUS_dnVec%d3 =   - dnVec%d3

    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF

  END FUNCTION AD_R_MINUS_dnVec
  FUNCTION AD_dnVec_MINUS_Vec(dnVec,Vec)  RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: Vec(:)

    character (len=*), parameter :: name_sub='AD_dnVec_MINUS_Vec'


    dnVecRes    = dnVec

    dnVecRes%d0 = dnVecRes%d0 - Vec

  END FUNCTION AD_dnVec_MINUS_Vec
  FUNCTION AD_Vec_MINUS_dnVec(Vec,dnVec)  RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: Vec(:)

    character (len=*), parameter :: name_sub='AD_Vec_MINUS_dnVec'


    dnVecRes    = -dnVec

    dnVecRes%d0 = dnVecRes%d0 + Vec

  END FUNCTION AD_Vec_MINUS_dnVec
  FUNCTION AD_dnVec_MINUS_VecOFdnS(dnVec,VecOFdnS)  RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    TYPE (dnS_t),      intent(in)  :: VecOFdnS(:)

    character (len=*), parameter :: name_sub='AD_dnVec_MINUS_VecOFdnS'

    dnVecRes = VecOFdnS
    dnVecRes = -dnVecRes + dnVec

  END FUNCTION AD_dnVec_MINUS_VecOFdnS
  FUNCTION AD_VecOFdnS_MINUS_dnVec(VecOFdnS,dnVec)  RESULT (dnVecRes)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnVec_t)                 :: dnVecRes
    TYPE (dnVec_t),    intent(in)  :: dnVec
    TYPE (dnS_t),      intent(in)  :: VecOFdnS(:)

    character (len=*), parameter :: name_sub='AD_VecOFdnS_MINUS_dnVec'

    dnVecRes = VecOFdnS
    dnVecRes = dnVecRes - dnVec

  END FUNCTION AD_VecOFdnS_MINUS_dnVec
  FUNCTION AD_MINUS_dnVec(V) RESULT(Vres)
    USE QDUtil_m

    TYPE (dnVec_t)                       :: Vres
    CLASS (dnVec_t),       intent(in)    :: V


    character (len=*), parameter :: name_sub='AD_MINUS_dnS'


    CALL AD_dealloc_dnVec(Vres)

    Vres%nderiv = V%nderiv

    IF (allocated(V%d0)) Vres%d0 = -V%d0
    IF (allocated(V%d1)) Vres%d1 = -V%d1
    IF (allocated(V%d2)) Vres%d2 = -V%d2
    IF (allocated(V%d3)) Vres%d3 = -V%d3

  END FUNCTION AD_MINUS_dnVec
  !> @brief Public function which calculate dnVec**R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                TYPE (dnVec_t):           derived type which deals with the derivatives of a vector.
!! @param R                  real:                     exponent
!! @param sub_dnVec_EXP_R TYPE (dnVec_t) (result):  dnVec derived type
  FUNCTION AD_dnVec_EXP_R(dnVec,R) RESULT (sub_dnVec_EXP_R)
    USE QDUtil_m, ONLY : Rkind, ONE, TWO, THREE, out_unit
    IMPLICIT NONE


    TYPE (dnVec_t)                 :: sub_dnVec_EXP_R
    TYPE (dnVec_t),    intent(in)  :: dnVec
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,SizeVec_loc,nVar_loc,id,jd,kd
    integer :: err_dnVec_loc
    character (len=*), parameter :: name_sub='AD_dnVec_EXP_R'

    nderiv_loc  = AD_get_nderiv_FROM_dnVec(dnVec)
    SizeVec_loc = AD_get_Size_FROM_dnVec(dnVec)
    nVar_loc    = AD_get_nVar_FROM_dnVec(dnVec)

    !write(out_unit,*) 'nVar,SizeVec,nderiv',nVar_loc,SizeVec_loc,nderiv_loc

    CALL AD_alloc_dnVec(sub_dnVec_EXP_R,SizeVec_loc,nVar_loc,            &
                nderiv_loc,name_var='sub_dnVec_EXP_R',name_sub=name_sub)

    !write(out_unit,*) 'nderiv',nderiv_loc


    IF (nderiv_loc == 0) THEN
       sub_dnVec_EXP_R%d0 = dnVec%d0 ** R

    ELSE IF (nderiv_loc == 1) THEN
       sub_dnVec_EXP_R%d0 = dnVec%d0 ** R

       DO id=1,nVar_loc
         sub_dnVec_EXP_R%d1(:,id) = R * dnVec%d0 ** (R-ONE) * dnVec%d1(:,id)
       END DO

    ELSE IF (nderiv_loc == 2) THEN
       sub_dnVec_EXP_R%d0 = dnVec%d0 ** R

       DO id=1,nVar_loc
         sub_dnVec_EXP_R%d1(:,id) = R * dnVec%d0 ** (R-ONE) * dnVec%d1(:,id)
       END DO

       DO id=1,nVar_loc
       DO jd=1,nVar_loc
         sub_dnVec_EXP_R%d2(:,jd,id) = R*(R-ONE) * dnVec%d0 ** (R-TWO) * dnVec%d1(:,id) * dnVec%d1(:,jd) + &
                                            R * dnVec%d0 ** (R-ONE) * dnVec%d2(:,jd,id)
       END DO
       END DO

    ELSE IF (nderiv_loc == 3) THEN
       sub_dnVec_EXP_R%d0 = dnVec%d0 ** R

       DO id=1,nVar_loc
         sub_dnVec_EXP_R%d1(:,id) = R * dnVec%d0 ** (R-ONE) * dnVec%d1(:,id)
       END DO

       DO id=1,nVar_loc
       DO jd=1,nVar_loc
         sub_dnVec_EXP_R%d2(:,jd,id) = R*(R-ONE) * dnVec%d0 ** (R-TWO) * dnVec%d1(:,id) * dnVec%d1(:,jd) + &
                                            R * dnVec%d0 ** (R-ONE) * dnVec%d2(:,jd,id)
       END DO
       END DO

       DO id=1,nVar_loc
       DO jd=1,nVar_loc
       DO kd=1,nVar_loc
         sub_dnVec_EXP_R%d3(:,kd,jd,id) =                                  &
                            R*(R-ONE)*(R-TWO) * dnVec%d0**(R-THREE) *      &
                            dnVec%d1(:,id)*dnVec%d1(:,jd)*dnVec%d1(:,kd) + &
                            R*(R-ONE) * dnVec%d0**(R-TWO) * (              &
                               dnVec%d2(:,jd,id)*dnVec%d1(:,kd) +          &
                               dnVec%d2(:,kd,id)*dnVec%d1(:,jd) +          &
                               dnVec%d2(:,kd,jd)*dnVec%d1(:,id) ) +        &
                            R * dnVec%d0**(R-ONE) * dnVec%d3(:,kd,jd,id)

       END DO
       END DO
       END DO

    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_dnVec_EXP_R

!> @brief Public subroutine which prints a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param vec                TYPE (dnVec_t):      derived type which deals with the derivatives of a vector.
!! @param nio                integer (optional):  when present unit to print S, otherwise it is the default unit:out_unit
  SUBROUTINE AD_Write_dnVec(vec,nio,info)
    USE QDUtil_m, ONLY : Rkind, Write_Vec, Write_Mat, out_unit, RMatIO_format
    IMPLICIT NONE

    TYPE (dnVec_t),   intent(in)           :: vec
    integer,          intent(in), optional :: nio
    character(len=*), intent(in), optional :: info

    integer :: i,j,k,nio_loc,SizeVec_loc,nVar_loc

    IF (present(nio)) THEN
      nio_loc = nio
    ELSE
      nio_loc = out_unit
    END IF

    SizeVec_loc = AD_get_Size_FROM_dnVec(vec)
    nVar_loc    = AD_get_nVar_FROM_dnVec(vec)

    IF (SizeVec_loc == 1 .AND. nVar_loc > 1) THEN
      IF (allocated(vec%d0)) THEN
        write(nio_loc,'(a,' // RMatIO_format // ')') ' no derivative',vec%d0
      END IF

      IF (allocated(vec%d1)) THEN
        write(nio_loc,*) ' 1st derivative'
        CALL Write_Vec(vec%d1(1,:),nio_loc,5)
      END IF

      IF (allocated(vec%d2)) THEN
        write(nio_loc,*) ' 2d derivative'
        CALL Write_Mat(vec%d2(1,:,:),nio_loc,5)
      END IF
      IF (allocated(vec%d3)) THEN
        write(nio_loc,*) ' 3d derivative'
        DO i=1,ubound(vec%d3,dim=4)
        DO j=1,ubound(vec%d3,dim=3)
        DO k=1,ubound(vec%d3,dim=2)
          write(nio_loc,'(3(1x,i0)," : ",' // RMatIO_format // ')') k,j,i,vec%d3(1,k,j,i)
        END DO
        END DO
        END DO
      END IF
    ELSE
      IF (allocated(vec%d0)) THEN
         IF (present(info)) THEN
           write(nio_loc,*) ' no derivative of ',info
         ELSE
           write(nio_loc,*) ' no derivative'
         END IF
        CALL Write_vec(vec%d0,nio_loc,5)
      END IF

      IF (allocated(vec%d1)) THEN
        DO i=1,ubound(vec%d1,dim=2)
          IF (present(info)) THEN
            write(nio_loc,*) ' 1st derivative of ',info,i
          ELSE
            write(nio_loc,*) ' 1st derivative',i
          END IF
          CALL Write_vec(vec%d1(:,i),nio_loc,5)
        END DO
      END IF

      IF (allocated(vec%d2)) THEN
        DO i=1,ubound(vec%d2,dim=3)
        DO j=1,ubound(vec%d2,dim=2)
          IF (present(info)) THEN
            write(nio_loc,*) ' 2d derivative of ',info,j,i
          ELSE
            write(nio_loc,*) ' 2d derivative',j,i
          END IF
          CALL Write_vec(vec%d2(:,j,i),nio_loc,5)
        END DO
        END DO
      END IF

      IF (allocated(vec%d3)) THEN
        DO i=1,ubound(vec%d3,dim=4)
        DO j=1,ubound(vec%d3,dim=3)
        DO k=1,ubound(vec%d3,dim=2)
          IF (present(info)) THEN
            write(nio_loc,*) ' 3d derivative of ',info,k,j,i
          ELSE
            write(nio_loc,*) ' 3d derivative',k,j,i
          END IF
          CALL Write_vec(vec%d3(:,k,j,i),nio_loc,5)
        END DO
        END DO
        END DO
      END IF
    END IF

  END SUBROUTINE AD_Write_dnVec
!> @brief Public function to get nderiv from a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param vec                         TYPE (dnVec_t):     derived type which deals with the derivatives of a vector.
!! @param get_nderiv_FROM_dnVec    integer  (result):  nderiv value, check against vec%nderiv
  FUNCTION AD_get_nderiv_FROM_dnVec(vec) RESULT(nderiv)
    USE QDUtil_m, ONLY : out_unit
    IMPLICIT NONE

    integer                       :: nderiv
    TYPE (dnVec_t), intent(in)    :: vec

    nderiv = vec%nderiv

    IF (.NOT. allocated(vec%d0)) THEN
      nderiv = -1
    ELSE IF (.NOT. allocated(vec%d1)) THEN
      nderiv = 0
    ELSE IF (.NOT. allocated(vec%d2)) THEN
      nderiv = 1
    ELSE IF (.NOT. allocated(vec%d3)) THEN
      nderiv = 2
    ELSE
      nderiv = 3
    END IF

    IF (vec%nderiv /= nderiv) THEN
      write(out_unit,*) ' ERROR in AD_get_nderiv_FROM_dnVec'
      write(out_unit,*) '  Problem with nderiv in vec'
      CALL AD_Write_dnVec(vec)
      STOP 'ERROR in AD_get_nderiv_FROM_dnVec'
    END IF

    END FUNCTION AD_get_nderiv_FROM_dnVec

!> @brief Public function to get SizeVec (the size of the vector, dimension of the vector) from a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param vec                         TYPE (dnVec_t):     derived type which deals with the derivatives of a vector.
!! @param AD_get_Size_FROM_dnVec   integer  (result):  SizeVec value
  FUNCTION AD_get_Size_FROM_dnVec(vec) RESULT(SizeVec)

    integer                       :: SizeVec
    TYPE (dnVec_t), intent(in)    :: vec

    IF (.NOT. allocated(vec%d0)) THEN
      SizeVec = 0
    ELSE
      SizeVec = size(vec%d0)
    END IF

    END FUNCTION AD_get_Size_FROM_dnVec

!> @brief Public function to get nVar (number of coordinates) from a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param vec                       TYPE (dnVec_t):      derived type which deals with the derivatives of a scalar functions.
!! @param get_nVar_FROM_dnVec    integer  (result):   nVar value from the size of vec%d1.
  FUNCTION AD_get_nVar_FROM_dnVec(vec) RESULT(nVar)

    integer                       :: nVar
    TYPE (dnVec_t), intent(in)    :: vec

    IF (.NOT. allocated(vec%d1)) THEN
      nVar = 0
    ELSE
      nVar = size(vec%d1,dim=2)
    END IF

    END FUNCTION AD_get_nVar_FROM_dnVec
!> @brief Public function to get d0(:,:) from a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 07/11/2022
!!
!! @param vec                       TYPE (dnVec_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d0_FROM_dnVec      real  (result):      vec%d0(:)
    FUNCTION AD_get_d0_FROM_dnVec(vec) RESULT(d0)
      USE QDUtil_m, ONLY : Rkind
      IMPLICIT NONE

      TYPE (dnVec_t), intent(in)         :: vec
      real (kind=Rkind),   allocatable   :: d0(:)

      IF (allocated(vec%d0)) d0 = vec%d0
  
    END FUNCTION AD_get_d0_FROM_dnVec
!> @brief Public function to get d1(:,:,:) from a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 07/11/2022
!!
!! @param vec                       TYPE (dnVec_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d1_FROM_dnVec      real  (result):      vec%d1(:,:)
    FUNCTION AD_get_d1_FROM_dnVec(vec) RESULT(d1)
      USE QDUtil_m, ONLY : Rkind
      IMPLICIT NONE

      TYPE (dnVec_t), intent(in)         :: vec
      real (kind=Rkind),   allocatable   :: d1(:,:)

      IF (allocated(vec%d1)) d1 = vec%d1
  
    END FUNCTION AD_get_d1_FROM_dnVec
!> @brief Public function to get d2(:,:,:,:) from a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 07/11/2022
!!
!! @param vec                       TYPE (dnVec_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d2_FROM_dnVec      real  (result):      vec%d2(:,:,:)
    FUNCTION AD_get_d2_FROM_dnVec(vec) RESULT(d2)
      USE QDUtil_m, ONLY : Rkind
      IMPLICIT NONE

      TYPE (dnVec_t),       intent(in)    :: vec
      real (kind=Rkind),    allocatable   :: d2(:,:,:)

      IF (allocated(vec%d2)) d2 = vec%d2
  
    END FUNCTION AD_get_d2_FROM_dnVec
!> @brief Public function to get d0(:,:) from a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 07/11/2022
!!
!! @param vec                       TYPE (dnVec_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d3_FROM_dnVec      real  (result):      vec%d3(:,:,:,:)
    FUNCTION AD_get_d3_FROM_dnVec(vec) RESULT(d3)
      USE QDUtil_m, ONLY : Rkind
      IMPLICIT NONE

      TYPE (dnVec_t),       intent(in)    :: vec
      real (kind=Rkind),    allocatable   :: d3(:,:,:,:)

      IF (allocated(vec%d3)) d3 = vec%d3
  
    END FUNCTION AD_get_d3_FROM_dnVec
!> @brief Public function to get FlatdnVec(:) from a derived type dnVec.
!!
!> @author David Lauvergnat
!! @date 09/01/2023
!!
!! @param vec                       TYPE (dnVec_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d3_FROM_dnVec      real  (result):      FlatdnVec
    FUNCTION AD_get_Flatten_dnVec(vec,all_der,i_der) RESULT(FlatdnVec)
      USE QDUtil_m, ONLY : Rkind, out_unit
      IMPLICIT NONE

      TYPE (dnVec_t),       intent(in)    :: vec
      real (kind=Rkind),    allocatable   :: FlatdnVec(:)
      logical,              intent(in), optional :: all_der
      integer,              intent(in), optional :: i_der

      logical :: tab_der(0:3)
      integer :: nderiv,FlatdnVec_size,i,f

      nderiv = get_nderiv(vec)
      IF (nderiv < 0) THEN
        allocate(FlatdnVec(0))
        RETURN ! vec%d... are not allocated
      END IF

      tab_der(:) = .FALSE.

      IF (present(all_der)) THEN
        IF (all_der) THEN
          tab_der(0:nderiv) = .TRUE.
          IF (present(i_der)) THEN
             write(out_unit,*) ' ERROR in AD_get_Flatten_dnVec'
             write(out_unit,*) ' all_der and i_der are present and incompatible'
             write(out_unit,*) '   all_der,i_der',all_der,i_der
             STOP 'ERROR in AD_get_Flatten_dnVec: all_der and i_der are present and incompatible'
          END IF
        END IF
      END IF

      IF (present(i_der)) THEN
        IF (i_der < 0 .OR. i_der > nderiv) THEN
          write(out_unit,*) ' ERROR in AD_get_Flatten_dnVec'
          write(out_unit,*) ' i_der MUST be >= 0 and <= nderiv'
          write(out_unit,*) '   i_der,nderiv',i_der,nderiv
          STOP 'ERROR in AD_get_Flatten_dnVec: i_der MUST be >= 0 and <= nderiv'
        END IF
        tab_der(0:nderiv) = .FALSE.
        tab_der(i_der)    = .TRUE.
      ELSE
        tab_der(0:nderiv) = .TRUE.
      END IF
      ! size of FlatdnVec
      FlatdnVec_size = 0
      IF (tab_der(0)) FlatdnVec_size = FlatdnVec_size + size(vec%d0)
      IF (tab_der(1)) FlatdnVec_size = FlatdnVec_size + size(vec%d1)
      IF (tab_der(2)) FlatdnVec_size = FlatdnVec_size + size(vec%d2)
      IF (tab_der(3)) FlatdnVec_size = FlatdnVec_size + size(vec%d3)

      !write(out_unit,*) 'tab_der,FlatdnVec_size',tab_der,FlatdnVec_size

      allocate(FlatdnVec(FlatdnVec_size))

      i = 0
      IF (tab_der(0)) THEN
        f = i + size(vec%d0)
        FlatdnVec(i+1:f) = reshape(vec%d0,shape=[size(vec%d0)])
        i = f
      END IF
      IF (tab_der(1)) THEN
        f = i + size(vec%d1)
        FlatdnVec(i+1:f) = reshape(vec%d1,shape=[size(vec%d1)])
        i = f
      END IF
      IF (tab_der(2)) THEN
        f = i + size(vec%d2)
        FlatdnVec(i+1:f) = reshape(vec%d2,shape=[size(vec%d2)])
        i = f
      END IF
      IF (tab_der(3)) THEN
        f = i + size(vec%d3)
        FlatdnVec(i+1:f) = reshape(vec%d3,shape=[size(vec%d3)])
        i = f
      END IF

    END FUNCTION AD_get_Flatten_dnVec
!> @brief Public function which ckecks a derived type dnVec is zero (all components).
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param Check_dnVec_IS_ZERO   logical  (result):   result of the comparison
!! @param vec                      TYPE (dnVec_t):      derived type which deals with the derivatives of a vector.
!! @param epsi                     real (optional):     when present zero limit, otherwise 10^-10
  FUNCTION AD_Check_dnVec_IS_ZERO(vec,epsi) RESULT(IS_ZERO)
    USE QDUtil_m, ONLY : Rkind, ONETENTH
    IMPLICIT NONE

    logical                                  :: IS_ZERO
    TYPE (dnVec_t),     intent(in)           :: vec
    real(kind=Rkind),   intent(in), optional :: epsi


    IF (present(epsi)) THEN
      IS_ZERO = AD_get_maxval_OF_dnVec(vec) <= epsi
    ELSE
      IS_ZERO = AD_get_maxval_OF_dnVec(vec) <= ONETENTH**10
    END IF

    END FUNCTION AD_Check_dnVec_IS_ZERO
!> @brief Public function which gets the largest value of a derived type get_maxval_OF_dnVec (all components).
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param get_maxval_OF_dnVec   real  (result):      largest value (all components)
!! @param vec                      TYPE (dnVec_t):      derived type which deals with the derivatives of a vector.
  FUNCTION AD_get_maxval_OF_dnVec(vec,nderiv) RESULT(mval)
    USE QDUtil_m, ONLY : Rkind, ZERO
    IMPLICIT NONE

    real(kind=Rkind)                     :: mval
    TYPE (dnVec_t), intent(in)           :: vec
    integer,        intent(in), optional :: nderiv

    real(kind=Rkind) :: e0,e1,e2,e3
    integer          :: nderiv_loc

    nderiv_loc = AD_get_nderiv_FROM_dnVec(vec)
    IF (present(nderiv)) nderiv_loc = min(nderiv_loc,nderiv)

    IF (allocated(vec%d0) .AND. nderiv_loc >= 0) THEN
      e0 = maxval(abs(vec%d0))
    ELSE
      e0 = ZERO
    END IF

    IF (allocated(vec%d1) .AND. nderiv_loc >= 1) THEN
      e1 = maxval(abs(vec%d1))
    ELSE
      e1 = ZERO
    END IF

    IF (allocated(vec%d2) .AND. nderiv_loc >= 2) THEN
      e2 = maxval(abs(vec%d2))
    ELSE
      e2 = ZERO
    END IF

    IF (allocated(vec%d3) .AND. nderiv_loc >= 3) THEN
      e3 = maxval(abs(vec%d3))
    ELSE
      e3 = ZERO
    END IF

    mval = max(e0,e1,e2,e3)

    END FUNCTION AD_get_maxval_OF_dnVec
!! @brief Public subroutine which checks if the derived type dnVec is (correctly) allocated.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param vec                TYPE (dnVec_t):  derived type which deals with the derivatives of a vector.
!! @param nderiv             integer:         the derivative order.
  FUNCTION AD_Check_NotAlloc_dnVec(vec,nderiv) RESULT(NotAlloc)

    logical                       :: NotAlloc
    TYPE (dnVec_t), intent(in)    :: vec
    integer,        intent(in)    :: nderiv

    NotAlloc =               (nderiv >= 0 .AND. .NOT. allocated(vec%d0))
    NotAlloc = NotAlloc .OR. (nderiv >= 1 .AND. .NOT. allocated(vec%d1))
    NotAlloc = NotAlloc .OR. (nderiv >= 2 .AND. .NOT. allocated(vec%d2))
    NotAlloc = NotAlloc .OR. (nderiv >= 3 .AND. .NOT. allocated(vec%d3))

  END FUNCTION AD_Check_NotAlloc_dnVec
  FUNCTION AD_Check_Alloc_dnVec(vec) RESULT(Alloc)

    logical                       :: Alloc
    TYPE (dnVec_t), intent(in)    :: vec

    Alloc = allocated(vec%d0) .OR. allocated(vec%d1) .OR. allocated(vec%d2) .OR. allocated(vec%d3)

  END FUNCTION AD_Check_Alloc_dnVec
  FUNCTION AD_CheckInit_dnVec1_dnVec2(vec1,vec2,with_nderiv) RESULT(check)

    logical                                 :: check
    TYPE (dnVec_t), intent(in)              :: vec1,vec2
    logical,        intent(in), optional    :: with_nderiv

    integer :: nder1,nsize1,nVar1
    integer :: nder2,nsize2,nVar2
    logical :: with_nderiv_loc
    logical :: check_size,check_nVar


    nsize1 = get_Size(Vec1)
    nVar1  = get_nVar(Vec1)
    nsize2 = get_Size(Vec2)
    nVar2  = get_nVar(Vec2)

    check_size = (nsize1 > 0 .AND. nsize1 == nsize2)
    check_nVar = (nVar1  == nVar2) .AND. (nVar1 >= 0)
    check = check_size .AND. check_nVar

    IF (present(with_nderiv)) THEN
      with_nderiv_loc = with_nderiv
    ELSE
      with_nderiv_loc = .TRUE.
    END IF

    IF (with_nderiv_loc) THEN
      nder1 = get_nderiv(Vec1)
      nder2 = get_nderiv(Vec2)
      check = check .AND. (nder1 >= 0  .AND. nder1  == nder2)
    END IF

  END FUNCTION AD_CheckInit_dnVec1_dnVec2

  FUNCTION AD_dot_product_dnVec(Vec1,Vec2) RESULT(Sres)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnS_t)                         :: Sres
    TYPE (dnVec_t),        intent(in)    :: Vec1,Vec2

    integer           :: nsize1,nVar1,nderiv1,nsize2,nVar2,nderiv2,i,j,k
    logical           :: check
    real (kind=Rkind) :: val
    character (len=*), parameter :: name_sub='AD_dot_product_dnVec'

    IF (.NOT. CheckInit(Vec1,Vec2)) THEN
      write(out_unit,*) ' ERROR in AD_dot_product_dnVec'
      write(out_unit,*) ' Vec1 and Vec2 are not initilalized ...'
      write(out_unit,*) '    or'
      write(out_unit,*) '  Vec1 and Vec2 intializations are different'
      nderiv1 = get_nderiv(Vec1)
      nsize1  = get_Size(Vec1)
      nVar1   = get_nVar(Vec1)
      write(out_unit,*) '  nsize1,nVar1,nderiv1',nsize1,nVar1,nderiv1
      nderiv2 = get_nderiv(Vec2)
      nsize2  = get_Size(Vec2)
      nVar2   = get_nVar(Vec2)
      write(out_unit,*) '  nsize2,nVar2,nderiv2',nsize2,nVar2,nderiv2
      write(out_unit,*) '  nsize2, check',(nsize1 > 0 .AND. nsize1 == nsize2)
      write(out_unit,*) '  nVar2,  check',(nVar1 > 0  .AND. nVar1  == nVar2)
      write(out_unit,*) '  nderiv2,check',(nderiv1 >= 0  .AND. nderiv1  == nderiv2)
      STOP 'ERROR in AD_dot_product_dnVec: Vec1 and Vec2 intializations are different'
    END IF

    nderiv1 = get_nderiv(Vec1)
    nVar1   = get_nVar(Vec1)
    CALL alloc_dnS(Sres,nVar=nVar1,nderiv=nderiv1)
    Sres = ZERO

    IF (nderiv1 >= 0) THEN
      val = dot_product(Vec1%d0,Vec2%d0)
      CALL Set_dnS(Sres,val=val,ider=[0])
    END IF

    IF (nderiv1 >= 1) THEN
      DO i=1,nVar1
        val = dot_product(Vec1%d1(:,i),Vec2%d0) + dot_product(Vec1%d0,Vec2%d1(:,i))
        CALL Set_dnS(Sres,val=val,ider=[i])
      END DO
    END IF

    IF (nderiv1 >= 2) THEN
      DO i=1,nVar1
      DO j=1,nVar1
        val = dot_product(Vec1%d2(:,j,i),Vec2%d0)    + dot_product(Vec1%d0,Vec2%d2(:,j,i)) + &
              dot_product(Vec1%d1(:,j),Vec2%d1(:,i)) + dot_product(Vec1%d1(:,i),Vec2%d1(:,j))
        CALL Set_dnS(Sres,val=val,ider=[j,i])
      END DO
      END DO
    END IF

    IF (nderiv1 >= 3) THEN
      DO i=1,nVar1
      DO j=1,nVar1
      DO k=1,nVar1
        val = dot_product(Vec1%d3(:,k,j,i),Vec2%d0)    + dot_product(Vec1%d0,Vec2%d3(:,k,j,i))    + &
              dot_product(Vec1%d2(:,j,i),Vec2%d1(:,k)) + dot_product(Vec1%d1(:,k),Vec2%d2(:,j,i)) + &
              dot_product(Vec1%d2(:,k,j),Vec2%d1(:,i)) + dot_product(Vec1%d1(:,i),Vec2%d2(:,k,j)) + &
              dot_product(Vec1%d1(:,j),Vec2%d2(:,k,i)) + dot_product(Vec1%d2(:,k,i),Vec2%d1(:,j))
        CALL Set_dnS(Sres,val=val,ider=[k,j,i])
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_dot_product_dnVec
  FUNCTION AD_dot_product_dnVec_Vec(Vec1,Vec2) RESULT(Sres)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnS_t)                         :: Sres
    TYPE (dnVec_t),        intent(in)    :: Vec1
    real (kind=Rkind),     intent(in)    :: Vec2(:)

    integer           :: nsize1,nVar1,nderiv1,nsize2,i,j,k
    real (kind=Rkind) :: val
    character (len=*), parameter :: name_sub='AD_dot_product_dnVec_Vec'

    nderiv1 = get_nderiv(Vec1)
    nVar1   = get_nVar(Vec1)
    nsize1  = get_Size(Vec1)

    nsize2  = size(Vec2)

    IF (nsize1 /= nsize2) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' the sizes of Vec1 or Vec2 are not equal.'
      write(out_unit,*) '  nsize1,nsize2',nsize1,nsize2
      STOP 'ERROR in AD_dot_product_dnVec_Vec: the sizes of Vec1 or Vec2 are not equal.'
    END IF

 
    CALL alloc_dnS(Sres,nVar=nVar1,nderiv=nderiv1)
    Sres = ZERO

    IF (nderiv1 >= 0) THEN
      val = dot_product(Vec1%d0,Vec2)
      CALL Set_dnS(Sres,val=val,ider=[0])
    END IF

    IF (nderiv1 >= 1) THEN
      DO i=1,nVar1
        val = dot_product(Vec1%d1(:,i),Vec2)
        CALL Set_dnS(Sres,val=val,ider=[i])
      END DO
    END IF

    IF (nderiv1 >= 2) THEN
      DO i=1,nVar1
      DO j=1,nVar1
        val = dot_product(Vec1%d2(:,j,i),Vec2)
        CALL Set_dnS(Sres,val=val,ider=[j,i])
      END DO
      END DO
    END IF

    IF (nderiv1 >= 3) THEN
      DO i=1,nVar1
      DO j=1,nVar1
      DO k=1,nVar1
        val = dot_product(Vec1%d3(:,k,j,i),Vec2)
        CALL Set_dnS(Sres,val=val,ider=[k,j,i])
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_dot_product_dnVec_Vec
  FUNCTION AD_dot_product_Vec_dnVec(Vec1,Vec2) RESULT(Sres)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnS_t)                         :: Sres
    real (kind=Rkind),     intent(in)    :: Vec1(:)
    TYPE (dnVec_t),        intent(in)    :: Vec2

    integer           :: nsize1,nVar1,nderiv1,nsize2,nVar2,nderiv2,i,j,k
    logical           :: check
    real (kind=Rkind) :: val
    character (len=*), parameter :: name_sub='AD_dot_product_Vec_dnVec'

    nderiv2 = get_nderiv(Vec2)
    nsize2  = get_Size(Vec2)
    nVar2   = get_nVar(Vec2)
    
    nsize1  = size(Vec1)

    IF (nsize1 /= nsize2) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' the sizes of Vec1 or Vec2 are not equal.'
      write(out_unit,*) '  nsize1,nsize2',nsize1,nsize2
      STOP 'ERROR in AD_dot_product_Vec_dnVec: the sizes of Vec1 or Vec2 are not equal.'
    END IF

    CALL alloc_dnS(Sres,nVar=nVar2,nderiv=nderiv2)
    Sres = ZERO

    IF (nderiv2 >= 0) THEN
      val = dot_product(Vec1,Vec2%d0)
      CALL Set_dnS(Sres,val=val,ider=[0])
    END IF

    IF (nderiv2 >= 1) THEN
      DO i=1,nVar2
        val = dot_product(Vec1,Vec2%d1(:,i))
        CALL Set_dnS(Sres,val=val,ider=[i])
      END DO
    END IF

    IF (nderiv2 >= 2) THEN
      DO i=1,nVar2
      DO j=1,nVar2
        val = dot_product(Vec1,Vec2%d2(:,j,i))
        CALL Set_dnS(Sres,val=val,ider=[j,i])
      END DO
      END DO
    END IF

    IF (nderiv2 >= 3) THEN
      DO i=1,nVar2
      DO j=1,nVar2
      DO k=1,nVar2
        val = dot_product(Vec1,Vec2%d3(:,k,j,i))
        CALL Set_dnS(Sres,val=val,ider=[k,j,i])
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_dot_product_Vec_dnVec

  FUNCTION AD_matmul_Mat_dnVec(Mat1,Vec2) RESULT(Vres)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnVec_t)                       :: Vres
    TYPE (dnVec_t),        intent(in)    :: Vec2
    real (kind=Rkind),     intent(in)    :: Mat1(:,:)

    integer           :: nderivVec,nVarVec,nsizeVec,nsizeM1,nsizeM2,i,j,k
    character (len=*), parameter :: name_sub='AD_matmul_Mat_dnVec'

    nderivVec = get_nderiv(Vec2)
    nVarVec   = get_nVar(Vec2)
    nsizeVec  = get_Size(Vec2)

    nsizeM2  = size(Mat1,dim=2)
    nsizeM1  = size(Mat1,dim=1)

    IF (nsizeVec /= nsizeM2) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' the sizes of Vec2 and of the second dim of Mat1 are not equal.'
      write(out_unit,*) '  nsizeVec,nsizeM2',nsizeVec,nsizeM2
      STOP 'ERROR in AD_matmul_Mat_dnVec: the sizes of Vec and Mat are compatible.'
    END IF

 
    CALL alloc_dnVec(Vres,SizeVec=nsizeM1,nVar=nVarVec,nderiv=nderivVec)

    IF (nderivVec >= 0) THEN
      Vres%d0 = matmul(Mat1,Vec2%d0)
    END IF

    IF (nderivVec >= 1) THEN
      DO i=1,nVarVec
        Vres%d1(:,i) = matmul(Mat1,Vec2%d1(:,i))
      END DO
    END IF

    IF (nderivVec >= 2) THEN
      DO i=1,nVarVec
      DO j=1,nVarVec
        Vres%d2(:,j,i) = matmul(Mat1,Vec2%d2(:,j,i))
      END DO
      END DO
    END IF

    IF (nderivVec >= 3) THEN
      DO i=1,nVarVec
      DO j=1,nVarVec
      DO k=1,nVarVec
        Vres%d3(:,k,j,i) = matmul(Mat1,Vec2%d3(:,k,j,i))
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_matmul_Mat_dnVec

  FUNCTION AD_matmul_dnVec_Mat(Vec1,Mat2) RESULT(Vres)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnVec_t)                       :: Vres
    TYPE (dnVec_t),        intent(in)    :: Vec1
    real (kind=Rkind),     intent(in)    :: Mat2(:,:)

    integer           :: nderivVec,nVarVec,nsizeVec,nsizeM1,nsizeM2,i,j,k
    character (len=*), parameter :: name_sub='AD_matmul_dnVec_Mat'

    nderivVec = get_nderiv(Vec1)
    nVarVec   = get_nVar(Vec1)
    nsizeVec  = get_Size(Vec1)

    nsizeM2  = size(Mat2,dim=2)
    nsizeM1  = size(Mat2,dim=1)

    IF (nsizeVec /= nsizeM1) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' the sizes of Vec1 and of the first dim of Mat2 are not equal.'
      write(out_unit,*) '  nsizeVec,nsizeM2',nsizeVec,nsizeM1
      STOP 'ERROR in AD_matmul_dnVec_Mat: the sizes of Vec1 and Mat2 are compatible.'
    END IF

 
    CALL alloc_dnVec(Vres,SizeVec=nsizeM2,nVar=nVarVec,nderiv=nderivVec)

    IF (nderivVec >= 0) THEN
      Vres%d0 = matmul(Vec1%d0,Mat2)
    END IF

    IF (nderivVec >= 1) THEN
      DO i=1,nVarVec
        Vres%d1(:,i) = matmul(Vec1%d1(:,i),Mat2)
      END DO
    END IF

    IF (nderivVec >= 2) THEN
      DO i=1,nVarVec
      DO j=1,nVarVec
        Vres%d2(:,j,i) = matmul(Vec1%d2(:,j,i),Mat2)
      END DO
      END DO
    END IF

    IF (nderivVec >= 3) THEN
      DO i=1,nVarVec
      DO j=1,nVarVec
      DO k=1,nVarVec
        Vres%d3(:,k,j,i) = matmul(Vec1%d3(:,k,j,i),Mat2)
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_matmul_dnVec_Mat

  FUNCTION AD_cross_product_dnVec(Vec1,Vec2) RESULT(Vres)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnVec_t)                       :: Vres
    TYPE (dnVec_t),        intent(in)    :: Vec1,Vec2

    integer           :: nsize1,nVar1,nderiv1,nsize2,nVar2,nderiv2,i,j,k
    logical           :: check
    character (len=*), parameter :: name_sub='AD_cross_product_dnVec'

    IF (.NOT. CheckInit(Vec1,Vec2)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' Vec1 and Vec2 are not initilalized ...'
      write(out_unit,*) '    or'
      write(out_unit,*) '  Vec1 and Vec2 intializations are different'
      nderiv1 = get_nderiv(Vec1)
      nsize1  = get_Size(Vec1)
      nVar1   = get_nVar(Vec1)
      write(out_unit,*) '  nsize1,nVar1,nderiv1',nsize1,nVar1,nderiv1
      nderiv2 = get_nderiv(Vec2)
      nsize2  = get_Size(Vec2)
      nVar2   = get_nVar(Vec2)
      write(out_unit,*) '  nsize2,nVar2,nderiv2',nsize2,nVar2,nderiv2
      write(out_unit,*) '  nsize2, check',(nsize1 > 0 .AND. nsize1 == nsize2)
      write(out_unit,*) '  nVar2,  check',(nVar1  == nVar2),(nVar1 >= 0)
      write(out_unit,*) '  nderiv2,check',(nderiv1 >= 0  .AND. nderiv1  == nderiv2)
      STOP 'ERROR in AD_cross_product_dnVec: Vec1 and Vec2 intializations are different'
    END IF

    nsize1  = get_Size(Vec1)
    IF (nsize1 /= 3) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' the sizes of Vec1 and Vec2 are not equal to 3.'
      nsize1  = get_Size(Vec1)
      write(out_unit,*) '  nsize1,nsize2',nsize1
      STOP 'ERROR in AD_cross_product_dnVec: the sizes of Vec1 and Vec2 are not equal to 3.'
    END IF

    nderiv1 = get_nderiv(Vec1)
    nVar1   = get_nVar(Vec1)

    CALL alloc_dnVec(Vres,SizeVec=nsize1,nVar=nVar1,nderiv=nderiv1)

    IF (nderiv1 >= 0) THEN
      Vres%d0 = AD_cross_product(Vec1%d0,Vec2%d0)
    END IF

    IF (nderiv1 >= 1) THEN
      DO i=1,nVar1
        Vres%d1(:,i) = AD_cross_product(Vec1%d1(:,i),Vec2%d0) + &
                       AD_cross_product(Vec1%d0,Vec2%d1(:,i))
      END DO
    END IF

    IF (nderiv1 >= 2) THEN
      DO i=1,nVar1
      DO j=1,nVar1
        Vres%d2(:,j,i) = AD_cross_product(Vec1%d2(:,j,i),Vec2%d0)    + &
                         AD_cross_product(Vec1%d0,Vec2%d2(:,j,i))    + &
                         AD_cross_product(Vec1%d1(:,j),Vec2%d1(:,i)) + &
                         AD_cross_product(Vec1%d1(:,i),Vec2%d1(:,j))
      END DO
      END DO
    END IF

    IF (nderiv1 >= 3) THEN
      DO i=1,nVar1
      DO j=1,nVar1
      DO k=1,nVar1
        Vres%d3(:,k,j,i) = AD_cross_product(Vec1%d3(:,k,j,i),Vec2%d0)    + &
                           AD_cross_product(Vec1%d0,Vec2%d3(:,k,j,i))    + &
                           AD_cross_product(Vec1%d2(:,j,i),Vec2%d1(:,k)) + &
                           AD_cross_product(Vec1%d1(:,k),Vec2%d2(:,j,i)) + &
                           AD_cross_product(Vec1%d2(:,k,j),Vec2%d1(:,i)) + &
                           AD_cross_product(Vec1%d1(:,i),Vec2%d2(:,k,j)) + &
                           AD_cross_product(Vec1%d1(:,j),Vec2%d2(:,k,i)) + &
                           AD_cross_product(Vec1%d2(:,k,i),Vec2%d1(:,j))
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_cross_product_dnVec

  FUNCTION AD_cross_product_dnVec_Vec(Vec1,Vec2) RESULT(Vres)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnVec_t)                       :: Vres
    TYPE (dnVec_t),        intent(in)    :: Vec1
    real (kind=Rkind),     intent(in)    :: Vec2(:)

    integer           :: nsize1,nVar1,nderiv1,nsize2,i,j,k
    logical           :: check
    character (len=*), parameter :: name_sub='AD_cross_product_dnVec_Vec'

    nderiv1 = get_nderiv(Vec1)
    nsize1  = get_Size(Vec1)
    nVar1   = get_nVar(Vec1)
    nsize2  = size(vec2)

    IF (nsize1 /= 3 .OR. nsize2 /= 3) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' the sizes of Vec1 or Vec2 are not equal to 3.'
      write(out_unit,*) '  nsize1,nsize2',nsize1,nsize2
      STOP 'ERROR in AD_cross_product_dnVec_Vec: the sizes of Vec1 or Vec2 are not equal to 3.'
    END IF

    CALL alloc_dnVec(Vres,SizeVec=nsize1,nVar=nVar1,nderiv=nderiv1)

    IF (nderiv1 >= 0) THEN
      Vres%d0 = AD_cross_product(Vec1%d0,Vec2)
    END IF

    IF (nderiv1 >= 1) THEN
      DO i=1,nVar1
        Vres%d1(:,i) = AD_cross_product(Vec1%d1(:,i),Vec2)
      END DO
    END IF

    IF (nderiv1 >= 2) THEN
      DO i=1,nVar1
      DO j=1,nVar1
        Vres%d2(:,j,i) = AD_cross_product(Vec1%d2(:,j,i),Vec2)
      END DO
      END DO
    END IF

    IF (nderiv1 >= 3) THEN
      DO i=1,nVar1
      DO j=1,nVar1
      DO k=1,nVar1
        Vres%d3(:,k,j,i) = AD_cross_product(Vec1%d3(:,k,j,i),Vec2)
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_cross_product_dnVec_Vec

  FUNCTION AD_cross_product_Vec_dnVec(Vec1,Vec2) RESULT(Vres)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnVec_t)                       :: Vres
    real (kind=Rkind),     intent(in)    :: Vec1(:)
    TYPE (dnVec_t),        intent(in)    :: Vec2

    integer           :: nsize1,nsize2,nVar2,nderiv2,i,j,k
    character (len=*), parameter :: name_sub='AD_cross_product_Vec_dnVec'


    nderiv2 = get_nderiv(Vec2)
    nsize2  = get_Size(Vec2)
    nVar2   = get_nVar(Vec2)

    nsize1  = size(Vec1)

    IF (nsize1 /= 3 .OR. nsize2 /= 3) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' the sizes of Vec1 or Vec2 are not equal to 3.'
      write(out_unit,*) '  nsize1,nsize2',nsize1,nsize2
      STOP 'ERROR in AD_cross_product_Vec_dnVec: the sizes of Vec1 and Vec2 are not equal to 3.'
    END IF

    CALL alloc_dnVec(Vres,SizeVec=nsize2,nVar=nVar2,nderiv=nderiv2)

    IF (nderiv2 >= 0) THEN
      Vres%d0 = AD_cross_product(Vec1,Vec2%d0)
    END IF

    IF (nderiv2 >= 1) THEN
      DO i=1,nVar2
        Vres%d1(:,i) = AD_cross_product(Vec1,Vec2%d1(:,i))
      END DO
    END IF

    IF (nderiv2 >= 2) THEN
      DO i=1,nVar2
      DO j=1,nVar2
        Vres%d2(:,j,i) = AD_cross_product(Vec1,Vec2%d2(:,j,i))
      END DO
      END DO
    END IF

    IF (nderiv2 >= 3) THEN
      DO i=1,nVar2
      DO j=1,nVar2
      DO k=1,nVar2
        Vres%d3(:,k,j,i) = AD_cross_product(Vec1,Vec2%d3(:,k,j,i))
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_cross_product_Vec_dnVec
  !================================================================
!       produit vectoriel
!================================================================
  FUNCTION AD_cross_product(v1,v2) RESULT(v3)
    IMPLICIT NONE

    real (kind=Rkind) :: v3(3)

    real (kind=Rkind), intent(in)    :: v1(3)
    real (kind=Rkind), intent(in)    :: v2(3)

    v3(1) = v1(2)*v2(3) - v1(3)*v2(2)
    v3(2) =-v1(1)*v2(3) + v1(3)*v2(1)
    v3(3) = v1(1)*v2(2) - v1(2)*v2(1)

  END FUNCTION AD_cross_product
END MODULE ADdnSVM_dnVec_m
