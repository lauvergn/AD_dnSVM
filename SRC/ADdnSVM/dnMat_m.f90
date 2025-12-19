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
!> @brief Module which deals with derivatives of a matrix.
!!
!! This module deals with operations or functions of a matrix function and its derivatives, dnMat.
!!
!! There is a mapping between the matrix function M, its derivatives and the dnM derived type components:
!!
!! @li M(:,:)                   => M%d0(:,:)
!! @li dM(:,:)/dQ_i             => M%d1(:,:,i)
!! @li d^2M(:,:)/dQ_idQ_j       => M%d2(:,:,i,j)
!! @li d^3M(:,:)/dQ_idQ_jdQ_k   => M%d3(:,:,i,j,k)
!!
!! with M defined as:
!!  TYPE (dnMat_t) :: M
!!
!!
!! Some standard fortran operators (= + - * **) are overloaded (!!! not /):
!!
!! For instance the sum (+) of two dnMat variables, M1 and M2 correspond to:
!! @li (M1+M2)                 => M1%d0    + M2%d0
!! @li d(M1+M2)/dQ_i           => M1%d1(:,:,i) + M2%d1(:,:,i)
!! @li ....
!!
!!
!! @author David Lauvergnat
!! @date 09/08/2017
!!
MODULE ADdnSVM_dnMat_m
  USE QDUtil_m, ONLY : Rkind
  IMPLICIT NONE
  PRIVATE

  TYPE dnMat_t
     integer                        :: nderiv = -1

     real (kind=Rkind), allocatable :: d0(:,:)
     real (kind=Rkind), allocatable :: d1(:,:,:)
     real (kind=Rkind), allocatable :: d2(:,:,:,:)
     real (kind=Rkind), allocatable :: d3(:,:,:,:,:)

  CONTAINS
    PROCEDURE, PRIVATE :: AD_sub_dnMat2_TO_dnMat1
    PROCEDURE, PRIVATE :: AD_set_dnMat_TO_R
    PROCEDURE, PRIVATE :: AD_set_dnMat_FROM_MatOFdnS
    PROCEDURE, PRIVATE :: AD_set_dnMat_FROM_Mat
    GENERIC,   PUBLIC  :: assignment(=) => AD_set_dnMat_TO_R,                  &
                                           AD_set_dnMat_FROM_MatOFdnS,         &
                                           AD_set_dnMat_FROM_Mat
  END TYPE dnMat_t

  PUBLIC :: dnMat_t,alloc_dnMat,dealloc_dnMat,set_dnMat,Write_dnMat,Check_NotAlloc_dnMat
  PUBLIC :: transpose,matmul,operator (*),operator (**),operator (+),operator (-)
  PUBLIC :: DIAG_dnMat,SYM_dnMat
  PUBLIC :: submatrix_dnMat2_TO_dnMat1,dnS_TO_dnMat,dnMat_TO_dnS
  PUBLIC :: Mat_wADDTO_dnMat2_ider
  PUBLIC :: Check_dnMat_IS_ZERO,get_maxval_OF_dnMat
  PUBLIC :: get_nderiv,get_nVar,get_nsurf
  PUBLIC :: get_d0,get_d1,get_d2,get_d3,get_Flatten

  INTERFACE transpose
    MODULE PROCEDURE AD_TRANSPOSE_dnMat
  END INTERFACE
  INTERFACE matmul
    MODULE PROCEDURE AD_MATMUL_dnMat1_dnMat2,AD_MATMUL_dnMat1_Mat2, &
                     AD_MATMUL_Mat1_dnMat2
  END INTERFACE
  INTERFACE operator (*)
    MODULE PROCEDURE AD_sub_dnMat_TIME_R,AD_sub_R_TIME_dnMat
  END INTERFACE
  INTERFACE operator (**)
    MODULE PROCEDURE AD_sub_dnMat_EXP_R
  END INTERFACE
  INTERFACE operator (+)
    MODULE PROCEDURE AD_dnMat2_PLUS_dnMat1,AD_sub_dnMat_PLUS_R,AD_sub_R_PLUS_dnMat
  END INTERFACE
  INTERFACE operator (-)
    MODULE PROCEDURE AD_dnMat2_MINUS_dnMat1,AD_sub_dnMat_MINUS_R,AD_sub_R_MINUS_dnMat
  END INTERFACE

  INTERFACE alloc_dnMat
    MODULE PROCEDURE AD_alloc_dnMat
  END INTERFACE

  INTERFACE dealloc_dnMat
    MODULE PROCEDURE AD_dealloc_dnMat
  END INTERFACE

  INTERFACE set_dnMat
    MODULE PROCEDURE AD_set_dnMat
  END INTERFACE

  INTERFACE Write_dnMat
    MODULE PROCEDURE AD_Write_dnMat
  END INTERFACE

  INTERFACE DIAG_dnMat
    MODULE PROCEDURE AD_DIAG_dnMat
  END INTERFACE
  INTERFACE SYM_dnMat
    MODULE PROCEDURE AD_SYM_dnMat
  END INTERFACE

  INTERFACE submatrix_dnMat2_TO_dnMat1
    MODULE PROCEDURE AD_sub_Reduced_dnMat2_TO_dnMat1
  END INTERFACE
  INTERFACE dnS_TO_dnMat
    MODULE PROCEDURE AD_sub_dnS_TO_dnMat
  END INTERFACE
  INTERFACE dnMat_TO_dnS
    MODULE PROCEDURE AD_sub_dnMat_TO_dnS
  END INTERFACE
  INTERFACE Mat_wADDTO_dnMat2_ider
    MODULE PROCEDURE AD_Mat_wADDTO_dnMat2_ider
  END INTERFACE



  INTERFACE Check_dnMat_IS_ZERO
    MODULE PROCEDURE AD_Check_dnMat_IS_ZERO
  END INTERFACE
  INTERFACE get_maxval_OF_dnMat
    MODULE PROCEDURE AD_get_maxval_OF_dnMat
  END INTERFACE
  INTERFACE Check_NotAlloc_dnMat
    MODULE PROCEDURE AD_Check_NotAlloc_dnMat
  END INTERFACE

  INTERFACE get_nderiv
    MODULE PROCEDURE AD_get_nderiv_FROM_dnMat
  END INTERFACE
  INTERFACE get_nVar
    MODULE PROCEDURE AD_get_nVar_FROM_dnMat
  END INTERFACE
  INTERFACE get_nsurf
    MODULE PROCEDURE AD_get_nsurf_FROM_dnMat
  END INTERFACE
  INTERFACE get_sizeC
    MODULE PROCEDURE AD_get_sizeC_FROM_dnMat
  END INTERFACE
  INTERFACE get_sizeL
    MODULE PROCEDURE AD_get_sizeL_FROM_dnMat
  END INTERFACE

  INTERFACE get_d0
     MODULE PROCEDURE AD_get_d0_FROM_dnMat
  END INTERFACE
  INTERFACE get_d1
     MODULE PROCEDURE AD_get_d1_FROM_dnMat
  END INTERFACE
  INTERFACE get_d2
     MODULE PROCEDURE AD_get_d2_FROM_dnMat
  END INTERFACE
  INTERFACE get_d3
     MODULE PROCEDURE AD_get_d3_FROM_dnMat
  END INTERFACE

  INTERFACE get_Flatten
     MODULE PROCEDURE AD_get_Flatten_dnMat
  END INTERFACE

CONTAINS
!> @brief Public subroutine which allocates a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 21/06/2018
!!
!! @param Mat                TYPE (dnMat_t):        derived type which deals with the derivatives of a matrix.
!! @param nsurf              integer (optional):    number of electronic surfaces.
!! @param nVar               integer (optional):    number of coordinates (for the derivatives).
!! @param nderiv             integer (optional):    it enables to chose the derivative order (from 0 to 2).
!! @param err_dnMat          integer (optional):    to handle the errors errors (0: no error).
!! @param name_var           character (optional):  Name of the variable from the calling subroutine (debuging purpose).
!! @param name_sub           character (optional):  Name of the calling subroutine (debuging purpose).
  SUBROUTINE AD_alloc_dnMat(Mat,nsurf,sizeL,sizeC,nVar,nderiv,name_var,name_sub,err_dnMat)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnMat_t),    intent(inout)         :: Mat   !< derived type, which contains, matrix potential, its derivatives

    integer,           intent(in),  optional :: nsurf !< number of electronic surfaces
    integer,           intent(in),  optional :: sizeL,sizeC !< numbers of lines and columns

    integer,           intent(in),  optional :: nVar  !< number of coordinates (for the derivatives)
    integer,           intent(in),  optional :: nderiv  !< order of the derivatives [0,1,2]
    character (len=*), intent(in),  optional :: name_var,name_sub
    integer,           intent(out), optional :: err_dnMat  !< to handle the errors

    ! local variables
    integer :: sizeL_loc,sizeC_loc,nVar_loc,err_dnMat_loc,nderiv_loc



    err_dnMat_loc = 0 ! no error

    CALL AD_dealloc_dnMat(Mat,err_dnMat_loc)
    IF (err_dnMat_loc /= 0) THEN
      write(out_unit,*) ' ERROR in AD_alloc_dnMat'
      write(out_unit,*) ' Problem in AD_dealloc_dnMat CALL in AD_alloc_dnMat'
      IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
      IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
      IF (present(err_dnMat)) THEN
        err_dnMat = err_dnMat_loc
        RETURN
      ELSE
        STOP 'Problem in AD_dealloc_dnMat CALL in AD_alloc_dnMat'
      END IF
    END IF

    ! test nsurf, sizeL, sizeL
    !       T      F       F    => ok
    !       F      T       T    => ok
    !   all other posiblities are wrong
    IF ( (present(nsurf) .AND. .NOT. present(sizeL) .AND. .NOT. present(sizeC) ) .OR. &
         (.NOT. present(nsurf) .AND. present(sizeL) .AND. present(sizeC) ) ) THEN
      CONTINUE ! the two possiblities
      !write(out_unit,*) 'present(nsurf): ',present(nsurf)
      !write(out_unit,*) 'present(sizeL): ',present(sizeL)
      !write(out_unit,*) 'present(sizeC): ',present(sizeC)
    ELSE
      write(out_unit,*) ' ERROR in AD_alloc_dnMat'
      write(out_unit,*) ' wrong parameter presence:'
      write(out_unit,*) 'present(nsurf): ',present(nsurf)
      write(out_unit,*) 'present(sizeL): ',present(sizeL)
      write(out_unit,*) 'present(sizeC): ',present(sizeC)
      write(out_unit,*)
      write(out_unit,*) ' The two correct possibilities are:'
      write(out_unit,*) '----------------------------------------------'
      write(out_unit,*) ' present(nsurf) present(sizeL) present(sizeC)'
      write(out_unit,*) '    true           false           false'
      write(out_unit,*) '    false          true            true'
      write(out_unit,*) '----------------------------------------------'

      IF (present(err_dnMat)) THEN
        err_dnMat = 1
        RETURN
      ELSE
        STOP 'ERROR in AD_alloc_dnMat: wrong parameter presence,nsurf and sizeL or sizeC'
      END IF
    END IF

    IF (present(nsurf)) THEN
      sizeL_loc = nsurf
      sizeC_loc = nsurf
    ELSE IF (present(sizeL) .AND. present(sizeC)) THEN
      sizeL_loc = sizeL
      sizeC_loc = sizeC
    ELSE
      sizeL_loc = 1
      sizeC_loc = 1
    END IF
    IF (sizeL_loc < 1 .OR. sizeC_loc < 1) THEN
      write(out_unit,*) ' ERROR in AD_alloc_dnMat'
      write(out_unit,*) '  sizeL_loc < 0 or sizeC_loc < 0',sizeL_loc,sizeC_loc
      IF (present(err_dnMat)) THEN
        err_dnMat = 1
        RETURN
      ELSE
        STOP 'ERROR in AD_alloc_dnMat: sizeL < 0 or sizeC < 0'
      END IF
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
    Mat%nderiv = nderiv_loc

    !write(out_unit,*) 'Mat%nderiv in alloc_dnMat',Mat%nderiv

    allocate(Mat%d0(sizeL_loc,sizeC_loc),stat=err_dnMat_loc)
    IF (err_dnMat_loc /= 0) THEN
      write(out_unit,*) ' ERROR in AD_alloc_dnMat'
      write(out_unit,*) '  Problem with allocate of Mat%d0'
      IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
      IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
      IF (present(err_dnMat)) THEN
        err_dnMat = err_dnMat_loc
        RETURN
      ELSE
        STOP 'Problem with allocate in AD_alloc_dnMat'
      END IF
    END IF

    IF (nderiv_loc >= 1) THEN
      allocate(Mat%d1(sizeL_loc,sizeC_loc,nVar_loc),stat=err_dnMat_loc)
      IF (err_dnMat_loc /= 0) THEN
        write(out_unit,*) ' ERROR in AD_alloc_dnMat'
        write(out_unit,*) '  Problem with allocate of Mat%d1'
        write(out_unit,*) '  nVar > 0?',nVar_loc
        IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
        IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
        IF (present(err_dnMat)) THEN
          err_dnMat = err_dnMat_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnMat'
        END IF
      END IF
    END IF

    IF (nderiv_loc >= 2) THEN
      allocate(Mat%d2(sizeL_loc,sizeC_loc,nVar_loc,nVar_loc),stat=err_dnMat_loc)
      IF (err_dnMat_loc /= 0) THEN
        write(out_unit,*) ' ERROR in AD_alloc_dnMat'
        write(out_unit,*) '  Problem with allocate of Mat%d2'
        write(out_unit,*) '  nVar > 0',nVar_loc
        IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
        IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
        IF (present(err_dnMat)) THEN
          err_dnMat = err_dnMat_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnMat'
        END IF
      END IF
    END IF

    IF (nderiv_loc >= 3) THEN
      allocate(Mat%d3(sizeL_loc,sizeC_loc,nVar_loc,nVar_loc,nVar_loc),stat=err_dnMat_loc)
      IF (err_dnMat_loc /= 0) THEN
        write(out_unit,*) ' ERROR in AD_alloc_dnMat'
        write(out_unit,*) '  Problem with allocate of Mat%d2'
        write(out_unit,*) '  nVar > 0',nVar_loc
        IF (present(name_var)) write(out_unit,*) '  for the variable: ',name_var
        IF (present(name_sub)) write(out_unit,*) '  call from the subroutine: ',name_sub
        IF (present(err_dnMat)) THEN
          err_dnMat = err_dnMat_loc
          RETURN
        ELSE
          STOP 'Problem with allocate in AD_alloc_dnMat'
        END IF
      END IF
    END IF

  END SUBROUTINE AD_alloc_dnMat
!> @brief Public subroutine which deallocates a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 21/06/2018
!!
!! @param Mat                TYPE (dnMat_t):        derived type which deals with the derivatives of a matrix.
!! @param err_dnMat       integer (optional):    to handle the errors errors (0: no error).
  SUBROUTINE AD_dealloc_dnMat(Mat,err_dnMat)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnMat_t), intent(inout)         :: Mat        !< derived type, which contains, matrix potential, its derivatives
    integer,        intent(out), optional :: err_dnMat  !< to handle the errors

    ! local variables
    integer :: err_dnMat_loc

    err_dnMat_loc = 0
    IF (present(err_dnMat)) err_dnMat = 0

    IF (allocated(Mat%d0)) THEN
      deallocate(Mat%d0,stat=err_dnMat_loc)
      IF (err_dnMat_loc /= 0) THEN
        write(out_unit,*) ' ERROR in dealloc_dnMat'
        write(out_unit,*) '  Problem with deallocate of Mat%d0'
        IF (present(err_dnMat)) THEN
          err_dnMat = err_dnMat_loc
          RETURN
        ELSE
          STOP 'Problem with deallocate in dealloc_dnMat'
        END IF
      END IF
    END IF

    IF (allocated(Mat%d1)) THEN
      deallocate(Mat%d1,stat=err_dnMat_loc)
      IF (err_dnMat_loc /= 0) THEN
        write(out_unit,*) ' ERROR in dealloc_dnMat'
        write(out_unit,*) '  Problem with deallocate of Mat%d1'
        IF (present(err_dnMat)) THEN
          err_dnMat = err_dnMat_loc
          RETURN
        ELSE
          STOP 'Problem with deallocate in dealloc_dnMat'
        END IF
      END IF
    END IF

    IF (allocated(Mat%d2)) THEN
      deallocate(Mat%d2,stat=err_dnMat_loc)
      IF (err_dnMat_loc /= 0) THEN
        write(out_unit,*) ' ERROR in dealloc_dnMat'
        write(out_unit,*) '  Problem with deallocate of Mat%d2'
        IF (present(err_dnMat)) THEN
          err_dnMat = err_dnMat_loc
          RETURN
        ELSE
          STOP 'Problem with deallocate in dealloc_dnMat'
        END IF
      END IF
    END IF

    IF (allocated(Mat%d3)) THEN
      deallocate(Mat%d3,stat=err_dnMat_loc)
      IF (err_dnMat_loc /= 0) THEN
        write(out_unit,*) ' ERROR in dealloc_dnMat'
        write(out_unit,*) '  Problem with deallocate of Mat%d3'
        IF (present(err_dnMat)) THEN
          err_dnMat = err_dnMat_loc
          RETURN
        ELSE
          STOP 'Problem with deallocate in dealloc_dnMat'
        END IF
      END IF
    END IF

    Mat%nderiv = -1

  END SUBROUTINE AD_dealloc_dnMat
  SUBROUTINE AD_check_dim_dnMat(Mat,err_dnMat)
    USE QDUtil_m
    IMPLICIT NONE

    TYPE (dnMat_t),    intent(inout)   :: Mat   !< derived type, which contains, matrix potential, its derivatives
    integer,           intent(out)     :: err_dnMat  !< to handle the errors

    ! local variables
    integer :: dim0(2),dim1(3),dim2(4),dim3(5)

    err_dnMat = 0 ! no error

    dim0 = 0
    IF (allocated(Mat%d0)) dim0 = shape(Mat%d0)
    dim1 = 0
    IF (allocated(Mat%d1)) THEN 
      dim1 = shape(Mat%d1)
      IF (any(dim0 /= dim1(1:2))) err_dnMat = 1
    END IF
    dim2 = 0
    IF (allocated(Mat%d2)) THEN
      dim2 = shape(Mat%d2)
      IF (any(dim0 /= dim2(1:2))) err_dnMat = 1
      IF (dim2(3) /= dim2(4) .OR. dim2(3) /= dim1(3)) err_dnMat = 2
    END IF
    dim3 = 0
    IF (allocated(Mat%d3)) THEN 
      dim3 = shape(Mat%d3)
      IF (any(dim0 /= dim3(1:2))) err_dnMat = 1
      IF (dim3(3) /= dim3(4) .OR. dim3(3) /= dim3(5) .OR. dim3(3) /= dim1(3)) err_dnMat = 2
    END IF

    IF (err_dnMat /= 0) THEN
      IF (allocated(Mat%d0)) write(out_unit,*) 'shape(Mat%d0):',shape(Mat%d0)
      IF (allocated(Mat%d1)) write(out_unit,*) 'shape(Mat%d1):',shape(Mat%d1)
      IF (allocated(Mat%d2)) write(out_unit,*) 'shape(Mat%d2):',shape(Mat%d2)
      IF (allocated(Mat%d3)) write(out_unit,*) 'shape(Mat%d3):',shape(Mat%d3)
    END IF

  END SUBROUTINE AD_check_dim_dnMat
!> @brief Public subroutine which copies two "dnMat" derived types.
!!
!> @author David Lauvergnat
!! @date 21/06/2018
!!
!! @param dnMat1                TYPE (dnMat_t):     derived type which deals with the derivatives of a matrix.
!! @param dnMat2                TYPE (dnMat_t):     derived type which deals with the derivatives of a matrix.
  SUBROUTINE AD_sub_dnMat2_TO_dnMat1(dnMat1,dnMat2)
    USE QDUtil_m
    IMPLICIT NONE

    CLASS (dnMat_t), intent(inout) :: dnMat1
    CLASS (dnMat_t), intent(in)    :: dnMat2

    integer :: nderiv_loc,sizeL_loc,sizeC_loc,nVar_loc
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_sub_dnMat2_TO_dnMat1'

    nderiv_loc = AD_get_nderiv_FROM_dnMat(dnMat2)
    sizeL_loc  = AD_get_sizeL_FROM_dnMat(dnMat2)
    sizeC_loc  = AD_get_sizeC_FROM_dnMat(dnMat2)
    nVar_loc   = AD_get_nVar_FROM_dnMat(dnMat2)

    !write(out_unit,*) 'in ',name_sub,' nVar,sizeL,sizeC,nderiv',nVar_loc,nsurf_loc,nderiv_loc


    IF (nderiv_loc < 0 .OR. sizeL_loc < 1 .OR. sizeC_loc < 1 .OR. (nderiv_loc > 0  .AND. nVar_loc < 1)) RETURN

    dnMat1%nderiv = dnMat2%nderiv

    IF (nderiv_loc == 0) THEN
       dnMat1%d0 = dnMat2%d0
    ELSE IF (nderiv_loc == 1) THEN
       dnMat1%d0 = dnMat2%d0
       dnMat1%d1 = dnMat2%d1
    ELSE IF (nderiv_loc == 2) THEN
       dnMat1%d0 = dnMat2%d0
       dnMat1%d1 = dnMat2%d1
       dnMat1%d2 = dnMat2%d2
    ELSE IF (nderiv_loc == 3) THEN
       dnMat1%d0 = dnMat2%d0
       dnMat1%d1 = dnMat2%d1
       dnMat1%d2 = dnMat2%d2
       dnMat1%d3 = dnMat2%d3
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END SUBROUTINE AD_sub_dnMat2_TO_dnMat1
  SUBROUTINE AD_sub_Reduced_dnMat2_TO_dnMat1(dnMat1,dnMat2,lb,ub)
    USE QDUtil_m
    IMPLICIT NONE

    CLASS (dnMat_t),  intent(inout) :: dnMat1
    CLASS (dnMat_t),  intent(in)    :: dnMat2
    integer,          intent(in)    :: lb,ub

    integer :: nderiv_loc,nsurf_loc,nVar_loc
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_sub_Reduced_dnMat2_TO_dnMat1'

    nderiv_loc = AD_get_nderiv_FROM_dnMat(dnMat2)
    nsurf_loc  = AD_get_nsurf_FROM_dnMat(dnMat2)
    nVar_loc   = AD_get_nVar_FROM_dnMat(dnMat2)

    !write(out_unit,*) 'in ',name_sub,' nVar,nsurf,nderiv',nVar_loc,nsurf_loc,nderiv_loc


    IF (nderiv_loc < 0 .OR. nsurf_loc < 1 .OR. (nderiv_loc > 0  .AND. nVar_loc < 1)) RETURN

    IF (lb < 1 .OR. ub > nsurf_loc .OR. lb > ub) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The indexes lb and ub are wrong.'
      write(out_unit,*) 'lb,ub',lb,ub
      write(out_unit,*) 'The range is [1...',nsurf_loc,']'
      write(out_unit,*) 'It should never append! Check the source'
      STOP 'ERROR in AD_sub_Reduced_dnMat2_TO_dnMat1: lb or ub are wrong'
    END IF

    dnMat1%nderiv = dnMat2%nderiv

    IF (nderiv_loc == 0) THEN
       dnMat1%d0 = dnMat2%d0(lb:ub,lb:ub)
    ELSE IF (nderiv_loc == 1) THEN
       dnMat1%d0 = dnMat2%d0(lb:ub,lb:ub)
       dnMat1%d1 = dnMat2%d1(lb:ub,lb:ub,:)
    ELSE IF (nderiv_loc == 2) THEN
       dnMat1%d0 = dnMat2%d0(lb:ub,lb:ub)
       dnMat1%d1 = dnMat2%d1(lb:ub,lb:ub,:)
       dnMat1%d2 = dnMat2%d2(lb:ub,lb:ub,:,:)
    ELSE IF (nderiv_loc == 3) THEN
       dnMat1%d0 = dnMat2%d0(lb:ub,lb:ub)
       dnMat1%d1 = dnMat2%d1(lb:ub,lb:ub,:)
       dnMat1%d2 = dnMat2%d2(lb:ub,lb:ub,:,:)
       dnMat1%d3 = dnMat2%d3(lb:ub,lb:ub,:,:,:)
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END SUBROUTINE AD_sub_Reduced_dnMat2_TO_dnMat1
!> @brief Public subroutine which copies a dnS derived type to one element of dnMat derived type.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param Mat                   TYPE (dnMat_t):    derived type which deals with the derivatives of a matrix.
!! @param S                     TYPE(dnS):       derived type which deals with the derivatives of a scalar.
!! @param i,j                   integer (optional) indices of the matrix element. If not present i=j=1
  SUBROUTINE AD_sub_dnS_TO_dnMat(S,Mat,i,j)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnMat_t),     intent(inout) :: Mat
    TYPE (dnS_t),       intent(in)    :: S
    integer, optional,  intent(in)    :: i,j

    integer :: nderiv_dnMat,sizeL_loc,sizeC_loc,nVar_dnMat,nderiv_dnS,nVar_dnS
    integer :: i_loc,j_loc

    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_sub_dnS_TO_dnMat'


    nderiv_dnS = get_nderiv(S)
    nVar_dnS   = get_nVar(S)

    nderiv_dnMat = AD_get_nderiv_FROM_dnMat(Mat)
    sizeL_loc    = AD_get_sizeL_FROM_dnMat(Mat)
    sizeC_loc    = AD_get_sizeC_FROM_dnMat(Mat)
    nVar_dnMat   = AD_get_nVar_FROM_dnMat(Mat)

    i_loc = 1
    j_loc = 1
    IF (present(i)) i_loc = i
    IF (present(j)) j_loc = j


    IF (i_loc < 1 .OR. i_loc > sizeL_loc .OR. j_loc < 1 .OR. j_loc > sizeC_loc) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The matrix line   index, (',i_loc,') is out of range [1...',sizeL_loc,']'
      write(out_unit,*) ' The matrix column index, (',j_loc,') is out of range [1...',sizeC_loc,']'
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF

    IF (nderiv_dnS == -1) THEN
      IF (nderiv_dnMat == -1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' dnMat is not allocated.'
        write(out_unit,*) 'It should never append! Check the source.'
        STOP 'dnMat is not allocated.'
      END IF
      ! S (dnS) is a constant
      ! value
      Mat%d0(i_loc,j_loc) = get_d0(S)

      ! 1st order derivatives
      IF (nderiv_dnMat >= 1) Mat%d1(i_loc,j_loc,:) = ZERO

      ! 2d order derivatives
      IF (nderiv_dnMat >= 2) Mat%d2(i_loc,j_loc,:,:) = ZERO
    ELSE

      IF ( AD_check_notalloc_dnmat(Mat,nderiv_dnS) .OR.                  &
           nderiv_dnS /= nderiv_dnMat  .OR.  nVar_dnS /= nVar_dnMat .OR.  &
           sizeL_loc < 1 .OR. sizeC_loc < 1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' dnMat is not allocated or ...'
        write(out_unit,*) '  ... nderiv from dnMat or dnS are different or ...'
        write(out_unit,*) '  ... nVar from dnMat or dnS are different or ...'
        write(out_unit,*) '  ... sizeL,sizeC from dnMat are < 1'

        write(out_unit,*) 'nderiv from dnMat and dnS:',nderiv_dnMat,nderiv_dnS
        write(out_unit,*) 'nVar   from dnMat and dnS:',nVar_dnMat,nVar_dnS
        write(out_unit,*) 'sizeL,sizeC  from dnMat  :',sizeL_loc,sizeC_loc

        write(out_unit,*) 'It should never append! Check the source'
        STOP 'dnMat is not allocated or inconsistent nVar,nderiv parameters.'
      END IF

      ! value
      Mat%d0(i_loc,j_loc) = get_d0(S)

      ! 1st order derivatives
      IF (nderiv_dnS >= 1) THEN
        CALL sub_get_dn(S,d1=Mat%d1(i_loc,j_loc,:))
      END IF

      ! 2d order derivatives
      IF (nderiv_dnS >= 2) then
        CALL sub_get_dn(S,d2=Mat%d2(i_loc,j_loc,:,:))
      END IF

      ! 3d order derivatives
      IF (nderiv_dnS >= 3) then
        CALL sub_get_dn(S,d3=Mat%d3(i_loc,j_loc,:,:,:))
      END IF

    END IF

  END SUBROUTINE AD_sub_dnS_TO_dnMat
!> @brief Public subroutine which copies a dnS derived type to one element of dnMat derived type.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param Mat                   TYPE (dnMat_t):    derived type which deals with the derivatives of a matrix.
!! @param S                     TYPE(dnS):       derived type which deals with the derivatives of a scalar.
!! @param i,j                   integer (optional) indices of the matrix element. If not present i=j=1
  SUBROUTINE AD_sub_dnMat_TO_dnS(Mat,S,i,j)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    TYPE (dnMat_t),     intent(in)    :: Mat
    TYPE (dnS_t),       intent(inout) :: S
    integer, optional,  intent(in)    :: i,j

    integer :: nderiv_dnMat,sizeL_loc,sizeC_loc,nVar_dnMat,nderiv_dnS,nVar_dnS
    integer :: i_loc,j_loc

    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_sub_dnMat_TO_dnS'


    nderiv_dnS = get_nderiv(S)
    nVar_dnS   = get_nVar(S)

    nderiv_dnMat = AD_get_nderiv_FROM_dnMat(Mat)
    sizeL_loc    = AD_get_sizeL_FROM_dnMat(Mat)
    sizeC_loc    = AD_get_sizeC_FROM_dnMat(Mat)
    nVar_dnMat   = AD_get_nVar_FROM_dnMat(Mat)

    i_loc = 1
    j_loc = 1
    IF (present(i)) i_loc = i
    IF (present(j)) j_loc = j


    IF (i_loc < 1 .OR. i_loc > sizeL_loc .OR. j_loc < 1 .OR. j_loc > sizeC_loc) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The matrix line   index, (',i_loc,') is out of range [1...',sizeL_loc,']'
      write(out_unit,*) ' The matrix column index, (',j_loc,') is out of range [1...',sizeC_loc,']'
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF

    IF (nderiv_dnMat == -1) THEN
      IF (nderiv_dnS == -1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' dnS is not allocated.'
        write(out_unit,*) 'It should never append! Check the source.'
        STOP 'dnS is not allocated.'
      END IF
      ! dnMat is a constant
      S = Mat%d0(i_loc,j_loc)
      !CALL set_dnS_TO_R(S,Mat%d0(i_loc,j_loc))
    ELSE

      IF ( AD_check_notalloc_dnmat(Mat,nderiv_dnS) .OR. sizeL_loc < 1 .OR. sizeC_loc < 1) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' dnMat is not allocated or ...'
        write(out_unit,*) '  ... sizeL or sizeC from dnMat are < 1'
        write(out_unit,*) 'sizeL, sizeC  from dnMat        :',sizeL_loc,sizeC_loc

        write(out_unit,*) 'It should never append! Check the source'
        STOP 'dnMat is not allocated or inconsistent nsurf parameter.'
      END IF

      SELECT CASE (nderiv_dnMat)
      CASE (1)
        CALL set_dnS(S, d0=Mat%d0(i_loc,j_loc),       &
                        d1=Mat%d1(i_loc,j_loc,:))
      CASE (2)
        CALL set_dnS(S, d0=Mat%d0(i_loc,j_loc),       &
                        d1=Mat%d1(i_loc,j_loc,:),     &
                        d2=Mat%d2(i_loc,j_loc,:,:))
      CASE (3)
        CALL set_dnS(S, d0=Mat%d0(i_loc,j_loc),       &
                        d1=Mat%d1(i_loc,j_loc,:),     &
                        d2=Mat%d2(i_loc,j_loc,:,:),   &
                        d3=Mat%d3(i_loc,j_loc,:,:,:))
      CASE Default ! nderiv_dnS = -1, 0
        CALL set_dnS(S, d0=Mat%d0(i_loc,j_loc))
      END SELECT

    END IF

  END SUBROUTINE AD_sub_dnMat_TO_dnS

  SUBROUTINE AD_set_dnMat(dnMat,d0,d1,d2,d3)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    CLASS (dnMat_t),   intent(inout)         :: dnMat
    real (kind=Rkind), intent(in), optional  :: d0(:,:)
    real (kind=Rkind), intent(in), optional  :: d1(:,:,:)
    real (kind=Rkind), intent(in), optional  :: d2(:,:,:,:)
    real (kind=Rkind), intent(in), optional  :: d3(:,:,:,:,:)

    integer :: err_dim
    character (len=*), parameter :: name_sub='AD_set_dnMat'

    IF (present(d0)) THEN
      dnMat%d0     = d0
      dnMat%nderiv = 0
    END IF

    IF (present(d1)) THEN
      dnMat%d1     = d1
      dnMat%nderiv = 1
      IF (.NOT. present(d0)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d1 is present but not d0'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnMat'
      END IF
    END IF

    IF (present(d2)) THEN
      dnMat%d2     = d2
      dnMat%nderiv = 2
      IF (.NOT. present(d1)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d2 is present but not d1'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnMat'
      END IF
    END IF

    IF (present(d3)) THEN
      dnMat%d3     = d3
      dnMat%nderiv = 3
      IF (.NOT. present(d2)) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' d3 is present but not d2'
        write(out_unit,*) ' CHECK the fortran!!'
        STOP 'ERROR in AD_set_dnMat'
      END IF
    END IF

    err_dim = 0
    CALL AD_check_dim_dnMat(dnMat,err_dim)
    IF (err_dim /= 0) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' inconsistent dimensions of dnM'
      STOP 'ERROR in AD_set_dnMat: inconsistent dimensions'
    END IF
  
  END SUBROUTINE AD_set_dnMat
!> @brief Public subroutine which copies a dnS derived type to one element of dnMat derived type.
!!
!> @author David Lauvergnat
!! @date 30/07/2019
!!
!! @param Mat                   TYPE (dnMat_t):    derived type which deals with the derivatives of a matrix.
!! @param MatOFS                TYPE(dnS):       matrix of derived type which deals with the derivatives of a scalar.
  SUBROUTINE AD_set_dnMat_FROM_MatOFdnS(Mat,MatOFS)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    CLASS (dnMat_t),   intent(inout) :: Mat
    TYPE (dnS_t),      intent(in)    :: MatOFS(:,:)

    integer :: sizeL,sizeC,nderiv_dnS,nVar_dnS
    integer :: i,j

    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_set_dnMat_FROM_MatOFdnS'

    i=lbound(MatOFS,dim=1)
    j=lbound(MatOFS,dim=2)

    sizeL = size(MatOFS,dim=1)
    sizeC = size(MatOFS,dim=2)
    nVar_dnS   = get_nVar(MatOFS(i,j))
    nderiv_dnS = get_nderiv(MatOFS(i,j))


    CALL alloc_dnMat(Mat,sizeL=sizeL, sizeC=sizeC, nVar=nVar_dnS, nderiv=nderiv_dnS)

    DO j=1,sizeC
    DO i=1,sizeL
      CALL AD_sub_dnS_TO_dnMat(MatOFS(i,j),Mat,i,j)
    END DO
    END DO

  END SUBROUTINE AD_set_dnMat_FROM_MatOFdnS
  SUBROUTINE AD_set_dnMat_FROM_Mat(dnMat,Mat)
    USE QDUtil_m
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    CLASS (dnMat_t),   intent(inout) :: dnMat
    real (kind=Rkind), intent(in)    :: Mat(:,:)

    character (len=*), parameter :: name_sub='AD_set_dnMat_FROM_Mat'

    dnMat%d0     = Mat
    dnMat%nderiv = 0
  
  END SUBROUTINE AD_set_dnMat_FROM_Mat
  SUBROUTINE AD_Mat_wADDTO_dnMat2_ider(Mat1,w1,dnMat2,ider)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_dnS_m
    IMPLICIT NONE

    real (kind=Rkind),  intent(in)            :: Mat1(:,:)
    TYPE (dnMat_t),     intent(inout)         :: dnMat2
    integer,            intent(in),  optional :: ider(:)
    real (kind=Rkind),  intent(in)            :: w1

    integer :: nderiv,sizeL,sizeC,nVar
    integer :: i1,i1i,i1f
    integer :: i2,i2i,i2f
    integer :: i3,i3i,i3f

    character (len=*), parameter :: name_sub='AD_Mat_wADDTO_dnMat2_ider'

    nderiv = AD_get_nderiv_FROM_dnMat(dnMat2)
    sizeL  = AD_get_sizeL_FROM_dnMat(dnMat2)
    sizeC  = AD_get_sizeC_FROM_dnMat(dnMat2)
    nVar   = AD_get_nVar_FROM_dnMat(dnMat2)

    IF (.NOT. allocated(dnMat2%d0)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) '  dnMat2%d0 is not allocated.'
      write(out_unit,*) ' CHECK the fortran source!!'
      STOP
    END IF

    IF (.NOT. all(shape(Mat1) == shape(dnMat2%d0))) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) '  The shape of Mat1 dnMat2%d0 must be equal.'
      write(out_unit,*) '  shape(Mat1):      ',shape(Mat1)
      write(out_unit,*) '  shape(dnMat2%d0): ',shape(dnMat2%d0)
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
        dnMat2%d0(:,:) = w1*Mat1 + dnMat2%d0

      CASE (1)
        DO i1=i1i,i1f
          dnMat2%d1(:,:,i1) = w1*Mat1 + dnMat2%d1(:,:,i1)
        END DO

      CASE (2)
        DO i2=i2i,i2f
        DO i1=i1i,i1f
          dnMat2%d2(:,:,i1,i2) = w1*Mat1 + dnMat2%d2(:,:,i1,i2)
        END DO
        END DO

      CASE (3)

        !IF (present(ider)) write(6,*) 'ider',ider

        DO i3=i3i,i3f
        DO i2=i2i,i2f
        DO i1=i1i,i1f
          dnMat2%d3(:,:,i1,i2,i3) = w1*Mat1 + dnMat2%d3(:,:,i1,i2,i3)
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
      dnMat2%d0(:,:) = w1*Mat1 + dnMat2%d0
    END IF

  END SUBROUTINE AD_Mat_wADDTO_dnMat2_ider

!> @brief Public function which calculate set dnMat to zero (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                   TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param set_dnMat_TO_zero  TYPE (dnMat_t) (result):  dnMat derived type
  SUBROUTINE AD_set_dnMat_TO_zero(dnMat)
    USE QDUtil_m, ONLY : ZERO, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t), intent(inout) :: dnMat

    character (len=*), parameter :: name_sub='AD_set_dnMat_TO_zero'

    CALL AD_set_dnMat_TO_R(dnMat,ZERO)

  END SUBROUTINE AD_set_dnMat_TO_zero
!> @brief Public function which calculate set dnMat to R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param R                  real:                     some real number
!! @param set_dnMat_TO_R  TYPE (dnMat_t) (result):  dnMat derived type
  SUBROUTINE AD_set_dnMat_TO_R(dnMat,R)
    USE QDUtil_m, ONLY : Rkind, ZERO, out_unit
    IMPLICIT NONE

    CLASS (dnMat_t), intent(inout) :: dnMat
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_set_dnMat_TO_R'

    nderiv_loc = AD_get_nderiv_FROM_dnMat(dnMat)

    !write(out_unit,*) 'nderiv',nderiv_loc


    IF (nderiv_loc == 0) THEN
       dnMat%d0 = R
    ELSE IF (nderiv_loc == 1) THEN
       dnMat%d0 = R
       dnMat%d1 = ZERO
    ELSE IF (nderiv_loc == 2) THEN
       dnMat%d0 = R
       dnMat%d1 = ZERO
       dnMat%d2 = ZERO
    ELSE IF (nderiv_loc == 3) THEN
       dnMat%d0 = R
       dnMat%d1 = ZERO
       dnMat%d2 = ZERO
       dnMat%d3 = ZERO
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 or nderiv < 0 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END SUBROUTINE AD_set_dnMat_TO_R
!> @brief Public function which calculate dnMat*R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param R                  real:                     some real number
!! @param sub_dnMat_TIME_R TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_sub_dnMat_TIME_R(dnMat,R) RESULT (sub_dnMat_TIME_R)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                 :: sub_dnMat_TIME_R
    TYPE (dnMat_t),    intent(in)  :: dnMat
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_sub_dnMat_TIME_R'

    nderiv_loc = AD_get_nderiv_FROM_dnMat(dnMat)

    !write(out_unit,*) 'nderiv',nderiv_loc

    sub_dnMat_TIME_R%nderiv = dnMat%nderiv

    IF (nderiv_loc == 0) THEN
       sub_dnMat_TIME_R%d0 = dnMat%d0 * R

    ELSE IF (nderiv_loc == 1) THEN
       sub_dnMat_TIME_R%d0 = dnMat%d0 * R
       sub_dnMat_TIME_R%d1 = dnMat%d1 * R

    ELSE IF (nderiv_loc == 2) THEN
       sub_dnMat_TIME_R%d0 = dnMat%d0 * R
       sub_dnMat_TIME_R%d1 = dnMat%d1 * R
       sub_dnMat_TIME_R%d2 = dnMat%d2 * R
    ELSE IF (nderiv_loc == 3) THEN
       sub_dnMat_TIME_R%d0 = dnMat%d0 * R
       sub_dnMat_TIME_R%d1 = dnMat%d1 * R
       sub_dnMat_TIME_R%d2 = dnMat%d2 * R
       sub_dnMat_TIME_R%d3 = dnMat%d3 * R
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_sub_dnMat_TIME_R
!> @brief Public function which calculate R*dnMat (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param R                  real:                     some real number
!! @param sub_R_TIME_dnMat TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_sub_R_TIME_dnMat(R,dnMat)  RESULT(sub_R_TIME_dnMat)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                :: sub_R_TIME_dnMat
    TYPE (dnMat_t),   intent(in)  :: dnMat
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_sub_R_TIME_dnMat'

    nderiv_loc = AD_get_nderiv_FROM_dnMat(dnMat)

    !write(out_unit,*) 'nderiv',nderiv_loc

    sub_R_TIME_dnMat%nderiv = dnMat%nderiv

    IF (nderiv_loc == 0) THEN
       sub_R_TIME_dnMat%d0 = dnMat%d0 * R

    ELSE IF (nderiv_loc == 1) THEN
       sub_R_TIME_dnMat%d0 = dnMat%d0 * R
       sub_R_TIME_dnMat%d1 = dnMat%d1 * R

    ELSE IF (nderiv_loc == 2) THEN
       sub_R_TIME_dnMat%d0 = dnMat%d0 * R
       sub_R_TIME_dnMat%d1 = dnMat%d1 * R
       sub_R_TIME_dnMat%d2 = dnMat%d2 * R
    ELSE IF (nderiv_loc == 3) THEN
       sub_R_TIME_dnMat%d0 = dnMat%d0 * R
       sub_R_TIME_dnMat%d1 = dnMat%d1 * R
       sub_R_TIME_dnMat%d2 = dnMat%d2 * R
       sub_R_TIME_dnMat%d3 = dnMat%d3 * R
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_sub_R_TIME_dnMat
!> @brief Public function which calculate dnMat1+dnMat2 (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param dnMat1                    TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param dnMat2                    TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param dnMat2_PLUS_dnMat1 TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_dnMat2_PLUS_dnMat1(dnMat1,dnMat2)  RESULT(dnMat2_PLUS_dnMat1)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                :: dnMat2_PLUS_dnMat1
    TYPE (dnMat_t), intent(in)    :: dnMat1,dnMat2

    integer :: nderiv
    integer :: nVar1,nVar2
    integer :: sizeL1,sizeL2
    integer :: sizeC1,sizeC2

    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_dnMat2_PLUS_dnMat1'

    nderiv = min(AD_get_nderiv_FROM_dnMat(dnMat1),AD_get_nderiv_FROM_dnMat(dnMat2))

    nVar1   = AD_get_nVar_FROM_dnMat(dnMat1)
    nVar2   = AD_get_nVar_FROM_dnMat(dnMat2)

    sizeL1   = AD_get_sizeL_FROM_dnMat(dnMat1)
    sizeL2   = AD_get_sizeL_FROM_dnMat(dnMat2)
    sizeC1   = AD_get_sizeC_FROM_dnMat(dnMat1)
    sizeC2   = AD_get_sizeC_FROM_dnMat(dnMat2)


    !write(out_unit,*) 'in ',name_sub,' nsurf,nVar,nderiv',nsurf,nVar,nderiv

    CALL AD_dealloc_dnMat(dnMat2_PLUS_dnMat1)

    IF (nVar1 /= nVar2) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nVar1 and nVar2 differ'
      write(out_unit,*) ' nVar1,nVar2',nVar1,nVar2
      STOP 'ERROR in AD_dnMat2_PLUS_dnMat1: nVar1 and nVar2 differ'
    END IF

    IF (nderiv < 0 .OR. (nderiv > 0  .AND. nVar1 < 1)) RETURN

    IF (sizeL1 /= sizeL2 .OR. sizeC1 /= sizeC2) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The shapes of the matrices differ.'
      write(out_unit,*) ' sizeL1,sizeL2',sizeL1,sizeL2
      write(out_unit,*) ' sizeC1,sizeC2',sizeC1,sizeC2
      write(out_unit,*) 'It should never append! Check the source'
      STOP 'ERROR in AD_dnMat2_PLUS_dnMat1: The shapes of the matrices differ'
    END IF

    dnMat2_PLUS_dnMat1%nderiv = nderiv

    IF (nderiv == 0) THEN
       dnMat2_PLUS_dnMat1%d0 = dnMat1%d0 + dnMat2%d0
    ELSE IF (nderiv == 1) THEN
       dnMat2_PLUS_dnMat1%d0 = dnMat1%d0 + dnMat2%d0
       dnMat2_PLUS_dnMat1%d1 = dnMat1%d1 + dnMat2%d1
    ELSE IF (nderiv == 2) THEN
       dnMat2_PLUS_dnMat1%d0 = dnMat1%d0 + dnMat2%d0
       dnMat2_PLUS_dnMat1%d1 = dnMat1%d1 + dnMat2%d1
       dnMat2_PLUS_dnMat1%d2 = dnMat1%d2 + dnMat2%d2
    ELSE IF (nderiv == 3) THEN
       dnMat2_PLUS_dnMat1%d0 = dnMat1%d0 + dnMat2%d0
       dnMat2_PLUS_dnMat1%d1 = dnMat1%d1 + dnMat2%d1
       dnMat2_PLUS_dnMat1%d2 = dnMat1%d2 + dnMat2%d2
       dnMat2_PLUS_dnMat1%d3 = dnMat1%d3 + dnMat2%d3
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_dnMat2_PLUS_dnMat1
!> @brief Public function which calculate dnMat+R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param R                  real:                     some real number
!! @param sub_dnMat_EXP_R TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_sub_dnMat_PLUS_R(dnMat,R)  RESULT (sub_dnMat_PLUS_R)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                 :: sub_dnMat_PLUS_R
    TYPE (dnMat_t),    intent(in)  :: dnMat
    real (kind=Rkind), intent(in)  :: R

    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_sub_dnMat_PLUS_R'


    sub_dnMat_PLUS_R    = dnMat

    sub_dnMat_PLUS_R%d0 = sub_dnMat_PLUS_R%d0 + R

    ! the derivatives of R are zero => nothing to be add to %d1 and %d2

  END FUNCTION AD_sub_dnMat_PLUS_R
!> @brief Public function which calculate R+dnMat (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param R                  real:                     some real number
!! @param sub_R_PLUS_dnMat TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_sub_R_PLUS_dnMat(R,dnMat) RESULT (sub_R_PLUS_dnMat)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                 :: sub_R_PLUS_dnMat
    TYPE (dnMat_t),    intent(in)  :: dnMat
    real (kind=Rkind), intent(in)  :: R

    character (len=*), parameter :: name_sub='AD_sub_R_PLUS_dnMat'


    sub_R_PLUS_dnMat    = dnMat

    sub_R_PLUS_dnMat%d0 = sub_R_PLUS_dnMat%d0 + R

    ! the derivatives of R are zero

  END FUNCTION AD_sub_R_PLUS_dnMat
!> @brief Public function which calculate dnMat1-dnMat2 (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param dnMat1                    TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param dnMat2                    TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param dnMat2_MINUS_dnMat1 TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_dnMat2_MINUS_dnMat1(dnMat1,dnMat2) RESULT (dnMat2_MINUS_dnMat1)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                :: dnMat2_MINUS_dnMat1
    TYPE (dnMat_t), intent(in)    :: dnMat1,dnMat2

    integer :: nderiv
    integer :: nVar1,nVar2
    integer :: sizeL1,sizeL2
    integer :: sizeC1,sizeC2
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_dnMat2_MINUS_dnMat1'

    nderiv = min(AD_get_nderiv_FROM_dnMat(dnMat1),AD_get_nderiv_FROM_dnMat(dnMat2))

    nVar1   = AD_get_nVar_FROM_dnMat(dnMat1)
    nVar2   = AD_get_nVar_FROM_dnMat(dnMat2)

    sizeL1   = AD_get_sizeL_FROM_dnMat(dnMat1)
    sizeL2   = AD_get_sizeL_FROM_dnMat(dnMat2)
    sizeC1   = AD_get_sizeC_FROM_dnMat(dnMat1)
    sizeC2   = AD_get_sizeC_FROM_dnMat(dnMat2)

    IF (nVar1 /= nVar2) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nVar1 and nVar2 differ'
      write(out_unit,*) ' nVar1,nVar2',nVar1,nVar2
      STOP 'ERROR in AD_dnMat2_MINUS_dnMat1: nVar1 and nVar2 differ'
    END IF

    IF (nderiv < 0 .OR. (nderiv > 0  .AND. nVar1 < 1)) RETURN

    IF (sizeL1 /= sizeL2 .OR. sizeC1 /= sizeC2) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The shapes of the matrices differ.'
      write(out_unit,*) ' sizeL1,sizeL2',sizeL1,sizeL2
      write(out_unit,*) ' sizeC1,sizeC2',sizeC1,sizeC2
      write(out_unit,*) 'It should never append! Check the source'
      STOP 'ERROR in AD_dnMat2_MINUS_dnMat1: The shapes of the matrices differ'
    END IF

    CALL AD_dealloc_dnMat(dnMat2_MINUS_dnMat1)

    dnMat2_MINUS_dnMat1%nderiv = nderiv

    IF (nderiv == 0) THEN
       dnMat2_MINUS_dnMat1%d0 = dnMat1%d0 - dnMat2%d0
    ELSE IF (nderiv == 1) THEN
       dnMat2_MINUS_dnMat1%d0 = dnMat1%d0 - dnMat2%d0
       dnMat2_MINUS_dnMat1%d1 = dnMat1%d1 - dnMat2%d1
    ELSE IF (nderiv == 2) THEN
       dnMat2_MINUS_dnMat1%d0 = dnMat1%d0 - dnMat2%d0
       dnMat2_MINUS_dnMat1%d1 = dnMat1%d1 - dnMat2%d1
       dnMat2_MINUS_dnMat1%d2 = dnMat1%d2 - dnMat2%d2
    ELSE IF (nderiv == 3) THEN
       dnMat2_MINUS_dnMat1%d0 = dnMat1%d0 - dnMat2%d0
       dnMat2_MINUS_dnMat1%d1 = dnMat1%d1 - dnMat2%d1
       dnMat2_MINUS_dnMat1%d2 = dnMat1%d2 - dnMat2%d2
       dnMat2_MINUS_dnMat1%d3 = dnMat1%d3 - dnMat2%d3
    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_dnMat2_MINUS_dnMat1
!> @brief Public function which calculate dnMat-R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                  TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param R                    real:                     some real number
!! @param sub_dnMat_MINUS_R TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_sub_dnMat_MINUS_R(dnMat,R) RESULT (sub_dnMat_MINUS_R)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE


    TYPE (dnMat_t)                 :: sub_dnMat_MINUS_R
    TYPE (dnMat_t),    intent(in)  :: dnMat
    real (kind=Rkind), intent(in)  :: R

    character (len=*), parameter :: name_sub='AD_sub_dnMat_MINUS_R'


    sub_dnMat_MINUS_R = dnMat

    sub_dnMat_MINUS_R%d0 = dnMat%d0 - R

    ! the derivatives of R are zero

  END FUNCTION AD_sub_dnMat_MINUS_R
!> @brief Public function which calculate R-dnMat (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                  TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param R                    real:                     some real number
!! @param sub_R_MINUS_dnMat TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_sub_R_MINUS_dnMat(R,dnMat) RESULT (sub_R_MINUS_dnMat)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE


    TYPE (dnMat_t)                 :: sub_R_MINUS_dnMat
    TYPE (dnMat_t),    intent(in)  :: dnMat
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_sub_R_MINUS_dnMat'

    nderiv_loc = AD_get_nderiv_FROM_dnMat(dnMat)

    sub_R_MINUS_dnMat%nderiv = nderiv_loc

    !write(out_unit,*) 'nderiv',nderiv_loc
    IF (nderiv_loc == 0) THEN
       sub_R_MINUS_dnMat%d0 = R - dnMat%d0

    ELSE IF (nderiv_loc == 1) THEN
       sub_R_MINUS_dnMat%d0 = R - dnMat%d0
       sub_R_MINUS_dnMat%d1 =   - dnMat%d1


    ELSE IF (nderiv_loc == 2) THEN
       sub_R_MINUS_dnMat%d0 = R - dnMat%d0
       sub_R_MINUS_dnMat%d1 =   - dnMat%d1
       sub_R_MINUS_dnMat%d2 =   - dnMat%d2

    ELSE IF (nderiv_loc == 3) THEN
       sub_R_MINUS_dnMat%d0 = R - dnMat%d0
       sub_R_MINUS_dnMat%d1 =   - dnMat%d1
       sub_R_MINUS_dnMat%d2 =   - dnMat%d2
       sub_R_MINUS_dnMat%d3 =   - dnMat%d3

    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF

  END FUNCTION AD_sub_R_MINUS_dnMat
!> @brief Public function which calculate dnMat**R (and derivatives).
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                TYPE (dnMat_t):           derived type which deals with the derivatives of a matrix.
!! @param R                  real:                     exponent
!! @param sub_dnMat_EXP_R TYPE (dnMat_t) (result):  dnMat derived type
  FUNCTION AD_sub_dnMat_EXP_R(dnMat,R) RESULT (sub_dnMat_EXP_R)
    USE QDUtil_m, ONLY : Rkind, ONE, TWO, THREE, out_unit
    IMPLICIT NONE


    TYPE (dnMat_t)                 :: sub_dnMat_EXP_R
    TYPE (dnMat_t),    intent(in)  :: dnMat
    real (kind=Rkind), intent(in)  :: R

    integer :: nderiv_loc,nVar_loc
    integer :: id,jd,kd
    character (len=*), parameter :: name_sub='AD_sub_dnMat_EXP_R'

    nderiv_loc = AD_get_nderiv_FROM_dnMat(dnMat)
    nVar_loc = AD_get_nVar_FROM_dnMat(dnMat)

    !write(out_unit,*) 'nderiv',nderiv_loc

    sub_dnMat_EXP_R = dnMat ! for the allocation

    IF (nderiv_loc == 0) THEN
       sub_dnMat_EXP_R%d0 = dnMat%d0 ** R

    ELSE IF (nderiv_loc == 1) THEN
       sub_dnMat_EXP_R%d0 = dnMat%d0 ** R

       DO id=1,nVar_loc
         sub_dnMat_EXP_R%d1(:,:,id) = R * dnMat%d0 ** (R-ONE) * dnMat%d1(:,:,id)
       END DO

    ELSE IF (nderiv_loc == 2) THEN
       sub_dnMat_EXP_R%d0 = dnMat%d0 ** R

       DO id=1,nVar_loc
         sub_dnMat_EXP_R%d1(:,:,id) = R * dnMat%d0 ** (R-ONE) * dnMat%d1(:,:,id)
       END DO

       DO id=1,nVar_loc
       DO jd=1,nVar_loc
         sub_dnMat_EXP_R%d2(:,:,jd,id) = R*(R-ONE) * dnMat%d0 ** (R-TWO) * dnMat%d1(:,:,id) * dnMat%d1(:,:,jd) + &
                                            R * dnMat%d0 ** (R-ONE) * dnMat%d2(:,:,jd,id)
       END DO
       END DO

    ELSE IF (nderiv_loc == 3) THEN
       sub_dnMat_EXP_R%d0 = dnMat%d0 ** R

       DO id=1,nVar_loc
         sub_dnMat_EXP_R%d1(:,:,id) = R * dnMat%d0 ** (R-ONE) * dnMat%d1(:,:,id)
       END DO

       DO id=1,nVar_loc
       DO jd=1,nVar_loc
         sub_dnMat_EXP_R%d2(:,:,jd,id) = R*(R-ONE) * dnMat%d0 ** (R-TWO) * dnMat%d1(:,:,id) * dnMat%d1(:,:,jd) + &
                                            R * dnMat%d0 ** (R-ONE) * dnMat%d2(:,:,jd,id)
       END DO
       END DO

       DO id=1,nVar_loc
       DO jd=1,nVar_loc
       DO kd=1,nVar_loc
         sub_dnMat_EXP_R%d3(:,:,kd,jd,id) =                             &
                            R*(R-ONE)*(R-TWO) * dnMat%d0**(R-THREE) *   &
                   dnMat%d1(:,:,id)*dnMat%d1(:,:,jd)*dnMat%d1(:,:,kd) + &
                            R*(R-ONE) * dnMat%d0**(R-TWO) * (           &
                               dnMat%d2(:,:,jd,id)*dnMat%d1(:,:,kd) +   &
                               dnMat%d2(:,:,kd,id)*dnMat%d1(:,:,jd) +   &
                               dnMat%d2(:,:,kd,jd)*dnMat%d1(:,:,id) ) + &
                             R * dnMat%d0**(R-ONE) * dnMat%d3(:,:,kd,jd,id)

       END DO
       END DO
       END DO

    ELSE
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nderiv > 3 is NOT possible',nderiv_loc
      write(out_unit,*) 'It should never append! Check the source'
      STOP
    END IF
  END FUNCTION AD_sub_dnMat_EXP_R
  FUNCTION AD_TRANSPOSE_dnMat(dnMat)  RESULT(TransdnMat) ! check with t(t(dnmat))-dnMat
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                :: TransdnMat
    TYPE (dnMat_t), intent(in)    :: dnMat

    integer :: nderiv,sizeL,sizeC,nVar,id,jd,kd
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_TRANSPOSE_dnMat'

    nderiv = AD_get_nderiv_FROM_dnMat(dnMat)
    sizeL  = AD_get_sizeL_FROM_dnMat(dnMat)
    sizeC  = AD_get_sizeC_FROM_dnMat(dnMat)
    nVar   = AD_get_nVar_FROM_dnMat(dnMat)
    !write(out_unit,*) 'in ',name_sub,' sizeL,sizeC,nVar,nderiv',sizeL,sizeC,nVar,nderiv


    CALL AD_alloc_dnMat(TransdnMat,sizeL=sizeC,sizeC=sizeL,nVar=nVar,nderiv=nderiv)

    nderiv = get_nderiv(TransdnMat)

    IF (nderiv < 0) RETURN


    IF (nderiv >= 0) THEN
      TransdnMat%d0(:,:) = transpose(dnMat%d0)
    END IF

    IF (nderiv >= 1) THEN
      DO id=1,nVar
        TransdnMat%d1(:,:,id) = transpose(dnMat%d1(:,:,id))
      END DO
    END IF

    IF (nderiv >= 2) THEN
      DO id=1,nVar
      DO jd=1,nVar
        TransdnMat%d2(:,:,jd,id) = transpose(dnMat%d2(:,:,jd,id))
      END DO
      END DO
    END IF

    IF (nderiv >= 3) THEN
      DO id=1,nVar
      DO jd=1,nVar
      DO kd=1,nVar
        TransdnMat%d3(:,:,kd,jd,id) = transpose(dnMat%d3(:,:,kd,jd,id))
      END DO
      END DO
      END DO
    END IF

    nderiv = get_nderiv(TransdnMat)

  END FUNCTION AD_TRANSPOSE_dnMat

  FUNCTION AD_SYM_dnMat(dnMat)  RESULT(SymdnMat) ! check with t(t(dnmat))-dnMat
    USE QDUtil_m, ONLY : Rkind, HALF, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                :: SymdnMat
    TYPE (dnMat_t), intent(in)    :: dnMat

    integer :: nderiv,nsurf,nVar,id,jd,kd
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_SYM_dnMat'

    nderiv = AD_get_nderiv_FROM_dnMat(dnMat)
    nsurf  = AD_get_nsurf_FROM_dnMat(dnMat)
    nVar   = AD_get_nVar_FROM_dnMat(dnMat)

    !write(out_unit,*) 'in ',name_sub,' nsurf,nVar,nderiv',nsurf,nVar,nderiv

    CALL AD_dealloc_dnMat(SymdnMat)

    IF (nderiv < 0 .OR. nsurf < 1 .OR. (nderiv > 0  .AND. nVar < 1)) RETURN

    CALL AD_alloc_dnMat(SymdnMat,nsurf=nsurf,nVar=nVar,nderiv=nderiv,      &
                        name_var='SymdnMat',name_sub=name_sub)

    IF (nderiv >= 0) THEN
      SymdnMat%d0(:,:) = HALF*(transpose(dnMat%d0) + dnMat%d0)
    END IF

    IF (nderiv >= 1) THEN
      DO id=1,nVar
        SymdnMat%d1(:,:,id) = HALF*(transpose(dnMat%d1(:,:,id)) + dnMat%d1(:,:,id))
      END DO
    END IF

    IF (nderiv >= 2) THEN
      DO id=1,nVar
      DO jd=1,nVar
        SymdnMat%d2(:,:,jd,id) = HALF*(transpose(dnMat%d2(:,:,jd,id)) + dnMat%d2(:,:,jd,id))
      END DO
      END DO
    END IF

    IF (nderiv >= 3) THEN
      DO id=1,nVar
      DO jd=1,nVar
      DO kd=1,nVar
        SymdnMat%d3(:,:,kd,jd,id) = HALF*(transpose(dnMat%d3(:,:,kd,jd,id)) + dnMat%d3(:,:,kd,jd,id))
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_SYM_dnMat

  FUNCTION AD_MATMUL_dnMat1_dnMat2(dnMat1,dnMat2)  RESULT(MatmuldnMat)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    TYPE (dnMat_t)                :: MatmuldnMat
    TYPE (dnMat_t), intent(in)    :: dnMat1,dnMat2

    integer :: nderiv
    integer :: nVar1,nVar2
    integer :: sizeL1,sizeC1,sizeL2,sizeC2
    integer :: id,jd,kd
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_MATMUL_dnMat1_dnMat2'


    nderiv = min(AD_get_nderiv_FROM_dnMat(dnMat1),AD_get_nderiv_FROM_dnMat(dnMat2))

    nVar1   = AD_get_nVar_FROM_dnMat(dnMat1)
    nVar2   = AD_get_nVar_FROM_dnMat(dnMat2)
    IF (nVar1 /= nVar2) STOP ' ERROR in AD_MATMUL_dnMat1_dnMat2: nVar1 and nVar2 differ'

    sizeL1  = AD_get_sizeL_FROM_dnMat(dnMat1)
    sizeC1  = AD_get_sizeC_FROM_dnMat(dnMat1)
    sizeL2  = AD_get_sizeL_FROM_dnMat(dnMat2)
    sizeC2  = AD_get_sizeC_FROM_dnMat(dnMat2)
    IF (sizeC1 /= sizeL2) STOP ' ERROR in AD_MATMUL_dnMat1_dnMat2: sizeC1 and sizeL2 differ'

    IF (nderiv < 0 .OR. sizeL1 < 1 .OR. sizeC2 < 1 .OR. (nderiv > 0  .AND. nVar1 < 1)) RETURN

    CALL AD_alloc_dnMat(MatmuldnMat,sizeL=sizeL1,sizeC=sizeC2,nVar=nVar1,nderiv=nderiv,                  &
                        name_var='MatmuldnMat',name_sub=name_sub)

    IF (nderiv >= 0) THEN
      MatmuldnMat%d0(:,:) = matmul(dnMat1%d0,dnMat2%d0)
    END IF

    IF (nderiv >= 1) THEN
      DO id=1,nVar1
        MatmuldnMat%d1(:,:,id) = matmul(dnMat1%d0,dnMat2%d1(:,:,id)) +  &
                                 matmul(dnMat1%d1(:,:,id),dnMat2%d0)
      END DO
    END IF

    IF (nderiv >= 2) THEN
      DO id=1,nVar1
      DO jd=1,nVar1
        MatmuldnMat%d2(:,:,jd,id) =                                     &
                    matmul(dnMat1%d0,           dnMat2%d2(:,:,jd,id)) + &
                    matmul(dnMat1%d1(:,:,jd),   dnMat2%d1(:,:,id))    + &
                    matmul(dnMat1%d1(:,:,id),   dnMat2%d1(:,:,jd))    + &
                    matmul(dnMat1%d2(:,:,jd,id),dnMat2%d0)

      END DO
      END DO
    END IF

    IF (nderiv >= 3) THEN
      DO id=1,nVar1
      DO jd=1,nVar1
      DO kd=1,nVar1
        MatmuldnMat%d3(:,:,kd,jd,id) =                                  &
             matmul(dnMat1%d0,              dnMat2%d3(:,:,kd,jd,id))  + &
             matmul(dnMat1%d1(:,:,kd),      dnMat2%d2(:,:,jd,id))     + &
             matmul(dnMat1%d1(:,:,id),      dnMat2%d2(:,:,kd,jd))     + &
             matmul(dnMat1%d1(:,:,jd),      dnMat2%d2(:,:,id,kd))     + &
             matmul(dnMat1%d2(:,:,jd,id),   dnMat2%d1(:,:,kd))        + &
             matmul(dnMat1%d2(:,:,kd,jd),   dnMat2%d1(:,:,id))        + &
             matmul(dnMat1%d2(:,:,id,kd),   dnMat2%d1(:,:,jd))        + &
             matmul(dnMat1%d3(:,:,kd,jd,id),dnMat2%d0)

      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_MATMUL_dnMat1_dnMat2

  FUNCTION AD_MATMUL_dnMat1_Mat2(dnMat1,Mat2)  RESULT(MatmuldnMat)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE


    TYPE (dnMat_t)                  :: MatmuldnMat
    TYPE (dnMat_t),   intent(in)    :: dnMat1
    real(kind=Rkind), intent(in)    :: Mat2(:,:)

    integer :: nderiv,nVar,id,jd,kd
    integer :: sizeL1,sizeC1,sizeL2,sizeC2
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_MATMUL_dnMat1_Mat2'


    nderiv = AD_get_nderiv_FROM_dnMat(dnMat1)
    nVar   = AD_get_nVar_FROM_dnMat(dnMat1)

    sizeL1  = AD_get_sizeL_FROM_dnMat(dnMat1)
    sizeC1  = AD_get_sizeC_FROM_dnMat(dnMat1)
    sizeL2  = size(Mat2,dim=1)
    sizeC2  = size(Mat2,dim=2)
    IF (sizeC1 /= sizeL2) STOP ' ERROR in AD_MATMUL_dnMat1_Mat2: sizeC1 and sizeL2 differ'

    !write(out_unit,*) 'in ',name_sub,' nsurf,nVar,nderiv',nsurf,nVar,nderiv

    IF (nderiv < 0 .OR. sizeL1 < 1 .OR. sizeC2 < 1 .OR. (nderiv > 0  .AND. nVar < 1)) RETURN

    CALL AD_alloc_dnMat(MatmuldnMat,sizeL=sizeL1,sizeC=sizeC2,nVar=nVar,nderiv=nderiv,                  &
                        name_var='MatmuldnMat',name_sub=name_sub)

    IF (nderiv >= 0) THEN
      MatmuldnMat%d0(:,:) = matmul(dnMat1%d0,Mat2)
    END IF

    IF (nderiv >= 1) THEN
      DO id=1,nVar
        MatmuldnMat%d1(:,:,id) = matmul(dnMat1%d1(:,:,id),Mat2)
      END DO
    END IF

    IF (nderiv >= 2) THEN
      DO id=1,nVar
      DO jd=1,nVar
        MatmuldnMat%d2(:,:,jd,id) = matmul(dnMat1%d2(:,:,jd,id),Mat2)
      END DO
      END DO
    END IF

    IF (nderiv >= 3) THEN
      DO id=1,nVar
      DO jd=1,nVar
      DO kd=1,nVar
        MatmuldnMat%d3(:,:,kd,jd,id) = matmul(dnMat1%d3(:,:,kd,jd,id),Mat2)
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_MATMUL_dnMat1_Mat2
  FUNCTION AD_MATMUL_Mat1_dnMat2(Mat1,dnMat2)  RESULT(MatmuldnMat)
    USE QDUtil_m, ONLY : Rkind, out_unit
    IMPLICIT NONE


    TYPE (dnMat_t)                  :: MatmuldnMat
    real(kind=Rkind), intent(in)    :: Mat1(:,:)
    TYPE (dnMat_t),   intent(in)    :: dnMat2

    integer :: nderiv,nVar,id,jd,kd
    integer :: sizeL1,sizeC1,sizeL2,sizeC2
    integer :: err_dnMat_loc
    character (len=*), parameter :: name_sub='AD_MATMUL_Mat1_dnMat2'


    nderiv = AD_get_nderiv_FROM_dnMat(dnMat2)
    nVar   = AD_get_nVar_FROM_dnMat(dnMat2)


    sizeL2  = AD_get_sizeL_FROM_dnMat(dnMat2)
    sizeC2  = AD_get_sizeC_FROM_dnMat(dnMat2)
    sizeL1  = size(Mat1,dim=1)
    sizeC1  = size(Mat1,dim=2)
    IF (sizeC1 /= sizeL2) STOP ' ERROR in AD_MATMUL_Mat1_dnMat2: sizeC1 and sizeL2 differ'

    !write(out_unit,*) 'in ',name_sub,' nsurf,nVar,nderiv',nsurf,nVar,nderiv

 
    IF (nderiv < 0 .OR. sizeL1 < 1 .OR. sizeC2 < 1 .OR. (nderiv > 0  .AND. nVar < 1)) RETURN

    CALL AD_alloc_dnMat(MatmuldnMat,sizeL=sizeL1,sizeC=sizeC2,nVar=nVar,nderiv=nderiv,                  &
                        name_var='MatmuldnMat',name_sub=name_sub)

    IF (nderiv >= 0) THEN
      MatmuldnMat%d0(:,:) = matmul(Mat1,dnMat2%d0)
    END IF

    IF (nderiv >= 1) THEN
      DO id=1,nVar
        MatmuldnMat%d1(:,:,id) = matmul(Mat1,dnMat2%d1(:,:,id))
      END DO
    END IF

    IF (nderiv >= 2) THEN
      DO id=1,nVar
      DO jd=1,nVar
        MatmuldnMat%d2(:,:,jd,id) = matmul(Mat1,dnMat2%d2(:,:,jd,id))
      END DO
      END DO
    END IF

    IF (nderiv >= 3) THEN
      DO id=1,nVar
      DO jd=1,nVar
      DO kd=1,nVar
        MatmuldnMat%d3(:,:,kd,jd,id) = matmul(Mat1,dnMat2%d3(:,:,kd,jd,id))
      END DO
      END DO
      END DO
    END IF

  END FUNCTION AD_MATMUL_Mat1_dnMat2
!> @brief Public subroutine which prints a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
!! @param Mat                TYPE (dnMat_t):      derived type which deals with the derivatives of a matrix.
!! @param nio                integer (optional):  when present unit to print S, otherwise it is the default unit:out_unit
  SUBROUTINE AD_Write_dnMat(Mat,nio,info)
    USE QDUtil_m, ONLY : Rkind, Write_Vec, Write_Mat, &
                         out_unit, RMatIO_format
    IMPLICIT NONE

    TYPE (dnMat_t),   intent(in)           :: Mat
    integer,          intent(in), optional :: nio
    character(len=*), intent(in), optional :: info

    integer :: i,j,k,nio_loc,sizeL,sizeC,nVar_loc

    IF (present(nio)) THEN
      nio_loc = nio
    ELSE
      nio_loc = out_unit
    END IF

    sizeL  = AD_get_sizeL_FROM_dnMat(Mat)
    sizeC  = AD_get_sizeC_FROM_dnMat(Mat)

    nVar_loc   = AD_get_nVar_FROM_dnMat(Mat)

    IF (sizeL == 1 .AND. sizeC == 1 .AND. nVar_loc > 1) THEN
      IF (allocated(Mat%d0)) THEN
        write(nio_loc,'(a,' // RMatIO_format // ')') ' no derivative',Mat%d0
      END IF

      IF (allocated(Mat%d1)) THEN
        write(nio_loc,*) ' 1st derivative'
        CALL Write_Vec(Mat%d1(1,1,:),nio_loc,5)
      END IF

      IF (allocated(Mat%d2)) THEN
        write(nio_loc,*) ' 2d derivative'
        CALL Write_Mat(Mat%d2(1,1,:,:),nio_loc,5)
      END IF
      IF (allocated(Mat%d3)) THEN
        write(nio_loc,*) ' 3d derivative'
        DO i=1,ubound(Mat%d3,dim=5)
        DO j=1,ubound(Mat%d3,dim=4)
        DO k=1,ubound(Mat%d3,dim=3)
          write(nio_loc,'(3(1x,i0)," : ",' // RMatIO_format // ')') k,j,i,Mat%d3(1,1,k,j,i)
        END DO
        END DO
        END DO
      END IF
    ELSE
      IF (allocated(Mat%d0)) THEN
         IF (present(info)) THEN
           write(nio_loc,*) ' no derivative of ',info
         ELSE
           write(nio_loc,*) ' no derivative'
         END IF
        CALL Write_Mat(Mat%d0,nio_loc,5)
      END IF

      IF (allocated(Mat%d1)) THEN
        DO i=1,ubound(Mat%d1,dim=3)
          IF (present(info)) THEN
            write(nio_loc,*) ' 1st derivative of ',info,i
          ELSE
            write(nio_loc,*) ' 1st derivative',i
          END IF
          CALL Write_Mat(Mat%d1(:,:,i),nio_loc,5)
        END DO
      END IF

      IF (allocated(Mat%d2)) THEN
        DO i=1,ubound(Mat%d2,dim=4)
        DO j=1,ubound(Mat%d2,dim=3)
          IF (present(info)) THEN
            write(nio_loc,*) ' 2d derivative of ',info,j,i
          ELSE
            write(nio_loc,*) ' 2d derivative',j,i
          END IF
          CALL Write_Mat(Mat%d2(:,:,j,i),nio_loc,5)
        END DO
        END DO
      END IF

      IF (allocated(Mat%d3)) THEN
        DO i=1,ubound(Mat%d3,dim=5)
        DO j=1,ubound(Mat%d3,dim=4)
        DO k=1,ubound(Mat%d3,dim=3)
          IF (present(info)) THEN
            write(nio_loc,*) ' 3d derivative of ',info,k,j,i
          ELSE
            write(nio_loc,*) ' 3d derivative',k,j,i
          END IF
          CALL Write_Mat(Mat%d3(:,:,k,j,i),nio_loc,5)
        END DO
        END DO
        END DO
      END IF
    END IF
    flush(nio_loc)
  END SUBROUTINE AD_Write_dnMat
!> @brief Public function to get nderiv from a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param Mat                         TYPE (dnMat_t):     derived type which deals with the derivatives of a matrix.
!! @param get_nderiv_FROM_dnMat    integer  (result):  nderiv value, check against Mat%nederiv
  FUNCTION AD_get_nderiv_FROM_dnMat(Mat) RESULT(nderiv)
    USE QDUtil_m, ONLY : out_unit
    IMPLICIT NONE

    integer                       :: nderiv
    TYPE (dnMat_t), intent(in)    :: Mat

    nderiv = Mat%nderiv

    IF (.NOT. allocated(Mat%d0)) THEN
      nderiv = -1
    ELSE IF (.NOT. allocated(Mat%d1)) THEN
      nderiv = 0
    ELSE IF (.NOT. allocated(Mat%d2)) THEN
      nderiv = 1
    ELSE IF (.NOT. allocated(Mat%d3)) THEN
      nderiv = 2
    ELSE
      nderiv = 3
    END IF

    IF (Mat%nderiv /= nderiv) THEN
      write(out_unit,*) ' ERROR in AD_get_nderiv_FROM_dnMat'
      write(out_unit,*) '  Problem with nderiv in Mat'
      write(out_unit,*) '  nderiv,Mat%nderiv',nderiv,Mat%nderiv
      CALL AD_Write_dnMat(Mat)
      STOP 'ERROR in AD_get_nderiv_FROM_dnMat'
    END IF

    END FUNCTION AD_get_nderiv_FROM_dnMat

!> @brief Public function to get nsurf (the number of electronic surfaces, dimension of the matrix) from a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param Mat                         TYPE (dnMat_t):     derived type which deals with the derivatives of a matrix.
!! @param get_nderiv_FROM_dnMat    integer  (result):  nderiv value
  FUNCTION AD_get_nsurf_FROM_dnMat(Mat) RESULT(nsurf)
    USE QDUtil_m, ONLY : out_unit

    integer                       :: nsurf
    TYPE (dnMat_t), intent(in)    :: Mat

    integer :: sizeL_loc,sizeC_loc

    IF (.NOT. allocated(Mat%d0)) THEN
      sizeL_loc = 0
      sizeC_loc = 0
    ELSE
      sizeL_loc = size(Mat%d0,dim=1)
      sizeC_loc = size(Mat%d0,dim=2)
    END IF
    IF (sizeL_loc /= sizeC_loc) THEN
      write(out_unit,*) ' ERROR in AD_get_nsurf_FROM_dnMat'
      write(out_unit,*) '  Mat is not a square matrix!'
      write(out_unit,*) '  shape(mat)',shape(Mat)
      STOP 'ERROR in AD_get_nsurf_FROM_dnMat: Mat is not a square matrix'
    ELSE
      nsurf = sizeL_loc
    END IF

    END FUNCTION AD_get_nsurf_FROM_dnMat
    FUNCTION AD_get_sizeC_FROM_dnMat(Mat) RESULT(sizeC)

      integer                       :: sizeC
      TYPE (dnMat_t), intent(in)    :: Mat
  
  
      IF (.NOT. allocated(Mat%d0)) THEN
        sizeC = 0
      ELSE
        sizeC = size(Mat%d0,dim=2)
      END IF

    END FUNCTION AD_get_sizeC_FROM_dnMat
    FUNCTION AD_get_sizeL_FROM_dnMat(Mat) RESULT(sizeL)

      integer                       :: sizeL
      TYPE (dnMat_t), intent(in)    :: Mat
  
  
      IF (.NOT. allocated(Mat%d0)) THEN
        sizeL = 0
      ELSE
        sizeL = size(Mat%d0,dim=1)
      END IF

    END FUNCTION AD_get_sizeL_FROM_dnMat
!> @brief Public function to get nVar (number of coordinates) from a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param Mat                       TYPE (dnMat_t):      derived type which deals with the derivatives of a scalar functions.
!! @param get_nVar_FROM_dnMat    integer  (result):   nVar value from the size of Mat%d1.
  FUNCTION AD_get_nVar_FROM_dnMat(Mat) RESULT(nVar)

    integer                       :: nVar
    TYPE (dnMat_t), intent(in)    :: Mat

    IF (.NOT. allocated(Mat%d1)) THEN
      nVar = 0
    ELSE
      nVar = size(Mat%d1,dim=3)
    END IF

    END FUNCTION AD_get_nVar_FROM_dnMat
!> @brief Public function to get d0(:,:) from a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 07/11/2022
!!
!! @param Mat                       TYPE (dnMat_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d0_FROM_dnMat      real  (result):      Mat%d0(:,:)
    FUNCTION AD_get_d0_FROM_dnMat(Mat) RESULT(d0)
      USE QDUtil_m, ONLY : Rkind
      IMPLICIT NONE

      TYPE (dnMat_t), intent(in)         :: Mat
      real (kind=Rkind),   allocatable   :: d0(:,:)

      IF (allocated(Mat%d0)) d0 = Mat%d0
  
    END FUNCTION AD_get_d0_FROM_dnMat
!> @brief Public function to get d1(:,:,:) from a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 07/11/2022
!!
!! @param Mat                       TYPE (dnMat_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d1_FROM_dnMat      real  (result):      Mat%d1(:,:,:)
    FUNCTION AD_get_d1_FROM_dnMat(Mat) RESULT(d1)
      USE QDUtil_m, ONLY : Rkind
      IMPLICIT NONE

      TYPE (dnMat_t), intent(in)         :: Mat
      real (kind=Rkind),   allocatable   :: d1(:,:,:)

      IF (allocated(Mat%d1)) d1 = Mat%d1
  
    END FUNCTION AD_get_d1_FROM_dnMat
!> @brief Public function to get d2(:,:,:,:) from a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 07/11/2022
!!
!! @param Mat                       TYPE (dnMat_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d2_FROM_dnMat      real  (result):      Mat%d2(:,:,:,:)
    FUNCTION AD_get_d2_FROM_dnMat(Mat) RESULT(d2)
      USE QDUtil_m, ONLY : Rkind
      IMPLICIT NONE

      TYPE (dnMat_t),       intent(in)    :: Mat
      real (kind=Rkind),    allocatable   :: d2(:,:,:,:)

      IF (allocated(Mat%d2)) d2 = Mat%d2
  
    END FUNCTION AD_get_d2_FROM_dnMat
!> @brief Public function to get d0(:,:) from a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 07/11/2022
!!
!! @param Mat                       TYPE (dnMat_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d3_FROM_dnMat      real  (result):      Mat%d3(:,:,:,:,:)
    FUNCTION AD_get_d3_FROM_dnMat(Mat) RESULT(d3)
      USE QDUtil_m, ONLY : Rkind
      IMPLICIT NONE

      TYPE (dnMat_t),       intent(in)    :: Mat
      real (kind=Rkind),    allocatable   :: d3(:,:,:,:,:)

      IF (allocated(Mat%d3)) d3 = Mat%d3
  
    END FUNCTION AD_get_d3_FROM_dnMat
!> @brief Public function to get FlatdnMat(:) from a derived type dnMat.
!!
!> @author David Lauvergnat
!! @date 09/01/2023
!!
!! @param Mat                       TYPE (dnMat_t):      derived type which deals with the derivatives of a scalar functions.
!! @param AD_get_d3_FROM_dnMat      real  (result):      FlatdnMat
    FUNCTION AD_get_Flatten_dnMat(Mat,all_der,i_der) RESULT(FlatdnMat)
      USE QDUtil_m, ONLY : Rkind, out_unit
      IMPLICIT NONE

      real (kind=Rkind),    allocatable   :: FlatdnMat(:)

      TYPE (dnMat_t),       intent(in)    :: Mat
      logical,              intent(in), optional :: all_der
      integer,              intent(in), optional :: i_der

      logical :: tab_der(0:3)
      integer :: nderiv,FlatdnMat_size,i,f

      nderiv = get_nderiv(Mat)
      IF (nderiv < 0) THEN
        allocate(FlatdnMat(0))
        RETURN ! Mat%d... are not allocated
      END IF

      tab_der(:) = .FALSE.

      IF (present(all_der)) THEN
        IF (all_der) THEN
          tab_der(0:nderiv) = .TRUE.
          IF (present(i_der)) THEN
             write(out_unit,*) ' ERROR in AD_get_Flatten_dnMat'
             write(out_unit,*) ' all_der and i_der are present and incompatible'
             write(out_unit,*) '   all_der,i_der',all_der,i_der
             STOP 'ERROR in AD_get_Flatten_dnMat: all_der and i_der are present and incompatible'
          END IF
        END IF
      END IF

      IF (present(i_der)) THEN
        IF (i_der < 0 .OR. i_der > nderiv) THEN
          write(out_unit,*) ' ERROR in AD_get_Flatten_dnMat'
          write(out_unit,*) ' i_der MUST be >= 0 and <= nderiv'
          write(out_unit,*) '   i_der,nderiv',i_der,nderiv
          STOP 'ERROR in AD_get_Flatten_dnMat: i_der MUST be >= 0 and <= nderiv'
        END IF
        tab_der(0:nderiv) = .FALSE.
        tab_der(i_der)    = .TRUE.
      ELSE
        tab_der(0:nderiv) = .TRUE.
      END IF
      ! size of FlatdnMat
      FlatdnMat_size = 0
      IF (tab_der(0)) FlatdnMat_size = FlatdnMat_size + size(Mat%d0)
      IF (tab_der(1)) FlatdnMat_size = FlatdnMat_size + size(Mat%d1)
      IF (tab_der(2)) FlatdnMat_size = FlatdnMat_size + size(Mat%d2)
      IF (tab_der(3)) FlatdnMat_size = FlatdnMat_size + size(Mat%d3)

      !write(out_unit,*) 'tab_der,FlatdnMat_size',tab_der,FlatdnMat_size

      allocate(FlatdnMat(FlatdnMat_size))

      i = 0
      IF (tab_der(0)) THEN
        f = i + size(Mat%d0)
        FlatdnMat(i+1:f) = reshape(Mat%d0,shape=[size(Mat%d0)])
        i = f
      END IF
      IF (tab_der(1)) THEN
        f = i + size(Mat%d1)
        FlatdnMat(i+1:f) = reshape(Mat%d1,shape=[size(Mat%d1)])
        i = f
      END IF
      IF (tab_der(2)) THEN
        f = i + size(Mat%d2)
        FlatdnMat(i+1:f) = reshape(Mat%d2,shape=[size(Mat%d2)])
        i = f
      END IF
      IF (tab_der(3)) THEN
        f = i + size(Mat%d3)
        FlatdnMat(i+1:f) = reshape(Mat%d3,shape=[size(Mat%d3)])
        i = f
      END IF

    END FUNCTION AD_get_Flatten_dnMat
!> @brief Public function which ckecks a derived type dnMat is zero (all components).
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param Check_dnMat_IS_ZERO   logical  (result):   result of the comparison
!! @param Mat                      TYPE (dnMat_t):      derived type which deals with the derivatives of a matrix.
!! @param epsi                     real (optional):     when present zero limit, otherwise 10^-10
  FUNCTION AD_Check_dnMat_IS_ZERO(Mat,epsi) RESULT(IS_ZERO)
    USE QDUtil_m, ONLY : Rkind, ONETENTH
    IMPLICIT NONE

    logical                                  :: IS_ZERO
    TYPE (dnMat_t),     intent(in)           :: Mat
    real(kind=Rkind),   intent(in), optional :: epsi


    IF (present(epsi)) THEN
      IS_ZERO = AD_get_maxval_OF_dnMat(Mat) <= epsi
    ELSE
      IS_ZERO = AD_get_maxval_OF_dnMat(Mat) <= ONETENTH**10
    END IF

    END FUNCTION AD_Check_dnMat_IS_ZERO
!> @brief Public function which gets the largest value of a derived type get_maxval_OF_dnMat (all components).
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param get_maxval_OF_dnMat   real  (result):      largest value (all components)
!! @param Mat                      TYPE (dnMat_t):      derived type which deals with the derivatives of a matrix.
  FUNCTION AD_get_maxval_OF_dnMat(Mat,nderiv) RESULT(mval)
    USE QDUtil_m, ONLY : Rkind, ZERO
    IMPLICIT NONE

    real(kind=Rkind)                     :: mval
    TYPE (dnMat_t), intent(in)           :: Mat
    integer,        intent(in), optional :: nderiv

    real(kind=Rkind) :: e0,e1,e2,e3
    integer          :: nderiv_loc

    nderiv_loc = AD_get_nderiv_FROM_dnMat(Mat)
    IF (present(nderiv)) nderiv_loc = min(nderiv_loc,nderiv)

    IF (allocated(Mat%d0) .AND. nderiv_loc >= 0) THEN
      e0 = maxval(abs(Mat%d0))
    ELSE
      e0 = ZERO
    END IF

    IF (allocated(Mat%d1) .AND. nderiv_loc >= 1) THEN
      e1 = maxval(abs(Mat%d1))
    ELSE
      e1 = ZERO
    END IF

    IF (allocated(Mat%d2) .AND. nderiv_loc >= 2) THEN
      e2 = maxval(abs(Mat%d2))
    ELSE
      e2 = ZERO
    END IF

    IF (allocated(Mat%d3) .AND. nderiv_loc >= 3) THEN
      e3 = maxval(abs(Mat%d3))
    ELSE
      e3 = ZERO
    END IF

    mval = max(e0,e1,e2,e3)

    END FUNCTION AD_get_maxval_OF_dnMat
!! @brief Public subroutine which checks if the derived type dnMat is (correctly) allocated.
!!
!> @author David Lauvergnat
!! @date 25/06/2018
!!
!! @param Mat                TYPE (dnMat_t):  derived type which deals with the derivatives of a matrix.
!! @param nderiv             integer:         the derivative order.
  FUNCTION AD_Check_NotAlloc_dnMat(Mat,nderiv) RESULT(NotAlloc)

    logical                       :: NotAlloc
    TYPE (dnMat_t), intent(in)    :: Mat
    integer,        intent(in)    :: nderiv

    NotAlloc =               (nderiv >= 0 .AND. .NOT. allocated(Mat%d0))
    NotAlloc = NotAlloc .OR. (nderiv >= 1 .AND. .NOT. allocated(Mat%d1))
    NotAlloc = NotAlloc .OR. (nderiv >= 2 .AND. .NOT. allocated(Mat%d2))
    NotAlloc = NotAlloc .OR. (nderiv >= 3 .AND. .NOT. allocated(Mat%d3))

  END FUNCTION AD_Check_NotAlloc_dnMat

  SUBROUTINE AD_DIAG_dnMat(dnMat,dnMatDiag,dnVec,dnVecProj,dnVec0,type_diag)
    USE QDUtil_m, ONLY : Rkind, ONETENTH, ZERO, ONE, TEN, PI, &
                         diagonalization, out_unit, &
                         Write_Vec, Write_Mat
    IMPLICIT NONE


    TYPE (dnMat_t),     intent(in)              :: dnMat
    TYPE (dnMat_t),     intent(inout)           :: dnMatDiag ! we keep it as a matrix
    TYPE (dnMat_t),     intent(inout)           :: dnVec
    TYPE (dnMat_t),     intent(inout), optional :: dnVecProj
    TYPE (dnMat_t),     intent(inout), optional :: dnVec0
    integer,            intent(in),    optional :: type_diag

    integer                       :: nVar,nderiv,nsurf
    real(kind=Rkind), allocatable :: Vec(:,:),tVec(:,:),Eig(:),Mtemp(:,:),Vi(:),Vj(:)
    TYPE (dnMat_t)                :: dnMat_OnVec
    integer                       :: i,j,k,id,jd,kd,i_max
    real (kind=Rkind)             :: ai,aj,aii,aij,aji,ajj,th,cc,ss,aii_max,max_diff
    real (kind=Rkind)             :: err1,err2,Rot(2,2),Mij(2,2),RMij(2,2)

    real (kind=Rkind)             :: epsi = ONETENTH**10
    integer                       :: type_diag_loc


!----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='AD_DIAG_dnMat'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
!-----------------------------------------------------------

    IF (debug) THEN
      write(out_unit,*) ' BEGINNING ',name_sub
      flush(out_unit)
    END IF

    type_diag_loc = 2
    IF (present(type_diag)) type_diag_loc = type_diag
    IF (debug) write(out_unit,*) 'type_diag',type_diag_loc

    nderiv = AD_get_nderiv_FROM_dnMat(dnMat)
    nVar   = AD_get_nVar_FROM_dnMat(dnMat)
    nsurf  = AD_get_nsurf_FROM_dnMat(dnMat)

    CALL AD_dealloc_dnMat(dnMatDiag)
    CALL AD_dealloc_dnMat(dnVec)
    IF (present(dnVecProj)) CALL AD_dealloc_dnMat(dnVecProj)

    IF (nderiv < 0) RETURN

    CALL AD_alloc_dnMat(dnMatDiag,nsurf=nsurf,nVar=nVar,nderiv=nderiv)
    dnMatDiag = ZERO
    CALL AD_alloc_dnMat(dnVec,nsurf=nsurf,nVar=nVar,nderiv=nderiv)
    dnVec     = ZERO

    ! the zero order: normal diagonalization
    allocate(Eig(nsurf))
    allocate(Vec(nsurf,nsurf))
    allocate(tVec(nsurf,nsurf))
    allocate(Mtemp(nsurf,nsurf))

        !diagonalization(RMat,REigVal,REigVec,nb_diago,diago_type,sort,phase,IEigVe
    CALL diagonalization(dnMat%d0,Eig,Vec,nsurf,diago_type=type_diag_loc,sort=1,phase=.TRUE.)

    IF (present(dnVec0)) THEN
      IF (debug) write(out_unit,*) 'Vec0 is present: the eigenvector phases are checked'
      flush(out_unit)

       DO i=1,nsurf
         IF (dot_product(dnVec0%d0(:,i),Vec(:,i)) < ZERO) THEN
            IF (debug) write(out_unit,*) 'Change phase:',i
            Vec(:,i) = -Vec(:,i)
          END IF
       END DO

       IF (debug) THEN
         write(out_unit,*) 'Vec before rotation'
         CALL Write_Mat(Vec,nio=out_unit,nbcol=5)
       END IF

       !For degenerated eigenvectors (works only with 2 vectors)
       DO i=1,nsurf-1
         IF ( abs(Eig(i)-Eig(i+1)) < TEN*epsi) THEN
           j = i+1
           IF (debug) write(out_unit,*) 'degenerated vectors',i,j

           aii = dot_product(dnVec0%d0(:,i),Vec(:,i))
           aji = dot_product(dnVec0%d0(:,j),Vec(:,i))
           aij = dot_product(dnVec0%d0(:,i),Vec(:,j))
           ajj = dot_product(dnVec0%d0(:,j),Vec(:,j))

           !change the phase of one vector (i) if det(Mij)<0
           IF ((aii*ajj-aij*aji) < 0) THEN
             IF (debug) write(out_unit,*) 'det < 0',(aii*ajj-aij*aji)
             Vec(:,i) = -Vec(:,i)
             aii = -aii
             aji = -aji
           ELSE
             IF (debug) write(out_unit,*) 'det > 0'
           END IF
           IF (debug) write(out_unit,*) 'aii,ajj,aji,aij',aii,ajj,aji,aij

           Mij(1,:) = [aii,aij]
           Mij(2,:) = [aji,ajj]

           th = atan2(aji-aij,aii+ajj) ! we have to test with +pi as well

           cc = cos(th)
           ss = sin(th)

           Rot(1,:) = [ cc,ss]
           Rot(2,:) = [-ss,cc]

           RMij = matmul(Rot,Mij)
           RMij(1,1) = RMij(1,1)-1 ; RMij(2,2) = RMij(2,2)-1
           IF (debug) write(out_unit,*) 'RMij',RMij
           err1 = sqrt(sum(RMij**2))
           IF (debug) write(out_unit,*) 'Err, th',th,err1
           ! th+pi => Rot=-Rot
           RMij = -matmul(Rot,Mij) ; RMij(1,1) = RMij(1,1)-1 ; RMij(2,2) = RMij(2,2)-1
           IF (debug) write(out_unit,*) 'RMij',RMij
           err2 = sqrt(sum(RMij**2))
           IF (debug) write(out_unit,*) 'Err, th+pi',th+pi,err2

           IF (err2 < err1) THEN
             th = th+pi
             cc = -cc
             ss = -ss
           END IF

           IF (debug) write(out_unit,*) 'theta',th

           IF (abs(th) < epsi) CYCLE

           Vj       = Vec(:,j)
           Vi       = Vec(:,i)
           Vec(:,i) =  cc * Vi + ss * Vj
           Vec(:,j) = -ss * Vi + cc * Vj
         END IF
       END DO

       IF (debug) write(out_unit,*) 'Change phase?'
       flush(out_unit)

       DO i=1,nsurf
         IF (dot_product(dnVec0%d0(:,i),Vec(:,i)) < ZERO) THEN
            IF (debug) write(out_unit,*) 'Change phase:',i
            Vec(:,i) = -Vec(:,i)
          END IF
       END DO


       max_diff = -ONE
       i_max    = 0
       DO i=1,nsurf
         aii = dot_product(dnVec0%d0(:,i),Vec(:,i))
         IF (abs(aii-ONE) > max_diff) THEN
           aii_max  = aii
           max_diff = abs(aii-ONE)
           i_max    = i
         END IF
         IF (debug) write(out_unit,*) '<Vec0(:,i)|Vec(:,i)> :',i,aii
       END DO
       IF (max_diff > 0.2_Rkind .OR. debug) THEN
         write(out_unit,*) 'Largest difference to one of <Vec0(:,i)|Vec(:,i)> :',i_max,aii_max
         write(out_unit,*) 'Vec:'
         CALL Write_Mat(Vec,nio=out_unit,nbcol=5)
         write(out_unit,*) 'Vec0:'
         CALL Write_Mat(dnVec0%d0,nio=out_unit,nbcol=5)
       END IF
    ELSE
      IF (debug) write(out_unit,*) 'Vec0 is absent: the eigenvector phases are not checked'
    END IF

    tVec         = transpose(Vec)
    dnVec%d0     = matmul(tVec,Vec)                  ! Identity matrix
    dnMatDiag%d0 = matmul(tVec,matmul(dnMat%d0,Vec)) ! diagonal matrix

    dnMat_OnVec = matmul(tVec,matmul(dnMat,Vec)) ! dnMat on the Eigenvector basis
    !  for dnMat_OnVec%d0: Eigenvalues on the diagonal

    IF (nderiv > 0) THEN

      DO id=1,nVar
        Mtemp = dnMat_OnVec%d1(:,:,id)

        DO i=1,nsurf
          ! d1Eig
          dnMatDiag%d1(i,i,id) = Mtemp(i,i)

          ! d1Vec: projection on <i|
          dnVec%d1(i,i,id) = ZERO

          ! d1Vec: projection on <j|
          DO j=1,nsurf
            IF (j == i) CYCLE ! already done
            IF (abs(Eig(i)-Eig(j)) < epsi) CYCLE ! for degenerated eigenvalues

            dnVec%d1(j,i,id) = Mtemp(j,i)/(Eig(i)-Eig(j))

          END DO

        END DO

      END DO


    END IF

    IF (nderiv > 1) THEN

      DO id=1,nVar
      DO jd=1,nVar
        Mtemp = dnMat_OnVec%d2(:,:,jd,id) +                             &
                      matmul(dnMat_OnVec%d1(:,:,id),dnVec%d1(:,:,jd)) + &
                      matmul(dnMat_OnVec%d1(:,:,jd),dnVec%d1(:,:,id))
        DO i=1,nsurf
          Mtemp(:,i) = Mtemp(:,i) -                                     &
                                dnMatDiag%d1(i,i,id)*dnVec%d1(:,i,jd) - &
                                dnMatDiag%d1(i,i,jd)*dnVec%d1(:,i,id)
        END DO

        DO i=1,nsurf
          ! d1Eig
          dnMatDiag%d2(i,i,jd,id) = Mtemp(i,i)

          ! d1Vec: projection on <i|
          dnVec%d2(i,i,jd,id) = -dot_product(dnVec%d1(:,i,id),dnVec%d1(:,i,jd))

          ! d1Vec: projection on <j|
          DO j=1,nsurf
            IF (j == i) CYCLE ! already done
            IF (abs(Eig(i)-Eig(j)) < epsi) CYCLE ! for degenerated eigenvalues

            dnVec%d2(j,i,jd,id) = Mtemp(j,i)/(Eig(i)-Eig(j))

          END DO

        END DO

      END DO
      END DO

    END IF

    IF (nderiv > 2) THEN

      DO id=1,nVar
      DO jd=1,nVar
      DO kd=1,nVar

        Mtemp = dnMat_OnVec%d3(:,:,kd,jd,id) +                          &
                   matmul(dnMat_OnVec%d2(:,:,kd,id),dnVec%d1(:,:,jd)) + &
                   matmul(dnMat_OnVec%d2(:,:,jd,kd),dnVec%d1(:,:,id)) + &
                   matmul(dnMat_OnVec%d2(:,:,id,jd),dnVec%d1(:,:,kd)) + &
                   matmul(dnMat_OnVec%d1(:,:,id),dnVec%d2(:,:,jd,kd)) + &
                   matmul(dnMat_OnVec%d1(:,:,kd),dnVec%d2(:,:,id,jd)) + &
                   matmul(dnMat_OnVec%d1(:,:,jd),dnVec%d2(:,:,kd,id))

        DO i=1,nsurf
          Mtemp(:,i) = Mtemp(:,i) -                                     &
                             dnMatDiag%d2(i,i,kd,id)*dnVec%d1(:,i,jd) - &
                             dnMatDiag%d2(i,i,jd,kd)*dnVec%d1(:,i,id) - &
                             dnMatDiag%d2(i,i,id,jd)*dnVec%d1(:,i,kd) - &
                             dnMatDiag%d1(i,i,id)*dnVec%d2(:,i,jd,kd) - &
                             dnMatDiag%d1(i,i,kd)*dnVec%d2(:,i,id,jd) - &
                             dnMatDiag%d1(i,i,jd)*dnVec%d2(:,i,kd,id)
        END DO

        DO i=1,nsurf
          ! d1Eig
          dnMatDiag%d3(i,i,kd,jd,id) = Mtemp(i,i)

          ! d1Vec: projection on <i|
          dnVec%d3(i,i,kd,jd,id) = - &
               dot_product(dnVec%d1(:,i,kd),dnVec%d2(:,i,jd,id)) - &
               dot_product(dnVec%d1(:,i,jd),dnVec%d2(:,i,id,kd)) - &
               dot_product(dnVec%d1(:,i,id),dnVec%d2(:,i,kd,jd))

          ! d1Vec: projection on <j|
          DO j=1,nsurf
            IF (j == i) CYCLE ! already done
            IF (abs(Eig(i)-Eig(j)) < epsi) CYCLE ! for degenerated eigenvalues

            dnVec%d3(j,i,kd,jd,id) = Mtemp(j,i)/(Eig(i)-Eig(j))

          END DO

        END DO

      END DO
      END DO
      END DO
    END IF


    IF (present(dnVecProj)) dnVecProj = dnVec ! since here dnVec are the projected vectors

    ! unproject the dnVec: correct yes (check with transpose(Vec).dnMat.Vec )
    dnVec%d0(:,:) = Vec
    IF (nderiv > 0) THEN
      DO id=1,nVar
        dnVec%d1(:,:,id) = matmul(Vec,dnVec%d1(:,:,id))
      END DO
    END IF
    IF (nderiv > 1) THEN
      DO id=1,nVar
      DO jd=1,nVar
        dnVec%d2(:,:,jd,id) = matmul(Vec,dnVec%d2(:,:,jd,id))
      END DO
      END DO
    END IF
    IF (nderiv > 2) THEN
      DO id=1,nVar
      DO jd=1,nVar
      DO kd=1,nVar
        dnVec%d3(:,:,kd,jd,id) = matmul(Vec,dnVec%d3(:,:,kd,jd,id))
      END DO
      END DO
      END DO
    END IF

    IF (allocated(Eig))   deallocate(Eig)
    IF (allocated(Vec))   deallocate(Vec)
    IF (allocated(tVec))  deallocate(tVec)
    IF (allocated(Mtemp)) deallocate(Mtemp)

    CALL AD_dealloc_dnMat(dnMat_OnVec)

    IF (debug) THEN
      IF (present(dnVecProj)) CALL AD_Write_dnMat(dnVecProj,info='dnVecProj')
      CALL AD_Write_dnMat(dnVec,info='dnVec')
      CALL AD_Write_dnMat(dnMatDiag,info='dnMatDiag')
      write(out_unit,*) ' END ',name_sub
      flush(out_unit)
    END IF

  END SUBROUTINE AD_DIAG_dnMat

END MODULE ADdnSVM_dnMat_m
