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
MODULE QDUtil_Quadrature_m
#ifndef __WITHRK16
#define __WITHRK16 1
#endif
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE

  TYPE Quadrature_t
    real (kind=Rkind), allocatable :: x(:,:) ! x(ndim,nq): grid points
    real (kind=Rkind), allocatable :: w(:)   ! w(ndim,nq): weights
    character (len=:), allocatable :: name
    real (kind=Rkind)              :: Sii = -HUGE(ONE) ! errors of the overlap (diagonal)
    real (kind=Rkind)              :: Sij = -HUGE(ONE) ! errors of the overlap (off diagonal)
    real (kind=Rkind),  allocatable :: d0gb(:,:)  ! d0gb(nq,nb)
    real (kind=Rkind),  allocatable :: d0bgw(:,:) ! d0bgw(nb,nq)
#if __WITHRK16 == 1
  CONTAINS
    PROCEDURE, PRIVATE, PASS(Quadrature)   :: Quadrature_Rk16_TO_Quadrature_QDUtil
    GENERIC,   PUBLIC  :: assignment(=) => Quadrature_Rk16_TO_Quadrature_QDUtil
#endif
  END TYPE Quadrature_t

  PUBLIC :: Quadrature_t
  PUBLIC :: Init_Quadrature,Init_Quadrature_HP,Init_Quadrature_NP
  PUBLIC :: dealloc_Quadrature,Write_Quadrature
  PUBLIC :: Test_Quadrature_QDUtil

  INTERFACE Init_Quadrature_HP
    MODULE PROCEDURE Init_Quadrature_HP_QDUtil
  END INTERFACE
  INTERFACE Init_Quadrature_NP
    MODULE PROCEDURE Init_Quadrature_NP_QDUtil
  END INTERFACE
  INTERFACE Init_Quadrature
    MODULE PROCEDURE Init_Quadrature_NP_QDUtil
  END INTERFACE
  INTERFACE dealloc_Quadrature
    MODULE PROCEDURE dealloc_Quadrature_QDUtil
  END INTERFACE
  INTERFACE Write_Quadrature
    MODULE PROCEDURE Write_Quadrature_QDUtil
  END INTERFACE
  INTERFACE Check_Overlap
    MODULE PROCEDURE Check_Overlap_QDUtil
  END INTERFACE
  INTERFACE Make_d0bgw
    MODULE PROCEDURE Make_d0bgw_QDUtil
  END INTERFACE
  
  CONTAINS
  SUBROUTINE Init_Quadrature_NP_QDUtil(Quadrature,nq,name,A,B,xc,scale,isym_grid,err)
    USE QDUtil_String_m
    USE QDUtil_diago_m
    USE QDUtil_RW_MatVec_m
    USE QDUtil_HermiteH_m
    USE QDUtil_BoxAB_m
    USE QDUtil_Fourier_m
    USE QDUtil_LegendreP_m
    IMPLICIT NONE

    TYPE (Quadrature_t), intent(inout)         :: Quadrature
    integer,             intent(in)            :: nq
    character (len=*),   intent(in)            :: name
    real (kind=Rkind),   intent(in),  optional :: A,B
    real (kind=Rkind),   intent(in),  optional :: xc,scale
    integer,             intent(in),  optional :: isym_grid
    integer,             intent(out), optional :: err


    TYPE(BoxAB_t)                   :: BoxAB
    TYPE(FourierAB_t)               :: FourierAB
    TYPE(HO_t)                      :: HO
    real (kind=Rkind)               :: xc_loc,scale_loc
    integer                         :: i,isym_grid_loc,err_loc
    real (kind=Rkind), allocatable  :: Xmat(:,:),DVR(:,:),x(:),w(:)

    integer                          :: ib,jb,ib_max,iq,nb
    real (kind=Rkind),   parameter   :: ZeroTresh    = TEN**2*epsilon(ONE)

    !---------------------------------------------------------------------
    logical,parameter :: debug= .FALSE.
    !logical,parameter :: debug= .TRUE.
    character (len=*), parameter :: name_sub='Init_Quadrature_NP_QDUtil'
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*)
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*)
      write(out_unit,*) 'ZeroTresh',ZeroTresh
      write(out_unit,*)
    END IF
    flush(out_unit)
    !---------------------------------------------------------------------
    CALL dealloc_Quadrature(Quadrature)
    err_loc = 0

    IF (present(isym_grid)) THEN
      isym_grid_loc = isym_grid
    ELSE
      isym_grid_loc = 0
    END IF

    !=========================================================================
    IF (nq < 1) THEN
      err_loc = -2
      write(out_unit,*) 'ERROR in Init_Quadrature_NP_QDUtil: nq < 1'
      write(out_unit,*) 'nq: ',nq
      IF (.NOT. present(err)) STOP 'ERROR in Init_Quadrature_NP_QDUtil: nq < 1'
    END IF
    nb = nq
    !=========================================================================

    !=========================================================================
    IF (TO_lowercase(name) == 'fourierab' .OR. TO_lowercase(name) == 'boxab') THEN
      IF (present(A) .AND. present(B)) THEN
        IF (B <= A) THEN
          err_loc = -2
          write(out_unit,*) 'ERROR in Init_Quadrature_NP_QDUtil: B <= A for FourierAB or BoxAB quadrature'
          write(out_unit,*) 'A,B: ',A,B
          IF (.NOT. present(err)) &
            STOP 'ERROR in Init_Quadrature_NP_QDUtil: B <= A for FourierAB or BoxAB quadrature'
        END IF
      ELSE ! A or B are not present
        err_loc = -2
        write(out_unit,*) 'ERROR in Init_Quadrature_NP_QDUtil: A and B must be present for FourierAB or BoxAB quadrature'
        write(out_unit,*) 'present(A),present(B): ',present(A),present(B)
        IF (.NOT. present(err)) &
          STOP 'ERROR in Init_Quadrature_NP_QDUtil: A and B must be present for FourierAB or BoxAB quadrature'
      END IF
    END IF
    !=========================================================================

    !=========================================================================
    xc_loc = ZERO
    IF (present(xc)) xc_loc = xc
    scale_loc = ONE
    IF (present(scale)) scale_loc = scale

    IF (scale_loc <= ZERO) THEN
      err_loc = -2
      write(out_unit,*) 'ERROR in Init_Quadrature_NP_QDUtil: the scale factor (scale) is <= ZERO'
      write(out_unit,*) 'present(scale): ',present(scale)
      write(out_unit,*) 'scale_loc:      ',scale_loc
      IF (.NOT. present(err)) &
        STOP 'ERROR in Init_Quadrature_NP_QDUtil: the scale factor (scale) is <= ZERO'
    END IF
    !=========================================================================

    !=========================================================================
    IF (err_loc == 0) THEN
      Quadrature%name = name
      SELECT CASE(TO_lowercase(name))
      CASE ('fourier')
        FourierAB = FourierAB_t(A=-PI,B=PI,ReNorm=.TRUE.,isym_grid=isym_grid_loc)
        CALL Fourier_Quadrature(x,w,nq,FourierAB,err_loc)
       
        IF (err_loc == 0) THEN
          Quadrature%x = reshape(x,shape=[1,nq])
          Quadrature%w = w
         
          allocate(Quadrature%d0gb(nq,nb))
          CALL TabGB_Fourier(Quadrature%d0gb,x,FourierAB)
        END IF
       
      CASE ('fourierab')
        FourierAB = FourierAB_t(A=A,B=B,ReNorm=.TRUE.,isym_grid=isym_grid_loc)
        CALL Fourier_Quadrature(x,w,nq,FourierAB,err_loc)
       
        IF (err_loc == 0) THEN
          Quadrature%x = reshape(x,shape=[1,nq])
          Quadrature%w = w
         
          allocate(Quadrature%d0gb(nq,nb))
          CALL TabGB_Fourier(Quadrature%d0gb,x,FourierAB)
        END IF
      CASE ('sine')
        BoxAB = BoxAB_t(A=ZERO,B=PI,ReNorm=.TRUE.,isym_grid=isym_grid_loc)
        CALL BoxAB_Quadrature(x,w,nq,BoxAB,err_loc)
       
        IF (err_loc == 0) THEN
          Quadrature%x = reshape(x,shape=[1,nq])
          Quadrature%w = w
         
          allocate(Quadrature%d0gb(nq,nb))
          CALL TabGB_BoxAB(Quadrature%d0gb,x,BoxAB)
        END IF
      CASE ('boxab')
        BoxAB = BoxAB_t(A=A,B=B,ReNorm=.TRUE.,isym_grid=isym_grid_loc)
        CALL BoxAB_Quadrature(x,w,nq,BoxAB,err_loc)
       
        IF (err_loc == 0) THEN
          Quadrature%x = reshape(x,shape=[1,nq])
          Quadrature%w = w
         
          allocate(Quadrature%d0gb(nq,nb))
          CALL TabGB_BoxAB(Quadrature%d0gb,x,BoxAB)
        END IF
       
      CASE ('ho','hermiteh')
        allocate(x(nq))
        allocate(DVR(nq,nq))
       
        allocate(Xmat(nq,nq))
        CALL X_HermiteH(Xmat)
        IF (debug) CALL Write_Mat(Xmat,nio=out_unit,nbcol=5,info='Xmat')
        CALL diagonalization(Xmat,x,DVR)
        IF (debug) CALL Write_Mat(DVR,nio=out_unit,nbcol=5,info='DVR')
        Quadrature%x = reshape(x,shape=[1,nq])
       
        allocate(Quadrature%d0gb(nq,nb))
        allocate(Quadrature%w(nq))
        CALL TabGB_HermiteH(Quadrature%d0gb,x,gauss=.TRUE.,renorm=.TRUE.)
       
        ! weight
        DO iq=1,nq
          ib_max = maxloc(abs(Quadrature%d0gb(iq,:)),dim=1)
          Quadrature%w(iq) = (DVR(ib_max,iq)/Quadrature%d0gb(iq,ib_max))**2
        END DO
        !CALL Weight_OF_grid_QDUtil(Quadrature%w,d0gb,nb,nq)

        IF (TO_lowercase(name) == 'ho') THEN
          HO = HO_t(xc=xc_loc,scale=scale_loc,ReNorm=.TRUE.)
          x = x/HO%Scale + HO%xc
          Quadrature%x = reshape(x,shape=[1,nq])
          Quadrature%w = Quadrature%w * HO%Scale
          CALL TabGB_HO(Quadrature%d0gb,x,HO)
        END IF

      CASE ('legendrep','pl0')
        allocate(x(nq))
        allocate(DVR(nq,nq))
       
        allocate(Xmat(nq,nq))
        CALL X_LegendreP(Xmat)
        IF (debug) CALL Write_Mat(Xmat,nio=out_unit,nbcol=5,info='Xmat')
        CALL diagonalization(Xmat,x,DVR)
        IF (debug) CALL Write_Mat(DVR,nio=out_unit,nbcol=5,info='DVR')
        Quadrature%x = reshape(x,shape=[1,nq])
       
        allocate(Quadrature%d0gb(nq,nb))
        allocate(Quadrature%w(nq))
        CALL TabGB_LegendreP(Quadrature%d0gb,x,renorm=.TRUE.)
       
        ! weights
        DO iq=1,nq
          ib_max = maxloc(abs(Quadrature%d0gb(iq,:)),dim=1)
          Quadrature%w(iq) = (DVR(ib_max,iq)/Quadrature%d0gb(iq,ib_max))**2
        END DO
        !CALL Weight_OF_grid_QDUtil(Quadrature%w,d0gb,nb,nq)

      CASE default
        err_loc = -3
        IF (.NOT. present(err)) &
          STOP 'ERROR in Init_Quadrature_NP_QDUtil: no default quadrature'
      END SELECT
    END IF
    !=========================================================================

    !=========================================================================
    IF (err_loc == 0) THEN
      CALL Make_d0bgw(Quadrature)
      CALL Check_Overlap(Quadrature,nio=out_unit)
      IF (abs(Quadrature%Sii) >= nb*ZeroTresh .OR. abs(Quadrature%Sij) >= nb*ZeroTresh) err_loc = 1
    END IF
    !=========================================================================

    IF (allocated(x))    deallocate(x)
    IF (allocated(w))    deallocate(w)
    IF (allocated(Xmat)) deallocate(Xmat)
    IF (allocated(DVR))  deallocate(DVR)

    IF (present(err)) err = err_loc
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'err_loc: ',err_loc
      CALL Write_Quadrature(Quadrature)
      write(out_unit,*) 'END ',name_sub
    END IF
    !---------------------------------------------------------------------
  END SUBROUTINE Init_Quadrature_NP_QDUtil
  SUBROUTINE Init_Quadrature_HP_QDUtil(Quadrature,nq,name,A,B,xc,scale,isym_grid,err)
    USE QDUtil_String_m
    USE QDUtil_RW_MatVec_m
    USE QDUtil_Quadrature_Rk16_m
    IMPLICIT NONE

    TYPE (Quadrature_t), intent(inout)         :: Quadrature
    integer,             intent(in)            :: nq
    character (len=*),   intent(in)            :: name
    real (kind=Rkind),   intent(in),  optional :: A,B
    real (kind=Rkind),   intent(in),  optional :: xc,scale
    integer,             intent(in),  optional :: isym_grid
    integer,             intent(out), optional :: err

#if __WITHRK16 == 1
    TYPE (Quadrature_Rk16_t)        :: Quadrature_Rk16
#endif

    real (kind=Rkind)               :: A_loc,B_loc
    real (kind=Rkind)               :: xc_loc,scale_loc
    integer                         :: isym_grid_loc,err_loc

    integer                         :: i

    integer                          :: ib,jb,ib_max,iq,nb
    real (kind=Rkind),   parameter   :: ZeroTresh    = TEN**2*epsilon(ONE)

    !---------------------------------------------------------------------
    logical,parameter :: debug= .FALSE.
    !logical,parameter :: debug= .TRUE.
    character (len=*), parameter :: name_sub='Init_Quadrature_HP_QDUtil'
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*)
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*)
      write(out_unit,*) 'ZeroTresh',ZeroTresh
      write(out_unit,*)
    END IF
    flush(out_unit)
    !---------------------------------------------------------------------
    CALL dealloc_Quadrature(Quadrature)
    err_loc = 0

    IF (present(isym_grid)) THEN
      isym_grid_loc = isym_grid
    ELSE
      isym_grid_loc = 0
    END IF

    !=========================================================================
    IF (nq < 1) THEN
      err_loc = -2
      write(out_unit,*) 'ERROR in ',name_sub,' : nq < 1'
      write(out_unit,*) 'nq: ',nq
      IF (.NOT. present(err)) STOP 'ERROR in Init_Quadrature_HP_QDUtil: nq < 1'
    END IF
    nb = nq
    !=========================================================================

    !=========================================================================
    IF (TO_lowercase(name) == 'fourierab' .OR. TO_lowercase(name) == 'boxab') THEN
      IF (present(A) .AND. present(B)) THEN
        IF (B <= A) THEN
          err_loc = -2
          write(out_unit,*) 'ERROR in ',name_sub,': B <= A for FourierAB or BoxAB quadrature'
          write(out_unit,*) 'A,B: ',A,B
          IF (.NOT. present(err)) &
            STOP 'ERROR in Init_Quadrature_HP_QDUtil: B <= A for FourierAB or BoxAB quadrature'
        END IF
      ELSE ! A or B are not present
        err_loc = -2
        write(out_unit,*) 'ERROR in ',name_sub,': A and B must be present for FourierAB or BoxAB quadrature'
        write(out_unit,*) 'present(A),present(B): ',present(A),present(B)
        IF (.NOT. present(err)) &
          STOP 'ERROR in Init_Quadrature_HP_QDUtil: A and B must be present for FourierAB or BoxAB quadrature'
      END IF
    END IF
    A_loc = ZERO
    IF (present(A)) A_loc = A
    B_loc = PI
    IF (present(B)) B_loc = B
    !=========================================================================

    !=========================================================================
    xc_loc = ZERO
    IF (present(xc)) xc_loc = xc
    scale_loc = ONE
    IF (present(scale)) scale_loc = scale

    IF (scale_loc <= ZERO) THEN
      err_loc = -2
      write(out_unit,*) 'ERROR in ',name_sub,': the scale factor (scale) is <= ZERO'
      write(out_unit,*) 'present(scale): ',present(scale)
      write(out_unit,*) 'scale_loc:      ',scale_loc
      IF (.NOT. present(err)) &
        STOP 'ERROR in Init_Quadrature_HP_QDUtil: the scale factor (scale) is <= ZERO'
    END IF
    !=========================================================================
#if __WITHRK16 == 1
    !=========================================================================
    IF (err_loc == 0) THEN
      CALL Init_Quadrature(Quadrature_Rk16,nq=nq,name=name,                           &
                           xc=real(xc_loc,kind=Rk16),scale=real(scale_loc,kind=Rk16), &
                           A=real(A_loc,kind=Rk16),B=real(B_loc,kind=Rk16),           &
                           isym_grid=isym_grid_loc,err=err_loc)
    END IF
    IF (err_loc == 0) THEN 
      Quadrature = Quadrature_Rk16 ! quadrature tranfer
      CALL dealloc_Quadrature(Quadrature_Rk16)
      CALL Check_Overlap(Quadrature,nio=out_unit)
    END IF
#else
    err_loc = -10
    write(out_unit,*) 'ERROR in ',name_sub,': Reals with quadruple precision (real128) are not available'
    write(out_unit,*) 'Rk16 (real128): ',Rk16
    write(out_unit,*) '=> use only the Init_Quadrature_NP subroutine.'
    IF (.NOT. present(err)) &
        STOP 'ERROR in Init_Quadrature_HP_QDUtil: Reals with quadruple precision (real128) are not available'
#endif
    IF (present(err)) err = err_loc
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'err_loc: ',err_loc
      CALL Write_Quadrature(Quadrature)
      write(out_unit,*) 'END ',name_sub
    END IF
    !---------------------------------------------------------------------
  END SUBROUTINE Init_Quadrature_HP_QDUtil
  SUBROUTINE dealloc_Quadrature_QDUtil(Quadrature)
    IMPLICIT NONE

    TYPE (Quadrature_t), intent(inout)        :: Quadrature


    IF (allocated(Quadrature%name))  deallocate(Quadrature%name)
    IF (allocated(Quadrature%x))     deallocate(Quadrature%x)
    IF (allocated(Quadrature%w))     deallocate(Quadrature%w)

    Quadrature%Sii = -HUGE(ONE)
    Quadrature%Sij = -HUGE(ONE)

    IF (allocated(Quadrature%d0gb))  deallocate(Quadrature%d0gb)
    IF (allocated(Quadrature%d0bgw)) deallocate(Quadrature%d0bgw)

  END SUBROUTINE dealloc_Quadrature_QDUtil
#if __WITHRK16 == 1
  SUBROUTINE Quadrature_Rk16_TO_Quadrature_QDUtil(Quadrature,Quadrature_Rk16)
    USE QDUtil_Quadrature_Rk16_m
    IMPLICIT NONE
    CLASS (Quadrature_t),     intent(out)       :: Quadrature
    TYPE (Quadrature_Rk16_t), intent(in)        :: Quadrature_Rk16

    Quadrature%name  = Quadrature_Rk16%name
    Quadrature%x     = Quadrature_Rk16%x
    Quadrature%w     = Quadrature_Rk16%w
    Quadrature%d0gb  = Quadrature_Rk16%d0gb
    Quadrature%d0bgw = Quadrature_Rk16%d0bgw

  END SUBROUTINE Quadrature_Rk16_TO_Quadrature_QDUtil
#endif
  SUBROUTINE Check_Overlap_QDUtil(Quadrature,nio)
    USE QDUtil_RW_MatVec_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (Quadrature_t), intent(inout)        :: Quadrature
    integer,             intent(in)           :: nio

    integer :: ib,nb
    real (kind=Rkind), allocatable   :: S(:,:)
    real (kind=Rkind) :: Sii,Sij


    !---------------------------------------------------------------------
    logical,parameter :: debug= .FALSE.
    !logical,parameter :: debug= .TRUE.
    character (len=*), parameter :: name_sub='Check_Overlap_QDUtil'
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*)
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*)
    END IF
    !---------------------------------------------------------------------

    nb = size(Quadrature%d0gb,dim=2)

    S = matmul(Quadrature%d0bgw,Quadrature%d0gb)
    Sii = ZERO
    Sij = ZERO
    DO ib=1,nb
      Sii = max(Sii,abs(S(ib,ib)-ONE))
      Sij = max(Sij,maxval(abs(S(1:ib-1,ib))))
      Sij = max(Sij,maxval(abs(S(ib+1:nb,ib))))
    END DO
    Quadrature%Sii = Sii
    Quadrature%Sij = Sij

    IF (debug .OR. print_level >= 2 .OR. Sii > ONETENTH**6 .OR. Sij > ONETENTH**6 ) THEN
      write(nio,*)
      write(nio,*) 'Sii,Sij',Quadrature%Sii,Quadrature%Sij
      write(nio,*)
      CALL write_Mat(S,nio=nio,nbcol=5,info='S')
      write(nio,*)
      DO ib=1,nb
        write(nio,*) 'd0gb(:,',TO_string(ib),')',Quadrature%d0gb(:,ib)
      END DO
      !CALL write_Mat(Quadrature%d0gb,nio=nio,nbcol=5,info='d0gb')
    ELSE
      IF (print_level == 1 ) write(nio,*) 'Sii,Sij',Quadrature%Sii,Quadrature%Sij
    END IF

    IF (allocated(S))     deallocate(S)

    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    !---------------------------------------------------------------------
  END SUBROUTINE Check_Overlap_QDUtil
  SUBROUTINE Make_d0bgw_QDUtil(Quadrature)
    USE QDUtil_RW_MatVec_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (Quadrature_t), intent(inout)        :: Quadrature

    integer :: ib,nb,nq


    !---------------------------------------------------------------------
    logical,parameter :: debug= .FALSE.
    !logical,parameter :: debug= .TRUE.
    character (len=*), parameter :: name_sub='Make_d0bgw_QDUtil'
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*)
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*)
    END IF
    !---------------------------------------------------------------------

    nq = size(Quadrature%d0gb,dim=1)
    nb = size(Quadrature%d0gb,dim=2)
    allocate(Quadrature%d0bgw(nb,nq))
    DO ib=1,nb
      Quadrature%d0bgw(ib,:) = Quadrature%d0gb(:,ib) * Quadrature%w(:)
    END DO

    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    !---------------------------------------------------------------------
  END SUBROUTINE Make_d0bgw_QDUtil
  SUBROUTINE Write_Quadrature_QDUtil(Quadrature,nio,info)
    USE QDUtil_String_m
    IMPLICIT NONE
    TYPE (Quadrature_t), intent(in)           :: Quadrature
    integer,             intent(in), optional :: nio
    character (len=*),   intent(in), optional :: info

    integer :: i,nio_loc

    nio_loc = out_unit
    IF (present(nio)) nio_loc = nio

    write(nio_loc,'(a)') '======================================================================='
    IF (present(info)) write(nio_loc,'(a)') info

    IF (allocated(Quadrature%name)) THEN 
      write(nio_loc,'(2a)') '== Quadrature for basis set: ',Quadrature%name
    ELSE
      write(nio_loc,'(a)') '== Quadrature for basis set: no name yet'
    END IF
    IF (allocated(Quadrature%x) .AND. allocated(Quadrature%w)) THEN 
      write(nio_loc,*) 'nq: ',size(Quadrature%w)
    ELSE
      write(nio_loc,*) 'nq: not initialized'
    END IF
    IF (Quadrature%Sii /= -HUGE(ONE) .OR. Quadrature%Sii /= -HUGE(ONE)) THEN
      write(nio_loc,*) 'Sii,Sij (errors on the overlap)',Quadrature%Sii,Quadrature%Sij
    ELSE
      write(nio_loc,*) 'Sii,Sij (errors on the overlap): -HUGE, -HUGE'
    END IF

    IF (allocated(Quadrature%x)) THEN
      DO i=1,size(Quadrature%x,dim=1)
        write(nio_loc,*) 'x(' // TO_string(i) // ',:): ',(Quadrature%x(i,:))
      END DO
    ELSE
      write(nio_loc,'(a)') 'Quadrature%x is not allocated'
    END IF
    IF (allocated(Quadrature%w)) THEN
        write(nio_loc,*) 'w(:):   ',Quadrature%w
    ELSE
      write(nio_loc,'(a)') 'Quadrature%w is not allocated'
    END IF
    write(nio_loc,'(a)') '======================================================================='

  END SUBROUTINE Write_Quadrature_QDUtil


  SUBROUTINE Weight_OF_grid_QDUtil(w,d0RGB,nb,nq)
    USE QDUtil_Matrix_m
    IMPLICIT NONE
    integer,           intent(in)    :: nb,nq
    real (kind=Rkind), intent(inout) :: w(nq)
    real (kind=Rkind), intent(in)    :: d0RGB(nq,nb)

    !---------- working variables ----------------------------------------
    real (kind=Rkind) :: A(nb**2,nq)
    real (kind=Rkind) :: B(nb**2)

    real (kind=Rkind) :: AtA(nq,nq)
    real (kind=Rkind) :: AtB(nq)
    integer           ::  i,j,ij

    !---------------------------------------------------------------------
    logical,parameter :: debug= .FALSE.
    !logical,parameter :: debug= .TRUE.
    character (len=*), parameter :: name_sub='Weight_OF_grid_QDUtil'
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*)
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*)
    END IF
    !---------------------------------------------------------------------

    ! matrices of the linear (rectangular) system A.W=B
    ij = 0
    B(:) = ZERO
    DO i=1,nb
    DO j=1,nb
      ij = ij + 1
      IF (i == j) B(ij) = ONE
      A(ij,:) = d0RGB(:,i)*d0RGB(:,j)
    END DO
    END DO

    ! matrices of the linear (square nq*nq) system AtA.W=AtB
    AtA(:,:) = matmul(transpose(A),A)
    AtB(:)   = matmul(transpose(A),B)

    ! Solve the linear system AtA.W=AtB
    W = LinearSys_Solve(AtA,AtB)

    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'w',w
      write(out_unit,*) 'END ',name_sub
    END IF
    !---------------------------------------------------------------------
  END SUBROUTINE Weight_OF_grid_QDUtil
  SUBROUTINE Test_Quadrature_QDUtil()
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_Quadrature_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL set_print_level(prtlev=1,force=.TRUE.)
    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

    CALL Test_QuadratureError_QDUtil()

    CALL Test_QuadratureSine_QDUtil()
    CALL Test_QuadratureFourier_QDUtil()
    CALL Test_QuadratureHO_QDUtil()
    CALL Test_QuadratureLegendre_QDUtil()

    CALL Test_Quadrature_NPHP_QDUtil()
    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_Quadrature_QDUtil
  SUBROUTINE Test_QuadratureHO_QDUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    TYPE (Quadrature_t) :: xw
    integer :: nq,isym_grid,err_grid
    logical :: skip
    character (len=:), allocatable :: info_grid
    character (len=:), allocatable :: name_grid

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QuadratureHO_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------
    CALL Initialize_Test(test_var,test_name='QuadratureHO')

    !========================================================================================
    ! HO quadrature test
    name_grid = 'HermiteH'
    DO nq=1,5
      info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)
      CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,err=err_grid)
      res_test = (err_grid ==0)
      CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
      IF (.NOT. res_test .OR. debug) THEN
        CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
      END IF
    END DO

    name_grid = 'HermiteH'
    nq = 65
    IF (Rkind == Rk4) nq = 25 ! Because in simple precision, it does not work with nq=65
    info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)
    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !========================================================================================

    name_grid = 'HO'
    nq = 11
    info_grid = name_grid // '_xc1._scale0.5' // ' quadrature_nq=' // TO_string(nq)
    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,xc=ONE,scale=HALF,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !========================================================================================

    CALL Finalize_Test(test_var)

    !========================================================================================
    CALL dealloc_Quadrature(xw)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_QuadratureHO_QDUtil
  SUBROUTINE Test_QuadratureLegendre_QDUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    TYPE (Quadrature_t) :: xw
    integer :: nq,isym_grid,err_grid
    logical :: skip
    character (len=:), allocatable :: info_grid
    character (len=:), allocatable :: name_grid

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QuadratureLegendre_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------
    CALL Initialize_Test(test_var,test_name='QuadratureLegendre')

    !========================================================================================
    ! HO quadrature test
    name_grid = 'LegendreP'
    DO nq=1,11
      info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)
      CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,err=err_grid)
      res_test = (err_grid ==0)
      CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
      IF (.NOT. res_test .OR. debug) THEN
        CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
      END IF
    END DO

    name_grid = 'LegendreP'
    nq = 127
    info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)
    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF

    CALL Finalize_Test(test_var)

    !========================================================================================
    CALL dealloc_Quadrature(xw)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_QuadratureLegendre_QDUtil
  SUBROUTINE Test_QuadratureSine_QDUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    TYPE (Quadrature_t) :: xw
    integer :: nq,isym_grid,err_grid
    logical :: skip
    character (len=:), allocatable :: info_grid
    character (len=:), allocatable :: name_grid

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QuadratureSine_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------
    CALL Initialize_Test(test_var,test_name='QuadratureSine')

    !========================================================================================
    ! sine quadrature test
    name_grid = 'sine'
    DO nq=1,8
      info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)

      CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,err=err_grid)
      res_test = (err_grid ==0)
      CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
      IF (.NOT. res_test .OR. debug) THEN
        CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
      END IF
    END DO

    ! BoxAB quadrature test
    nq=10
    name_grid = 'BoxAB'
    info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)

    CALL Init_Quadrature_NP(xw,nq=nq,A=-ONE,B=ONE,name=name_grid,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !========================================================================================

    CALL Finalize_Test(test_var)

    !========================================================================================
    CALL dealloc_Quadrature(xw)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_QuadratureSine_QDUtil
  SUBROUTINE Test_QuadratureFourier_QDUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    TYPE (Quadrature_t) :: xw
    integer :: nq,isym_grid,err_grid
    logical :: skip
    character (len=:), allocatable :: info_grid
    character (len=:), allocatable :: name_grid

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QuadratureFourier_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------
    CALL Initialize_Test(test_var,test_name='QuadratureFourier')

  
    !========================================================================================
    ! Fourier quadrature test
    nq=9
    name_grid = 'Fourier'
    DO nq=1,8
    DO isym_grid=-1,1
      info_grid = name_grid // ' quadrature_isym' // TO_string(isym_grid) // '_nq=' // TO_string(nq)

      skip = (mod(nq,2) == 0 .AND. isym_grid /= 0)
      IF (skip) CYCLE
      CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,isym_grid=isym_grid,err=err_grid)
      res_test = (err_grid ==0)
      CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ', Overlap check: T = ' // TO_string(res_test)))
      IF (.NOT. res_test .OR. debug) THEN
        CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
      END IF
    END DO
    END DO

    ! FourierAB (-1.,1.)
    nq=7
    isym_grid = 0
    name_grid = 'FourierAB'
    info_grid = name_grid // ' quadrature_isym' // TO_string(isym_grid) // '_nq=' // TO_string(nq)

    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,A=-ONE,B=ONE,isym_grid=isym_grid,err=err_grid)
    res_test = (err_grid == 0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !========================================================================================

    CALL Finalize_Test(test_var)

    !========================================================================================
    CALL dealloc_Quadrature(xw)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_QuadratureFourier_QDUtil
  SUBROUTINE Test_QuadratureError_QDUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    integer                          :: iVal,iExaVal
    real (kind=Rkind)                :: Val,ExaVal

    TYPE (Quadrature_t) :: xw
    integer :: nq,isym_grid,err_grid
    logical :: skip
    character (len=:), allocatable :: info_grid
    character (len=:), allocatable :: name_grid

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QuadratureError_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='QuadratureError')

    !========================================================================================
    ! XX quadrature test
    nq=4
    name_grid = 'XXX'
    info_grid = name_grid // ' quadrature' // '_nq=' // TO_string(nq)

    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,err=err_grid)
    res_test = (err_grid == 0)
    CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,info=(info_grid // ': Overlap check: F = ' // TO_string(res_test)))
    write(test_var%test_log_file_unit,*) 'The grid ',name_grid,' does not exist.'
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    !========================================================================================

    !========================================================================================
    ! Fourier with nq=0 test
    nq=0
    name_grid = 'Fourier'
    info_grid = name_grid // ' quadrature' // '_nq=' // TO_string(nq)

    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,err=err_grid)
    res_test = (err_grid == 0)
    CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,info=(info_grid // ': Overlap check: F = ' // TO_string(res_test)))
    write(test_var%test_log_file_unit,*) 'The number grid points is ZERO.'
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    !========================================================================================

    !========================================================================================
    ! BoxAB (1.,1.), it should rise an error (B<=A)
    nq=7
    isym_grid = 0
    name_grid = 'BoxAB'
    info_grid = name_grid // ' quadrature_isym_nq=' // TO_string(nq)

    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,A=ONE,B=ONE,err=err_grid)
    res_test = (err_grid == 0)
    CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,info=(info_grid // ': Overlap check: F = ' // TO_string(res_test)))
    write(test_var%test_log_file_unit,*) 'Quadrature impossible: B = A'
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)

    ! FourierAB (1.,-1.), it should rise an error (B<=A)
    nq=7
    isym_grid = 0
    name_grid = 'FourierAB'
    info_grid = name_grid // ' quadrature_isym' // TO_string(isym_grid) // '_nq=' // TO_string(nq)

    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,A=ONE,B=-ONE,isym_grid=isym_grid,err=err_grid)
    res_test = (err_grid == 0)
    CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,info=(info_grid // ': Overlap check: F = ' // TO_string(res_test)))
    write(test_var%test_log_file_unit,*) 'Quadrature impossible: B <= A'
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    !========================================================================================

    !========================================================================================
    ! This should generate an error on the overlap (the last basis function on the grid is zero)
    nq=6
    isym_grid = -1
    name_grid = 'Fourier'
    info_grid = name_grid // ' quadrature_isym' // TO_string(isym_grid) // '_nq=' // TO_string(nq)

    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,isym_grid=isym_grid,err=err_grid)
    res_test = (err_grid == 0)
    CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,info=(info_grid // ': Overlap check: F = ' // TO_string(res_test)))
    write(test_var%test_log_file_unit,*) &
      'When nb (=nq) is even and when the grid starts in A (-PI), the overlap CANNOT be the identity matrix.'
    write(test_var%test_log_file_unit,*) ' Because,the last basis function on the grid is zero (d0gb(:,ib)=ZERO).'
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)

    nq=4
    isym_grid = 1
    name_grid = 'Fourier'
    info_grid = name_grid // ' quadrature_isym' // TO_string(isym_grid) // '_nq=' // TO_string(nq)

    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,isym_grid=isym_grid,err=err_grid)
    res_test = (err_grid == 0)
    CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,info=(info_grid // ': Overlap check: F = ' // TO_string(res_test)))
    write(test_var%test_log_file_unit,*) &
       'When nb (=nq) is even and when the grid ends in B (PI), the overlap CANNOT be the identity matrix.'
    write(test_var%test_log_file_unit,*) ' Because,the last basis function on the grid is zero (d0gb(:,ib)=ZERO).'
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    !========================================================================================

    !========================================================================================
    name_grid = 'HO'
    nq = 5
    info_grid = name_grid // '_xc1._scale-0.5' // ' quadrature_nq=' // TO_string(nq)
    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,xc=ONE,scale=-HALF,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !========================================================================================

    CALL Finalize_Test(test_var)

    !========================================================================================
    CALL dealloc_Quadrature(xw)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_QuadratureError_QDUtil

  SUBROUTINE Test_Quadrature_NPHP_QDUtil()
    USE QDUtil_NumParameters_m
    USE QDUtil_Test_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    TYPE (Quadrature_t)      :: xw
    integer :: nq,nb,isym_grid,err_grid,err
    logical :: skip
    character (len=:), allocatable :: info_grid
    character (len=:), allocatable :: name_grid
    real (kind=Rkind),   parameter :: ZeroTresh    = TEN**2*epsilon(ONE)

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_Quadrature_NPHP_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='Quadrature_NP-HP')

    !=====================================================================================================================
    ! BoxAB quadrature test #1
    nq=101
    name_grid = 'BoxAB'
    info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)

    ! Normal Precision
    CALL Init_Quadrature_NP(xw,nq=nq,A=-ONE,B=ONE,name=name_grid,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)

    ! High Precision (only if Rk16 is defined (Rk16>0)), test #2 or not
    IF (Rk16 > 0) THEN
      info_grid = name_grid // ' (HP) quadrature_nq=' // TO_string(nq)
      CALL Init_Quadrature_HP(xw,nq=nq,A=-ONE,B=ONE,name=name_grid,err=err_grid)
      res_test = (err_grid ==0)
      CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !=====================================================================================================================

    !=====================================================================================================================
    ! BoxAB quadrature test #3 or #2
    nq=128
    name_grid = 'FourierAB'
    info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)

    ! Normal Precision
    CALL Init_Quadrature_NP(xw,nq=nq,A=-ONE,B=ONE,name=name_grid,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)

    ! High Precision (only if Rk16 is defined (Rk16>0)), test #4 or not
    IF (Rk16 > 0) THEN
      info_grid = name_grid // ' (HP) quadrature_nq=' // TO_string(nq)
      CALL Init_Quadrature_HP(xw,nq=nq,A=-ONE,B=ONE,name=name_grid,err=err_grid)
      res_test = (err_grid ==0)
      CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !=====================================================================================================================

    !=====================================================================================================================
    ! HO quadrature test #5 or #3
    nq=65
    IF (Rkind == Rk4) nq = 25 ! Because in simple precision, it does not work with nq=65
    name_grid = 'HO'
    info_grid = name_grid // '_xc1._scale0.5' // ' quadrature_nq=' // TO_string(nq)

    ! Normal Precision
    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,xc=ONE,scale=HALF,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)

    ! High Precision (only if Rk16 is defined (Rk16>0)) test #6 or not
    IF (Rk16 > 0) THEN
      info_grid = name_grid // ' (HP) quadrature_nq=' // TO_string(nq)
      CALL Init_Quadrature_HP(xw,nq=nq,name=name_grid,xc=ONE,scale=HALF,err=err_grid)
      res_test = (err_grid ==0)
      CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !=====================================================================================================================

    !=====================================================================================================================
    ! Legendre quadrature test #7 or #4
    nq=128
    name_grid = 'LegendreP'
    info_grid = name_grid // ' quadrature_nq=' // TO_string(nq)

    ! Normal Precision
    CALL Init_Quadrature_NP(xw,nq=nq,name=name_grid,err=err_grid)
    res_test = (err_grid ==0)
    CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
    CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)

    ! High Precision (only if Rk16 is defined (Rk16>0)) test #8 or not
    IF (Rk16 > 0) THEN
      info_grid = name_grid // ' (HP) quadrature_nq=' // TO_string(nq)
      CALL Init_Quadrature_HP(xw,nq=nq,name=name_grid,err=err_grid)
      res_test = (err_grid ==0)
      CALL Logical_Test(test_var,test1=res_test,info=(info_grid // ': Overlap check: T = ' // TO_string(res_test)))
      CALL Write_Quadrature(xw,nio=test_var%test_log_file_unit)
    END IF
    !=====================================================================================================================

    CALL Finalize_Test(test_var)

    !========================================================================================
    CALL dealloc_Quadrature(xw)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_Quadrature_NPHP_QDUtil

END MODULE QDUtil_Quadrature_m