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
MODULE QDUtil_NumParameters_m
  !$ USE omp_lib
  USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : INPUT_UNIT,OUTPUT_UNIT,real32,real64,real128,int32,int64
  IMPLICIT NONE
#ifndef __WITHRK16
#define __WITHRK16 1
#endif

  PUBLIC
  PRIVATE :: INPUT_UNIT,OUTPUT_UNIT,real32,real64,real128,int32,int64

  integer, parameter :: RkS        = real32 ! 4
  integer, parameter :: RkD        = real64 ! 8
  integer, parameter :: Rk4        = real32 ! 4
  integer, parameter :: Rk8        = real64 ! 8
#if __WITHRK16 == 1
  integer, parameter :: Rk16       = real128 ! 16
  integer, parameter :: RkQ        = real128 ! 16
#else
  integer, parameter :: Rk16       = -1
  integer, parameter :: RkQ        = -1
#endif
  integer, parameter :: IkS        = int32  ! 4
  integer, parameter :: IkD        = int64  ! 8
  integer, parameter :: Ik4        = int32  ! 4
  integer, parameter :: Ik8        = int64  ! 8

  integer, parameter :: Rkind       =                         &
#if defined(__RKIND)
      __RKIND
#else
      real64
#endif
  integer, parameter :: Ikind      = int32  ! 4
  integer, parameter :: ILkind     = int64  ! 8

  real (kind=Rkind), parameter :: ZERO    = 0._Rkind
  real (kind=Rkind), parameter :: ONE     = 1._Rkind
  real (kind=Rkind), parameter :: TWO     = 2._Rkind
  real (kind=Rkind), parameter :: THREE   = 3._Rkind
  real (kind=Rkind), parameter :: FOUR    = 4._Rkind
  real (kind=Rkind), parameter :: FIVE    = 5._Rkind
  real (kind=Rkind), parameter :: SIX     = 6._Rkind
  real (kind=Rkind), parameter :: SEVEN   = 7._Rkind
  real (kind=Rkind), parameter :: EIGHT   = 8._Rkind
  real (kind=Rkind), parameter :: NINE    = 9._Rkind
  real (kind=Rkind), parameter :: TEN     = 10._Rkind
  real (kind=Rkind), parameter :: ELEVEN  = 11._Rkind
  real (kind=Rkind), parameter :: TWELVE  = 12._Rkind
  real (kind=Rkind), parameter :: HUNDRED = 100._Rkind

  real (kind=Rkind), parameter :: HALF      = ONE/TWO
  real (kind=Rkind), parameter :: THIRD     = ONE/THREE
  real (kind=Rkind), parameter :: FOURTH    = ONE/FOUR
  real (kind=Rkind), parameter :: QUARTER   = ONE/FOUR
  real (kind=Rkind), parameter :: FIFTH     = ONE/FIVE
  real (kind=Rkind), parameter :: SIXTH     = ONE/SIX
  real (kind=Rkind), parameter :: ONETENTH  = ONE/TEN
  real (kind=Rkind), parameter :: TWOTENTHS = TWO/TEN

  real (kind=Rkind), parameter ::                                              &
                pi = 3.14159265358979323846264338327950288419716939937511_Rkind

  real(kind=Rk4), parameter :: pi_Rk4         = &
                 3.14159265358979323846264338327950288419716939937511_Rk4

  real(kind=Rk8), parameter :: pi_Rk8         = &
                3.14159265358979323846264338327950288419716939937511_Rk8

#if __WITHRK16 == 1
  real(kind=Rk16), parameter :: pi_Rk16         = &
                 3.14159265358979323846264338327950288419716939937511_Rk16
#endif

  complex (kind=Rkind), parameter :: EYE      = (ZERO,ONE)
  complex (kind=Rkind), parameter :: CZERO    = (ZERO,ZERO)
  complex (kind=Rkind), parameter :: CONE     = (ONE,ZERO)
  complex (kind=Rkind), parameter :: CTWO     = (TWO,ZERO)
  complex (kind=Rkind), parameter :: CHALF    = (HALF,ZERO)

  integer :: in_unit   = INPUT_UNIT  ! Unit for the ouptput files, with the ISO_FORTRAN_ENV
  integer :: out_unit  = OUTPUT_UNIT ! Unit for the input   files, with the ISO_FORTRAN_ENV

  integer, protected :: print_level  = -2        ! 0 minimal, 1 default, 2 large, -1 nothing, -2 not initialized

  integer, parameter :: Name_len     = 20
  integer, parameter :: Name_longlen = 50
  integer, parameter :: Line_len     = 255
  integer, parameter :: error_l      = 80

  character (len=*), parameter :: QDUtil_version =                         &
#if defined(__QD_VERSION)
      __QD_VERSION
#else
      'unknown: -D__QD_VERSION=?'
#endif

  character (len=*), parameter :: QDUtil_compile_date =                     &
#if defined(__COMPILE_DATE)
      __COMPILE_DATE
#else
      'unknown: -D__COMPILE_DATE=?'
#endif

  character (len=*), parameter :: QDUtil_compile_host =                      &
#if defined(__COMPILE_HOST)
      __COMPILE_HOST
#else
      "unknown: -D__COMPILE_HOST=?"
#endif
  logical, private :: QDUtil_Print_Version_done = .FALSE.

  PRIVATE :: QDUtil_set_print_level
  INTERFACE set_print_level
    MODULE PROCEDURE QDUtil_set_print_level
  END INTERFACE

CONTAINS
  SUBROUTINE QDUtil_set_print_level(prtlev,force)
    IMPLICIT NONE
    integer, intent(in)           :: prtlev
    logical, intent(in), optional :: force

    IF (present(force)) THEN
      IF (force .OR. print_level < -1) print_level = prtlev
    ELSE
      IF (print_level < -1)            print_level = prtlev
    END IF

  END SUBROUTINE QDUtil_set_print_level
  SUBROUTINE version_QDUtil(Print_Version)
    USE iso_fortran_env, ONLY : compiler_version,compiler_options
    IMPLICIT NONE

    logical,             intent(in)    :: Print_Version

    IF (Print_Version) THEN
      QDUtil_Print_Version_done = .TRUE.
      write(out_unit,*) '================================================='
      write(out_unit,*) '================================================='
      write(out_unit,*) '== QD (Quantum Dynamics) Util Libraries ========='
      write(out_unit,*) '== QDUtil version:    ',QDUtil_version
      write(out_unit,*) '-------------------------------------------------'
      write(out_unit,*) '== Compiled on       "',QDUtil_compile_host, '" the ',QDUtil_compile_date
      write(out_unit,*) '== Compiler:         ',compiler_version()
      write(out_unit,*) '== Compiler options: ',compiler_options()
      write(out_unit,*) '-------------------------------------------------'
      write(out_unit,*) 'QDUtil is a free software under the MIT Licence.'
      write(out_unit,*) '  Copyright (c) 2022 David Lauvergnat [1]'
      write(out_unit,*)
      write(out_unit,*) '  [1]: Institut de Chimie Physique, UMR 8000, CNRS-Université Paris-Saclay, France'
      write(out_unit,*) '=================================================' 
#if __LAPACK == 0
      write(out_unit,*) '  Lapack library is not linked'
#else
      write(out_unit,*) '  Lapack library is linked'
#endif
#if __WITHRK16 == 1
      write(out_unit,*) 'Reals with quadruple precision (real128) are available'
#else
    write(out_unit,*) 'Reals with quadruple precision (real128) are NOT available'
#endif
      write(out_unit,*) '  WITHRK16',__WITHRK16
      write(out_unit,*) '  Rk16',Rk16
      write(out_unit,*) '  Rkind',Rkind

      write(out_unit,*) '=================================================' 
    END IF
    
  END SUBROUTINE version_QDUtil
  SUBROUTINE Test_QDUtil_NumParameters()
    USE QDUtil_Test_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind),   parameter   :: ZeroTresh    = 100._Rkind*epsilon(1._Rkind)
    integer                          :: i

    real (kind=Rkind), parameter   :: tab_ParaReal(*) = [                      &
          0._Rkind,  1._Rkind,2._Rkind, 3._Rkind, 4._Rkind, 5._Rkind,  6._Rkind,&
          7._Rkind,  8._Rkind,9._Rkind,10._Rkind,11._Rkind,12._Rkind,100._Rkind,&
          0.5_Rkind,1._Rkind/3._Rkind,0.25_Rkind,0.25_Rkind,0.2_Rkind,          &
          1._Rkind/6._Rkind,0.1_Rkind,0.2_Rkind]

    real (kind=Rkind),    allocatable   :: tab_Real(:)
    complex (kind=Rkind), allocatable   :: tab_Cplx(:)
    complex (kind=Rkind), parameter     :: tab_ParaCplx(*) = [          &
           (0._Rkind,1._Rkind),(0._Rkind,0._Rkind),(1._Rkind,0._Rkind), &
           (2._Rkind,0._Rkind),(0.5_Rkind,0._Rkind)]

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_NumParameters'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='NumParameters')

    tab_Real = [ZERO,ONE,TWO,THREE,FOUR,FIVE,SIX,           &
                SEVEN,EIGHT,NINE,TEN,ELEVEN,TWELVE,HUNDRED, &
                HALF,THIRD,FOURTH,QUARTER,FIFTH,            &
                SIXTH,ONETENTH,TWOTENTHS]
    res_test = all(abs(tab_Real-tab_ParaReal) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='22 real parameters ')
    IF (.NOT. res_test .OR. debug) THEN
      DO i=1,size(tab_Real)
        write(out_unit,*) 'Val,Paramter,Diff',tab_Real(i),tab_ParaReal(i),tab_Real(i)-tab_ParaReal(i)
      END DO
    END IF
    CALL Flush_Test(test_var)

    res_test = abs(PI-FOUR*atan(ONE)) < ZeroTresh
    CALL Logical_Test(test_var,test1=res_test,info='PI value ')
    IF (.NOT. res_test .OR. debug) write(out_unit,*) 'Val,Paramter,Diff',FOUR*atan(ONE),PI,FOUR*atan(ONE)-PI

    CALL Flush_Test(test_var)
    tab_Cplx = [EYE,CZERO,CONE,CTWO,CHALF]
    res_test = all(abs(tab_ParaCplx-tab_Cplx) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='5 complex parameters ')
    IF (.NOT. res_test .OR. debug) THEN
      DO i=1,size(tab_Cplx)
        write(out_unit,*) 'Val,Paramter,Diff',tab_Cplx(i),tab_ParaCplx(i),tab_Cplx(i)-tab_ParaCplx(i)
      END DO
    END IF
    CALL Flush_Test(test_var)

    ! print_level
    CALL set_print_level(1,force=.TRUE.)

    res_test = (print_level == 1)
    CALL Logical_Test(test_var,test1=res_test,info='print_level=1')

    CALL set_print_level(0,force=.TRUE.)
    res_test = (print_level == 0)
    CALL Logical_Test(test_var,test1=res_test,info='set_print_level(0)')
    CALL set_print_level(2)
    res_test = (print_level == 0)
    CALL Logical_Test(test_var,test1=res_test,info='set_print_level(0)')
    CALL set_print_level(1,force=.FALSE.)
    res_test = (print_level == 0)
    CALL Logical_Test(test_var,test1=res_test,info='set_print_level(0)')
    CALL Flush_Test(test_var)


    ! finalize the tests
    CALL Finalize_Test(test_var)

  END SUBROUTINE Test_QDUtil_NumParameters
END MODULE QDUtil_NumParameters_m
