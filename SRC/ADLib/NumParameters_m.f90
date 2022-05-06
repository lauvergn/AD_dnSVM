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
  MODULE ADLib_NumParameters_m
!$ USE omp_lib
      USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : INPUT_UNIT,OUTPUT_UNIT,real64,real128,int32,int64
      IMPLICIT NONE

      PUBLIC
      PRIVATE :: INPUT_UNIT,OUTPUT_UNIT,real64,real128,int32,int64

      integer, parameter :: Rkind        = real64 ! 8
      !integer, parameter :: Rkind        = real128 ! 8
      integer, parameter :: Ikind        = int32  ! 4
      integer, parameter :: ILkind       = int64  ! 8
      integer, parameter :: Name_len     = 20
      integer, parameter :: Name_longlen = 50
      integer, parameter :: Line_len     = 255
      integer, parameter :: error_l      = 80


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

      real (kind=Rkind), parameter ::                                   &
       pi = 3.14159265358979323846264338327950288419716939937511_Rkind

      complex (kind=Rkind), parameter :: EYE      = (0._Rkind,1._Rkind)
      complex (kind=Rkind), parameter :: CZERO    = (0._Rkind,0._Rkind)
      complex (kind=Rkind), parameter :: CONE     = (1._Rkind,0._Rkind)
      complex (kind=Rkind), parameter :: CHALF    = (0.5_Rkind,0._Rkind)

      integer :: print_level = 1        ! 0 minimal, 1 default, 2 large, -1 nothing

      character (len=Name_longlen) :: EneIO_format = "f20.5"
      character (len=Name_longlen) :: RMatIO_format = "f18.10"
      character (len=Name_longlen) :: CMatIO_format = "'(',f15.7,' +i',f15.7,')'"

      integer :: in_unitp  = INPUT_UNIT  ! Unit for the ouptput files, with the ISO_FORTRAN_ENV
      integer :: out_unitp = OUTPUT_UNIT ! Unit for the input, with the ISO_FORTRAN_ENV

  END MODULE ADLib_NumParameters_m
