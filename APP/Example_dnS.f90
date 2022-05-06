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
!> @brief Program which checks the dnS module
!!
!! @licence MIT
!!
!! @section install_sec Installation
!!
!! Dependencies: this module needs the fortran modules in the @e Lib/Lib directory.
!!
!! Build the module and the testing unit (with dependencies):
!!
!!     make dnS.x
!!
!! Build the module documentation (with doxygen):
!!
!!     make doxy
!!
!! @section test_sec Tests
!!
!! To test the installation, you can run tests (dnS and ModLib).
!!
!!     cd Tests ; ./run_test_dnS
!!
!! The results will be compared to previous ones in run_tests/RES_old
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
PROGRAM Example_dnS
  USE ADdnSVM_dnSVM_m
  IMPLICIT NONE

  TYPE (dnS_t)                :: X,Y,Z,f,r,th
  TYPE (dnS_t), allocatable   :: Vec_dnS(:)
  integer                     :: i
  integer                     :: nderiv = 1

  character (len=*), parameter :: name_sub='Example_dnS'


  write(out_unitp,'(a)') "== Example_dnS =="

  X = Variable( Val=HALF, nVar=3, iVar=1, nderiv=1 )
  Y = Variable( Val=ONE,  nVar=3, iVar=2, nderiv=1 )
  Z = Variable( Val=TWO,  nVar=3, iVar=3, nderiv=1 )

  CALL Write_dnS(X,info='X: 1st variable. Value: 0.5')
  CALL Write_dnS(Y,info='Y: 2d  variable. Value: 1.0')
  CALL Write_dnS(Z,info='Z: 3d  variable. Value: 2.0')


  f = ONE + cos(X)**2 + sin(Y*Z)

  CALL Write_dnS(f,info='f=1.0 + cos(X)**2 + sin(Y*Z), value: 2.67945')

  write(out_unitp,'(a)') "== Jacobian matrix (polar transformation) =="
  r  = Variable( Val=TWO,  nVar=2, iVar=1, nderiv=1 )
  th = Variable( Val=Pi/3, nVar=2, iVar=2, nderiv=1 )

  x = r*cos(th)
  y = r*sin(th)
  write(out_unitp,*) '[dx/dr, dx/dth]:',get_d1(x)
  write(out_unitp,*) '[dy/dr, dy/dth]:',get_d1(y)

  write(out_unitp,*) 'analytical: [dx/dr, dx/dth]:   [0.5,     -1.732...]'
  write(out_unitp,*) 'analytical: [dy/dr, dy/dth]:   [0.866..,  1.      ]'

  write(out_unitp,*) '== Box(x,i) [0,Pi] =='
  write(out_unitp,*) ' [sin(x)/sqrt(pi/2), sin(2x)/sqrt(pi/2), sin(3x)/sqrt(pi/2) ...]'

  Vec_dnS = dnBox(X,[1,2,3,4,5,6])
  DO i=1,size(Vec_dnS)
    CALL Write_dnS(Vec_dnS(i),out_unitp,info='dnBox_' // int_TO_char(i))
  END DO

END PROGRAM Example_dnS
