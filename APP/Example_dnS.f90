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
  USE QDUtil_m
  USE ADdnSVM_m
  IMPLICIT NONE

  TYPE (dnS_t)                  :: X,Y,Z,f,r,th,g
  TYPE (dnS_t),     allocatable :: Vec(:),VecXY(:)
  real(kind=Rkind), allocatable :: JacNewOld(:,:) ! JacNewOld(iN,iO). iN and iO are the index of the new and old coordinates
  real(kind=Rkind), allocatable :: DeltaQ(:),Q(:)
  real(kind=Rkind) :: ValExact,ValTayl

  integer                     :: i
  integer                     :: nderiv = 1

  character (len=*), parameter :: name_sub='Example_dnS'

  CALL version_ADdnSVM(Print_Version=.TRUE.)

  write(out_unit,'(a)') "== Example_dnS =="

  Vec = Variable([HALF,ONE,TWO], nderiv=3)


  CALL Write_dnS(Vec(1),info='X: 1st variable. Value: 0.5',nderiv=1,Rfmt='f20.10')
  CALL Write_dnS(Vec(2),info='Y: 2d  variable. Value: 1.0',nderiv=1,Rfmt='f20.10')
  CALL Write_dnS(Vec(3),info='Z: 3d  variable. Value: 2.0',nderiv=1,Rfmt='f20.10')

  f = ONE + cos(Vec(1))**2 + sin(Vec(2)*Vec(3))

  CALL Write_dnS(f,info='f=1.0 + cos(X)**2 + sin(Y*Z), value: 2.67945',nderiv=1,Rfmt='f20.10')

  write(out_unit,*)
  write(out_unit,*)
  write(out_unit,*) 'Talylor expansion:'
  Q = get_d0(Vec)
  deltaQ = [ONE,ONE,ONE]*ONETENTH**2
  Q = Q + deltaQ

  ValExact = (ONE + cos(Q(1))**2 + sin(Q(2)*Q(3)))
  write(out_unit,*) 'Val (exact)',ValExact

  ValTayl = TO_Taylor(f,DeltaQ,nderiv=0)
  write(out_unit,*) 'Val (order 0), diff', ValTayl,ValExact-ValTayl
  ValTayl = TO_Taylor(f,DeltaQ,nderiv=1)
  write(out_unit,*) 'Val (order 1), diff', ValTayl,ValExact-ValTayl
  ValTayl = TO_Taylor(f,DeltaQ,nderiv=2)
  write(out_unit,*) 'Val (order 2), diff', ValTayl,ValExact-ValTayl
  ValTayl = TO_Taylor(f,DeltaQ,nderiv=3)
  write(out_unit,*) 'Val (order 3), diff', ValTayl,ValExact-ValTayl
  write(out_unit,*)
  write(out_unit,*)

  write(out_unit,'(a)') "== Jacobian matrix (polar transformation) =="
  r  = Variable( Val=TWO,  nVar=2, iVar=1, nderiv=1 )
  th = Variable( Val=Pi/3, nVar=2, iVar=2, nderiv=1 )

  x = r*cos(th)
  y = r*sin(th)
  write(out_unit,*) '[dx/dr, dx/dth]:',get_d1(x)
  write(out_unit,*) '[dy/dr, dy/dth]:',get_d1(y)

  Vec   = Variable([TWO,Pi/3], nderiv=1 ) ! Vec(1) : r, Vec(2) : th
  VecXY = Vec(1)*[cos(Vec(2)),sin(Vec(2))] ! polar transformation


  write(out_unit,*) 'Jac(inew,iold)=[ dQinew/dQiold ]:'
  JacNewOld = get_Jacobian( VecXY )
  CALL Write_Mat(JacNewOld,out_unit,5)

  write(out_unit,*) 'analytical: [dx/dr, dx/dth]:   [0.5,     -1.732...]'
  write(out_unit,*) 'analytical: [dy/dr, dy/dth]:   [0.866..,  1.      ]'

  write(out_unit,*) '== derivative of dnS =='
  write(out_unit,*) ' f=2*x**2 * y ;  g=(df/dx)**2 * x/y'
  write(out_unit,*) ' x = 1. , y=2.'
  Vec   = Variable([ONE,TWO], nderiv=2 ) ! Vec(1) : x, Vec(2) : y
  X = Vec(1) ; Y=Vec(2)
  f     = TWO * X**2 * Y
  g     = deriv(f,ider=1)
  CALL Write_dnS(g,info='df/dx=4*x*y, value: 8',Rfmt='f20.10')

  g     = deriv(f,ider=1)**2 * X/Y
  CALL Write_dnS(g,info='g=16 * x**3 * y, value: 32',Rfmt='f20.10')

  write(out_unit,*) '== Box(x,i) [0,Pi] =='
  write(out_unit,*) ' [sin(x)/sqrt(pi/2), sin(2x)/sqrt(pi/2), sin(3x)/sqrt(pi/2) ...]'
  write(out_unit,*) ' x = 0.5'

  X   = Variable(Val=HALF, nderiv=1 )
  Vec = dnBox(X,[1,2,3,4,5,6])
  DO i=1,size(Vec)
    CALL Write_dnS(Vec(i),out_unit,info='dnBox_' // int_TO_char(i),Rfmt='f20.10')
  END DO


  nderiv = 1
  Vec   = Variable([HALF,ONE,TWO,THREE], nderiv=nderiv)
  CALL Write_dnS(Vec(1),info='Vec(1)',Rfmt='f20.10')
  CALL Write_dnS(Vec(2),info='Vec(2)',Rfmt='f20.10')

  VecXY = Variable([get_d0(Vec(1))*get_d0(Vec(2))],nderiv=nderiv)
  CALL Write_dnS(VecXY(1),info='VecXY(1)',Rfmt='f20.10')

  f     = cos(VecXY(1))
  CALL Write_dnS(f,info='f',Rfmt='f20.10')

  CALL Write_dnS(cos(Vec(1)*Vec(2)),info='cos(Vec(1)*Vec(2))',Rfmt='f20.10')

  f   = cos(Vec(1)*Vec(2))-dnF_OF_dnS(f,[Vec(1)*Vec(2)])
  CALL Write_dnS(f,info='dnF_OF_dnS, value: 0.',Rfmt='f20.10')


END PROGRAM Example_dnS
