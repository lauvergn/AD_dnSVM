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
!!     cd Tests ; ./run_tests
!!
!! The results will be compared to previous ones in run_tests/RES_old
!!
!> @author David Lauvergnat
!! @date 03/08/2017
!!
PROGRAM TEST_dnPoly
  USE QDUtil_m
  USE QDUtil_Test_m
  USE ADdnSVM_m
  IMPLICIT NONE

  TYPE (dnS_t)                        :: dnX,dnTh,dnPhi,Sana,SExact
  TYPE (dnS_t), allocatable           :: Vec_dnS(:)

  integer                             :: i,nderiv
  real(kind=Rkind)                    :: Norm,x,xx
  character (len=*), parameter        :: name_sub='TEST_dnPoly'
  TYPE (test_t)                       :: test_var
  real (kind=Rkind), parameter        :: ZeroTresh   = TEN**2*epsilon(ONE)


  CALL read_arg()

  CALL Initialize_Test(test_var,test_name='dnPoly',PrintFlag = (print_level > 0))


  IF (Rkind == Rk4) THEN
    nderiv = 1
    CALL Set_step_dnS(ONETENTH**3)
  ELSE
    nderiv = 1
  END IF
  write(out_unit,'(a,i2)') "== TESTING dnPoly module with nderiv=",nderiv

  x       = HALF
  dnX     = Variable(x,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives

  Sana  = dnBox(dnX,2)
  Norm  = ONE/sqrt(pi/2)
  CALL set_dnS(SExact,d0=sin(2*x)*Norm,d1=[2*cos(2*x)*Norm]) ! sin(2*x)/sqrt(pi/)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Box(x,2)        = sin(2*x)/sqrt(pi/2)        ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnBox(x,2)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)


  Sana  = dnMonomial(dnX,2)
  CALL set_dnS(SExact,d0=x**2,d1=[2*x]) ! x**2
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Monomial(x,2)   = x**2                       ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnMonomial(x,2)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  Sana  = dnFourier(dnX,2) ! sin(x)/sqrt(pi)
  Norm  = ONE/sqrt(pi)
  CALL set_dnS(SExact,d0=sin(x)*Norm,d1=[cos(x)*Norm]) ! sin(x)/sqrt(pi)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Fourier(x,2)    = sin(x)/sqrt(pi)            ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnFourier(x,2)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)


  Sana  = dnFourier2(dnX,-2) ! sin(2x)/sqrt(pi)
  Norm  = ONE/sqrt(pi)
  CALL set_dnS(SExact,d0=sin(2*x)*Norm,d1=[2*cos(2*x)*Norm]) ! sin(x)/sqrt(pi)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Fourier2(x,-2)  = sin(2x)/sqrt(pi)           ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnFourier2(x,-2)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)


  Sana  = dnLegendre0(dnX,2)
  Norm  = ONE/sqrt(TWO/(TWO*2+ONE))
  CALL set_dnS(SExact,d0=(THREE*x**2-ONE)/TWO*Norm,d1=[THREE*x*Norm]) ! (3x**2-1)/2/norm
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Legendre0(x,2)  = (3x**2-1)/2/Norm           ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnLegendre0(x,2)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  Sana    = dnLegendre(dnX,2,0)
  SExact  = dnLegendre0(dnX,2)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Legendre(x,2,0) = Legendre0(x,2)             ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnLegendre(x,2,0)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnLegendre0(x,2)')
  END IF
  CALL Flush_Test(test_var)

  Sana    = dnLegendre(dnX,1,1,ReNorm=.FALSE.)
  SExact  = -sqrt(ONE-dnX**2)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Legendre(x,1,1) = -sqrt(ONE-x**2)            ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnLegendre(x,1,1)',Rfmt='f15.11')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact',          Rfmt='f15.11')
  END IF
  CALL Flush_Test(test_var)

  Sana    = dnLegendre(dnX,1,1)
  SExact  = -sqrt(ONE-dnX**2)/sqrt(FOUR/THREE)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Legendre(x,1,1) = -sqrt(ONE-x**2)/sqrt(4/3)  ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnLegendre(x,1,1)',Rfmt='f15.11')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact',          Rfmt='f15.11')
  END IF
  CALL Flush_Test(test_var)

  ! Jacobi(x,2,1,1) = 3 + 15(x-1)/2 +  15((x-1)/2)**2
  xx    = (x-1)/2
  Sana  = dnJacobi(dnX,n=2,alpha=1,beta=1,ReNorm=.FALSE.) ! Jacobi(x,2,1,1)
  CALL set_dnS(SExact,d0=3+15*xx+15*xx**2,d1=[15*HALF + 15*xx])
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Jacobi(x,2,1,1) = 3+15(xx+xx**2); xx=(x-1)/2 ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnJacobi(x,2,1,1)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  Sana  = dnHermite(dnX,2) ! Hermite(x,2) = 4*x**2-2
  Norm  = ONE/sqrt(sqrt(pi)*TWO**2*gamma_perso(2+1))
  CALL set_dnS(SExact,d0=(4*x**2-2)*Norm,d1=[(8*x)*Norm])
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Hermite(x,2)    = (4*x**2-2)/Norm            ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnHermite(x,2)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  Sana  = dnExpHermite(dnX,2) ! Exp(-x**2/2)Hermite(x,2)
  Norm  = ONE/sqrt(sqrt(pi)*TWO**2*gamma_perso(2+1))
  CALL set_dnS(SExact,d0=Exp(-x**2/2)*(4*x**2-2)*Norm,d1=[-2*x*Exp(-x**2/2)*(2*x**2-5)*Norm])
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='ExpHermite(x,2) = Exp(-x**2/2)(4*x**2-2)/Norm')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='dnExpHermite(x,2)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  ! Real Spherical Harmonics
  dnTh  = dnX
  dnPhi = dnX
  Sana  = RSphericalHarmonics2(dnTh,dnPhi,0,0)
  SExact  = ONE/sqrt(4*pi)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Y00(th,phi) ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='Y00(th,phi)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  Sana    = RSphericalHarmonics2(dnTh,dnPhi,1,0)
  SExact  = cos(dnTh)/sqrt(TWO/THREE)/ONE/sqrt(2*pi)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Y10(th,phi) ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='Y10(th,phi)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  Sana    = RSphericalHarmonics2(dnTh,dnPhi,1,-1)
  SExact  = -sin(dnTh)*sin(dnPhi)/sqrt(FOUR/THREE)/ONE/sqrt(pi)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Y1-1(th,phi) ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='Y1-1(th,phi)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  Sana    = RSphericalHarmonics2(dnTh,dnPhi,1,1)
  SExact  = -sin(dnTh)*cos(dnPhi)/sqrt(FOUR/THREE)/ONE/sqrt(pi)
  CALL Logical_Test(test_var,                                                   &
                    test1=AD_Check_dnS_IS_ZERO(Sana-SExact,ZeroTresh),       &
                    info='Y11(th,phi) ')
  IF (test_var%PrintFlag) THEN
    CALL Write_dnS(Sana,  string=test_var%test_log,info='Y11(th,phi)')
    CALL Write_dnS(SExact,string=test_var%test_log,info='dnExact')
  END IF
  CALL Flush_Test(test_var)

  CALL Finalize_Test(test_var)

CONTAINS
  SUBROUTINE read_arg()
    IMPLICIT NONE

    character(len=:), allocatable :: arg,arg2
    integer :: iarg,arg_len,prt_lev

    IF (COMMAND_ARGUMENT_COUNT() /= 0 .AND. COMMAND_ARGUMENT_COUNT() /= 2) THEN
      write(out_unit,*) ' ERROR in read_arg'
      write(out_unit,*) ' Wrong TEST_dnPoly argument number!'
      write(out_unit,*) 'argument number',COMMAND_ARGUMENT_COUNT()
      write(out_unit,*) ' You can have 0 or 2 arguments.'
      STOP 'Wrong TEST_dnPoly argument number'
    END IF


    DO iarg=1,COMMAND_ARGUMENT_COUNT(),2

      CALL GET_COMMAND_ARGUMENT( NUMBER=iarg, LENGTH=arg_len )
      allocate( character(len=arg_len) :: arg )
      CALL GET_COMMAND_ARGUMENT( NUMBER=iarg, VALUE=arg )

      CALL GET_COMMAND_ARGUMENT( NUMBER=iarg+1, LENGTH=arg_len )
      allocate( character(len=arg_len) :: arg2 )
      CALL GET_COMMAND_ARGUMENT( NUMBER=iarg+1, VALUE=arg2 )

      SELECT CASE(arg)
      CASE("-p","--print")
        read(arg2,*) prt_lev
        CALL set_print_level(prt_lev,force=.TRUE.)
      CASE Default
        write(out_unit,*) ' ERROR in read_arg'
        write(out_unit,*) ' Wrong TEST_dnPoly argument!'
        write(out_unit,*) '   arg: "',arg,'"'
        write(out_unit,*) ' The possibilities are:'
        write(out_unit,*) '    -p or --print'
        STOP 'Wrong TEST_dnPoly argument'
      END SELECT

      IF (print_level > 0) write(out_unit,*) 'Argument number: ',iarg,' ==> arg: "',arg,'", arg2: "',arg2,'"'

      deallocate(arg)
      deallocate(arg2)
    END DO
    IF (print_level > 0) write(out_unit,*) '=================================='

  END SUBROUTINE read_arg

END PROGRAM TEST_dnPoly
