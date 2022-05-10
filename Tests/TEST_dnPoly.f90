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
  USE ADLib_NumParameters_m
  USE ADLib_Util_m
  USE ADdnSVM_m
  USE ADLib_Test_m
  IMPLICIT NONE

  TYPE (dnS_t)                        :: dnX,Sana,Sres
  TYPE (dnS_t), allocatable           :: Vec_dnS(:)

  integer                             :: i,nderiv
  integer                             :: nb_Err,nb_OK,nb_Test
  real(kind=Rkind)                    :: Norm,x,xx
  character (len=*), parameter        :: name_sub='TEST_dnPoly'
  logical                             :: prlev
  TYPE (test_t)                       :: test_var


  CALL read_arg()
  prlev = (print_level > 0)

  CALL test_initialyze(test_var,test_name='dnPoly')


  nderiv = 1
  write(out_unitp,'(a,i2)') "== TESTING dnPoly module with nderiv=",nderiv
  nb_Test = 0
  nb_OK   = 0
  nb_Err  = 0



  x       = HALF
  dnX     = Variable(x,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives

  Sana  = dnBox(dnX,2)
  Norm  = ONE/sqrt(pi/2)
  CALL set_dnS(Sres,d0=sin(2*x)*Norm,d1=[2*cos(2*x)*Norm]) ! sin(2*x)/sqrt(pi/)
  CALL test_logical(test_var,                                                 &
                    test1=AD_Check_dnS_IS_ZERO(Sana-Sres,ONETENTH**10),       &
                    info='Box(x,2)        = sin(2*x)/sqrt(pi/2)        ')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnBox(x,2)')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnAna')


  Sana  = dnMonomial(dnX,2)
  CALL set_dnS(Sres,d0=x**2,d1=[2*x]) ! x**2
  CALL test_logical(test_var,                                                 &
                    test1=AD_Check_dnS_IS_ZERO(Sana-Sres,ONETENTH**10),       &
                    info='Monomial(x,2)   = x**2                       ')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnMonomial(x,2)')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnAna')


  Sana  = dnFourier(dnX,2) ! sin(x)/sqrt(pi)
  Norm  = ONE/sqrt(pi)
  CALL set_dnS(Sres,d0=sin(x)*Norm,d1=[cos(x)*Norm]) ! sin(x)/sqrt(pi)
  CALL test_logical(test_var,                                                 &
                    test1=AD_Check_dnS_IS_ZERO(Sana-Sres,ONETENTH**10),       &
                    info='Fourier(x,2)    = sin(x)/sqrt(pi)            ')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnFourier(x,2)')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnAna')


  Sana  = dnLegendre0(dnX,2) ! sin(x)/sqrt(pi)
  Norm  = ONE/sqrt(TWO/(TWO*2+ONE))
  CALL set_dnS(Sres,d0=(THREE*x**2-ONE)/TWO*Norm,d1=[THREE*x*Norm]) ! (3x**2-1)/2/norm
  CALL test_logical(test_var,                                                 &
                    test1=AD_Check_dnS_IS_ZERO(Sana-Sres,ONETENTH**10),       &
                    info='Legendre0(x,2)  = (3x**2-1)/2/Norm           ')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnLegendre0(x,2)')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnAna')


  ! Jacobi(x,2,1,1) = 3 + 15(x-1)/2 +  15((x-1)/2)**2
  xx    = (x-1)/2
  Sana  = dnJacobi(dnX,n=2,alpha=1,beta=1,ReNorm=.FALSE.) ! Jacobi(x,2,1,1)
  nb_Test = nb_Test + 1
  CALL set_dnS(Sres,d0=3+15*xx+15*xx**2,d1=[15*HALF + 15*xx])
  CALL test_logical(test_var,                                                 &
                    test1=AD_Check_dnS_IS_ZERO(Sana-Sres,ONETENTH**10),       &
                    info='Jacobi(x,2,1,1) = 3+15(xx+xx**2); xx=(x-1)/2 ')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnJacobi(x,2,1,1)')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnAna')


  Sana  = dnHermite(dnX,2) ! Hermite(x,2) = 4*x**2-2
  Norm  = ONE/sqrt(sqrt(pi)*TWO**2*gamma_perso(2+1))
  CALL set_dnS(Sres,d0=(4*x**2-2)*Norm,d1=[(8*x)*Norm])
  CALL test_logical(test_var,                                                 &
                    test1=AD_Check_dnS_IS_ZERO(Sana-Sres,ONETENTH**10),       &
                    info='Hermite(x,2)    = (4*x**2-2)/Norm            ')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnHermite(x,2)')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnAna')


  Sana  = dnExpHermite(dnX,2) ! Exp(-x**2/2)Hermite(x,2)
  Norm  = ONE/sqrt(sqrt(pi)*TWO**2*gamma_perso(2+1))
  CALL set_dnS(Sres,d0=Exp(-x**2/2)*(4*x**2-2)*Norm,d1=[-2*x*Exp(-x**2/2)*(2*x**2-5)*Norm])
  CALL test_logical(test_var,                                                 &
                    test1=AD_Check_dnS_IS_ZERO(Sana-Sres,ONETENTH**10),       &
                    info='ExpHermite(x,2) = Exp(-x**2/2)(4*x**2-2)/Norm')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnExpHermite(x,2)')
  IF (prlev) CALL Write_dnS(Sana,nio=test_var%test_log_file_unit,info='dnAna')



  CALL test_finalize(test_var)

CONTAINS
  SUBROUTINE read_arg()
    USE ADLib_NumParameters_m
    IMPLICIT NONE

    character(len=:), allocatable :: arg,arg2
    integer :: iarg,arg_len

    IF (COMMAND_ARGUMENT_COUNT() /= 0 .AND. COMMAND_ARGUMENT_COUNT() /= 2) THEN
      write(out_unitp,*) ' ERROR in read_arg'
      write(out_unitp,*) ' Wrong TEST_dnPoly argument number!'
      write(out_unitp,*) 'argument number',COMMAND_ARGUMENT_COUNT()
      write(out_unitp,*) ' You can have 0 or 2 arguments.'
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
        read(arg2,*) print_level
      CASE Default
        write(out_unitp,*) ' ERROR in read_arg'
        write(out_unitp,*) ' Wrong TEST_dnPoly argument!'
        write(out_unitp,*) '   arg: "',arg,'"'
        write(out_unitp,*) ' The possibilities are:'
        write(out_unitp,*) '    -p or --print'
        STOP 'Wrong TEST_dnPoly argument'
      END SELECT

      IF (print_level > 0) write(out_unitp,*) 'Argument number: ',iarg,' ==> arg: "',arg,'", arg2: "',arg2,'"'

      deallocate(arg)
      deallocate(arg2)
    END DO
    IF (print_level > 0) write(out_unitp,*) '=================================='

  END SUBROUTINE read_arg

END PROGRAM TEST_dnPoly
